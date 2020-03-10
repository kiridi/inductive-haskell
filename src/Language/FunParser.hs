module Language.FunParser(funParser) where
    import Language.Parsing
    import Language.Syntax
    import Data.Char
    import Language.Types
    import Language.Infer
    import qualified Data.Set as Set
    
    data Token = 
        IDENT IdKind Ident | NUMBER Integer | CHARACTER Char
      | LPAR | RPAR | COMMA | EQUAL | ASSIGN | SEMI | SSEMI
      | IF | THEN | ELSE | LET | REC | VAL | LAMBDA | IN | WHILE | DO
      | ORELSE | PIPE | MINUS | STAR | AMPER | ARRAY | BRA | KET 
      | LBRACE | RBRACE | ARROW | VBAR | DOT | OPEN
      | BADTOK Char
      | PEX | NEX | SYNTH
      | INT | BOOL | CHAR | AT
      deriving Eq
      
    data IdKind = 
      ID | MONOP | CONSOP | MULOP | ADDOP | RELOP 
      deriving (Eq, Show)
    
    instance Show Token where
      show t = 
        case t of 
          IDENT k x -> show x; NUMBER n -> show n; 
          CHARACTER c -> "\'" ++ [c] ++ "\'"
          LPAR -> "("; RPAR -> ")"; COMMA -> ","
          EQUAL -> "="; SEMI -> ";"; SSEMI -> ";;"; ASSIGN -> ":="
          IF -> "if"; THEN -> "then"; ELSE -> "else"; LET -> "let"
          REC -> "rec"; VAL -> "val"; LAMBDA -> "lambda"; IN -> "in"
          WHILE -> "while"; DO -> "do"; ORELSE -> "orelse"; PIPE -> ">>"
          MINUS -> "-"; STAR -> "*"; AMPER -> "&"; ARRAY -> "array"
          BRA -> "["; KET -> "]"; LBRACE -> "{"; RBRACE -> "}"
          ARROW -> "=>"; VBAR -> "|"; DOT -> "."; OPEN -> "open"; 
          BADTOK c -> [c]; 

          PEX -> "PEx"; NEX -> "NEx"; SYNTH -> "Synthesize"; AT -> "@"; 
          INT -> "Int"; BOOL -> "Bool" ; CHAR -> "Char"
    
    kwlookup = 
      make_kwlookup (IDENT ID)
        [("if", IF), ("then", THEN), ("else", ELSE), ("let", LET), ("in", IN),
          ("rec", REC), ("val", VAL), ("lambda", LAMBDA), ("while", WHILE), 
          ("do", DO), ("orelse", ORELSE), ("array", ARRAY), ("open", OPEN),
          ("div", IDENT MULOP "div"), ("mod", IDENT MULOP "mod"), 
          ("PEx", PEX), ("NEx", NEX), ("Synthesize", SYNTH), ("@", AT),
          ("Int", INT), ("Bool", BOOL), ("Char", CHAR)]
    
    lexer =
      do 
        c <- nextch
        case c of
          _ | isAlpha c ->
            do 
              s <- star (\ c -> isAlphaNum c || c == '_')
              return (kwlookup (c:s))
          _ | isDigit c ->
            do s <- star isDigit; return (NUMBER (read (c:s)))
          '\'' ->
            do c <- nextch; nextch; return (CHARACTER c)
          '=' -> switch [('>', return ARROW)] (return EQUAL)
          '+' -> return (IDENT ADDOP "+")
          '-' -> switch [('-', do scanComment; lexer)] (return MINUS)
          '*' -> return STAR
          '&' -> return AMPER
          '!' -> return (IDENT MONOP "!")
          '~' -> return (IDENT MONOP "~")
          ',' -> return COMMA
          '@' -> return AT
          '<' -> switch [('=', return (IDENT RELOP "<=")),
                            ('>', return (IDENT RELOP "<>"))]
                    (return (IDENT RELOP "<"))
          '>' -> switch [('=', return (IDENT RELOP ">=")),
                ('>', return PIPE)]
                    (return (IDENT RELOP ">"))
          '(' -> return LPAR
          ')' -> return RPAR
          '[' -> return BRA
          ']' -> return KET
          '{' -> return LBRACE
          '}' -> return RBRACE
          '.' -> return DOT
          '|' -> return VBAR
          ';' -> switch [(';', return SSEMI)] (return SEMI)
          ':' -> switch [('=', return ASSIGN)] 
            (return (IDENT CONSOP ":"))
          ' ' -> lexer
          '\t' -> lexer 
          '\n' -> do incln; lexer
          '\r' -> do incln; lexer
          _ -> return (BADTOK c)
                  
    scanComment =
      do 
        c <- nextch
        case c of
          '\n' -> incln
          _ -> scanComment
    
    -- {\syn _phrase_ \arrow\ _expr_ ";;" \orr\ _def_ ";;"}
    p_phrase =
      do e <- p_expr; eat SSEMI; return (Calculate e)
      <+> do d <- p_def; eat SSEMI; return (Define d)
      <+> do eat SYNTH; name <- p_name; (_, ins) <- p_fsts; eat AT; out <- p_type; eat SSEMI; let typ = Arrow (TTuple ins) out in return (Synth name (Forall (Set.toList $ ftv typ) typ))
    
    -- {\syn _def_ \arrow\ "val" _eqn_ \orr\ "rec" _eqn_ \orr\ "array" _name_ "[" _expr_ "]" \orr\ "open" _expr_}
    p_def = 
      do eat VAL; (x, e) <- p_eqn; return (Val x e)
      <+> do eat REC; (x, e) <- p_eqn; return (Rec x e)
      <+> do eat PEX; ident <- p_name; ins <- p_actuals; eat ARROW; out <- p_expr; return (PEx ident ins out)
      <+> do eat NEX; ident <- p_name; ins <- p_actuals; eat ARROW; out <- p_expr; return (NEx ident ins out)
    
    -- {\syn _eqn_ \arrow\ _name_ "=" _expr_ \orr\ _name_ _formals_ "=" _expr_}
    p_eqn =
      do x <- p_name; eat EQUAL; e <- p_expr; return (x, e)
      <+> do x <- p_name; xs <- p_formals; eat EQUAL; e <- p_expr; return (x, Lambda xs e)
      
    p_type =
      do eat INT; return (BaseType "Int") 
      <+> do eat BOOL; return (BaseType "Bool")
      <+> do eat CHAR; return (BaseType "Char")
      <+> do x <- p_name; return (TVar x)
      <+> do eat BRA; t <- p_type; eat KET; return (TArray t)
      <+> do eat LPAR; ins <- p_list0 p_type COMMA; eat RPAR; eat ARROW; out <- p_type; return (Arrow (TTuple ins) out)

    p_var =
      do n <- p_name; eat AT; t <- p_type; return (n, t)

    p_fsts = 
      do eat LPAR; xs <- p_list0 p_var COMMA; eat RPAR; return (unzip xs)
    
    p_formals = 
      do eat LPAR; xs <- p_list0 p_name COMMA; eat RPAR; return xs

    p_expr = 
      do eat LET; d <- p_def; eat IN; e1 <- p_expr; return (Let d e1)
      <+> do eat LAMBDA; xs <- p_formals; e1 <- p_expr; return (Lambda xs e1)
      <+> p_cond
    
    p_cond = 
      do eat IF; e1 <- p_cond; eat THEN; e2 <- p_cond;
         eat ELSE; e3 <- p_cond; return (If e1 e2 e3)
      <+> p_term5

    p_term5 = p_opchainl p_relop p_term4 
    p_term4 = p_opchainl p_addop p_term3
    p_term3 = p_opchainl p_mulop p_term2
    p_term2 = p_opchainr (p_ident CONSOP) p_term1
    
    p_relop = p_ident RELOP <+> (do eat EQUAL; return "=")
    p_addop = p_ident ADDOP <+> (do eat MINUS; return "-")
    p_mulop = p_ident MULOP <+> (do eat STAR; return "*")
    
    p_opchainl :: Parser t Ident -> Parser t Expr -> Parser t Expr
    p_opchainl p_op p_rand = 
      do e0 <- p_rand; p_tail e0
      where
        p_tail e1 =
          do w <- p_op; e2 <- p_rand; p_tail (Apply (Variable w) [e1, e2])
          <+> return e1
    
    p_opchainr :: Parser t Ident -> Parser t Expr -> Parser t Expr
    p_opchainr =
      p_chainr mkop
      where mkop w e1 e2 = Apply (Variable w) [e1, e2]
    
    p_chainr :: (a -> b -> b -> b) -> 
        Parser t a -> Parser t b -> Parser t b 
    p_chainr mk p_op p_rand =
      do e1 <- p_rand; p_tail e1
      where
        p_tail e1 =
          do w <- p_op; e2 <- p_chainr mk p_op p_rand; 
                        return (mk w e1 e2)
          <+> return e1
    
    p_term1 =
      do w <- p_monop; e <- p_term1; return (Apply (Variable w) [e])
      <+> p_term0
    
    p_monop = p_ident MONOP <+> (do eat MINUS; return "~");
    
    p_term0 =
      do e0 <- p_primary; p_qualifiers e0
      where
        p_qualifiers e1 =
          do aps <- p_actuals; p_qualifiers (Apply e1 aps)
          <+> return e1
    
    p_actuals =
      do eat LPAR; aps <- p_list0 p_expr COMMA; eat RPAR; return aps
    
    p_primary =
      do n <- p_number; return (Number n)
      <+> do x <- p_name; return (Variable x)
      <+> do c <- p_char; return (Character c)
      <+> do eat LPAR; e <- p_expr; eat RPAR; return e
    
    p_base =
      (do e <- p_expr; eat VBAR; return e) <+> return Empty
    
    p_binding =
      do x <- p_name; eat ARROW; e <- p_expr; return (x, e)
    
    p_number =
      do t <- scan; case t of NUMBER n -> return n; _ -> p_fail
    
    p_char =
      do t <- scan; case t of CHARACTER c -> return c; _ -> p_fail
    
    p_name = p_ident ID <+> (do eat LPAR; x <- p_op; eat RPAR; return x)
    
    p_op =
      p_ident MONOP <+> p_addop <+> p_mulop <+> p_relop
    
    p_ident k =
      do t <- scan; case t of IDENT k' x | k == k' -> return x; _ -> p_fail
    
    funParser :: Syntax Token Phrase
    funParser = (lexer, p_phrase)