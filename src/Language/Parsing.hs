{-# LANGUAGE Rank2Types #-}

module Language.Parsing(nextch, switch, star, incln, make_kwlookup,
    scan, eat, (<+>), p_fail, p_list0, p_list,
    print_value, print_defn, primwrap,
    dialog, dialogm, printStrLn, showlist, joinwith,
  Parser, Syntax) where

import Language.Environment
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Exception
import qualified Control.Concurrent
import qualified System.Environment
import System.IO(stdout, stderr, hPutStrLn, hFlush)
import System.IO.Unsafe(unsafePerformIO)
import GHC.ConsoleHandler(installHandler, Handler(Catch))
import Control.Applicative(Applicative(..))
import Control.Monad(liftM, ap)
    
type LineNumber = Int

data LexState = MkLexState String LineNumber

data LexTok t = MkLexTok t LineNumber

token_of (MkLexTok t n) = t

newtype Lexer t = MkLexer (LexState -> Maybe (LexTok t, LexState))

runLexer (MkLexer g) st = g st

instance Monad Lexer where
  return x = 
    MkLexer (\ (MkLexState input lnum) -> 
      Just (MkLexTok x lnum, MkLexState input lnum))

  m >>= f = 
    MkLexer (\ st -> 
      case runLexer m st of 
        Just (x, st') -> runLexer (f (token_of x)) st'
        Nothing -> Nothing)

peekch :: Lexer Char
peekch = MkLexer g 
  where 
    g (MkLexState (c:s) n) = Just (MkLexTok c n, MkLexState (c:s) n)
    g (MkLexState [] n) = Just (MkLexTok '\0' n, MkLexState [] n)

nextch :: Lexer Char
nextch = MkLexer g 
  where 
    g (MkLexState (c:s) n) = Just (MkLexTok c n, MkLexState s n)
    g (MkLexState [] n) = Nothing

incln :: Lexer ()
incln = 
  MkLexer (\ (MkLexState s n) -> 
    Just (MkLexTok () n, MkLexState s (n+1)))

star :: (Char -> Bool) -> Lexer String
star p = 
  do 
    c <- peekch
    if p c then do nextch; s <- star p; return (c:s) else return []

switch :: [(Char, Lexer t)] -> Lexer t -> Lexer t
switch arms deflt =
  do 
    c <- peekch
    case List.lookup c arms of 
      Just t -> do nextch; t
      Nothing -> deflt

lex_string :: Lexer t -> LineNumber -> String -> [LexTok t]
lex_string lexer n s = loop (MkLexState s n)
  where
    loop st =
      case runLexer lexer st of
        Just (x, st') -> x : loop st'
        Nothing -> []

make_kwlookup :: (String -> t) -> [(String, t)] -> String -> t
make_kwlookup deflt table =
  (\ s -> case Map.lookup s kwtable of Just k -> k; Nothing -> deflt s)
  where kwtable = Map.fromList table

data LexBuf t = MkLexBuf Int [LexTok t] [LexTok t] Int

initbuf :: [LexTok t] -> LexBuf t
initbuf toks = MkLexBuf 0 [] toks 0

bufpos :: LexBuf t -> Int
bufpos (MkLexBuf n _ _ _) = n

emptyb :: LexBuf t -> Bool
emptyb (MkLexBuf _ _ rr _) = null rr

addtoks :: LexBuf t -> [LexTok t] -> LexBuf t
addtoks (MkLexBuf n ll rr nmax) toks =
  MkLexBuf n ll (rr ++ toks) nmax

peekb :: LexBuf t -> LexTok t
peekb (MkLexBuf _ _ (x:rr)  _) = x

forward :: LexBuf t -> LexBuf t
forward (MkLexBuf n ll (x:rr) nmax) =
  MkLexBuf (n+1) (x:ll) rr (max nmax (n+1))

gettok :: LexBuf t -> Int -> LexTok t
gettok (MkLexBuf n ll rr  _) k =
  if k < n then ll!!(n-k-1) else rr!!(k-n)

rewind :: Int -> LexBuf t -> LexBuf t
rewind n0 (MkLexBuf n ll rr nmax) =
  MkLexBuf n0 (drop (n-n0) ll) (reverse (take (n-n0) ll) ++ rr) nmax

commit :: LexBuf t -> LexBuf t
commit (MkLexBuf n ll rr nmax) = MkLexBuf 0 [] rr 0

errtok :: LexBuf t -> Int
errtok (MkLexBuf _ _ _ nmax) = nmax-1

data Parser t a = 
  MkParser (forall w . 
    LexBuf t -> SCont t w a -> FCont t w -> Result t w)

data Result t w = 
    Success w (LexBuf t)
  | Failure (LexBuf t)
  | More ([LexTok t] -> Result t w)

type SCont t w a = a -> LexBuf t -> Result t w

type FCont t w = LexBuf t -> Result t w

callParser (MkParser f) = f

runParser :: Parser t a -> LexBuf t -> Result t a
runParser p buf = callParser p buf Success Failure

instance Monad (Parser t) where
  return x = 
    MkParser (\ buf ks kf -> ks x buf)

  p >>= f = 
    MkParser (\ buf ks kf ->
      callParser p buf (\ x buf' ->
        callParser (f x) buf' ks kf) kf)

p_fail :: Parser t a
p_fail = MkParser (\ buf ks kf -> kf buf)

scan :: Parser t t
scan = 
  MkParser (\ buf ks kf ->
    if emptyb buf then 
      More (\ toks -> callParser scan (addtoks buf toks) ks kf)
    else 
      ks (token_of (peekb buf)) (forward buf))

infixr 5 <+>

(<+>) :: Parser t a -> Parser t a -> Parser t a
p <+> q =
  MkParser (\ buf ks kf -> 
    let n = bufpos buf in
    callParser p buf ks
      (\ buf' -> callParser q (rewind n buf') ks kf))

eat :: Eq t => t -> Parser t t
eat x = do y <- scan; if x == y then return y else p_fail

p_seq :: Parser t a -> Parser t [a]
p_seq p =
  (do x <- p; xs <- p_seq p; return (x:xs)) <+> return []

p_list :: Eq t => Parser t a -> t -> Parser t [a]
p_list p sep =
  do x <- p; xs <- p_seq (do eat sep; p); return (x:xs)

p_list0 :: Eq t => Parser t a -> t -> Parser t [a]
p_list0 p sep = p_list p sep <+> return []

type Syntax t a = (Lexer t, Parser t a)

dialog :: (Eq t, Show t) => 
  Syntax t a -> (a -> s -> (String, s)) -> s -> IO ()
dialog syntax obey init = dialogm syntax obey' init
  where 
    obey' e s = 
      let (out, s') = obey e s in do printStrLn out; return s'

dialogm :: (Eq t, Show t) => 
    Syntax t a -> (a -> s -> IO s) -> s -> IO ()
dialogm syntax obey init =
  do 
    args <- System.Environment.getArgs
    arg_loop init args
  where
    arg_loop st ["-e", exp] = 
      do st' <- load_file syntax obey st exp; return ()
    arg_loop st (f : args) = 
      do text <- readFile f; 
         st' <- load_file syntax obey st text; arg_loop st' args
    arg_loop st [] =
      do read_eval_print syntax obey st; putStrLn "\nBye"

read_eval_print :: (Show t) => 
    Syntax t a -> (a -> s -> IO s) -> s -> IO ()
read_eval_print (lexer, parser) obey st0 = listen st0
  where
    listen st = 
      let parse = runParser parser (initbuf []) in
      do 
        trap_interrupts; 
        stz <- execute (loop 1 parse) st (Just st)
        case stz of
          Just st' -> listen st'; Nothing -> return ()  

    loop n result st =
      case result of
          Success x _ ->
        -- Recognized a phrase: execute it
            do st' <- obey x st; return (Just st')
          Failure buf' ->
        -- A syntax error was found: print it and start again
            do hPutStrLn stderr (syntax_error buf'); return (Just st)
          More k ->
            do
              line <- readline (if n == 1 then ">>> " else "... ")
              case line of
                Nothing -> 
                  -- We got EOF at the prompt
                  return Nothing
                Just s ->
                  -- We got a line: resume parsing
                  loop (n+1) (k (lex_string lexer n s)) st

load_file :: (Show t) => 
    Syntax t a -> (a -> s -> IO s) -> s -> String -> IO s
load_file (lexer, parser) obey st0 text =
  loop (initbuf (lex_string lexer 1 text)) st0
  where
    loop buf st =
      if emptyb buf then
        -- End of file
          return st 
      else
        case runParser parser buf of
          Success x buf' ->
            do st' <- execute (obey x) st st; loop (commit buf') st'
          Failure buf' ->
            do hPutStrLn stderr (syntax_error buf'); return st
          More _ ->
            do hPutStrLn stderr "Unexpected end of file"; return st

syntax_error :: Show t => LexBuf t -> String
syntax_error buf =
  let MkLexTok tok lnum = gettok buf (errtok buf) in
  "Syntax error on line " ++ show lnum ++ " at token '" ++ show tok ++ "'"

execute :: (a -> IO b) -> a -> b -> IO b
execute g x dflt =
  Control.Exception.catch (g x) (\ exc -> 
    let ex = show (exc :: Control.Exception.SomeException) in
    do hPutStrLn stderr ("Error: " ++ ex); return dflt)

trap_interrupts :: IO ()
trap_interrupts =
  do
    thr <- Control.Concurrent.myThreadId; --semi-colon important here
    GHC.ConsoleHandler.installHandler
      (Catch (\ e -> Control.Exception.throwTo thr 
      (Control.Exception.ErrorCall "interrupted")))
    return ()

readline :: String -> IO (Maybe String)
readline prompt =
  do 
    putStr prompt; hFlush stdout
    Control.Exception.catch 
      (do s <- getLine; return (Just (s ++ "\n"))) 
      (\ exc -> 
        let ex = exc :: Control.Exception.IOException in return Nothing)

print_value :: Show a => a -> String
print_value v = (\ v -> "--> " ++ show v) $! v

print_defn :: Show d => Environment d -> String -> String
print_defn env x = 
  (\ v -> "First order function -- " ++ x ++ " -- added to BK ") $! (find env x)

printStrLn :: String -> IO ()
printStrLn "NoPrint" = putStr ""
printStrLn "" = putChar '\n'
printStrLn (c:s) = do putChar c; hFlush stdout; printStrLn s

primwrap :: Show a => String -> ([a] -> b) -> [a] -> b
primwrap prim f args =
  unsafePerformIO 
    (Control.Exception.catch (return $! f args) match_fail)
  where 
    match_fail :: Control.Exception.PatternMatchFail -> b
    match_fail e = 
      error ("bad arguments to primitive " ++ prim ++ ": " 
                        ++ showlist args)

showlist :: Show a => [a] -> String
showlist = joinwith ", " . map show

joinwith :: String -> [String] -> String
joinwith sep = concat . List.intersperse sep


instance Functor Lexer where fmap = liftM
instance Applicative Lexer where pure = return; (<*>) = ap
instance Functor (Parser t) where fmap = liftM
instance Applicative (Parser t) where pure = return; (<*>) = ap