{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.IntMap as M
import System.IO
import Text.Printf
import Text.Show

type Id = Int
type Names = IntMap String
type Env = (Id, Names)

type MonadLambda m =
  ( MonadState Env m
  , Applicative m
  )

runLambda :: State Env a -> (a, Names)
runLambda p = f $ runState p (1, M.empty)
  where f (result, (_, names)) = (result, names)

gensym :: MonadLambda m => m Id
gensym = do
  (i,m) <- get
  put (i+1,m)
  return i

gensym' :: MonadLambda m => String -> m Id
gensym' s = do
  (i,m) <- get
  put (i+1, insert i s m)
  return i

data Expr
  = Lam Id Expr
  | App Id Expr Expr
  | Var Id
    deriving Show

var :: MonadLambda m => Id -> m Expr
var = return . Var

app :: MonadLambda m => m Expr -> m Expr -> m Expr
app e1 e2 = App <$> gensym <*> e1 <*> e2

lam :: MonadLambda m => String -> (m Expr -> m Expr) -> m Expr
lam name body = do
  x <- gensym' name
  Lam x <$> body (var x)

church :: MonadLambda m => Int -> m Expr
church n | n >= 0 =
  lam "f" $ \f -> lam "x" $ \x -> iter n (app f) x
  where
    iter :: Int -> (m Expr -> m Expr) -> m Expr -> m Expr
    iter 0 _ x = x
    iter i f x = iter (i-1) f (f x)

add :: MonadLambda m => m Expr
add =
  lam "m" $ \m ->
  lam "n" $ \n ->
  lam "f" $ \f ->
  lam "x" $ \x ->
  app (app m f) (app (app n f) x)

shape x = ("shape", x)
style x = ("style", x)
color x = ("color", x)
fillcolor x = ("fillcolor", x)
fontcolor x = ("fontcolor", x)
fixedsize = ("fixedsize", "true")
width x = ("width", show x)
height x = ("height", show x)
label x = ("label", x)
dir x = ("dir", x)
arrowtail x = ("arrowtail", x)

lamColor = "#aa0000"

lamAttrs =
  [ shape "diamond"
  , style "filled"
  , color lamColor
  , fillcolor "#ffaaaa"
  ]

lamVarEdgeAttrs =
  [ color lamColor
  , style "bold"
  , dir "both"
  , arrowtail "box"
  ]

varAttrs =
  [ shape "box"
  , fixedsize
  , width 0.3
  , height 0.3
  , style "filled"
  , color "#0000aa"
  , fillcolor "#ccccff"
  ]

appColor = "#00aa00"

appAttrs =
  [ shape "circle"
  , fontcolor appColor
  , fixedsize
  , width 0.25
  , height 0.25
  , style "filled"
  , color appColor
  , fillcolor "#aaffaa"
  ]

appEdge1Attrs =
  [ color appColor
  , style "bold"
  , dir "both"
  , arrowtail "box"
  ]

edgeAttrs =
  [ color "#555577"
  , style "bold"
  ]

showAttr :: (String, String) -> String -> String
showAttr (k, v) = ((k ++ "=\"" ++ v ++ "\"") ++)

showAttrs as = showListWith showAttr as ";"


type MonadDot m =
  ( MonadReader Names m
  , MonadWriter [String] m
  )

runDot :: Names -> WriterT [String] (Reader Names) () -> Handle -> IO ()
runDot names p h = do
  hPutStrLn h "digraph {"
  hPutStrLn h "node [fontname=\"Lora-Bold\"];"
  hPutStr h $ unlines $ runReader (execWriterT p) names
  hPutStrLn h "}"

nodeName :: Expr -> String
nodeName (Lam i _) = "lam" ++ show i
nodeName (App i _ _) = "app" ++ show i
nodeName (Var i) = "var" ++ show i

withLabel i s attrs = do
  names <- ask
  return $ case M.lookup i names of
   Nothing -> label s : attrs
   Just name -> label (s++name) : attrs

edge e1 e2 attrs =
  nodeName e1 ++ " -> " ++ nodeName e2 ++ " " ++ showAttrs attrs

redex (Lam _ _) = "@"
redex _ = ""

dot :: MonadDot m => Expr -> m ()
dot e = case e of
  Var i -> return ()

  Lam i e1 -> do
    la <- withLabel i "λ" lamAttrs
    va <- withLabel i "" varAttrs
    tell [ nodeName e ++ " " ++ showAttrs la
         , nodeName (Var i) ++ " " ++ showAttrs va
         , edge e (Var i) lamVarEdgeAttrs
         , edge e e1 edgeAttrs
         ]
    dot e1

  App i e1 e2 -> do
    aa <- withLabel i (redex e1) appAttrs
    tell [ nodeName e ++ " " ++ showAttrs aa
         , edge e e1 appEdge1Attrs
         , edge e e2 edgeAttrs
         ]
    dot e1
    dot e2

reduce :: Expr -> Maybe Expr
reduce (App _ (Lam x b) e) = Just $ subst x e b
reduce (App i e1 e2) = do
  case reduce e1 of
   Nothing -> App i e1 <$> reduce e2
   Just e1' -> Just $ App i e1' e2
reduce (Lam x b) = Lam x <$> reduce b
reduce (Var _) = Nothing

subst :: Id -> Expr -> Expr -> Expr
subst i e (Var j) | i == j = e
                  | otherwise = Var j
subst i e (Lam x b) = Lam x (subst i e b)
subst i e (App j e1 e2) = App j (subst i e e1) (subst i e e2)

loop :: Int -> Names -> Maybe Expr -> IO ()
loop i _ Nothing = putStrLn "Done"
loop i d (Just e) = do
  let fn = "hgen" ++ printf "%02d" i ++ ".dot"
  putStrLn $ "Writing " ++ fn
  withFile fn WriteMode $ runDot d $ dot e
  loop (i+1) d (reduce e)

main =
  loop 1 d (Just c3)
  where (c3, d) = runLambda $ app (app add $ church 3) (app (app add (church 2)) (church 2))
