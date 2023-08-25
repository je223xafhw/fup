import Control.Monad

data Tree a = Node (Tree a) a (Tree a)
            | Empty
     deriving Show

testTree = Node (Node Empty 42 Empty) 73 (Node Empty 55 Empty)

testTree1 = Node testTree 7 testTree

testTree2 = Node testTree1 0 testTree1

--    numberTree testTree
-- -> Node (Node Empty (0,42) Empty) (1,73) (Node Empty (2,55) Empty)


numberTree :: Tree a -> Tree (Int,a)
numberTree t = fst (help t 0)
  where help :: Tree a -> Int -> (Tree (Int,a), Int)
        help Empty          label = (Empty, label)
        help (Node tl x tr) label =
           let (tl',label1) = help tl label
               (tr',label2) = help tr (label1 + 1)
           in
             (Node tl' (label1,x) tr', label2) 

newtype State s a = ST (s -> (a,s))

numberTreeM t = runState 0 (help t)
  where help :: Tree a -> State Int (Tree (Int,a))
        help Empty = return Empty
        help (Node tl x tr) = do
          tl' <- help tl
          n <- get
          put (n+1)
          tr' <- help tr
          return (Node tl' (n,x) tr')


numberTreeA t = runState 0 (help t)
  where help :: Tree a -> State Int (Tree (Int,a))
        help Empty = pure Empty
        --help (Node tl x tr) = 
        --  ((\tl' n tr' -> Node tl' (n,x) tr') <$>
        --  (help tl)) <*> get <*> (help tr)
        -- Aber der Zustand kann hier nicht modifiziert werden.
        -- Würde einer Veränderung von Just nach Nothing wie
        -- bei Maybe bedeuten und das geht nur in der Monade.
        -- Man benötigt also tatsächlich die StateMonade, 
        -- Applicative reicht nicht aus.    

        help (Node tl x tr) = 
           Node <$>
            (help tl) <*>
            (get >>= \n -> put (n+1) >> return (n,x)) <*> 
            (help tr)
   
instance Applicative (State s) where

  pure = return
  
  --(<*>) :: State s (a -> b) -> State s a -> State s b
  ST sf <*> ST sa = ST $ \s ->
     let (f,s1) = sf s
         (x,s2) = sa s1 in
       (f x, s2)
  
instance Functor (State s) where

  fmap f (ST sf) = ST $ \s ->
     let (x,s1) = sf s in
        (f x, s1)
  
instance Monad (State s) where

  -- return :: a -> State s a
  return x = ST (\s -> (x,s))

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  st_sf >>= f = ST $ \s -> 
     let ST sf = st_sf
         (x, s1) = sf s
         ST sg = f x in
       sg s1

get :: State s s
get = ST $ \s -> (s,s)

put :: s -> State s ()
put new_s = ST $ \_ -> ((),new_s) 

runState :: s -> State s a -> a
runState init (ST sf) =
   let (res, s_fin) = sf init in
     res 
