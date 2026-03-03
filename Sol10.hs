module Sol10 where

--task 1
encrypt :: FilePath -> FilePath -> IO()
encrypt fin fout = do
    contents <- readFile fin
    let encrypted = encrypt' contents
    writeFile fout encrypted
    where
        encrypt' :: String -> String
        encrypt' str = unwords $ map reverse $ words str

--task 2
wc:: FilePath -> IO()
wc fin = do
    contents <- readFile fin
    let (linesCount,wordsCount,bytesCount) = wc' contents
    putStrLn $ show linesCount ++ " " ++ show wordsCount ++ " " ++ show bytesCount
    where
        wc':: String -> (Int,Int,Int)
        wc' cont = (linesCnt, wordsCnt, bytesCnt)
            where
                wordsCnt = length $ words cont
                linesCnt = length $ lines cont
                bytesCnt = length cont

--task 3
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap :: ( a -> b) ->State s a -> State s b
    fmap f (State fs) =
        State (\s -> let(x,s') = fs s in (f x, s'))

instance Applicative (State s) where
    pure:: a -> State s a
    pure x = State $ \s -> (x,s)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State s1) <*> (State s2) =
        State $ \s ->
            let (f,s') = s1 s
                (x,s'') = s2 s'
            in (f x , s'')

    liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
    liftA2 op (State s1) (State s2) =
        State $ \s -> let(x,s') = s1 s
                         (y,s'') = s2 s'
                    in (x `op` y,s'')

instance Monad (State s) where
    return :: a -> State s a
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State s1) >>= f =
        State $ \s ->
            let (x,s') = s1 s
                State s2 = f x
            in s2 s'

get::State s s
get = State $ \s ->(s,s)

put:: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a 
evalState state initial = fst $ runState state initial
