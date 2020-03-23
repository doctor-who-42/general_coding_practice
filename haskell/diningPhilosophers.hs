{- |
Transcription of the dining philosopher's problem in concurrent programming
Inspired by [S. Hym's Lectures on Functional Programming](https://www.fil.univ-lille1.fr/portail/index.php?dipl=L&sem=S6&ue=PF&label=Pr%C3%A9sentation)
-}

module Main where

    import Control.Monad (forever, replicateM)
    import Control.Concurrent (forkIO, threadDelay)
    import Control.Concurrent.STM (TMVar, atomically, takeTMVar, putTMVar, newTMVar)
    import System.Random (randomRIO)
    import System.IO (hFlush, stdout)

    main = do
            theChopsticks <- atomically $ replicateM nbPhilosophers $ newTMVar ()
            mapM_ (forkIO . createPhilosopher theChopsticks) [1 .. nbPhilosophers-1]
            createPhilosopher theChopsticks 0
        where 
            nbPhilosophers          = 5
            nthChopstick cs n       = cs !! (n `mod` nbPhilosophers)
            createPhilosopher cs n  = dine n (nthChopstick cs n) (nthChopstick cs (n+1))

    type Chopstick = TMVar ()

    -- a philosopher takes its right and left chopsticks
    takeChopsticks :: Chopstick -> Chopstick -> IO ()
    takeChopsticks r l = atomically $ do
        takeTMVar r --takes right chopstick
        takeTMVar l --takes left chopstick
    
    -- a philosopher puts down its right and left chopstick
    returnChopsticks :: Chopstick -> Chopstick -> IO ()
    returnChopsticks r l = atomically $ do
        putTMVar r () --returns right chopstick
        putTMVar l () --returns left chopstick
    
    -- a philosopher waits some random time before moving on to next action
    waits :: IO ()
    waits = threadDelay =<< randomRIO (1000000, 100000)

    displayMsg :: Int -> String -> IO ()
    displayMsg n msg = do
        putStrLn $ "Philosopher " ++ show n ++ " " ++ msg
        hFlush stdout
    
    --dinner organisation
    dine :: Int -> Chopstick -> Chopstick -> IO ()
    dine n r l = forever $ do
            displayMsg n "thinks."
            waits
            displayMsg n "is hungry."
            takeChopsticks r l
            displayMsg n "eats."
            waits
            displayMsg n "is full."
            returnChopsticks r l