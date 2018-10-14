
module EGMain where

import           Prelude

data SomeData a b = SomeData a b

myData :: SomeData Int Int
myData = SomeData 1 2

myData1 :: SomeData Int String
myData1 = SomeData  2 "1"

worker1 :: forall a b. SomeData a b -> IO ()
worker1 _ = putStrLn "Hello from Worker 1"

worker2 :: forall a b. SomeData a b -> IO ()
worker2 _ = putStrLn "Hello from Worker 2"

mergeThem :: [IO ()] -> IO ()
mergeThem = foldl (>>) (pure ())

main1 :: IO ()
main1 = mergeThem [
                    worker1 myData,
                    worker1 myData1
                  ]

main2 :: IO ()
main2 = mergeThem [
                    worker2 myData,
                    worker2 myData1
                  ]

-- -- will not compile because list is not of the same type
-- -- [SomeData Int Int, SomeData Int String]
-- main2Dry :: IO ()
-- main2Dry = mergeThem $ worker2 <$> [
--                       EGChild.myData,
--                       EGChild1.myData
--                   ]
-- --
-- -- what I would really like is something like this but this
-- -- would not work for the same reason as above
-- mainShared :: (SomeData a b -> IO ()) -> IO ()
-- mainShared worker = mergeThem $ worker <$> [
--                      EGChild.myData,
--                      EGChild1.myData
--                   ]
--

-- this would be OK too
mainShared :: (forall a b. SomeData a b -> IO ()) -> IO ()
mainShared worker = mergeThem [
                    worker myData,
                    worker myData1
                    ]

demo1 = mainShared worker1
demo2 = mainShared worker2
