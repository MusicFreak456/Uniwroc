data StreamTrans i o a
 = Return a
 | ReadS (Maybe i -> StreamTrans i o a)
 | WriteS o (StreamTrans i o a)

data Queue a 
  = Empty
  | Queue a [a] [a]

qPush :: Queue a -> a -> Queue a 
qPush Empty x = Queue x [] []
qPush (Queue y f r) x = Queue y f (x:r)

qPop :: Queue a -> (Maybe a, Queue a)
qPop Empty = (Nothing, Empty)
qPop (Queue x [] []) = (Just x, Empty)
qPop (Queue x [] r)  = (Just x, Queue y revr []) where (y:revr) = reverse r
qPop (Queue x (y:f) r) = (Just x, Queue y f r)

runCycle :: StreamTrans a a b -> b
runCycle  = 
  runCycleBuff Empty where
    runCycleBuff :: Queue a -> StreamTrans a a b -> b
    runCycleBuff _ (Return x) = x
    runCycleBuff queue (ReadS cont) = 
      runCycleBuff nextQueue (cont nextElem) where
        (nextElem, nextQueue) = qPop queue
    runCycleBuff queue (WriteS x trans) = runCycleBuff (qPush queue x) trans