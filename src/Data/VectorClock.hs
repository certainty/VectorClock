{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Data.VectorClock
  (VectorClock,
   empty,
   toList,
   increment,
   descends,
   dominates,
   (==>),
   concurrent,
   add,
   merge,
   fromList)
where
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Binary (Binary(..))
import qualified Data.Map.Strict as Map

-- | Implementation of a VectorClock
-- See: http://basho.com/posts/technical/why-vector-clocks-are-easy/
--      http://basho.com/posts/technical/why-vector-clocks-are-hard/
--      http://basho.com/posts/technical/vector-clocks-revisited/

type ClockValue = Integer

newtype VectorClock actor = VectorClock {
  _clocks :: Map.Map actor ClockValue
} deriving (Eq, Show, Data, Typeable, Generic)

instance (Binary a, Ord a) => Binary (VectorClock a) where
    put = put . toList
    get = fmap fromList get

empty :: (Ord actor) => VectorClock actor
empty = VectorClock Map.empty

toList :: (Ord actor) => VectorClock actor -> [(actor, ClockValue)]
toList = Map.toList . _clocks

fromList :: (Ord actor) => [(actor, ClockValue)] -> VectorClock actor
fromList = VectorClock . Map.fromList

add :: (Ord actor) => ClockValue -> actor -> VectorClock actor -> VectorClock actor
add n actor vclock = vclock { _clocks = clocks' }
  where
    clocks'        = Map.insertWith adder actor n (_clocks vclock)
    adder _new old = n + old

increment :: (Ord actor) => actor -> VectorClock actor -> VectorClock actor
increment = add 1

-- | To merge two clocks we construct a new clock by computing the actor-wise maximum.
--   If one actor isn't present in one clock it is simply assumed to be 0
merge :: (Ord actor) => VectorClock actor -> VectorClock actor -> VectorClock actor
merge vcA vcB = VectorClock clocksAB
  where
    clocksAB = Map.unionWith max (_clocks vcA) (_clocks vcB)

-- | The clock B descends A if all values in A have a greater or equal value in B
--
-- Example:
--   a = empty
--   b = fromList [(42, 3), (10, 20)]
--   c = merge a b
--
--   b `descends` a == True
--   (c `descends` a && c `descends` b) == True
-- |
descends :: (Ord actor) => VectorClock actor -> VectorClock actor -> Bool
descends vcB = fold folder True
  where
    clocksB                   = _clocks vcB
    folder False _ _          = False
    folder True actorA valueA = Map.findWithDefault 0 actorA clocksB >= valueA

-- | The clock B dominates A if B descends A and there exists one value
--   in B that is strictly greater than in A
--
-- Example:
--  a = empty
--  b = fromList [(42, 3), (10, 20)]
--  c = increment b 42
--  d = c
--
--  b `dominates` a == True
--  c `dominates` b == True
--  d `dominates` c == False

dominates :: (Ord actor) => VectorClock actor -> VectorClock actor -> Bool
dominates vcB vcA = descends vcB vcA && existsGreaterInB
  where
    existsGreaterInB           = fold folder False vcB
    clocksA                    = _clocks vcA
    folder True _ _            = True
    folder False actorB valueB = valueB > Map.findWithDefault 0 actorB clocksA

fold :: (Ord actor) => (b -> actor -> ClockValue -> b) -> b -> VectorClock actor -> b
fold f initial = Map.foldlWithKey' f initial . _clocks

-- | A ==> B means B is caused by A
--   B is caused by A if B dominates A

(==>) :: (Ord actor) => VectorClock actor -> VectorClock actor -> Bool
(==>) = flip dominates

-- | tow actors are concurrent if neither dominates the other
concurrent :: (Ord actor) => VectorClock actor -> VectorClock actor -> Bool
concurrent vcA vcB = not (vcA ==> vcB || vcB ==> vcA)
