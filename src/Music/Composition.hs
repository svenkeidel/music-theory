{-# LANgUAGE TypeFamilies #-}
{-# LANgUAGE GADTs #-}
{-# LANgUAGE TypeOperators #-}
module Music.Composition where

import Prelude hiding (id,(.),Functor(..),map)
import Music.Pitch
import Control.Category

type Time = Rational
type Duration = Rational

data Event
  = Note Duration Pitch
  | Rest Duration

instance Eq Event where
  (Rest d1) == (Rest d2) = d1 == d2
  (Note d1 p1) == (Note d2 p2) = d1 == d2 && p1 == p2
  (Note d1 _) == (Rest d2) = d1 == 0 && d2 == 0
  (Rest d1) == (Note d2 _) = d1 == 0 && d2 == 0

instance Ord Event where
  Note d1 p1 <= Note d2 p2 = d1 <= d2 && p1 <= p2
  Rest d1 <= Rest d2 = d1 <= d2
  Rest _ <= Note _ _ = True
  Note d1 _ <= Rest d2 = d1 == 0 && d2 == 0

duration :: Event -> Duration
duration (Note d _) = d
duration (Rest d) = d

data Music :: * -> * where
  -- Two pieces of music that are performed _after_ each other
  (:|:) :: Music a -> Music a -> Music a
  -- Two pieces of music that are performed concurrently
  (:-:) :: Music a -> Music a -> Music a
  -- A music event
  Event :: a -> Music a

denote :: Music Event -> [(Time,Event)]
denote = fst . go 0
  where
    go :: Time -> Music Event -> ([(Time,Event)],Duration)
    go t m = case m of
      (m1 :|: m2) ->
        let (es1,d1) = go t m1
            (es2,d2) = go (t+d1) m2
        in (es1++es2, d1+d2)
      (m1 :-: m2) ->
        let (es1,d1) = go t m1
            (es2,d2) = go t m2
        in (es1++es2, max d1 d2)
      (Event (Note 0 _)) -> ([],0)
      (Event (Rest 0)) -> ([],0)
      (Event e) -> ([(t,e)],duration e)
