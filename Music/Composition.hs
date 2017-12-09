{-# LANgUAGE TypeFamilies #-}
{-# LANgUAGE PolyKinds #-}
{-# LANgUAGE GADTs #-}
{-# LANgUAGE TypeOperators #-}
{-# LANgUAGE LiberalTypeSynonyms #-}
{-# LANgUAGE MultiParamTypeClasses #-}
{-# LANgUAGE FunctionalDependencies #-}
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

data MusicF :: (* -> *) -> (* -> *) where
  -- Two pieces of music that are performed _after_ each other
  (:|:) :: f a -> f a -> MusicF f a
  -- Two pieces of music that are performed concurrently
  (:-:) :: f a -> f a -> MusicF f a
  -- A music event
  Event :: a -> MusicF f a

class Fixpoint fix c | c -> fix, fix -> c where
  fix :: c (f (fix f)) (fix f)
  unfix :: c (fix f) (f (fix f))

class (Category c, Category d) => Functor f c d | f -> c, f -> d where
  map :: c x y -> d (f x) (f y)

fold :: (Functor f c c, Fixpoint fix c) => c (f a) a -> c (fix f) a
fold f = f . map (fold f) . unfix

-- type Music = Fix MusicF

-- data Fix (f :: a -> a) where
--   Fix :: f (Fix f) -> Fix f

-- unfix :: Fix f -> f (Fix f)
-- unfix (Fix f) = f

-- fold :: Functor f => (f a -> a) -> (Fix f -> a)
-- fold f = f . fmap (fold f) . unfix

-- f ~> g = forall x. f x -> g x

-- duration :: (Num a,Ord a) => MusicF f ~> f
-- duration m = case m of
--   m1 :|: m2 -> m1 + m2
  

-- duration :: (Num n,Ord n) => Alg Music n
-- duration = Alg {sequential = (+), parallel = max, event = id}

-- denote :: Music -> [(Time,Event)]
-- denote = fst . go 0
--   where
--     -- c Music Duration -> c ((Time,Music),Duration) [(Time,Event)] -> c (Time,Music) [(Time,Event)]
--     -- c Music y -> c (Music,y) z -> c Music z

--     traverse :: (Music -> y) -> (Music -> y -> z) -> (Music -> z) 
--     traverse f g (m1 :|: m2) = _

--     go :: Time -> Music -> ([(Time,Event)],Duration)
--     go t m = case m of
--       (m1 :|: m2) ->
--         let (es1,d1) = go t m1
--             (es2,d2) = go (t+d1) m2
--         in (es1++es2, d1+d2)
--       (m1 :-: m2) ->
--         let (es1,d1) = go t m1
--             (es2,d2) = go t m2
--         in (es1++es2, max d1 d2)
--       (Event (Note 0 _)) -> ([],0)
--       (Event (Rest 0)) -> ([],0)
--       (Event e) -> ([(t,e)],duration (Event e))
