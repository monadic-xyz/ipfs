{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module Network.IPFS.Git.RemoteHelper.Generic
    ( HKD
    , gvalidate
    , ginvalidate
    )
where

import           Generics.SOP

-- The venerable defunctionalisation, applied generically, with a hipster touch
-- as popularised by Sandy "type fam" Maguire ("Higher Kinded Data", HKD)

-- | Eliminates the boring identity functor 'I'
type family HKD (f :: * -> *) (a :: *) :: * where
    HKD I a = a
    HKD f a = f a

-- | Generically lift the applicative functor 'f' out of a structure.
--
-- Example:
--
-- Given a datatype parametrised over the functor 'Maybe', return 'Just' the
-- value if all 'Maybe' fields are 'Just', thereby changing the functor to 'I'.
-- 'Nothing' otherwise.
--
-- >>> :{
-- >>> data Person f = Person { name :: f String, age :: f Int }
-- >>>     deriving (Show, Generic, GHC.Generic)
-- >>> :}
-- >>> gvalidate $ Person (Just "LeBoeuf") (Just 42)
-- Just (Person (I "LeBoeuf") (I 42))
-- >>> gvalidate $ Person Nothing (Just 69)
-- Nothing
--
gvalidate
    :: ( Generic (a f)
       , Generic (a I)
       , Applicative f
       , AllZip2 (LiftedCoercible I f) (Code (a f)) (Code (a I))
       )
    => a f
    -> f (a I)
gvalidate = (to <$>) . hsequence . hcoerce . from

-- | Generically change the functor of a structure from 'I' to 'f', given a
-- constructor of 'f'.
--
-- Morally the inverse of 'gvalidate', although the specialisation to the
-- identity functor makes this trivial.
--
-- If 'a f' is a 'Semigroup', this can be used to apply defaults to a partial
-- value.
--
-- >>> :{
-- >>> instance Semigroup (Person Last) where
-- >>>     a <> b = Person { name = on (<>) name a b, age = on (<>) age a b }
-- >>> -- For convenience, also define 'Monoid'
-- >>> instance Monoid (Person Last) were
-- >>>     mempty  = Person mempty mempty
-- >>>     mappend = (<>)
-- >>> :}
-- >>> defaultPerson :: Person I
-- >>> defaultPerson = Person "Sandy" 28
-- >>> gvalidate $ ginvalidate pure defaultPerson <> Person (pure "Maguire") mempty
-- Just (Person (I "Maguire") (I 28)
--
ginvalidate
    :: ( Generic (a f)
       , Generic (a I)
       , AllZip2 (LiftedCoercible f I) (Code (a I)) (Code (a f))
       )
    => (forall b. b -> f b)
    -> a I
    -> a f
ginvalidate f = to . hcoerce . hmap (f . unI) . from
