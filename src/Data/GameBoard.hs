{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}


module Data.GameBoard where


import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Lens
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Foldable
import qualified Data.List                   as List
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Safe


------------
-- Types. --
------------

type Measure  = Int
type Position = (Measure, Measure)


data Board α = Board [[α]]

instance Eq α ⇒ Eq (Board α) where
    Board rows₁ == Board rows₂ = rows₁ ≡ rows₂

instance Functor Board where
    fmap f (Board rows) =
        Board $ fmap (fmap f) rows

instance Applicative Board where
    pure x = Board [[x]]
    Board rows₁ <*> Board rows₂ = Board (fmap getZipList zippedRows)
        where
          zippedRows = zipWith (⊛) (fmap ZipList rows₁) (fmap ZipList rows₂)

instance Monoid α ⇒ Monoid (Board α) where
    mempty = Board [[]]
    board₁ `mappend` board₂ = (⊕) ⦷ board₁ ⊛ board₂

instance Foldable Board where
    foldMap f (Board rows) = foldMap (foldMap f) rows

instance Traversable Board where
    traverse fun (Board rows) = Board ⦷ traverse (traverse fun) rows

type instance IxValue (Board α) = α
type instance Index   (Board α) = Position
instance Ixed (Board α) where
    ix (i,j) f (Board rows) =
        Board ⦷ (ix j ∘ ix i) f rows

instance FunctorWithIndex Position Board where
    imap f (Board rows) = Board (zipWith applyRow rows [0..])
        where
          applyRow   r i = zipWith (applyCol i) r [0..]
          applyCol i c j = f (i,j) c

instance FoldableWithIndex Position Board where
    ifoldMap f board = let Board rowsʹ = imap f board
                       in fold (join rowsʹ)

instance TraversableWithIndex Position Board where
    itraverse f board = let Board rowsʹ = imap f board
                        in Board ⦷ sequenceA (fmap sequenceA rowsʹ)

instance Each (Board α) (Board α) α α where
    each f (Board rows) =
        Board ⦷ (each ∘ each) f rows

instance {-# OVERLAPPABLE #-} Show α ⇒ Show (Board α) where
    show (Board rows) = fmap show rows ≫= (⧺"\n")
instance {-# OVERLAPPING #-} Show (Board Char) where
    show (Board rows) = rows ≫= (⧺"\n")
instance {-# OVERLAPPING #-} Show (Board String) where
    show (Board rows) = rows ≫= concat & (⧺ "\n")


----------
-- API. --
----------

-- | Return (width, height).
dimensions ∷ Board α → (Measure, Measure)
dimensions (Board rows) = (length columns, length rows)
    where columns = headDef [] rows


-- | Lens over rows.
row ∷ Int → Lens' (Board α) [α]
row i = lens getRow setRow
    where
      getRow (Board rows)      = rows ^. ix i
      setRow (Board rows) rowʹ = Board (rows & ix i .~ rowʹ)


-- | Lens over columns.
column ∷ Int → Lens' (Board α) [α]
column i = lens getCol setCol
    where
      getCol board = transpose board ^. row i
      setCol board col =
          let boardʹ = transpose board & row i .~ col
          in transpose boardʹ


-- | Lens over diagonals protruding from a position.
diagonals ∷ Position → Lens' (Board α) [α]
diagonals (x,y) = lens getCoordsʹ setCoordsʹ
    where
      getCoordsʹ board = let (w,h) = dimensions board
                         in getCoords board (diagonalsʹ (x,y) w h)
      setCoordsʹ board = let (w,h) = dimensions board
                         in setCoords board (diagonalsʹ (x,y) w h)


-- | Set objects at positions.
setCoords ∷ Board α → [Position] → [α] → Board α
setCoords board positions content =
    foldl' (\b (pos, c) → b& ix pos .~ c) board (zip positions content)


-- | Get objects at positions.
getCoords ∷ Board α → [Position] → [α]
getCoords board positions =
    board ^.. itraversed ∘ ifiltered (\i _ → i∈positions)
    -- Alternatively:
    -- fmap fromJust ∘ filter isJust ∘ fmap (\p → board ^? ix p) $ positions


-- | Flip the board along its side.
transpose ∷ Board α → Board α
transpose (Board rows) = Board (List.transpose rows)


----------------
-- Utilities. --
----------------

diagonalsʹ ∷ Position → Measure → Measure → [Position]
diagonalsʹ (x,y) w h = join (List.transpose positions)
    where
      positions = [zip left up, zip right up,
                    zip left down, zip right down]
      down  = [y+1, y+2 .. h]
      up    = [y-1, y-2 .. 0]
      right = [x+1, x+2 .. w]
      left  = [x-1, x-2 .. 0]
