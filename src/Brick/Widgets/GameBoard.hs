{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}


module Brick.Widgets.GameBoard where

import           Brick.Types
import           Control.Applicative.Unicode
import           Control.Arrow.Unicode
import           Control.Lens                hiding (Context)
import           Control.Monad.Writer
import           Data.GameBoard
import           Data.List
import           FRP.Sodium
import           Graphics.Vty                hiding (Event)
import           Prelude.Unicode
import           Prelude.Unicode.SR


type LoggedWidget = Writer String Widget

data BoardEvent = Move Position        -- ^ Move the player
                | Dance Position       -- ^ Move all dancers synchronously.
                deriving (Show, Eq)


-- | An FRP interface to the boardWidget.
gameBoard ∷ Position                          -- ^ Initial Koala position.
          → [Position]                        -- ^ Initial dancer positions.
          → Event BoardEvent                  -- ^ A stream of BoardEvents.
          → Reactive (Behavior LoggedWidget)
gameBoard pos₀ dancers₀ evStream = do

  koalaStream  ← accum pos₀ (fmap koala evStream)
  dancerStream ← makeDancers dancers₀
  let newBoard = updateBoard ⦷ koalaStream ⊛ dancerStream
  return newBoard

      where
        updateBoard ∷ Position → [Position] → LoggedWidget
        updateBoard δplayer δdancers = do logPos δplayer δdancers
                                          boardWidget δplayer δdancers

        koala ∷ BoardEvent → Position → Position
        koala (Move (x,y)) = (+x) ⁂ (+y)
        koala _            = id

        dancer ∷ BoardEvent → Position → Position
        dancer (Dance (x,y)) = (+x) ⁂ (+y)
        dancer _             = id

        makeDancers ∷ [Position] → Reactive (Behavior [Position])
        makeDancers positions = do

          dancers ← mapM (\pos₀ʹ → accum pos₀ʹ (fmap dancer evStream)) positions
          return $ sequenceA dancers

        logPos ∷ Position → [Position] → Writer String ()
        logPos playerPos _dancePos = tell koalaMsg

            where
              koalaMsg = "Koala now at: " ⧺ show playerPos ⧺ "\n"
              -- dancerMsg = "Dancer now at: " ⧺ show dancePos


-- | A Brick widget wrapping a Data.GameBoard.
boardWidget ∷ Position      -- ^ Koala position.
            → [Position]    -- ^ Dancer positions.
            → LoggedWidget
boardWidget pos dancers = return ∘ Widget Fixed Fixed $ do

        context ← getContext
        let w            = availWidth  context
            h            = availHeight context
            board        = emptyBoard w h & putKoala & putDancers
            boardLines   = fmap (take w) ∘ take h ∘ lines ∘ show $ board

            boardStr     = foldl1' vertJoin (string mempty ⦷ boardLines)
            result       = Result boardStr boardCursors visRequests

        return result

    where
      putKoala     = (ix pos .~ koala) ∘ (diagonals pos .~ cycle crosshairs)
      putDancers b = foldl' (\board p → board & ix p .~ dancer) b dancers

      koala        = '🐨'
      dancer       = '🐼'
      crosshairs   = cycle ['a' .. 'z']

      boardCursors = []
      visRequests  = []


emptyBoard ∷ Int → Int → Board Char
emptyBoard w h = fillBoard w h '_'


fillBoard ∷ Int → Int → α → Board α
fillBoard w h x = Board linesʹ

    where
      columns = replicate w x
      linesʹ  = replicate h columns

