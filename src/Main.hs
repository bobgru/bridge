{-# LANGUAGE RecordWildCards #-}

module Main where
import Data.Maybe

main :: IO ()
main = do
  putStrLn "hello world"

data Suit
  = Club
  | Diamond 
  | Heart 
  | Spade 
  deriving (Enum, Ord, Eq, Show)

data Number
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Enum, Ord, Eq, Show)
  
data Card = Card { cSuit :: Suit, cNumber :: Number }
  deriving (Ord, Eq, Show)

rank :: Card -> Int
rank (Card s n) = 13 * fromEnum s + fromEnum n

honor :: Card -> Bool
honor (Card _ n) = n >= Ten

-- | Determine whether one card ranks higher than another
-- given the suit of the opening lead and the possible trump suit.
capture :: Card -> Card -> Suit -> Maybe Suit -> Bool
capture (Card s1 n1) (Card s2 n2) lead trump =
  if s1 == s2
  then n1 > n2
  else if isJust trump && s1 == fromJust trump
       then True
       else if isJust trump && s2 == fromJust trump
         then False
         else if s1 == lead
              then True
              else if s2 == lead
                   then False
                   else True  -- irrelevant case

data Player
  = North 
  | East 
  | South 
  | West
  deriving (Enum, Ord, Eq, Show)

partner :: Player -> Player
partner North = South
partner South = North
partner East  = West
partner West  = East

nextPlayer :: Player -> Player
nextPlayer North = East
nextPlayer East  = South
nextPlayer South = West
nextPlayer West  = North

-- | For accumulating the trick during a round of play,
-- and for saving the trick for later scoring.
data Trick = Trick
  { tNorth :: Maybe Card
  , tEast  :: Maybe Card
  , tSouth :: Maybe Card
  , tWest  :: Maybe Card
  , tLead  :: Maybe Player
  , tWinner :: Maybe Player }
  deriving (Eq, Show)

-- | State of trick at beginning of a round of play.
emptyTrick :: Trick
emptyTrick = Trick Nothing Nothing Nothing Nothing Nothing Nothing

-- | State of trick at end of a round of play, not considering
-- whether the winning card has been determined.
completeTrick :: Trick -> Bool
completeTrick Trick{..} =
  (== 4) . length $ catMaybes [tNorth, tEast, tSouth, tWest]

isPlayerTrick :: Player -> Trick -> Bool
isPlayerTrick p t
  | not (completeTrick t) = False
  | otherwise = w == p || w == partner p
    where w = fromJust (tWinner t)

addCardToTrick :: Player -> Card -> Trick -> Trick
addCardToTrick p c t = t''
  where
    t' = case p of
          North -> t { tNorth = Just c }
          East  -> t { tEast  = Just c }
          South -> t { tSouth = Just c }
          West  -> t { tWest  = Just c }
    t'' = if isNothing (tLead t) then t' { tLead = Just p } else t'

getSuitOfOpeningLead :: Trick -> Maybe Suit
getSuitOfOpeningLead t@Trick{..}
  | isNothing tLead = Nothing
  | otherwise       = fmap cSuit $ getPlayerCardInTrick (fromJust tLead) t

getPlayerCardInTrick :: Player -> Trick -> Maybe Card
getPlayerCardInTrick p Trick{..} =
  case p of
    North -> tNorth
    East  -> tEast
    South -> tSouth
    West  -> tWest

-- A Deck is a pile of cards, not necessarily all 52.
data Deck = Deck {dkCards :: [Card]} deriving (Eq, Show)

emptyDeck :: Deck
emptyDeck = Deck []

sortedDeck :: Deck
sortedDeck = Deck cs
  where cs = [ Card s n | s <- [Club .. Spade], n <- [Two .. Ace]]

shuffle :: (Monad m) => Deck -> m Deck
shuffle = undefined

cut :: (Monad m) => Deck -> m (Deck, Deck)
cut = undefined

removeCardFromDeck :: Card -> Deck -> Deck
removeCardFromDeck c (Deck cs) = Deck $ filter (/= c) cs

-- | A Deal is the state of cards held by all players.
-- The player hold fewer cards as the game progresses,
-- and have no cards after the last trick is taken.
data Deal = Deal
  { dlNorth :: Deck
  , dlEast  :: Deck
  , dlSouth :: Deck
  , dlWest  :: Deck
  } deriving (Eq, Show)

-- | State of deal in which players have no cards, either
-- before play has started or after the last card has been played.
emptyHands :: Deal
emptyHands = (Deal emptyDeck emptyDeck emptyDeck emptyDeck)

-- | Distribute all cards in a deck to players.
deal :: Deck -> Deal
deal dk = deal' dk emptyHands
  where deal' :: Deck -> Deal -> Deal
        deal' (Deck []) dl = dl
        deal' (Deck (c1:c2:c3:c4:cs)) dl = deal' dk' dl'
          where
            dk' = Deck cs
            dl' = dl
              { dlNorth = Deck (c1 : dkCards (dlNorth dl))
              , dlEast  = Deck (c2 : dkCards (dlEast  dl))
              , dlSouth = Deck (c3 : dkCards (dlSouth dl))
              , dlWest  = Deck (c4 : dkCards (dlWest  dl))
              }
        deal' dk _ = error $ "Invalid deck: " ++ show dk

getPlayerHand :: Player -> Deal -> Deck
getPlayerHand North = dlNorth
getPlayerHand East  = dlEast
getPlayerHand South = dlSouth
getPlayerHand West  = dlWest

setPlayerHand :: Player -> Deal -> Deck -> Deal
setPlayerHand North dl dk = dl { dlNorth = dk }
setPlayerHand East  dl dk = dl { dlEast  = dk }
setPlayerHand South dl dk = dl { dlSouth = dk }
setPlayerHand West  dl dk = dl { dlWest  = dk }

data Bid = Bid
  { bNumber :: Int          -- tricks to win
  , bSuit  :: Suit          -- TODO how is this to be used?
  , bTrump :: Maybe Suit    -- trump suit, if any
  , bDoubled :: Maybe Int   -- number times doubled, if any
  } deriving Show

markTrickWinner :: Maybe Suit -> Trick -> Trick
markTrickWinner trump t@Trick{..}
  | isNothing tLead = error $ "Invalid trick -- no opening lead"
  | otherwise = t { tWinner = Just p }
    where
      lead = cSuit . fromJust $ getPlayerCardInTrick (fromJust tLead) t

      n =  capture (fromJust tNorth) (fromJust tEast)  lead trump
        && capture (fromJust tNorth) (fromJust tSouth) lead trump
        && capture (fromJust tNorth) (fromJust tWest)  lead trump
    
      e =  capture (fromJust tEast)  (fromJust tNorth) lead trump
        && capture (fromJust tEast)  (fromJust tSouth) lead trump
        && capture (fromJust tEast)  (fromJust tWest)  lead trump
    
      s =  capture (fromJust tSouth) (fromJust tEast)  lead trump
        && capture (fromJust tSouth) (fromJust tNorth) lead trump
        && capture (fromJust tSouth) (fromJust tWest)  lead trump
    
      w =  capture (fromJust tWest)  (fromJust tNorth) lead trump
        && capture (fromJust tWest)  (fromJust tSouth) lead trump
        && capture (fromJust tWest)  (fromJust tEast)  lead trump

      p = if n then North else if e then East else if s then South else West

-- | Game state.
data Game = Game
  { gWinningBid   :: Bid
  , gDeclarer     :: Player
  , gNextPlayer   :: Player
  , gDeal         :: Deal
  , gTricks       :: [Trick]
  , gCurrentTrick :: Trick
  } deriving Show

-- | State of a game after the auction but before play has begun.
initialGame :: Bid -> Player -> Deal -> Game
initialGame b p d = Game b p (nextPlayer p) d [] emptyTrick

-- | State of a game after all cards have been played.
gameOver :: Game -> Bool
gameOver g = gDeal g == emptyHands 

getPlayerHandFromGame :: Player -> Game -> Deck
getPlayerHandFromGame p Game{..} = getPlayerHand p gDeal

validCardToPlay :: Player -> Card -> Game -> Bool
validCardToPlay p c g =
  -- player has that card
  c `elem` h

  &&

  (
    -- player is making opening lead
    t == emptyTrick

    -- or player is following suit
    || s == cSuit c

    -- or player has no cards of opening lead suit
    || all ((s /=) . cSuit) h
  )

  where 
    h = dkCards (getPlayerHandFromGame p g)
    t = gCurrentTrick g
    s = fromJust (getSuitOfOpeningLead t)


-- | Play a card on behalf of the next player, and
-- advance the game state.
play :: Card -> Game -> Game
play c g
  | gameOver g =
      error $ "All cards have been played"
  | not (validCardToPlay (gNextPlayer g) c g) =
      error $ "Invalid card to play"
  | otherwise  = g { gNextPlayer   = p'
                   , gDeal         = d'
                   , gTricks       = ts'
                   , gCurrentTrick = t''
                   }
    where 
      p  = gNextPlayer g
      d  = gDeal g
      t  = gCurrentTrick g
      b  = gWinningBid g
      ts = gTricks g
      h  = getPlayerHand p d
      tr = bTrump (gWinningBid g)

      h' = removeCardFromDeck c h
      d' = setPlayerHand p d h'
      p' = nextPlayer p
      t' = addCardToTrick p c t

      (t'', ts') =
        if completeTrick t'
        then let tw = markTrickWinner tr t' in (tw, tw:ts)
        else (t', ts)


getPlayerTricks :: Player -> Game -> [Trick]
getPlayerTricks p Game{..} = filter (isPlayerTrick p) ts
  where
    ts = filter completeTrick gTricks


