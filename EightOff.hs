module EightOff where
  import System.Random
  import Data.List
  import Data.Maybe

  data Suit = Heart | Diamond | Spades | Clubs deriving (Eq, Enum, Show)
  data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | King | Queen deriving (Eq, Enum, Show)
  type Card = (Suit,Rank)
  type Deck = [Card]

  type Foundations = [Deck]
  type Tableau = [Deck]
  type Cells = [Card]

  type EOBoard = (Foundations,Tableau,Cells)

  -- data Foundation = Card (Foundation)

  -- newDeck :: [Deck]
  -- newDeck = map (\suit -> rank -> (suit,rank)) [Heart .. Clubs] [Ace .. Queen]
  -- newDeck = zip [Heart .. Clubs] [Ace .. Queen]

  randSeed :: Int
  randSeed = 13

  pack :: Deck
  pack = [(suit,rank) | suit <- [Heart .. ], rank <- [Ace .. ]]

  packCount :: Int
  packCount = length pack

  pipValue :: Card -> Int
  pipValue (suit,rank) = fromEnum rank

  sCard :: Card -> Card
  sCard (suit,Queen) = (suit,Ace)
  sCard (suit,rank) = (suit,succ rank)

  pCard :: Card -> Card
  pCard (suit,Ace) = (suit,Queen)
  pCard (suit,rank) = (suit, pred rank)

  isAce :: Card -> Bool
  isAce (suit,rank) = rank == Ace

  isKing :: Card -> Bool
  isKing (suit,rank) = rank == King

  shuffle :: Deck -> Deck
  -- zip the given deck to a random set of numbers, sort this by those numbers and then unzip using map
  shuffle deck = map fst (sortBy sortComparator (zip deck (take packCount (randoms (mkStdGen randSeed):: [Int]))))

  sortComparator :: (Card,Int) -> (Card,Int) -> Ordering
  sortComparator ((a,b),c) ((x,y),z)
    | c > z = GT
    | c < z = LT
    | otherwise = EQ

  makeCells :: Deck -> Cells
  makeCells deck = drop 48 deck

  makeTableau :: Deck -> Tableau
  makeTableau deck = makeTableauA deck 0

  makeTableauA :: Deck -> Int -> Tableau
  makeTableauA deck 8 = []
  makeTableauA deck count = (makeTableauColumn deck count):makeTableauA deck (count+1)

  makeTableauColumn :: Deck -> Int -> Deck
  makeTableauColumn deck colNumber = take 6 (drop (6*colNumber) deck)

  eODeal :: EOBoard
  eODeal = ([], makeTableau newDeck, makeCells newDeck)
            where newDeck = shuffle pack

  moveToCell :: Card -> Cells -> Cells
  moveToCell card cells
    | length cells == 8 = cells
    | otherwise = appendCard card cells

  -- moveToFoundations :: Card ->

  foundationsWin :: Foundations -> Bool
  foundationsWin [] = True
  foundationsWin foundations@(h:t)
    | isKing (last h) = foundationsWin t
    | otherwise = False

  -- tryCardMove :: Card -> EOBoard -> EOBoard

  checkTabSucc :: Card -> Tableau -> Tableau
  checkTabSucc _ [] = []
  checkTabSucc card tableau@(h:t)
    | card == sCard (head h) = (card:h):t
    | otherwise = h:(checkTabSucc card t)

  -- inserts a card at the top of a deck
  insertCard :: Card -> Deck -> Deck
  insertCard card deck = card:deck

  -- inesrts a card at the bottom of a deck
  appendCard :: Card -> Deck -> Deck
  appendCard _ [] = []
  appendCard card deck@(h:t) = h:(appendCard card t)
  --
  -- getCellContainingAce :: Cells -> Maybe Int
  -- getCellContainingAce cells = getCellContainingAceA cells 0
  --
  -- getCellContainingAceA :: Cells -> Int -> Maybe Int
  -- getCellContainingAceA [] _ = Nothing
  -- getCellContainingAceA cells@(h:t) index
  --   | cardRank == Ace = Just index
  --   | otherwise = getCellContainingAceA t (index+1)
  --   where (cardSuit,cardRank) = h

  -- getCellContainingCardSuit :: Cells -> Suit -> Maybe Int
  -- getCellContainingCardSuit cells card = elemIndex cardSuit

  -- Gets position of first cell that contains an ace
  getCellContainingAce :: Cells -> Maybe Int
  getCellContainingAce cells = elemIndex Ace (map (\x -> snd x) cells)

  -- Gets position of a successor card in the cells
  getCellContainingSuccessor :: Cells -> Card -> Maybe Int
  getCellContainingSuccessor cells card = elemIndex (sCard card) cells

  -- Gets tableau that has Ace as top card
  getTableauWithAce :: Tableau -> Maybe Int
  getTableauWithAce tableau = elemIndex Ace (map (\x -> snd (head x)) tableau)

  -- Gets tableau that has successor card as top card
  getTableauWithSuccessor :: Tableau -> Card -> Maybe Int
  getTableauWithSuccessor tableau card = elemIndex (sCard card) (map (\x -> head x) tableau)
