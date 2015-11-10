module EightOff where
  import System.Random
  import Data.List
  data Suit = Heart | Diamond | Spades | Clubs deriving (Eq, Enum, Show)
  data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | King | Queen deriving (Eq, Enum, Show)
  type Card = (Suit,Rank)
  type Deck = [Card]

  type Foundations = [Deck]
  type Tableau = [Deck]
  type Cells = (Card,Card,Card,Card)

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



  -- eODeal :: EOBoard
  -- eODeal = (tableau,reserves,(EmptyCard, EmptyCard, EmptyCard, EmptyCard))
  --   where tableau = take (packCount - 4) x
  --         reserves = []
  --         x = shuffle pack
