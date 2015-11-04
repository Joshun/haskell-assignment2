module EightOff where
  data Suit = Heart | Diamond | Spades | Clubs deriving (Enum, Show)
  data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | King | Queen deriving (Enum, Show)
  type Card = (Suit,Rank)
  type Deck = [Card]

  data Foundation = Card (Foundation)

  -- newDeck :: [Deck]
  -- newDeck = map (\suit -> rank -> (suit,rank)) [Heart .. Clubs] [Ace .. Queen]
  -- newDeck = zip [Heart .. Clubs] [Ace .. Queen]

  newDeck :: Deck
  newDeck = [(suit,rank) | suit <- [Heart .. ], rank <- [Ace .. ]]

  newDeckTotal :: Int
  newDeckTotal = length newDeck

  pipValue :: Card -> Int
  pipValue (suit,rank) = fromEnum rank
