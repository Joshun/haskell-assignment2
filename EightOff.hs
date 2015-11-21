module EightOff where
  import System.Random
  import Data.List
  import Data.Maybe

  data Suit = Hearts | Diamonds | Spades | Clubs deriving (Eq, Enum, Show)
  data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Enum, Show)
  type Card = (Suit,Rank)
  type Deck = [Card]

  type Foundations = [Deck]
  type Tableau = [Deck]
  type Cells = [Card]

  type EOBoard = (Foundations,Tableau,Cells)

  randSeed :: Int
  randSeed = 27

  pack :: Deck
  pack = [(suit,rank) | suit <- [Hearts .. ], rank <- [Ace .. ]]

  packCount :: Int
  packCount = length pack

  -- Returns the pip (rank) value of a given card
  pipValue :: Card -> Int
  pipValue (suit,rank) = fromEnum rank

  -- Returns successor card for ranks for which this is defined (otherwise Nothing)
  sCard :: Card -> Maybe Card
  sCard (suit,King) = Nothing
  sCard (suit,rank) = Just (suit,succ rank)

  -- Returns predecessor card for ranks for which this is defined (otherwise Nothing)
  pCard :: Card -> Maybe Card
  pCard (suit,Ace) = Nothing
  pCard (suit,rank) =  Just (suit, pred rank)

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
  makeTableau deck@(h:t)
    -- if deck length is less than 6 then we have finished making tableau columns as (6*8) = 48 leaving the last 4 for the cells
    | length deck < 6 = []
    | otherwise = take 6 deck:(makeTableau (drop 6 deck))

  eODeal :: EOBoard
  eODeal = ([], makeTableau newDeck, makeCells newDeck)
            where newDeck = shuffle pack

  moveToCell :: Card -> Cells -> Cells
  moveToCell card cells
    | length cells == 8 = cells
    | otherwise = appendCard card cells

  -- inserts a card at the top of a deck
  insertCard :: Card -> Deck -> Deck
  insertCard card deck = card:deck

  -- inesrts a card at the bottom of a deck
  appendCard :: Card -> Deck -> Deck
  appendCard card [] = [card]
  appendCard card deck@(h:t) = h:(appendCard card t)

  -- Returns the first empty foundation that is available, and Nothing otherwise
  getEmptyFoundation :: Foundations -> Maybe Int
  getEmptyFoundation foundations
    | foundationLength < 4 = Just foundationLength
    | otherwise = Nothing
    where foundationLength = length foundations

  -- Gets position of first cell that contains an ace
  getCellContainingAce :: Cells -> Maybe Int
  getCellContainingAce cells = elemIndex Ace (map (\x -> snd x) cells)

  -- Returns the card at a given cell position
  getCardAtCell :: Cells -> Int -> Card
  getCardAtCell cells index = cells !! index

  -- Returns the card at the top of a given tableau column
  getTopCardAtTableau :: Tableau -> Int -> Card
  getTopCardAtTableau tableau index = head(tableau !! index)

  -- Gets position of a successor card in the cells
  getCellContainingSuccessor :: Cells -> Card -> Maybe Int
  getCellContainingSuccessor cells card
    | isJust successorCard = elemIndex (fromJust successorCard) cells
    | otherwise = Nothing
    where successorCard = sCard card

  -- Gets tableau that has Ace as top card
  getTableauWithAce :: Tableau -> Maybe Int
  getTableauWithAce tableau = elemIndex Ace (map (\x -> snd (head x)) tableau)

  -- Gets tableau that has successor card as top card
  getTableauWithSuccessor :: Tableau -> Card -> Maybe Int
  getTableauWithSuccessor tableau card
    | isJust successorCard = elemIndex (fromJust successorCard) (map (\x -> head x) tableau)
    | otherwise = Nothing
    where successorCard = sCard card

  -- True if there are any vacant cells
  cellSpaceRemaining :: Cells -> Bool
  cellSpaceRemaining cells = length cells < 8

  -- Removes a card from a cell
  removeCardFromCell :: Card -> Cells -> Cells
  removeCardFromCell card cells = [x | x <- cells, x /= card ]

    -- Inserts a card at top of specified foundation
  moveCardToFoundation :: Card -> Foundations -> Int -> Foundations
  moveCardToFoundation card [] _ = [[card]]
  moveCardToFoundation card foundations@(h:t) foundIndex
    | foundIndex > 0 = h:moveCardToFoundation card t (foundIndex - 1)
    | otherwise = (insertCard card h):t

  -- Removes top card from specified tableau
  removeTopCardFromTableau :: Tableau -> Int -> Tableau
  removeTopCardFromTableau [] _ = []
  removeTopCardFromTableau foundations@(h:t) foundIndex
    | foundIndex > 0 = h:removeTopCardFromTableau t (foundIndex - 1)
    -- here we are leaving out the top card
    | otherwise = (tail h):t

  tryProcessEmptyFoundationA :: EOBoard -> Maybe Int -> Maybe EOBoard
  tryProcessEmptyFoundationA board@(foundations,tableau,cells) foundationNum
    | not (isJust (foundationNum)) = Nothing
    | isJust cellAceResult = Just(moveCellCardToFoundation board (fromJust cellAceResult) (fromJust foundationNum))
    | isJust tableauAceResult = Just(moveTableauTopCardToFoundation board (fromJust tableauAceResult) (fromJust foundationNum))
    | otherwise = Nothing
    where cellAceResult = getCellContainingAce cells
          tableauAceResult = getTableauWithAce tableau

  -- helper function which tries to move aces from either cells or tableau to an empty foundation
  tryProcessEmptyFoundation :: EOBoard -> Maybe EOBoard
  tryProcessEmptyFoundation board@(foundations,tableau,cells)
    | isJust foundationNum = tryProcessEmptyFoundationA board foundationNum
    | otherwise = Nothing
    where foundationNum = getEmptyFoundation foundations

  -- Removes card from given cell and moves it to given foundation
  moveCellCardToFoundation :: EOBoard -> Int -> Int -> EOBoard
  moveCellCardToFoundation board@(foundations,tableau,cells) cellIndex foundationIndex =
    (((moveCardToFoundation (getCardAtCell cells cellIndex)) foundations foundationIndex), tableau, removeCardFromCell (getCardAtCell cells cellIndex) cells)

  -- Removes top card from given tableau and moves it to given foundation
  moveTableauTopCardToFoundation :: EOBoard -> Int -> Int -> EOBoard
  moveTableauTopCardToFoundation board@(foundations,tableau,cells) tableauIndex foundationIndex =
    ((moveCardToFoundation (getTopCardAtTableau tableau tableauIndex) foundations foundationIndex), removeTopCardFromTableau tableau tableauIndex, cells)

  -- Helper function which tries to see if there is a successor card in cells or tableau to those in one of the foundations and if so moves it to the foundation
  tryProcessSuccessors :: EOBoard -> Maybe EOBoard
  tryProcessSuccessors board = tryProcessSuccessorsA board 0

  tryProcessSuccessorsA :: EOBoard -> Int -> Maybe EOBoard
  tryProcessSuccessorsA board@(foundations,tableau,cells) foundationIndex
    | foundationIndex == (length foundations) = Nothing
    | isJust cellSuccessorResult = Just (moveCellCardToFoundation board (fromJust cellSuccessorResult) foundationIndex)
    | isJust tableauSuccessorResult = Just (moveTableauTopCardToFoundation board (fromJust tableauSuccessorResult) foundationIndex)
    | otherwise = tryProcessSuccessorsA board (foundationIndex+1)
    where foundationTopCard = head (foundations !! foundationIndex)
          cellSuccessorResult = getCellContainingSuccessor cells foundationTopCard
          tableauSuccessorResult = getTableauWithSuccessor tableau foundationTopCard

  -- Function that tries to make all possible moves to the foundations
  toFoundations :: EOBoard -> EOBoard
  toFoundations board@(foundations, tableau, cells)
    | isJust emptyFoundationResult = toFoundations (fromJust emptyFoundationResult)
    | isJust processSuccessorsResult = toFoundations (fromJust processSuccessorsResult)
    | otherwise = board
    where emptyFoundationResult = tryProcessEmptyFoundation board
          processSuccessorsResult = tryProcessSuccessors board
