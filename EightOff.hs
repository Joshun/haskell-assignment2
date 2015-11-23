module EightOff where
  import System.Random
  import Data.List
  import Data.Maybe

  data Suit = Hearts | Diamonds | Spades | Clubs deriving (Eq, Enum, Show)
  data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Enum, Show)
  type Card = (Suit,Rank)
  -- Data structure to represent a complete deck of (52) cards
  type Deck = [Card]
  -- Data structure for a stack of cards (i.e. a tableau column or a single foundation)
  -- In this structure, first card in list is top card of the foundation / tableau stack and last card is the bottom card
  type CardStack = [Card]

  type Foundations = [CardStack]
  type Tableau = [CardStack]
  type Cells = [Card]

  type EOBoard = (Foundations,Tableau,Cells)

  -- Random seed used by shuffle
  randSeed :: Int
  randSeed = 27

  -- Generates a new pack of cards (Deck)
  pack :: Deck
  pack = [(suit,rank) | suit <- [Hearts .. ], rank <- [Ace .. ]]

  -- Constant for the length of a pack of cards (52 in a standard deck)
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

  -- Returns true if a given card is an Ace
  isAce :: Card -> Bool
  isAce (suit,rank) = rank == Ace

  -- Returns true if a given card is a King
  isKing :: Card -> Bool
  isKing (suit,rank) = rank == King

  -- Takes a Deck of cards and returns a shuffled Deck
  shuffle :: Deck -> Deck
  -- zip the given deck to a random set of numbers, sort this by those numbers and then unzip using map
  shuffle deck = map fst (sortBy sortComparator (zip deck (take packCount (randoms (mkStdGen randSeed):: [Int]))))

  -- Comparator function used by shuffle
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

  -- Inserts a card at the top of a stack of cards
  insertCard :: Card -> CardStack -> CardStack
  insertCard card [] = [card]
  insertCard card deck = card:deck

  -- Inserts a card at the bottom of a stack of cards
  appendCard :: Card -> CardStack -> CardStack
  appendCard card [] = [card]
  appendCard card deck@(h:t) = h:(appendCard card t)

  -- Returns the first empty foundation that is available, and Nothing otherwise
  getEmptyFoundation :: Foundations -> Maybe Int
  getEmptyFoundation foundations
  -- Not all 4 foundations have been created yet so return length as this equals the index of the next foundation
    | foundationLength < 4 = Just foundationLength
  -- Else all foundations have been created so return Nothing
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

  -- Gets index of a successor card in the cells
  getCellContainingSuccessor :: Cells -> Card -> Maybe Int
  getCellContainingSuccessor cells card
  -- If card has valid successor, see if it is in the cells and if so return its index
    | isJust successorCard = elemIndex (fromJust successorCard) cells
  -- Return Nothing if card doesn't have a valid successor (i.e. King)
    | otherwise = Nothing
    where successorCard = sCard card

  -- Gets index of Tableau column that has Ace as top card
  getTableauWithAce :: Tableau -> Maybe Int
  getTableauWithAce tableau = elemIndex Ace (map (\x -> snd (head x)) tableau)

  -- Gets index of Tableau column that has successor card as top card
  getTableauWithSuccessor :: Tableau -> Card -> Maybe Int
  getTableauWithSuccessor tableau card
  -- If card has valid successor, see if the top card of any tableau columns is equal to this
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
  -- If foundation is empty, create new one with card inserted
  moveCardToFoundation card [] _ = [[card]]
  -- Otherwise insert card at top of foundation when we've found it
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

  -- Helper function which tries to move aces from either cells or tableau to an empty foundation
  tryProcessEmptyFoundation :: EOBoard -> Maybe EOBoard
  tryProcessEmptyFoundation board@(foundations,tableau,cells)
    | isJust foundationNum = tryProcessEmptyFoundationA board foundationNum
    | otherwise = Nothing
    where foundationNum = getEmptyFoundation foundations

  tryProcessEmptyFoundationA :: EOBoard -> Maybe Int -> Maybe EOBoard
  tryProcessEmptyFoundationA board@(foundations,tableau,cells) foundationNum
    -- If there are no empty foundations, return Nothing immediately
    | not (isJust (foundationNum)) = Nothing
    -- See if any there are any aces in cells and if so move them to the empty foundation
    | isJust cellAceResult = Just(moveCellCardToFoundation board (fromJust cellAceResult) (fromJust foundationNum))
    -- Else see if there are any aces in tableau and if so move them to the empty foundation
    | isJust tableauAceResult = Just(moveTableauTopCardToFoundation board (fromJust tableauAceResult) (fromJust foundationNum))
    -- If no aces were found in either cells or tableau, return Nothing
    | otherwise = Nothing
    where cellAceResult = getCellContainingAce cells
          tableauAceResult = getTableauWithAce tableau

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
    -- We have checked all the foundations for successors in cells and tableau and found none
    | foundationIndex == (length foundations) = Nothing
    -- If a successor was found in the cells, move it to the top of the foundation
    | isJust cellSuccessorResult = Just (moveCellCardToFoundation board (fromJust cellSuccessorResult) foundationIndex)
    -- If a successor was found in the tableau, move it to the top of the foundation
    | isJust tableauSuccessorResult = Just (moveTableauTopCardToFoundation board (fromJust tableauSuccessorResult) foundationIndex)
    -- If current foundation didn't have any successors in the cells or tableau, repeat on next foundation
    | otherwise = tryProcessSuccessorsA board (foundationIndex+1)
    where foundationTopCard = head (foundations !! foundationIndex)
          cellSuccessorResult = getCellContainingSuccessor cells foundationTopCard
          tableauSuccessorResult = getTableauWithSuccessor tableau foundationTopCard

  -- Function that tries to make all possible moves to the foundations
  toFoundations :: EOBoard -> EOBoard
  toFoundations board@(foundations, tableau, cells)
    -- See if an ace could be moved to an empty foundation
    | isJust emptyFoundationResult = toFoundations (fromJust emptyFoundationResult)
    -- Else see if a successor card could be moved to a foundation
    | isJust processSuccessorsResult = toFoundations (fromJust processSuccessorsResult)
    -- Otherwise, there are no more moves to be made to the foundations so return Nothing
    | otherwise = board
    where emptyFoundationResult = tryProcessEmptyFoundation board
          processSuccessorsResult = tryProcessSuccessors board
