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

  resMaybe :: (Maybe a) -> a
  resMaybe (Just x) = x

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
  appendCard card [] = [card]
  appendCard card deck@(h:t) = h:(appendCard card t)

  getEmptyFoundation :: Foundations -> Maybe Int
  getEmptyFoundation foundations
    | foundationLength < 4 = Just foundationLength
    | otherwise = Nothing
    where foundationLength = length foundations

  -- Gets position of first cell that contains an ace
  getCellContainingAce :: Cells -> Maybe Int
  getCellContainingAce cells = elemIndex Ace (map (\x -> snd x) cells)

  getCardAtCell :: Cells -> Int -> Card
  getCardAtCell cells index = cells !! index

  getTopCardAtTableau :: Tableau -> Int -> Card
  getTopCardAtTableau tableau index = head(tableau !! index)

  -- Gets position of a successor card in the cells
  getCellContainingSuccessor :: Cells -> Card -> Maybe Int
  getCellContainingSuccessor cells card = elemIndex (sCard card) cells

  -- Gets tableau that has Ace as top card
  getTableauWithAce :: Tableau -> Maybe Int
  getTableauWithAce tableau = elemIndex Ace (map (\x -> snd (head x)) tableau)

  -- Gets tableau that has successor card as top card
  getTableauWithSuccessor :: Tableau -> Card -> Maybe Int
  getTableauWithSuccessor tableau card = elemIndex (sCard card) (map (\x -> head x) tableau)

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


  -- helper function which tries to move aces from either cells or tableau to an empty foundation
  processEmptyFoundation :: EOBoard -> Int -> Maybe EOBoard
  processEmptyFoundation board@(foundations,tableau,cells) foundationNum
    | isJust cellAceResult = Just ((moveCardToFoundation (getCardAtCell cells (resMaybe cellAceResult)) foundations foundationNum), tableau, removeCardFromCell (getCardAtCell cells (resMaybe cellAceResult)) cells)
    | isJust tableauAceResult = Just ((moveCardToFoundation (getTopCardAtTableau tableau (resMaybe tableauAceResult)) foundations foundationNum), removeTopCardFromTableau tableau (resMaybe tableauAceResult), cells)
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
    | foundationIndex > 4 = Nothing
    | isJust cellSuccessorResult = Just (moveCellCardToFoundation board (resMaybe cellSuccessorResult) foundationIndex)
    | isJust tableauSuccessorResult = Just (moveTableauTopCardToFoundation board (resMaybe tableauSuccessorResult) foundationIndex)
    | otherwise = tryProcessSuccessorsA board (foundationIndex+1)
    where tableauTopCard = head (head tableau)
          cellSuccessorResult = getCellContainingSuccessor cells tableauTopCard
          tableauSuccessorResult = getTableauWithSuccessor tableau tableauTopCard

  -- Function that tries to make all possible moves to the foundations
  toFoundations :: EOBoard -> EOBoard
  toFoundations board@(foundations, tableau, cells)
    | isJust emptyFoundationIndex && isJust (processEmptyFoundation board (resMaybe emptyFoundationIndex)) = toFoundations (resMaybe (processEmptyFoundation board (resMaybe emptyFoundationIndex)))
    | isJust (tryProcessSuccessors board) = toFoundations (resMaybe (tryProcessSuccessors board))
    | otherwise = board
    where emptyFoundationIndex = getEmptyFoundation foundations
