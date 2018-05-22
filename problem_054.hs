import Data.List

problem54 = do
  content <- readLines "files/problem_054.txt"
  return $ length
         $ filter (==Player1Wins)
         $ map compare''
         $ map parseHands content

readLines = fmap lines . readFile

parseHands = toHands . split . (map toCard) . words
  where split x = (take 5 x, drop 5 x)
        toHands x = (toHand $ fst x, toHand $ snd x)

toCard (v:s:[]) = Card { value = toValue v, suit = toSuit s } 

toValue char =
  case char of
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'J' -> Jack
  'Q' -> Queen
  'K' -> King
  'A' -> Ace

toSuit char = 
  case char of
  'C' -> Clubs
  'D' -> Diamonds
  'H' -> Hearts
  'S' -> Spades

toHand cards = (Nothing, cards) <+>
               royalFlush <+>
               straightFlush <+>
               fullHouse <+>
               flush <+>
               straight <+>
               threeOfAKind <+>
               twoPairs <+>
               onePair <+>
               highCard

(hand, cards) <+> f = 
  case hand of
  Nothing   -> (f cards, cards)
  _         -> (hand, cards)

isLengthEqualto n x = length x == n
isEqualValue card card' = value card == value card'
areEqualValues cards cards' = all (==True) $ map (\x -> isEqualValue (fst x) (snd x)) $ zip (sort cards) (sort cards')

compare' cards cards' = let f = (map value) . reverse . sort in (f cards) `compare` (f cards')

compare'' :: ((Maybe Hand, Cards), (Maybe Hand, Cards)) -> Result
compare'' ((Nothing, _), _) = Draw
compare'' (_, (Nothing, _)) = Draw
compare'' ((Just hand, cards), (Just hand', cards')) =
  case hand `compare` hand' of
  LT -> Player2Wins
  GT -> Player1Wins
  EQ -> let handCards       = getCards hand
            handCards'      = getCards hand'
            remainingCards  = cards \\ handCards
            remainingCards' = cards' \\ handCards'
            in case remainingCards `compare'` remainingCards' of
            LT -> Player2Wins
            GT -> Player1Wins
            EQ -> Draw

areConsecutive = isSucc . (map value) . sort
areSameSuit cards = (length $ nub $ map suit cards) == 1

isSucc (a:b:c:d:e:[]) = (succ a == b && succ b == c && succ c == d && succ d == e)
isSucc _ = False

highCard cards = Just $ HighCard $ head $ reverse $ sort cards

onePair cards = 
  case f cards of
  (x:[]) -> Just(OnePair x)
  _      -> Nothing
  where f = (filter (isLengthEqualto 2)) . (groupBy isEqualValue) . sort

twoPairs cards = 
  case f cards of
  (x:y:[]) -> Just(TwoPairs (OnePair x) (OnePair y))
  _        -> Nothing
  where f = (filter (isLengthEqualto 2)) . (groupBy isEqualValue) . sort

threeOfAKind cards =
  case f cards of
  (x:[]) -> Just(ThreeOfAKind x)
  _      -> Nothing
  where f = (filter (isLengthEqualto 3)) . (groupBy isEqualValue) . sort
     
straight cards = if areConsecutive cards then Just(Straight cards) else Nothing

flush cards = if areSameSuit cards then Just(Flush cards) else Nothing

fullHouse cards = 
  case threeOfAKind cards of
  Nothing                    -> Nothing
  Just (ThreeOfAKind cards') -> let cards'' = cards \\ cards'
                                    in case onePair cards'' of
                                    Nothing                 -> Nothing
                                    Just (OnePair cards''') -> Just(FullHouse (ThreeOfAKind cards') (OnePair cards'''))

straightFlush cards = if areSameSuit cards && areConsecutive cards then Just(StraightFlush cards) else Nothing

royalFlush cards = if areSameSuit cards && hasTenJackQueenKingAce then Just(RoyalFlush) else Nothing
  where values                 = map value cards
        hasTenJackQueenKingAce = (values \\ [Ten, Jack, Queen, King, Ace]) == []

data Card = Card { value :: Value , suit :: Suit } deriving (Eq)

instance Ord Card where
  Card { value = v } `compare` Card { value = v' } = v `compare` v'

instance Show Card where
  show Card { value = value, suit = s } = show value ++ " of " ++ show s

type Cards = [Card]

data Value =
  Two
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
  deriving (Eq, Ord, Enum)

instance Show Value where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Suit =
  Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Show)

data Hand =
  HighCard Card
  | OnePair Cards
  | TwoPairs Hand Hand
  | ThreeOfAKind Cards
  | Straight Cards
  | Flush Cards
  | FullHouse Hand Hand
  | FourOfAKind Cards
  | StraightFlush Cards
  | RoyalFlush
  deriving (Show)

instance Eq Hand where
  (HighCard Card { value = v }) == (HighCard Card { value = v' }) = v == v'
  (OnePair cards) == (OnePair cards')                             = areEqualValues cards cards'
  (TwoPairs hand hand') == (TwoPairs hand'' hand''')              = hand == hand'' &&  hand' == hand'''
  (ThreeOfAKind cards) == (ThreeOfAKind cards')                   = areEqualValues cards cards'
  (Straight cards) == (Straight cards')                           = areEqualValues cards cards'
  (Flush _) == (Flush _)                                          = True
  (FullHouse hand hand') == (FullHouse hand'' hand''')            = hand == hand'' &&  hand' == hand'''
  (FourOfAKind cards) == (FourOfAKind cards')                     = areEqualValues cards cards'
  (StraightFlush cards) == (StraightFlush cards')                 = areEqualValues cards cards'
  RoyalFlush == RoyalFlush                                        = True

instance Ord Hand where
  (HighCard Card { value = v }) `compare` (HighCard Card { value = v' }) = v `compare` v'
  (HighCard _) `compare` (OnePair _)                                     = LT
  (HighCard _) `compare` (TwoPairs _ _)                                  = LT
  (HighCard _) `compare` (ThreeOfAKind _)                                = LT
  (HighCard _) `compare` (Straight _)                                    = LT
  (HighCard _) `compare` (Flush _)                                       = LT
  (HighCard _) `compare` (FullHouse _ _)                                 = LT
  (HighCard _) `compare` (FourOfAKind _)                                 = LT
  (HighCard _) `compare` (StraightFlush _)                               = LT
  (HighCard _) `compare` RoyalFlush                                      = LT
  (OnePair cards) `compare` (OnePair cards')                             = cards `compare'` cards'
  (OnePair _) `compare` (HighCard _)                                     = GT
  (OnePair _) `compare` (TwoPairs _ _)                                   = LT
  (OnePair _) `compare` (ThreeOfAKind _)                                 = LT
  (OnePair _) `compare` (Straight _)                                     = LT
  (OnePair _) `compare` (Flush _)                                        = LT
  (OnePair _) `compare` (FullHouse _ _)                                  = LT
  (OnePair _) `compare` (FourOfAKind _)                                  = LT
  (OnePair _) `compare` (StraightFlush _)                                = LT
  (OnePair _) `compare` RoyalFlush                                       = LT
  (TwoPairs hand hand') `compare` (TwoPairs hand'' hand''')              = let handComparison = hand' `compare` hand'''
                                                                               handComparison' = hand `compare` hand''
                                                                               in if handComparison == EQ then handComparison'
                                                                                  else handComparison
  (TwoPairs _ _) `compare` (HighCard _)                                  = GT
  (TwoPairs _ _) `compare` (OnePair _)                                   = GT
  (TwoPairs _ _) `compare` (ThreeOfAKind _)                              = LT
  (TwoPairs _ _) `compare` (Straight _)                                  = LT
  (TwoPairs _ _) `compare` (Flush _)                                     = LT
  (TwoPairs _ _) `compare` (FullHouse _ _)                               = LT
  (TwoPairs _ _) `compare` (FourOfAKind _)                               = LT
  (TwoPairs _ _) `compare` (StraightFlush _)                             = LT
  (TwoPairs _ _) `compare` RoyalFlush                                    = LT
  (ThreeOfAKind cards) `compare` (ThreeOfAKind cards')                   = cards `compare'` cards'
  (ThreeOfAKind _) `compare` (HighCard _)                                = GT
  (ThreeOfAKind _) `compare` (OnePair _)                                 = GT
  (ThreeOfAKind _) `compare` (TwoPairs _ _)                              = GT
  (ThreeOfAKind _) `compare` (Straight _)                                = LT
  (ThreeOfAKind _) `compare` (Flush _)                                   = LT
  (ThreeOfAKind _) `compare` (FullHouse _ _)                             = LT
  (ThreeOfAKind _) `compare` (FourOfAKind _)                             = LT
  (ThreeOfAKind _) `compare` (StraightFlush _)                           = LT
  (ThreeOfAKind _) `compare` RoyalFlush                                  = LT
  (Straight cards) `compare` (Straight cards')                           = cards `compare'` cards'
  (Straight _) `compare` (HighCard _)                                    = GT
  (Straight _) `compare` (OnePair _)                                     = GT
  (Straight _) `compare` (TwoPairs _ _)                                  = GT
  (Straight _) `compare` (ThreeOfAKind _)                                = GT
  (Straight _) `compare` (Flush _)                                       = LT
  (Straight _) `compare` (FullHouse _ _)                                 = LT
  (Straight _) `compare` (FourOfAKind _)                                 = LT
  (Straight _) `compare` (StraightFlush _)                               = LT
  (Straight _) `compare` RoyalFlush                                      = LT
  (Flush cards) `compare` (Flush cards')                                 = cards `compare'` cards'
  (Flush _) `compare` (HighCard _)                                       = GT
  (Flush _) `compare` (OnePair _)                                        = GT
  (Flush _) `compare` (TwoPairs _ _)                                     = GT
  (Flush _) `compare` (ThreeOfAKind _)                                   = GT
  (Flush _) `compare` (Straight _)                                       = LT
  (Flush _) `compare` (FullHouse _ _)                                    = LT
  (Flush _) `compare` (FourOfAKind _)                                    = LT
  (Flush _) `compare` (StraightFlush _)                                  = LT
  (Flush _) `compare` RoyalFlush                                         = LT
  (FullHouse hand hand') `compare` (FullHouse hand'' hand''')            = let handComparison = hand' `compare` hand'''
                                                                               handComparison' = hand `compare` hand''
                                                                               in if handComparison == EQ then handComparison'
                                                                                  else handComparison
  (FullHouse _ _) `compare` (HighCard _)                                 = GT
  (FullHouse _ _) `compare` (OnePair _)                                  = GT
  (FullHouse _ _) `compare` (TwoPairs _ _)                               = GT
  (FullHouse _ _) `compare` (ThreeOfAKind _)                             = GT
  (FullHouse _ _) `compare` (Straight _)                                 = GT
  (FullHouse _ _) `compare` (Flush _)                                    = GT
  (FullHouse _ _) `compare` (FourOfAKind _)                              = LT
  (FullHouse _ _) `compare` (StraightFlush _)                            = LT
  (FullHouse _ _) `compare` RoyalFlush                                   = LT
  (FourOfAKind cards) `compare` (FourOfAKind cards')                     = cards `compare'` cards'
  (FourOfAKind _) `compare` (HighCard _)                                 = GT
  (FourOfAKind _) `compare` (OnePair _)                                  = GT
  (FourOfAKind _) `compare` (TwoPairs _ _)                               = GT
  (FourOfAKind _) `compare` (ThreeOfAKind _)                             = GT
  (FourOfAKind _) `compare` (Straight _)                                 = GT
  (FourOfAKind _) `compare` (Flush _)                                    = GT
  (FourOfAKind _) `compare` (FullHouse _ _)                              = GT
  (FourOfAKind _) `compare` (StraightFlush _)                            = LT
  (FourOfAKind _) `compare` RoyalFlush                                   = LT
  (StraightFlush cards) `compare` (StraightFlush cards')                 = cards `compare'` cards'
  (StraightFlush _) `compare` (HighCard _)                               = GT
  (StraightFlush _) `compare` (OnePair _)                                = GT
  (StraightFlush _) `compare` (TwoPairs _ _)                             = GT
  (StraightFlush _) `compare` (ThreeOfAKind _)                           = GT
  (StraightFlush _) `compare` (Straight _)                               = GT
  (StraightFlush _) `compare` (Flush _)                                  = GT
  (StraightFlush _) `compare` (FullHouse _ _)                            = GT
  (StraightFlush _) `compare` (FourOfAKind _)                            = GT
  (StraightFlush _) `compare` RoyalFlush                                 = LT
  RoyalFlush `compare` RoyalFlush                                        = EQ
  RoyalFlush `compare` _                                                 = GT

getCards (HighCard card)        = [card]
getCards (OnePair cards)        = cards
getCards (TwoPairs hand hand')  = getCards hand ++ getCards hand' 
getCards (ThreeOfAKind cards)   = cards
getCards (Straight cards)       = cards
getCards (Flush cards)          = cards
getCards (FullHouse hand hand') = getCards hand ++ getCards hand'
getCards (FourOfAKind cards)    = cards
getCards (StraightFlush cards)  = cards

data Result = Player1Wins | Player2Wins | Draw deriving (Eq, Show)