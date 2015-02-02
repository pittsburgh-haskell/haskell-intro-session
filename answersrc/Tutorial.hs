{-|
Module: Tutorial
Description: Tutorial code
Copyright: Franklin Chen, 2015
License: BSD3
Maintainer: franklinchen@franklinchen.com

Code snippets to study and/or complete.

Instructions:

* Get this Cabal project configured

    * @cabal install --enable-tests --only-dependencies@
    * @cabal configure --enable-tests@

* You may want to run
    @
      cabal haddock
    @
  in order to generate pretty hyperlinked Haddock documentation to
  @dist\/doc\/html\/haskell-intro-session\/index.html@.

* In GHCi, use @
    :l Tutorial
  @ to load this module.

* Use @
    :r
  @ to reload after editing the file.

* Use GHCi's history, autocomplete for convenience.

* Follow along in code until the next @FIXME@.

* Write or fix code that has a @FIXME@ comment. Replace all the 'undefined'.

* Hint: writing your own type hole variables @_x@ in expressions may be helpful
while trying to complete code.

* Run doctests for this module in GHCi with @
    :test
  @ (a command I defined in our project's @.ghci@ config file)

* Feel free to experiment with changing code (and associated tests) to see what happens!
-}
module Tutorial where

-- Import a standard library module we will use
import qualified Data.Char as Char

-- | Illustrate type declaration
--
-- Type names must be Capitalized
--
-- >>> aSum
-- 15
aSum :: Int
aSum = 12 + 3

-- | Illustrate append '++'
--
-- >>> aGreeting
-- "hello world"
aGreeting :: String
aGreeting = "hello" ++ " " ++ "world"

--
-- Functions
--

-- | Illustrate function definition
--
-- >>> increment 4
-- 5
increment :: Int -> Int
increment i = i + 1

-- | Desugared version: what the compiler actually does underneath.
unsweetIncrement :: Int -> Int
unsweetIncrement = \i -> i + 1

-- | Another function definition
--
-- >>> isTooLong "establishment"
-- True
isTooLong :: String -> Bool
isTooLong word = length word > 12

-- | Desugared version.
--
-- >>> sweetIsTooLong "establishment"
-- True
sweetIsTooLong :: String -> Bool
sweetIsTooLong = \word -> length word > 12

-- | Function that "takes multiple parameters"
--
-- >>> greet "Hello" "Franklin" "!"
-- "Hello, Franklin!"
greet :: String -> String -> String -> String
greet greeting name terminator =
  greeting ++ ", " ++ name ++ terminator

-- | Desugared version.
--
-- >>> desugaredGreet "Hello" "Franklin" "!"
-- "Hello, Franklin!"
desugaredGreet :: String -> String -> String -> String
desugaredGreet = \greeting -> \name -> \terminator ->
  greeting ++ ", " ++ name ++ terminator

-- | Greet redundantly, 3 times
--
-- Practice using core lambda syntax instead of sweet syntax.
--
-- >>> redundantGreet "Hello" "Franklin"
-- "Hello Hello Hello, Franklin!?"
redundantGreet :: String -- ^ greeting
               -> String -- ^ name
               -> String
redundantGreet = \greeting name ->
  greeting ++ " " ++ greeting ++ " " ++ greeting ++ ", " ++ name ++ "!?"

-- | Function that takes single tupled parameter
--
-- >>> greetTupled ("Hello", "Franklin", "!")
-- "Hello, Franklin!"
greetTupled :: (String, String, String) -> String
greetTupled (greeting, name, terminator) =
  greeting ++ ", " ++ name ++ terminator

--
-- Tagged unions
--

-- | Type synonym: just syntactic alias
type AccountNumber = Int

-- | Type synonym: just syntactic alias
type Why = String

-- | 'Yes' variant has 2 fields, 'No' has 1 field,
-- 'Ignore' has 0 fields
data OptIn =
    Yes AccountNumber -- ^ user submitted account number
  | No Why            -- ^ user submitted a reason not to opt in
  | Ignore            -- ^ user filled in nothing
    deriving (Show)

-- | I love getting spam.
violateMyPrivacy :: OptIn
violateMyPrivacy = Yes 1234

-- | Take me off your list.
refuseIt :: OptIn
refuseIt = No "no thanks"

-- | if/then/else
--
-- >>> commentary 70
-- "OK"
commentary :: Int -> String
commentary temperature =
  if temperature > 75 then
    "Too hot"
  else if temperature < 63 then
    "Too cold"
  else
    "OK"

-- | Haskellers try to avoid using if/then/else!
-- Use guards, much more readable.
--
-- >>> sweetCommentary 70
-- "OK"
sweetCommentary :: Int -> String
sweetCommentary temperature
  | temperature > 75 = "Too hot"
  | temperature < 63 = "Too cold"
  | otherwise        = "OK"

--
-- Pattern matching on tagged unions
--

-- | Did the user respond?
--
-- >>> userResponded violateMyPrivacy
-- True
--
-- >>> userResponded refuseIt
-- True
--
-- >>> userResponded Ignore
-- False
userResponded :: OptIn -> Bool
userResponded response =
  case response of
    Yes _  -> True
    No _   -> True
    Ignore -> False

-- | Sweet version of the pattern match
--
-- Usually more concise.
sweetUserResponded :: OptIn -> Bool
sweetUserResponded (Yes _ ) = True
sweetUserResponded (No _)   = True
sweetUserResponded Ignore   = False

--
-- Lists (our own definition)
--

-- | 'List' has type parameter @elemType@
data List elemType = End -- ^ empty
                   | Construct elemType (List elemType) -- ^ a pair

-- Applying 'List' type constructor to 'Int' returns a type
-- as though we manually wrote:
--
-- @data ListInt = End | Construct Int ListInt@

-- | An expression of our 'List' type
ourList :: List Int
ourList = Construct 7 (Construct 42 (Construct 12 End))

--
-- Lists (Haskell's built-in)
--

-- | How to construct a list, using cons
unsweetList :: [Int]
unsweetList = 7 : (42 : (12 : []))

-- | Same thing with sugar
--
-- >>> unsweetList == sweetList
-- True
sweetList :: [Int]
sweetList = [7, 42, 12]

--
-- List map and filter
--

-- | Using 'map' of type
--     @
--       (input -> output) -> ([input] -> [output])
--     @
--
-- Use a library function from
-- <http://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Char.html
-- 'Data.Char'>
--
-- Remember that 'String' abbreviates @[Char]@
--
-- >>> allCaps "Franklin"
-- "FRANKLIN"
allCaps :: String -> String
allCaps s = map Char.toUpper s

-- or just:
-- 'allCaps' is the 'Char' 'toUpper' function lifted to 'String' world.
-- allCaps = map Char.toUpper

-- | Return list of responses with content.
--
-- Use 'filter' of type
--   @
--     (input -> Bool) -> ([input] -> [input])
--   @
-- Note: 'filter' lifts an element checker into a list filterer
--
-- >>> actuallyResponded [Ignore, Yes 1234, No "I hate spam"]
-- [Yes 1234,No "I hate spam"]
actuallyResponded :: [OptIn] -> [OptIn]
actuallyResponded = filter userResponded

-- | Introducing "let/in" block-scoped local bindings.
--
-- Use only '+' and '*' operators, with let.
-- Don't compute the sum twice.
--
-- >>> squareOfSum 3 2
-- 25
squareOfSum :: Int -> Int -> Int
squareOfSum x y =
  let
    z = x + y
  in
    z * z

--
-- Exercises in pattern matching and recursion
--

-- | Define 'map' ourselves, using pattern matching and recursion.
--
-- What should an empty list map to?
--
-- If a list has a head @x@ and a tail @xs@, what should be the
-- head and tail of the output list?
--
-- >>> ourMap length ["good", "morning"]
-- [4,7]
ourMap :: (input -> output) -> [input] -> [output]
ourMap _ []     = []
ourMap f (x:xs) = f x : ourMap f xs
  -- Hint: replace 'undefined' with
  --   _head : _tail

-- | Define 'filter' ourselves, using pattern matching and recursion.
--
-- What should an empty list filter to?
--
-- If a list has a head @x@ and a tail @xs@, what should be the
-- head and tail of the output list, depending on whether
-- the current @x@ satisfies @p@?
--
-- >>> ourFilter Char.isUpper "HelloDearWorld"
-- "HDW"
ourFilter :: (input -> Bool) -> [input] -> [input]
ourFilter _ []     = []
ourFilter p (x:xs) | p x = x : ourFilter p xs
                   | otherwise = ourFilter p xs

--
-- Type classes
--

-- | Our own type.
data Color =  Red | Blue

-- | define 'Color' to belong to type class 'Show'
instance Show Color where
  show Red = "red"
  show Blue = "blue"

-- | Example of a 'Color' expression
c :: Color
c = Blue

-- | Does the right thing
--
-- >>> str
-- "blue"
str :: String
str = show c

-- | Use of 'print' of type
--     @
--       Show showable => showable -> IO ()
--     @
--
-- >>> colorAction
-- [False,True,False]
-- red
colorAction :: IO ()
colorAction = do
  print [False, True, False]
  print Red

--
-- Deriving from a type class.
-- Let the compiler generate a sensible default implementation.
--

-- | deriving is very useful if you are OK with defaults.
--
-- >>> let s = Small 5
-- >>> show s
-- "Small 5"
data Size = Big Double | Small Int
  deriving (Show)

--
-- The important 'Maybe' type constructor (other languages call @Option@)
-- that is a type-safe alternative to the @null@ concept in some languages.
--
-- Defined as
--   @
--     data Maybe aType = Nothing | Just aType
--   @

-- | Sample key-value pair list.
--
-- Use 'lookup' of type
--   @
--     Eq a => a -> [(a, b)] -> Maybe b
--   @
--
-- >>> lookup "banana" keyValues
-- Just 5
--
-- >>> lookup "pear" keyValues
-- Nothing
keyValues :: [(String, Int)]
keyValues = [("apple", 2), ("banana", 5)]

-- | Runtime tag checking: given an 'OptIn', determine whether it is
-- tagged as a 'No', and if so, return 'Just' its string, else
-- return 'Nothing'
--
-- Hint: runtime tag checking is pattern matching on a tagged union type.
--
-- >>> getWhy (Yes 123)
-- Nothing
--
-- >>> getWhy (No "nonsense")
-- Just "nonsense"
--
-- >>> getWhy Ignore
-- Nothing
getWhy :: OptIn -> Maybe Why
getWhy (Yes _) = Nothing
getWhy (No why) = Just why
getWhy Ignore = Nothing

--
-- Type inference
--

-- | No type annotation provided, but it is reconstructed by compiler.
--
-- Note: list append operator is '++' of type
--   @
--     [a] -> [a] -> [a]
--   @
--
-- >>> mystery "hello" '-'
-- 11
--
-- >>> mystery [True, False] False
-- 5
--
mystery :: [a] -> a -> Int
mystery x y = length (x ++ [y] ++ x)

--
-- Lazy
--

-- | Only needs to evaluate the first 5 elements of
-- infinite list of odds
--
-- prop> sumFiveOdds == 1 + 3 + 5 + 7 + 9
sumFiveOdds :: Integer
sumFiveOdds = sum (take 5 [1, 3..])

--
-- Sweet
--

-- List comprehension

-- | A record type describing a person
data Person = P {
  firstName :: String, -- ^ first name
  parents :: [Person], -- ^ parents (using list is sloppy buy convenient)
  age :: Int           -- ^ age in years
} deriving (Show)

-- | a person
alice :: Person
alice = P { firstName = "Alice", parents = [bob, carol], age = 12 }

-- | a person
bob :: Person
bob = P { firstName = "Bob", parents = [david, esther], age = 49 }

-- | a person
carol :: Person
carol = P { firstName = "Carol", parents = [frank, grace], age = 50 }

-- | a person
david :: Person
david = P { firstName = "David", parents = [], age = 77 }

-- | a person
esther :: Person
esther = P { firstName = "Esther", parents = [], age = 72 }

-- | a person
frank :: Person
frank = P { firstName = "Frank", parents = [], age = 74 }

-- | a person
grace :: Person
grace = P { firstName = "Grace", parents = [], age = 78 }

-- | List comprehension in action
--
-- >>> oldGrandparents alice
-- [P {firstName = "David", parents = [], age = 77},P {firstName = "Grace", parents = [], age = 78}]
oldGrandparents :: Person -> [Person]
oldGrandparents person =
  [grandparent | parent <- parents person
               , let grandparents = parents parent
               , grandparent <- grandparents
               , age grandparent > 75]

-- | More examples of syntax
--
-- Operator section @(/ 100.0)@
--
-- "the function that divides its parameter by 100.0"
--
-- sugar for
--   @\x -> x / 100.0@
--
-- >>> percents
-- [0.588,0.955]
percents :: [Float]
percents = map (/ 100.0) [58.8, 95.5]

-- | Composition pipeline: read the composition right to left
--
-- '.' has type
--   @
--     (b -> c) -> (a -> b) -> a -> c
--   @
--
-- Feed value of type @a@ in to pass through to @b@ to get to type @c@.
--
-- >>> addOneThenDouble 2
-- 6
addOneThenDouble :: Int -> Int
addOneThenDouble = (*2) . (+1)

-- | Illustrate 'IO' API
main :: IO ()
main = do      -- "begin block for 'IO' context"
  s <- getLine -- "get string @s@ from stdin"
  putStrLn ("hello " ++ s ++ "!!")
