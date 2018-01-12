{-#LANGUAGE MultiWayIf#-}
{-#LANGUAGE LambdaCase#-}
{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.Random
import System.Environment

import Logics
import Generation
import Assert
import Sentence

-- SAMPLES --

mytruth =
  [ "2 + 2 = 4"
  , "Kamil is male name"
  , "ducks are birds"
  , "second world war has begun in 1939"
  , "nobody expects the spanish inquisition"
  , "Age of Empires is RTS game"
  , "in Heroes III you may recruit Nagas in Tower"
  , "Asia is not in Africa"
  , "lion is king of the jungle"
  , "Lisp is older than C"
  , "Małgosia from \"Kraina Grzybów\" is a squirrel"
  , "Ardbeg is known of distillation of whiskey single malt"
  , "Google uses computer clasters"
  ]

myfalse =
  [ "6 / 2 = 124"
  , "HTML is programming language"
  , "homeopathy works"
  , "Python is static typed"
  , "Ur-Shak from Gothic was a man"
  , "every Gregory is an alcoholic"
  , "Slash is bass player in Nirvana"
  , "lion is not king of the jungle"
  , "goldcrest is an item"
  , "there exists greatest natural number"
  , "Emacs is an OS"
  , "Jan Paweł II died at 22:38"
  , "India Pale Ale is a kind of wine"
  , "Earth is flat"
  ]

myunknown =
  [ "P = NP"
  , "9/11 was planned"
  , "author of this program likes sausages"
  , "every single Windows 8 bug was planned" -- who knows?
  , "Riemann hypothesis is true"
  , "AVL trees are always the best ones when used right"
  , "there exists set which cardinality is strictly between natural and real numbers"
  , "spanish inquisition will come soon" -- nobody expects
  , "IDA will become open-source"
  , "jazz is cool"
  ]

main = do
  args <- getArgs
  g <- getStdGen

  putStrLn "How deep?"
  (d :: Int) <- readLn

  putStrLn "True, False, Any? (T/F/A)"
  v <- flip fmap getLine (\l -> if  -- super-extreme parsing lol
    | 'T' `elem` l -> True3
    | 'F' `elem` l -> False3
    | otherwise -> Unknown3
    )

  let s = generateSentence g v d m
      -- m = makeModel ["T"] ["F"] ["U"]
      m = makeModel mytruth myfalse myunknown
  when ("--check" `elem` args) $ do
    putStrLn $ case (assertValue m False3 s) of
      OK -> "OK"
      Fail s -> s
  putStrLn $ "\n" ++ toString s ++ "\n"
