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
  , "Kamil to męskie imię"
  , "kaczki to ptaki"
  , "2 wojna światowa zakończyła się w 1945 roku"
  , "nikt nie spodziewał się hiszpańskiej inkwizycji"
  , "Age of Empires to RTS"
  , "Nagi z Heroes III można rekrutować w Fortecy"
  , "Azja nie leży w Afryce"
  , "lew jest król dżungli"
  , "Lisp jest językiem starszym od C"
  , "Małgosia z \"Krainy Grzybów\" jest wiewiórką"
  , "Ardbeg destyluje whisky single malt"
  , "Google używa klastrów"
  ]

myfalse =
  [ "6 / 2 = 124"
  , "HTML jest językiem programowania"
  , "homeopatia działa"
  , "Python jest statycznie typowany"
  , "Ur-Shak z Gothica był człowiekiem"
  , "każdy Grześ jest alkoholikiem"
  , "Slash jest basistą Nirvany"
  , "lew nie jest król dżungli"
  , "mysikrólik to ssak"
  , "istnieje największa liczba naturalna"
  , "Emacs jest systemem operacyjnym"
  , "Jan Paweł II zmarł o godzinie 22:38"
  , "India Pale Ale jest gatunkiem wina"
  ]

myunknown =
  [ "P = NP"
  , "9/11 był zaplanowany"
  , "autor tego zdania lubi pierogi"
  , "wszystkie wady Windowsa 8 były zamierzone"
  , "hipoteza Riemanna jest prawdziwa"
  , "kwanty istnieją"
  , "szczepionki są szkodliwe"
  , "hiszpańska inkwizycja przyjdzie niebawem"
  , "IDA przejdzie pewnego dnia na open-source"
  , "jazz jest fajny"
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
