module Assert where

import Logics
import Sentence

data Assertion = OK | Fail String
  deriving (Eq, Show)
assertAll = foldl combine OK where
  combine OK OK = OK
  combine (Fail a) _ = Fail a
  combine _ (Fail b) = Fail b

assertAny = foldl combine OK where
  combine OK _ = OK
  combine _ OK = OK
  combine (Fail a) (Fail b) = Fail (a ++ " and " ++ b)
assert p s = if p then OK else Fail s

mapAssert _ OK = OK
mapAssert f (Fail s) = Fail (f s)

assertValue :: Model -> Bool3 -> Sentence -> Assertion
assertValue m True3 s =
  mapAssert (++ ("\n\tat " ++ toString s ++ " [ = False3 ]\n")) $
  case s of
    PlainSentence ps -> let val = valueInModel m s
      in assert (val == True3) ("\"" ++ ps ++ "\" is " ++ show val)
    Not a -> assertValue m False3 a
    Or l r -> assertAny . map (uncurry (assertValue m)) $
      [(True3, l), (True3, r)]
    And l r -> assertAll . map (uncurry (assertValue m)) $
      [(True3, l), (True3, r)]
    Impl l r -> assertAny . map (uncurry (assertValue m)) $
      [(False3, l), (True3, r)]
    Iff l r -> assertAny . map
               (assertAll . map (uncurry (assertValue m))) $
               [ [(False3, l), (False3, r)]
               , [(True3, l), (True3, r)]
               ]
assertValue m False3 s =
  mapAssert (++ ("\n\tat " ++ toString s ++ " [ = True3 ]\n")) $
  case s of
    PlainSentence ps -> let val = valueInModel m s
      in assert (val == False3) ("\"" ++ ps ++ "\" is " ++ show val)
    Not a -> assertValue m True3 a
    Or l r -> assertAll . map (uncurry (assertValue m)) $
      [(False3, l), (False3, r)]
    And l r -> assertAny . map (uncurry (assertValue m)) $
      [(False3, l), (False3, r)]
    Impl l r -> assertAll . map (uncurry (assertValue m)) $
      [(True3, l), (False3, r)]
    Iff l r -> assertAny . map (uncurry (assertValue m)) $
      [ (True3, l), (False3, r)
      , (False3, l), (True3, r)
      ]

