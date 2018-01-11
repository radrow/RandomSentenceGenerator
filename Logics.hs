{-#LANGUAGE MultiWayIf#-}
{-#LANGUAGE ScopedTypeVariables#-}
module Logics where

import Sentence

-- | Truth, Falsehood, Unknown / Subjective
type Model = ([String], [String], [String])

data Bool3 = True3 | False3 | Unknown3
  deriving (Eq, Show, Read)

not3 True3 = False3
not3 False3 = True3
not3 Unknown3 = Unknown3

fromBool True = True3
fromBool False = False3

makeModel :: [String] -> [String] -> [String] -> Model
makeModel t f u =
  if any (`elem` f) t
  then error "Contradict model"
  else (t, f, filter (\a -> a `notElem` (t ++ f)) u)

valueInModel :: Model -> Sentence -> Bool3
valueInModel (t, f, _) (PlainSentence s) = if
  | s `elem` t -> True3
  | s `elem` f -> False3
  | otherwise -> Unknown3
valueInModel m (Not s) = not3 (valueInModel m s)
valueInModel m (Or p q) = case (valueInModel m p, valueInModel m q) of
  (True3, _) -> True3
  (_, True3) -> True3
  (False3, False3) -> False3
  _ -> Unknown3
valueInModel m (And p q) = case (valueInModel m p, valueInModel m q) of
  (False3, _) -> False3
  (_, False3) -> False3
  (True3, True3) -> True3
  _ -> Unknown3
valueInModel m (Impl p q) = case (valueInModel m p, valueInModel m q) of
  (False3, _) -> True3
  (_, True3) -> True3
  (True3, False3) -> False3
  _ -> Unknown3
valueInModel m (Iff p q) = do
  let pv = valueInModel m p
      qv = valueInModel m q
  if (pv == Unknown3) || (qv == Unknown3)
    then Unknown3
    else fromBool $ pv == qv
