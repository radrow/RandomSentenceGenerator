{-#LANGUAGE LambdaCase#-}
module Sentence where

data Sentence = PlainSentence String
              | Or Sentence Sentence
              | And Sentence Sentence
              | Impl Sentence Sentence
              | Iff Sentence Sentence
              | Not Sentence
              deriving (Eq, Show)


toString :: Sentence -> String
toString = \case
  PlainSentence s -> s
  Not a -> "it is not true, that " ++ toString a
  Or a b -> "(" ++ toString a ++ " or " ++ toString b ++ ")"
  And a b -> "(" ++ toString a ++ " and " ++ toString b++ ")"
  Impl a b -> "(if " ++ toString a ++ ", then " ++ toString b++ ")"
  Iff a b -> "(" ++ toString a ++ " if and only if when " ++ toString b++ ")"

