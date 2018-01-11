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
  Not a -> "nieprawda, że " ++ toString a
  Or a b -> "(" ++ toString a ++ " lub " ++ toString b ++ ")"
  And a b -> "(" ++ toString a ++ " oraz " ++ toString b++ ")"
  Impl a b -> "(jeśli " ++ toString a ++ ", to " ++ toString b++ ")"
  Iff a b -> "(" ++ toString a ++ " wtedy i tylko wtedy, gdy " ++ toString b++ ")"

