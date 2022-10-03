module SoundChanges where

import Phonology

data Domain
  = MonoDom Segment
  | MultiDom [Segment]

data Codomain
  = MonoCod Segment
  | MultiCod [Segment]
  | Elision

data Rewrite = Rewrite Domain Codomain  

apply :: Rewrite -> Lexis -> Lexis
apply = undefined
