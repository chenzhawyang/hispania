module Phonology where

data Segment
  = ConsSeg Consonant
  | VocSeg Vowel
  deriving (Eq)

data Consonant  = Consonant
  { manner :: Manner
  , place :: Place
  , voice :: Voice
  }
  deriving (Eq)

data Manner
  = Nasal
  | Stop
  | Fric
  | Affr
  | Approx
  | Tap
  | Trill
  | Lateral
  deriving (Eq, Show)

data Place
  = Bilab
  | Dental
  | Palatal
  | PalatoVelar
  | Velar
  | LabVelar
  | Glottal
  deriving (Eq, Show)

data Voice
  = Voiceless
  | Voiced
  deriving (Eq, Show)

data Vowel = Vowel
  { height :: Height
  , centrality :: Centrality
  , dure :: Duration
  }
  deriving (Eq)

data Height
  = Low
  | LoMid
  | Mid
  | HiMid
  | High
  deriving (Eq)

data Centrality
  = Front
  | Cent
  | Back
  deriving (Eq)

data Duration
  = Short
  | Long
  deriving (Eq)

data Syllable = Syllable
  { onset :: Onset
  , nucleus :: Nucleus
  , coda :: Coda
  , stress :: Stress
  }
  deriving (Eq)

data Onset
  = ZeroOnset
  | SingOnset Consonant
  | DiOnset Consonant Consonant
  | TriOnset Consonant Consonant Consonant
  deriving (Eq)

data Nucleus
  = Monoph Vowel
  | Dipth Vowel Vowel
  deriving (Eq)

data Coda
  = ZeroCoda
  | SingCoda Consonant
  | DiCoda Consonant Consonant
  | TriCoda Consonant Consonant Consonant
  deriving (Eq)

data Stress
  = Unstressed
  | Stressed
  deriving (Eq)

type Lexis = [Syllable]
