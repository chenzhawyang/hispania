module Rules where

import Phonology
import Segments
-- import SoundChanges

{- latin -> proto-romance -}

{- loss of quantity -}

vowel_quantity_merger :: Vowel -> Vowel
vowel_quantity_merger v@(Vowel _ _ Long) = v { dure = Short }
vowel_quantity_merger v | v == seg_a = v
                        | High <- height v = v { height = HiMid }
                        | Mid <- height v = v { height = LoMid }
