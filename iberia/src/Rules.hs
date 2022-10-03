module Rules where

import Phonology
import Segments
-- import SoundChanges

{- latin -> proto-romance -}

{- loss of quantity -}

vowel_quantity_merger :: Vowel -> Vowel
vowel_quantity_merger v@(Vowel _ _ Long) = v
vowel_quantity_merger v | v == seg_a = v { dure = Short }
                        | High <- height v = v { dure = Short, height = HiMid }
                        | Mid <- height v = v { dure = Short,  height = LoMid }
