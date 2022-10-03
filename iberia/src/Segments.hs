module Segments where

import Phonology

-- import qualified Data.ByteString.Char8 as C
-- import qualified Data.ByteString.UTF8 as S

{- latin segments -}

seg_p :: Consonant
seg_p =
  let m = Stop
      p = Bilab
      v = Voiceless
  in Consonant m p v

seg_b :: Consonant
seg_b =
  let m = Stop
      p = Bilab
      v = Voiced
  in Consonant m p v

seg_t :: Consonant
seg_t =
  let m = Stop
      p = Dental
      v = Voiceless
  in Consonant m p v

seg_d :: Consonant
seg_d =
  let m = Stop
      p = Dental
      v = Voiced
  in Consonant m p v

seg_k :: Consonant
seg_k =
  let m = Stop
      p = Velar
      v = Voiceless
  in Consonant m p v

seg_g :: Consonant
seg_g =
  let m = Stop
      p = Velar
      v = Voiced
  in Consonant m p v

seg_kw :: Consonant
seg_kw =
  let m = Stop
      p = LabVelar
      v = Voiceless
  in Consonant m p v

seg_gw :: Consonant
seg_gw =
  let m = Stop
      p = LabVelar
      v = Voiced
  in Consonant m p v

seg_s :: Consonant
seg_s =
  let m = Fric
      p = Dental
      v = Voiceless
  in Consonant m p v

seg_z :: Consonant
seg_z =
  let m = Fric
      p = Dental
      v = Voiced
  in Consonant m p v

seg_f :: Consonant
seg_f =
  let m = Fric
      p = Bilab
      v = Voiceless
  in Consonant m p v

seg_h :: Consonant
seg_h =
  let m = Fric
      p = Glottal
      v = Voiceless
  in Consonant m p v

seg_m :: Consonant
seg_m =
  let m = Nasal
      p = Bilab
      v = Voiced
  in Consonant m p v

seg_n :: Consonant
seg_n =
  let m = Nasal
      p = Dental
      v = Voiced
  in Consonant m p v

seg_r :: Consonant
seg_r =
  let m = Trill
      p = Dental
      v = Voiced
  in Consonant m p v

seg_l :: Consonant
seg_l =
  let m = Lateral
      p = Dental
      v = Voiced
  in Consonant m p v

seg_j :: Consonant
seg_j =
  let m = Approx
      p = Palatal
      v = Voiced
  in Consonant m p v

seg_w :: Consonant
seg_w =
  let m = Approx
      p = Bilab
      v = Voiced
  in Consonant m p v

seg_a :: Vowel
seg_a =
  let h = Low
      c = Cent
      d = Short
  in Vowel h c d

seg_a' :: Vowel
seg_a' =
  let h = Low
      c = Cent
      d = Long
  in Vowel h c d

seg_e :: Vowel
seg_e =
  let h = Mid
      c = Front
      d = Short
  in Vowel h c d

seg_e' :: Vowel
seg_e' =
  let h = Mid
      c = Front
      d = Long
  in Vowel h c d

seg_i :: Vowel
seg_i =
  let h = High
      c = Front
      d = Short
  in Vowel h c d

seg_i' :: Vowel
seg_i' =
  let h = High
      c = Front
      d = Long
  in Vowel h c d

seg_u :: Vowel
seg_u =
  let h = High
      c = Back
      d = Short
  in Vowel h c d

seg_u' :: Vowel
seg_u' =
  let h = High
      c = Back
      d = Long
  in Vowel h c d

seg_o :: Vowel
seg_o =
  let h = Mid
      c = Back
      d = Short
  in Vowel h c d

seg_o' :: Vowel
seg_o' =
  let h = Mid
      c = Back
      d = Long
  in Vowel h c d  

{- proto-romance segments additum -}

{- western romance segments additum -}

{- iberian romance segments additum -}

{- old spanish segments additum -}

{- modern spanish segments additum -}
