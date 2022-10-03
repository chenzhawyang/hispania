module Glossary where

import Phonology
import Segments

{- numerals -}

gloss_unus :: Lexis
gloss_unus =
  let s1 = Syllable o n c s
        where o = ZeroOnset
              n = Monoph seg_u'
              c = ZeroCoda
              s = Stressed
      s2 = Syllable o n c s
        where o = SingOnset seg_n
              n = Monoph seg_u
              c = SingCoda seg_s
              s = Unstressed
  in [s1, s2]

gloss_duo :: Lexis
gloss_duo =
  let s1 = Syllable o n c s
        where o = SingOnset seg_d
              n = Monoph seg_u'
              c = ZeroCoda
              s = Stressed
      s2 = Syllable o n c s
        where o = ZeroOnset
              n = Monoph seg_o
              c = ZeroCoda
              s = Unstressed
  in [s1, s2]

gloss_tres :: Lexis
gloss_tres =
  let s1 = Syllable o n c s
        where o = DiOnset seg_t seg_r
              n = Monoph seg_e'
              c = SingCoda seg_s
              s = Stressed
  in [s1]

gloss_quattuor :: Lexis
gloss_quattuor =
  let s1 = Syllable o n c s
        where o = SingOnset seg_kw
              n = Monoph seg_a
              c = SingCoda seg_t
              s = Stressed
      s2 = Syllable o n c s
        where o = SingOnset seg_t
              n = Monoph seg_u
              c = ZeroCoda
              s = Unstressed              
      s3 = Syllable o n c s
        where o = ZeroOnset
              n = Monoph seg_o
              c = SingCoda seg_r
              s = Unstressed
  in [s1, s2, s3]

gloss_quinque :: Lexis
gloss_quinque =
  let s1 = Syllable o n c s
        where o = SingOnset seg_kw
              n = Monoph seg_i'
              c = SingCoda seg_n
              s = Stressed
      s2 = Syllable o n c s
        where o = SingOnset seg_kw
              n = Monoph seg_e
              c = ZeroCoda
              s = Unstressed
  in [s1, s2]

gloss_sex :: Lexis
gloss_sex =
  let s1 = Syllable o n c s
        where o = SingOnset seg_s
              n = Monoph seg_e
              c = DiCoda seg_k seg_s
              s = Stressed            
  in [s1]

gloss_septem :: Lexis
gloss_septem =
  let s1 = Syllable o n c s
        where o = SingOnset seg_s
              n = Monoph seg_e
              c = SingCoda seg_p
              s = Stressed
      s2 = Syllable o n c s
        where o = SingOnset seg_t
              n = Monoph seg_e
              c = SingCoda seg_m
              s = Unstressed
  in [s1, s2]

gloss_octo :: Lexis
gloss_octo =
  let s1 = Syllable o n c s
        where o = ZeroOnset
              n = Monoph seg_o
              c = SingCoda seg_k
              s = Stressed
      s2 = Syllable o n c s
        where o = SingOnset seg_t
              n = Monoph seg_o
              c = ZeroCoda
              s = Unstressed
  in [s1, s2]

gloss_novem :: Lexis
gloss_novem =
  let s1 = Syllable o n c s
        where o = SingOnset seg_n
              n = Monoph seg_o
              c = ZeroCoda
              s = Stressed
      s2 = Syllable o n c s
        where o = SingOnset seg_w
              n = Monoph seg_e
              c = SingCoda seg_m
              s = Unstressed
  in [s1, s2]

gloss_decem :: Lexis
gloss_decem =
  let s1 = Syllable o n c s
        where o = SingOnset seg_d
              n = Monoph seg_e
              c = ZeroCoda
              s = Stressed
      s2 = Syllable o n c s
        where o = SingOnset seg_k
              n = Monoph seg_e
              c = SingCoda seg_m
              s = Unstressed
  in [s1, s2]
