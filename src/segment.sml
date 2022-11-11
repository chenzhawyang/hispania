datatype height = Low
		| LowMid
		| Mid
		| HighMid
		| High

datatype centrality = Front
		    | Central
		    | Back

datatype vowel = Vowel of height * centrality

datatype voice = Voiced | Voiceless

datatype place = Bilabial | Labiodental
	       | Dental
	       | Alveolar
	       | Palatal
	       | Velar | Labiovelar
	       | Glottal
			      				       
datatype manner = Nasal
		| Stop
		| NonSibil
		| Sibilant
		| Affricate
		| Approximant
		| Tap
		| Trill
		| Lateral

datatype consonant = Consonant of voice * place * manner

val seg_a = Vowel (Low, Central)
val seg_e = Vowel (Mid, Front)
val seg_i = Vowel (High, Front)
val seg_o = Vowel (Mid, Back)
val seg_u = Vowel (High, Back)
val seg_i' = Vowel (HighMid, Front)
val seg_e' = Vowel (LowMid, Front)
val seg_u' = Vowel (HighMid, Back)
val seg_o' = Vowel (LowMid, Back)
		   

fun vocToStr (Vowel (Low, Central)) = "a"
  | vocToStr (Vowel (Mid, Front)) = "e"
  | vocToStr (Vowel (High, Front)) = "i"
  | vocToStr (Vowel (Mid, Back)) = "o"
  | vocToStr (Vowel (High, Back)) = "u"
  | vocToStr (Vowel (HighMid, Front)) = "\201\170"
  | vocToStr (Vowel (LowMid, Front)) = "\201\155"
  | vocToStr (Vowel (HighMid, Back)) = "\202\138"
  | vocToStr (Vowel (LowMid, Back)) = "\201\148"

val seg_p = Consonant (Voiceless, Bilabial, Stop)
val seg_b = Consonant (Voiced, Bilabial, Stop)
val seg_ph = Consonant (Voiceless, Bilabial, NonSibil)		      
val seg_bh = Consonant (Voiced, Bilabial, NonSibil)
val seg_m = Consonant (Voiced, Bilabial, Nasal)
		      
val seg_t = Consonant (Voiceless, Dental, Stop)
val seg_d = Consonant (Voiced, Dental, Stop)
val seg_th = Consonant (Voiceless, Dental, NonSibil)
val seg_dh = Consonant (Voiced, Dental, NonSibil)
val seg_n = Consonant (Voiced, Dental, Nasal)

val seg_s = Consonant (Voiceless, Alveolar, Sibilant)
val seg_z = Consonant (Voiced, Alveolar, Sibilant)

val seg_ts = Consonant (Voiceless, Dental, Affricate)
val seg_dz = Consonant (Voiced, Dental, Affricate)
		       
val seg_ch = Consonant (Voiceless, Palatal, Affricate)
val seg_dg = Consonant (Voiced, Palatal, Affricate)

val seg_gn = Consonant (Voiced, Palatal, Nasal)
val seg_lh = Consonant (Voiced, Palatal, Lateral)
val seg_jh = Consonant (Voiced, Palatal, NonSibil)

val seg_k = Consonant (Voiceless, Velar, Stop)
val seg_g = Consonant (Voiced, Velar, Stop)
val seg_kw = Consonant (Voiceless, Labiovelar, Stop)
val seg_gw = Consonant (Voiced, Labiovelar, Stop)
val seg_x = Consonant (Voiceless, Velar, NonSibil)
val seg_gh = Consonant (Voiced, Velar, NonSibil)

val seg_j = Consonant (Voiced, Palatal, Approximant)
val seg_w = Consonant (Voiced, Labiovelar, Approximant)

val seg_l = Consonant (Voiced, Dental, Lateral)
val seg_rr = Consonant (Voiced, Dental, Trill)
val seg_r = Consonant (Voiced, Dental, Tap)
		       
val seg_h = Consonant (Voiceless, Glottal, NonSibil)

fun consToStr (Consonant (Voiceless, Bilabial, Stop)) = "p"
  | consToStr (Consonant (Voiced, Bilabial, Stop)) = "b"
  | consToStr (Consonant (Voiceless, Bilabial, NonSibil)) = "\201\184"
  | consToStr (Consonant (Voiced, Bilabial, NonSibil)) = "\206\178"
  | consToStr (Consonant (Voiced, Bilabial, Nasal)) = "m"
  | consToStr (Consonant (Voiceless, Dental, Stop)) = "t"
  | consToStr (Consonant (Voiced, Dental, Stop)) = "d"
  | consToStr (Consonant (Voiceless, Dental, NonSibil)) = "\206\184"
  | consToStr (Consonant (Voiced, Dental, NonSibil)) = "\195\176"
  | consToStr (Consonant (Voiced, Dental, Nasal)) = "n"
  | consToStr (Consonant (Voiceless, Alveolar, Sibilant)) = "s"
  | consToStr (Consonant (Voiced, Alveolar, Sibilant)) = "z"
  | consToStr (Consonant (Voiceless, Dental, Affricate)) = "\202\166"
  | consToStr (Consonant (Voiced, Dental, Affricate)) = "\202\163"
  | consToStr (Consonant (Voiceless, Palatal, Affricate)) = "t" ^ "\202\131"
  | consToStr (Consonant (Voiced, Palatal, Affricate)) = "d" ^ "\202\146"
  | consToStr (Consonant (Voiced, Palatal, Nasal)) = "\201\178"
  | consToStr (Consonant (Voiced, Palatal, Lateral)) = "\202\142"
  | consToStr (Consonant (Voiced, Palatal, NonSibil)) = "\202\157"
  | consToStr (Consonant (Voiceless, Velar, Stop)) = "k"
  | consToStr (Consonant (Voiced, Velar, Stop)) = "g"
  | consToStr (Consonant (Voiceless, Labiovelar, Stop)) = "k" ^ "\202\183"
  | consToStr (Consonant (Voiced, Labiovelar, Stop)) = "g" ^ "\202\183"
  | consToStr (Consonant (Voiceless, Velar, NonSibil)) = "x"
  | consToStr (Consonant (Voiced, Velar, NonSibil)) = "\201\163"
  | consToStr (Consonant (Voiced, Palatal, Approximant)) = "j"
  | consToStr (Consonant (Voiced, Labiovelar, Approximant)) = "w"
  | consToStr (Consonant (Voiced, Dental, Lateral)) = "l"
  | consToStr (Consonant (Voiced, Dental, Trill)) = "r"
  | consToStr (Consonant (Voiced, Dental, Tap)) = "\201\190"
