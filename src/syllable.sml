use "segment.sml";

datatype onset = ZeroOnset
	       | Onset of consonant
               | POnset of consonant * consonant
               | OnsetM of consonant * consonant
	       | POnsetM of consonant * consonant * consonant

datatype nucleus = Monophthong of vowel
		 | Diphthong of vowel * vowel
		 | LongVowel of vowel

datatype coda = ZeroCoda
              | Codetta of consonant
              | CodaC of consonant * consonant
	      | CodaCC of consonant * consonant * consonant

datatype stress = Stressed | Unstressed

datatype syllable = Syllable of onset * nucleus * coda * stress
						
(* syllable weight *)

datatype weight = Light | Heavy					    

(* fun weight (Syllable (_, rhyme, _)) = *)
(*     let fun weight' (Rhyme (Monophthong _, ZeroCoda)) = Light *)
(* 	  | weight' _ = Heavy *)
(*     in weight' rhyme end *)

(* assign stress *)
						    
(* print syllables *)
						    
fun onsToStr ZeroOnset = ""
  | onsToStr (Onset cons) = consToStr cons
  | onsToStr (POnset (cons1, cons2)) = (consToStr cons1) ^ (consToStr cons2)
  | onsToStr (OnsetM (cons1, cons2)) = (consToStr cons1) ^ (consToStr cons2)
  | onsToStr (POnsetM (cons1, cons2, cons3)) = (consToStr cons1) ^ (consToStr cons2) ^ (consToStr cons3)

fun nucToStr (Monophthong v) = vocToStr v
  | nucToStr (Diphthong (v1, v2)) = (vocToStr v1) ^ (vocToStr v2)
  | nucToStr (LongVowel v) = vocToStr v ^ ":"

fun codToStr ZeroCoda = ""
  | codToStr (Codetta cons) = consToStr cons
  | codToStr (CodaC (cons1, cons2)) = (consToStr cons1) ^ (consToStr cons2)
  | codToStr (CodaCC (cons1, cons2, cons3)) = (consToStr cons1) ^ (consToStr cons2) ^ (consToStr cons3)

fun syllToStr (Syllable (on, nuc, cod, stress)) =
    let val on' = onsToStr on
	val nuc' = nucToStr nuc
	val cod' = codToStr cod
    in if stress = Stressed
       then "[|" ^ on' ^ nuc' ^ cod' ^ "|]"
       else "[" ^ on' ^ nuc' ^ cod' ^ "]"
    end

(* phonological word *)

type pWord = syllable list

(* print pWord *)		      

fun pWordToStr [] = ""
  | pWordToStr (x :: xs) = (syllToStr x) ^ (pWordToStr xs)

fun pWordToStrLn pword = (pWordToStr pword) ^ "\n"

fun printPWord pword = (print o pWordToStr) pword						  
