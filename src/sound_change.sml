use "segment.sml";
use "syllable.sml";
use "utils.sml";

(* rewrites that changes a constituent of the syllable *)

datatype rewrite = Onsetism of (onset -> onset)
		 | Nucleusim of (nucleus -> nucleus)
		 | Codism of (coda -> coda)

(* syllabism that rewrites a syllable to another *)

datatype context = NoContext
                 | Predicate of (syllable -> bool)
                               
datatype syllabism = Syllabism of (syllable -> syllable) * context
                                                               
fun mkSyllF (Onsetism f) (Syllable (on, nuc, cod, stress))
    = let val on' = f on
      in Syllable (on', nuc, cod, stress) end
  | mkSyllF (Nucleusim f) (Syllable (on, nuc, cod, stress))
    = let val nuc' = f nuc 
      in Syllable (on, nuc', cod, stress) end
  | mkSyllF (Codism f) (Syllable (on, nuc, cod, stress))
    = let val cod' = f cod
      in Syllable (on, nuc, cod', stress) end
          
fun mkSyllabism (r : rewrite) context 
    = let val f = mkSyllF r
      in Syllabism (f, context) end

(* sound changes with a name *)
                               
datatype soundChange = SoundChange of (pWord -> pWord) * string (* this string is the name of the sound change *)

fun mkSoundChange (Syllabism (f, context)) name
    = case context of
          NoContext => let val sc = map f
                       in SoundChange (sc, name) end
       |  Predicate p => let val sc = applyOnly p f
                         in SoundChange (sc, name) end

(* reflext with its phonological history *)

datatype reflex = Reflex of pWord * string
	 
(* apply the sound change to a word and keep a record of its history *)

fun newHistory pword1 pword2 name
    =  (pWordToStr pword1) ^ " => " ^ (pWordToStr pword2) ^ " (" ^ name ^ ")\n"

fun apply (SoundChange (f, name)) (Reflex (pword, history))
    = let val pword' = f pword
	  val history' = if pword = pword'
			 then history
			 else history ^ (newHistory pword pword' name)
      in Reflex (pword', history') end

(* chain shift *)    

(* type chainShift = soundChange list *)	 

(* print reflex *)

(* datatype reflex = Reflex of pWord * string (* this string is the phonological history of this reflex *)	  *)

(* fun printRefl (Reflex (_, history)) = print history	  *)
