use "segment.sml";
use "syllable.sml";

(* sound change *)

datatype soundChange
  = SoundChange of (pWord -> pWord) * string (* this string is the name of the sound change *)					  

datatype reflex = Reflex of pWord * string (* this string is the phonological history of this reflex *)

fun newHistory pword1 pword2 name
    =  (pWordToStr pword1) ^ " => " ^ (pWordToStr pword2) ^ " (" ^ name ^ ")\n"
									      
fun apply (SoundChange (f, name)) (Reflex (pword, history))
    = let val pword' = f pword
	  val history' = if pword = pword'
			 then history
			 else history ^ (newHistory pword pword' name)
      in Reflex (pword', history') end

(* chain shift *)    

type chainShift = soundChange list

(* print reflex *)    

fun printRefl (Reflex (_, history)) = print history

(* utils *)

					    
