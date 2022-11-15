use "segment.sml";
use "syllable.sml";
use "sound_change.sml";
					        
(* vowel shifts *)

(* loss of quantity *)
local fun loss_quant_const (Monophthong v)
          = let fun f (Vowel (High, height)) = Vowel (HighMid, height)
	          | f (Vowel (Mid, height)) = Vowel (LowMid, height)
	          | f (v as (Vowel (Low, Central))) = v
            in Monophthong (f v) end
        | loss_quant_const (LongVowel v) = Monophthong v
        | loss_quant_const nuc = nuc
in val loss_quant_rewrite = Nucleusim loss_quant_const
end

val loss_quant : soundChange
    = let val syllabism = mkSyllabism loss_quant_rewrite NoContext
          val name = "Loss of Quantity"
      in mkSoundChange syllabism name end

(* local fun loss_quant_seg (Vowel (High, height)) = Vowel (HighMid, height) *)
(* 	| loss_quant_seg (Vowel (Mid, height)) = Vowel (LowMid, height) *)
(* 	| loss_quant_seg (v as (Vowel (Low, Central))) = v *)
(* in fun loss_quant_nuc (Monophthong v) = Monophthong (loss_quant_seg v) *)
(*      | loss_quant_nuc (LongVowel v) = Monophthong v *)
(*      | loss_quant_nuc nuc = nuc *)
(* end *)

(* fun loss_quant_syll (Syllable (onset, Rhyme (nuc, coda), stress)) *)
(* 		     = let val nuc' = loss_quant_nuc nuc *)
(* 			   val rhyme' = Rhyme (nuc', coda) *)
(* 		       in Syllable (onset, rhyme', stress) end *)

(* fun loss_quant_pword pword = map loss_quant_syll pword *)

(* val loss_quant = SoundChange (loss_quant_pword, "Loss of Vowel Quantity")			    *)



(* great merger *)

(* fun great_merger_seg (Vowel (HighMid, Front)) = Vowel (Mid, Front) *)
(*   | great_merger_seg (Vowel (HighMid, Back)) = Vowel (Mid, Back) *)
(*   | great_merger_seg v = v *)

(* fun great_merger_syll (Syllable (onset, rhyme, stress)) *)
(*     = let fun merge (Rhyme (Monophthong v, coda)) = Rhyme (Monophthong (great_merger_seg v), coda) *)
(* 	    | merge rhyme = rhyme *)
(*       in Syllable (onset, merge rhyme, stress) end *)

(* fun great_merger_pword pword = map great_merger_syll pword *)

(* val great_merger = SoundChange (great_merger_pword, "Great Merger")				    *)

(* atonic merger *)
