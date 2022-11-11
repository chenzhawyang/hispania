use "segment.sml";
use "syllable.sml";
use "sound_change.sml";

local val lu =
	  let val onset = Onset seg_l
	      val nucleus = Monophthong seg_u
	      val coda = ZeroCoda
	      val rhyme = Rhyme (nucleus, coda)
	  in Syllable (onset, rhyme, Stressed) end
      val pu =
	  let val onset = Onset seg_p
	      val nucleus = Monophthong seg_u
	      val coda = ZeroCoda
	      val rhyme = Rhyme (nucleus, coda)
	  in Syllable (onset, rhyme, Unstressed) end
in val lupu = Reflex ([lu, pu], "") end

local val ca =
	  let val onset = Onset seg_k
	      val nucleus = Monophthong seg_a
	      val coda = ZeroCoda
	      val rhyme = Rhyme (nucleus, coda)
	  in Syllable (onset, rhyme, Stressed) end
      val sa =
	  let val onset = Onset seg_s
	      val nucleus = Monophthong seg_a
	      val coda = ZeroCoda
	      val rhyme = Rhyme (nucleus, coda)
	  in Syllable (onset, rhyme, Unstressed) end
in val casa = Reflex ([ca, sa], "") end
