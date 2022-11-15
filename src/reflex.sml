use "sound_change.sml";
use "history.sml";

local val lu =
          let val on = Onset seg_l
	      val nuc = Monophthong seg_u
	      val cod = ZeroCoda
          in Syllable (on, nuc, cod, Stressed) end
      val pu =
          let val on = Onset seg_p
	      val nuc = Monophthong seg_u
	      val cod = ZeroCoda
          in Syllable (on, nuc, cod, Unstressed) end
      val pword = [lu, pu]
in val lupu_etymon = mkRefl pword (pWordToStrLn pword)
end

local val lupu_prRom = apply loss_quant lupu_etymon
in val lupu_refl = lupu_prRom end
