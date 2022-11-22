use "phonology.sml";

(* proto-romance vocalism *)

(* loss of vowel quantity *)

fun loss_quant_subst (Monophthong v)
    = let fun f (Vowel (High, height)) = Vowel (HighMid, height)
	    | f (Vowel (Mid, height)) = Vowel (LowMid, height)
	    | f v = v
      in Monophthong (f v) end
  | loss_quant_subst (LongVowel v) = Monophthong v
  | loss_quant_subst nuc = nuc

val loss_quant_rewrite = Nucleusism loss_quant_subst

val loss_quant : soundChange
    = let val syllabism = mkSyllabism loss_quant_rewrite NoContext
          val name = "Loss of Vowel Quantity"
      in mkSoundChange syllabism name end

(* dehiaticization *)

structure Dehiaticization = struct

end
                                
(* great merger *)

fun great_merger_subst (Monophthong v)
    = let fun f (Vowel (HighMid, Front)) = Vowel (Mid, Front)
            | f (Vowel (HighMid, Back)) = Vowel (Mid, Back)
            | f v = v
      in Monophthong (f v) end
  | great_merger_subst nuc = nuc

val great_merger_rewrite = Nucleusism great_merger_subst

val great_merger : soundChange
    = let val syllabism = mkSyllabism great_merger_rewrite NoContext
          val name = "Great Merger"
      in mkSoundChange syllabism name end

(* atonic merger *)

fun atonic_merger_subst (Monophthong v)
    = let fun f (Vowel (LowMid, Front)) = Vowel (Mid, Front)
            | f (Vowel (LowMid, Back)) = Vowel (Mid, Back)
            | f _ = v
      in Monophthong (f v) end
  | atonic_merger_subst nuc = nuc

val atonic_merger_rewrite = Nucleusism atonic_merger_subst

fun not_stressed (Syllable (on, nuc, cod, Unstressed)) = true
  | not_stressed _ = false

val atonic_merger
    = let val syllabism = mkSyllabism atonic_merger_rewrite (Predicate not_stressed)
          val name = "Atonic Merger"
      in mkSoundChange syllabism name end

(* monophthongization *)

fun mono_subst' (diph as Diphthong (v1, v2))
    = let fun f (Vowel (Mid, Back)) (Vowel (Mid, Front)) = LongVowel (Vowel (Mid, Front))
            | f (Vowel (Low, Central)) (Vowel (High, Back)) = LongVowel (Vowel (Mid, Back))
            | f (Vowel (Low, Central)) (Vowel (Mid, Front)) = Monophthong (Vowel (Mid, Front))
            | f _ _ = diph
      in f v1 v2 end
  | mono_subst' nuc = nuc

fun mono_subst nuc
    = case nuc of
          Diphthong _ => mono_subst' nuc
       |  _ => nuc

val mono_rewrite = Nucleusism mono_subst

val mono = let val syllabism = mkSyllabism mono_rewrite NoContext
               val name = "Monophthongization"
           in mkSoundChange syllabism name end

(* proto-romance vowel shift *)

local val sc1 = mono
      val sc2 = loss_quant
      val sc3 = great_merger
      val sc4 = atonic_merger
in val prRom_vowel_shift = [sc1, sc2, sc3, sc4] end

(* proto-romance consonantism *)

(* elision of word-final [m] *)

fun elide_m_subst cod
    = case cod of
          Codetta c => if c = seg_m
                       then ZeroCoda
                       else cod
        | _ => cod

val elide_m_rewrite = Codism elide_m_subst

val elide_m = let val syllabism = mkSyllabism elide_m_rewrite WordFinal
                  val name = "Elision of word-final [m]"
              in mkSoundChange syllabism name end

(* deaspiration of [h] *)

fun deasp_subst onset
    = case onset of
          Onset on => if on = seg_h
                      then ZeroOnset
                      else onset
        | _ => onset

val deasp_rewrite = Onsetism deasp_subst

val deasp = let val syllabism = mkSyllabism deasp_rewrite NoContext
                val name = "Deaspiration of [h]"
            in mkSoundChange syllabism name end

(* fortition of word-initial [w]*)

fun fortition_w_subst onset
    = case onset of
          Onset on => if on = seg_w
                      then Onset seg_bh
                      else onset
        | _ => onset

val fortition_w_rewrite = Onsetism fortition_w_subst

val fortition_w = let val syllabism = mkSyllabism fortition_w_rewrite NoContext
                      val name = "Fortition of [w]"
                  in mkSoundChange syllabism name end

(* palatalization *)

structure Palatalize = struct
    fun palatalize_subst input output onset =
        case onset of
            Onset cons => if cons = input
                          then Onset output
                          else onset
          | _ => onset

    fun mkSyllF onsetism syll =
        let val (Syllable (onset, nuc, cod, stress)) = syll
            val onset' = onsetism onset
        in Syllable (onset', nuc, cod, stress) end

    fun mkPalPred (nuc_pred : nucleus -> bool) syll =
        let val (Syllable (_, nuc, _, _)) = syll
        in nuc_pred nuc end
end

(* palatalization of [t] *)

structure PalatalizeT = struct
    val subst =
        let val input = seg_t
            val output = seg_ts
        in Palatalize.palatalize_subst input output end

    fun nuc_context (Monophthong v) = (v = seg_i)
      | nuc_context (Diphthong (v1, v2)) = (v1 = seg_i)
      | nuc_context _ = false

    fun syllF onsetism syll =
        let val Syllable (on, nuc, cod, stress) = syll
            val on' = onsetism on
            val nuc' = case nuc of
                           Monophthong _ => nuc
                         | Diphthong (v1, v2) => Monophthong v2
                         | _ => nuc
        in Syllable (on', nuc', cod, stress) end

    val syllabism = let val context = Palatalize.mkPalPred nuc_context
                    in Syllabism (syllF subst, Predicate context) end

    val sound_change = let val name = "Palatalization of [t]"
                       in mkSoundChange syllabism name end
end

val palatalize_t = PalatalizeT.sound_change

(* palatalization of [d] *)

structure PalatalizeD = struct
    val subst =
        let val input = seg_d
            val output = seg_dg
        in Palatalize.palatalize_subst input output end

    fun nuc_context (Monophthong v) = (v = seg_i)
      | nuc_context (Diphthong (v1, v2)) = (v1 = seg_i)
      | nuc_context _ = false

    fun syllF onsetism syll =
        let val Syllable (on, nuc, cod, stress) = syll
            val on' = onsetism on
            val nuc' = case nuc of
                           Monophthong _ => nuc
                         | Diphthong (v1, v2) => Monophthong v2
                         | _ => nuc
        in Syllable (on', nuc', cod, stress) end

    val syllabism = let val context = Palatalize.mkPalPred nuc_context
                    in Syllabism (syllF subst, Predicate context) end

    val sound_change = let val name = "Palatalization of [d]"
                       in mkSoundChange syllabism name end
end

val palatalize_d = PalatalizeD.sound_change

(* palatalization of [k] *)

structure PalatalizeK = struct
    val subst =
        let val input = seg_k
            val output = seg_ch
        in Palatalize.palatalize_subst input output end

    fun nuc_context (Monophthong v) =
        if v = seg_i orelse v = seg_e orelse v = seg_e'
        then true
        else false
      | nuc_context (Diphthong (v1, v2)) = (v1 = seg_i)
      | nuc_context nuc = false

    val syll_context = Palatalize.mkPalPred nuc_context

    val sound_change =
        let val syllabism = let val syllF = Palatalize.mkSyllF subst
                                val pred = Predicate syll_context
                            in Syllabism (syllF, pred) end
            val name = "Palatalization of [k]"
        in mkSoundChange syllabism name end

end

val palatalize_k = PalatalizeK.sound_change

(* palatalization of [g] *)

structure PalatalizeG = struct
    val palatalize_g_subst =
        let val input = seg_g
            val output = seg_dg
        in Palatalize.palatalize_subst input output end

    fun nuc_context (Monophthong v) =
        if v = seg_i orelse v = seg_e orelse v = seg_e'
        then true
        else false
      | nuc_context (Diphthong (v1, v2)) = (v1 = seg_i)
      | nuc_context nuc = false

    val syll_context = Palatalize.mkPalPred nuc_context

    val palatalize_g =
        let val syllabism = let val syllF = Palatalize.mkSyllF palatalize_g_subst
                                val pred = Predicate syll_context
                            in Syllabism (syllF, pred) end
            val name = "Palatalization of [g]"
        in mkSoundChange syllabism name end
end

val palatalize_g = PalatalizeG.palatalize_g

(* proto-romance consonantal shift *)

local val sc1 = elide_m
      val sc2 = deasp
in val prRom_early_cons_shift = [sc1, sc2] end

local val sc1 = fortition_w
in val prRom_cons_shift = [sc1] end

local val sc1 = palatalize_t
    val sc2 = palatalize_d
in val dental_palatalizations = [sc1, sc2] end

local val sc1 = palatalize_g
      val sc2 = palatalize_k
in val velar_palatalizations = [sc1, sc2] end

(* proto-romance sound changes *)

local val consonantism_1 = prRom_early_cons_shift
      val vocalism = prRom_vowel_shift
      val consonantism_2 = prRom_cons_shift
      val palatalizations = dental_palatalizations @ velar_palatalizations
in val prRom_lang_shift = [consonantism_1, vocalism, consonantism_2, palatalizations] end

(* western romance vocalism *)

(* diphthongization of low-mid [epsilon] (PrRom) *)

fun diph_epsilon_subst nuc
    = case nuc of
          Monophthong v => if v = seg_e'
                           then Diphthong (seg_i, seg_e)
                           else nuc
        | _ => nuc

val diph_epsilon_rewrite = Nucleusism diph_epsilon_subst

fun is_stressed (Syllable (onset, nuc, cod, Stressed)) = true
  | is_stressed _ = false

fun is_open_syll (Syllable (onset, nuc, cod, stress))
    = case cod of
          ZeroCoda => true
        | _ => false

fun all_true [] x = true
  | all_true (f :: fs) x = (f x) andalso (all_true fs x)

val diph_epsilon = let val syllabism = let val p = all_true [is_stressed, is_open_syll]
                                       in mkSyllabism diph_epsilon_rewrite (Predicate p) end
                       val name = "Diphthongization of [\201\155]"
                   in mkSoundChange syllabism name end

(* western romance vowel shift *)

local val sc1 = diph_epsilon
in val WRom_vowel_shift = [sc1] end

(* western romance consonantism *)

(* degemination *)

structure Degemination = struct

end

(* lenition *)

structure Lenition = struct
    fun mkSubstF f onset =
        case onset of
            Onset cons => Onset (f cons)
          | OnsetM (cons1, cons2) => let val cons1' = f cons1 
                                     in OnsetM (cons1', cons2) end
          | _ => onset

    fun mkSyllF f syll =
        let val Syllable (on, nuc, cod, stress) = syll
            val on' = f on
        in Syllable (on', nuc, cod, stress) end

    local 
        fun context syll1 syll2 = 
            let fun context1 syll =
                    let val Syllable (on, nuc, cod, stress) = syll
                    in case cod of
                           ZeroCoda => true
                         | _ => false
                    end
                fun context2 syll = 
                    let val Syllable (on, nuc, cod, stress) = syll
                    in case on of 
                           Onset _ => true
                         | OnsetM _ => true
                         | _ => false
                    end
            in (context1 syll1) andalso (context2 syll2) end
    in 
        fun mkPWordF f pword =
            case pword of
                [] => pword
              | [x] => pword
              | [x, y] => if context x y
                          then [x, f y]
                          else pword
              | (x :: y :: xs) => if context x y
                                  then x :: mkPWordF f ((f y) :: xs)
                                  else x :: mkPWordF f (y :: xs)
    end

    fun mkLenition (lenite : consonant -> consonant) name =
        let val subst = mkSubstF lenite
            val syllF = mkSyllF subst
            val pWordF = mkPWordF syllF
        in SoundChange (pWordF, name) end
end

(* lenition i *)

structure LenitionI = struct
    fun lenite (Consonant (Voiceless, place, Stop)) = Consonant (Voiced, place, Stop)
      | lenite (Consonant (Voiced, place, Stop)) = Consonant (Voiced, place, NonSibil)
      | lenite (Consonant (Voiceless, place, Sibilant)) = Consonant (Voiced, place, Sibilant)
      | lenite cons = cons

    val sound_change = let val name = "Intervocalic Lenition I"
                       in Lenition.mkLenition lenite name end
end

val lenition_1 = LenitionI.sound_change

(* western romance consonantal shift *)

val WRom_cons_shift = [lenition_1]

(* western romance sound changes *)

local val vocalism = WRom_vowel_shift
      val consonantism = WRom_cons_shift
in val WRom_lang_shift = [vocalism, consonantism] end

(* old spanish vocalism *)

(* diphthongization of [epsilon] in closed syllables *)

val diph_epsilon_2 = let val syllabism = mkSyllabism diph_epsilon_rewrite (Predicate is_stressed)
                       val name = "Diphthongization of [\201\155] II"
                   in mkSoundChange syllabism name end

(* diphthongization of the low-mid back vowel *)

fun diph_open_o_subst nuc
    = case nuc of
          Monophthong v => if v = seg_o'
                           then Diphthong (seg_u, seg_e)
                           else nuc
        | _ => nuc

val diph_open_o_rewrite = Nucleusism diph_open_o_subst

val diph_open_o = let val syllabism = mkSyllabism diph_open_o_rewrite (Predicate is_stressed)
                      val name = "Diphthongization of [\201\148]"
                   in mkSoundChange syllabism name end

(* old spanish vowel shift *)

local val sc1 = diph_epsilon_2
      val sc2 = diph_open_o
in val OSp_vowel_shift = [sc1, sc2] end

(* old spanish consonantism *)

(* fronting of palatal affricates *)

structure FrontingPalatal = struct
    fun subst onset =
        case onset of
            Onset cons => if cons = seg_ch
                          then Onset seg_ts
                          else onset
          | _ => onset

    val rewrite = Onsetism subst

    val sound_change =
        let val syllabism = mkSyllabism rewrite NoContext
            val name = "Fronting of [t\202\131]"
        in mkSoundChange syllabism name end
end

val fronting_palatal = FrontingPalatal.sound_change

(* lenition ii *)

structure LenitionII = struct
    fun lenite (Consonant (Voiced, place, Stop)) = Consonant (Voiced, place, NonSibil)
      | lenite (Consonant (Voiceless, place, Sibilant)) = Consonant (Voiced, place, Sibilant)
      | lenite (Consonant (Voiceless, place, Affricate)) = Consonant (Voiced, place, Affricate)
      | lenite cons = cons

    val sound_change = let val name = "Intervocalic Lenition II"
                       in Lenition.mkLenition lenite name end
end

val lenition_2 = LenitionII.sound_change

(* old spanish consonantal shifts *)

local val sc1 = fronting_palatal
      val sc2 = lenition_2
in val OSp_cons_shift = [sc1, sc2] end

(* old spanish sound changes *)

local val vocalism = OSp_vowel_shift
      val consonantism = OSp_cons_shift
in val OSp_lang_shift = [vocalism, consonantism] end

(* modern spanish vocalism *)

(* modern spanish consonantism *)

(* betacism *)

structure Betacism = struct
    fun subst onset =
        case onset of
            Onset cons => if cons = seg_bh
                          then Onset seg_b
                          else onset
          | _ => onset

    val rewrite = Onsetism subst

    val sound_change =
        let val syllabism = mkSyllabism rewrite WordInit
            val name = "Betacism"
        in mkSoundChange syllabism name end
end

val betacism = Betacism.sound_change

(* deaffrication of [dg] *)

fun deaffric_dg_subst onset =
    case onset of
        Onset cons => if cons = seg_dg
                      then Onset seg_zh
                      else onset
      | _ => onset

val deaffric_dg_rewrite = Onsetism deaffric_dg_subst

val deaffric_dg = let val syllabism = mkSyllabism deaffric_dg_rewrite NoContext
                      val name = "Deaffrication of [d\202\146]"
                  in mkSoundChange syllabism name end

(* deaffrication of [ts] and [dz] *)

structure DeaffricationDent = struct
    fun deaffric_dent_subst cons =
        case cons of
            Consonant (voice, Dental, Affricate) => Consonant (voice, Dental, Sibilant)
          | _ => cons

    fun deaffric_dent_syll f syll =
        let val Syllable (on, nuc, cod, stress) = syll
            val on' = case on of
                          Onset cons => Onset (f cons)
                        | _ => on
            val cod' = case cod of
                           Codetta cons => Codetta (f cons)
                         | _ => cod
        in Syllable (on', nuc, cod', stress) end

    val deaffric_dent_syllabism =
        let val syllF = deaffric_dent_syll deaffric_dent_subst
        in Syllabism (syllF, NoContext) end

    val deaffric_dent =
        let val syllabism = deaffric_dent_syllabism
            val name = "Deaffrication of Dental Affricates"
        in mkSoundChange syllabism name end
end

val deaffric_dent = DeaffricationDent.deaffric_dent

(* devoicing of sibilants *)

structure DevoiceSibil = struct
    fun devoice_sibil_seg cons =
        case cons of
            Consonant (Voiced, place, Sibilant) => Consonant (Voiceless, place, Sibilant)
          | _ => cons

    fun devoice_sibil_syll f syll =
        let val Syllable (on, nuc, cod, stress) = syll
            val on' = case on of
                          Onset cons => Onset (f cons)
                        | _ => on
            val cod' = case cod of
                           Codetta cons => Codetta (f cons)
                         | _ => cod
        in Syllable (on', nuc, cod', stress) end

    val devoice_sibil_syllabism =
        let val syllF = devoice_sibil_syll devoice_sibil_seg
        in Syllabism (syllF, NoContext) end

    val devoice_sibil = let val syllabism = devoice_sibil_syllabism
                            val name = "Devoicing of Sibilants"
                        in mkSoundChange syllabism name end
end

val devoice_sibil = DevoiceSibil.devoice_sibil

(* desibilation of dental sibilants *)

structure Desibilation = struct
    fun subst cons =
        case cons of
            Consonant (voice, Dental, Sibilant) => Consonant (voice, Dental, NonSibil)
          | _ => cons

    fun syllF f syll =
        let val Syllable (on, nuc, cod, stress) = syll
            val on' = case on of
                          Onset cons => Onset (f cons)
                        | _ => on
            val cod' = case cod of
                           Codetta cons => Codetta (f cons)
                         | _ => cod
        in Syllable (on', nuc, cod', stress) end

    val syllabism = Syllabism (syllF subst, NoContext)

    val sound_change = let val name = "Desibilation of Dental Sibilants"
                       in mkSoundChange syllabism name end
end

val desibil_dent = Desibilation.sound_change

(* retraction of [sh] *)

fun retraction_sh_subst onset
    = case onset of
          Onset cons => if cons = seg_sh
                        then Onset seg_x
                        else onset
        | _ => onset

val retraction_sh_rewrite = Onsetism retraction_sh_subst

val retraction_sh = let val syllabism = mkSyllabism retraction_sh_rewrite NoContext
                        val name = "Retraction of [\202\131]"
                    in mkSoundChange syllabism name end

(* modern spanish consonantal shift *)

local val sc1 = betacism
in val Es_cons_shift = [sc1] end

local val sc1 = deaffric_dg
      val sc2 = devoice_sibil
      val sc3 = deaffric_dent
      val sc4 = desibil_dent
      val sc5 = retraction_sh
in val Es_sibil_shift = [sc1, sc2, sc3, sc4, sc5] end

(* modern spanish sound changes *)

val Es_lang_shift = [Es_cons_shift, Es_sibil_shift]

(* gramática histórica española *)

val historia = [ prRom_lang_shift
               , WRom_lang_shift
               , OSp_lang_shift
               , Es_lang_shift ]
