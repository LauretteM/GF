concrete CatAfr of Cat = CommonX - [Adv,AdV] ** open ResAfr, Prelude in {
  flags optimize=all_subs ;

  lincat

-- Tensed/Untensed

    S  = {s : Order => Str ; hasNeg : Bool } ;
--    QS = {s : QForm => Str} ;
    RS = {s : Gender => Number => Str ; hasNeg : Bool } ;
--    SSlash = {s : Order => Str} ** {c2 : Preposition} ;

-- Sentence

    Cl = Clause ;
--    ClSlash = Clause ** {c2 : Preposition} ;
--    Imp = {s : Polarity => ImpForm => Str} ;

-- Question

--    QCl = {s : ResAfr.Tense => Anteriority => Polarity => QForm => Str} ;
--    IP = {s : NPCase => Str ; n : Number} ;
--    IComp  = {s : Agr => Str} ; 
--    IDet   = {s : Gender => Str ; n : Number} ;
--    IQuant = {s : Number => Gender => Str} ;

-- Relative

    RCl = {s : ResAfr.Tense => Anteriority => Polarity => Gender => Number => Str ; hasNeg : Polarity => Bool } ;
    RP = {s : Str ; a : RAgr ; hasPrep : Bool } ;

-- Verb

    VP = ResAfr.VP ;
    VPSlash = ResAfr.VP ** {c2 : Preposition ; hasN2 : Bool ; isV3 : Bool } ;
    Comp = {s : Agr => Str ; s2 : Str } ; 

-- Adjective

    AP = {s : AForm => Str ; finNie : Bool ; s2 : Str } ; -- isPre : Bool} ; 

-- Noun

    CN = {s : NForm => Str ; g : Gender ; hasNwd : Bool ; finNie : Bool } ;
    NP = { s : NPCase => Str ;
           a : Agr ;
           finNie : Bool ;
           hasNwd : Bool ;
           isPerson : Bool  -- True if PN or Pron: "hy sien hom/Jan nie" vs "hy sien nie die man nie"
         } ;
    Pron = Pronoun ;

    Det = {s : Str ; n : Number ; isNeg : Bool } ; -- a : Adjf
    Quant = { s : Str ; isNeg : Bool } ;
--    Quant = {
--      s  : Bool => Number => Gender => Str ; 
--      sp : Number => Gender => Str ; 
--      a  : Adjf
--      } ;
--    Predet = {s : Number => Gender => Str} ;
    Num = {s : Str ; n : Number ; isNum : Bool} ;
    Card = {s : Str ; n : Number} ;
    Ord = {s : Str} ;

-- Numeral

    Numeral = {s : CardOrd => Str ; n : Number } ;
    Digits = {s : CardOrd => Str ; n : Number } ;

-- Structural

--    Conj = {s1,s2 : Str ; n : Number} ;
    Subj = {s : Str ; o : Order } ;
    Prep = {s : Str} ;

-- Open lexical classes, e.g. Lexicon

    V, VS, VQ, VA = ResAfr.VVerb ;
    VV = VVerb ** {om : Str ; te : Str} ;
    V2, V2A, V2S, V2Q = VVerb ** {c2 : Preposition; hasPrep2 : Bool} ;
--    V2V = VVerb ** {c2 : Preposition ; isAux : Bool} ;
    V3 = VVerb ** {c2 : Preposition ; c3 : Preposition ; hasPrep2 : Bool } ;

    A  = Adjective ;
    A2 = Adjective ** {c2 : Preposition} ;

    N  = Noun ;
    N2 = {s : NForm => Str ; g : Gender} ** {c2 : Preposition} ;
    N3 = {s : NForm => Str ; g : Gender} ** {c2,c3 : Preposition} ;
    PN = {s : NPCase => Str} ;
    
-- Not inherited from CommonX
    Adv = {s : Str ; isNwd : Bool ; isClause : Bool } ;
    AdV = {s : Str ; isNwd : Bool } ;

}
