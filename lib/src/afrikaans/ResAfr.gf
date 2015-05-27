--# -path=.:../abstract:../common

--1 Afrch auxiliary operations.
--
-- (c) 2009 Femke Johansson and Aarne Ranta

resource ResAfr = ParamX ** open Prelude in {

  flags optimize=all ;
  coding=utf8 ;

--2 For $Noun$

  param
    Case = Nom | Gen ;
    Gender = Masc | Fem ;

    NForm = NF Number Case ;

    NPCase = NPNom | NPAcc ;

  oper 
    Noun = {s : NForm => Str ; g : Gender} ;

    mkNoun : (_,_ : Str) -> Gender -> Noun = \sg,pl,g -> {
      s = table {
        NF Sg Nom => sg ;
        NF Sg Gen => add_s sg ; 
        NF Pl Nom => pl ;
        NF Pl Gen => add_s pl
        } ;
     g = g
     } ;

--Volgens Afrikaanse Woordelys & Spelreëls, 2009
--
-- Uitsonderings wat in die leksikon hanteer moet word:
-- 
-- * enige uitsonderings wat in die AWS vermeld word
-- * enige woord wat in die mv. "te" kry, soos lig, ligte
-- * enige meerlettergrepige woord wat met "ie","ël","el","em","en","ng","ior","er","êr","erd","aar","aard","ier"
--    eindig wat nie 'n "s" in die mv. kry nie
-- * eiename wat nie reëlmatig verbuig
-- * woorde met wisselvorme in die mv. moet as sinonieme in die leksikon hanteer word
--
    regNoun : Str -> Noun = \s -> case s of {
      _ + #cons + ("i" | "o" | "u" ) => mkNoun s (s + "'s") Masc ; --ski, ski's --R13.7
      #cons* + ("ie" | "oe") =>mkNoun s (s + "ë") Masc ; --knie, knieë --R13.10
      #cons* + ("ee") =>mkNoun s (init s + "ë") Masc ; --fee, feë --R13.10
      #cons* + "a" => mkNoun s (s + "'s") Masc ; --ma, ma's R13.7
      _ + ("a" | "e" | "ie" | "ee" | "é" | "ê" | "ô") => mkNoun s (s + "s") Masc ; --gogga, goggas --R13.5
      
      b + v@("oo") + "g" => mkNoun s (b + init v + "ë") Masc ; --boog, boë --R13.11
      b + v@("e"|"ie"|"o"|"oe") + "g" => mkNoun s (b + v + "ë") Masc ; --kroeg, kroeë --R13.11
      b + v@("aa") + "g" => mkNoun s (b + init v + "e") Masc ; --kraag, krae --R13.11
      b + v@("a") + "g" => mkNoun s (b + v + "e") Masc ; --dag, dae --R13.11
      b + v@("ei"|"eu"|"oe"|"ou"|"ie"|"y"|"ui") + "g" => mkNoun s (b + v + "e") Masc ; --tuig, tuie --R13.1
      
      _ + ("oir" | "ion" | "je") => mkNoun s (s + "s") Masc ; --uit Nederlandse reël
      
      _ + ("rm" | "lm") => mkNoun s (s + "s") Masc ; --R13.3
      
      ? + ? + ? + _ + 
        ("ël" |"el" | "em" | "um" | "ing" | "or" | "ior" | "er" | "êr" | "erd" | "aar" | "aard" | "ier") => -- unstressed
                                            mkNoun s (s + "s") Masc ; --R13.3
      
      ? + ? + _ + (#cons + "en") => mkNoun s (s + "s") Masc ; --R13.3
        
      
      _ +                     ("i"|"u")  => mkNoun s (s + "e") Masc ; --R13.4
      b + v@("aa"|"ee"|"oo"|"uu") + c@?  => mkNoun s (b + shortVoc v c + "e") Masc ; --brood, brode --R13.1
      b + ("ei"|"eu"|"oe"|"ou"|"ie"|"y"|"ui") + ? => mkNoun s (endCons s + "e") Masc ; --geluid, geluide --R13.1
      b + v@("a"|"e"|"i"|"o"|"u" ) + "f" => mkNoun s (b + v + "ww" + "e") Masc ; --stof, stowwe --R13.1
      b + v@("a"|"e"|"i"|"o"|"u" ) + c@? => mkNoun s (b + v + c + c + "e") Masc ; --dak, dakke --R13.1
      _ => mkNoun s (endCons s + "e") Masc --R13.1
      } ;

    regNounG : Str -> Gender -> Noun = \s,g -> {
      s = (regNoun s).s ;
      g = g
      } ;

    shortVoc : Str -> Str -> Str = \v,s -> init v + endCons s ;

    endCons : Str -> Str = \s -> case s of {
      _ + ("ts" |"rs" | "ls" | "ds" | "ns" | "ms") => s ;
      b + "f" => b + "w" ;
      _ => s
      } ;

    vowel : pattern Str = #("a"|"e"|"i"|"o"|"u") ;
    cons : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"z") ;
    dupCons : pattern Str = #("b"|"d"|"f"|"g"|"k"|"l"|"m"|"n"|"p"|"r"|"s"|"t") ;

    add_s : Str -> Str = \s -> s ++ "se";

  param 
    AForm = APred | AAttr | AGen ;
    
  oper
    Adjective = {s : Degree => AForm => Str} ;

    mkAdjective : (_,_,_,_,_ : Str) -> Adjective = \ap,aa,ag,ac,as -> {
      s = table {
        Posit  => table {APred => ap ; AAttr => aa ; AGen => ag} ; 
        Compar => table {APred => ac ; AAttr => ac ; AGen => ac + "s"} ; ----
        Superl => table {APred => as ; AAttr => as ; AGen => as + "s"}   ----
        }
      } ;
      
--Volgens Afrikaanse Morfologie: Capital Exemplaria, Combrinck, 1990
    regAdjective : Str -> Adjective = \s ->  ----
      let 
        se : Str = case s of {
          b + v@("aal"|"baar"|"eel"|"loos") => b + init (init v) + last v + "e" ; --p288
          _ + ("agtig"|"ant"|"ent"|"êr"|"ies"|"ig"|"lik"|"matig"|"s") => s + "e" ; --p288
          b + "ief" => b + "iewe" ; --p288
          
          --b + ("ei"|"eu"|"oe"|"ou"|"ie"|"y"|"ui") + ?  => endCons s + "e" ;
          b + v@("ou"|"y") + "d"  => b + v + "e" ; --koud, koue / wyd, wye
          
          --b + v@("oo"|"ee") + "d" => b + init v + "ë" ; --leeg, leë
          b + v@("oo"|"ee") + ("g"|"d") => b + init v + "ë" ; --leeg, leë
          b + v@("e"|"ie"|"o"|"oe") + "g" => b + v + "ë" ; --moeg, moeë
          b + v@("aa") + "g" => b + init v + "e" ; --vaag, vae
          b + v@("a") + "g" => b + v + "e" ; --kan nog nie aan 'n voorbeeld dink nie
          
          b + v@("aa"|"ee"|"oo"|"uu") + "r" => s ; --duur, duur
          b + v@("aa"|"ee"|"oo"|"uu") + c@#cons => b + shortVoc v c + "e" ; --gaaf, gawe
          b + v@("a"|"e"|"i"|"o"|"u" ) + "f" => b + v + "ww" + "e" ; --grof, growwe
          --b + v@("a"|"e"|"i"|"o"|"u" ) + c@? => b + v + c + c + "e" ; --stom, growwe
          _ + "d" => s + "e" ; --p286
          _ => s 
          } ;
        ser : Str = case se of {
          b + v@("aa"|"ee"|"oo"|"uu") + "r" => se + "der" ;
          b + v@("a"|"i"|"o"|"u" ) + c@#cons => b + v + c + c + "er" ; --dom, dommer
          _ + "r" => se + "der" ;
          _ + "ë" => se + "r" ;
          _ + "e" => se + "r" ;
          _ => se + "er"
          } ;
        sst : Str = case s of {
          _ + "s" => s + "te" ;
          _ => s + "ste"
          } ;
      in
      mkAdjective s se (s + "s") ser sst ;
      
      
    semregAdjective : Str -> Str -> Adjective = \ap,aa -> mkAdjective ap aa (ap + "s") (aa + "r") (ap + "ste") ;
      
      --shortVoc : Str -> Str -> Str = \v,s -> init v + endCons s ;

  param 
    VForm =         --!
       VInf      -- wees
     | VPres     -- is 
     | VPast     -- was  --# notpresent
     | VPerf     -- gewees
     ;

--    VPart = VPart_aan | VPart_af | VPart_be ;

  oper
    Verb : Type = {s: VForm => Str};
    
  mkVerb : (_,_,_,_ : Str) -> 
    	Verb = \aai, aaien, aaide, geaaid -> {
    	s = table {
    		VInf        => aaien; -- hij/zij/het/wij aaien
    		VPres       => aai;   -- ik aai
    		VPast       => aaide; -- ik aaide  --# notpresent --!afr!       lyk vir nou soos VPres
    		VPerf       => geaaid -- ik heb geaaid 
    	}
    };
    
  regVerb : Str -> Verb = \s -> mkVerb s s s ("ge"+s) ;

  irregVerb : (kan, kon : Str) -> Verb = \kan,kon ->
    mkVerb kan kan kon kon ;


-- To add a prefix (like "ein") to an already existing verb.

  prefixV : Str -> VVerb -> VVerb = \ein,verb ->
    let
      vs = verb.s ;
      einb : Bool -> Str -> Str = \b,geb -> 
        if_then_Str b (ein + geb) geb ;
    in
    {s = table {
      f@(VInf | VPerf) => ein + vs ! f ; ---- TODO: eingegeven
      f       => vs ! f
      } ;
     prefix = ein ;
     vtype = verb.vtype ;
     isPref = True ;
     isAux = verb.isAux 
     } ;
--  zijn_V : VVerb = {
--    s = table {
--       VInf      => "wees" ;
--       VPres     => "is" ; 
--       VPast     => "was" ; --# notpresent
--       VPerf     => "gewees" 
--       } ;
--    aux = VZijn ;
--    prefix = [] ;
--    vtype = VAct ;
--    } ;

  het_V : VVerb = {
    s = table {
       VInf      => "hê" ;
       VPres     => "het" ; 
       VPast     => "het" ; --# notpresent
       VPerf     => "gehad" 
       } ;
    prefix = [] ;
    vtype = VAct ;
    isPref = False ;
    isAux = False
    } ;

  Pronoun : Type = {
      nom : Str ; -- hy
      acc : Str ; -- hom
      poss : Str ; -- sy
      substposs : Str ; -- syne
      a : Agr
    } ;

  sal_V : VVerb = {
    s = table {
       VInf      => "sal" ;
       VPres     => "sal" ; 
       VPast     => "sou" ; --# notpresent
       VPerf     => "sou" --!afr!	perfektum moet hom soos past gedra
       } ;
    prefix = [] ;
    vtype = VAct ;
    isPref = False ;
    isAux = False 
    } ;

--  word_V : VVerb = {
--    s = table {
--       VInf      => "word" ;
--       VPres     => "word" ; 
--       VPast     => "word" ; --# notpresent
--       VPerf     => "geword" 
--       } ;
--    aux = VHebben ;
--    prefix = [] ;
--    vtype = VAct ;
--    } ;

  mkPronoun : (x1,x2,x3,x4 : Str) -> Gender -> Number -> Person -> Pronoun = 
      \hy,hom,sy,syne,g,n,p -> {
         nom = hy ;
                acc = hom ;
                poss = sy ;
         substposs  = syne ;
         a = {g = g ; n = n ; p = p} 
  } ;

--    het_Pron : Pronoun = mkPronoun "dit" "dit" "hy" "hy" "hom" "sy" "syne" Neutr Sg P3 ;


-- Complex $CN$s, like adjectives, have strong and weak forms.

param
    Adjf = Strong | Weak ;
    VAux = VHebben | VZijn ;
    VType = VAct | VRefl ;

  oper 
    VVerb = Verb ** {prefix : Str ; vtype : VType ; isPref : Bool ; isAux : Bool } ;
    v2vv : Verb -> VVerb = \v -> {s = v.s ; prefix = [] ; vtype = VAct ; isPref = False ; isAux = False } ;

---- The order of sentence is depends on whether it is used as a main
---- clause, inverted, or subordinate.

  oper 
    Preposition = Str ;
    appPrep : Preposition -> (NPCase => Str) -> Str = \p,np -> p ++ np ! NPAcc ; ----

  param  
    Order = Main | Inv | Sub ;

  oper
    vForm : Tense -> VForm = \t -> case t of {
      Pres  => VPres 
     ;  Fut   => VPres  --# notpresent
     ;  Past | Cond => VPast   -- Fut and Cond affect zullen --# notpresent
      } ;

--2 For $Relative$

--  param 
--    RAgr = RNoAg | RAg Number Person ;

--2 For $Numeral$

--  param
--    CardOrd = NCard Gender Case | NOrd AForm ;
--    DForm = DUnit  | DTeen  | DTen ;

--2 Transformations between parameter types

  oper Agr : Type = {g : Gender ; n : Number ; p : Person} ;

  oper
    agrP3 : Number -> Agr = agrgP3 Masc ;

    agrgP3 : Gender -> Number -> Agr = \g,n -> 
      {g = g ; n = n ; p = P3} ;

-- Used in $NounAfr$.

--    agrAdj : Gender -> Adjf -> NForm -> AForm = \g,a,n ->
--      case <a,g,n> of {
--        <Strong,Neutr,NF Sg _> => APred ;
--        _ => AAttr
--        } ;

  oper VP : Type = {
      s  : VVerb ;
      n : Str ;
      hasPrep : Bool ;
      nPerson : Bool ;
      adv : Str ; -- vinnig
      adV : Str ; -- altyd/nooit
      double1 : Bool ; -- insert first "nie" if sentence polarity is negative, because certain slots are filled (hy loop nie/hy loop *nie* goed nie)
      double2 : Bool ;  -- always insert second "nie", because of n-word (niemand *nie*), unless...
      subNeg : Bool ;   -- a final "nie" is present in a subclause
      inf : Str * Bool ;
      ppart : Str * Bool ;
      ext : Str 
--      n0 : Agr => Str ;      -- je
--      n2 : Agr => Str ;      -- je vrouw
--      a2 : Str ;             -- vandaag
--      isAux : Bool ;         -- is a double infinitive
--      inf : Str * Bool ;     -- sagen (True = non-empty)
--      ext : Str              -- dass sie kommt
  } ;
  
    predV : VVerb -> VP = \vverb -> {
       s = vverb ;
       n = [] ;
       hasPrep = False ;
       nPerson = False ;
       adv = [] ;
       adV = [] ;
       double1 = vverb.isPref ;
       double2 = False ;
       subNeg = False ;
       inf = <[],False> ;
       ppart = <[],False> ;
       ext = []
    } ;
    
  vvPred : VVerb -> VP -> VP = \vv,vp ->  {
       s = vv ;
       n = vp.n ;
       hasPrep = vp.hasPrep ;
       nPerson = vp.nPerson ;
       adv = vp.adv ;
       adV = vp.adV ;
       double1 = True ;
       double2 = vp.double2 ;
       subNeg = vp.subNeg ;
       inf = <(vp.s.s!VInf) ++ vp.inf.p1,True> ;
       ppart = <(vp.s.s!VPerf) ++ vp.ppart.p1,True> ;
       ext = []
     } ;

--  predVGen : Bool -> VVerb -> VP = \isAux, verb -> {
--    s = verb ;
--    a1  : Polarity => Str = negation ;
--    n0  : Agr => Str = \\a => case verb.vtype of {
--      VAct  => [] ;
--      VRefl => reflPron ! a
--      } ;
--    n2  : Agr => Str = \\a => [] ;
--    a2  : Str = [] ;
--    isAux = isAux ; ----
--    inf : Str * Bool = <[],False> ;
--    ext : Str = []
--    } ;

  -- first "nie"
  negation : Polarity => Str = 
      table { Pos => [] ; Neg => "nie" } ; -- present if sentence negation requires it : hy loop nie[uNeg] / hy loop nie[iNeg] goed nie[uNeg]
  
  -- second "nie"
  negation2 : Bool -> Bool -> Polarity => Bool = \subNeg,b ->  
      case subNeg of { True => \\p => False ;
                       False => table { Pos => b ;             -- [uNeg] : because of n-word
                                        Neg => True }          -- [uNeg] : either because of n-word or because of sentence negation
                     } ;
  
  putNeg : Bool -> Str = \b -> case b of { True => "nie" ; False => [] } ;

-- Extending a verb phrase with new constituents

--  insertObj : (Agr => Str) -> VP -> VP = insertObjNP False ;

  insertObjNP : Bool -> Bool -> Str -> VP -> VP = \isNeg, isPerson, obj,vp -> {
        s = vp.s ;
        n = obj ;
        hasPrep = vp.hasPrep ;
        nPerson = isPerson ;
        adv = vp.adv ;
        adV = vp.adV ;
        double1 = case <isPerson,vp.hasPrep> of { <False,_> => True ;
                                                  <True,True> => True ; 
                                                  <True,False> => vp.double1 }  ;
        double2 = case isNeg of { True => True ; False => vp.double2 } ;
        subNeg = vp.subNeg ;
        inf = vp.inf ;
        ppart = vp.ppart ;
        ext = vp.ext
--    a1 = vp.a1 ;
--    n0 = \\a => case isPron of {True  => obj ! a ; _ => []} ++ vp.n0 ! a;
--    n2 = \\a => case isPron of {False => obj ! a ; _ => []} ++ vp.n2 ! a;
--    a2 = vp.a2 ;
--    isAux = vp.isAux ;
--    ext = vp.ext
    } ;

  insertAdV : Str -> Polarity -> VP -> VP = \adv,p,vp -> {
    s = vp.s ;
    n = vp.n ;
    hasPrep = vp.hasPrep ;
    nPerson = vp.nPerson ;
    adv = vp.adv ; -- ver
    adV = adv ++ vp.adV ;    -- altyd/nooit
    double1 = True ;
    double2 = case p of 
            { Neg => True ;
              Pos => vp.double2
            } ;
    subNeg = vp.subNeg ;
    inf = vp.inf ;
    ppart = vp.ppart ;
    ext = vp.ext
--    a1 = \\a => adv ++ vp.a1 ! a ; -- immer nicht
--    n0 = vp.n0 ;
--    n2 = vp.n2 ;
--   a2 = vp.a2 ;
--    isAux = vp.isAux ;
--    inf = vp.inf ;
--    ext = vp.ext
    } ;

  insertAdv : Str -> VP -> VP = \adv,vp -> {
    s = vp.s ;
    n = vp.n ;
    hasPrep = vp.hasPrep ;
    nPerson = vp.nPerson ;
    adv = adv ++ vp.adv ;
    adV = vp.adV ;
    double1 = True ;
    double2 = vp.double2 ;
    subNeg = vp.subNeg ;
    inf = vp.inf ;
    ppart = vp.ppart ;
    ext = vp.ext
--    n0 = vp.n0 ;
--    n2 = vp.n2 ;
--    a2 = vp.a2 ++ adv ;
--    isAux = vp.isAux ;
--    inf = vp.inf ;
--    ext = vp.ext
    } ;

  insertExtrapos : Bool -> Str -> VP -> VP = \isNeg,ext,vp -> {
    s = vp.s ;
    n = vp.n ;
    hasPrep = vp.hasPrep ;
    nPerson = vp.nPerson ;
    adv = vp.adv ;
    adV = vp.adV ;
    double1 = vp.double1 ;
    double2 = vp.double2 ;
    subNeg = isNeg ;
    inf = vp.inf ;
    ppart = vp.ppart ;
    ext = vp.ext + ext
--    a1 = vp.a1 ;
--    n0 = vp.n0 ;
--    n2 = vp.n2 ;
--    a2 = vp.a2 ;
--    isAux = vp.isAux ;
--    inf = vp.inf ;
--    ext = vp.ext ++ ext
    } ;

  insertPPart : Str -> VP -> VP = \ppart,vp -> {
    s = vp.s ;
    n = vp.n ;
    hasPrep = vp.hasPrep ;
    nPerson = vp.nPerson ;
    adv = vp.adv ;
    adV = vp.adV ;
    double1 = vp.double1 ;
    double2 = vp.double2 ;
    subNeg = vp.subNeg ;
    inf = vp.inf ;
    ppart = <vp.ppart.p1 ++ ppart,True> ;
    ext = vp.ext
  } ;

  insertInf : Str -> VP -> VP = \inf,vp -> {
    s = vp.s ;
    n = vp.n ;
    hasPrep = vp.hasPrep ;
    nPerson = vp.nPerson ;
    adv = vp.adv ;
    adV = vp.adV ;
    double1 = vp.double1 ;
    double2 = vp.double2 ;
    subNeg = vp.subNeg ;
    inf = <inf ++ vp.inf.p1, True> ;
    ppart = vp.ppart ;
    ext = vp.ext
--    s = vp.s ;
--    a1 = vp.a1 ;
--    n0 = vp.n0 ;
--    n2 = vp.n2 ;
--    a2 = vp.a2 ;
--    isAux = vp.isAux ; ----
--    inf = <inf ++ vp.inf.p1, True> ;
--    ext = vp.ext
    } ;

-- For $Sentence$.

  Clause : Type = {
    s : Tense => Anteriority => Polarity => Order => Str ;
    hasNeg : Tense => Anteriority => Polarity => Bool
    } ;
    
    orBool : Bool -> Bool -> Bool = \a,b -> 
                case a of { True => True ;
                            False => case b of { True => True ; False => False } } ;

    mkClause : Str -> Agr -> Bool -> VP -> Clause = \subj,agr,n,vp -> 
        let 
            n2 = (negation2 vp.subNeg (orBool n vp.double2))
        in 
        {
        hasNeg = \\t,a,p => n2!p ;
        s = \\t,a,p,o => 
            let
                vform = vForm t ;
                vperf = vp.s.s ! VPerf ;
                -- verb : <sal,ophou> , <hou,[]>, <het,opgehou>
                verb : Order => (Str * Str) = case <t,a> of {
                    <Fut,Simul>  => \\_ => <sal_V.s ! VPres, vp.s.s ! VInf ++ vp.inf.p1> ;
                    <Fut,Anter>  => \\_ => <sal_V.s ! VPres, vperf ++ vp.ppart.p1 ++ het_V.s ! VPres> ;
                    <Cond,Simul> => \\_ => <sal_V.s ! VPres, vp.s.s ! VInf ++ vp.inf.p1> ;
                    <Cond,Anter> => \\_ => <sal_V.s ! VPres, vperf ++ vp.ppart.p1 ++ het_V.s ! VPres> ;
                    <_, Anter>   => table { Sub => case (vp.s.isAux) of
                                                     { True => <(vp.s.s!VPerf), vp.ppart.p1 ++ het_V.s ! VPres> ;
                                                       False => <vperf, het_V.s ! VPres> } ;
                                            _ => case (vp.s.isAux) of
                                                     { True => <(vp.s.s!VPerf), vp.ppart.p1 ++ het_V.s ! VPres> ;
                                                       False => <het_V.s ! VPres, vperf> } 
                                          } ;
                    <_, Simul>   => table { Sub  => <vp.s.s ! VInf, [] > ;
                                            _ => <vp.s.s ! vform, vp.inf.p1>
                                          } 
                    } ;
                neg : Polarity => (Str * Str) = 
                        case <t,a> of {
                            <(Pres|Past),  Simul> => \\p => < case vp.double1 of {True => negation!p ; False => []} , putNeg (n2!p)  > ;
                            <_,_> => \\p => <negation!p, putNeg (n2!p) > 
                        } ;
                pref = case <t,a,o> of {
                    <Fut,Simul,(Main|Sub)> => [] ;
                    <Fut,Simul,Inv> => [] ;
                    <Cond,Simul,_> => [] ;
                    <Fut,Anter,_> => [] ;
                    <Cond,Anter,_> => [] ;
                    <_,Simul,Main> => vp.s.prefix ;
                    <_,Simul,_> => [] ;
                    <_,_,_> => []
                    } ;
            in
                case o of {
                    Main =>
                    case <vp.nPerson,vp.hasPrep> of {
                            <True,True> =>   subj ++ (verb!Main).p1 ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ vp.n ++ (verb!Main).p2 ++ vp.ext ++ (neg!p).p2 ;
                            <True,False> =>  subj ++ (verb!Main).p1 ++ vp.n ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ (verb!Main).p2 ++ vp.ext ++ (neg!p).p2 ;
                            <False,True> =>  subj ++ (verb!Main).p1 ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ vp.n ++ (verb!Main).p2 ++ vp.ext ++ (neg!p).p2;
                            <False,False> => subj ++ (verb!Main).p1 ++ (neg!p).p1 ++ vp.adV ++ vp.n ++ vp.adv ++ pref ++ (verb!Main).p2 ++ vp.ext ++ (neg!p).p2
                            
                            } ;
                    Sub => 
                    case <vp.nPerson,vp.hasPrep> of {
                            <True,True> =>   subj ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ vp.n ++ (verb!Sub).p1 ++ (verb!Sub).p2 ++ vp.ext ++ (neg!p).p2 ;
                            <True,False> =>  subj ++ pref ++ vp.n ++ (negation!p) ++ vp.adV ++ vp.adv ++ (verb!Sub).p1 ++ (verb!Sub).p2 ++ vp.ext ++ (neg!p).p2 ;
                            <False,True> =>  subj ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ vp.n ++ (verb!Sub).p1 ++ (verb!Sub).p2 ++ vp.ext ++ (neg!p).p2;
                            <False,False> => subj ++ pref ++ vp.n ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ (verb!Sub).p1 ++ (verb!Sub).p2 ++ vp.ext ++ (neg!p).p2
                            
                            } ;
                    -- to finish (copied from Main)
                    Inv =>
                    case <vp.nPerson,vp.hasPrep> of {
                            <True,True> =>   subj ++ (verb!Inv).p1 ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ vp.n ++ (verb!Inv).p2 ++ vp.ext ++ (neg!p).p2 ;
                            <True,False> =>  subj ++ (verb!Inv).p1 ++ vp.n ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ (verb!Inv).p2 ++ vp.ext ++ (neg!p).p2 ;
                            <False,True> =>  subj ++ (verb!Inv).p1 ++ (neg!p).p1 ++ vp.adV ++ vp.adv ++ pref ++ vp.n ++ (verb!Inv).p2 ++ vp.ext ++ (neg!p).p2;
                            <False,False> => subj ++ (verb!Inv).p1 ++ (neg!p).p1 ++ vp.adV ++ vp.n ++ vp.adv ++ pref ++ (verb!Inv).p2 ++ vp.ext ++ (neg!p).p2
                            }
                }
    } ;

--  mkClause : Str -> Agr -> VP -> Clause = \subj,agr,vp -> {
--      s = \\t,a,b,o =>
--        let
--          ord   = case o of {
--            Sub => True ;  -- glue prefix to verb
--            _ => False
--            } ;
--          vform = vForm t ;
--          auxv = (auxVerb vp.s.aux).s ;
--          vperf = vp.s.s ! VPerf ;
--          verb : Str * Str = case <t,a> of {
--            <Fut|Cond,Simul>  => <sal_V.s ! vform, vp.s.s ! VInf> ; --# notpresent
--            <Fut|Cond,Anter>  => <sal_V.s ! vform, vperf ++ auxv ! VInf> ; --# notpresent
--            <_,       Anter>  => <auxv ! vform,       vperf> ; --# notpresent
--            <_,       Simul>  => <vp.s.s ! vform,     []>
--            } ;
--          fin   = verb.p1 ;
--          neg   = vp.a1 ! b ;
--          obj0  = vp.n0 ! agr ;
--          obj   = vp.n2 ! agr ;
--          compl = obj0 ++ neg ++ obj ++ vp.a2 ++ vp.s.prefix ;
--          inf   = 
--            case <vp.isAux, vp.inf.p2, a> of {                  --# notpresent
--              <True,True,Anter> => vp.s.s ! VInf ++ vp.inf.p1 ; --# notpresent
--              _ =>                                              --# notpresent
--                 vp.inf.p1 ++ verb.p2 ++ neg
--              }                                                 --# notpresent
--              ;
--          extra = vp.ext ;
--          inffin = 
--            case <a,vp.isAux> of {                              --# notpresent
--              <Anter,True> => fin ++ inf ; -- double inf   --# notpresent
--              _ =>                                              --# notpresent
--              inf ++ fin              --- or just auxiliary vp
--            }                                                   --# notpresent
--        in
--        case o of {
--          Main => subj ++ fin ++ compl ++ inf ++ extra ;
--          Inv  => fin ++ subj ++ compl ++ inf ++ extra ;
--          Sub  => subj ++ compl ++ inffin ++ extra
--          }
--    } ;

--  auxVerb : VAux -> VVerb = \a -> case a of {
--    VHebben => hebben_V ;
--    VZijn   => zijn_V 
--    } ;

--  infVP : Bool -> VP -> ((Agr => Str) * Str * Str) = \isAux, vp -> 
--    <
--     \\agr => vp.n0 ! agr ++  vp.n2 ! agr ++  vp.a2,
--     vp.a1 ! Pos ++ 
--     if_then_Str isAux [] "om" ++ "te" ++ vp.s.s ! VInf,
--     vp.inf.p1 ++ vp.ext
--    > ;

--  useInfVP : Bool -> VP -> Str = \isAux,vp ->
--    let vpi = infVP isAux vp in
--    vpi.p1 ! agrP3 Sg ++ vpi.p3 ++ vpi.p2 ;

--  reflPron : Agr => Str = table {
--    {n = Sg ; p = P1} => "my" ;	--afr
--    {n = Sg ; p = P2} => "jou" ;	--afr
--    {n = Sg ; p = P3} => "hom" ;	--afr
--    {g = masculine ; n = Sg ; p = P3} => "hom" ;	--afr
--    {g = feminine ; n = Sg ; p = P3} => "haar" ;	--afr
--    {n = Pl ; p = P1} => "ons" ;	--afr
--    {n = Pl ; p = P2} => "julle" ;	--afr
--    {n = Pl ; p = P3} => "hulle"	--afr
--    } ;

  conjThat : Str = "dat" ;

--  conjThan : Str = "as" ;	--afr

--  conjAgr : Agr -> Agr -> Agr = \a,b -> {
--      g = Neutr ; ----
--      n = conjNumber a.n b.n ;
--      p = conjPerson a.p b.p
--      } ;

-- The infinitive particle "zu" is used if and only if $vv.isAux = False$.
 
--  infPart : Bool -> Str = \b -> if_then_Str b [] "te" ;

--  mkDet : Str -> Str -> Number -> {s,sp : Gender => Str ; n : Number ; a : Adjf} =
--    \deze,dit,n -> {
--      s  = \\g => case <n,g> of {<Sg,Neutr> => dit ; _ => deze} ;
--      sp = \\g => case <n,g> of {<Sg,Neutr> => dit ; _ => deze} ;
--      n = n ;
--      a = Weak
--      } ;

--  mkQuant : Str -> Str -> {
--    s  : Bool => Number => Gender => Str ; 
--    sp : Number => Gender => Str ; 
--    a  : Adjf
--    } = 
--    \deze,dit -> {
--      s  = \\_ ,n,g => case <n,g> of {<Sg,Neutr> => dit ; _ => deze} ;
--      sp = \\   n,g => case <n,g> of {<Sg,Neutr> => dit ; _ => deze} ;
--      a = Weak
--      } ;

--  mkPredet : Str -> Str -> {s : Number => Gender => Str} = 
--    \deze,dit -> {
--      s  = \\n,g => case <n,g> of {<Sg,Neutr> => dit ; _ => deze}
--      } ;

  mkNP : Str -> Gender -> Number -> Bool -> {s : NPCase => Str ; a : Agr ; isNeg : Bool ; isPerson : Bool } =
    \s,g,n,b -> {
      s = \\_ => s ;
      a = agrgP3 g n ;
      isNeg = b ;
      isPerson = False
      } ;

  auxVV : VVerb -> VVerb ** {om : Str ; te : Str } = \v -> {
    s = v.s ;
    vtype = v.vtype ;
    prefix = v.prefix ;
    isPref = v.isPref ;
    isAux = True ;
    om = [] ;
    te = []
    } ;

--  heavyNP : 
--    {s : NPCase => Str ; a : Agr} -> {s : NPCase => Str ; a : Agr ; isPron : Bool} = \np ->
--    np ** {isPron = False} ;

  mkAdV : Str -> Polarity -> { s : Str ; p : Polarity }  = \str,p -> {s = str ; p = p } ;

}
