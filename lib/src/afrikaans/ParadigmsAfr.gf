--# -path=.:../common:../abstract:../../prelude

--1 Afrch Lexical Paradigms
--
-- Aarne Ranta 2009
--
-- This is an API for the user of the resource grammar 
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
-- 
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$. 
--
-- The structure of functions for each word class $C$ is the following:
-- first we give a handful of patterns that aim to cover all
-- cases, from the most regular (with just one argument) to the worst. 
-- The name of this function is $mkC$.
-- 
-- There is also a module [``IrregAfr`` IrregAfr.gf] 
-- which covers irregular verbs.


resource ParadigmsAfr = open 
  (Predef=Predef), 
  Prelude, 
  ResAfr,
  CatAfr
  in 
{
--2 Parameters 

-- To abstract over gender names, we define the following identifiers.

oper
  masculine : Gender ; 
  feminine  : Gender ; 

--2 Nouns

  mkN : overload {
    mkN : (muis : Str) -> N ;   -- muis-muise, with some predictable exceptions
    mkN : (bit : Str) -> Gender -> N ; -- if gender is not predictable
    mkN : (vrou,vrouens : Str) -> Gender -> N ; -- worst-case for nouns
  } ;

-- Relational nouns need a preposition. The most common is "van".

  mkN2 : overload {
    mkN2 : N -> N2 ;        -- relational noun with preposition van
    mkN2 : N -> Prep -> N2  -- other preposition than van
    } ;   

-- Use the function $mkPrep$ or see the section on prepositions below to  
-- form other prepositions.
-- Some prepositions are moreover constructed in [StructuralAfr StructuralAfr.html].
--
-- Three-place relational nouns ("die Verbindung von x nach y") need two prepositions.

  mkN3 : N -> Prep -> Prep -> N3 ; -- e.g. afstand + van + naar

--3 Proper names and noun phrases

  mkPN : overload {
    mkPN : Str -> PN ; -- Johannesburg
    mkPN : Str -> Gender -> PN ; -- Marie
    } ;

--2 Adjectives

  mkA : overload {
    mkA : (vers : Str) -> A ; -- regular adjective
    mkA : (sag, sagte : Str) -> A ; --"semi-irregular"
    mkA : (goed,goeie,goeds,beter,beste : Str) -> A ; -- irregular adjective
    } ;

-- Invariable adjective are a special case. 

  invarA : Str -> A ;            -- adjective with just one form

-- Two-place adjectives are formed by adding a preposition to an adjective.

  mkA2 : A -> Prep -> A2 ;  -- e.g. getroud + met

--2 Adverbs

-- Adverbs are formed from strings.

  mkAdv : overload {
    mkAdv : Str -> Adv ;
    mkAdv : Str -> Polarity -> Adv ;
  } ;

--2 Prepositions

-- A preposition is formed from a string.

  mkPrep : Str -> Prep ;

--2 Verbs

  mkV : overload {
    mkV : (aaien : Str) -> V ;  -- regular verb
    mkV : (breken,brak,gebroken : Str) -> V ; -- theme of irregular verb
    mkV : (breken,brak,braken,gebroken : Str) -> V ; -- also past plural irregular
    mkV : (aai,aait,aaien,aaide,aaide,aaiden,geaaid : Str) -> V ; -- worst-case verb

-- To add a movable suffix e.g. "auf(fassen)".

    mkV : Str -> V -> V -- add movable suffix, e.g. af + stappen
    } ;

--3 Three-place verbs

-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 : overload {
    mkV3 : V -> V3 ;                  -- geven,(accusative),(dative)
    mkV3 : V -> Prep -> V3 ;          -- sturen,(accusative),naar
    mkV3 : V -> Prep -> Prep -> V3 ;  -- praten, met, over
    } ;


--3 Other complement patterns
--
-- Verbs and adjectives can take complements such as sentences,
-- questions, verb phrases, and adjectives.

--  mkV0  : V -> V0 ; --%
--  mkVS  : V -> VS ;
--  mkV2S : V -> Prep -> V2S ;
--  mkVV  : V -> VV ;
--  mkV2V : V -> Prep -> V2V ;
--  mkVA  : V -> VA ;
--  mkV2A : V -> Prep -> V2A ;
--  mkVQ  : V -> VQ ;
--  mkV2Q : V -> Prep -> V2Q ;

--  mkAS  : A -> AS ;
--  mkA2S : A -> Prep -> A2S ;
--  mkAV  : A -> AV ;
--  mkA2V : A -> Prep -> A2V ;
-- Notice: categories $AS, A2S, AV, A2V$ are just $A$,
-- and the second argument is given as an adverb. Likewise
-- $V0$ is just $V$.
--  V0 : Type ;
--  AS, A2S, AV, A2V : Type ;

  mkOrd : A -> Ord = \a -> lin Ord {s = a.s ! Posit} ;

  mkN = overload {
    mkN : (muis : Str) -> N 
    = \a -> lin N (regNoun a) ;
    mkN : (bit : Str) -> Gender -> N 
    = \a,b -> lin N (regNounG a b) ;
    mkN : (vrou,vrouens : Str) -> Gender -> N
    = \a,b,c -> lin N (mkNoun a b c) ;
  } ;

  mkN2 = overload {
    mkN2 : N -> N2 
    = \n -> lin N2 (n ** {c2 = "van"}) ; 
    mkN2 : N -> Prep -> N2 
    = \n,p -> lin N2 (n ** {c2 = p.s}) ; 
    } ;   
  mkN3 n p q = lin N3 (n ** {c2 = p.s ; c3 = q.s}) ; 

  mkPN = overload {
    mkPN : Str -> PN = \s -> lin PN {s = \\_ => s ; g = masculine } ;
    mkPN : Str -> Gender -> PN = \s,g -> lin PN {s = \\_ => s; g = g } ;
    } ;

  masculine = Masc ;
  feminine  = Fem ;

  mkA = overload {
    mkA : (vers : Str) -> A = \a -> lin A (regAdjective a) ;
    mkA : (sag, sagte : Str) -> A = \a,b -> lin A (semregAdjective a b) ;
    mkA : (goed,goeie,goeds,beter,beste : Str) -> A = \a,b,c,d,e -> lin A (mkAdjective a b c d e) ;
    } ;

  mkPrep s = lin Prep (ss s) ;

  mkV = overload {
    mkV : (loop : Str) -> V = 
      \s -> lin V (v2vv (regVerb s)) ;
    mkV : (kan,kon : Str) -> V = 
      \a,b -> lin V (v2vv (irregVerb a b)) ;
    mkV : (wil,wou,gewil : Str) -> V = 
      \a,b,c -> lin V (v2vv (mkVerb a a b c)) ;
    mkV : Str -> V -> V = \v,s ->lin V (prefixV v s) ;
    } ;

--3 Two-place verbs
  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> lin V2 (v2vv (regVerb s) ** {c2 = [] ; hasPrep = False }) ;
    mkV2 : V -> V2 = \s -> lin V2 (s ** {c2 = [] ; hasPrep = False }) ;
    mkV2 : V -> Prep -> V2  = \s,p -> lin V2 (s ** {c2 = p.s ; hasPrep = True }) ;

--4 Three-place verbs
  mkV3 = overload {
    mkV3 : V -> Prep -> Prep -> V3 = mkmaxV3 ;
    mkV3 : V -> Prep -> V3 = \v,p -> mkmaxV3 v (mkPrep []) p ; 
    mkV3 : V -> V3 = \v -> mkmaxV3 v (mkPrep []) (mkPrep []) ; 
    } ;
  mkmaxV3 : V -> Prep -> Prep -> V3 = \v,c,d -> lin V3 (v ** {c2 = c.s ; c3 = d.s}) ;

--  invarA = \s -> lin A {s = \\_,_ => s} ; ---- comparison

  mkA2 = \a,p -> lin A2 (a ** {c2 = p.s}) ;

  mkAdv = overload {
    mkAdv : Str -> Adv = \s -> lin Adv {s = s ; p = Pos } ;
    mkAdv : Str -> Polarity -> Adv = \s,p -> lin Adv {s = s ; p = p } ;
  } ;

--  noPrep = mkPrep [] ;
--  prepV2  : V -> Prep -> V2 ;
--  prepV2 v c = lin V2 (v ** {c2 = c.s}) ;

--  mkVS v = lin VS v ;
--  mkVQ v = lin VQ v ;
--  mkVV v = lin VV (v ** {isAux = False}) ;

--  V0 : Type = V ;

--  mkV0 v = v ;
--  mkV2S v p = lin V2S (prepV2 v p) ;
--  mkV2V v p = lin V2V (prepV2 v p ** {isAux = False}) ;
--  mkVA  v   = lin VA v ;
--  mkV2A v p = lin V2A (prepV2 v p) ;
--  mkV2Q v p = lin V2Q (prepV2 v p) ;

}
