--# -path=.:../common:../abstract:../../prelude

--1 Afrikaans Lexical Paradigms
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


resource ParadigmsAfr = open 
  (Predef=Predef), 
  Prelude, 
  ResAfr,
  CatAfr
  in 
{
----2 Parameters 
--
---- To abstract over gender names, we define the following identifiers.

oper

  masculine : Gender ; 
  feminine  : Gender ; 
--  neuter    : Gender ; --%
--  utrum     : Gender ; --%
----afr!
--  de  : Gender ; -- non-neutrum
--  het : Gender ; -- neutrum
--  --die : Gender ;



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

-- Three-place relational nouns ("die Verbinding van x na y") need two prepositions.

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


---- Invariable adjective are a special case. 
--
--  invarA : Str -> A ;            -- adjective with just one form
--
--
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
--
--  zijnV  : V -> V ; -- force zijn as auxiliary (default hebben)
--
--  reflV  : V -> V ; -- reflexive verb e.g. zich afvragen
--
----3 Three-place verbs
--
---- Three-place (ditransitive) verbs need two prepositions, of which
---- the first one or both can be absent.

------3 Other complement patterns
------
------ Verbs and adjectives can take complements such as sentences,
------ questions, verb phrases, and adjectives.
--
--  mkV0  : V -> V0 ; --%
  mkVS  : V -> VS ;
--  mkV2S : V -> Prep -> V2S ;
--  mkVV  : V -> Bool -> VV ;
--  mkV2V : V -> Prep -> V2V ;
--  mkVA  : V -> VA ;
--  mkV2A : V -> Prep -> V2A ;
--  mkVQ  : V -> VQ ;
--  mkV2Q : V -> Prep -> V2Q ;
----
----  mkAS  : A -> AS ;
----  mkA2S : A -> Prep -> A2S ;
----  mkAV  : A -> AV ;
----  mkA2V : A -> Prep -> A2V ;
----
------ Notice: categories $AS, A2S, AV, A2V$ are just $A$, 
------ and the second argument is given as an adverb. Likewise 
------ $V0$ is just $V$.
----
----  V0 : Type ;
----  AS, A2S, AV, A2V : Type ;
----
--  mkOrd : A -> Ord = \a -> lin Ord {s = a.s ! Posit} ;

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

--  zijnV v = v ; -- lin V (v2vvAux v VZijn) ;
--  reflV v = lin V {s = v.s ; aux = v.aux ; prefix = v.prefix ; vtype = VRefl} ;
--
--  zijn_V : V = lin V ResAfr.zijn_V ;
--  hebben_V : V = lin V ResAfr.hebben_V ;


----3 Two-place verbs

  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> lin V2 (v2vv (regVerb s) ** {c2 = [] ; hasPrep = False }) ;
    mkV2 : V -> V2 = \s -> lin V2 (s ** {c2 = [] ; hasPrep = False }) ;
    mkV2 : V -> Prep -> V2  = \s,p -> lin V2 (s ** {c2 = p.s ; hasPrep = True }) ;
    } ;

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

  mkVS v = lin VS v ;
--  mkVQ v = lin VQ v ;

  mkVV : V -> Bool -> VV = \v,b -> case b of { True => lin VV (v ** {om = "om" ; te = "te" }) ;
                                                 False => lin VV (v ** {om = [] ; te = "te" }) } ;

}
