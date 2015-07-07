concrete VerbAfr of Verb = CatAfr ** open Prelude, ResAfr in {
--
--  flags optimize=all_subs ;
--
  lin
    UseV = predV ;

    ComplVV v vp = vvPred v vp ;

    ComplVS v s = 
      insertSubCl s.hasNeg (conjThat ++ s.s ! Sub) (predV v) ;
--    ComplVQ v q = 
--      insertExtrapos (q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertObjAP (ap.s ! APred) (predV v) ;

    SlashV2a v = (prepV v.hasPrep v) ** {c2 = v.c2 ; hasDirObj = False } ; 
    
    -- inserts the direct object
    Slash2V3 v np = case np.isPerson of 
                    { True => insertObjNP np.isNeg np.isPerson (appPrep v.c2 np.s) (predV v) ** { c2 = v.c3 ; hasDirObj = True } ;
                      False => insertExt np.isNeg (appPrep v.c2 np.s) (prepV v.hasP3 v) ** { c2 = v.c3 ; hasDirObj = True } } ;
                        
    -- insertObjNP np.isNeg np.isPerson (appPrep v.c2 np.s) (predV v) ** { c2 = [] ; hasDirObj = False } ;
--      insertObj (\\_ => appPrep v.c2 np.s) (predVv v) ** {c2 = v.c3} ;

    -- inserts indirect object
    Slash3V3 v np = insertExt np.isNeg (appPrep v.c3 np.s) (prepV v.hasP3 v) ** { c2 = v.c2 ; hasDirObj = False } ;
                    --case <v.hasIndPrep> of {
                        --<True,_> => insertExt np.isNeg (appPrep v.c3 np.s) (predV v) ** { c2 = v.c2 ; hasDirObj = False } ;
                        --<False,False> => insertExt np.isNeg (appPrep v.c3 np.s) (predV v) ** { c2 = v.c2 ; hasDirObj = False } ;
                        --<False,True> => insertObjNP np.isNeg np.isPerson (appPrep v.c3 np.s) (predV v) ** { c2 = v.c2 ; hasDirObj = False } 
                    --} ;
    --insertIndObjNP (appPrep v.c2 np.s) (predV v) ** { c2 = v.c2 ; hasDirObj = True } ;
--      insertObj (\\_ => appPrep v.c3 np.s) (predVv v) ** {c2 = v.c2} ;
--
--    SlashV2S v s = 
--      insertExtrapos (conjThat ++ s.s ! Sub) (predVv v) ** {c2 = v.c2} ;
--    SlashV2Q v q = 
--      insertExtrapos (q.s ! QIndir) (predVv v) ** {c2 = v.c2} ;
--    SlashV2V v vp = 
--      let 
--        vpi = infVP False vp 
--      in
--      insertExtrapos vpi.p3 (
--        insertInf vpi.p2 (
--          insertObj vpi.p1 ((predVv v)))) ** {c2 = v.c2} ;
--
--    SlashV2A v ap = 
--      insertObj (\\_ => ap.s ! APred) (predVv v) ** {c2 = v.c2} ;
--

-- DO NEXT: fix this and Slash2V3 and Slash3V3
    ComplSlash vp np = case <vp.hasDirObj,np.isPerson,vp.hasPrep> of 
                        { <False,True,False> => insertObjNP np.isNeg np.isPerson (appPrep vp.c2 np.s) vp ; -- hy sien Jan nie, hy gee [Jan] nie 'n kans nie
                          <False,False,False> => insertExtPre np.isNeg (appPrep vp.c2 np.s) vp ; -- hy sien nie die man nie, hy gee nie [die man] 'n kans nie
                          <True,True,False> => insertExt np.isNeg (appPrep vp.c2 np.s) vp ; -- hy gee Jan nie ['n kans] nie
                          <True,False,False> => insertExt np.isNeg (appPrep vp.c2 np.s) vp ; -- hy gee nie die man ['n kans] nie
                          
                          <False,True,True> => insertExtPre np.isNeg (appPrep vp.c2 np.s) vp ; -- hy kyk nie na Jan nie, hy stuur nie die boom [na Jan] nie
                          <False,False,True> => insertExtPre np.isNeg (appPrep vp.c2 np.s) vp ; -- hy kyk nie na die man nie, hy stuur nie die boom [na die man] nie
                          <True,True,True> => insertExt np.isNeg (appPrep vp.c2 np.s) vp ; -- hy stuur nie [die boom] na Jan nie
                          <True,False,True> => insertExt np.isNeg (appPrep vp.c2 np.s) vp  -- hy stuur nie [die boom] na die man nie
                        } ;

    SlashVV v vp = (vvPred v vp) ** {c2 = vp.c2 ; hasDirObj = False } ;
--      let 
--        vpi = infVP v.isAux vp 
--      in
--      insertExtrapos vpi.p3 (
--        insertInf vpi.p2 (
--          insertObj vpi.p1 (
--            predVGen v.isAux (v2v v)))) ** {c2 = vp.c2} ;
--
--    SlashV2VNP v np vp = 
--      let 
--        vpi = infVP False vp 
--      in
--      insertExtrapos vpi.p3 (
--        insertInf vpi.p2 (
--          insertObj vpi.p1 (
--            insertObj (\\_ => appPrep v.c2 np.s) (
--              predVv v)))) ** {c2 = v.c2} ;
--
--    UseComp comp = insertObj comp.s (predV zijn_V) ; -- agr not used
--    
----edited
--    CompCN cn = {s = \\a => case a.n of {
--        Sg => "'n" ++ cn.s ! Strong ! NF a.n Nom ;
--        Pl => cn.s ! Strong ! NF a.n Nom
--        }
--      } ;
--
--
--    CompAP ap = {s = \\_ => ap.s ! APred} ;
--    CompNP np = {s = \\_ => np.s ! NPNom} ;
--    CompAdv a = {s = \\_ => a.s} ;
--
    AdvVP vp adv = insertAdv adv.s adv.p vp ;
    AdVVP adv vp = insertAdV adv.s adv.p vp ;
--
--    ReflVP vp = insertObj (\\a => appPrep vp.c2 (\\_ => reflPron ! a )) vp ;
--
--    PassV2 v = insertInf (v.s ! VPerf) (predV word_V) ;
--
------ workaround for a subtyping bug
--  oper
--    v2v : VVerb -> VVerb = \v -> 
--      {s = v.s ; aux = v.aux ; prefix = v.prefix ; vtype = v.vtype} ;
--    predVv : VVerb -> ResAfr.VP = \v -> predV (v2v v) ;
}
