concrete VerbAfr of Verb = CatAfr ** open Prelude, ResAfr in {
--
--  flags optimize=all_subs ;
--
  lin
    UseV v = predV v ;

    ComplVV v vp = vvPred v vp ;

    ComplVS v s = 
      insertSubCl s.hasNeg (conjThat ++ s.s ! Sub) (predV v) ;
--    ComplVQ v q = 
--      insertExtrapos (q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertObjAP (ap.s ! APred) (predV v) ;

    SlashV2a v = (prepV v.hasPrep2 (predV v)) ** {c2 = v.c2 ; hasN2 = False ; isV3 = False } ; 
    
    -- inserts n2
    Slash2V3 v np = case <np.isPerson,v.hasPrep2> of 
                    { <True,False> => insertObjNP2a np.hasNwd np.isPerson (appPrep v.c2 np.s) (prepV v.hasPrep2 (predV v)) ** { c2 = v.c3 ; hasN2 = True ; isV3 = True } ;
                      <_,_>        => insertObjNP2b np.hasNwd np.finNie np.isPerson (appPrep v.c2 np.s) (prepV v.hasPrep2 (predV v)) ** { c2 = v.c3 ; hasN2 = True ; isV3 = True } } ;
                        
    -- insertObjNP np.isNeg np.isPerson (appPrep v.c2 np.s) (predV v) ** { c2 = [] ; hasDirObj = False } ;
--      insertObj (\\_ => appPrep v.c2 np.s) (predVv v) ** {c2 = v.c3} ;

    -- inserts n3
    Slash3V3 v np = insertObjNP3 np.hasNwd np.finNie np.isPerson (appPrep v.c3 np.s) (prepV v.hasPrep2 (predV v)) ** { c2 = v.c2 ; hasN2 = False ; isV3 = True } ;   
    

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

    ComplSlash vp np = case <vp.isV3,vp.hasN2,np.isPerson,vp.hasPrep> of 
                        { 
                          <False,_,True,False> => insertObjNP2a np.hasNwd np.isPerson (appPrep vp.c2 np.s) vp ; -- hy sien [Jan] nie vandag nie
                          <False,_,_,True> => insertObjNP3 np.hasNwd np.finNie np.isPerson (appPrep vp.c2 np.s) vp ; --! hy kyk nie vandag [na Jan] nie
                          <False,_,_,_>        => insertObjNP2b np.hasNwd np.finNie np.isPerson (appPrep vp.c2 np.s) vp ; -- hy sien nie vandag [die man] nie/hy kyk nie vandag [na Jan] nie
                          
                          <True,False,True,False> => insertObjNP2a np.hasNwd np.isPerson (appPrep vp.c2 np.s) vp ; -- hy stuur [Jan] nie vandag na hom nie
                          <True,False,_,_>    => insertObjNP2b np.hasNwd np.finNie np.isPerson (appPrep vp.c2 np.s) vp ; -- hy stuur nie die man vandag [na hom] nie
                          
                          <True,True,_,_>         => insertObjNP3 np.hasNwd np.finNie np.isPerson (appPrep vp.c2 np.s) vp 
                        
                        } ;
                        
                        
                        
                        --{ <False,True,False> => insertObjNP np.isNeg np.isPerson (np.s!NPAcc) vp ; -- hy sien [Jan] nie, hy belowe (haar) nie (die vrou) [Jan] nie
                        --  <False,False,False> => insertObjNP np.isNeg (np.s!NPAcc) vp ; -- hy sien nie [die man] nie, hy gee nie die man ['n kans] nie
                        --  <True,True,False> => insertExt np.isNeg (np.s!NPAcc) vp ; -- hy gee [Jan] nie 'n kans nie
                        --  <True,False,False> => insertExt np.isNeg (np.s!NPAcc) vp ; -- hy gee nie [die man] 'n kans nie
                          
                        --  <False,True,True> => insertObjNP np.isNeg np.isPerson (appPrep vp.c2 np.s) vp ;  -- hy kyk nie [na Jan] nie, hy stuur nie [die boom] na Jan nie
                        --  <False,False,True> => insertObjNP np.isNeg np.isPerson (appPrep vp.c2 np.s) vp ;  -- hy kyk nie [na die man] nie, hy stuur nie [die boom] na die man nie
                        --  <True,True,True> => insertExt np.isNeg (appPrep vp.c2 np.s) vp ; -- hy kyk nie na Jan nie, hy stuur nie die boom [na Jan] nie
                        --  <True,False,True> => insertExt np.isNeg (appPrep vp.c2 np.s) vp -- hy kyk nie na die man nie, hy stuur nie die boom [na die man] nie
                        --} ;

    SlashVV v vp = (vvPred v vp) ** {c2 = vp.c2 ; isV3 = vp.isV3 ; hasN2 = vp.hasN2 } ;

--    SlashV2VNP v np vp = 
--      let 
--        vpi = infVP False vp 
--      in
--      insertExtrapos vpi.p3 (
--        insertInf vpi.p2 (
--          insertObj vpi.p1 (
--            insertObj (\\_ => appPrep v.c2 np.s) (
--              predVv v)))) ** {c2 = v.c2} ;

    UseComp comp = insertSubCl False (comp.s2) (insertObjAP (comp.s!(agrP3 Sg)) (predV is_V)) ;
   
----edited
--    CompCN cn = {s = \\a => case a.n of {
--        Sg => "'n" ++ cn.s ! Strong ! NF a.n Nom ;
--        Pl => cn.s ! Strong ! NF a.n Nom
--        }
--      } ;
--
--
    CompAP ap = {s = \\_ => ap.s ! APred ; s2 = ap.s2} ;
--    CompNP np = {s = \\_ => np.s ! NPNom} ;
--    CompAdv a = {s = \\_ => a.s} ;
--

    -- check the kind of adv and insert accordingly
    AdvVP vp adv = case adv.isClause of { True => insertSubCl adv.isNwd adv.s vp ;
                                          False => insertAdv adv.s adv.isNwd vp } ;
    AdVVP adv vp = insertAdV adv.s adv.isNwd vp ;
    
    UseCopula = predV is_V ;
    
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
