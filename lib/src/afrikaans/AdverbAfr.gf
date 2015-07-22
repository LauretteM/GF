concrete AdverbAfr of Adverb = CatAfr ** open ResAfr, Prelude in {


--  lin
--    PositAdvAdj a = {s = a.s ! Posit ! APred} ;

--    ComparAdvAdj cadv a np = {
--      s = cadv.s ++ a.s ! Posit ! APred ++ cadv.p ++ np.s ! NPNom
--      } ;
--    ComparAdvAdjS cadv a s = {
--      s = cadv.s ++ a.s ! Posit ! APred ++ cadv.p ++ s.s ! Sub
--      } ;

--    PrepNP prep np = {s = appPrep prep.s np.s} ;

--    AdAdv = cc2 ;

--    PositAdAAdj a = {s = a.s ! Posit ! APred} ;

lin    SubjS subj s = {s = subj.s ++ s.s ! Sub ; isNwd = s.hasNeg ; isClause = True } ;

--    AdnCAdv cadv = {s = cadv.s ++ conjThan} ;

}
