concrete NounAfr of Noun = CatAfr ** open ResAfr, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      s = \\c => det.s ++ cn.s ! NF det.n Nom ; -- kan dalk vereenvoudig (2011-01-14)
      a = agrP3 det.n ;
      isPerson = False ;
      hasNwd = det.isNeg ;
      finNie = cn.finNie
      } ;

    DetNP det = {
      s = \\_ => det.s ;
      a = agrP3 det.n ;
      isPerson = False ;
      hasNwd = det.isNeg ;
      finNie = False
      } ;

    UsePN pn = {s = pn.s ; a = agrP3 Sg ; hasNwd = False ; finNie = False ; isPerson = True } ;

    UsePron pron = {
      s = table {NPNom => pron.nom ; NPAcc => pron.acc} ;
      a = pron.a ;
      isPerson = True ;
      hasNwd = False ;
      finNie = False
      } ;

--    PredetNP pred np = heavyNP {
--      s = \\c => 
--        pred.s ! np.a.n ! np.a.g ++ np.s ! c ; ---- g
--      a = np.a
--      } ;

--    PPartNP np v2 = heavyNP {
--      s = \\c => np.s ! c ++ v2.s ! VPerf ; -- invar part
--      a = np.a
--      } ;

--    AdvNP np adv = heavyNP {
--      s = \\c => np.s ! c ++ adv.s ;
--      a = np.a
--      } ;

--    DetQuantOrd quant num ord = 
--      let 
--        n = num.n ;
--        a = quant.a
--      in {
--        s  = \\g => quant.s ! num.isNum ! n ! g ++ 
--                      num.s ++ ord.s ! agrAdj g quant.a (NF n Nom) ;
--        sp = \\g => quant.sp ! n ! g ++ 
--                      num.s ++ ord.s ! agrAdj g quant.a (NF n Nom) ;
--        n = n ;
--        a = a
--        } ;

    DetQuant quant num = {
        s = quant.s ++ num.s ;
        n = num.n ;
        isNeg = quant.isNeg
        } ;

    PossPron p = {
        s  = p.poss ;
        isNeg = False ;
      } ;

    NumCard n = {s = n.s ; n = n.n ; isNum = True} ;

    NumPl = {s = []; n = Pl ; isNum = False} ; 
    NumSg = {s = []; n = Sg ; isNum = False} ; 

    NumDigits numeral = {s = numeral.s ! NCard ; n = numeral.n } ;
    OrdDigits numeral = {s = numeral.s ! NOrd} ;

    NumNumeral numeral = {s = numeral.s ! NCard ; n = numeral.n } ;
    OrdNumeral numeral = {s = numeral.s ! NOrd } ;

--    AdNum adn num = {s = \\g,c => adn.s ++ num.s!g!c; n = num.n } ;

    OrdSuperl a = {s = a.s ! Superl ! AAttr } ;

    DefArt = { s = "die" ; isNeg = False } ;

    IndefArt = { s = "'n" ; isNeg = False } ;

--    MassNP cn = {
--      s = \\c => cn.s ! Strong ! NF Sg Nom ;
--      a = agrP3 Sg ;
--      isPron = False
--      } ;

    UseN = \n -> {
      s = \\f => n.s!f ;
      g = n.g ;
      hasNwd = False ;
      finNie = False
      } ;
      
--    UseN2 = \n -> {
--      s = \\_ => n.s ;
--      g = n.g
--      } ;

--    ComplN2 f x = {
--      s = \\_,nc => f.s ! nc ++ appPrep f.c2 x.s ;
--      g = f.g
--      } ;

--    ComplN3 f x = {
--      s = \\nc => f.s ! nc ++ appPrep f.c2 x.s ;
--      g = f.g ; 
--      c2 = f.c3
--      } ;

--    Use2N3 f = {
--      s = f.s ;
--      g = f.g ; 
--      c2 = f.c2
--      } ;

--    Use3N3 f = {
--      s = f.s ;
--      g = f.g ; 
--      c2 = f.c3
--      } ;

    AdjCN ap cn = {
        s = \\n => ap.s!AAttr ++ cn.s!n ;
        g = cn.g ;
        hasNwd = cn.hasNwd ;
        finNie = cn.finNie
        } ;

    RelCN cn rs = {
      s = \\nc => cn.s ! nc ++ rs.s ! cn.g ! (case nc of {NF n c => n}) ;
      g = cn.g ;
      hasNwd = cn.hasNwd ;
      finNie = rs.hasNeg
      } ;

    RelNP np rs = {
      s = \\c => np.s ! c ++ "," ++ rs.s ! np.a.g ! np.a.n ;
      a = np.a ;
      isPerson = False ;
      hasNwd = np.hasNwd ;
      finNie = rs.hasNeg
      } ;

--    SentCN cn s = {
--      s = \\a,nc => cn.s ! a ! nc ++ s.s ;
--      g = cn.g
--      } ;

--    AdvCN cn s = {
--      s = \\a,nc => cn.s ! a ! nc ++ s.s ;
--      g = cn.g
--      } ;

--    ApposCN  cn np = let g = cn.g in {
--      s = \\a,nc => cn.s ! a ! nc ++ np.s ! NPNom ;
--      g = g ;
--      isMod = cn.isMod
--      } ;

}
