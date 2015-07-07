--# -path=.:../common:../abstract:../../prelude

concrete ExtraAfr of ExtraAfrAbs = CatAfr ** 
  open ResAfr, Coordination, Prelude, IrregAfr in 
{

flags 
  coding=utf8 ;
  --optimize=all_subs ;

  lin
    fast_Adv = {s = "vinnig" ; p = Pos} ; 
    never_AdV = lin AdV (mkAdV "nooit" Neg) ;
    nowhere_Adv = {s = "nêrens" ; p = Neg } ;
    chance_N = lin N (regNoun "kans") ;
    --nowhere_AdV = lin AdV (mkAdV "nêrens" Neg) ;

--    ICompAP ap = {s = \\_ => "hoe" ++ ap.s ! APred} ; 
--    IAdvAdv adv = {s = "hoe" ++ adv.s} ;

}
