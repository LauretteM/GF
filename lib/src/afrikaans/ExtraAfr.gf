--# -path=.:../common:../abstract:../../prelude

concrete ExtraAfr of ExtraAfrAbs = CatAfr ** 
  open ResAfr, Coordination, Prelude, IrregAfr in 
{

flags 
  coding=utf8 ;
  --optimize=all_subs ;
--    ICompAP ap = {s = \\_ => "hoe" ++ ap.s ! APred} ; 
--    IAdvAdv adv = {s = "hoe" ++ adv.s} ;

}
