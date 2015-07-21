--# -path=.:../abstract:../common:../api

concrete TestAfr of TestAfrAbs = 
  GrammarAfr,
  LexiconAfr
  ,ExtraAfr
  ** open ResAfr in {

flags startcat = Phr ; unlexer = text ; lexer = text ;

  lin
    fast_Adv = {s = "vinnig" ; p = Pos ; isClause = False } ; 
    never_AdV = lin AdV (mkAdV "nooit" Neg) ;
    nowhere_Adv = {s = "nêrens" ; p = Neg ; isClause = False } ;
    chance_N = lin N (regNoun "kans") ;
    tired_A = lin A (regAdjective "moeg") ;

} ;
