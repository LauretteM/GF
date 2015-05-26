--# -path=.:../abstract:../common:../api

concrete TestAfr of TestAfrAbs = 
  GrammarAfr,
  LexiconAfr
  ,ExtraAfr
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;
