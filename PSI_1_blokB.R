ocena_kredytowa<-function(dochod, zadluzenie){
  procentdochodu<-zadluzenie/dochod
  if(procentdochodu<0.3){
    return("KREDYT PRZYZNANY")}
  else if(procentdochodu<=0.5&&procentdochodu>0.3){
    return ("WYMAGA WERYFIKACJI")}
  else{
    return("KREDYT ODRZUCONY")}
}
ocena_kredytowa(10000,2000)
ocena_kredytowa(10000,4000)
ocena_kredytowa(10000,6000)
