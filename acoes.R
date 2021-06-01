get_acoes<-function(data_inicio=as.Date('2000-01-01'),data_fim=Sys.Date()){
  acoes_procuradas=(depara_empresas%>%filter(razao_social %in% empresas_analisadas))$ticker
  acoes_procuradas=paste(acoes_procuradas,".SA",sep="")
  sss=BatchGetSymbols(acoes_procuradas,first.date=data_inicio,last.date=data_fim,bench.ticker="^BVSP")
  acoes=sss$df.tickers
  return(acoes)
}