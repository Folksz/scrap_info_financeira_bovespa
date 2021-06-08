get_acoes<-function(tickers,data_inicio=as.Date('2000-01-01'),data_fim=Sys.Date()){
  
  acoes_procuradas=paste(tickers,".SA",sep="")
  sss=BatchGetSymbols(acoes_procuradas,first.date=data_inicio,last.date=data_fim,bench.ticker="^BVSP")
  acoes=sss$df.tickers
  acoes$ticker=substr(acoes$ticker,1,str_length(acoes$ticker)-3)
  return(acoes)
}