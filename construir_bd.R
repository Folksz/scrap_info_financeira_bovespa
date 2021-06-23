


construir_bd<-function(anos,diretorio){
  bulk=serie_CVM(anos)
  tbl_dre=construir_DRE(bulk)
  tbl_bp_ativo=construir_ativo(bulk)
  tbl_bp_passivo=construir_passivo(bulk)
  depara=construir_depara(bulk)
  tbl_empresas=depara$tbl_empresas
  tbl_ticker=depara$tbl_ticker
  tbl_divulgacoes=contruir_release(bulk)
  
  
  
  
  write.csv(tbl_dre,paste(diretorio,"tbl_dre.csv",sep=""),row.names=FALSE)
  write.csv(tbl_bp_ativo,paste(diretorio,"tbl_bp_ativo.csv",sep=""),row.names=FALSE)
  write.csv(tbl_bp_passivo,paste(diretorio,"tbl_bp_passivo.csv",sep=""),row.names=FALSE)
  write.csv(tbl_empresas,paste(diretorio,"tbl_empresas.csv",sep=""),row.names=FALSE)
  write.csv(tbl_ticker,paste(diretorio,"tbl_ticker.csv",sep=""),row.names=FALSE)
  write.csv(tbl_divulgacoes,paste(diretorio,"tbl_divulgacoes.csv",sep=""),row.names=FALSE)
  
}