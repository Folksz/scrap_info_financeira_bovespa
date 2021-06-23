atualizar_ponta<-function(diretorio){
  bulk=serie_CVM(2021)
  
  tbl_dre_ponta=construir_DRE(bulk)
  tbl_bp_ativo_ponta=construir_ativo(bulk)
  tbl_bp_passivo_ponta=construir_passivo(bulk)
  depara_ponta=construir_depara(bulk)
  tbl_empresas_ponta=depara_ponta$tbl_empresas
  tbl_ticker_ponta=depara_ponta$tbl_ticker
  tbl_divulgacoes_ponta=contruir_release(bulk)
  
  
  tbl_dre=read.csv(paste(diretorio,"tbl_dre.csv",sep=""))
  tbl_bp_ativo=read.csv(paste(diretorio,"tbl_bp_ativo.csv",sep=""))
  tbl_bp_passivo=read.csv(paste(diretorio,"tbl_bp_passivo.csv",sep=""))
  tbl_empresas=read.csv(paste(diretorio,"tbl_empresas.csv",sep=""))
  tbl_ticker=read.csv(paste(diretorio,"tbl_ticker.csv",sep=""))
  tbl_divulgacoes=read.csv(paste(diretorio,"tbl_divulgacoes.csv",sep=""))
  
  tbl_dre=rbind(tbl_dre,anti_join(tbl_dre_ponta,tbl_dre))
  tbl_bp_ativo=rbind(tbl_bp_ativo,anti_join(tbl_bp_ativo_ponta,tbl_bp_ativo))
  tbl_bp_passivo=rbind(tbl_bp_passivo,anti_join(tbl_bp_passivo_ponta,tbl_bp_passivo))
  tbl_empresas=rbind(tbl_empresas,anti_join(tbl_empresas_ponta,tbl_empresas))
  tbl_ticker=rbind(tbl_ticker,anti_join(tbl_ticker_ponta,tbl_ticker))
  tbl_divulgacoes=rbind(tbl_divulgacoes,anti_join(tbl_divulgacoes_ponta,tbl_divulgacoes))
  

  #OLHAR o tbl_empresas
  #olhar o tbl_ticker

  
  write.csv(tbl_dre,paste(diretorio,"tbl_dre.csv",sep=""),row.names=FALSE)
  write.csv(tbl_bp_ativo,paste(diretorio,"tbl_bp_ativo.csv",sep=""),row.names=FALSE)
  write.csv(tbl_bp_passivo,paste(diretorio,"tbl_bp_passivo.csv",sep=""),row.names=FALSE)
  write.csv(tbl_empresas,paste(diretorio,"tbl_empresas.csv",sep=""),row.names=FALSE)
  write.csv(tbl_ticker,paste(diretorio,"tbl_ticker.csv",sep=""),row.names=FALSE)
  write.csv(tbl_divulgacoes,paste(diretorio,"tbl_divulgacoes.csv",sep=""),row.names=FALSE)
  

}
