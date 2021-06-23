get_acoes<-function(tickers,data_inicio=as.Date('2000-01-01'),data_fim=Sys.Date()){
  sss=BatchGetSymbols(paste(tickers,".SA",sep=""),first.date=data_inicio,last.date=data_fim,bench.ticker="^BVSP")
  acoes=sss$df.tickers
  acoes$ticker=substr(acoes$ticker,1,str_length(acoes$ticker)-3)
  acoes=acoes%>%summarise(dte_data=ref.date,str_ticker=ticker,vl_close=price.close)
  return(acoes)
}

metrica_via_preco<-function(tickers=c("PETR3","RADL3"),data_inicio=as.Date('2011-01-01'),data_fim=Sys.Date()){
  historico_acao=get_acoes(tickers,data_inicio,data_fim)  
  
  
  depara_cvm=merge(tbl_ticker%>%filter(str_ticker %in% tickers),
                   tbl_empresas%>%dplyr::select(regex,str_cia_CVM))%>%summarise(str_ticker,str_cia_CVM)
  
  
  infos_bp=merge(rbind(tbl_bp_ativo,tbl_bp_passivo)%>%select(dte_data,cd_conta,str_cia_CVM,vl_conta),depara_cvm,by="str_cia_CVM")%>%
    filter(cd_conta %in% c("1","1.01","2.01","2.03","2.03.09"))%>%dcast(dte_data+str_ticker~cd_conta,value.var="vl_conta")%>%
    summarise(dte_data,str_ticker,patrimonio_liquido=(`2.03`-`2.03.09`),ativo=`1`,ativo_circulante=`1.01`,cap_giro=`1.01`-`2.01`)
  
  
  #Pegar o Release
  
  #TRANSFOMRAR EM SERIE DIARIA
  
  infos_bp=merge(merge(tbl_divulgacoes,depara_cvm,by="str_cia_CVM"),
                 infos_bp,by=c("dte_refer","str_ticker"),by.y=c("dte_data","str_ticker"))
  
  #PEGAR O EQUIVALENTE TICKER EMPRESA
  #historico_acao=merge(historico_acao,bulk$tbl_ticker,by="str_ticker")
  
  historico_acao=merge(historico_acao%>%mutate(dte_data=as.Date(dte_data)),
                       infos_bp%>%mutate(dt_publi=as.Date(dt_publi)),
                       by.x=c("dte_data","str_ticker"),by.y=c("dt_publi","str_ticker"),all=TRUE)
  
  colnames(historico_acao)
  historico_acao=historico_acao%>%group_by(str_ticker)%>%
    mutate(dte_refer=na.locf(dte_refer,na.rm=FALSE),
           str_cia_CVM=na.locf(str_cia_CVM,na.rm=FALSE),
           patrimonio_liquido=na.locf(patrimonio_liquido,na.rm=FALSE),
           ativo=na.locf(ativo,na.rm=FALSE),
           ativo_circulante=na.locf(ativo_circulante,na.rm=FALSE),
           cap_giro=na.locf(cap_giro,na.rm=FALSE))
  
  info_dre=merge(tbl_dre,depara_cvm,by="str_cia_CVM")%>%filter(cd_conta=='3.11.01')%>%
    group_by(str_ticker)%>%
    mutate(LucroLiquido=rollapply(vl_conta,FUN=sum,width=4,align='right',partial=FALSE,fill=NA))
  
  info_dre=info_dre[!is.na(info_dre$LucroLiquido),]
  
  historico_acao=historico_acao[!is.na(historico_acao$dte_refer),]
  
  
  historico_acao=merge(historico_acao,
                       info_dre%>%select(dte_data,LucroLiquido,str_ticker),
                       by.x=c("dte_refer","str_ticker"),
                       by.y=c("dte_data","str_ticker"),all=TRUE)
  

  
  historico_acao=na.omit(historico_acao)
  
  numero_de_acoes=buscar_num_acoes(tickers)
  
  historico_acao=merge(historico_acao,numero_de_acoes%>%summarise(str_ticker,vl_num_acoes=vl_acao),by="str_ticker")
  
  historico_acao=historico_acao%>%
    mutate(P_L=vl_num_acoes*vl_close/(1000*LucroLiquido),
           P_VP=vl_num_acoes*vl_close/(1000*patrimonio_liquido),
           P_ATIVO=vl_num_acoes*vl_close/(1000*ativo),
           P_Cap_Giro=vl_num_acoes*vl_close/(1000*cap_giro),
           P_Ativ_Circ=vl_num_acoes*vl_close/(1000*ativo_circulante))
  return(historico_acao)
}
