metrica_via_preco<-function(tickers=c("PETR3","RADL3"),data_inicio=as.Date('2012-01-01'),data_fim=Sys.Date()){
  historico_acao=get_acoes(tickers,data_inicio,data_fim)  
  
  historico_acao=historico_acao%>%
    select(dte_data=ref.date,str_ticker=ticker,vl_fechamento=price.close)
  
  
  infos_bp=rbind(bulk$ativo,bulk$passivo)%>%
    select(dte_data,str_conta,str_nome_cia,vl_valor)%>%
    filter(str_conta %in% c("1","1.01","2.01","2.03","2.03.09"))%>%dcast(dte_data+str_nome_cia~str_conta,value.var="vl_valor")%>%
    summarise(dte_data,str_nome_cia,patrimonio_liquido=(`2.03`-`2.03.09`),ativo=`1`,ativo_circulante=`1.01`,cap_giro=`1.01`-`2.01`)
  
  
  #Pegar o Release
  
  #TRANSFOMRAR EM SERIE DIARIA
  
  infos_bp=merge(bulk$tbl_divulgacoes,infos_bp,by=c("dt_referencia","str_nome_cia"),by.y=c("dte_data","str_nome_cia"))
  
  #PEGAR O EQUIVALENTE TICKER EMPRESA
  historico_acao=merge(historico_acao,bulk$tbl_ticker,by="str_ticker")
  historico_acao=merge(historico_acao,infos_bp,by.x=c("dte_data","str_nome_cia"),by.y=c("dt_divulgacao","str_nome_cia"),all=TRUE)
  
  historico_acao=historico_acao%>%group_by(str_ticker)%>%
    mutate(dt_referencia=na.locf(dt_referencia,na.rm=FALSE),
           patrimonio_liquido=na.locf(patrimonio_liquido,na.rm=FALSE),
           ativo=na.locf(ativo,na.rm=FALSE),
           ativo_circulante=na.locf(ativo_circulante,na.rm=FALSE),
           cap_giro=na.locf(cap_giro,na.rm=FALSE))
  
  info_dre=bulk$dre%>%filter(str_conta=='3.11.01')%>%
    group_by(str_nome_cia)%>%
    mutate(LucroLiquido=rollapply(vl_valor,FUN=sum,width=4,align='right',partial=FALSE,fill=NA))
  
  info_dre=info_dre[!is.na(info_dre$LucroLiquido),]
  
  
  
  historico_acao=historico_acao[!is.na(historico_acao$dt_referencia),]
  
  
  historico_acao=merge(historico_acao%>%mutate(dt_referencia=as.Date(dt_referencia)),
                       info_dre%>%select(dte_data,LucroLiquido,str_nome_cia)%>%
                         mutate(dte_data=as.Date(dte_data)),by.x=c("dt_referencia","str_nome_cia"),by.y=c("dte_data","str_nome_cia"),all=TRUE)
  
  
  
  historico_acao=na.omit(historico_acao)
  
  
  historico_acao=merge(historico_acao,bulk$num_acoes,by="str_ticker")
  
  historico_acao=historico_acao%>%
    mutate(P_L=vl_acao*vl_fechamento/(1000*LucroLiquido),
           P_VP=vl_acao*vl_fechamento/(1000*patrimonio_liquido),
           P_ATIVO=vl_acao*vl_fechamento/(1000*ativo),
           P_Cap_Giro=vl_acao*vl_fechamento/(1000*cap_giro),
           P_Ativ_Circ=vl_acao*vl_fechamento/(1000*ativo_circulante))
}
