#Metricas por data
metricas_empresas_por_data<-function(tickers,data=as.Date('2021-03-31')){
  empresas=bulk$tbl_ticker%>%filter(str_ticker%in% tickers)
  dre_df=merge(empresas,bulk$dre,by="str_nome_cia")
  ativo_df=merge(empresas,bulk$ativo,by="str_nome_cia")
  passivo_df=merge(empresas,bulk$passivo,by="str_nome_cia")
  dre_anual=dre_df%>%
    group_by(str_ticker,str_conta)%>%
    arrange(dte_data)%>%
    mutate(str_conta=paste(str_conta,"acumulado",sep="_"),
           vl_valor=rollapply(vl_valor,FUN=sum,width=4,align='right',partial=FALSE,fill=NA))
  dre_anual=rbind(dre_df,dre_anual)

  indicadores=rbind(ativo_df%>%mutate(dte_data=as.Date(dte_data)),
                    passivo_df%>%mutate(dte_data=as.Date(dte_data)),
                    dre_anual)%>%filter(dte_data==data)%>%
    group_by(str_ticker)%>%dcast(str_ticker~str_conta,value.var="vl_valor")
  
  indicadores=merge(indicadores,bulk$num_acoes,by="str_ticker")
  
  info=indicadores%>%
    summarise(str_ticker,
              patrimonio_liquido=(`2.03`-`2.03.09`),
              lucro_liquido_12M=`3.11.01_acumulado`,
              #Ativo Circulante/Passivo Circulante
              indice_liquidez=`1.01`/`2.01`,
              #Lucro Liquido/Receita Liquida
              margem_liquida=100*`3.11.01`/`3.01`,
              #Ebit: Ebit/Receita Liquida
              margem_ebit=100*(`3.05` - `3.04.06`- `3.04.05`-`3.04.04`)/(`3.01`),
              #Margem Bruta:Lucro Bruto/Receita Liquida
              margem_bruta=100*`3.03`/`3.01`,
              #Giro Ativo: Receita Liquida (acumulado 12 meses)/Ativo Total
              giro_ativo=(`3.01_acumulado`/`1`),
              #Divida bruta / patrimonio liquido
              divida_bruta_patrimonio=((`2.01.04`+`2.02.01`)/patrimonio_liquido),
              #ROE: Lucro Liquido (acumulado 12 meses)/patrimonio liqudio
              ROE=100*lucro_liquido_12M/patrimonio_liquido,
              #EBIT (acumulado 12 meses)/ativo
              ebit_ativo=100*(`3.05_acumulado` - `3.04.06_acumulado`- `3.04.05_acumulado`-`3.04.04_acumulado`)/(`1`),
              #Lucro Por Acao: Lucro Liquido 12 meses/num acoes
              lucro_liquido_acao=1000*lucro_liquido_12M/vl_acao,
              valor_patrimonial_acao=1000*patrimonio_liquido/vl_acao) 
  
  
  #Margem EBIT
  #Margem Liquida
  #EBIT/ATIVO
  #ROIC
  #ROE
  return(info)
}
