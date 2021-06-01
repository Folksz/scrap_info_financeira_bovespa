#Metricas por data
metricas_por_empresa<-function(ativo,passivo,dre,data=as.Date('2021-03-31')){
  #LIQUIDEZ CORR
  dre_df=data.frame()
  ativo_df=data.frame()
  passivo_df=data.frame()
  for(empresa in names(dre)){
  dre_df=rbind(dre_df,dre[[empresa]])  
  ativo_df=rbind(ativo_df,ativo[[empresa]])
  passivo_df=rbind(passivo_df,passivo[[empresa]])
  }
  dre_anual=rbind(dre_df,
                  dre_df%>%
                    group_by(DENOM_CIA,conta)%>%
                    arrange(dte_data)%>%
                    mutate(conta=paste(conta,"acumulado",sep="_"),valor=rollapply(valor,FUN=sum,width=4,align='right',partial=TRUE)))
  #P/L :PRECISA DA COTACAO
  #P/VPa : PRECISA DA COTACAO
  #P/EBIT : PRECISA DA COTACAO
  #PSR
  #EV/EBITDA: PRECISA DA COTACAO
  #EV/EBIT : PRECISA DA COTACAO
  #Margem Bruta
  
  indicadores=rbind(ativo_df%>%mutate(dte_data=as.Date(dte_data)),
                    passivo_df%>%mutate(dte_data=as.Date(dte_data)),
                    dre_anual)%>%filter(dte_data==data)%>%
    group_by(DENOM_CIA)%>%dcast(DENOM_CIA~conta,value.var="valor",fun.aggregate=sum)%>%
    summarise(DENOM_CIA,
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
    divida_bruta_patrimonio=((`2.01.04`+`2.02.01`)/(`2.03`-`2.03.09`)),
    #ROE: Lucro Liquido (acumulado 12 meses)/patrimonio liqudio
    ROE=100*`3.11.01_acumulado`/(`2.03`-`2.03.09`),
    #EBIT (acumulado 12 meses)/ativo
    ebit_ativo=100*(`3.05_acumulado` - `3.04.06_acumulado`- `3.04.05_acumulado`-`3.04.04_acumulado`)/(`1`)
    )                                                                                                                                        
  #Margem EBIT
  #Margem Liquida
  #EBIT/ATIVO
  #ROIC
  #ROE
  return(indicadores)
}
