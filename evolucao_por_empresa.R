
evolucao_dre_por_empresa<-function(){
  quais_mostrar=list("3.01"="3.01-Receita Bruta",
                     "3.02"="3.02-Custo",
                     "3.03"="3.03-Lucro Bruto",
                     "3.04"="3.04-Despesa Operacional",
                     #                     "3.05"="Receita Bruta",
                     "3.06"="3.06-Resultado Financeiro",
                     "3.07"="3.07-Ebitda",
                     "3.08"="3.08-IR e Outros",
                     "3.09"="3.09-Lucro Liquido"
  )
  infos=list()
  for(empresa in empresas_analisadas){
    dre_empresa=dre[[empresa]]
    dre_empresa=dre_empresa%>%group_by(DENOM_CIA,conta)%>%arrange(dte_data)%>%mutate(valor_acumulado12M=rollapply(valor,FUN=sum,width=4,align='right',partial=TRUE),
                                                                                     variacao_3M=100*round(valor/lag(valor,1)-1,2),
                                                                                     variacao_M12=100*(valor_acumulado12M/lag(valor_acumulado12M,1)-1),12)
    info=dre_empresa%>%filter(conta %in%   names(quais_mostrar))%>%mutate(nome_empresa=quais_mostrar[conta])
    info=info%>%ungroup()%>%select(dte_data,variacao_3M,variacao_M12,valor,valor_acumulado12M,nome_empresa)%>%
      gather(variable,value,-c(dte_data,nome_empresa))
    info=info%>%dcast(paste(nome_empresa,variable)~dte_data)
    info[,-1]=round(info[,-1],2)    
    infos[[empresa]]=info
  }
  return(infos)
}



evolucao_bp_por_empresa<-function(){
  quais_mostrar=list("1"="1-Ativo Total",
                     "1.01"="1.01-Ativo Circulante",
                     "1.01.03"="1.01.03-Contas a Receber",
                     "1.01.04"="1.01.04-Estoques",
                     "1.02"="1.02-Ativo Nao Circulante",
                     "1.02.01"="1.02.01-Ativo Realizavel a Longo Prazo",
                     "1.02.03"="1.02.03-Imobilizado",
                     "1.02.04"="1.02.04-Estoques",
                     #--------------------------------#
                     "2"="2-Passivo Total",
                     "2.01"="2.01-Passivo Circulante",
                     "2.01.02"="2.01.02-Fornecedores",
                     "2.02"="2.02-Passivo Nao Circulante",
                     "2.03"="2.03-Patrimonio Liquido")
  infos=list()
  for(empresa in empresas_analisadas){
    bp_empresa=rbind(ativo[[empresa]],passivo[[empresa]])
    
    bp_empresa=bp_empresa%>%
      group_by(DENOM_CIA,conta)%>%
      arrange(dte_data)%>%
      mutate(variacao_3M=100*round(valor/lag(valor,1)-1,2),
             variacao_12M=100*round(valor/lag(valor,4)-1,3))
    
    info=bp_empresa%>%
      filter(conta %in% names(quais_mostrar))%>%
      mutate(nome_empresa=quais_mostrar[conta])
    
    info=info%>%
      ungroup()%>%
      select(dte_data,variacao_3M,variacao_12M,valor,nome_empresa)%>%
      gather(variable,value,-c(dte_data,nome_empresa))
    
    info=info%>%dcast(paste(nome_empresa,variable)~dte_data)
    info[,-1]=round(info[,-1],2)    
    infos[[empresa]]=info
  }
  return(infos)
}







evolucao_metrica_por_empresa<-function(){
  info=list()
  
  for(empresa in names(ativo)){
    
    dre_df=dre[[empresa]]
    ativo_df=ativo[[empresa]]
    passivo_df=passivo[[empresa]]
    
    dre_anual=rbind(dre_df,
                    dre_df%>%
                      group_by(conta)%>%
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
                      dre_anual)%>%group_by(dte_data)%>%
      dcast(dte_data~conta,value.var="valor",fun.aggregate=sum)%>%
      summarise(dte_data,
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
    
    info[[empresa]]=indicadores
  }
  
  
  
  
  return(info)
  
}
