
evolucao_dre_por_empresa<-function(ticker){
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
  empresa=(bulk$tbl_ticker%>%filter(str_ticker==ticker))$str_nome_cia
  dre_empresa=bulk$dre%>%filter(str_nome_cia==empresa)
  dre_empresa=dre_empresa%>%group_by(str_conta)%>%
    arrange(dte_data)%>%
    mutate(vl_valor_acumulado12M=rollapply(vl_valor,FUN=sum,width=4,align='right',partial=FALSE,fill=NA),
                                        variacao_3M=100*round(vl_valor/lag(vl_valor,1)-1,2),
                                        variacao_anual=100*(vl_valor_acumulado12M/lag(vl_valor_acumulado12M,1)-1))
  
  
  
  info=dre_empresa%>%filter(str_conta %in%   names(quais_mostrar))%>%mutate(str_conta=quais_mostrar[str_conta])
  info=info%>%ungroup()%>%select(dte_data,variacao_3M,variacao_anual,vl_valor,vl_valor_acumulado12M,str_conta)%>%
    gather(variable,value,-c(dte_data,str_conta))
  info=info%>%dcast(paste(str_conta,variable)~dte_data)
  info[,-1]=round(info[,-1],2)    
colnames(info)[1]=ticker
  return(info)
}



evolucao_bp_por_empresa<-function(ticker){
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
  
  empresa=(bulk$tbl_ticker%>%filter(str_ticker==ticker))$str_nome_cia
  
  
  bp_empresa=rbind(bulk$ativo%>%filter(str_nome_cia==empresa),bulk$passivo%>%filter(str_nome_cia==empresa))
  
  bp_empresa=bp_empresa%>%
    group_by(str_nome_cia,str_conta)%>%
    arrange(dte_data)%>%
    mutate(variacao_3M=100*round(vl_valor/lag(vl_valor,1)-1,2),
           variacao_anual=100*round(vl_valor/lag(vl_valor,4)-1,3))
  
  info=bp_empresa%>%
    filter(str_conta %in% names(quais_mostrar))%>%
    mutate(str_conta=quais_mostrar[str_conta])
  
  info=info%>%
    ungroup()%>%
    select(dte_data,variacao_3M,variacao_anual,vl_valor,str_conta)%>%
    gather(variable,value,-c(dte_data,str_conta))
  

  
  info=info%>%dcast(paste(str_conta,variable)~dte_data)
  info[,-1]=round(info[,-1],2)    
  colnames(info)[1]=ticker  
  return(info)
}






evolucao_metrica_por_empresa<-function(ticker){
  
  empresa=unique((bulk$tbl_ticker%>%filter(str_ticker==ticker))$str_nome_cia)
  
  
  dre_df=bulk$dre%>%filter(str_nome_cia==empresa)
  ativo_df=bulk$ativo%>%filter(str_nome_cia==empresa)
  passivo_df=bulk$passivo%>%filter(str_nome_cia==empresa)
  
  dre_anual=rbind(dre_df,
                  dre_df%>%
                    group_by(str_conta)%>%
                    arrange(dte_data)%>%
                    mutate(str_conta=paste(str_conta,"acumulado",sep="_"),vl_valor=rollapply(vl_valor,FUN=sum,width=4,align='right',partial=FALSE,fill=NA)))
  
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
    dcast(dte_data~str_conta,value.var="vl_valor",fun.aggregate=sum)
  
  
  indicadores$vl_acao=(bulk$num_acoes%>%filter(str_ticker==ticker))$vl_acao
  
    
  info=indicadores%>%
    summarise(dte_data,
              #Ativo Circulante/Passivo Circulante
              patrimonio_liquido=(`2.03`-`2.03.09`),
              lucro_liquido_12M=`3.11.01_acumulado`,
              
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
              
              lucro_liquido_acao=1000*lucro_liquido_12M/vl_acao,
              valor_patrimonial_acao=1000*patrimonio_liquido/vl_acao)  
              
              
  return(info)
}
