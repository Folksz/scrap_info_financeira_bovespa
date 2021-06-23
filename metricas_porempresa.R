evolucao_dre_por_empresa<-function(ticker,tbl_dre,tbl_empresas,tbl_ticker){
  quais_mostrar=list("3.01"="Receita_Bruta",
                     "3.02"="Custo",
                     "3.03"="Lucro_Bruto",
                     "3.04"="Despesa_Operacional",
                     "3.06"="Resultado_Financeiro",
                     "3.07"="Ebitda",
                     "3.08"="IR_e_Outros",
                     "3.09"="Lucro_Liquido"
  )
  empresa_analisada=(merge(tbl_ticker%>%filter(str_ticker == ticker),
                          tbl_empresas%>%dplyr::select(regex,str_cia_CVM))%>%summarise(str_ticker,str_cia_CVM))$str_cia_CVM

  dre_empresa=tbl_dre%>%filter(str_cia_CVM==empresa_analisada)
  dre_empresa=dre_empresa%>%
    group_by(cd_conta)%>%
    arrange(dte_data)%>%
    mutate(vl_valor_acumulado12M=rollapply(vl_conta,FUN=sum,width=4,align='right',partial=FALSE,fill=NA),
           variacao_3M=100*(vl_conta/lag(vl_conta,1)-1),
           variacao_anual=100*(vl_valor_acumulado12M/lag(vl_valor_acumulado12M,1)-1))
  
  info=dre_empresa%>%filter(cd_conta %in%   names(quais_mostrar))%>%mutate(str_conta=quais_mostrar[cd_conta])
  info=info%>%ungroup()%>%select(dte_data,variacao_3M,variacao_anual,vl_conta,vl_valor_acumulado12M,str_conta)%>%
    gather(variable,value,-c(dte_data,str_conta))
  info=info%>%dcast(paste(str_conta,variable)~dte_data)
  colnames(info)[1]=ticker
  x=t(info)
  colnames(x)=x[1,]
  x=x[-1,]
  x=data.frame(x)
  return(x)
}
evolucao_bp_por_empresa<-function(ticker,tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker){
  quais_mostrar=list("1"="Ativo_Total",
                     "1.01"="Ativo_Circulante",
                     "1.01.03"="Contas_a_Receber",
                     "1.01.04"="Estoques_Circulante",
                     "1.02"="Ativo_Nao_Circulante",
                     "1.02.01"="Ativo_Realizavel_a_Longo_Prazo",
                     "1.02.03"="Imobilizado",
                     "1.02.04"="Estoques_Nao_Circulante",
                     #--------------------------------#
                     "2"="Passivo_Total",
                     "2.01"="Passivo_Circulante",
                     "2.01.02"="Fornecedores",
                     "2.02"="Passivo_Nao_Circulante",
                     "2.03"="Patrimonio_Liquido")
  empresa_analisada=(merge(tbl_ticker%>%filter(str_ticker == ticker),
                           tbl_empresas%>%dplyr::select(regex,str_cia_CVM))%>%summarise(str_ticker,str_cia_CVM))$str_cia_CVM
  bp_empresa=rbind(tbl_bp_ativo%>%filter(str_cia_CVM==empresa_analisada),
                   tbl_bp_passivo%>%filter(str_cia_CVM==empresa_analisada))
  bp_empresa=bp_empresa%>%
    group_by(cd_conta)%>%
    arrange(dte_data)%>%
    mutate(variacao_3M=100*(vl_conta/lag(vl_conta,1)-1),
           variacao_anual=100*(vl_conta/lag(vl_conta,4)-1))
  info=bp_empresa%>%
    filter(cd_conta %in% names(quais_mostrar))%>%
    mutate(ds_conta=quais_mostrar[cd_conta])
  info=info%>%
    ungroup()%>%
    select(dte_data,variacao_3M,variacao_anual,vl_conta,ds_conta)%>%
    gather(variable,value,-c(dte_data,ds_conta))
  info=info%>%dcast(paste(ds_conta,variable)~dte_data)
  colnames(info)[1]=ticker  
  info=t(info)
  colnames(info)=info[1,]
  info=data.frame(info[-1,])
  return(info)
}
evolucao_metrica_por_empresa<-function(ticker,tbl_dre,tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker){
  empresa_analisada=(merge(tbl_ticker%>%filter(str_ticker == ticker),
                           tbl_empresas%>%dplyr::select(regex,str_cia_CVM))%>%summarise(str_ticker,str_cia_CVM))$str_cia_CVM
  dre_df=tbl_dre%>%filter(str_cia_CVM==empresa_analisada)
  ativo_df=tbl_bp_ativo%>%filter(str_cia_CVM==empresa_analisada)
  passivo_df=tbl_bp_passivo%>%filter(str_cia_CVM==empresa_analisada)
  
  dre_anual=rbind(dre_df,
                  dre_df%>%
                    group_by(cd_conta)%>%
                    arrange(dte_data)%>%
                    mutate(cd_conta=paste(cd_conta,"acumulado",sep="_"),
                           vl_conta=rollapply(vl_conta,FUN=sum,width=4,align='right',partial=FALSE,fill=NA)))
  indicadores=rbind(ativo_df,
                    passivo_df,
                    dre_anual)%>%group_by(dte_data)%>%
    dcast(dte_data~cd_conta,value.var="vl_conta",fun.aggregate=sum)
  numero_de_acoes=buscar_num_acoes(ticker)
  indicadores$vl_acao=numero_de_acoes$vl_acao
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
  
  colnames(info)[1]=ticker
  return(info)
}
