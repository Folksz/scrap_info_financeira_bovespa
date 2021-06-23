#Metricas por data
metricas_empresas_por_data<-function(empresas_analisadas,tbl_dre,tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker,data){
  depara_cvm=merge(tbl_ticker%>%filter(str_ticker %in% empresas_analisadas),
                     tbl_empresas%>%dplyr::select(regex,str_cia_CVM))%>%summarise(str_ticker,str_cia_CVM)
  numero_de_acoes=buscar_num_acoes(depara_cvm$str_ticker)
  
  dre_df=merge(tbl_dre,depara_cvm,by="str_cia_CVM")
  ativo_df=merge(tbl_bp_ativo,depara_cvm,by="str_cia_CVM")
  passivo_df=merge(tbl_bp_passivo,depara_cvm,by="str_cia_CVM")
  
  dre_anual=dre_df%>%
    group_by(str_ticker,ds_conta)%>%
    arrange(dte_data)%>%
    mutate(cd_conta=paste(cd_conta,"acumulado",sep="_"),
           vl_conta=rollapply(vl_conta,FUN=sum,width=4,align='right',partial=FALSE,fill=NA))
  dre_anual=rbind(dre_df,dre_anual)
  
  indicadores=rbind(ativo_df,
                    passivo_df,
                    dre_anual)%>%filter(dte_data==data)%>%
                    group_by(str_ticker)%>%
    dcast(str_ticker~cd_conta,value.var="vl_conta")
  indicadores=merge(indicadores,numero_de_acoes,by="str_ticker")
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
  
  info=data.frame(t(info))
  colnames(info)=info['str_ticker',]
  info=tail(info,-1)
  #Margem EBIT
  #Margem Liquida
  #EBIT/ATIVO
  #ROIC
  #ROE
  return(info)
}


