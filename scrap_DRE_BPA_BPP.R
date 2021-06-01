
download_toda_serie<-function(anos,sufixos_desejaveis){
  lista_itr_TODO=baixar_excel_porano(anos,sufixos_desejaveis,"itr")
  lista_dfp_TODO=baixar_excel_porano(anos,sufixos_desejaveis,"dfp")
return(list("itr"=lista_itr_TODO,"dfp"=lista_dfp_TODO))  
}

filtrar_dados_brutos<-function(bulk,empresas){
  lista_itr=bulk[['itr']]
  lista_dfp=bulk[['dfp']]
  
  for(tipo in names(lista_itr)){
    lista_itr[[tipo]]=lista_itr[[tipo]]%>%filter(DENOM_CIA %in% empresas)
  }
  
  for(tipo in names(lista_dfp)){
    lista_dfp[[tipo]]=lista_dfp[[tipo]]%>%filter(DENOM_CIA %in% empresas)
  }
  
    return(list("itr"=lista_itr,"dfp"=lista_dfp))  
}


construir_balancos<-function(bulk,empresas_analisadas){
  #Construcao de Ativo, Passivo e DRE
  ativos=list()
  passivos=list()
  
  for(empresa in empresas_analisadas){
    ativo=abrir_balanco(bulk[['itr']][['itr_cia_aberta_BPA_con']]%>%filter(DENOM_CIA == empresa),bulk[['dfp']][['dfp_cia_aberta_BPA_con']]%>%filter(DENOM_CIA == empresa))
    ativo['DENOM_CIA']=empresa
    ativos[[empresa]]=ativo#rbind(ativos,ativo)
    passivo=abrir_balanco(bulk[['itr']][['itr_cia_aberta_BPP_con']]%>%filter(DENOM_CIA == empresa),bulk[['dfp']][['dfp_cia_aberta_BPP_con']]%>%filter(DENOM_CIA == empresa))
    passivo['DENOM_CIA']=empresa
    #passivos=rbind(passivos,passivo)
    passivos[[empresa]]=passivo
    
  }
  
  #ativo=abrir_balanco(bulk[['itr']][['itr_cia_aberta_BPA_con']]%>%filter(DENOM_CIA %in% empresas_analisadas),bulk[['dfp']][['dfp_cia_aberta_BPA_con']]%>%filter(DENOM_CIA %in% empresas_analisadas))
  return(list("ativo"=ativos,"passivo"=passivos))
  
  
}

construir_dre<-function(bulk,empresas_analisadas){
  dres=list()
  
  for(empresa in empresas_analisadas){
    dre=abrir_DRE(bulk[['itr']][['itr_cia_aberta_DRE_con']]%>%filter(DENOM_CIA == empresa),bulk[['dfp']][['dfp_cia_aberta_DRE_con']]%>%filter(DENOM_CIA == empresa))
    dre['DENOM_CIA']=empresa
    # dres=rbind(dres,dre)
    dres[[empresa]]=dre
  }
  return(dres)
}

listar_datas_divulgacoes<-function(bulk){
  divulgacoes=rbind(bulk[['itr']][['itr_cia_aberta_']],bulk[['dfp']][['dfp_cia_aberta_DRE_']])
  divulgacoes=divulgacoes%>%filter(VERSAO==1)%>%select(DT_REFER,DENOM_CIA,DT_RECEB)
  return(divulgacoes)
}
#Caso se Queira Analisar soh a ponta
# ativo_prova=ativo%>%filter(dte_data>as.Date('2019-08-01'))%>%filter(conta %in% c("1","1.01","1.02","1.01.01","1.01.08.03.03"))
# k=ativo_prova%>%dcast(descricao~dte_data,value.var="valor")
# passivo_prova=passivo%>%filter(dte_data>as.Date('2019-08-01'))%>%filter(conta %in% c("2","2.01","2.02","2.03","2.03.01","2.03.02.05"))
# k=passivo_prova%>%dcast(descricao~dte_data,value.var="valor")
# dre_prova=dre%>%filter(dte_data>as.Date('2019-08-01'))%>%filter(conta %in% c("3.01"))
# k=dre_prova%>%dcast(descricao~dte_data,value.var="valor")
# 








