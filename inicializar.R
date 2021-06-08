
inicializar<-function(anos,tickers){
  sufixos_desejaveis=c("BPA_con","BPP_con","DRE_con","","DFC_MD_con","DFC_MI_con") #VOU ANALISAR SOH OS CONSOLIDADOS
  
  tbl_empresas=depara_nomefantasia_codigo() #Faz um depara entre nome fantasia, ticker e nome empresa
  #Pega todos os tickers para criar uma table
  cnpjs=listar_empresas_cnpj()
  #Agrega as informacoes das empresas com o CNPJ
  tbl_empresas=merge(tbl_empresas,cnpjs,by="str_nome_cia")
  depara_setores=buscar_setores() #Lista os setores e subsetores de cada empresa
  tbl_empresas=merge(tbl_empresas,depara_setores,by="str_nome_comercial")
  
  
  tbl_ticker=tbl_empresas%>%select(str_ticker,str_nome_cia)
  
  tbl_empresas=tbl_empresas%>%select(-str_ticker)
  tbl_empresas=tbl_empresas[!duplicated(tbl_empresas),]

  #Procura a equivalencia de ticker e empresa
  empresas=unique((tbl_ticker%>%filter(str_ticker %in% tickers))$str_nome_cia)
  

  bulk=download_toda_serie(anos,sufixos_desejaveis) #PUXA TODAS AS INFOS DE TODAS AS EMPRESAS
  bulk=filtrar_dados_brutos(bulk,empresas)#FILTRA SO PRAS EMPRESAS DESEJADAS
  
  tbl_divulgacoes=listar_datas_divulgacoes(bulk) #Cria a data de divulga??o das empresas
  tbl_divulgacoes=tbl_divulgacoes%>%filter(str_nome_cia %in% empresas)
  
  #Lista de empresas disponiveis
  balancos=construir_balancos(bulk)
  ativo=balancos$ativo
  passivo=balancos$passivo
  dre=construir_dre(bulk)
  
  num_acoes=buscar_num_acoes(tickers)
  

  
  bulk<<-list(tbl_ticker=tbl_ticker,
              tbl_empresas=tbl_empresas,
              ativo=ativo,
              passivo=passivo,
              dre=dre,
              num_acoes=num_acoes,
              tbl_divulgacoes=tbl_divulgacoes)
  
}