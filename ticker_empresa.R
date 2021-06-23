
ticker_empresa<-function(){
  html="https://www.fundamentus.com.br/detalhes.php?papel="
  depara=(read_html(html)%>%html_nodes(xpath='//*[@id="test1"]')%>%html_table())[[1]]
  colnames(depara)=c("str_ticker","str_nome_cia_ticker","str_razao_social")
  #Tira os Espacos
  depara$str_razao_social=stri_trans_general(depara$str_razao_social, "Latin-ASCII")
  #Tira o Traco
  depara$str_razao_social=  gsub("- ","",as.character(depara$str_razao_social))
  return (depara)
}


construir_depara<-function(bulk){
  tbl_ticker_empresa=ticker_empresa()
  tbl_setores=buscar_setores()
  tbl_CVM=data.frame(unique(bulk$itr$itr_cia_aberta_%>%select(str_cia_CVM=DENOM_CIA,CNPJ_CIA))) 
  
  tbl_empresas=merge(tbl_ticker_empresa%>%mutate(regex=str_replace_all(str_razao_social, "[^[:alnum:]]", ""),
                                        regex=str_replace_all(regex, "BANCO", "BCO")),
                   tbl_CVM%>%mutate(regex=str_replace_all(str_cia_CVM, "[^[:alnum:]]", ""),
                                    regex=str_replace_all(regex, "BANCO", "BCO")),by="regex")%>%select(-regex)

  tbl_empresas=merge(tbl_empresas%>%mutate(regex=str_replace_all(str_ticker, "[^[:alnum:]]", ""),
                                                 regex=gsub('[0-9]+', '', regex)),
                     tbl_setores%>%mutate(regex=str_codigo),by="regex")

  tbl_ticker=tbl_empresas%>%summarise(str_ticker,regex)
 
  tbl_empresas=tbl_empresas%>%select(-str_ticker)
  tbl_empresas=tbl_empresas[!duplicated(tbl_empresas),]
  
  
  return(list("tbl_ticker"=tbl_ticker,"tbl_empresas"=tbl_empresas))
  
  
  
  
}