#pega do fundamentus o ticker, nome comercial e razao social

buscar_num_acoes<-function(tickers){
  num_acoes=data.frame()
  for(ticker in tickers){
    html=paste("https://www.fundamentus.com.br/detalhes.php?papel=",ticker,sep="")
    acao=as.numeric(str_replace_all((read_html(html)%>%html_nodes(xpath='//*[@class="w728"]')%>%
                                            html_table())[[2]][4][2,1],"\\.", ""))
    num_acoes=rbind(num_acoes,data.frame("str_ticker"=ticker,"vl_acao"=acao))
  }
  return(num_acoes)
}


depara_nomefantasia_codigo<-function(){
  html="https://www.fundamentus.com.br/detalhes.php?papel="
  depara=(read_html(html)%>%html_nodes(xpath='//*[@id="test1"]')%>%html_table())[[1]]
  colnames(depara)=c("str_ticker","str_nome_comercial","str_nome_cia")
  #Tira os Espacos
  depara$str_nome_cia=  stri_trans_general(depara$str_nome_cia, "Latin-ASCII")
  #Tira o Traco
  depara$str_nome_cia=  gsub("- ","",as.character(depara$str_nome_cia))

  return (depara)
}

buscar_setores<-function(){
  temp<-tempfile()
  download.file("http://www.b3.com.br/lumis/portal/file/fileDownload.jsp?fileId=8AA8D0975A2D7918015A3C81693D4CA4",temp,mode="wb")
  nome_arquivo=as.character(unzip(temp,list=TRUE)[1])
  unzip(temp,files=nome_arquivo)
  df=read.xlsx(nome_arquivo,sheetIndex=1,encoding = "UTF-8")
  df=df[rowSums(!is.na(df))!=0,]
  colnames(df)=c("str_setor","str_subsetor","str_nome_comercial","str_codigo","str_mercado","str_segmento")
  df=df%>%filter(str_mercado != "SEGMENTO" | is.na(str_mercado))%>%
    mutate(str_segmento=ifelse(is.na(str_codigo) & is.na(str_segmento),str_nome_comercial,NA))
  df[,c("str_setor","str_subsetor","str_segmento")]=na.locf(df[,c("str_setor","str_subsetor","str_segmento")],na.rm=FALSE)
  df=df%>%filter(!is.na(str_codigo) | str_nome_comercial !="SEGMENTO")%>%mutate(str_nome_comercial=trimws(str_nome_comercial))
  
  return(df)
}

listar_empresas<-function(anos,sufixos_desejaveis){
  empresas=unique(baixar_excel_porano(c(2020,2021),'',"itr")[['itr_cia_aberta_']]$DENOM_CIA)
  return(empresas)
}
listar_empresas_cnpj<-function(anos,sufixos_desejaveis){
  empresas=unique(baixar_excel_porano(c(2020,2021),'',"itr")[['itr_cia_aberta_']][,c("CNPJ_CIA","DENOM_CIA")])
  colnames(empresas)=c("str_cnpj","str_nome_cia")
  return(empresas)
}

