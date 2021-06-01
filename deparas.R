depara_nomefantasia_codigo<-function(){
  html="https://www.fundamentus.com.br/detalhes.php?papel="
  depara=(read_html(html)%>%html_nodes(xpath='//*[@id="test1"]')%>%html_table())[[1]]
  colnames(depara)=c("ticker","nome_comercial","razao_social")
  depara%>%filter(ticker=="PETR3")
  depara$razao_social=  stri_trans_general(depara$razao_social, "Latin-ASCII")
  depara$razao_social=  gsub("- ","",as.character(depara$razao_social))

  return (depara)
}

buscar_setores<-function(){
  temp<-tempfile()
  download.file("http://www.b3.com.br/lumis/portal/file/fileDownload.jsp?fileId=8AA8D0975A2D7918015A3C81693D4CA4",temp,mode="wb")
  nome_arquivo=as.character(unzip(temp,list=TRUE)[1])
  unzip(temp,files=nome_arquivo)
  df=read.xlsx(nome_arquivo,sheetIndex=1,encoding = "UTF-8")
  df=df[rowSums(!is.na(df))!=0,]
  colnames(df)=c("setor","subsetor","nome","codigo","mercado","segmento")
  df$segmento=ifelse(is.na(df$codigo) & is.na(df$segmento),df$nome,NA  )
  df=df%>%dplyr::filter(mercado!="SEGMENTO" | is.na(mercado))
  df$setor=na.locf(df$setor,na.rm=FALSE)
  df$subsetor=na.locf(df$subsetor,na.rm=FALSE)
  df$segmento=na.locf(df$segmento,na.rm=FALSE)
  df=df%>%filter(!is.na(codigo))
  df=df%>%dplyr::filter(nome!="SEGMENTO" )
  return(df)
}

listar_empresas<-function(anos,sufixos_desejaveis){
  empresas=unique(baixar_excel_porano(c(2020),'',"itr")[['itr_cia_aberta_']]$DENOM_CIA)
  return(empresas)
}
listar_empresas_cnpj<-function(anos,sufixos_desejaveis){
  empresas=unique(baixar_excel_porano(c(2020),'',"itr")[['itr_cia_aberta_']][,c("CNPJ_CIA","DENOM_CIA")])
  return(empresas)
}

