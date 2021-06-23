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