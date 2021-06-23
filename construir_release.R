contruir_release<-function(bulk){
  releases=rbind(bulk$itr$itr_cia_aberta_,bulk$dfp$dfp_cia_aberta_)
  releases=releases%>%filter(VERSAO==1)%>%summarise(str_cia_CVM=DENOM_CIA,dte_refer=DT_REFER,dt_publi=DT_RECEB)
  return(releases)
}