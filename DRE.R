construir_DRE<-function(bulk){
  dre=bulk$itr$itr_cia_aberta_DRE_con
  dre_anual=bulk$dfp$dfp_cia_aberta_DRE_con
  dre=dre%>%filter(ORDEM_EXERC=='ÚLTIMO')%>%filter(month(DT_INI_EXERC)==month(DT_FIM_EXERC)-2)
  dre_anual=dre_anual%>%filter(ORDEM_EXERC=='ÚLTIMO')%>%rename(VL_ANUAL=VL_CONTA)
  dre_3tri=dre%>%mutate(ano=year(DT_FIM_EXERC))%>%group_by(ano,CD_CONTA,DENOM_CIA)%>%summarise(tres_tri=sum(VL_CONTA,na.rm=TRUE))
  dre_anual=merge(dre_anual%>%mutate(ano=year(DT_REFER)),dre_3tri,by=c("ano","CD_CONTA","DENOM_CIA"))
  dre_anual$VL_CONTA=dre_anual$VL_ANUAL-dre_anual$tres_tri
  
  dre=rbind(dre%>%dplyr::select(DENOM_CIA,CD_CONTA,VL_CONTA,DT_FIM_EXERC,DS_CONTA),
            dre_anual%>%dplyr::summarise(DENOM_CIA,CD_CONTA,VL_CONTA,DT_FIM_EXERC,DS_CONTA))
  colnames(dre)=c("str_cia_CVM","cd_conta","vl_conta","dte_data","ds_conta")
  return(dre)
}
