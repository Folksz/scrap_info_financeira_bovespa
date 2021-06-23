construir_ativo<-function(bulk){
  bp_ativo=bulk$itr$itr_cia_aberta_BPA_con
  bp_ativo_anual=bulk$dfp$dfp_cia_aberta_BPA_con
  
  bp_ativo=bp_ativo%>%filter(ORDEM_EXERC=='ÚLTIMO')
  bp_ativo_anual=bp_ativo_anual%>%filter(ORDEM_EXERC=='ÚLTIMO')
  
  
  bp_ativo=rbind(bp_ativo%>%dplyr::select(DENOM_CIA,CD_CONTA,VL_CONTA,DT_FIM_EXERC,DS_CONTA),
                 bp_ativo_anual%>%dplyr::summarise(DENOM_CIA,CD_CONTA,VL_CONTA,DT_FIM_EXERC,DS_CONTA))
  colnames(bp_ativo)=c("str_cia_CVM","cd_conta","vl_conta","dte_data","ds_conta")
  return(bp_ativo)
}

construir_passivo<-function(bulk){
  bp_passivo=bulk$itr$itr_cia_aberta_BPP_con
  bp_passivo_anual=bulk$dfp$dfp_cia_aberta_BPP_con
  
  bp_passivo=bp_passivo%>%filter(ORDEM_EXERC=='ÚLTIMO')
  bp_passivo_anual=bp_passivo_anual%>%filter(ORDEM_EXERC=='ÚLTIMO')
  
  
  bp_passivo=rbind(bp_passivo%>%dplyr::select(DENOM_CIA,CD_CONTA,VL_CONTA,DT_FIM_EXERC,DS_CONTA),
                   bp_passivo_anual%>%dplyr::summarise(DENOM_CIA,CD_CONTA,VL_CONTA,DT_FIM_EXERC,DS_CONTA))
  colnames(bp_passivo)=c("str_cia_CVM","cd_conta","vl_conta","dte_data","ds_conta")
  
  return(bp_passivo)
}


