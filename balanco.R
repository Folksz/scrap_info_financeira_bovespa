
abrir_balanco<-function(df_itr,df_dfp){
  df_reduzido=df_itr%>%
    select(DT_REFER,DT_FIM_EXERC,CD_CONTA,DS_CONTA,VL_CONTA)%>%
    group_by(DT_FIM_EXERC,CD_CONTA,DS_CONTA)%>%filter(DT_REFER==max(DT_REFER))%>%summarise(VL_CONTA)%>%ungroup()
  
  if(max(df_reduzido$DT_FIM_EXERC) !=max(df_dfp$DT_FIM_EXERC) ){
    df_reduzido=complementar_balanco(df_reduzido,df_dfp)
  }  
  
  return(df_reduzido%>%rename(dte_data=DT_FIM_EXERC,str_conta=CD_CONTA,str_descricao=DS_CONTA,vl_valor=VL_CONTA)%>%arrange(dte_data))
}

complementar_balanco<-function(df_itr_reduzido,df_dfp){
  #print("Existem mais dados no DFP no que no itr")
  datas_faltantes=unique(df_dfp$DT_FIM_EXERC)[!unique(df_dfp$DT_FIM_EXERC)%in%unique(df_itr_reduzido$DT_FIM_EXERC)]
  
  df_dfp_reduzido=df_dfp%>%
    select(DT_REFER,DT_FIM_EXERC,CD_CONTA,DS_CONTA,VL_CONTA)%>%
    group_by(DT_FIM_EXERC,CD_CONTA,DS_CONTA)%>%filter(DT_REFER==max(DT_REFER))%>%summarise(VL_CONTA)
  
  df_dfp_reduzido=df_dfp_reduzido%>%filter(DT_FIM_EXERC %in% datas_faltantes)
  
  df_itr_reduzido=rbind(df_itr_reduzido,df_dfp_reduzido)
  return(df_itr_reduzido)
}