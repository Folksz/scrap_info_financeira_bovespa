df_itr=lista_itr[['itr_cia_aberta_DRE_con']]
df_dfp=lista_dfp[['dfp_cia_aberta_DRE_con']]

construir_DRE<-function(df_itr,df_dfp){
  # df=lista_itr[['itr_cia_aberta_BPA_con']]
  #GUARDAR O NUMERO A PARTIR DO CODIGO
  
  nomes_unicos=unique(df_itr[,c("CD_CONTA","DS_CONTA")])
  nomes_unicos=nomes_unicos[!(duplicated(nomes_unicos$CD_CONTA)),]
  #QUERO PEGAR SOH O DADO TRIMESTRAL
 
   df_itr_reduzido=df_itr%>%select(DT_REFER,CD_CONTA,VL_CONTA,DT_INI_EXERC,DT_FIM_EXERC,DT_REFER)
  
  #SUPOSICOES: O 3 tri sempre pega o de maior dt_refer
  #SUPOSICOES: O 2 tri sempre pega o de maior dt_refer
  #SUPOSICOES: O 1 tri eh a diferenca entre o acumulado de 1 a 3 - 2 - 3
  #SUPONDO PEGAR A ULTIMA VERSAO
   
   df_itr_reduzido=df_itr_reduzido%>%group_by(DT_INI_EXERC,
                           DT_FIM_EXERC,
                           CD_CONTA)%>%filter(DT_REFER==max(DT_REFER))%>%
    summarise(VL_CONTA)%>%ungroup()
  
   df_itr_reduzido=df_itr_reduzido%>%group_by(CD_CONTA)%>%
    mutate(
      VL_CONTA=ifelse( year(DT_FIM_EXERC) != year(today()) & month(DT_FIM_EXERC)==3 & month(DT_INI_EXERC)==1,lead(VL_CONTA,1)-lead(VL_CONTA,3),VL_CONTA))%>%
    filter(month(DT_FIM_EXERC)==month(DT_INI_EXERC)+2)%>%select(-DT_INI_EXERC)%>%ungroup()
   df_itr_reduzido$DT_FIM_EXERC=as.Date(df_itr_reduzido$DT_FIM_EXERC)  
  #PEGA A ULTIMA VERSAO  
  
  df_dfp_reduzido=df_dfp%>%select(DT_REFER,CD_CONTA,VL_CONTA,DT_FIM_EXERC,DT_REFER)
  df_dfp_reduzido=df_dfp_reduzido%>%group_by(DT_FIM_EXERC,CD_CONTA)%>%filter(DT_REFER==max(DT_REFER))%>%summarise(VL_CONTA)

  #ESTRATEGIA PARA PEGAR O ULTIMO SEMESTRE

  ultimo_trimestre_incompleto=df_itr_reduzido%>%
    group_by(
      CD_CONTA,
      ano=year(DT_FIM_EXERC))%>%
    summarise(incompleto=sum(VL_CONTA))%>%ungroup() #AQUI FAZ A SOMA DO INCOMPLETO
    
    ultimo_trimestre_completo=df_dfp_reduzido%>%
            group_by(CD_CONTA,ano=year(DT_FIM_EXERC))%>%
            summarise(completo=sum(VL_CONTA))%>%ungroup()
  #A SUBTRACAO DOS DOIS DAH O ULTIMO TRIMESTRE

  ultimo_trimestre=merge(ultimo_trimestre_completo,ultimo_trimestre_incompleto,by=c("ano","CD_CONTA"))%>%summarise(
    DT_FIM_EXERC=as.Date(paste(ano,'12','31',sep='-')),
    CD_CONTA,
    VL_CONTA=completo-incompleto
  )  
    
    

  df_itr_reduzido=rbind(df_itr_reduzido,ultimo_trimestre%>%select(DT_FIM_EXERC,CD_CONTA,VL_CONTA))
  df_itr_reduzido=merge(df_itr_reduzido,nomes_unicos,by=c("CD_CONTA"),all=TRUE)
  df_itr_reduzido=df_itr_reduzido%>%select(dte_data=DT_FIM_EXERC,conta=CD_CONTA,descricao=DS_CONTA,valor=VL_CONTA)%>%arrange(dte_data)


  return(df_itr_reduzido)
}