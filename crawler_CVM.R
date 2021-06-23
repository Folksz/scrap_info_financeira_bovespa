
# sufixos_disponiveis=c('',#OUTRAS INFOS , COMO DATA DA DIVULGACAO
#                       'DVA_con', #DEMONSTRACAO DE VALOR ADICIONADO CONSOLIDADO
#                       'DVA_ind', #DEMONSTRACAO DE VALOR ADICIONADO INDIVIDUAL
#                       'DRE_ind', #DEMONSTRACAO DE RESULTADOS INDIVIDUAL
#                       'DRE_con',  #DEMONSTRACAO DE RESULTADOS CONSOLIDADO
#                       'DMPL_ind', #DEMONSTRACAO DAS MUTACOES DO PATRIMONIO LIQUIDO INDIVIDUAL
#                       'DMPL_con', #DEMONSTRACAO DAS MUTACOES DO PATRIMONIO LIQUIDO CONSOLIDADO
#                       'DFC_MD_con', #FLUXO DE CAIXA METODO DIRETO CONSOLIDADO
#                       'DFC_MD_ind', #FLUXO DE CAIXA METODO DIRETO INDIVIDUAL
#                       'DFC_MI_con', #FLUXO DE CAIXA METODO INDIRETO CONSOLIDADO
#                       'DFC_MI_ind', #FLUXO DE CAIXA METODO INDIRETO INDIRETO
#                       'BPA_con', #BALANCO PATRIMONIAL ATIVO CONSOLIDADO
#                       'BPA_ind', #BALANCO PATRIMONIAL ATIVO INDIVIDUAL
#                       'BPP_con', #BALANCO PATRIMONIAL PASSIVO CONSOLIDADO
#                       'BPP_ind',
#                       "distribuicao_capital") #BALANCO PATRIMONIAL PASSIVO INDIVIDUAL)
serie_CVM<-function(anos){
  sufixos_desejaveis=c("BPA_con","BPP_con","DRE_con","DRE_ind","","DFC_MD_con","DFC_MI_con") #VOU ANALISAR SOH OS CONSOLIDADOS
  lista_itr=excel_por_ano(anos,sufixos_desejaveis,"itr")
  lista_dfp=excel_por_ano(anos,sufixos_desejaveis,"dfp")
  return(list("itr"=lista_itr,"dfp"=lista_dfp))  
}
excel_por_ano<-function(anos,sufixos_desejaveis,itr_dfp){
  lista=list()
    for(ano in anos){
      temp<-tempfile()
      #Baixa o arquivo por ano
      site_base=paste('http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/ITR/DADOS/','itr_cia_aberta_',ano,".zip",sep="")
      site_base=str_replace(site_base,"ITR",toupper(itr_dfp))
      site_base=str_replace(site_base,"itr",tolower(itr_dfp))
      download.file(site_base,temp)
      for(sufixo in sufixos_desejaveis){
        nome_sufixo=paste("itr_cia_aberta_",sufixo,sep="")
        nome_sufixo=str_replace(nome_sufixo,"itr",itr_dfp)
        nome_sufixo_ano=paste(nome_sufixo,"_",ano,sep="")
        nome_sufixo_ano=str_replace(nome_sufixo_ano,"__","_")
        arquivo_sufixo=paste(nome_sufixo_ano,".csv",sep="")
        #Baixa o arquivo pelo sufixo desejado
        unzip(temp,arquivo_sufixo)
        df=read.csv(arquivo_sufixo,sep=";")
        lista[[nome_sufixo]]=rbind(lista[[nome_sufixo]],df)
        unlink(arquivo_sufixo)  
      }  
      unlink(temp)  
    }
  return(lista)
}