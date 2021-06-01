rm(list=ls())

source("libraries.R")


main<-function(){
  
anos=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
sufixos_desejaveis=c("BPA_con","BPP_con","DRE_con","","DFC_MD_con","DFC_MI_con") #VOU ANALISAR SOH OS CONSOLIDADOS

#Funcoes Depara
empresas=sort(listar_empresas()) #Lista todas as empresas disponiveis
cnpjs=listar_empresas_cnpj() #Faz um depara da empresa com o cnpj correspondente
depara_empresas=depara_nomefantasia_codigo() #Faz um depara entre nome fantasia, ticker e nome empresa
depara_setores=buscar_setores() #Lista os setores e subsetores de cada empresa



empresas_analisadas=c("RAIA DROGASIL S.A.","PETROLEO BRASILEIRO S.A. PETROBRAS")
bulk=download_toda_serie(anos,sufixos_desejaveis) #PUXA TODAS AS INFOS DE TODAS AS EMPRESAS


bulk=filtrar_dados_brutos(bulk,empresas_analisadas)#FILTRA SO PRAS EMPRESAS DESEJADAS


depara_divulgacoes=listar_datas_divulgacoes(bulk) #Cria a data de divulgação das empresas


#Lista de empresas disponiveis
balancos=construir_balancos(bulk,empresas_analisadas)
ativo=balancos$ativo
passivo=balancos$passivo



dre=construir_dre(bulk,empresas_analisadas)



#Eh a metrica pra cada empresa em uma data especifica
metricas=metricas_por_empresa(ativo,passivo,dre,
                              data=as.Date('2021-03-31'))



dre_evolucao=evolucao_dre_por_empresa()
bp_evolucao=evolucao_bp_por_empresa()
metricas_evolucao=evolucao_metrica_por_empresa()

#Agora incluindo uma biblioteca que puxa os tickers
acoes=get_acoes(as.Date('2000-01-01'),Sys.Date())


#DOLAR,JUROS,INFLACAO


}



