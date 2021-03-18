rm(list=ls())
#VARIAVEIS GLOBAIS
library("readxl")
library("xlsx")
library("dplyr")
library("stringr")
library("lubridate")
library("reshape2")

sufixos_disponiveis=c('',#OUTRAS INFOS , COMO DATA DA DIVULGACAO
                      'DVA_con', #DEMONSTRACAO DE VALOR ADICIONADO CONSOLIDADO
                      'DVA_ind', #DEMONSTRACAO DE VALOR ADICIONADO INDIVIDUAL
                      'DRE_ind', #DEMONSTRACAO DE RESULTADOS INDIVIDUAL
                      'DRE_con',  #DEMONSTRACAO DE RESULTADOS CONSOLIDADO
                      'DMPL_ind', #DEMONSTRACAO DAS MUTACOES DO PATRIMONIO LIQUIDO INDIVIDUAL
                      'DMPL_con', #DEMONSTRACAO DAS MUTACOES DO PATRIMONIO LIQUIDO CONSOLIDADO
                      'DFC_MD_con', #FLUXO DE CAIXA METODO DIRETO CONSOLIDADO
                      'DFC_MD_ind', #FLUXO DE CAIXA METODO DIRETO INDIVIDUAL
                      'DFC_MI_con', #FLUXO DE CAIXA METODO INDIRETO CONSOLIDADO
                      'DFC_MI_ind', #FLUXO DE CAIXA METODO INDIRETO INDIRETO
                      'BPA_con', #BALANCO PATRIMONIAL ATIVO CONSOLIDADO
                      'BPA_ind', #BALANCO PATRIMONIAL ATIVO INDIVIDUAL
                      'BPP_con', #BALANCO PATRIMONIAL PASSIVO CONSOLIDADO
                      'BPP_ind' ) #BALANCO PATRIMONIAL PASSIVO INDIVIDUAL)


#Importar outros arquivos
source("funcoes_scrap.R")
source("balanco.R")
source("dre.R")


#Mostra todas as Empresas


anos=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
sufixos_desejaveis=c("BPA_con","BPP_con","DRE_con","","DFC_MD_con","DFC_MI_con") #VOU ANALISAR SOH OS CONSOLIDADOS

#Baixa todos os itrs e dfps das empresas consolidadas
lista_itr_TODO=baixar_excel_porano(anos,sufixos_desejaveis,"itr")
lista_dfp_TODO=baixar_excel_porano(anos,sufixos_desejaveis,"dfp")

#Agora escolha de uma soh empresa pra ficar mais facil e paupavel, mas guarda a lista todos pra caso queira usar de novo

#Lista de empresas
empresas=sort(listar_empresas())
empresa_analisada=c("RAIA DROGASIL S.A.")

# Captura os itrs e dfps
lista_itr=filtrar_empresas_desejadas(lista_itr_TODO,empresa_analisada)
lista_dfp=filtrar_empresas_desejadas(lista_dfp_TODO,empresa_analisada)


#Construcao de Ativo, Passivo e DRE
ativo=construir_balanco(lista_itr[['itr_cia_aberta_BPA_con']],lista_dfp[['dfp_cia_aberta_BPA_con']])
passivo=construir_balanco(lista_itr[['itr_cia_aberta_BPP_con']],lista_dfp[['dfp_cia_aberta_BPP_con']])
dre=construir_DRE(lista_itr[['itr_cia_aberta_DRE_con']],lista_dfp[['dfp_cia_aberta_DRE_con']])

#Caso se Queira Analisar soh a ponta
ativo_prova=ativo%>%filter(dte_data>as.Date('2019-08-01'))%>%filter(conta %in% c("1","1.01","1.02","1.01.01","1.01.08.03.03"))
k=ativo_prova%>%dcast(descricao~dte_data,value.var="valor")
passivo_prova=passivo%>%filter(dte_data>as.Date('2019-08-01'))%>%filter(conta %in% c("2","2.01","2.02","2.03","2.03.01","2.03.02.05"))
k=passivo_prova%>%dcast(descricao~dte_data,value.var="valor")
dre_prova=dre%>%filter(dte_data>as.Date('2019-08-01'))%>%filter(conta %in% c("3.01"))
k=dre_prova%>%dcast(descricao~dte_data,value.var="valor")



