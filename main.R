rm(list=ls())
source("libraries.R")
#Inicializar com bulk()
#Construcao do BD
anos=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
construir_bd(anos,"output/")

#Atualizar Ponta, soh o ultimo ano
atualizar_ponta(diretorio="output/")


diretorio="output/"
tbl_dre<-read.csv(paste(diretorio,"tbl_dre.csv",sep=""))
tbl_bp_ativo<-read.csv(paste(diretorio,"tbl_bp_ativo.csv",sep=""))
tbl_bp_passivo<-read.csv(paste(diretorio,"tbl_bp_passivo.csv",sep=""))
tbl_empresas<-read.csv(paste(diretorio,"tbl_empresas.csv",sep=""))
tbl_ticker<-read.csv(paste(diretorio,"tbl_ticker.csv",sep=""))
tbl_divulgacoes<-read.csv(paste(diretorio,"tbl_divulgacoes.csv",sep=""))

#Construcao de Metricas Empresas por Data
empresas_analisadas=c("ABEV3","RADL3","MGLU3")
empresas_por_data=metricas_empresas_por_data(empresas_analisadas,tbl_dre,tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker,data='2021-03-31')
#Construcao de Metricas Evolucao de empresas
dre_evolucao=evolucao_dre_por_empresa(ticker="ABEV3",tbl_dre,tbl_empresas,tbl_ticker)
bp_evolucao=evolucao_bp_por_empresa(ticker="ABEV3",tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker)
metrica_evolucao=evolucao_metrica_por_empresa(ticker="ABEV3",tbl_dre,tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker)
#Construcao Metriaca preco
metrica_preco=metrica_via_preco(tickers=c("PETR3","RADL3"),data_inicio=as.Date('2011-01-01'),data_fim=Sys.Date())