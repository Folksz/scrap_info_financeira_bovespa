rm(list=ls())

source("libraries.R")

#Inicializar com bulk()

#Para Inicializar o Programa eh necessario definir quais anos quer desejar e quais empresas

anos=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
#anos=c(2020,2021)

tickers=c("PETR3","PETR4","ABEV3","RADL3")

inicializar(anos,tickers)




#Eh a metrica pra cada empresa em uma data especifica
metricas=metricas_empresas_por_data(tickers=c('PETR3','RADL3'),data=as.Date('2021-03-31'))

dre_evolucao=evolucao_dre_por_empresa(ticker="PETR3")
bp_evolucao=evolucao_bp_por_empresa(ticker="PETR3")
metricas_evolucao=evolucao_metrica_por_empresa(ticker="PETR3")

#Agora incluindo uma biblioteca que puxa os tickers
acoes=get_acoes(tickers=c('PETR3','RADL3'),data_inicio=as.Date('2000-01-01'),Sys.Date())
#Metricas de Preco e Usando o release  pra pegar o dia exato

metricas_preco=metrica_via_preco(tickers=c("PETR3","RADL3"),data_inicio=as.Date('2012-01-01'),data_fim=Sys.Date())













