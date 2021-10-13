#Autor: João Maria de Andrade 
#Data: 06-09-2021
#Versão:01 
#Objetivo: Script para leitura dos dados do Inmet e extração das coordenadas 


#Bibliotecas Utilizadas 

library(dplyr) #Manipulação de Data frame 
library(readr) #Leitura dos arquivos csv - https://github.com/rstudio/cheatsheets/blob/master/data-import.pdf
library (hydroGOF) #Estatisticas de desempenho 
library(lubridate) #Manipulação de dados mensais, diários, anuais 

#-------------------------------------------------------
#Parte 1- Listar todos os arquivos com os dados das estações

#files_Inmet = list.files("C:\\Users\\jonet\\Documents\\Doutorado-UFPE\\Periodo 2021_2\\SIG\\01_Dados de Campo\\01_Est_Convencional_2000_2020\\"
#                         , pattern='*.csv',full.names=TRUE)

filterDate_estacoes2 <- read_csv("filterDate_estacoes2.csv") #Estações com filtro
#View(filterDate_estacoes2)
files_Inmet=filterDate_estacoes2$x


#Dados de satélite 
#name_sat<-c("PERSIANN-CDRStats_2000-01-01_2021-01-01") ptsERAStats_2000-01-01_2020-12-31_2
#name_sat<-c("ptsTRMM_mm_hour_2000-01-01_2021-01-01")
#name_sat<-c("ptsTERRACLIMATE_mm_2000-01-01_2021-01-01")
name_sat<-c("ptsEraMonth_2000-01-01_2021-01-01")
cam_sat<-paste0("02_Dados de Satelite/",name_sat,".csv")
#dados_satelite <-read_csv("02_Dados de Satelite/ptsCHIRPSStats_2000-01-01_2021-01-01.csv") #Chirps 
dados_satelite <-read_csv(cam_sat) #Outos satélites 

#-----------Pré-processamento dos dados TRMM - Excluvisamente------------------ 
#dados_satelite_temp1<-dados_satelite

#dados_satelite <- dados_satelite_temp1 %>%
#  mutate(precipitation = precipitation*days_in_month(date)*24)

#-----------Pré-processamento dos dados ERA (media mensal)- Excluvisamente------------------ 
dados_satelite_temp1<-dados_satelite

dados_satelite <- dados_satelite_temp1 %>%
  mutate(precipitation = precipitation*days_in_month(date)*1000)

#---------------------------------------------------------------------------------------------
#round(dados_satelite2$total_precipitation,2)

#Selecao das colunas desejaveis 
dados_est<-select(dados_satelite,date,Nome, precipitation) #Dados Chirps
#dados_est<-select(dados_satelite,date,Nome, total_precipitation) #ERA_Land
names(dados_est)<-c("date","Nome","precipitation")
dados_est$precipitation<-round(dados_est$precipitation,2) #Deixar os dados com duas casas decimais 


#DadaIo<-c('2000-01-01')
#DadaFim<-c('2020-12-31')
#Quant_dados<-length(seq(from=as.Date(DadaIo), to=as.Date(DadaFim), by=1))

#Constução de uma data.frame vazio para armazenar as estatísticas produzidas no processamento
Dados_estatisticas=NULL

for (i in 1:(length(files_Inmet))) {
#i=1
print(i)
dados_estacao <- read_delim(files_Inmet[i],delim = ";", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE,skip=9)


#Leitura dos dados de precipitação 
data_precipitation_temp1<-select(dados_estacao,`Data Medicao`,`PRECIPITACAO TOTAL, DIARIO(mm)`)


#Convertendo os valores de precipitação em número 
data_precipitation_temp1$`PRECIPITACAO TOTAL, DIARIO(mm)`=as.numeric(data_precipitation_temp1$`PRECIPITACAO TOTAL, DIARIO(mm)`)

Confe_num<-data_precipitation_temp1$`PRECIPITACAO TOTAL, DIARIO(mm)`
#if (is.na(sum(data_precipitation_temp1$`PRECIPITACAO TOTAL, DIARIO(mm)`))==FALSE) {

#Leitura das informações da estação - Nome, Latitude e Longitude. 
Dado_Est <- read_delim(files_Inmet[i], delim = ":", escape_double = FALSE, col_names = FALSE,
                       trim_ws = TRUE,n_max =9)

Nome<-c(as.character(Dado_Est[1,2]))
#Codigo<-c(Codigo,as.character(Dado_Est[2,2]))
Latitude<-c(as.numeric(Dado_Est[3,2]))
Longitude<-c(as.numeric(Dado_Est[4,2]))




#Parte 3 - Processamento dos dados de satélite e dados de campo 
#Realizar pre-processamento para avaliar se os dados estão na mesma unidade (mm)


#Filtro para selecionar apenas uma única estação e realizar a métrica e filtro para selecionar os dados que estejam na mesma 
#Janela temporal 
#---------------------Operação para filtrar os dias ---------------------------------------------------

#Date_star<-as.Date(data_precipitation$`Data Medicao`[1])
#Operação para determinar o start_date e end_date

#------------------------------Start_Date ------------------------------
m<-length(data_precipitation_temp1$`Data Medicao`)
n <- 0
j <- 1
while (n < 1) {
  if (is.na(data_precipitation_temp1$`PRECIPITACAO TOTAL, DIARIO(mm)`[j])==FALSE){
    Date_star=as.Date(data_precipitation_temp1$`Data Medicao`[j])
    n=n+1
  }
  j=j+1
}


#------------------------------End_Date ------------------------------
#End Date - Se (O ultimo valor da coluna precipitação for Na - Selecione o penultimo )
#Date_end<-as.Date(data_precipitation$`Data Medicao`[length(data_precipitation$`Data Medicao`)])
#while()#if ()
n <- 0
j <- 0

while (n < 1) {
  if (is.na(data_precipitation_temp1$`PRECIPITACAO TOTAL, DIARIO(mm)`[m-j])==FALSE){
    Date_end=as.Date(data_precipitation_temp1$`Data Medicao`[m-j])
    n=n+1
    #Condição para caso o final da data esteja muito 
    if (month(Date_end)<=10){
      Date_end=as.Date(paste0(year(Date_end)-1, "-12","-31"),"%Y-%m-%d")
    }
  }
  j=j+1
}


#Filtro para os dados de precipitação

#Cpnvertendo a colu
#data_precipitation_temp1$`Data Medicao`<-as.Date(data_precipitation_temp1$`Data Medicao`)
#Filtro dos dados de precipitação 
data_precipitation<-data_precipitation_temp1 %>% 
  filter(`Data Medicao`<=as.Date(Date_end) & `Data Medicao`>=as.Date(Date_star))


#Dados de satélite 

Data_end_sat<-as.Date(paste0(year(Date_end),"-",month(Date_end),"-","01")) #Efetuei esse opereção para evitar erro na conversão nos filtros dos dados mensais (satélíte) e observados em campo 
Date_star_sat<-as.Date(paste0(year(Date_star),"-",month(Date_star),"-","01")) #Efetuei esse opereção para evitar erro na conversão nos filtros dos dados mensais (satélíte) e observados em campo 

#Convertendo a coluna de data em data 
#data_satelite_est$date<-as.Date(data_satelite_est$date)
data_satelite_est<-dados_est %>%
  filter(Nome==Nome[1]) %>% 
  filter(date<=as.Date(Data_end_sat) & date>=as.Date(Date_star_sat))
data_satelite_est$date<-as.Date(data_satelite_est$date)
#filter(data,)


Data_End_obs<-data_satelite_est$date[length(data_satelite_est$date)] #Foi necessário criar essa linha por conta que o satélite TRMM tem apenas até o ano 2019 

data_precipitation<-data_precipitation %>% 
  filter(`Data Medicao`<=as.Date(Data_End_obs) & `Data Medicao`>=as.Date(Date_star))

#--------------------------------------------------------------------------------------

#Parte 4 - Cálculo das métricas 
#Atenção: Colocar Na onde estivar vazio 


#4.1 Valores diários 
#sim_dia=data_satelite_est$precipitation
##obs_dia=data_precipitation$`PRECIPITACAO TOTAL, DIARIO(mm)`
#est_dia <- gof(sim_dia, obs_dia)
#fic.out<-paste0("03_Resultados/Imagens/CHIRSPS/",i,"_",Nome,"_dia.png")
#png(file=fic.out,
#    width=800, height=500)

#Possibiliade de indicadores 
#ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", 
#"rNSE", "d", "md", "rd", "r", "R2", "bR2", "KGE", "VE"
Estatisticas<-c("RMSE","PBIAS","NSE","r", "R2","KGE") #Alterar para indicar alguma específica 

#ggof(sim_dia, obs_dia,gofs=Estatisticas,ylab=c("Rainfall[mm]"),dates = data_satelite_est$date, digits = 5)
#dev.off() #Para salvar os gráficos plotar os gráficos


#-----------------------------------------Valores Mensais-----------------------------
#Construção de um data frame 
#Date=seq(from=as.Date("2000-01-01"),to=as.Date("2020-12-31"), by=1)
Date=as.Date(data_precipitation$`Data Medicao`)


Dados_Obs_Sat<-data.frame(
  #Sat=data_satelite_est$precipitation,
  Obs=data_precipitation$`PRECIPITACAO TOTAL, DIARIO(mm)`,
  Date=as.Date(Date)
)


#Criando coluna de ano, mês e número de dias no mes
Dados_Obs_Sat_month_temp1 <- Dados_Obs_Sat %>%
  mutate(month = month(Date))%>%
  mutate(day_month=days_in_month(Date))%>%
  mutate(year = year(Date))

#1*Criando coluna com arquivos de média mensal de precipitação e numero de dias no mês 
#2 Criando uma nova coluna (Precipitação acumulada mensal) com a multiplicação da média mensal e o número de dias no mês 
#group_by - Selecionar os grupos para realizar as operações

Dados_month_obs<-Dados_Obs_Sat_month_temp1 %>%
  group_by(year,month) %>%
  summarise(days_in_month = mean(day_month,na.rm = TRUE),mes_Obs = mean(Obs,na.rm = TRUE))%>%
  mutate(mes_Obs = mes_Obs*days_in_month)
  

#Alterar ordem das colunas 
#Sem valores preenchidos, apenas somandos os valores em "Nas"
# Dados_year_obs <- Dados_Obs_Sat_month_temp1 %>%
#   group_by(year,month) %>%
#   summarise(mes_Obs = sum(Obs,na.rm = TRUE))

#Dados de satélite - Preenchendo os valores "NA com media mensal"
Dados_month_obs<-Dados_Obs_Sat_month_temp1 %>%
  group_by(year,month) %>%
  summarise(days_in_month = mean(day_month,na.rm = TRUE),mes_Obs = mean(Obs,na.rm = TRUE))%>%
  mutate(mes_Obs = mes_Obs*days_in_month)

#Dados de satélite 
data_satelite_est$date<-as.Date(data_satelite_est$date)
names(data_satelite_est)<-c("Date","Nome","Sat")

#Criando coluna de ano, mês e número de dias no mes
Dados_Sat_month_temp1 <- data_satelite_est %>%
  mutate(month = month(Date))%>%
  mutate(day_month=days_in_month(Date))%>%
  mutate(year = year(Date))

Dados_month_sat <- Dados_Sat_month_temp1 %>%
  group_by(year,month) %>%
  summarise(mes_Sim = sum(Sat,na.rm = TRUE))%>% 
  mutate(Data_mes_ano=as.Date(paste0(year,"-",month,"-1"),"%Y-%m-%d")) #Inserindo o eixo da time series


sim_mes=Dados_month_sat$mes_Sim
obs_mes=Dados_month_obs$mes_Obs
est_Mes <- gof(sim_mes, obs_mes)
fic.out<-paste0("03_Resultados/Imagens/CHIRSPS/",i,"_",Nome,"_mes.png")
#png(file=fic.out,
#    width=800, height=500)
ggof(sim_mes, obs_mes,gofs=Estatisticas,ylab=c("Rainfall[mm]"),dates=Dados_month_sat$Data_mes_ano)
dev.off() #Para salvar os gráficos plotar os gráficos

#Valores anuais 
#Dados observados - sem preencher NAs 
# Dados_year_obs <- Dados_Obs_Sat_month_temp1 %>%
#   group_by(year) %>%
#   summarise(year_Obs = sum(Obs,na.rm = TRUE))

#Valores anuais 
#Dados observados 
Dados_year_obs <- Dados_month_obs %>%
  group_by(year) %>%
  summarise(year_Obs = sum(mes_Obs,na.rm = TRUE))



Dados_year_sat <- Dados_Sat_month_temp1 %>%
  group_by(year) %>%
  summarise(year_Sim = sum(Sat,na.rm = TRUE)) %>%
  mutate(Data_mes_ano=as.Date(paste0(year,"-",01,"-1"),"%Y-%m-%d")) #Inserindo o eixo da time series



#z1=length(Dados_Obs_Sat_month_temp1$Sat)
#sum(Dados_Obs_Sat_month_temp1$Sat[(z1-365):z1])
#sum(Dados_Obs_Sat_month_temp1$Obs[(z1-365):z1])

#Organizando os dados para calcular as estatísticas 
obs_year=Dados_year_obs$year_Obs
sim_year=Dados_year_sat$year_Sim

est_year <- gof(sim_year, obs_year)

fic.out<-paste0("03_Resultados/Imagens/CHIRSPS/",i,"_",Nome,"_year.png")
#png(file=fic.out,
#    width=800, height=500)
ggof(sim_year, obs_year,gofs=Estatisticas,ylab=c("Rainfall[mm]"),dates=Dados_year_sat$Data_mes_ano)




#Armazenar os resultados 
#estMes[1]
#r 
#Bias 
#Rmse 
#KGE

#Retirando as estatísticas do dia. 
Dados_temp<-data.frame(
  Nome=Nome, 
  Latitude=Latitude[1],
  Longitude=Longitude[1],
  Data_inicio=Date_star,
  Data_fim=Date_end,
  #RMSE_dia=est_dia[4],
  RMSE_mes=est_Mes[4],
  RMSE_ano=est_year[4],
  #r_dia=est_dia[16],
  r_mes=est_Mes[16],
  r_ano=est_year[16],
  #pbias_dia=est_dia[6],
  pbias_mes=est_Mes[6],
  pbias_ano=est_year[6],
  #KGE_dia=est_dia[19],
  KGE_mes=est_Mes[19],
  KGE_ano=est_year[19]
)
dev.off() #Para salvar os gráficos plotar os gráficos
Dados_estatisticas=bind_rows(Dados_estatisticas,Dados_temp)

#Data_export<-bind_rows(Dado_Est,dados_estacao)
#}
}

#i=1
#Exportar produto final 
#Alterar caminho quando utilizar outro satélie
cam_sat_out<-paste0("03_Resultados/",name_sat,"_99_.csv")
write_delim(Dados_estatisticas,file=cam_sat_out,delim =",")
#write_delim(Dados_estatisticas,file="03_Resultados/Dados_Estatisticas_CHIRPs.xlsx",delim =",")

