#Autor: João Maria de Andrade 
#Data: 06-09-2021
#Versão:01 
#Objetivo: Script para leitura dos dados do Inmet e extração das coordenadas 


#Bibliotecas Utilizadas 

library(dplyr) #Manipulação de Data frame 
library(readr) #Leitura dos arquivos csv - https://github.com/rstudio/cheatsheets/blob/master/data-import.pdf
library (hydroGOF) #Estatisticas de desempenho 
library(lubridate) #Manipulação de dados mensais, diários, anuais 
library(ggplot2)#Produção de maps e gráficos 
library(viridis) #Escala de Cores 
library(geobr) #Shapes do Brasil, biomas, Estados e municípios 
library(sf) #Operação com shapes  #https://geocompr.robinlovelace.net/geometric-operations.html
library(ggpubr) #Unir varios gráficos - http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
library(rnaturalearth) #Shapes do planeta  
library(rnaturalearthdata) #Shapes do planeta  
library(rgdal) #Manipulação de dados espaciais
library(sp)#Manipulação de dados espaciais
library(tidyr) #Maniputação de data frame 
library(writexl) #Salvando dados excel
library(geobr) #shapes brasileiras



#------------------------------Parte 1 - Leitura dos Dados (Estatísticas dos Satélites)---------------------- 

#Dados_Estatisticas_CHIRPs <- read_csv("03_Resultados/Finais/Dados_Estatisticas_CHIRPs.csv")
CHIRPs <- read_csv("03_Resultados/Finais/Dados_Estatisticas_CHIRPs_2_gapfill_NAs.csv")
ERA<-read_csv("03_Resultados/Finais/ptsEraMonth_2000-01-01_2021-01-01_2_gapfill_NAs.csv")
TRMM<-read_csv("03_Resultados/Finais/ptsTRMM_mm_hour_2000-01-01_2021-01-01_2_gapfill_NAs.csv")
TERRACLIMATE<-read_csv("03_Resultados/Finais/ptsTERRACLIMATE_mm_2000-01-01_2021-01-01_2_gapfill_NAs.csv")

names(CHIRPs)<-names(ERA) #Padronizar o nome das colunas dos Chirps e outros satélites
names(CHIRPs)<-c("Nome","Latitude","Longitude","Data_inicio","Data_fim","RMSE_dia","RMSE_mes","RMSE_ano","r_dia","r_mes",      
                 "r_ano","pbias_dia","pbias_mes","pbias_ano","KGE_dia","KGE_mes","KGE_ano")


#Dados_interesse<-c("RMSE_mes","RMSE_ano","r_mes","r_ano","pbias_mes","pbias_ano","KGE_mes","KGE_ano")
Dados_interesse<-c("RMSE_mes","r_mes","pbias_mes")

#Parte 1.2 - Resumo dos dados 
Sumary_ERA<-summary(select(ERA,Dados_interesse),mean) #Reprocessar os dados 
Sumary_CHIRPs<-summary(select(CHIRPs,Dados_interesse),mean)
Sumary_TRMM<-summary(select(TRMM,Dados_interesse),mean)
Sumary_TERRACLIMATE<-summary(select(TERRACLIMATE,Dados_interesse),mean)

#teste1<-as.data.frame(Sumary_TERRACLIMATE)
#Construção dos valores máximos e mínimos 
Maximos<-NULL
Minimos<-NULL
#Media<-NULL


Date_Base<-list(CHIRPs,TRMM,TERRACLIMATE,ERA)
#Names_Sat<-c("A) CHIRPS","B) TRMM","C) TERRACLIMATE","D) ERA")
Names_Sat<-c("CHIRPS","TRMM","TERRACLIMATE","ERA5-LAND")
for (i in 1:(length(Dados_interesse))) {
  Date_Base_temp2<-select(as.data.frame(Date_Base[1]),Dados_interesse[i])
  for (j in 1:(length(Names_Sat))) {
  #i=1
  #j=1
  Date_Base_temp=select(as.data.frame(Date_Base[[j]]),Dados_interesse[i])
  Date_Base_temp2<-cbind(Date_Base_temp2,Date_Base_temp)
  }
  Max_temp<-max(Date_Base_temp2)
  Maximos<-c(Maximos,Max_temp)
  Min_temp<-min(Date_Base_temp2)
  Minimos<-c(Minimos,Min_temp)
  #Media_temp<-mean(Date_Base_temp[,1])
  #Media<-c(Media,Media_temp)
}
Maximos
Minimos



#-------------------------------Parte 2_Gráficos_Espaciais------------------------------------
# From CRAN
#install.packages("geobr")
#library(geobr)
#library(geobr)
#library(ggplot2)
#library(sf)
library(dplyr)
# Development version
#utils::remove.packages('geobr')
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
#2.1 -----Plotagens Espaciais-----------------------------------------------------------------
#Paleta de Cores https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html 
#geobr
#Leitua dos dados do Brasil 
uf <- read_state(code_state=12, year=2017)
states <- read_state(year=2019)
region<-read_region(year=2019)
region_Nordeste <- subset(region, grepl("2", region$code_region)) #Seleção apenas da região nordeste 
Biomas <- read_biomes(year=2019)
world <- ne_countries(scale = "medium", returnclass = "sf")

#Clip entre a shape dos bioma e a Shape do Nordeste
Nordeste_Bioma = st_intersection(Biomas, region_Nordeste)
plot(Nordeste_Bioma)

#Names_Sat<-c("CHIRPS","TRMM","TERRACLIMATE")
#Names_Sat<-c("CHIRPS","TRMM")
#Date_Base<-list(CHIRPs,TRMM,TERRACLIMATE)
#Date_Base<-list(CHIRPs,TRMM)
Dados_interesse<-c("RMSE_mes","RMSE_ano","r_mes","r_ano","pbias_mes","pbias_ano","KGE_mes","KGE_ano")
Nomes_Col<-c("RMSE","RMSE","r","r","pbias","pbias","KGE","KGE")
Ordem<-c("Dados Mensais","Dados Anuais","Dados Mensais","Dados Anuais","Dados Mensais","Dados Anuais","Dados Mensais","Dados Anuais")

#Processamento apenas dos dados Mensais (RMSE,r,pbias)
Dados_interesse<-c("RMSE_mes","r_mes","pbias_mes")
Nomes_Col<-c("RMSE","r","pbias")
Ordem<-c("Dados Mensais","Dados Mensais","Dados Mensais")

Direction<-c(1,-1,1) #Sentido positivo do gradiente do gráfico 
library(viridis)
for (i in 1:(length(Dados_interesse))) {
  for (j in 1:(length(Names_Sat))) {
    #i=1
    #j=1
    Date_Base2=select(as.data.frame(Date_Base[j]),Longitude,Latitude,Dados_interesse[i])
    names(Date_Base2)=c("Longitude","Latitude","Col_int")
    Media<-c(mean(Date_Base2$Col_int))
    plot_teste<-
    ggplot()+
    geom_sf(data= world,fill="#FFFFFF")+
    geom_sf(data=Nordeste_Bioma, fill="#FFFFFF", color="#696969", size=.1, show.legend = FALSE) +
    geom_point(data=Date_Base2,size = 2,aes(Longitude, Latitude,colour=Col_int)) +
    scale_color_viridis(option = "C",limits=c(Minimos[i],Maximos[i]),direction=Direction[i])+
    ylab("Latitude")+
    coord_sf(xlim = c(-48.5, -34), ylim = c(-19, 0), expand = FALSE)+
    #ggtitle(Names_Sat[j], subtitle = Ordem[i]) +
    ggtitle(Names_Sat[j]) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"),
          axis.text = element_text(size=8))+
    guides(colour=guide_colourbar(title=Nomes_Col[i],order = 1))+
    annotate("text", x =-38, y = -1, label = paste0("Mean: ",round(Media,2),size=2))
    
    fic.out<-paste0("03_Resultados/Imagens3/",Names_Sat[j],"_",Dados_interesse[i],".png")
    png(file=fic.out,
        width=350, height=400)
    plot(plot_teste)
    dev.off() #Para salvar os gráficos plotar os gráficos
}
}

#2.2-----------------------BloxPlot-Por-Bioma-----------------------------------------------

# library
library(ggplot2)

# create a data frame
variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)

# grouped boxplot
ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot()


#Eixo y (r_mensal)
#Eixo x (Bioma (AMZ, CAA, CERRADO, Mata Atlêntica)) #Criar uma coluna especificando
#Tipo de Satélites (CHIRPs,TRMM,TERRACLIMATE,ERA)

#2.2. Filtra as estações por bioma 
#Setar a projeteção no dados espacial criado 
#Constuir um for 
library(sf)


#------------------------------------------Seleção do Bioma----------------------------------------------------------- 
Nordeste_Bioma_AMZ <- subset(Nordeste_Bioma, grepl("1", Nordeste_Bioma$code_biome)) #Leitura do Bioma - Amazonia 
Nordeste_Bioma_CAA <- subset(Nordeste_Bioma, grepl("2", Nordeste_Bioma$code_biome)) #Leitura do Bioma - Caatinga 
Nordeste_Bioma_CER <- subset(Nordeste_Bioma, grepl("3", Nordeste_Bioma$code_biome)) #Leitura do Bioma - Cerrado
Nordeste_Bioma_MA <-  subset(Nordeste_Bioma, grepl("4", Nordeste_Bioma$code_biome)) #Leitura do Bioma - Mata Atlântica

plot(Nordeste_Bioma_AMZ)
plot(Nordeste_Bioma_CAA)
plot(Nordeste_Bioma_CER)
plot(Nordeste_Bioma_MA)



#----------------------------------------Escolha dos Satélite e Produto------------------------------

Date_Base<-list(CHIRPs,TRMM,TERRACLIMATE,ERA)
#Date_Base[[2]]
Names_Sat<-c("A) CHIRPS","B) TRMM","C) TERRACLIMATE","D) ERA5-LAND")

#Satelite<-c("CHIRPS","TRMM","TERRACLIMATE","ERA")

Name_sat=NULL
Name_bioma=NULL
Data_y=NULL
Dados_interesse_2<-c("RMSE_mes","r_mes","pbias_mes","KGE_mes")
name_var<-c("RMSE","r","pbias","KGE")

for (m in 1:length(Dados_interesse_2)) {
i=1
#Interação das variáveis de interesse
Name_sat=NULL
Name_bioma=NULL
Data_y=NULL  


for (i in 1:(length(Date_Base))) {
Teste_CHIRPS=NULL
#i=1
#m=2
Teste_CHIRPS<-select(Date_Base[[i]],Latitude,Longitude,Dados_interesse_2[m])

coordinates(Teste_CHIRPS) <- ~Longitude+Latitude #Conversão em dados Espaciais

#Alterando a projeção 
#proj4string(Teste_CHIRPS) <- CRS("+proj=longlat +datum=WGS84")
#proj4string(Nordeste_Bioma_CAA)

Teste_CHIRPS_reproj<- st_as_sf(Teste_CHIRPS) %>% st_set_crs(4674)
#plot(Teste_CHIRPS_reproj)
Teste_CHIRPS2=Teste_CHIRPS_reproj

#----------------------------------Interseção dos pontos e do Bioma---------------------------------
#RMSE - Exemplo 

Nordeste_Bioma_AMZ_int= st_intersection(Nordeste_Bioma_AMZ, Teste_CHIRPS2)#Amazonia
num_est_AMZ=Nordeste_Bioma_AMZ_int[[6]]                               #Amazonia 


Nordeste_Bioma_CAA_int = st_intersection(Nordeste_Bioma_CAA, Teste_CHIRPS2)#Caatinga 
num_est_CAA=Nordeste_Bioma_CAA_int[[6]]                                    #Caatinga 

Nordeste_Bioma_CER_int= st_intersection(Nordeste_Bioma_CER, Teste_CHIRPS2)#Cerrado
num_est_CER=Nordeste_Bioma_CER_int[[6]]                                   #Cerrado

Nordeste_Bioma_MA_int= st_intersection(Nordeste_Bioma_MA, Teste_CHIRPS2)#Mata Atlântica
num_est_MA=Nordeste_Bioma_MA_int[[6]]                                   #Mata Atlântica

total=length(num_est_AMZ)+length(num_est_CAA)+length(num_est_CER)+length(num_est_MA) #Total de Estações


#length(CHIRPs$Nome

Name_sat_temp<-rep(Satelite[i],each=total)

Name_bioma_temp<-c(rep("Amazônia",each=length(num_est_AMZ)),rep("Caatinga",each=length(num_est_CAA)),
              rep("Cerrado",each=length(num_est_CER)),rep("Mata Atlântica",each=length(num_est_MA)))

Data_y_temp<-c(num_est_AMZ,num_est_CAA,num_est_CER,num_est_MA)


Name_sat=c(Name_sat_temp,Name_sat)
Name_bioma=c(Name_bioma_temp,Name_bioma)
Data_y=c(Data_y_temp,Data_y)

} 


data=data.frame(Name_sat, Name_bioma, Data_y)

#data_temp1<-filter(data,Name_bioma=='Caatinga')

#summary(data_temp1)
        
        
plot_boxplot<-ggplot(data, aes(x=Name_sat, y=Data_y, fill=Name_bioma)) + 
      geom_boxplot()+
      ylab(name_var[m])+
      xlab("Produto")+
      labs(fill = "Bioma")

fic.out<-paste0("03_Resultados/Imagens/Boxplot/","Satelites","_",Dados_interesse_2[m],".png")
png(file=fic.out,
    width=800, height=400)
plot(plot_boxplot)
dev.off() #Para salvar os gráficos plotar os gráficos

}


#2.3-----------------------Time-Serie-----------------------------------------------
#2.3.1. Leitura dos Protudos 
Name_est<-c("TURIACU","CAXIAS","SERIDO (CAICO)","RECIFE (CURADO)")
Name_Biomas<-c("Amazônia","Cerrado","Caatinga","Mata Atlântica")

#AMZ - TURIACU
#Cerrado - CAXIAS 
#Caatinga - SERIDO (CAICO) 
#Mata Atlântica - RECIFE
library(readxl)

#Arquivos para plotagem dos dados de Serie temporal 
files_Inmet = list.files("C:\\Users\\jonet\\Documents\\Doutorado-UFPE\\Periodo 2021_2\\SIG\\Teste das Estatisticas\\CHIRPS\\"
                         , pattern='*.xlsx',full.names=TRUE)

for (i in 1:(length(Name_est))) {
#i=1

AMZ<- read_excel(files_Inmet[i])
names(AMZ)=c("year","Data_mes_ano","mes_obs","mes_CHIRPS")

#length()
Data_end_sat<-as.Date(AMZ$Data_mes_ano)[length(AMZ$Data_mes_ano)]
Date_star_sat<-as.Date(AMZ$Data_mes_ano)[1]

#2.3.2. Processamento dos outros Satélites 

#-----------Terraclimate - mm/mês 
Sat_Terraclimete<-read_csv(paste0("02_Dados de Satelite/","ptsTERRACLIMATE_mm_2000-01-01_2021-01-01",".csv")) 
Sat_Terraclimete_filter<-Sat_Terraclimete %>%
  filter(Nome==Nome[1]) %>% 
  filter(date<=as.Date(Data_end_sat) & date>=as.Date(Date_star_sat))
Sat_Terraclimete_filter$precipitation
AMZ$TERRACLIMATE<-Sat_Terraclimete_filter$precipitation

#-----------ERA - Media mensal mm
Sat_ERA<-read_csv(paste0("02_Dados de Satelite/","ptsEraMonth_2000-01-01_2021-01-01",".csv"))

#Conversão para precipitação acumulada mensal 
Sat_ERA_temp1<-Sat_ERA

Sat_ERA <- Sat_ERA_temp1 %>%
  mutate(precipitation = precipitation*days_in_month(date)*1000)


Sat_ERA_filter<-Sat_ERA %>%
  filter(Nome==Nome[1]) %>% 
  filter(date<=as.Date(Data_end_sat) & date>=as.Date(Date_star_sat))
#Sat_ERA_filter$precipitation

AMZ$ERA<-Sat_ERA_filter$precipitation

#---------TRMM - 

Sat_TRMM<-read_csv(paste0("02_Dados de Satelite/","ptsTRMM_mm_hour_2000-01-01_2021-01-01",".csv"))

#Conversão para precipitação acumulada mensal 
Sat_TRMM_temp1<-Sat_TRMM

Sat_TRMM <- Sat_TRMM_temp1 %>%
  mutate(precipitation = precipitation*days_in_month(date)*24)

Sat_TRMM_filter<-Sat_TRMM %>%
  filter(Nome==Nome[1]) %>% 
  filter(date<=as.Date(Data_end_sat) & date>=as.Date(Date_star_sat))
Sat_TRMM_filter$precipitation


Dif=-length(Sat_TRMM_filter$precipitation)+length(AMZ$Data_mes_ano)
Na_value<-rep(NA,Dif)

Sat_TRMM_filter_Gap_NA<-c(Sat_TRMM_filter$precipitation,Na_value)

AMZ$TRMM<-Sat_TRMM_filter_Gap_NA


#----------------------Gráfico Serie temporal ------------

AMZ$Data_mes_ano<-as.Date(AMZ$Data_mes_ano)

names(AMZ)<-c("year","Date","01_Obs","02_CHIRPS","03_TERRACLIMATE","04_ERA","05_TRMM")

#Exportar csv e excel 

fic.out2<-paste0("Teste das Estatisticas/Todos os Satelites/",i,"_",Name_Biomas[i],"_",Name_est[i],".xlsx")
write_xlsx(AMZ,fic.out2)

#Filtro para produção por satélite 

df <-AMZ %>%
  select(Date,"01_Obs","02_CHIRPS","03_TERRACLIMATE","04_ERA","05_TRMM") %>%
  gather(key = "Dados", value = "Precipitation", -Date)
head(df, 3)
df$Date<-as.Date(df$Date)


cores<-c("#000000","#FFA500","#00FFFF","#00FF00","#FF00FF")
labels<-c("Inmet","CHIRPS","TERRACLIMATE","ERA","TRMM")

plot_ts<-
  ggplot(data=df, aes(x=Date, y=Precipitation)) +
  geom_line(aes(color = Dados), size = 0.6) +
  #geom_point(aes(color = Classes), size = 0.5)+
  xlab("Date")+
  ylab("Precipitação [mm]")+
  scale_color_manual(values = cores,
                     labels=labels)+
  scale_x_date(date_breaks = "2 years", date_labels = "%y") +
  ggtitle(Name_Biomas[i], subtitle = Name_est[i])
  #theme_minimal()

fic.out<-paste0("03_Resultados/Imagens/Time_series/","Satelites","_",Name_Biomas[i],"_",Name_est[i],".png")
png(file=fic.out,
    width=800, height=300)
plot(plot_ts)
dev.off() #Para salvar os gráficos plotar os gráficos

#Apenas Chirps
df_2 <-AMZ %>%
  select(Date,"01_Obs","02_CHIRPS") %>%
  gather(key = "Dados", value = "Precipitation", -Date)
head(df_2, 3)
df_2$Date<-as.Date(df_2$Date)


cores<-c("#000000","#FFA500")
labels<-c("Inmet","CHIRPS")

plot_ts_chirps<-
  ggplot(data=df_2, aes(x=Date, y=Precipitation)) +
  geom_line(aes(color = Dados), size = 0.6) +
  #geom_point(aes(color = Classes), size = 0.5)+
  xlab("Date")+
  ylab("Precipitação [mm]")+
  scale_color_manual(values = cores,
                     labels=labels)+
  scale_x_date(date_breaks = "2 years", date_labels = "%y") +
  ggtitle(Name_Biomas[i], subtitle = Name_est[i])
#theme_minimal()

fic.out<-paste0("03_Resultados/Imagens/Time_series/","Chirps","_",Name_Biomas[i],"_",Name_est[i],".png")
png(file=fic.out,
    width=800, height=300)
plot(plot_ts_chirps)
dev.off() #Para salvar os gráficos plotar os gráficos


}

#OBS - Black - #000000
#Chirps - Laranja - #FFA500
#ERA - Azul - #00FFFF
#TerraClimate - Verde Claro - #00FF00 
#TRMM - Magenta - #FF00FF



P4<-ggplot(df, aes(x=Date, y=IVs)) + 
  #geom_line(aes(color = Classes), size = 1.2) +
  geom_line(aes(color = Classes), size = 1.2) +
  geom_point(aes(color = Classes), size = 0.5,colour="black")+
  #scale_fill_manual(values = c("#050505","#1488ff", "#167700","#faffc1","#98ff00","#ff3ea5","#ffa94a","#a8ffb8","#00ffff"),
  #                  labels=c("SE","CA","FLO","PAST","CAN","MIL","CIT","EUC","SOJ"))+
  #subs(faffc1 - ecf582)
  #scale_color_manual(values = c("#050505","#1488ff", "#167700","#ecf582","#98ff00","#ff3ea5","#ffa94a","#a8ffb8","#00ffff"),
  #labels=c("1.SE","2.CA","3.FLO","4.PAST","5.CAN","6.MIL","7.CIT","8.EUC","SOJ"))+
  scale_color_manual(values = c("#ff3ea5","#FF0000"),
                     labels=c("MIL","SOJ"))+
  #ylim(-0.25,1)+
  #theme(axis.text.x = element_text(face = "bold",size=6,angle=45))+
  #scale_fill_discrete(labels=c("SE","CA","FLO","PAST","cAN","MIL","CIT","EUC","SOJ"))+
  #facet_wrap(~Classes, scale="free")
  #facet_wrap(~Classes,labeller=label_parsed)+
  scale_x_date(date_breaks = "1 months", date_labels = "%b %y") +
  labs(y=Names[m])+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))



ggplot(AMZ, aes(x=Data_mes_ano, y=mes_obs )) +
  geom_line( color="steelblue") + 
  #geom_point() +
  xlab("") +
  #theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11"))) +
  ylim(0,1.5)





#Dados de satélite 
#name_sat<-c("ptsTRMM_mm_hour_2000-01-01_2021-01-01")
name_sat<-c("ptsTERRACLIMATE_mm_2000-01-01_2021-01-01")
#name_sat<-c("ptsEraMonth_2000-01-01_2021-01-01")

cam_sat<-paste0("02_Dados de Satelite/",name_sat,".csv") #Outros Satélites
dados_satelite <-read_csv(cam_sat) #Outos satélites 


data_satelite_est<-dados_satelite %>%
  filter(Nome==Nome[1]) %>% 
  filter(date<=as.Date(Data_end_sat) & date>=as.Date(Date_star_sat))
data_satelite_est$precipitation
AMZ$TERRACLIMATE<-data_satelite_est$precipitation
AMZ


