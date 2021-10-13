#Autor: João Maria de Andrade 
#Data: 06-09-2021
#Versão:01 
#Objetivo: Script para leitura dos dados do Inmet e extração das coordenadas 


#Bibliotecas Utilizadas 

library(dplyr) #Manipulação de Data frame 
library(readr) #Leitura dos arquivos csv - https://github.com/rstudio/cheatsheets/blob/master/data-import.pdf
library (hydroGOF) #Estatisticas de desempenho 
library(lubridate) #Manipulação de dados mensais, diários, anuais 
library(ggplot2)#Produção de maps 
library(viridis) #Escala de Cores 
library(geobr) #Shapes do Brasil, biomas, Estados e municípios 
library(sf) #Operação com shapes  #https://geocompr.robinlovelace.net/geometric-operations.html
library(ggpubr) #Unir varios gráficos - http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
library(rnaturalearth) #Shapes do planeta  
library(rnaturalearthdata) #Shapes do planeta  
library(rgdal) #Manipulação de dados espaciais
library(sp)#Manipulação de dados espaciais



#------------------------------Parte 1 - Leitura dos Dados (Estatísticas dos Satélites)---------------------- 

#Dados_Estatisticas_CHIRPs <- read_csv("03_Resultados/Finais/Dados_Estatisticas_CHIRPs.csv")
CHIRPs <- read_csv("03_Resultados/Finais/Dados_Estatisticas_CHIRPs_2_gapfill_NAs.csv")
ERA<-read_csv("03_Resultados/Finais/ptsEraMonth_2000-01-01_2021-01-01_2_gapfill_NAs.csv")
TRMM<-read_csv("03_Resultados/Finais/ptsTRMM_mm_hour_2000-01-01_2021-01-01_2_gapfill_NAs.csv")
TERRACLIMATE<-read_csv("03_Resultados/Finais/ptsTERRACLIMATE_mm_2000-01-01_2021-01-01_2_gapfill_NAs.csv")

names(CHIRPs)<-names(ERA) #Padronizar o nome das colunas dos Chirps e outros satélites
names(CHIRPs)<-c("Nome","Latitude","Longitude","Data_inicio","Data_fim","RMSE_dia","RMSE_mes","RMSE_ano","r_dia","r_mes",      
                 "r_ano","pbias_dia","pbias_mes","pbias_ano","KGE_dia","KGE_mes","KGE_ano")


Dados_interesse<-c("RMSE_mes","RMSE_ano","r_mes","r_ano","pbias_mes","pbias_ano","KGE_mes","KGE_ano")

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
Names_Sat<-c("A) CHIRPS","B) TRMM","C) TERRACLIMATE","D) ERA")
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

#2.1 -----Plotagens Espaciais-----------------------------------------------------------------
#Paleta de Cores https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html 

#Leitua dos dados do Brasil 
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

for (i in 1:(length(Dados_interesse))) {
  for (j in 1:(length(Names_Sat))) {
    Date_Base2=select(as.data.frame(Date_Base[j]),Longitude,Latitude,Dados_interesse[i])
    names(Date_Base2)=c("Longitude","Latitude","Col_int")
    Media<-c(mean(Date_Base2$Col_int))
    plot_teste<-
    ggplot()+
    geom_sf(data= world,fill="#FFFFFF")+
    geom_sf(data=Nordeste_Bioma, fill="#FFFFFF", color="#696969", size=.1, show.legend = FALSE) +
    geom_point(data=Date_Base2,size = 2,aes(Longitude, Latitude,colour=Col_int)) +
    scale_color_viridis(option = "C",limits=c(Minimos[i],Maximos[i]))+
    ylab("Latitude")+
    coord_sf(xlim = c(-48.5, -34), ylim = c(-19, 0), expand = FALSE)+
    ggtitle(Names_Sat[j], subtitle = Ordem[i]) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"),
          axis.text = element_text(size=8))+
    guides(colour=guide_colourbar(title=Nomes_Col[i],order = 1))+
    annotate("text", x =-38, y = -1, label = paste0("Média: ",round(Media,2)))
    
    fic.out<-paste0("03_Resultados/Imagens2/",Names_Sat[j],"_",Dados_interesse[i],".png")
    png(file=fic.out,
        width=400, height=450)
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
Names_Sat<-c("A) CHIRPS","B) TRMM","C) TERRACLIMATE","D) ERA")

#Satelite<-c("CHIRPS","TRMM","TERRACLIMATE","ERA")

Name_sat=NULL
Name_bioma=NULL
Data_y=NULL
Dados_interesse_2<-c("RMSE_mes","r_mes","pbias_mes","KGE_mes")
name_var<-c("RMSE","r","pbias","KGE")

for (m in 1:length(Dados_interesse_2)) {

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




