
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(reshape2)
library(ggplot2)
library(readr)
library(ggrepel)
library(rmarkdown)
library(png)
library(magick)
library(knitr)
library(readxl)
library(tidyverse)
library(DT)
library(kableExtra)
library(lubridate)
library(sp)
library(cowplot)
library(ncdf4)
library(metR)


# library(plyr)
# library(tidyverse)

data<-read.csv("input/capturas_besugo.csv",  header = TRUE, sep = ";", dec = ".") #Cargo los datos
#names(data)
colnames(data)[10:79] <- gsub("X","T", colnames(data)[10:79]) #Cambio el X por T_ de tallas.
names(data)[1]<-"Barco.Marea.Lance"  #Cambio el nombre de la columna Barco.Marea.Lance para que cuadre con anzuelos
#names(data)

#Existen especies capturadas y descartadas. Según me indican, para este informe debo unificar ambas variables en una 'Especie'.
#Voy a sumar las especies descartadas a las capturadas, cuando aparecen las mismas especies en el mismo Barco.Marea.Lance
#Para hacer esto creo una columna concatenando las columnas Barco.Marea.Lance Especie.capturada.Especie.descartada
#Es códgo es más largo porque cuando hay especies capturadas no hay descartadas, por lo tanto hay huecos.
#y es necesario hace este código más extenso para soluciarno.
data1<-data %>% 
  mutate(across(everything(), na_if,"")) %>% 
  unite(Barco_Lance_ID_sp, Barco.Marea.Lance,Especie.capturada,Especie.descartada, na.rm = TRUE, remove = FALSE)

data1[is.na(data1)]=""

data1<-data1 %>% 
  relocate(Barco_Lance_ID_sp, .after = Especie.descartada)
#names(data1)
#data1$Barco_Lance_ID_sp

#ok


#Relleno los vacíos con 0.
#No sé hacerlo directamente, así que separto el dataframe y lo hago solo en las columnas donde necesito
#names(data1)
data0<-data1[11:80]  #subset de los datos donde quiero reemplazar los huecos por 0
data0[data0==""]<-0 #convierto los huecos en 0
data2<-cbind(data1[4], data0) #uno esta parte del dataframe con la original
#names(data2)
#ok


#Sumo las filas que contienen el mismo codx por individiuos dentro de talla
#names(data2)
#str(data2) #Las tallas con caracter.
data2[, c(2:71)]<-sapply(data2[, c(2:71)], as.numeric) #Las paso a numéricas
data3<-data2 %>% group_by(Barco_Lance_ID_sp) %>% summarise_all(sum)
#ok

data3$Barco_Lance_ID_sp2<-data3$Barco_Lance_ID_sp #Duplico la columna para desglosar el Barco.Marea.Lance y poder luego calcular por barco
data3<-data3 %>% separate(Barco_Lance_ID_sp2, c('Barco', 'Marea', 'Lance', 'Especie')) #La separo
data3$Lance_ID<-paste(data3$Marea, "_", data3$Lance, sep = "") #Creo tambien el Lance_ID, que es realmente Marea_Lance
#head(data3)
#names(data3)

#Cambio mi matrix a formato largo con melt
#names(data3)
data4<-melt((data3), id.vars=c("Barco_Lance_ID_sp", "Barco","Marea","Lance","Especie","Lance_ID"))
#names(data4)
names(data4)[7]<-"Talla"  
names(data4)[8]<-"Numero" 
#names(data4)
#str(data4)
#head(data4)
data4$Talla<-gsub("T","",as.character(data4$Talla))
#head(data4)
#str(data4)
data4$Talla<-as.numeric(data4$Talla)

#Ahora creo una matriz expandida con todas las combinaciones de Barco_Lance_Id_Especie_Talla
#Creo un vector con el código único de Barco_LanceIDe y con todas las Especies
#names(data4)
data4$Barco_Lance_ID<-paste(data4$Barco,data4$Lance_ID, sep = "_")
data4$commonname<-paste(data4$Barco_Lance_ID_sp,data4$Talla, sep="_")
#head(data4)

Barco_LanceID<-unique(data4$Barco_Lance_ID)
Especies<-unique(data4$Especie)
Talla<-unique(data4$Talla)

#Expando la bbdd para que tenga todas las combinaciones de Barco_LanceID, Especie y Talla
data_expand<-expand.grid(Barco_LanceID,Especies,Talla)
names(data_expand)<-c("Barco_LanceID", "Especies","Talla")
#head(data_expand)
#str(data_expand)
data_expand$commonname<-paste(data_expand$Barco_LanceID,data_expand$Especies,data_expand$Talla, sep="_")
#names(data_expand)
#names(data4)

commonname <- intersect(names(data_expand), names(data4))
data5<-merge(data_expand, data4, by=commonname, all.x=T)
#str(data5)
#head(data5)
#names(data5)
data6<-data5[,c(1:4,11)]
#head(data6)
data6[is.na(data6)]=0

#Cargo los anzuelos
anzuelos<-read.csv("input/anzuelos_palangre_lance.csv",  header = TRUE, sep = ";", dec = ".")
#names(anzuelos)
anzuelos<-anzuelos[, c(1,4)]
#names(anzuelos)
#head(anzuelos)
#head(data6)
names(anzuelos)[1]<-"Barco_LanceID"
data7<-inner_join(data6,anzuelos, by="Barco_LanceID")
#names(data7)
#head(data7)

#Lo veo

datos<-data7
#names(datos)
datos$Barco<-substr(datos$commonname, 1, 2)
datos$Marea<-substr(datos$commonname, 4,5)
datos$Lance<-substr(datos$commonname, 7,8)
datos$Barco[datos$Barco=='OC']<- 'Os Castros'
datos$Barco[datos$Barco=='SS']<- 'Siempre San Pablo'
datos$Marea_Lance<-substr(datos$commonname, 4,8)
datos$CPUE<-datos$Numero/datos$Nº.anzuelos
datos$CPUEx1000<-datos$Numero/datos$Nº.anzuelos*(1000)
#head(datos)
datos$Marea<-as.numeric(datos$Marea)
datos$Lance<-as.numeric(datos$Lance)
#str(datos)
datos<-datos%>%mutate(Barco_Marea=paste(Barco,Marea,sep = "_"))


#Diagrama de tartas por barco y especies
datos<-datos%>%mutate(Barco_Marea=paste(Barco,Marea,sep = "_"))


########################
##### Besugo ###########
########################

# En primer lugar producir y guardar el cpue en número por talla y lance para el besugo. Se guardan archivos diferentes para cada barco y marea.
contbarcos=unique(datos$Barco)
for(i in 1:length(contbarcos))
{
  datsel=datos[datos$Barco==contbarcos[i],]
  contmarea=unique(datsel$Marea)
  for(e in 1:length(contmarea))
  {
    all_lengths=data.frame(Talla=c(1:80))
    
    cpue_length<-datsel[datsel$Numero!=0,] %>%
      filter(Especies=="SBR") %>%
      filter(Barco==contbarcos[i]) %>%
      filter(Marea==contmarea[e]) %>%
      select(., c("Talla", "Lance", "CPUEx1000")) %>%
      mutate(CPUEx1000=round(CPUEx1000, digits=2)) %>%
      dcast(., Talla ~ Lance, mean, margins = c("Talla", "Lance")) %>%
      rename_with(., .fn = ~paste0("Lance_",.), .cols = c(-1) ) %>%
      mutate(Talla=as.numeric(as.character(Talla))) %>%
      merge(., all_lengths, by="Talla", all.y=T) %>%
      replace(is.na(.), 0)
    
    write.csv(cpue_length, paste0("output/tablas/besugo_cpue_numero_talla_lance_", contbarcos[i], "_marea_", contmarea[e], ".csv"))
  }
}


# Ahora, CPUE promedio por barco y talla, todos los lances juntos.
all_lengths=data.frame(expand.grid(c(1:80), unique(datos$Barco)))
colnames(all_lengths)=c("Talla", "Barco")

mean_cpue_length<-datos[datos$Numero!=0,] %>%
      filter(Especies=="SBR") %>%
      select(., c("Talla", "Barco", "CPUEx1000")) %>%
      mutate(CPUEx1000=round(CPUEx1000, digits=2)) %>%
      mutate(Talla=as.numeric(as.character(Talla))) %>%
      merge(., all_lengths, by=c("Talla", "Barco"), all.y=T) %>%
      replace(is.na(.), 0) %>%
      dcast(., Talla ~ Barco, mean, margins = c("Talla", "Barco"))

write.csv(mean_cpue_length, paste0("output/tablas/besugo_cpue_numero_talla_barco.csv"))


################################################
##### Todas las especies juntas      ###########
################################################

# En primer lugar producir y guardar el cpue en número por talla y lance para el besugo. Se guardan archivos diferentes para cada barco y marea.
contbarcos=unique(datos$Barco)
for(i in 1:length(contbarcos))
{
  datsel=datos[datos$Barco==contbarcos[i],]
  contmarea=unique(datsel$Marea)
  for(e in 1:length(contmarea))
  {
    all_lengths=data.frame(Talla=c(1:80))
    
    cpue_length<-datsel[datsel$Numero!=0,] %>%
      filter(Barco==contbarcos[i]) %>%
      filter(Marea==contmarea[e]) %>%
      select(., c("Talla", "Lance", "CPUEx1000")) %>%
      mutate(CPUEx1000=round(CPUEx1000, digits=2)) %>%
      dcast(., Talla ~ Lance, sum, margins = c("Lance")) %>%
      rename_with(., .fn = ~paste0("Lance_",.), .cols = c(-1) ) %>%
      mutate(Talla=as.numeric(as.character(Talla))) %>%
      merge(., all_lengths, by="Talla", all.y=T) %>%
      replace(is.na(.), 0)
    
    write.csv(cpue_length, paste0("output/tablas/todas_especies_cpue_numero_talla_lance_", contbarcos[i], "_marea_", contmarea[e], ".csv"))
  }
}


# Ahora, CPUE promedio por barco y talla, todos los lances juntos.
all_lengths=data.frame(expand.grid(c(1:80), unique(datos$Barco_LanceID), unique(datos$Especies)))
colnames(all_lengths)=c("Talla", "Barco_LanceID", "Especies")

library(plyr)

mean_cpue_length<-datos[datos$Numero!=0,] %>%
  merge(., all_lengths, by=c("Talla", "Barco_LanceID", "Especies"), all.y=T) %>%
  replace(is.na(.), 0) %>%
  ddply(., .(Talla, Barco, Especies), summarize, CPUEx1000=mean(CPUEx1000)) %>%
  mutate(CPUEx1000=round(CPUEx1000, digits=2)) %>%
  mutate(Talla=as.numeric(as.character(Talla))) %>%
  dcast(., Talla ~ Barco, sum, margins = c("Talla", "Barco"))

write.csv(mean_cpue_length, paste0("output/tablas/todas_especies_cpue_numero_talla_barco.csv"))



##########################################################################
##### Valor CPUE por especies, todos los lances juntos         ###########
##########################################################################

# Ahora, CPUE promedio por barco y talla, todos los lances juntos.
tabla_sp <- as.data.frame(read_excel("input/Besugo_diciembre_2021_VF5.xlsx", sheet = "Especies")) %>%
  select("Código FAO", "Nombre común") %>%
  rename(., c("Código FAO"="Especies", "Nombre común"="sp_comun"))

data=merge(datos,tabla_sp, by="Especies")


all_lengths=data.frame(expand.grid(c(1:80), unique(data$Barco_LanceID), unique(data$sp_comun)))
colnames(all_lengths)=c("Talla", "Barco_LanceID", "sp_comun")

contbarcos=unique(datos$Barco)
for(i in 1:length(contbarcos))
{
mean_cpue_length<-datos[datos$Numero!=0,] %>%
  filter(Barco==contbarcos[i]) %>%
  merge(.,tabla_sp, by="Especies") %>%
  merge(., all_lengths, by=c("Talla", "Barco_LanceID", "sp_comun"), all.y=T) %>%
  replace(is.na(.), 0) %>%
  ddply(., .(Talla, sp_comun), summarize, CPUEx1000=mean(CPUEx1000)) %>%
  mutate(CPUEx1000=round(CPUEx1000, digits=2)) %>%
  dcast(., Talla ~ sp_comun, sum, margins = c("Talla", "sp_comun"))

write.csv(mean_cpue_length, paste0("output/tablas/cpue_numero_talla_especie_", contbarcos[i], "_.csv"))

}


















