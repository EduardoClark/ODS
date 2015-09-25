######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

##Genera Metadatos Adicionales
require(readxl)
Files <- list.files("Metadatos/Metadatos XLSX/",pattern = "xlsx",full.names = TRUE)
for(i in 1:68){
  assign(paste("T",i,sep=""),read_excel(Files[i])[1:5,])
}
remove(Files,i)
MD <- as.data.frame(t(T1[,-1]));MD$V6 <- row.names(MD)
remove(T1)
Files <- ls(pattern = "T")
for(i in 2:68){
  print(i)
  TMP <- get(Files[i])
  TMP <- as.data.frame(t(TMP[,-1]));TMP$V6 <- row.names(TMP)
  MD <- rbind(MD,TMP)
  remove(TMP)
}
remove(list=ls(pattern = "T"))
MD <- MD[is.na(MD$V1)==FALSE,]
names(MD) <- c("")


TMP <- read_excel(Files[1],col_names = c("V1","V2"),col_types =c("text","text")) 
TMP <- as.data.frame(t(TMP[,-1]))
names(MD)<-c("Descripcion_del_indicador","Dependencia2","DesagregagionGeo2","DesagregacionTemporal2","RangoTiempo","Nombre_del_indicador")

##Mergea Con tabla Metadatos
Metadatos <- read.csv("data-out/metadata.csv",stringsAsFactors = FALSE)
Metadatos <- left_join(Metadatos,MD)
write.csv(Metadatos,"data-out/metadata.csv",row.names = FALSE)
remove(list=ls())
  