######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

##Genera Metadatos Adicionales
Files <- list.files("Metadatos/Metadatos XLSX/",pattern = "xlsx",full.names = TRUE)
for(i in 1:125){
  assign(paste("T",i,sep=""),read_excel(Files[i])[1:5,])
}
remove(Files,i)
MD <- as.data.frame(t(T1[,-1]));MD$V6 <- row.names(MD)
remove(T1)
Files <- ls(pattern = "T")
for(i in 2:125){
  print(i)
  TMP <- get(Files[i])
  TMP <- as.data.frame(t(TMP[,-1]));TMP$V6 <- row.names(TMP)
  MD <- rbind(MD,TMP)
  remove(TMP)
}
remove(list=ls(pattern = "T"))
remove(Files,i)
MD <- MD[is.na(MD$V1)==FALSE,]
names(MD)<-c("Descripcion_del_indicador","Dependencia","DesagregagionGeo2","DesagregacionTemporal2","RangoTiempo","Nombre_del_indicador")
MD2 <- MD[,c(2,6)]
MD2 <- MD2 %>% arrange(as.character(Dependencia))
MD2$Clave <- c(7,NA,6,12,13,
               14,15,16,9,NA,
               NA,51,45,46,47,
               NA,NA,49,50,18,
               19,17,136,137,145,
               147,149,137,138,139,
               141,142,166,144,146,
               148,150,151,152,153,
               154,155,156,NA,157,
               158,159,160,161,162,
               163,164,NA,165,166,
               135,NA,NA,90,94,
               99,105,106,107,NA,
               NA,100,NA,101,102,
               103,104,NA,NA,NA,
               NA,NA,111,112,114,
               116,NA,120,113,117,
               118,119,121,63,61,
               64,NA,129,75,70,
               72,73,74,76,77,
               78,79,83,84,86,
               88,89,NA,NA,NA,
               68,60,62,129,130,
               131,132,133,NA,96,
               NA,110,95,97,98)
MD2$Clave<-paste("i",MD2$Clave,sep="")
MD2 <- MD2[,c(2,3)]
MD <- left_join(MD,MD2)
MD <- MD[,c(7,1,3,4,5)]
remove(MD2)

##Mergea Con tabla Metadatos
Metadatos <- ODS
Metadatos <- left_join(Metadatos,MD)
Metadatos[82:119,5] <- as.character(Metadatos[82:119,24])
Metadatos[82:119,4] <- as.character(Metadatos[82:119,25])
Metadatos <- unique(Metadatos)
Metadatos$Dup <- 0
for(i in  2:nrow(Metadatos)){
  Metadatos[i,27] <- ifelse(Metadatos[i-1,22]==Metadatos[i,22],1,0)
}
remove(i)
Metadatos <- Metadatos[Metadatos$Dup!=1,]


  