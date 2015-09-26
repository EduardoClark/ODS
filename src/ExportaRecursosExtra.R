######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

#Exporta los recursos adicionales

#Write subgroup descriptions
Keys <- unique(rbind_all(lapply(X = ls(pattern="KeyI"),FUN = get)))
remove(list=ls(pattern="KeyI"))
Keys$id3 <- gsub("X..","",Keys$id3)
write.csv(Keys,"data-out/codigosgrupos.csv",row.names = FALSE)
remove(Keys)

#Write date ranges
EstatalMeta$ranget <- as.numeric(EstatalMeta$ranget)
write.csv(EstatalMeta,"data-out/rangostemporales.csv",row.names = FALSE)
remove(EstatalMeta)

#Desagregacion Geografica
DesGeo <- unique(Estatal[,c(1,7)])
write.csv(DesGeo,"data-out/desagregaciongeografica.csv",row.names = FALSE)
remove(DesGeo)


#MetaData General 
names(ODS) <- gsub("\\.","_",names(ODS))
names(ODS) <- gsub("ó","o",names(ODS))
names(ODS) <- gsub("á","a",names(ODS))
names(ODS)[1] <- "Nombre_del_objetivo"
ODS$X_1 <-NULL
ODS <- ODS[1:108,]
ODS <- ODS[ODS$Clave!="i176",]
ODS <- ODS[ODS$Clave!="i127",]
ODS <- ODS[ODS$Clave!="i92",]
ODS <- ODS[ODS$Clave!="i91",]
ODS <- ODS[ODS$Clave!="i175",]
ODS <- ODS[ODS$Clave!="i172",]
#MetaData Especifica
remove(Missing,Prueba,TMP,TMP2,i)
source("src/MetadatosExtra.R")
Metadatos$Dup<-NULL
Metadatos$Nombre_del_indicador <- gsub("Diputadas","Porcentaje de curules en la Cámara de Diputados ocupadas por mujeres",Metadatos$Nombre_del_indicador)
Metadatos$Nombre_del_indicador <- gsub("Senadoras","Porcentaje de curules en la Cámara de Senadores ocupadas por mujeres",Metadatos$Nombre_del_indicador)
Metadatos$Nombre_del_indicador <- gsub("Agua entubada en el ambito de la vivienda","Agua entubada en el ámbito de la vivienda",Metadatos$Nombre_del_indicador)
Metadatos$Nombre_del_indicador[43] <- "Residuos sólidos urbanos manejados adecuadamente"
Metadatos$Nombre_del_indicador[44] <- "Accesibilidad al Espacio Público Abierto"
write.csv(Metadatos,"data-out/metadata.csv",row.names = FALSE)

#Clear
remove(list=ls())


