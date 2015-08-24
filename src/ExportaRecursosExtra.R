######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

#Exporta los recursos adicionales

#Write subgroup descriptions
Keys <- unique(rbind(KeyI127,KeyI131,KeyI132,KeyI133,KeyI50,KeyI88,KeyI90))
remove(KeyI127,KeyI131,KeyI132,KeyI133,KeyI50,KeyI88,KeyI90)
Keys$id3 <- gsub("X..","",Keys$id3)
write.csv(Keys,"data-out/codigosgrupos.csv",row.names = FALSE)
remove(Keys)

#Write date ranges
write.csv(EstatalMeta,"data-out/rangostemporales.csv",row.names = FALSE)
remove(EstatalMeta)

#Desagregacion Geografica
DesGeo <- unique(Estatal[,c(1,7)])
write.csv(DesGeo,"data-out/desagregaciongeografica.csv",row.names = FALSE)
remove(DesGeo)

#MetaData General 
ODS <- ODS[,c(12,1:3,7:11)]
names(ODS) <- gsub("\\.","_",names(ODS))
names(ODS)[6] <- "Desagregacion"
names(ODS)[9] <- "Información_externa_BD"
write.csv(ODS,"data-out/metadata.csv",row.names = FALSE)
remove(ODS,Estatal)