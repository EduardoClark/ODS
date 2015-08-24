######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

#Carga la hoja mas reciente con las ligas de los indicadores
source("src/KeyGD.R")
Sheets <- gs_ls()
gs_key(Key) %>% gs_download(ws="Sheet1",to = "ODS.csv",overwrite = TRUE)
ODS <- read.csv("ODS.csv",stringsAsFactors = FALSE)
ODS$Clave <- paste("i",1:nrow(ODS),sep="")
ODS <- ODS[ODS$URL.indicador!="",]
remove(Sheets,Key)
