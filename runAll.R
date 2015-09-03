######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

##Al correr estos archivos se generan los archivos en versión consumible para portal de monitoreo
source("src/loadLibraries.R")
source("src/KeyGD.R")
source("src/loadIndicadorSheet.R")
source("src/Clean.R")
source("src/LimpiaCodigosINEGI.R")
# source("src/DownloadMunicipalities.R")#Adds an extra indicador to simulate a municipal level indicator before we had any
source("src/ExportaRecursosExtra.R")
