######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

##Crea indicador falso a nivel municipal
dir.create("tmp")
download.file("http://mapserver.inegi.org.mx/MGN/mgm2014v6_2.zip",destfile = "tmp/muns.zip")
unzip("tmp/muns.zip",exdir = "tmp")
Muns <- read.dbf("tmp/mgm2013v6_2.dbf",as.is = TRUE)[,4]
unlink("tmp",recursive = TRUE,force = TRUE)

#################### Crea 2 indicadores falsos
Ind <- unique(Estatal[Estatal$id=="i7",c(1,3,5,6)]) %>% mutate(id="i1000",DesGeo="M") 
Ind <- expand.grid(unique(Ind$id),unique(Muns),
                   unique(Ind$t),1,unique(Ind$m),
                   unique(Ind$id2),unique(Ind$DesGeo))
names(Ind) <- names(Estatal)
Ind$valor <- runif(nrow(Ind),min = 200,max = 1000)

Ind2 <- unique(Estatal[Estatal$id=="i78",c(1,3,5,6)])  %>% mutate(id="i1001",DesGeo="M") 
Ind2 <- expand.grid(unique(Ind2$id),unique(Muns),
                   unique(Ind2$t),1,unique(Ind2$m),
                   unique(Ind2$id2),unique(Ind2$DesGeo))
names(Ind2) <- names(Estatal)
Ind2$valor <- runif(nrow(Ind2),min = 200,max = 1000)
Ind <- rbind(Ind,Ind2);remove(Ind2)
TMP2 <- data.frame(ranget=unique(Ind[,c(1,3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,Ind)
EstatalMeta <- rbind(EstatalMeta,TMP2)
remove(Ind,TMP2,Muns)
#Exporta la base de datos
write.csv(Estatal,"data-out/datos.csv",row.names=FALSE)

#Meta
ExtraMeta <- ODS[1:2,]
ExtraMeta$Clave <- c("i1000","i1001")
ExtraMeta$Nombre.del.indicador <- c("Esto es una prueba","También esto es una prueba")
ODS <- rbind(ODS,ExtraMeta);remove(ExtraMeta)
