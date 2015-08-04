require(dplyr)
require(tidyr)
require(reshape2)
ODS <- read.csv("~/Downloads/Indicadores enlazados datos.gob.mx - Sheet1.csv",stringsAsFactors = FALSE)
ODS$Clave <- paste("i",1:nrow(ODS),sep="")
ODS <- ODS[ODS$URL.a.datos.gob.mx!="",]

##Clean I7
i=2
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP[,c(6,3,4,5)]
names(TMP) <- c("id","cve","t","valor")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))

##DB
Estatal <- TMP
EstatalMeta <- TMP2

##Clean I9
i=3
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP[,c(6,1,3,2)]
names(TMP) <- c("id","cve","t","valor")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I12
i=4
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP %>% gather(t,valor,Carencia.alimentación......2010:Carencia.alimentación......2012)
TMP <- TMP[,c(7,1,8,9)]
names(TMP) <- c("id","cve","t","valor")
TMP$t <- substr(TMP$t,start = regexpr(pattern = "[0-9]",text = TMP$t),1000)
TMP$cve <- ifelse(is.na(TMP$cve)==TRUE,0,TMP$cve)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I12
i=5
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP %>% gather(t,valor,Rezago.educativo.....2010:Rezago.educativo.....2012)
TMP <- TMP[,c(7,1,8,9)]
names(TMP) <- c("id","cve","t","valor")
TMP$t <- substr(TMP$t,start = regexpr(pattern = "[0-9]",text = TMP$t),1000)
TMP$cve <- ifelse(is.na(TMP$cve)==TRUE,0,TMP$cve)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I12
i=6
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP %>% gather(t,valor,Coeficiente.de.Gini.2010:Coeficiente.de.Gini.2012)
TMP <- TMP[,c(3,1,4,5)]
names(TMP) <- c("id","cve","t","valor")
TMP$t <- substr(TMP$t,start = regexpr(pattern = "[0-9]",text = TMP$t),1000)
TMP$cve <- ifelse(is.na(TMP$cve)==TRUE,0,TMP$cve)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I15
i=7
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP %>% gather(t,valor,Intensidad.de.la.pobreza.2010:Intensidad.de.la.pobreza.2012)
TMP <- TMP[,c(3,1,4,5)]
names(TMP) <- c("id","cve","t","valor")
TMP$t <- substr(TMP$t,start = regexpr(pattern = "[0-9]",text = TMP$t),1000)
TMP$cve <- ifelse(is.na(TMP$cve)==TRUE,0,TMP$cve)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I16
i=8
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP %>% gather(t,valor,Profundidad.de.la.pobreza.2010:Profundidad.de.la.pobreza.2012)
TMP <- TMP[,c(3,1,4,5)]
names(TMP) <- c("id","cve","t","valor")
TMP$t <- substr(TMP$t,start = regexpr(pattern = "[0-9]",text = TMP$t),1000)
TMP$cve <- ifelse(is.na(TMP$cve)==TRUE,0,TMP$cve)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I17
i=9
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP %>% gather(t,valor,Suscriptores.de.telefonía.móvil.por.cada.100.habitantes.Semestre.1:Suscriptores.de.telefonía.móvil.por.cada.100.habitantes.Semestre.2) %>% mutate(cve=0)
TMP <- TMP[,c(6,9,1,8,7)]
TMP$t <- ifelse(TMP$t=="Suscriptores.de.telefonía.móvil.por.cada.100.habitantes.Semestre.1",6,12)
names(TMP) <- c("id","cve","t","valor","m")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal$m <- 0
Estatal <- rbind(Estatal,TMP)
EstatalMeta$rangetm <- 0
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I18
i=10
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$cve <- 0
TMP$m <- 0
TMP <- TMP[,c(6,7,1,3,8)]
names(TMP) <- c("id","cve","t","valor","m")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I19
i=11
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$cve <- 0
TMP$m <- 0
TMP <- TMP[,c(6,7,1,3,8)]
names(TMP) <- c("id","cve","t","valor","m")
TMP$valor <- gsub("\\%","",TMP$valor)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I45
i=12
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(4,1,2,3,5)]
names(TMP) <- c("id","cve","t","valor","m")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I46
i=13
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(4,1,2,3,5)]
names(TMP) <- c("id","cve","t","valor","m")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I47
i=14
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(4,1,2,3,5)]
names(TMP) <- c("id","cve","t","valor","m")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I48
i=15
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(4,1,2,3,5)]
names(TMP) <- c("id","cve","t","valor","m")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I49
i=16
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(4,1,2,3,5)]
names(TMP) <- c("id","cve","t","valor","m")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I50
i=17
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,X..Hombres,X..Mujeres) 
TMP$id3 <- paste(TMP$id3,TMP$Quintil.de.ingresos,sep="-")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI50 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(4,2,3,7,5,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal$id2 <- "a"
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I51
i=18
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(4,1,2,3,5)]
names(TMP) <- c("id","cve","t","valor","m")
TMP$id2 <- "a"
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I52
i=19
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(6,1,2,5,7)]
names(TMP) <- c("id","cve","t","valor","m")
TMP$id2 <- "a"
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I111
i=22
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(t,valor,X2014)
TMP$t <- gsub("X","",TMP$t)
TMP <- TMP[,c(3,1,5,6,4)]
names(TMP) <- c("id","cve","t","valor","m")
TMP$id2 <- "a"
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I127
i=23
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- left_join(TMP,data.frame(Gpo_edad=unique(TMP$Gpo_edad),id2=letters[1:length(unique(TMP$Gpo_edad))]))
KeyI127 <- data.frame(id=TMP$id,id3=unique(TMP$Gpo_edad),id2=letters[1:length(unique(TMP$Gpo_edad))])
TMP <- TMP[,c(5,2,1,4,6,7)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I129
i=24
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$cve <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(3,5,1,2,4,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I130
i=25
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,2,1,3,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I131
i=26
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- paste(TMP$Sexo,TMP$Gpo_edad,sep="-")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI131 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(6,2,1,5,7,9)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I132
i=27
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- paste(TMP$Sexo,TMP$Gpo_edad,sep="-")
TMP$id3 <- gsub(" Hom","Hom",
                gsub(" Muj","Muj",
                     gsub("-30","-De 30",TMP$id3)))
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI132 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(6,2,1,5,7,9)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I133
i=28
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$Sexo <- ifelse(grepl("Hom",TMP$Sexo)==TRUE,"Hombres",TMP$Sexo)
TMP$Sexo <- ifelse(grepl("Muj",TMP$Sexo)==TRUE,"Mujeres",TMP$Sexo)
TMP$Gpo_edad <- ifelse(grepl("29",TMP$Gpo_edad)==TRUE,"De 15 a 29 años",TMP$Gpo_edad)
TMP$id3 <- paste(TMP$Sexo,TMP$Gpo_edad,sep="-")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI133 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(6,2,1,5,7,9)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I135
i=29
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,2,1,3,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean DBBase
remove(TMP,TMP2,i)
#Key for subgroups
Keys <- unique(rbind(KeyI50,KeyI127,KeyI131,KeyI132,KeyI133))
remove(KeyI50,KeyI127,KeyI131,KeyI132,KeyI133)
Keys$id3 <- gsub("X..","",Keys$id3)

#INEGI Codes 
Estatal$cve <- gsub("Aguascalientes","1",
                    gsub("Baja California Sur","3",
                         gsub("Baja California","2",
                              gsub("Campeche","4",
                                   gsub("Coahuila de Zaragoza","5",
                                        gsub("Colima","6",
                                             gsub("Chiapas","7",
                                                  gsub("Chihuahua","8",
                                                       gsub("Distrito Federal","9",
                                                            gsub("Durango","10",
                                                                 gsub("Guanajuato","11",
                                                                      gsub("Guerrero","12",
                                                                           gsub("Hidalgo","13",
                                                                                gsub("Jalisco","14",
                                                                                     gsub("México","15",
                                                                                          gsub("Michoacán de Ocampo","16",
                                                                                               gsub("Morelos","17",
                                                                                                    gsub("Nayarit","18",
                                                                                                         gsub("Nuevo León","19",
                                                                                                              gsub("Oaxaca","20",
                                                                                                                   gsub("Puebla","21",
                                                                                                                        gsub("Querétaro","22",
                                                                                                                             gsub("Quintana Roo","23",
                                                                                                                                  gsub("San Luis Potosí","24",
                                                                                                                                       gsub("Sinaloa","25",
                                                                                                                                            gsub("Sonora","26",
                                                                                                                                                 gsub("Tabasco","27",
                                                                                                                                                      gsub("Tamaulipas","28",
                                                                                                                                                           gsub("Tlaxcala","29",
                                                                                                                                                                gsub("Veracruz de Ignacio de la Llave","30",
                                                                                                                                                                     gsub("Yucatán","31",
                                                                                                                                                                          gsub("Zacatecas","32",
                                                                                                                                                                               gsub("Nacional","0",
                                                                                                                                                                                    gsub("Michoacán","16",
                                                                                                                                                                                         gsub("Veracruz","30",
                                                                                                                                                                                              gsub("Coahuila","5",
                                                                                                                                                                                                   gsub("Michoacan","16",
                                                                                                                                                                                                        gsub("Nuevo Leon","19",
                                                                                                                                                                                                             gsub("Queretaro","22",
                                                                                                                                                                                                                  gsub("San Luis Potosi","24",
                                                                                                                                                                                                                       gsub("Yucatan","31",
                                                                                                                                                                                                                            gsub("Mexico","15",Estatal$cve))))))))))))))))))))))))))))))))))))))))))
Estatal$cve <- gsub("2 Sur","3",Estatal$cve)
Estatal$cve <- as.numeric(as.character(gsub(pattern = "[a-z]",replacement = "",ignore.case = TRUE,x = Estatal$cve)))
Estatal$DesGeo <- ifelse(Estatal$cve==0,"Nacional","Estatal")
write.csv(Estatal,"data-out/datos.csv",row.names=FALSE)

#Write subgroup descriptions
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


