######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################

### Limpia los ODS que tienen liga al  21/08/2015

##Clean I7
i=which(ODS$Clave=="i7")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP[,c(5,1,3,4)]
names(TMP) <- c("id","cve","t","valor")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))

##DB
Estatal <- TMP
EstatalMeta <- TMP2

##Clean I9
i=which(ODS$Clave=="i9")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP <- TMP[,c(6,1,3,2)]
names(TMP) <- c("id","cve","t","valor")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP$t))
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I12
i=which(ODS$Clave=="i12")
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

##Clean I13
i=which(ODS$Clave=="i13")
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

##Clean I14
i=which(ODS$Clave=="i14")
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
i=which(ODS$Clave=="i15")
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
i=which(ODS$Clave=="i16")
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
i=which(ODS$Clave=="i17")
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
i=which(ODS$Clave=="i18")
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
i=which(ODS$Clave=="i19")
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
i=which(ODS$Clave=="i46")
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
i=which(ODS$Clave=="i47")
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
i=which(ODS$Clave=="i48")
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
i=which(ODS$Clave=="i49")
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
i=which(ODS$Clave=="i50")
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
i=which(ODS$Clave=="i51")
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

# ##Clean I52
# i=which(ODS$Clave=="i52")
# TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
# TMP$id <- ODS$Clave[i]
# TMP$m <- 0
# TMP <- TMP[,c(6,1,2,5,7)]
# names(TMP) <- c("id","cve","t","valor","m")
# TMP$id2 <- "a"
# TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
# names(TMP2) <- c("id","ranget","rangetm")
# Estatal <- rbind(Estatal,TMP)
# EstatalMeta <- rbind(EstatalMeta,TMP2)

##Clean I111
i=which(ODS$Clave=="i111")
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
i=which(ODS$Clave=="i127")
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
i=which(ODS$Clave=="i129")
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
i=which(ODS$Clave=="i130")
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
i=which(ODS$Clave=="i131")
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
i=which(ODS$Clave=="i132")
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
i=which(ODS$Clave=="i133")
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
i=which(ODS$Clave=="i135")
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

##Clean I98
# i=which(ODS$Clave=="i98")
# TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
# TMP$id <- ODS$Clave[i]
# TMP$m <- 0
# TMP$id2 <- "a"
# # TMP <- TMP[,c(9,1,)]
# names(TMP) <- c("id","cve","t","valor","m","id2")
# TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
# names(TMP2) <- c("id","ranget","rangetm")
# Estatal <- rbind(Estatal,TMP)
# EstatalMeta <- rbind(EstatalMeta,TMP2)
# 

##Clean I108
i=which(ODS$Clave=="i108")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,1,2,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I97
i=which(ODS$Clave=="i97")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP$t <- 2010
TMP <- TMP[,c(4,1,7,3,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I96
i=which(ODS$Clave=="i96")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$cve <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,6,1,3,5,7)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I95
i=which(ODS$Clave=="i95")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[,c(1,4,7,8,9)]
TMP <- TMP %>% gather(id3,valor,Densidad.de.población.bruta..2010...por.km2,Densidad.de.población.bruta..2015..por.km2) 
TMP$id3 <- gsub("km2","",TMP$id3)
TMP$id3 <- gsub("[a-z\u00E0-\u00FC]","",TMP$id3,ignore.case = TRUE)
TMP$id3 <- gsub("\\.","",TMP$id3,ignore.case = TRUE)
TMP <- TMP[,c(2,1,4,5,3)]
TMP$id2 <- "a"
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I90
i=which(ODS$Clave=="i90")
TMP <- read.csv(ODS$URL.indicador[i],fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,Rezago.habitacional.por.condiciones.de.espacio..calculado.con.el.Modulo.de.Condiciones.Sociecoómicas.de.la.ENIGH.2014:Rezago.habitacional.Total..calculado.con.el.Modulo.de.Condiciones.Sociecoómicas.de.la.ENIGH.2014) 
TMP$id3 <- gsub("\\."," ",TMP$id3,ignore.case = TRUE)
TMP$id3 <- gsub("  calculado con el Modulo de Condiciones Sociecoómicas de la ENIGH","",TMP$id3)
TMP$t <- gsub("[a-z\u00E0-\u00FC]","",TMP$id3,ignore.case = TRUE)
TMP$id3 <- gsub("[0-9]","",TMP$id3,ignore.case = TRUE)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI90 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(2,1,6,5,3,7)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I88
i=which(ODS$Clave=="i88")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- paste("Quintil",TMP$Quintil.de.ingreso,sep="-")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
TMP$cve <- 0
KeyI88 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(6,10,1,5,7,9)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)


#Clean I86
i=which(ODS$Clave=="i86")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,2,1,3,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I84
i=which(ODS$Clave=="i84")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I83
i=which(ODS$Clave=="i83")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I79
i=which(ODS$Clave=="i79")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I78
i=which(ODS$Clave=="i78")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I77
i=which(ODS$Clave=="i77")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)


#Clean I76
i=which(ODS$Clave=="i76")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I74
i=which(ODS$Clave=="i74")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I73
i=which(ODS$Clave=="i73")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I70
i=which(ODS$Clave=="i70")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,3,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I68
i=which(ODS$Clave=="i68")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- gsub("[a-z\u00E0-\u00FC]","",TMP$cve,ignore.case = TRUE)
TMP$cve <- gsub("-","",TMP$cve,ignore.case = TRUE)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I64
i=which(ODS$Clave=="i64")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- gsub("[0-9]","",TMP$cve,ignore.case = TRUE)
TMP$cve <- gsub("-","",TMP$cve,ignore.case = TRUE)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I63
i=which(ODS$Clave=="i63")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I62
i=which(ODS$Clave=="i62")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,3,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I61
i=which(ODS$Clave=="i61")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- gsub("[0-9]","",TMP$cve,ignore.case = TRUE)
TMP$cve <- gsub("-","",TMP$cve,ignore.case = TRUE)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I60
i=which(ODS$Clave=="i60")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,2,1,3,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- gsub("Estados Unidos Mexicanos","NACIONAL",TMP$cve,ignore.case = TRUE)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I6
i=which(ODS$Clave=="i6")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(5,1,3,4,6,7)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- gsub('([[:upper:]])', ' \\1', TMP$cve)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I121
i=which(ODS$Clave=="i121")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,1,2,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- substr(TMP$cve,1,2)
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I120
i=which(ODS$Clave=="i120")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE,fileEncoding="ISO-8859-3")
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(49,1,2,5,50,51)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- substr(TMP$cve,1,3)
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I119
i=which(ODS$Clave=="i119")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(6,1,2,5,7,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- substr(TMP$cve,1,2)
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I118
i=which(ODS$Clave=="i118")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP$ENTIDAD <- substr(TMP$ENTIDAD,1,2)
TMP$cve <- paste(formatC(TMP$ENTIDAD,width = 2,flag = 0),
                   formatC(TMP$NUMMPIO,width = 3,flag = 0),sep="")
TMP <- TMP[,c(9,12,5,8,10,11)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)


#Clean I117
i=which(ODS$Clave=="i117")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(8,1,2,6,9,10)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$cve <- substr(TMP$cve,1,2)
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I116
i=which(ODS$Clave=="i116")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,X.H1GPRE,X.M1GPRE,X.T1GPRE) 
TMP$id3 <- paste(TMP$id3,TMP$SERVICIO,sep=" - ")
TMP$id3 <- gsub("X.H1GPRE","Hombres",TMP$id3);TMP$id3 <- gsub("X.M1GPRE","Mujeres",TMP$id3);TMP$id3 <- gsub("X.T1GPRE","Total",TMP$id3)
TMP$id3 <- gsub("[0-9]","",TMP$id3);TMP$id3 <- gsub("_"," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI116 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$ENTIDAD <- substr(TMP$ENTIDAD,1,2)
TMP$cve <- paste(formatC(TMP$ENTIDAD,width = 2,flag = 0),
                 formatC(TMP$NUMMPIO,width = 3,flag = 0),sep="")
TMP <- TMP[,c(11,16,6,14,12,15)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I114
i=which(ODS$Clave=="i114")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- TMP$SERVICIO
TMP$id3 <- gsub("[0-9]","",TMP$id3);TMP$id3 <- gsub("_"," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI114 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$ENTIDAD <- substr(TMP$ENTIDAD,1,2)
TMP$cve <- paste(formatC(TMP$ENTIDAD,width = 2,flag = 0),
                 formatC(TMP$NUMMPIO,width = 3,flag = 0),sep="")
TMP <- TMP[,c(12,16,6,11,13,15)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I113
i=which(ODS$Clave=="i113")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- TMP$SERVICIO
TMP$id3 <- gsub("[0-9]","",TMP$id3);TMP$id3 <- gsub("_"," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI113 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$ENTIDAD <- substr(TMP$ENTIDAD,1,2)
TMP$cve <- paste(formatC(TMP$ENTIDAD,width = 2,flag = 0),
                 formatC(TMP$NUMMPIO,width = 3,flag = 0),sep="")
TMP <- TMP[,c(10,14,6,9,11,13)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I112
i=which(ODS$Clave=="i112")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,Tasa.neta.de.matriculación.en.la.enseñanza.primaria..6.a.11.años.de.edad.,Tasa.neta.de.matriculación.en.secundaria..12.a.14.años.de.edad.,Tasa.neta.de.matriculación.en.media.superior..15.a.17.años.de.edad.) 
TMP$id3 <- ifelse(grepl("primaria",TMP$id3)==TRUE,"Primaria",
                  ifelse(grepl("secundaria",TMP$id3)==TRUE,"Secundaria","Media Superior"))
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI112 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$ENTIDAD <- substr(TMP$ENTIDAD,1,2)
TMP$cve <- paste(formatC(TMP$ENTIDAD,width = 2,flag = 0),
                 formatC(TMP$NUMMPIO,width = 3,flag = 0),sep="")
TMP <- TMP[,c(11,16,4,14,12,15)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP$t <- substr(TMP$t,1,4)
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I107
i=which(ODS$Clave=="i107")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Madres.jefas.de.familia) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI107 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i176")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I106
i=which(ODS$Clave=="i106")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Personas.con.discapacidad) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI106 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i175")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I105
i=which(ODS$Clave=="i105")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Madres.jefas.de.familia) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI105 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i174")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I104
i=which(ODS$Clave=="i104")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Madres.jefas.de.familia) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI104 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i173")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I103
i=which(ODS$Clave=="i103")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Personas.con.discapacidad) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI103 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i172")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I102
i=which(ODS$Clave=="i102")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP$id2 <- "a"
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i172")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I101
i=which(ODS$Clave=="i101")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Madres.jefas.de.familia) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI101 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i170")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I100
i=which(ODS$Clave=="i100")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Madres.jefas.de.familia) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI100 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i169")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I99
i=which(ODS$Clave=="i99")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
names(TMP)[2] <- "Total"
TMP <- TMP %>% gather(id3,valor,Total:Madres.jefas.de.familia) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI99 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(2,7,1,5,3,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
#B
i=which(ODS$Clave=="i168")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I98
i=which(ODS$Clave=="i98")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$t <- "2010"
TMP$id2 <- "a"
TMP <- TMP[,c(9,1,11,6,10,12)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I94
i=which(ODS$Clave=="i94")
#A
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,Tiempo.de.Traslado.Total,Tiempo.de.Traslado.a.la.Escuela,Tiempo.de.Traslado.al.Trabajo) 
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP$id3 <- paste(TMP$Ambito," - ", TMP$id3," - Edades ",TMP$Por.Grupo.de.Edad,sep=" ")
TMP$id3 <- gsub("  - Edades  Nacional","",TMP$id3);TMP$id3 <- gsub("  - Edades  Urbano","",TMP$id3);TMP$id3 <- gsub("  - Edades  Rural","",TMP$id3)
TMP$id3 <- gsub("  "," ",TMP$id3)
i=which(ODS$Clave=="i177")
TMP2 <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP2$id <- ODS$Clave[i]
TMP2$m <- 0
TMP2$id2 <- "a"
TMP2 <- TMP2 %>% gather(id3,valor,Tiempo.de.Traslado.Total,Tiempo.de.Traslado.a.la.Escuela,Tiempo.de.Traslado.al.Trabajo) 
TMP2$id3 <- gsub("\\."," ",TMP2$id3)
TMP2$id3 <- paste(TMP2$Ambito," - ", TMP2$id3," - Duración de Jornada Labora de ",TMP2$Por.Duración.de.la.Jornada.Laboral,sep=" ")
TMP2$id3 <- gsub("  - Duración de Jornada Labora de  Nacional","",TMP2$id3);TMP2$id3 <- gsub("  - Duración de Jornada Labora de  Urbano","",TMP2$id3);TMP2$id3 <- gsub("  - Duración de Jornada Labora de  Rural","",TMP2$id3)
TMP2$id3 <- gsub("  "," ",TMP2$id3)
TMP$cve <- 0
TMP <- TMP[,c(4,8,3,7,5,6)]
TMP2$cve <- 0
TMP2$id <- "i94"
TMP2 <- TMP2[,c(4,9,3,8,5,7)]
TMP <- rbind(TMP,TMP2)
remove(TMP2)
DoubleLetters <- expand.grid(letters,c("",letters));DoubleLetters <- paste(DoubleLetters$Var1,DoubleLetters$Var2,sep="")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=DoubleLetters[1:length(unique(TMP$id3))]))
KeyI94 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=DoubleLetters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(1,2,3,4,5,7)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I89
i=which(ODS$Clave=="i89")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP %>% gather(id3,valor,Proporción.de.la.población.urbana.que.habita.en.viviendas.precarias.con.base.en.la.ENIGH.2010:Proporción.de.la.población.urbana.que.habita.en.viviendas.precarias.con.base.en.la.ENIGH.2014) 
TMP$id3 <- gsub("[a-z]",replacement = "",TMP$id3,ignore.case = TRUE)
TMP$id3 <- gsub("ó",replacement = "",TMP$id3,ignore.case = TRUE)
TMP$id3 <- gsub("\\.",replacement = "",TMP$id3,ignore.case = TRUE)
TMP <- TMP[,c(2,1,5,6,3,4)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I177
i=which(ODS$Clave=="i177")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,Tiempo.de.Traslado.Total:Tiempo.de.Traslado.al.Trabajo)
TMP$Por.Duración.de.la.Jornada.Laboral <- gsub(pattern = "Nacional",replacement = "A",TMP$Por.Duración.de.la.Jornada.Laboral)
TMP$Por.Duración.de.la.Jornada.Laboral <- gsub(pattern = "Urbano",replacement = "A",TMP$Por.Duración.de.la.Jornada.Laboral)
TMP$Por.Duración.de.la.Jornada.Laboral <- gsub(pattern = "Rural",replacement = "A",TMP$Por.Duración.de.la.Jornada.Laboral)
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP$id3 <- paste(TMP$id3,TMP$Ambito,TMP$Por.Duración.de.la.Jornada.Laboral,sep=" - ")
TMP$id3 <- gsub(" - A","",TMP$id3)
DoubleLetters <- expand.grid(letters,c("",letters));DoubleLetters <- paste(DoubleLetters$Var1,DoubleLetters$Var2,sep="")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=DoubleLetters[1:length(unique(TMP$id3))]))
KeyI177 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=DoubleLetters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(4,9,3,7,5,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I176
i=which(ODS$Clave=="i176")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(5,1,3,2,4,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I175
i=which(ODS$Clave=="i175")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I174
i=which(ODS$Clave=="i174")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I173
i=which(ODS$Clave=="i173")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I172
i=which(ODS$Clave=="i172")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I171
i=which(ODS$Clave=="i171")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I170
i=which(ODS$Clave=="i170")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I169
i=which(ODS$Clave=="i169")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I168
i=which(ODS$Clave=="i168")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)

#Clean I168
i=which(ODS$Clave=="i168")
TMP <- read.csv(ODS$URL.indicador[i],stringsAsFactors = FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(4,1,3,2,5,6)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)


#Clean I167
dir.create("tmp")
i=which(ODS$Clave=="i167")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/24_3_tipo_violencia_sit_conyugal_cualquier_agresor .csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[is.na(TMP$Clave.de.la.entidad)==FALSE,]
TMP$id3 <- paste(TMP$Tipo.de.violencia,TMP$Situación.conyugal,sep=" - ")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI167 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(10,2,1,8,11,13)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I166
dir.create("tmp")
i=which(ODS$Clave=="i166")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/24_2_tipo_violencia_mujeres_indigena_cualquier_agresor.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[is.na(TMP$Clave.de.la.entidad)==FALSE,]
TMP$id3 <- paste(TMP$Tipo.de.violencia,sep="")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI166 <- data.frame(id=TMP$id,id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(9,2,1,7,10,12)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I165
dir.create("tmp")
i=which(ODS$Clave=="i165")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/24_1_tipo_violencia_mujeres_cualquier_agresor.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP[is.na(TMP$Clave.de.la.entidad)==FALSE,]
TMP$id3 <- paste(TMP$Tamaño.de.localidad,TMP$Tipo.de.violencia,sep=" - ")
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI165 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(9,2,1,8,10,12)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I164
dir.create("tmp")
i=which(ODS$Clave=="i164")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/23_porcentaje_gasto_en_vivienda.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- TMP$Tamaño.de.localidad
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI164 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP <- TMP[,c(9,13,1,5,10,12)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I163
dir.create("tmp")
i=which(ODS$Clave=="i163")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/22_pnea_15_24_no_asiste_escuela.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,Porcentaje.de.población.no.económicamente.activa.de.15.a.24.años.que.no.asiste.a.la.escuela:Porcentaje.de.mujeres.de.15.a.24.años.no.económicamente.activas.que.no.asisten.a.la.escuela) 
TMP$id3 <- gsub("Porcentaje.de.población.no.económicamente.activa.de.15.a.24.años.que.no.asiste.a.la.escuela","Total",TMP$id3)
TMP$id3 <- gsub("Porcentaje.de.hombres.de.15.a.24.años.no.económicamente.activos.que.no.asisten.a.la.escuela","Hombres",TMP$id3)
TMP$id3 <- gsub("Porcentaje.de.mujeres.de.15.a.24.años.no.económicamente.activas.que.no.asisten.a.la.escuela","Mujeres",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI163 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(11,3,1,14,12,15)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I162
dir.create("tmp")
i=which(ODS$Clave=="i162")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/21_pib_per_capita_ppc.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(10,2,1,7,11,12)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I161
dir.create("tmp")
i=which(ODS$Clave=="i161")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/20_energia_solar.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)[,1:12]
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,Porcentaje..de.viviendas.con.disponibilidad.de.paneles.y.calentadores.solares,Porcentaje.de.viviendas.en.localidades.urbanas.con.disponibilidad.de.paneles.y.calentadores.solares,Porcentaje.de.viviendas.en.localidades.rurales.con.disponibilidad.de.paneles.y.calentadores.solares) 
TMP$id3 <- gsub("Porcentaje..de.viviendas.con.disponibilidad.de.paneles.y.calentadores.solares","Total",TMP$id3)
TMP$id3 <- gsub("Porcentaje.de.viviendas.en.localidades.urbanas.con.disponibilidad.de.paneles.y.calentadores.solares","Urbano",TMP$id3)
TMP$id3 <- gsub("Porcentaje.de.viviendas.en.localidades.rurales.con.disponibilidad.de.paneles.y.calentadores.solares","Rural",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI161 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(10,2,1,13,11,14)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I160
dir.create("tmp")
i=which(ODS$Clave=="i160")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/19_focos_ahorradores.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)[,1:12]
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor,Porcentaje.de.viviendas.con.disponibilidad.de.focos.ahorradores,Porcentaje.de.viviendas.en.localidades.urbanas.con.disponibilidad.de.focos.ahorradores,Porcentaje.de.viviendas.en.localidades.rurales.con.disponibilidad.de.focos.ahorradores) 
TMP$id3 <- gsub("Porcentaje.de.viviendas.con.disponibilidad.de.focos.ahorradores","Total",TMP$id3)
TMP$id3 <- gsub("Porcentaje.de.viviendas.en.localidades.urbanas.con.disponibilidad.de.focos.ahorradores","Urbano",TMP$id3)
TMP$id3 <- gsub("Porcentaje.de.viviendas.en.localidades.rurales.con.disponibilidad.de.focos.ahorradores","Rural",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI160 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(10,2,1,13,11,14)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I159
dir.create("tmp")
i=which(ODS$Clave=="i159")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/18_bis_lenia_o_carbon.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(7,2,1,4,8,9)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I158
dir.create("tmp")
i=which(ODS$Clave=="i158")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/18_Lenia_o_carbon.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP$cve <- paste(formatC(TMP$Clave.de.la.entidad,width = 2,flag = 0),formatC(TMP$Clave.del.municipio,width = 3,flag = 0),sep="")
TMP$cve <- gsub(" NA","",TMP$cve)
TMP <- TMP[,c(11,14,1,8,12,13)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I157
dir.create("tmp")
i=which(ODS$Clave=="i157")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/17_agua_entubada.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- TMP$Clasificación
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI157 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- paste(formatC(TMP$Clave.de.la.entidad,width = 2,flag = 0),formatC(TMP$Clave.del.municipio,width = 3,flag = 0),sep="")
TMP$cve <- gsub(" NA","",TMP$cve)
TMP <- TMP[,c(12,16,1,9,13,15)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I156
dir.create("tmp")
i=which(ODS$Clave=="i156")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/16_proporcion_saneamiento_2010.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- TMP$Tamaño.de.localidad
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI156 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- paste(formatC(TMP$Clave.de.la.entidad,width = 2,flag = 0),formatC(TMP$Clave.del.municipio,width = 3,flag = 0),sep="")
TMP$cve <- gsub(" NA","",TMP$cve)
TMP <- TMP[,c(10,14,1,9,11,13)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I155
dir.create("tmp")
i=which(ODS$Clave=="i155")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/15_proporcion_abastecimiento_agua_2010.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- TMP$Tamaño.de.localidad
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI155 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- paste(formatC(TMP$Clave.de.la.entidad,width = 2,flag = 0),formatC(TMP$Clave.del.municipio,width = 3,flag = 0),sep="")
TMP$cve <- gsub(" NA","",TMP$cve)
TMP <- TMP[,c(10,14,1,9,11,13)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I155
dir.create("tmp")
i=which(ODS$Clave=="i155")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/15_proporcion_abastecimiento_agua_2010.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id3 <- TMP$Tamaño.de.localidad
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI155 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- paste(formatC(TMP$Clave.de.la.entidad,width = 2,flag = 0),formatC(TMP$Clave.del.municipio,width = 3,flag = 0),sep="")
TMP$cve <- gsub(" NA","",TMP$cve)
TMP <- TMP[,c(10,14,1,9,11,13)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I154
dir.create("tmp")
i=which(ODS$Clave=="i154")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/14_mujeres_puestos_directivos.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP <- TMP %>% gather(id3,valor, Mujeres.en.puestos.gerenciales:Porcentaje.de.mujeres.en.puestos.directivos.de.la.administración.pública)
TMP$id3 <- gsub("\\."," ",TMP$id3)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI154 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP$cve <- 0
TMP$m <- gsub("Tercer trimestre",9,TMP$Trimestre)
TMP$m <- gsub("Primer trimestre",3,TMP$m)
TMP$m <- gsub("Segundo trimestre",6,TMP$m)
TMP$m <- gsub("Cuarto trimestre",12,TMP$m)
TMP <- TMP[,c(4,9,1,7,5,8)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP <- TMP[is.na(TMP$t)==FALSE,]
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I153
dir.create("tmp")
i=which(ODS$Clave=="i153")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/12_porcentaje_mujeres_alcaldias.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$id2 <- "a"
TMP <- TMP[,c(8,2,1,6,9,10)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean I152
dir.create("tmp")
i=which(ODS$Clave=="i152")
download.file(ODS$URL.indicador[i],destfile = "tmp/tmp.zip")
unzip(zipfile = "tmp/tmp.zip",exdir = "tmp")
TMP <- read.csv("tmp/11_promedio_horas_trabajo_domestico_cuidados.csv",fileEncoding="ISO-8859-3",stringsAsFactors=FALSE)
TMP$id <- ODS$Clave[i]
TMP$m <- 0
TMP$cve <- 0
TMP$id3 <- TMP$Nacional
TMP$Nacional <- gsub("Estados Unidos Mexicanos","Total",TMP$Nacional)
TMP <- left_join(TMP,data.frame(id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))]))
KeyI152 <- data.frame(id=unique(TMP$id),id3=unique(TMP$id3),id2=letters[1:length(unique(TMP$id3))])
TMP <- TMP[,c(9,11,1,7,10,13)]
names(TMP) <- c("id","cve","t","valor","m","id2")
TMP2 <- data.frame(id=ODS$Clave[i],ranget=unique(TMP[,c(3,5)]))
names(TMP2) <- c("id","ranget","rangetm")
Estatal <- rbind(Estatal,TMP)
EstatalMeta <- rbind(EstatalMeta,TMP2)
unlink("tmp/",recursive = TRUE)

#Clean DBBase
remove(TMP,TMP2,i)
##########################################################################################################################

#####################################
Missing <- anti_join(ODS, data.frame(Clave=unique(Estatal$id)))
####################################

