######################################################################
### Title: Limpia los recursos de indicadores ODS
###        y genera csv unificados para la plataforma de seguimiento
### Date: 21/08/2015
######################################################################


#Unifica los codigos INEGI

#INEGI Codes 
Estatal$cve <- gsub(ignore.case = TRUE,"Aguascalientes","1",
                    gsub(ignore.case = TRUE,"Baja California Sur","3",
                         gsub(ignore.case = TRUE,"Baja California","2",
                              gsub(ignore.case = TRUE,"Campeche","4",
                                   gsub(ignore.case = TRUE,"Coahuila de Zaragoza","5",
                                        gsub(ignore.case = TRUE,"Colima","6",
                                             gsub(ignore.case = TRUE,"Chiapas","7",
                                                  gsub(ignore.case = TRUE,"Chihuahua","8",
                                                       gsub(ignore.case = TRUE,"Distrito Federal","9",
                                                            gsub(ignore.case = TRUE,"Durango","10",
                                                                 gsub(ignore.case = TRUE,"Guanajuato","11",
                                                                      gsub(ignore.case = TRUE,"Guerrero","12",
                                                                           gsub(ignore.case = TRUE,"Hidalgo","13",
                                                                                gsub(ignore.case = TRUE,"Jalisco","14",
                                                                                     gsub(ignore.case = TRUE,"México","15",
                                                                                          gsub(ignore.case = TRUE,"Michoacán de Ocampo","16",
                                                                                               gsub(ignore.case = TRUE,"Morelos","17",
                                                                                                    gsub(ignore.case = TRUE,"Nayarit","18",
                                                                                                         gsub(ignore.case = TRUE,"Nuevo León","19",
                                                                                                              gsub(ignore.case = TRUE,"Oaxaca","20",
                                                                                                                   gsub(ignore.case = TRUE,"Puebla","21",
                                                                                                                        gsub(ignore.case = TRUE,"Querétaro","22",
                                                                                                                             gsub(ignore.case = TRUE,"Quintana Roo","23",
                                                                                                                                  gsub(ignore.case = TRUE,"San Luis Potosí","24",
                                                                                                                                       gsub(ignore.case = TRUE,"Sinaloa","25",
                                                                                                                                            gsub(ignore.case = TRUE,"Sonora","26",
                                                                                                                                                 gsub(ignore.case = TRUE,"Tabasco","27",
                                                                                                                                                      gsub(ignore.case = TRUE,"Tamaulipas","28",
                                                                                                                                                           gsub(ignore.case = TRUE,"Tlaxcala","29",
                                                                                                                                                                gsub(ignore.case = TRUE,"Veracruz de Ignacio de la Llave","30",
                                                                                                                                                                     gsub(ignore.case = TRUE,"Yucatán","31",
                                                                                                                                                                          gsub(ignore.case = TRUE,"Zacatecas","32",
                                                                                                                                                                               gsub(ignore.case = TRUE,"Nacional","0",
                                                                                                                                                                                    gsub(ignore.case = TRUE,"Michoacán","16",
                                                                                                                                                                                         gsub(ignore.case = TRUE,"Veracruz","30",
                                                                                                                                                                                              gsub(ignore.case = TRUE,"Coahuila","5",
                                                                                                                                                                                                   gsub(ignore.case = TRUE,"Michoacan","16",
                                                                                                                                                                                                        gsub(ignore.case = TRUE,"Nuevo Leon","19",
                                                                                                                                                                                                             gsub(ignore.case = TRUE,"Queretaro","22",
                                                                                                                                                                                                                  gsub(ignore.case = TRUE,"San Luis Potosi","24",
                                                                                                                                                                                                                       gsub(ignore.case = TRUE,"Yucatan","31",
                                                                                                                                                                                                                            gsub(ignore.case = TRUE,"Mexico","15",Estatal$cve))))))))))))))))))))))))))))))))))))))))))
Estatal$cve <- gsub("2 Sur","3",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("5 de Zaragoza","5",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("16 de Ocampo","16",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("30 de Ignacio de la Llave","30",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Estado de 15","15",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Estado de 15","15",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("22 de Arteaga","22",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("30 Llave","30",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("22 Arteaga","22",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Estados Unidos Mexicanos","0",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Republica Mexicana","0",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("M\xe9xico","15",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Michoac\xe1n","16",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Nuevo Le\xf3n","19",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Quer\xe9taro","22",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("San Luis Potos\xed","24",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("Yucat\xe1n","31",Estatal$cve, ignore.case = TRUE)
Estatal$cve <- gsub("30 Igancio de la Llave","30",Estatal$cve, ignore.case = TRUE)

Estatal$cve <- as.numeric(Estatal$cve)
Estatal$DesGeo <- ifelse(Estatal$cve==0,"N",
                         ifelse(Estatal$cve>=1001,"M","E"))

#Exporta la base de datos
write.csv(Estatal,"data-out/datos.csv",row.names=FALSE)

