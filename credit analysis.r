library(dplyr)
library(lubridate)
library(ggplot2)
library(gapminder)
library(stringr)
# ------------------------------------------------------------------------------
# Carga datos
# ------------------------------------------------------------------------------
datos <- read.csv("DataCPM_Evaluacion_clean.csv", encoding = "UTF-8", col.names = c("SEXO"
                                                          , "FECHA.NACIMIENTO"
                                                          , "FECHA.CONTRATACION"
                                                          , "TIPO.VIVIENDA"
                                                          , "ESTADO.CIVIL"
                                                          , "CIUDAD"
                                                          , "SUCURSAL"
                                                          , "PRODUCTO"
                                                          , "MONTO.CONTRATADO"
                                                          , "SALDO.ACTUAL"
                                                          , "INTERES.VIGENTE"
                                                          , "DIAS.MORA"))
# ------------------------------------------------------------------------------
# Limpieza de variables
# ------------------------------------------------------------------------------
datos$SEXO <- substr(datos$SEXO,1,1) 
datos$FECHA.NACIMIENTO <- gsub("[^A-Za-z0-9]", "-",datos$FECHA.NACIMIENTO)
datos$FECHA.CONTRATACION <- gsub("[^A-Za-z0-9]", "-",datos$FECHA.CONTRATACION)
datos$ESTADO.CIVIL <- ifelse(str_detect(datos$ESTADO.CIVIL, pattern = "CASADO.*"), "CASADO"
                             , ifelse(str_detect(datos$ESTADO.CIVIL, pattern = "SOLTERO.*"), "SOLTERO"
                             , ifelse(str_detect(datos$ESTADO.CIVIL, pattern = "UNION.*"), "UNION LIBRE", "VIUDO")))
datos$CIUDAD <- str_replace_all(datos$CIUDAD, pattern = "\\\\", replacement = "")
datos$PRODUCTO <- stringi::stri_trans_general(datos$PRODUCTO,"Latin-ASCII")
datos$PRODUCTO <- str_replace_all(datos$PRODUCTO, pattern = " _.*", replacement = "") %>% stringi::stri_trim(., "right")
datos$DIAS.MORA <- ifelse(datos$DIAS.MORA %in% c("cero","Zero", "S/N Mora"),"0",datos$DIAS.MORA) %>% as.integer()
# ------------------------------------------------------------------------------
# Asignando tipos de variables
# ------------------------------------------------------------------------------
datos$SEXO <- as.factor(datos$SEXO)
datos$FECHA.NACIMIENTO <- c(as.Date(datos$FECHA.NACIMIENTO[1:40], format = "%d-%m-%Y")
                                , as.Date(datos$FECHA.NACIMIENTO[41:60], format = "%Y-%m-%d")
                                , as.Date(datos$FECHA.NACIMIENTO[61:nrow(datos)], format = "%d-%m-%Y"))
datos$FECHA.CONTRATACION <- c(as.Date(datos$FECHA.CONTRATACION[1:40], format = "%d-%m-%y")
                              , as.Date(datos$FECHA.CONTRATACION[41:60], format = "%Y-%m-%d")
                              , as.Date(datos$FECHA.CONTRATACION[61:nrow(datos)], format = "%d-%m-%y"))
datos$TIPO.VIVIENDA <- as.factor(datos$TIPO.VIVIENDA)
datos$ESTADO.CIVIL <- as.factor(datos$ESTADO.CIVIL)
datos$CIUDAD <- as.factor(datos$CIUDAD)
datos$SUCURSAL <- as.factor(datos$SUCURSAL)
# ------------------------------------------------------------------------------
# Generando nuevas variables
# ------------------------------------------------------------------------------
datos$MES.CONTRATACION <- month(datos$FECHA.CONTRATACION, label = TRUE)
datos$ANIO.CONTRATACION <- year(datos$FECHA.CONTRATACION)
datos$EDAD.CONTRATACION <- year(datos$FECHA.CONTRATACION) - year(datos$FECHA.NACIMIENTO)
datos <-  datos %>% relocate(MES.CONTRATACION:EDAD.CONTRATACION, .after = FECHA.CONTRATACION)

#Filtrado de datos, evitando casos atipicos
#Filtrado, quitar casos atìpicos para el anàlisis estadìstico. 
#-------------------------------------------------------------------------------
datos <- datos %>%
  filter(DIAS.MORA<100 )


# ------------------------------------------------------------------------------
#Graficas para la composición de los datos





datos %>%
  group_by(SEXO, ESTADO.CIVIL, ANIO.CONTRATACION, CIUDAD) %>%
  summarize(Clientes=n()) %>%
  ggplot(aes(SEXO, Clientes, fill = ESTADO.CIVIL))+
  geom_bar(stat='identity', position = 'fill')+
  facet_wrap(~ANIO.CONTRATACION)+labs(x = "SEXO"
                                      , y = "Porcentaje de clientes"
                                      , title="Composición de clientes por año y sexo")


datos %>%
  group_by(PRODUCTO, ANIO.CONTRATACION, SEXO, CIUDAD) %>%
  summarize(Clientes=n()) %>%
  ggplot(aes(PRODUCTO, Clientes, fill = SEXO))+
  geom_bar(stat='identity', position = 'fill')+
  facet_wrap(~ANIO.CONTRATACION)+labs(x = "PRODUCTO"
                                      , y = "PRODUCTO PORCENTAJE"
                                      , title="COMPOSICIÓN PRODUCTO POR AÑO")



datos %>%
  group_by(PRODUCTO, ANIO.CONTRATACION,SEXO, CIUDAD, TIPO.VIVIENDA) %>%
  summarize(Clientes=n()) %>%
  ggplot(aes(PRODUCTO, Clientes, fill = TIPO.VIVIENDA))+
  geom_bar(stat='identity', position = 'fill')+
  labs(x = "PRODUCTO"
                                      , y = "TIPO DE VIVIENDA PORCENTAJE"
                                      , title="COMPOSICIÓN PRODUCTO POR PRODUCTO")


#---------------------------------------------------------
# Graficas para la determinación de la mejor sucursal y el producto más rentable

by_sucursal <- datos %>%
  group_by(SUCURSAL, CIUDAD) %>%
  summarize(Utilidad = (MONTO.CONTRATADO- SALDO.ACTUAL)/n())

by_sucursal <- as.data.frame(by_sucursal)

by_sucursal <- by_sucursal %>% top_n(10) #Nos quedamos con el top10 de las mejores sucursales


ggplot(by_sucursal, aes(x=reorder(SUCURSAL, -Utilidad), y=Utilidad, fill=CIUDAD)) + 
  geom_bar(stat="identity", position="dodge") +
  ggtitle('Utilidad de sucursales') +xlab("Sucursales") + ylab("Utilidad") 



mas_rentable <- datos2 %>%
  group_by(PRODUCTO) %>%
  summarize(Total.Utilidad = sum( (MONTO.CONTRATADO- SALDO.ACTUAL)/n()) )


ggplot(mas_rentable, aes(x=reorder(PRODUCTO, -Total.Utilidad), y=Total.Utilidad, fill=PRODUCTO)) + 
  geom_bar(stat="identity", position="dodge") +
  ggtitle('Utilidad de Producto') +xlab("PRODUCTO") + ylab("Utilidad") 

#-----------------------------------------------------------------------------------------------------
#Histograma de edad
by_edad <- datos %>%
          group_by(EDAD.CONTRATACION, PRODUCTO) %>%
          summarize(Utilidad = (MONTO.CONTRATADO- SALDO.ACTUAL)/n())

hist(x = by_edad$EDAD.CONTRATACION, main = "Histograma de edad", xlab= "Edad", ylab="Utilidad", col ="purple")









