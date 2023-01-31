
# Creación, limpieza y merge de bases de datos ####

## Librerías ####
library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)


##---------------------------------------------------------------------------##
## 1. Bases GDP y Trade ####
##---------------------------------------------------------------------------##

# Se incorporan variables: mou_bri, gdp y trade

### Importar bases ####
btrade <- read_excel("data/otras/trade.xlsx")
bgdp <- read_excel("data/otras/gdp.xlsx")

# Reemplazar países repetidos. Pongo un sólo caso. 
# Para el trabajo, habría que limpiar trade y gdp de antemano

# btrade$country[btrade$country == "Total"] <- "World"...
# write.xlsx(btrade, file = "data/otras/trade2.xlsx")


### Merge GDP y Trade ####
base_v1.5 <- full_join(bgdp, btrade) #1.5 porque la 1 era la vieja base manual
write.xlsx(base_v1.5, file = "data/versiones/base_china_v1.5.xlsx")

# Base_v1.5 fue manipulada manualmente para agregar MOU BRI desde china_pe 
# Nueva base: base_china_v2. 


### Alternativa: creación automatizada de base_china_v2 ####
#Importación y limpieza de  base china_pe
base_pe <- read_excel("data/otras/china_pe.xlsx")
base_bri <- base_pe %>% 
  select(country, mou_year, mou_bri)
base_bri$mou_year[base_bri$mou_year == "na"] <- NA

#Creo una nueva base con la lista de todos los países por cada año del mou_bri 
years <- rep(c(2013:2021),216)
countries <- sort(rep(base_pe$country, 9))
countries_year <- data.frame(cbind("country"=countries, "mou_year"=years))

#Merge de la base de bri con la nueva lista de países por año
base_bri2 <- full_join(base_bri, countries_year, by=c("country", "mou_year"))

#Cambio los NA de mou_bri por un 0 
base_bri2$mou_bri[is.na(base_bri2$mou_bri)] <- 0

#La reordenamos para que el loop avance por país y por año
#Cambio los años a tipo numérico porque están en caracter (culpa de los na)
base_bri_ordenada <- base_bri2[order(base_bri2$country, base_bri2$mou_year), ]

#Ahora vamos con el loop
x <- 0
for (i in base_bri_ordenada$country){
  x <- 0
  print(x)
  for (n in base_bri_ordenada$mou_bri){
    if (n == 1) {
      x <- 1
    }
    print(x)
  }
}

### INCOMPLETO!!!! ###


##---------------------------------------------------------------------------##
## 2. Base China Política Exterior (s/Mou BRI) ####
##---------------------------------------------------------------------------##

# Se incorporan variables region, income, partnerships_rpc, tlc_rpc y recogn_rpc

### Importar bases ####
bchina_pe <- read_excel("data/otras/china_pe.xlsx")
base_v2 <- read_excel("data/versiones/base_china_v2.xlsx") # Es versión 2 porque se manipuló manualmente

names(bchina_pe)
names(base_v2)

bchina_pe2 <- bchina_pe %>% 
  select(-mou_bri, -id, -mou_year)

### Merge China PE ####
base_v3 <- full_join(base_v2, bchina_pe2)
paisescorregidos <- data.frame(unique(base_v3$country))

write.xlsx(base_v3, file = "data/versiones/base_china_v3.xlsx")


##----------------------------------------------------------------------------##
## 3. Base UN Votes ####
##----------------------------------------------------------------------------##

# Aquí se procedió con los siguientes pasos:
# 1. Importación y segmentación de base completesVotes de AG de la UN 
# 2. Función un_conv para clasificar convergencia con China por país y sesión
# 3. Ejecución de la función, lista de conv_china y merge con base previa
# 4. Cálculo de la media por país y por año en unconv_china_mean
# 5. Adecuación y merge de unconv_china_mean por país y año (bases v.4 y v.5)


### Carga de la base de votos de UN ####
load(file = "data/otras/UNVotes.RData")
base_china_v3 <- read_excel("data/versiones/base_china_v3.xlsx") #Retoma desde la exportación de la base_v3 en script_bases

### Segmentación de sesiones y votos de China desde 2010 ####
unvotes_china2000 <- completeVotes %>% 
  filter(ccode == 710 , year == 2000) #Vamos con la prueba para 2010

### Segmentación de base principal desde 2010 ####
unvotes_total2000 <- completeVotes %>% 
  filter(ccode != 710, year == 2000) #Idem unvotes_china2010

### Función un_conv ####
#Creamos función con  doble iteración por fila de votos China y por fila de votos total
un_conv <- function(dfgeneral,dfreferencia) {
  
  count <- list()
  
  for(i in 1:nrow(dfreferencia)){
    #Itera para cada fila de la base de votos chinos
    
    for (row in 1:nrow(dfgeneral)){
      #Itera para cada fila de la base de otros votos (hay que sacar a China)
      
      if (dfreferencia[i,1] == dfgeneral[row,1]){
        #Verifica si coincide la sesión
        
        if (dfgeneral[row,4]== 2 & dfreferencia[i,4] < 4 ){
          count <- c(count,0.5) #Si el otro país se abstiene ante el voto chino, suma 0.5

        }else if (dfreferencia[i,4] > 3){
          count <- c(count,0) #Si China está ausente o no es miembro, suma 0

        }else if (dfreferencia[i,4] == dfgeneral[row,4]){
          count <- c(count,1) #Si coinciden los votos, suma 1

        }else {
          count <- c(count,0) #Si no coinciden, suma 0 

        }
      }
    }
  }
  
  return(count)
}

#Armar lista vacía y ejecutar la función sobre las bases
listaVotos <- list()
listaVotos <- un_conv(unvotes_total2001,unvotes_china2001)

#Agregar la lista con convergencia a la base (y cambiar el tipo a numerico)
unconv_china2001 <- unvotes_total2001 %>% 
  mutate(conv_china = as.numeric(listaVotos))

write_csv(x = unconv_china, file = "data/otras/unconv_china.csv")

#Acá tuve que sacar los códigos del país porque Country y Countryname están mal cargadas para 2019.
refcode <- select(unconv_china, ccode, Country, Countryname)
unique(refcode$ccode)
write.xlsx(x = refcode, file = "data/otras/refcode.xlsx")

### Creación de la media anual del UNVotes Convergence ####
#Sacar media de convergencia por país y por año y crear tabla final
#No me coinciden los nombres, lo calculo por código de país
unconv_china_mean <- with(unconv_china, 
                        aggregate(conv_china, list("ccode"=ccode, "year"=year), 
                        mean))

unique(unconv_china_mean$year)

write_csv(x = unconv_china_mean, file = "data/otras/unconv_china_mean.csv")

#Limpieza y merge con códigos by abv para poder agregar unconv_mean
base_china_v4 <- full_join(x = base_china_v3, y = refcode, by = "abv")
write.xlsx(x = base_china_v4, file = "data/versiones/base_china_v4.xlsx")

#Listo! Ahora puedo hacer el merge con UN Votes.

### Merge final, limpieza y exportación ####
base_china_v5 <- full_join(x = base_china_v4, y = unconv_china_mean, by = c("ccode", "year"))
base_china_v5 <- select(base_china_v5, -country.y)
colnames(base_china_v5)[18] <- "unconv_china"
colnames(base_china_v5)[1] <- "country"
write.xlsx(base_china_v5, file = "data/versiones/base_china_v5.xlsx")



##----------------------------------------------------------------------------##
## 4. Bases AIIB, IED y DEVFIN ####
##----------------------------------------------------------------------------##

# Se incorporan variables de proyectos, años y/o montos de aiib, ied y devfin

### Importar bases ####
base_china_v5 <- read_excel("data/versiones/base_china_v5.xlsx")
b_aiib <- read_excel("data/otras/aiib_projects.xlsx")
b_ied <- read_excel("data/otras/ied_china.xlsx")
b_devfin <- read_excel("data/otras/devfin_china.xlsx")

### Merge AIIB ####
base_china_aiib <- left_join(base_china_v5, b_aiib, by = c("abv", "year", "country"))

### Merge IED ####
base_china_ied <- left_join(base_china_aiib, b_ied, by = c("country", "year"))

### Merge DevFin ####
base_china_devfin <- left_join(base_china_ied, b_devfin, by = c("country", "year", "abv"))

### Creación de id y exportación #### 
base_china_v6 <- base_china_devfin %>% 
  mutate (id = c(1:nrow(base_china_devfin))) %>% 
  select(id, colnames(base_china_devfin)) # Esto creo que al final no lo apliqué

write.xlsx(base_china_v6, file = "data/versiones/base_china_v6.xlsx")



##---------------------------------------------------------------------------##
## 5. Alternativa DH p/Base UN Votes ####
##---------------------------------------------------------------------------##

#Retomamos desde unconv_china, ya que incluye los temas de DDHH
unconv_china <- read_csv("data/otras/unconv_china.csv")

#Filtrar votos por Derechos Humanos
topicsVotes <- data.frame(unique(unconv_china$short))

unconv_china_hr <- unconv_china %>%
  filter(str_detect(tolower(descr), "human rights")) #Usamos una función str_detect para buscar dentro de los Chr

write.xlsx(unconv_china_hr, file = "data/otras/unconv_china_hr.xlsx")

# Calculamos nuevamente las medias anuales
unconv_china_hr_mean <- with(unconv_china_hr, 
                          aggregate(conv_china, list("ccode"=ccode, "year"=year), 
                                    mean))

write.xlsx(unconv_china_hr_mean, file = "data/otras/unconv_china_hr_mean.xlsx")

# Retomamos la base_china_v6
base_china_v6 <- read_excel("data/versiones/base_china_v6.xlsx")

# Merge con los datos de unconv para DDHH y reordenamos
base_china_merge <- full_join(x = base_china_v6, y = unconv_china_hr_mean, by = c("ccode", "year"))

base_china_v7 <- base_china_merge %>% 
  select(-unconv_china, unconv_china) %>% 
  rename(unconv_china_hr = x) %>% 
  select(-unconv_china_hr, unconv_china_hr) 

# Exportamos una nueva base segmentada por tema de DDHH
write.xlsx(base_china_v7, file = "data/versiones//base_china_v7.xlsx")



# Voy a tratar de dividir las IED y DevFin por GDP
base_china_v7 <- read_excel("data/versiones/base_china_v7.xlsx")

colnames(base_china_v7)

base_china_v8 <- base_china_v7 %>% 
  mutate(ied_gdp = ied_amount / gdp) %>% 
  mutate(devfin_gdp = devfin_total / gdp)

class(base_china_v8$devfin_gdp)

write.xlsx(base_china_v8, file = "data/versiones//base_china_v8.xlsx")
