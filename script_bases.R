### Creación, limpieza y merge de bases de datos ##############################

## Librerías

library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)


## Bases GDP y Trade ##########################################################

# Importar bases
btrade <- read_excel("data/otras/trade.xlsx")
bgdp <- read_excel("data/otras/gdp.xlsx")

# Reemplazar países repetidos. Pongo un sólo caso. 
# Para el trabajo, habría que limpiar trade y gdp de antemano

# btrade$country[btrade$country == "Total"] <- "World"...
# write.xlsx(btrade, file = "data/otras/trade2.xlsx")

# Merge GDP y Trade
base_v1.5 <- full_join(bgdp, btrade) #1.5 porque la 1 era la vieja base manual

write.xlsx(base_v1.5, file = "data/versiones/base_china_v1.5.xlsx")

# Base_v1.5 fue manipulada manualmente para agregar MOU BRI desde china_pe 
# Nueva base: base_china_v2. Voy a tratar de automatizarlo a continuación.

#Importo, segmento y limpio la base china_pe
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
#Camio los años a tipo numérico porque están en caracter (culpa de los na)
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


## Base China Politica Exterior ###############################################

# Importar bases
bchina_pe <- read_excel("data/otras/china_pe.xlsx")
base_v2 <- read_excel("data/versiones/base_china_v2.xlsx") # Es versión 2 porque se manipuló manualmente

names(bchina_pe)
names(base_v2)

bchina_pe2 <- bchina_pe %>% 
  select(-mou_bri, -id, -mou_year)

# Merge China PE
base_v3 <- full_join(base_v2, bchina_pe2)
paisescorregidos <- data.frame(unique(base_v3$country))

write.xlsx(base_v3, file = "data/versiones/base_china_v3.xlsx")


## Base UN Votes ##############################################################

# Remitirse a script_unvotes para ver:
# 1. Importación y segmentación de base completesVotes de AG de la UN 
# 2. Función un_conv para clasificar convergencia con China por país y sesión
# 3. Ejecución de la función, lista de conv_china y merge con base previa
# 4. Cálculo de la media por país y por año en unconv_china_mean
# 5. Adecuación y merge de unconv_china_mean por país y año (bases v.4 y v.5)


## Bases AIIB, IED y DEVFIN ###################################################

# Importar las bases
base_china_v5 <- read_excel("data/versiones/base_china_v5.xlsx")
b_aiib <- read_excel("data/otras/aiib_projects.xlsx")
b_ied <- read_excel("data/otras/ied_china.xlsx")
b_devfin <- read_excel("data/otras/devfin_china.xlsx")

# Merge AIIB
base_china_aiib <- left_join(base_china_v5, b_aiib, by = c("abv", "year", "ccode", "country"))

# Merge IED
base_china_ied <- left_join(base_china_aiib, b_ied, by = c("country", "year"))

# Merge DEVFIN
base_china_devfin <- left_join(base_china_ied, b_devfin, by = c("country", "year", "abv", "ccode"))

# Creación de id y exportación
base_china_v6 <- base_china_devfin %>% 
  mutate (id = c(1:nrow(base_china_devfin))) %>% 
  select(id, colnames(base_china_devfin))
  
write.xlsx(base_china_v6, file = "data/base_china_v6.xlsx")
