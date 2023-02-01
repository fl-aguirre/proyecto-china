library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)

#Importar la base
base_china <- read_excel("data/base_china_final.xlsx")
warnings()
names(base_china)

class(base1$unconv_china)

table(base_china$mou_bri) #Tabla para ver frecuencias mou_bri

base_china1 <- base_china %>% 
  mutate(unconv_china = as.numeric(unconv_china)) %>% 
  mutate(unconv_china_hr = as.numeric(unconv_china_hr))

class(base_china1$unconv_china)


## Filtrado de la base ####

base1 <- base_china1 %>% 
  filter(ied_amount != "na")

names(base1)
unique(base1$income)
summary(base1)

#Gráfico

ggplot(filter(base1, ied_gdp < 1), 
       aes(y = ied_gdp, x = partnerships_rpc)) +
  geom_point() +
  geom_smooth(method = "lm")


## IED ####

base2 <- base_china %>% 
  filter(gdp != "na" &
         year >= 2013 &
         ied_gdp != "na" &
         unconv_china != "na" &
         income != "na" &
         income != "High income")

unique(base2$year)
summary(base2)

ggplot(filter(base, ied_gdp > 0 & ied_gdp < 0.3 & unconv_china > 0), 
       aes(y = ied_gdp, x = unconv_china)) +
  geom_point() +
  geom_smooth(method = "lm")
# + 
#   geom_text_repel(data = base %>%
#                     filter(ied_gdp > 0 & ied_gdp < 1 & unconv_china > 0),
#                   aes(y = ied_gdp, label = country))


## DevFin ####

base3 <- base_china %>% 
  filter(gdp != "na" &
         year >= 2013 &
         devfin_gdp != "na" &
         unconv_china != "na" &
         income != "na" &
         income != "High income")

unique(base3$income)
summary(base3)

ggplot(filter(base, devfin_gdp > 0 & devfin_gdp < 0.3 & unconv_china > 0), 
       aes(y = devfin_gdp, x = unconv_china)) +
  geom_point() +
  geom_smooth(method = "lm") 
# +
#   geom_text_repel(data = base %>%
#                     filter(devfin_gdp > 0 & devfin_gdp < 1 & unconv_china > 0),
#                   aes(y = devfin_gdp, label = country))
