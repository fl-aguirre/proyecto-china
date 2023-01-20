library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)

#Importar la base
base_china <- read_excel("data/versiones/base_china_v8.xlsx")
names(base_china)

#Tabla para ver frecuencias mou_bri
table(base_china$mou_bri)


## Comercio ####

base1 <- base_china %>% 
  filter(gdp != "na" &
         year >= 2013 &
         trade_gdp != "na" &
         unconv_china != "na" & 
         income != "na" &
         income != "High income")

unique(base1$income)
summary(base1)

ggplot(filter(base1, trade_gdp > 0 & trade_gdp < 0.3 & unconv_china > 0), 
       aes(y = trade_gdp, x = unconv_china)) +
  geom_point() +
  geom_smooth(method = "lm")
# +
#   geom_text_repel(data = base %>%
#                     filter(trade_gdp > 0.2 & trade_gdp < 0.6 & unconv_china > 0.75),
#                     aes(y = trade_gdp, label = country))



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
