library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)

#Importar la base
base_china <- read_excel("data/base_china_v2.xlsx.xlsx")

names(base_china)

base <- base_china %>% 
  filter(gdp != "na" & mou_bri != "na" & trade_total != "na" & year >= 2013)

summary(base)

bri <- table(base$mou_bri)
bri


#COMERCIO
ggplot(filter(base, mou_bri < 2 & trade_gdp < 0.2), 
  aes(y = trade_gdp, x = mou_bri)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(data = base %>%
                    filter(mou_bri < 1 & trade_gdp > 0.15 & trade_gdp < 0.2 ),
                    aes(y = trade_gdp, label = country))

                  
                  