library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)

#Importar la base
base_china_v1 <- read_excel("data/otras/base_china_v1.xlsx")
View(base_china_v1)

colnames(base_china_v1)

#Seleccionar las variables en nueva base y limpiar GDP
base <- base_china_v1 %>% 
  select(id, 
         abv, 
         country,
         partnerships_rpc,
         mou_bri, 
         gdp,
         trade_rpc_2019,
         tradercp_gdp, 
         investtotal_gdp,
         devfinance_gdp) %>% 
  filter(gdp != "na")


bri <- table(base$mou_bri)
bri


#COMERCIO
##Base comercial
base_trade_bri <- base %>%
  filter(tradercp_gdp != "na") %>%
  mutate(trade_convert = as.numeric(tradercp_gdp)) %>%
  filter(trade_convert < 0.1)

class(base_trade_bri$trade_convert)
summary(base_trade_bri$trade_convert)
  

##Grafico base comercial
ggplot(base_trade_bri, aes(y = trade_convert, x = mou_bri)) +
  geom_point() +
  geom_smooth(method = "lm")
  
  # + geom_text_repel(data = base_trade_bri %>%
  #                   filter(trade_convert < 0.01),
  #                   aes(y = trade_convert, label=country))


#INVERSION
##Base inversión
base_invest_bri <- base %>%
  filter(investtotal_gdp != "na" & investtotal_gdp < 0.1)

class(base_invest_bri$investtotal_gdp)
summary(base_invest_bri$investtotal_gdp)

##Gráfico base inversión
ggplot(base_invest_bri, aes(x=mou_bri, y = investtotal_gdp)) +
  geom_point() +
  geom_smooth(method = "lm")
  
  # + geom_text_repel(data = base_invest_bri %>%
  #                   filter(investtotal_gdp > 0.05),
  #                   aes(y= investtotal_gdp, label=country))



#FINANCIAMIENTO AL DESARROLLO
##Base inversión
base_dev_bri <- base %>%
  filter(devfinance_gdp != "na" & devfinance_gdp < 0.5)

class(base_dev_bri$devfinance_gdp)
summary(base_dev_bri$devfinance_gdp)

##Gráfico base inversión
ggplot(base_dev_bri, aes(x=mou_bri, y = devfinance_gdp)) +
  geom_point() +
  geom_smooth(method = "lm")
  
  + geom_text_repel(data = base_dev_bri %>%
                  filter(mou_bri == 0 & devfinance_gdp > 0.1),
                  aes(y = devfinance_gdp, label=country))
    