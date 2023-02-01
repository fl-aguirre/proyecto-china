library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)


base_china <- read_excel("data/base_china_final_v3.xlsx")

unique(base_china$country)
colnames(base_china)

### Serie de tiempo para ver convergencia
base_china %>%
  filter(country %in% c("Argentina", "Brazil", "United States")) %>% 
  ggplot(aes(x = factor(year), y = unconv_china_hr, 
             color = country, linetype = country, group = country)) + 
  geom_line() +
  labs(x = "Year", y = "% Year", color = "", linetype = "")


### Regresión para Convergencia y otros indicadores
install.packages("ggpubr")
library(ggpubr)

regions <- unique(base_china$region)
regions

base_china %>%
  drop_na(trade_gdp) %>% 
  filter(
    ied_amount > 0,
    region %in% regions[3:5]) %>% 
  ggscatter(x = "ied_amount", y = "unconv_china_hr",
          add = "reg.line",  add.params = list(color = "black", 
                                               fill = "lightgray"), 
          conf.int = TRUE) + 
  stat_cor(method = "pearson") +
  scale_x_continuous(trans='log10')


### Modelo LM para Convergencia con IED y Comercio
modelo_ied <- lm(unconv_china_hr ~ ied_gdp_ln, data = base_china_final_v3)

summary(modelo_ied)

modelo_trade <- lm(unconv_china_hr ~ trade_gdp_ln, data = base_china_final_v3)

summary(modelo_trade)
