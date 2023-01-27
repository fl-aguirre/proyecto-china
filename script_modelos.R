library(tidyverse)

modelo_ied <- lm(unconv_china_hr ~ ied_gdp_ln, data = base_china_final_v3)

summary(modelo_ied)

modelo_trade <- lm(unconv_china_hr ~ trade_gdp_ln, data = base_china_final_v3)

summary(modelo_trade)
