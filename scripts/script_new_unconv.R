library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)


unga_hr <- read_excel("data/otras/nueva_unconv/unconv_china_hr2.xlsx")

unga_hr_mean <- with(unga_hr, 
                             aggregate(conv_china, list("abv"=Country, "year"=year), 
                                       mean))

write.xlsx(unga_hr_mean, file = "data/otras/nueva_unconv/unconv_china_hr_mean2.xlsx")


base_china_final <- read_excel("data/base_china_final.xlsx")

# Merge con los datos de unconv para DDHH y reordenamos
base_china_merge <- left_join(x = base_china_final, y = unga_hr_mean, by = c("abv", "year"))

base_china_final_v2 <- base_china_merge %>% 
  select(-unconv_china_hr) %>% 
  rename(unconv_china_hr = x)

base_china_final_v2  <- base_china_final_v2 %>% 
  mutate(trade_total = as.numeric(trade_total)) %>% 
  mutate(gdp = as.numeric(gdp)) %>% 
  mutate(expo = as.numeric(expo)) %>% 
  mutate(impo = as.numeric(impo)) %>% 
  mutate(balance = as.numeric(balance)) %>% 
  mutate(trade_gdp = as.numeric(trade_gdp)) %>% 
  mutate(unconv_china = as.numeric(unconv_china)) %>% 
  mutate(amount_aiib = as.numeric(amount_aiib)) %>% 
  mutate(bri_project = as.numeric(bri_project))

class(base_china_final_v2$bri_project)

names(base_china_final_v2)
 
# Exportamos una nueva base segmentada por tema de DDHH
write.xlsx(base_china_final_v2, file = "data/base_china_final_v2.xlsx")
