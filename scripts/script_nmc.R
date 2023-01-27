library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)

base_nmc <- read_delim("data/otras/nmc/NMC-60-abridged/NMC-60-abridged.csv", delim =";")

base_china <- read_excel("data/base_china_final_v2.xlsx")

base_merge <- base_nmc %>%
  rename(abv = stateabb) %>% 
  select(-version) %>% 
  right_join(base_china, by=c("abv","year"))

write.xlsx(base_merge, file = "data/base_china_final_v3.xlsx")
