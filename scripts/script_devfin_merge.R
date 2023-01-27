library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)

base_china <- read_excel("data/base_china_final.xlsx")
b_devfin <- read_excel("data/otras/devfin_china.xlsx")

base_merge <- left_join(base_china, b_devfin, by = c("abv", "year"))

write.xlsx(base_merge, file = "data/base_merge.xlsx")
