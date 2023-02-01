library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)

base_china_final_v5 <- read_excel("data/base_china_final_v5.xlsx")
unconv_china <- read_excel("data/otras/unconv_china.xlsx")

unconv_china_mean <- with(unconv_china, 
                          aggregate(conv_china, list("abv"=abv, "year"=year), 
                                    mean))

colnames(unconv_china_mean)[3] <- "unconv_china"

write.xlsx(x = unconv_china_mean, file = "data/otras/unconv_china_mean.xlsx")

base_final <- left_join(base_china_final_v4, unconv_china_mean, by=c("abv", "year"))

write.xlsx(x = base_final, file = "data/base_china_final_v5.xlsx")
