library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)

base <- read_excel("data/base_china_00_09.xlsx")
gdp <- read_excel("data/otras/gdp.xlsx")
ied <- read_excel("data/otras/ied_china2.xlsx")
dev <- read_excel("data/otras/devfin_china.xlsx")
trade <- read_excel("data/otras/trade2.xlsx")
unconv <- read_csv("data/otras/unconv_china_mean09.csv")
nmc <- read.csv("data/otras/nmc/NMC-60-abridged/NMC-60-abridged.csv", sep = ";" )

base1 <- left_join(base, gdp, by=c("abv", "year", "country"))
base2 <- left_join(base1, ied, by=c("abv", "year", "country"))

dev$ccode = as.character(dev$ccode)

base3 <- left_join(base2, dev, by=c("abv", "year", "country", "ccode"))
base3$year <- as.character(base3$year)

base4 <- left_join(base3, trade, by=c("abv", "year", "country"))

colnames(unconv)[3] <- "unconv_china"

unconv$ccode <- as.character(unconv$ccode)
unconv$year <- as.character(unconv$year)

base5 <- left_join(base4, unconv, by=c("ccode", "year"))
base6 <- left_join(base5, nmc, by=c("abv", "year", "ccode"))

nmc$ccode <- as.character(nmc$ccode)

base6 <- select(base6, -version)

write.xlsx(base6, file = "data/base_china_00_09.xlsx")


colnames(NMC_60_abridged)[1] <- "abv"

base_final <- left_join(base_china_final_v4, NMC_60_abridged, by=c("abv", "year", "ccode"))

write.xlsx(base_final, file = "data/base_china_final_v4.5.xlsx")
