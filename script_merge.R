library(tidyverse)
library(readxl)
library(openxlsx)

##Bases Trade y Gdp

#Importar bases
btrade <- read_excel("data/otras/trade.xlsx")
bgdp <- read_excel("data/otras/gdp.xlsx")


#Reemplazar países repetidos
# btrade$country[btrade$country == "Total"] <- "World"
# btrade$country[btrade$country == "Türkiye"] <- "Turkey"
# btrade$country[btrade$country == "Congo, Democratic Republic of the"] <- "Congo, Dem. Rep."
# btrade$country[btrade$country == "Congo"] <- "Congo, Rep."
# btrade$country[btrade$country == "Korea, Republic of"] <- "Korea, Rep."
#...

# write.xlsx(btrade, file = "otros/trade2.xlsx")


#Merge GDP y Trade
bmerge <- full_join(bgdp, btrade)
countries <- data.frame(unique(bmerge$country))

write.xlsx(bmerge, file = "data/base_merge.xlsx")

#Base_merge (trade y gdp) fue manipulada manualmente para incorporar MOU BRI por año
#Esa nueva base se llamó base_china_v2


##Base China Pol. Ext

#Importar bases
bchina_pe <- read_excel("data/otras/china_pe.xlsx")
base_v2 <- read_excel("data/otras/base_china_v2.xlsx")

names(bchina_pe)
names(base_v2)

bchina_pe2 <- bchina_pe %>% 
  select(-mou_bri, -id, -mou_year)

#Merge China PE
base_v3 <- full_join(base_v2, bchina_pe2)
paisescorregidos <- data.frame(unique(base_v3$country))

write.xlsx(base_v3, file = "data/base_china_v3.xlsx")


         