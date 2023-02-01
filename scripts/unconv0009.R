library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)


load(file = "data/otras/UNVotes.RData")

unvotes_china_09 <- completeVotes %>% 
  filter(ccode == 710 , year == 2009)

unvotes_total_09 <- completeVotes %>% 
  filter(ccode != 710, year == 2009)


un_conv <- function(dfgeneral,dfreferencia) {
  count <- list()
  for(i in 1:nrow(dfreferencia)){
    for (row in 1:nrow(dfgeneral)){
      if (dfreferencia[i,1] == dfgeneral[row,1]){
        if (dfgeneral[row,4]== 2 & dfreferencia[i,4] < 4 ){
          count <- c(count,0.5)
        }else if (dfreferencia[i,4] > 3){
          count <- c(count,0)
        }else if (dfreferencia[i,4] == dfgeneral[row,4]){
          count <- c(count,1)
        }else {
          count <- c(count,0)
        }
      }
    }
  }
  return(count)
}


listaVotos <- list()
listaVotos <- un_conv(unvotes_total_09,unvotes_china_09)

conv_09 <- unvotes_total_09 %>% 
  mutate(conv_china = as.numeric(listaVotos))

write.xlsx(conv_09, file = "data/conv_09.xlsx")



unconv_china_2000_2009 <- merge(conv0007, conv0809, all = TRUE)

write.xlsx(unconv_china_2000_2009, file = "data/unconv_china_2000_2009.xlsx")
