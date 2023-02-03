library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)


load(file = "data/otras/UNVotes.RData")

colnames(completeVotes)

unvotes_china_00_09 <- completeVotes %>% 
  filter(ccode == 710 , year >= 2000, year <= 2009) %>% 
  arrange(rcid)

unvotes_total_00_09 <- completeVotes %>% 
  filter(ccode != 710, year >= 2000, year <= 2009) %>% 
  arrange(rcid)



un_conv <- function(dfgeneral,dfreferencia) {
  
  count <- list()
  
  for(i in 1:nrow(dfreferencia)){
    #Itera para cada fila de la base de votos chinos
    
    for (row in 1:nrow(dfgeneral)){
      #Itera para cada fila de la base de otros votos (hay que sacar a China)
      
      if (dfreferencia[i,1] == dfgeneral[row,1]){
        #Verifica si coincide la sesión
        
        if (dfgeneral[row,4]== 2 & dfreferencia[i,4] < 4 ){
          count <- c(count,0.5) #Si el otro país se abstiene ante el voto chino, suma 0.5
          
        }else if (dfreferencia[i,4] > 3){
          count <- c(count,0) #Si China está ausente o no es miembro, suma 0
          
        }else if (dfreferencia[i,4] == dfgeneral[row,4]){
          count <- c(count,1) #Si coinciden los votos, suma 1
          
        }else {
          count <- c(count,0) #Si no coinciden, suma 0 
          
        }
      }else{next}
    }
  }
  
  return(count)
}


listaVotos <- list()
listaVotos <- un_conv(unvotes_total_00_09,unvotes_china_00_09)

conv_00_09 <- unvotes_total_00_09 %>% 
  mutate(conv_china = as.numeric(listaVotos))

write.xlsx(conv_00_09, file = "data/conv_00_09.xlsx")

class(conv_10_21$conv_china)

unconv_china <- merge(conv_00_09, conv_10_21, all = TRUE)

write.xlsx(unconv_china, file = "data/unconv_china.xlsx")



####CAMBIAR COUNTRY POR ABV EN LAS VARIABLES!!!

###UNCONV MEAN

unconv_china <- read_excel("data/unconv_china.xlsx")
base_china <- read_excel("data/base_china_final_v5.xlsx")

unconv_china_mean <- with(unconv_china, 
                          aggregate(conv_china, list("ccode"=ccode, "year"=year), 
                                    mean))

colnames(unconv_china_mean)[3] <- "unconv_china"

write.xlsx(x = unconv_china_mean, file = "data/unconv_china_mean.xlsx")

base_final <- left_join(base_china, unconv_china_mean, by=c("ccode", "year"))

write.xlsx(x = base_final, file = "data/base_china_final_v6.xlsx")



### VARIANTE DERECHOS HUMANOS

unconv_china_HR <- read_excel("data/otras/unconv_china_HR.xlsx")
base_china <- read_excel("data/base_china_final_v6.xlsx")

unconv_china_HR_mean <- with(unconv_china_HR, 
                          aggregate(conv_china, list("ccode"=ccode, "year"=year), 
                                    mean))

colnames(unconv_china_HR_mean)[3] <- "unconv_china_HR"

write.xlsx(x = unconv_china_HR_mean, file = "data/unconv_china_HR_mean.xlsx")

base_final <- left_join(base_china, unconv_china_HR_mean, by=c("ccode", "year"))

write.xlsx(x = base_final, file = "data/base_china_final_v6.5.xlsx")

