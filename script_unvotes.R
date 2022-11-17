library(tidyverse)
library(ggplot2)
library(dplyr)
library(openxlsx)

load(file = "data/otras/UNVotes.RData")

#Segmentación de sesiones y votos de China desde 2010
unvotes_china2010 <- completeVotes %>% 
  filter(ccode == 710 , year >= 2010) #Vamos con la prueba para 2010

#Segmentación de base principal desde 2010
unvotes_total2010 <- completeVotes %>% 
  filter(ccode != 710, year >= 2010) #Idem unvotes_china2010


#Creamos función con  doble iteración por fila de votos China y por fila de votos total
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
      }
    }
  }
  
  return(count)
}

#Armar lista vacía y ejecutar la función sobre las bases
listaVotos <- list()
listaVotos <- un_conv(unvotes_total2010,unvotes_china2010)

#Agregar la lista con convergencia a la base (y cambiar el tipo a numerico)
unconv_china <- unvotes_total2010 %>% 
  mutate(conv_china = as.numeric(listaVotos))

write_csv(x = unconv_china, file = "data/unconv_china.csv")

#Acá tuve que sacar los códigos del país porque Country y Countryname están mal cargadas para 2019.
refcode <- select(unconv_china, ccode, Country, Countryname)
unique(refcode$ccode)
write.xlsx(x = refcode, file = "data/otras/refcode.xlsx")


#Sacar media de convergencia por país y por año y crear tabla final
#No me coinciden los nombres, lo calculo por código de país
unconv_china_mean <- with(unconv_china, 
                        aggregate(conv_china, list("ccode"=ccode, "year"=year), 
                        mean))

unique(unconv_china_mean$year)

write_csv(x = unconv_china_mean, file = "data/unconv_china_mean.csv")

#Limpieza y merge con códigos by abv para poder agregar unconv_mean

base_china_v4 <- full_join(x = base_china_v3, y = refcode, by = "abv")
write.xlsx(x = base_china_v4, file = "data/versiones/base_china_v4.xlsx")

#Listo! Ahora puedo hacer el merge con UN Votes.

#Merge UNVotes Conv y limpieza
base_china_v5 <- full_join(x = base_china_v4, y = unconv_china_mean, by = c("ccode", "year"))
base_china_v5 <- select(base_china_v5, -country.y)
colnames(base_china_v5)[18] <- "unconv_china"
write.xlsx(base_china_v5, file = "data/versiones/base_china_v5.xlsx")
