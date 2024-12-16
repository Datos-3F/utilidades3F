
library(tidyverse)
cavs <- utilidades3F::obtener_capa("CAVs")

cavs1row <- cavs[1,]

cavs1row$lat <- strsplit(cavs1row$coordenadas,", ")[[1]][1]

cavs1row$lon <- strsplit(cavs1row$coordenadas,", ")[[1]][2]


#unlist in two variables 

cavs1row$lat <- cavs1row$pruebas[1]
cavs1row$lon <- cavs1row$pruebas[2]


obtener_direccion_url(lat = cavs1row$lat, lon = cavs1row$lon)
