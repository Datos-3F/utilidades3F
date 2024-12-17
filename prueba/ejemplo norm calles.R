{library(tidyverse)
library(stringdist)}
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)




{calles_ejemplo <- read.csv("pruebas/direcciones_pruebas.csv")
sample <- dplyr::sample_n(calles_ejemplo, 500)
sample_3f <- calles_ejemplo %>% filter(calles_ejemplo$partido == "Tres de Febrero") %>% sample_n(500)
}

ejemplo <- normalizar_calles(sample_3f, "calle")
utilidades3F::normalizar_calles(sample)
