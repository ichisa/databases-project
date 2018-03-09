library(readr)
library(tidyr)
library(dplyr)

lotes_formosa_00_suelos <- read_csv("lotes_formosa_00_suelos.csv", 
                                    col_types = cols(`Fecha,N,4,0` = col_datetime(format = "%Y")))


lotes_formosa_05_suelos <- read_csv("lotes_formosa_05_suelos.csv", 
                                    col_types = cols(`Fecha,N,4,0` = col_datetime(format = "%Y")))


lotes_formosa_10_suelos <- read_csv("lotes_formosa_10_suelos.csv", 
                                    col_types = cols(`Fecha,N,4,0` = col_datetime(format = "%Y")))



lot00 <- lotes_formosa_00_suelos %>% dplyr::select (`IND_PROD,N,19,11`, `Hectareas,N,13,2`) %>% mutate (year.def=as.factor(2000))
colnames(lot00) <- c("prod.index", "area","year.def")
lot05 <- lotes_formosa_05_suelos %>% dplyr::select (`IND_PROD,N,19,11`, `Shape_Area,N,19,11`) %>% mutate (year.def=as.factor(2005))
colnames(lot05) <- c("prod.index", "area","year.def")
lot10 <- lotes_formosa_10_suelos %>% dplyr::select (`IND_PROD,N,19,11`, `Shape_Area,N,19,11`) %>% mutate (year.def=as.factor(2010))
colnames(lot10) <- c("prod.index", "area", "year.def")

prod.index <- rbind(lot00, lot05, lot10)


ggplot (prod.index, aes(x=year.def, y=prod.index)) + 
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


