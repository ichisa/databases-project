library(readr)
X2005transposed <- read_csv("M.Sc/Databases & Geovisualizations/Dorman/Project/2005transposed.CSV", 
                            col_types = cols(X2 = col_double(), X3 = col_double(), 
                                             X4 = col_double()))
View(X2005transposed)

library(tidyverse)
library(lubridate)

df2005<-X2005transposed

df2005<-df2005%>%select(-N_PX_LOTE)

date<-as.Date(paste(df2005$X2,df2005$X3,df2005$X4, sep = "-"), "%Y-%m-%d")

df2005$date<-date

df2005<-df2005%>%select(-X2,-X3,-X4)

df <- gather(df2005, key="poligon", value="NDVI",-date)

df$poligon.id<-ifelse(substr(df$poligon, start = 2, stop = 2)=="_", as.integer(as.numeric(as.character(substr(df$poligon, start = 1, stop = 1)))), as.integer(as.numeric(as.character(substr(df$poligon, start = 1, stop = 2)))))

df_averaged<-df%>%group_by(poligon.id,date)%>%summarise(average=mean(NDVI))
df_averaged%>%ungroup()
plot(df_averaged$date,df_averaged$average, type="l")

