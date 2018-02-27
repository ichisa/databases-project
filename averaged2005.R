library(readr)
X2005transposed <- read_csv("/Users/Dimitra/Documents/databases-project/Project/2005transposed.CSV", 
                            col_types = cols(X2 = col_double(), X3 = col_double(), 
                                             X4 = col_double()))
View(X2005transposed)

library(tidyverse)
library(lubridate)

df2005<-X2005transposed

df2005<-df2005%>%select(-N_PX_LOTE)

date<-as.Date(paste(df2005$X2,df2005$X3,df2005$X4, sep = "-"), "%Y-%m-%d")

df2005$date<-date

#===remove the columns that contain the year, month & day seperately
df2005<-df2005%>%select(-X2,-X3,-X4)

#===combines the columns, makes the dataframe smaller&faster
df <- gather(df2005, key="poligon", value="NDVI",-date)

#===remove _ from the ids
df$poligon.id<-ifelse(substr(df$poligon, start = 2, stop = 2)=="_", as.integer(as.numeric(as.character(substr(df$poligon, start = 1, stop = 1)))), as.integer(as.numeric(as.character(substr(df$poligon, start = 1, stop = 2)))))

#===group by date and id to get one average value of NDVI for every day
df_averaged<-df%>%group_by(poligon.id,date)%>%summarise(average=mean(NDVI))
df_averaged%>%ungroup()
plot(df_averaged$date,df_averaged$average, type="l")

#===group by year and get min and max val of NDVI for each year

df_yearly_max<-df%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(-NDVI)%>%slice(1)
df_yearly_max%>%ungroup()

df_yearly_min<-df%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(NDVI)%>%slice(1)
df_yearly_min%>%ungroup()

