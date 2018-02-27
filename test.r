#uplo
library(readr)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
EVI2000 <- read_csv("databases/proj/EVI2000copy.csv", 
                    col_types = cols(LOTE = col_skip()))
#print(EVI2000, 5)

date<-as.Date(paste(EVI2000$X2,EVI2000$X3, EVI2000$X4,sep="-"),"%Y-%m-%d") #Converts the first three columns in one column with the date 
EVI2000$date<-date
EVI2000<-subset(EVI2000, select = -c(X2,X3,X4))# erase variables that are not needed any more

df2000<-gather(EVI2000, key="poligon", value="NDVI", -date) #rearranges the df

#converts the pligon variable into a numeric variable
df2000$poligon.id<-ifelse(substr(df2000$poligon, start = 2, stop = 2)=="_", as.integer(as.numeric(as.character(substr(df2000$poligon, start = 1, stop = 1)))), as.integer(as.numeric(as.character(substr(df2000$poligon, start = 1, stop = 2)))))
df2000 <- subset(df2000, select=-poligon)



# sumarices Temp data for each hour. Makes mean of data to sumarize. 
df2000.average <- df2000 %>% 
  group_by(poligon.id, date) %>% 
  summarise(Average = mean(NDVI))%>%
  #summarize (hour_CQ = (length(hour_QC[hour_QC==TRUE])>10))%>%
  print(.,n=80) 
df2000 %>% ungroup
df2000.average %>% ungroup

pol8<-filter(df2000.average, poligon.id==8)
plot9<-filter(df2000.average, poligon.id==9)
plot(pol8)
lines()
