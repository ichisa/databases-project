#uplo
library(readr)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

##############################
#========loads 2000==========#
##############################

EVI2000 <- read_csv("C:/Users/Elisa/Documents/databases/project/EVI2000R.csv", 
                    col_types = cols(LOTE = col_skip()))

#Converts the first three columns in one column with the date 
date<-as.Date(paste(EVI2000$X1,EVI2000$X2, EVI2000$X3,sep="-"),"%Y-%m-%d") 
EVI2000$date<-date
# erase variables that are not needed any more
EVI2000<-subset(EVI2000, select = -c(X1,X2,X3))
#rearranges the df
df2000<-gather(EVI2000, key="poligon", value="NDVI", -date)
#re-arranges data frames in three colums: Date-Polygon-Value
#converts the pligon variable into a numeric variable
df2000$poligon.id<-ifelse(substr(df2000$poligon, start = 2, stop = 2)=="_", as.integer(as.numeric(as.character(substr(df2000$poligon, start = 1, stop = 1)))), as.integer(as.numeric(as.character(substr(df2000$poligon, start = 1, stop = 2)))))
#removes unecessary columns
df2000 <- subset(df2000, select=-poligon)

##############################
#========loads 2005==========#
##############################

EVI2005 <- read_csv("C:/Users/Elisa/Documents/databases/project/EVI2005R.csv", 
                    col_types = cols(LOTE = col_skip()))

#Converts the first three columns in one column with the date 
date<-as.Date(paste(EVI2005$X1,EVI2005$X2, EVI2005$X3,sep="-"),"%Y-%m-%d")
EVI2005$date<-date
# erase variables that are not needed any more
EVI2005<-subset(EVI2005, select = -c(X1,X2,X3))
df2005<-gather(EVI2005, key="poligon", value="NDVI", -date) #rearranges the df
#re-arranges data frames in three colums: Date-Polygon-Value
#converts the pligon variable into a numeric variable
df2005$poligon.id<-ifelse(substr(df2005$poligon, start = 2, stop = 2)=="_", as.character(substr(df2005$poligon, start = 1, stop = 1)), as.character(substr(df2005$poligon, start = 1, stop = 2)))
#removes the info of the poligon as a character
df2005 <- subset(df2005, select=-poligon)



##############################
#========loads 2010==========#
##############################


#loads 2010_1
###########
EVI2010_1 <- read_csv("C:/Users/Elisa/Documents/databases/project/EVI2010_1R.csv", 
                    col_types = cols(LOTE = col_skip()))

date<-as.Date(paste(EVI2010_1R$X1,EVI2010_1R$X2, EVI2010_1R$X3,sep="-"),"%Y-%m-%d") #Converts the first three columns in one column with the date 
EVI2010_1$date<-date
EVI2010_1<-subset(EVI2010_1, select = -c(X1,X2,X3))# erase variables that are not needed any more

df2010_1<-gather(EVI2010_1, key="poligon", value="NDVI", -date) #rearranges the df


#re-arranges data frames in three colums: Date-Polygon-Value
#converts the pligon variable into a numeric variable
df2010_1$poligon.id<-ifelse(substr(df2010_1$poligon, start = 2, stop = 2)=="_", as.character(substr(df2010_1$poligon, start = 1, stop = 1)), as.character(substr(df2010_1$poligon, start = 1, stop = 2)))


df2010_1<- subset(df2005, select=-poligon)#removes the info of the poligon as a character


#loads 2010_2
###########
EVI2010_2 <- read_csv("C:/Users/Elisa/Documents/databases/project/EVI2010_2R.csv", 
                      col_types = cols(LOTE = col_skip()))

date<-as.Date(paste(EVI2010_2$X1,EVI2010_2$X2, EVI2010_2$X3,sep="-"),"%Y-%m-%d") #Converts the first three columns in one column with the date 
EVI2010_2$date<-date
EVI2010_2<-subset(EVI2010_2, select = -c(X1,X2,X3))# erase variables that are not needed any more

df2010_2<-gather(EVI2010_2, key="poligon", value="NDVI", -date) #rearranges the df


#re-arranges data frames in three colums: Date-Polygon-Value
#converts the pligon variable into a numeric variable
df2010_2$poligon.id<-ifelse(substr(df2010_2$poligon, start = 4, stop = 4)=="_", as.character(substr(df2010_2$poligon, start = 1, stop = 3)), as.character(substr(df2010_2$poligon, start = 1, stop = 2)))

df2010_2$poligon.id<-ifelse(substr(df2010_2$poligon, start = 2, stop = 2)=="_", as.character(substr(df2010_2$poligon, start = 1, stop = 1)), df2010_2$poligon.id)


df2010_1<- subset(df2010_1, select=-poligon)#removes the info of the poligon as a character

##combines data frames
df2010<-rbind(df2010_1, df2010_2)
df2000$year.deforest<-rep("2000", dim(df2000)[1])
df2005$year.deforest<-rep("2005", dim(df2005)[1])
df2010$year.deforest<-rep("2010", dim(df2010)[1])

dfNDVI<-rbind(df2000, df2005, df2010)

##saves data frames

save(df2000, file="df2000.Rda")
save(df2005, file="df2005.Rda")
save(df2010, file="df2010.Rda")
save(df2000, file="dfNDVI.Rda")




# sumarices Temp data for each hour. Makes mean of data to sumarize. 
df2000.average <- df2000 %>% 
  group_by(poligon.id, date) %>% 
  summarise(Average = mean(NDVI))%>%
  #summarize (hour_CQ = (length(hour_QC[hour_QC==TRUE])>10))%>%
  print(.,n=80) 
df2000 %>% ungroup
df2000.average %>% ungroup

pol8<-filter(df2000.average, poligon.id==8)

pol9<-filter(df2000.average, poligon.id==9)
plot(pol8$date, pol8$Average, type="l")
lines(pol9$date, plot9$Average)


