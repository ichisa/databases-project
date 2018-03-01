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
df_averaged2005<-df%>%group_by(poligon.id,date)%>%summarise(average=mean(NDVI))
df_averaged2005%>%ungroup()
plot(df_averaged2005$date,df_averaged2005$average, type="l")

#===group by year and get min and max val of NDVI for each year

df_yearly_max2005<-df%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(-NDVI)%>%slice(1)
df_yearly_max2005%>%ungroup()%>%select(date,year,poligon.id,NDVI)

df_yearly_min2005<-df%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(NDVI)%>%slice(1)
df_yearly_min2005%>%ungroup()%>%select(date,year,poligon.id,NDVI)

#==================================================================
# AVERAGE, MIN & MAX VALUES FOR YEAR 2000
#==================================================================

#AVERAGE
df_averaged2000<-df2000%>%group_by(poligon.id,date)%>%summarise(average=mean(NDVI))
df_averaged2000%>%ungroup()

#MAX
df_yearly_max2000<-df2000%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(-NDVI)%>%slice(1)
df_yearly_max2000%>%ungroup()%>%select(date,year,poligon.id,NDVI)

#MIN
df_yearly_min2000<-df2000%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(NDVI)%>%slice(1)
df_yearly_min2000%>%ungroup()%>%select(date,year,poligon.id,NDVI)

#==================================================================
# AVERAGE, MIN & MAX VALUES FOR YEAR 2010
#==================================================================

#AVERAGE
df_averaged2010<-df2010%>%group_by(poligon.id,date)%>%summarise(average=mean(NDVI))
df_averaged2010%>%ungroup()

#MAX
df_yearly_max2010<-df2010%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(-NDVI)%>%slice(1)
df_yearly_max2010%>%ungroup()%>%select(date,year,poligon.id,NDVI)

#MIN
df_yearly_min2010<-df2010%>%mutate(year=year(date))%>%group_by(poligon.id,year)%>%arrange(NDVI)%>%slice(1)
df_yearly_min2010%>%ungroup()%>%select(date,year,poligon.id,NDVI)

#============================================================================
#PLOTS: AVERAGE NDVIS FOR EVERY DATE (MIN VALUES) FOR YEARS 2000, 2005, 2010
#============================================================================
test1<-df_yearly_min2000%>%group_by(date)%>%summarise(averageperdate=mean(NDVI))%>%ungroup()
test2<-df_yearly_min2005%>%group_by(date)%>%summarise(averageperdate=mean(NDVI))%>%ungroup()
test3<-df_yearly_min2010%>%group_by(date)%>%summarise(averageperdate=mean(NDVI))%>%ungroup()
plot(test1,type="l", col="red")
lines(test2,col="blue")
lines(test3,col="green")


#============================================================================
#PLOTS: AVERAGE NDVIS FOR EVERY DATE (MAX VALUES) FOR YEARS 2000, 2005, 2010
#============================================================================
#test4<-df_yearly_max2000%>%group_by(date)%>%summarise(averageperdate=mean(NDVI))%>%ungroup()
#test5<-df_yearly_max2005%>%group_by(date)%>%summarise(averageperdate=mean(NDVI))%>%ungroup()
#test6<-df_yearly_max2010%>%group_by(date)%>%summarise(averageperdate=mean(NDVI))%>%ungroup()
#plot(test4,type="l", col="red")
#lines(test5,col="blue")
#lines(test6,col="green")


#============================================================================
#AVERAGED DATAFRAMES FIRST BY POLYGON ID AND THEN BY YEAR
#============================================================================
#Averaging maximum values for year 2000
dfpol<-df_yearly_max2000%>%group_by(poligon.id,year)%>%summarise(maximum=(max(NDVI)))%>%group_by(year)%>%summarise(maxpol=mean(maximum))%>%ungroup()

#Averaging maximum values for year 2005
dfpol1<-df_yearly_max2005%>%group_by(poligon.id,year)%>%summarise(maximum=(max(NDVI)))%>%group_by(year)%>%summarise(maxpol=mean(maximum))%>%ungroup()

#Averaging maximum values for year 2010
dfpol2<-df_yearly_max2010%>%group_by(poligon.id,year)%>%summarise(maximum=(max(NDVI)))%>%group_by(year)%>%summarise(maxpol=mean(maximum))%>%ungroup()

plot(dfpol,type="l",col="red",main="MAXIMUM VALUES")
lines(dfpol1,col="blue")
lines(dfpol2,col="black")

#Averaging minimum values for year 2000
dfpolm<-df_yearly_min2000%>%group_by(poligon.id,year)%>%summarise(minimum=(min(NDVI)))%>%group_by(year)%>%summarise(minpol=mean(minimum))%>%ungroup()

#Averaging minimum values for year 2005
dfpolm1<-df_yearly_min2005%>%group_by(poligon.id,year)%>%summarise(minimum=(min(NDVI)))%>%group_by(year)%>%summarise(minpol=mean(minimum))%>%ungroup()

#Averaging minimum values for year 2010
dfpolm2<-df_yearly_min2010%>%group_by(poligon.id,year)%>%summarise(minimum=(min(NDVI)))%>%group_by(year)%>%summarise(minpol=mean(minimum))%>%ungroup()

plot(dfpolm,type="l",col="red",main="MINIMUM VALUES")
lines(dfpolm1,col="blue")
lines(dfpolm2,col="black")

#Average values for year 2000
dfpola<-df_averaged2000%>%group_by(poligon.id,date)%>%summarise(average=(mean(average)))%>%group_by(date)%>%summarise(avepol=mean(average))
dfpola$date=year(date)

#Averaging minimum values for year 2005
dfpolm1<-df_yearly_min2005%>%group_by(poligon.id,year)%>%summarise(minimum=(min(NDVI)))%>%group_by(year)%>%summarise(minpol=mean(minimum))%>%ungroup()

#Averaging minimum values for year 2010
dfpolm2<-df_yearly_min2010%>%group_by(poligon.id,year)%>%summarise(minimum=(min(NDVI)))%>%group_by(year)%>%summarise(minpol=mean(minimum))%>%ungroup()

plot(dfpolm,type="l",col="red",main="MINIMUM VALUES")
lines(dfpolm1,col="blue")
lines(dfpolm2,col="black")

