install.packages("forecast")
install.packages("tidyquant")
install.packages("timetk")
install.packages("sweep")

library(dplyr)
library(tidyr)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)

load(file="C:/Users/Elisa/Documents/databases/project/df2000.Rda")


#NDVI monthly 

NDVI_monthly <- bike_sales %>%
  mutate(month = month(order.date, label = TRUE),
         year  = year(order.date)) %>%
  group_by(year, month) %>%
  summarise(total.qty = sum(quantity)) 
bike_sales_monthly




















####Attemp with 
#for (pp in unique(df2000$poligon.id)){
     
df.pol<-df2000%>%group_by(poligon.id, date)%>%summarise(poligon.NDVI=mean(NDVI))%>%filter(poligon.id==pp)

#plot(df.pol$date, df.pol$NDVI)
#stl(as.ts(df.pol), s.window = "periodic")

ts2000<-ts(data=df.pol$poligon.NDVI, frequency=24, start=c(18-02-2018), end=c(2017-01-17))
#plot(ts2000)
decomp<-stl(ts2000, s.window="periodic")
plot(decomp)
trend <- (decomp[["time.series"]][,2])

#axis(1, seq(from=2000, to =2017, by=1))

ts2002<-ts(data=times3$trend, frequency=15, start=c(2000, 02, 18), end=c(2017, 01,17))
decomp2<-stl(ts2002, s.window="periodic")
plot(decomp2)
