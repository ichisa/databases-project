#Plots results for the trends substracted from the time series

load("C:/Users/Elisa/Documents/databases/project/2ts2005.Rda")
load("C:/Users/Elisa/Documents/databases/project/2ts2000.Rda")
load("C:/Users/Elisa/Documents/databases/project/2ts2010.Rda")

#plots the trends of different time series together

plot(ts2000$date, ts2000$trend, col="red")
points(ts2005$date, ts2005$trend)
points(ts2010$date, ts2010$trend, col="blue")

ts2000$year.def <- rep("2000", dim(ts2000)[1])
ts2005$year.def <- rep("2005", dim(ts2005)[1])
ts2010$year.def <- rep("2010", dim(ts2010)[1])

ts.all.years <- rbind(ts2000,ts2005,ts2010)

ts.all.years %>% ggplot(aes(x=date, y=trend, group=year.def))+
  geom_smooth(aes(color=year.def))+
  labs(y = "NDVI trend")+
  labs(x = "Date")+
  theme(legend.text=element_text(size=7), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

