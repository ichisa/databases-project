#trend analysis from normalized data

#Required libraries
library(dplyr)
library(tidyr)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)

#Load file
load ("C:/Users/Elisa/Documents/databases/project/normData.Rda")

d2000 <- year.normData %>% filter (year.def==2000) 
d2005 <- year.normData %>% filter (year.def==2005)
d2010 <- year.normData %>% filter (year.def==2010)


###
#Choose year to analyze: complete with the year to be processed
###
year<-"2000"
###
# Puts the info of the chosen date into another called dy
assign("dy", eval(as.name(paste0("d", eval(year)))))
#=======================================
#Arranges Data per month (all data together). Monthly NDVI. 
#======================================

NDVI_monthly <- dy %>%
  mutate(month = month(Date),
         year  = year(Date)) %>%
  group_by(year, month, poligon.id) %>%
  summarise(mean.NDVI = mean(normNDVI.year)) 
NDVI_monthly <- NDVI_monthly %>%  group_by(year, month) %>%
  summarise(mean.NDVI = mean(mean.NDVI)) 

#two possible plots to visualice data
NDVI_monthly %>%
  ggplot(aes(x = month, y = mean.NDVI, group = year)) +
  geom_area(aes(fill = year), position = "stack") +
  labs(title = paste("Mean NDVI. Month plot for fields deforested", eval(year)), x = "Month", y = "NDVI") +
  scale_y_continuous() +
  theme_tq() +
  theme(legend.text=element_text(size=7), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

NDVI_monthly %>%
  ggplot(aes(x = month, y = mean.NDVI, group = year)) +
  geom_point(aes(color = year), position = "stack", size=3) +
  labs(title = paste("Mean normalized NDVI: Month Plot in", eval(year)), x = "Month", y = "NDVI") +
  scale_y_continuous() +
  theme(axis.text=element_text(size=16))+
  theme_tq()

##====================================
#NDVI monthly per group of poligon
#Timeseries analysis
##====================================
monthly_avg_by_pol <- dy %>%
  mutate(month = as_date(as.yearmon(Date))) %>%
  group_by(poligon.id, month) %>%
  summarise(mean.NDVI = mean(normNDVI.year))
monthly_avg_by_pol


monthly_avg_by_pol%>%
  ggplot(aes(x = month, y = mean.NDVI, group = poligon.id))+
  geom_line()

#Nest each time serie by group of poligon ID

monthly_avg_by_pol_nest <- monthly_avg_by_pol %>%
  group_by(poligon.id) %>%
  nest(.key = "data.tbl")
monthly_avg_by_pol_nest

#Coerse to a TS object class

monthly_avg_by_pol_ts <- monthly_avg_by_pol_nest %>%
  mutate(data.ts = map(.x       = data.tbl, 
                       .f       = tk_ts, 
                       select   = -month, 
                       start    = eval(as.numeric(year)+1), #The start is set regarding to the analized year
                       freq     = 12))
monthly_avg_by_pol_ts

#map the Error, Trend, Seasonal from stl function model

pp<-1
test<-stl(monthly_avg_by_pol_ts$data.ts[[pp]][,1], s.window = "periodic")#Runs the model for the first poligon
a1<-test$time.series[,2]
temp<-data.frame("trend"=as.numeric(a1),"poligon.id"=rep(1, length(a1)), "date"= unique(monthly_avg_by_pol$month))

#runs the model for the following poligons and creates a df
for (pp in c(2:length(monthly_avg_by_pol_ts$data.ts))){
  test<-stl(monthly_avg_by_pol_ts$data.ts[[pp]][,1], s.window = "periodic")
  a1<-test$time.series[,2]
  temp<-rbind(temp,data.frame("trend"=as.numeric(a1),"poligon.id"=rep(monthly_avg_by_pol_ts$poligon.id[[pp]], length(a1)), "date"= unique(monthly_avg_by_pol$month)))
}

#===========
#Plot results
#============
ggplot(data=temp, aes(group_by(as.character(poligon.id)))) +
  geom_line(aes(x=date, y=trend, color=as.character(poligon.id)), size=1)


#=====================================
#saves the data for the different years
#=======================================
if (year=="2000"){
  ts2000<-temp
  save (ts2000, file="2ts2000.Rda")}

if (year=="2005"){
  temp<- temp%>% filter (date>="2005-01-01")
  ts2005<-temp
  save (ts2005, file="2ts2005.Rda")}

if (year=="2010"){
  temp<- temp%>% filter (date>="2010-01-01")
  ts2010<-temp
  save (ts2010, file="2ts2010.Rda")}