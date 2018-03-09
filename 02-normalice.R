###---load required package---###

library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

#load data sets

load("C:/Users/Elisa/Documents/databases/project/df2005.Rda")
load("C:/Users/Elisa/Documents/databases/project/df2000.Rda")
load("C:/Users/Elisa/Documents/databases/project/df2010.Rda")

#changes col names
colnames(df2000) <- c("Date", "NDVI", "poligon.id", "year.def" )
colnames(df2005) <- c("Date", "NDVI", "poligon.id", "year.def" )
colnames(df2010) <- c("Date", "NDVI", "poligon.id", "year.def" )

#adds year
df2000 <- df2000 %>% mutate (year= year(Date))
df2005 <- df2005 %>% mutate (year= year(Date))
df2010 <- df2010 %>% mutate (year= year(Date))

# calculates mean for first year of deforestation
mean2000 <- df2000 %>% filter(year.def == "2000", year== 2000) %>% summarise (mm = mean(NDVI))
mean2000 <- as.data.frame(mean2000)
mean2005 <- df2005 %>% filter(year.def == "2005", year== 2005) %>% summarise (mm = mean(NDVI))
mean2005 <- as.data.frame(mean2005)
mean2010 <- df2010 %>% filter(year.def == "2010", year== 2010) %>% summarise (mm = mean(NDVI))
mean2010 <- as.data.frame(mean2010)

# normalices value to first year of deforestation
df2000$NDVI <- df2000$NDVI - mean2000$mm
df2005$NDVI <- df2005$NDVI - mean2005$mm
df2010$NDVI <- df2010$NDVI - mean2010$mm

#Discards years before deforestation
data2000 <- df2000%>% filter (Date > "2001-01-01")
data2005 <- df2005%>% filter (Date > "2006-01-01")
data2010 <- df2010%>% filter (Date > "2011-01-01")

df <- rbind(data2000, data2005, data2010)

rm(df2000, df2005, df2010, data2000, data2005, data2010)

###================================================================================
#1 normalizartion per date. In relation the the overall average of all the poligons in that date
###===============================================================================
 date.normData <- df %>%
  group_by(poligon.id, Date, year.def) %>%
  summarise(NDVI.poligon=mean(NDVI)) %>% # average per land plot since we consider. 1 obs per poligon.ID. Each plot (not each pixel) one observation
  ungroup%>% 
  group_by(Date) %>%
  mutate (avNDVI.Date = mean(NDVI.poligon)) %>% 
  ungroup %>% 
  mutate (normNDVI.date = NDVI.poligon - avNDVI.Date) #Subtract each poligon the mean

save (date.normData, file="dnorm.Rda")

 #plot  with geom smooth (Plots with every point it is too dificut to understand)
 date.normData%>% ggplot(aes(x=Date, y=normNDVI.date, group=year.def))+
   geom_smooth(aes(color=year.def), size=1.2)+
   labs(y = "daily normalized NDVI")+
   theme(legend.text=element_text(size=7), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14)) 

###========================================================== 
#2# normalization per year. Foesnt take aout the seasonal pattern. Then requires function to distinguish seasonal pattern from trend
 ##============================================================
 
 year.normData <- df %>%
   group_by(poligon.id, Date, year.def) %>%
  summarise(NDVI.poligon=mean(NDVI)) %>%
  mutate ( year= year(Date)) %>% # Creates variable year
  group_by(year) %>%
  mutate (avNDVI.year = mean(NDVI.poligon)) %>% #averages each year
  ungroup %>%
  mutate (normNDVI.year = NDVI.poligon - avNDVI.year)

 ##Save Files uuse to decompose seasonality 
 save(year.normData, file="normData.Rda") #File used for the trend - seasonality decomposition
 
#Plots the average result
#year.normData %>% ggplot(aes(x=Date, y=normNDVI.year, group=year.def))+
#  geom_smooth(aes(color=year.def), size=1.2)+
#  theme(legend.text=element_text(size=7), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

