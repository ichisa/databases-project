load(df2000.)


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


