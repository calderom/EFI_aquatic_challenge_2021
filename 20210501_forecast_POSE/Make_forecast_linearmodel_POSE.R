

predict_inflow_temp <- function(airtemp){
  airtemp_rollmean <-c(airtemp[1], zoo::rollmean(airtemp,2))

  temp_predict<- 2+0.76*airtemp_rollmean


  temp_predict[which(temp_predict<0)] <- 0
  return(temp_predict)
}




fcc <- read.csv("noaa-POSE-2021-05-01_daily.csv")[,1:3]

head(fcc)
fcc$inflowtemp <- NA
for (i in 1:length(unique(fcc$ensemble))) {
  fc_subset <- fcc[which(fcc$ensemble== unique(fcc$ensemble)[i]),]
  fc_subset$air_temperature <- fc_subset$air_temperature- 273.15
  fc_subset$inflowtemp <- predict_inflow_temp(fc_subset$air_temperature)
  fcc$inflowtemp[which(fcc$ensemble== unique(fcc$ensemble)[i])] <- fc_subset$inflowtemp
}
View(fcc)



# plotting

my.dates <- seq.Date(from = as.Date("2021-05-01"), to = as.Date("2021-05-08"), by = 1)
fcc.kurz <- fcc[which(as.Date(fcc$date)%in%as.Date(my.dates)),]
head(fcc.kurz)
tail(fcc.kurz)
library(ggplot2)
my.mean <- aggregate(inflowtemp ~ date, fcc.kurz, mean)
my.mean$ensemble <- "mean"
ggplot(data=fcc.kurz, aes(x=date, y=inflowtemp, group=ensemble)) + geom_line(col="grey")+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_line(data=my.mean,aes(x=date, y=inflowtemp), col="red")



df2write <- data.frame(time=fcc.kurz$date, ensemble=fcc.kurz$ensemble, siteID="POSE",temperature=fcc.kurz$inflowtemp)
head(df2write)

write.csv(df2write,"aquatics-2021-05-01-BTW.csv", quote = F, row.names = F)
