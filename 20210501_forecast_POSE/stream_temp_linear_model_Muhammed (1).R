library(ggplot2)
library(lubridate)
library(hydroGOF)
my.df <-read.table("C:\\Users\\shikhani\\Documents\\EFI_aquatic_challenge_2021-main/Inflow_temp_air_temp_rolling_EFI.csv", sep = ",", header = T, stringsAsFactors = F)[-c(1:210),]

my.df$inflow_temp[which(my.df$inflow_temp < 0)] <- 0

my.df$doy <- lubridate::yday(my.df$date)

#my.df.train <- my.df[which(my.df$date < lubridate::ymd("2019-11-01")),]
#my.df.vald <- my.df[-which(my.df$date < lubridate::ymd("2019-11-01")),]
rmse.df <- data.frame(lm=NA)
for (j in 1:7){
  # model 1 GAUSSIAN ----
  # first attempt basic linear model

  my.df.train <- my.df[which(my.df$date < lubridate::ymd("2019-01-01")),c(1,2,j+2)]
  my.df.vald <- my.df[-which(my.df$date < lubridate::ymd("2019-01-01")),c(1,2,j+2)]
  names(my.df.train) <- c("date" ,  "inflow_temp","airtemp")
  names(my.df.vald) <- c("date" ,  "inflow_temp","airtemp")


  inflowtemp.lm <- stats::glm(inflow_temp ~ airtemp, data = my.df.train, family = gaussian(link = "identity"))
  inflowtemp.lm.pred <- predict(inflowtemp.lm,newdata = my.df.vald, type = "response")

  # check data range validation period
  range(my.df.vald$inflow_temp)
  # predicted range realistic?
  range(inflowtemp.lm.pred)


  rmse.df[j,1] <- rmse(my.df.vald$inflow_temp,inflowtemp.lm.pred)
  # plot residuals to see if there are any big problems
  # par(mfrow = c(2,3))
  # plot(inflowtemp.lm)
  # inflowtemp.lm.resid <- resid(inflowtemp.lm)
  # acf(inflowtemp.lm.resid,type = "correlation")
  # hist(inflowtemp.lm.resid)


}
rmse.df


j=2
my.df.train <- my.df[which(my.df$date < lubridate::ymd("2019-01-01")),c(1,2,j+2)]
my.df.vald <- my.df[-which(my.df$date < lubridate::ymd("2019-01-01")),c(1,2,j+2)]
names(my.df.train) <- c("date" ,  "inflow_temp","airtemp")
names(my.df.vald) <- c("date" ,  "inflow_temp","airtemp")

plot( my.df.train$airtemp,my.df.train$inflow_temp)


inflowtemp.lm <- stats::glm(inflow_temp ~ airtemp, data = my.df.train, family = gaussian(link = "identity"))
abline(inflowtemp.lm)
inflowtemp.lm.pred <- predict(inflowtemp.lm,newdata = my.df.vald, type = "response")
cor(my.df.vald$inflow_temp,my.df.vald$airtemp )
summary(inflowtemp.lm)

plot(as.Date(my.df.vald$date), my.df.vald$inflow_temp, type = "l")
lines(as.Date(my.df.vald$date),inflowtemp.lm.pred,col=2 )

summary(lm(inflow_temp ~ airtemp, data = my.df.train))
