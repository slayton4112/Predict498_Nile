setwd("C:/Users/Greg/Desktop/Classes/NU/Capstone")
library(forecast)
library(dplyr)
library(tseries)
library(hts)
library(reshape2)

load("C:/Users/Greg/Desktop/Classes/NU/Capstone/Predict498_Nile/nile_data.RData")

response.var <- "SL.GDP.PCAP.EM.KD"
num.yrs <- 25
start.yr <- 1991
end.yr <- 2015

### Create df for naive/drift/mean models
##### 

response.df <- gdp[,c("iso2c", "country", "country.region", "year", response.var)]
unique(response.df$country)
response.df$missing = is.na(response.df[,response.var])*1

# Group by country and sum missing GDP
num.missing = response.df %>%
  group_by(country) %>%
  summarise(num_miss = sum(missing)) %>%
  arrange(desc(num_miss))

# Filter on countries missing GDP data
nonmiss <- filter(num.missing, num_miss == 0)
miss <- filter(num.missing, num_miss > 0)

# Drop the countries that are missing GDP data
response.df.dropna <- response.df[!(response.df$country %in% miss$country),]

### Create df for auto.arima (copy from Scott's code)
#####
filt_gdp = subset(gdp, !(country %in% miss$country))
arm_dta = filt_gdp[,c('country', 'year', response.var)]
order_arm_dta = arm_dta[order(arm_dta$country, arm_dta$year),]
arima.full.df = dcast(order_arm_dta, year ~ country, value.var=response.var)

names(arima.full.df)

arima.abbrev.df <- arima.full.df[arima.full.df$year < 2013,]
horizon <- 3

#####
# Create country list we can use to determine which countries will get predictions
country.list <- as.vector(nonmiss[order(nonmiss$country), "country"]$country)

accuracy.df <- data.frame(country     = character(),
#                           naive.oos.MAE    = numeric(),
#                           naive.oos.RMSE   = numeric(),
                          naive.oos.MAPE   = numeric(),
#                           drift.oos.MAE    = numeric(),
#                           drift.oos.RMSE   = numeric(),
                          drift.oos.MAPE   = numeric(),
#                           mean.oos.MAE    = numeric(),
#                           mean.oos.RMSE   = numeric(),
                          mean.oos.MAPE   = numeric(),
## Incl. arima OOS when I figure out arima models
#                           arima.oos.MAE   = numeric(),
#                           arima.oos.RMSE  = numeric(),
                          arima.oos.MAPE  = numeric()
)


## Create accuracy metrics for baseline models
#####
for (country in 1:length(country.list)) {
  country.name <- country.list[country]
  print(country.name)  
  country.df <- response.df.dropna[response.df.dropna$country == country.name,]
  country.df <- country.df[order(country.df$year),]
  actual.response <- as.data.frame(country.df[country.df$year >= 2013, response.var])
  colnames(actual.response) <- "actual"
  
# Check to make sure that all values aren't the same for response.var, implying messy data for the response
  if (length(unique(country.df[,response.var])) > 1) {
    # Separate in to abbreviated ts which we'll produce forecast off of
    country.ts.abbrev <- ts(country.df[country.df$year < 2013, response.var], start=c(1991,1), end=c(2012,1), frequency = 1)
    # Use full ts to determine the OOS error
    country.ts.full <- ts(country.df[, response.var], start=c(1991,1), end=c(2015,1), frequency = 1) 
    
    #Fit an arima to each country
    arima.fit <- auto.arima(arima.abbrev.df[,country.name])
    forecasted.response <- as.data.frame(forecast(arima.fit, h=3)$mean)
    colnames(forecasted.response) <- "forecasted"
    country.arima.df <- cbind(actual.response, forecasted.response)
    country.arima.df$error <- country.arima.df[,"actual"] - country.arima.df[,"forecasted"]
    country.arima.df$perc.error <- country.arima.df$error / country.arima.df$actual
    
    # populate df of errors based on various accuracy metrics - MAE, MAPE, RMSE
#     naive.oos.MAE    <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAE"]
#     naive.oos.RMSE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "RMSE"]
    naive.oos.MAPE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
#     drift.oos.MAE    <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAE"]
#     drift.oos.RMSE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "RMSE"]
    drift.oos.MAPE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAPE"]
#     mean.oos.MAE    <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAE"]
#     mean.oos.RMSE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "RMSE"]
    mean.oos.MAPE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
#     arima.oos.MAE <- mean(abs(country.arima.df[,"error"]))
#     arima.oos.RMSE <- sqrt(mean(as.vector(country.arima.df[,"error"])^2))
    arima.oos.MAPE <- mean(abs(country.arima.df[,"perc.error"])) * 100

    accuracy.df <- rbind(accuracy.df, data.frame(country     = country.name,
#                                                  naive.oos.MAE    = naive.oos.MAE ,
#                                                  naive.oos.RMSE   = naive.oos.RMSE,
                                                 naive.oos.MAPE   = naive.oos.MAPE,
#                                                  drift.oos.MAE    = drift.oos.MAE,
#                                                  drift.oos.RMSE   = drift.oos.RMSE,
                                                 drift.oos.MAPE   = drift.oos.MAPE,
#                                                  mean.oos.MAE     = mean.oos.MAE,
#                                                  mean.oos.RMSE    = mean.oos.RMSE,
                                                 mean.oos.MAPE    = mean.oos.MAPE,
#                                                  arima.oos.MAE    = arima.oos.MAE,
#                                                  arima.oos.RMSE   = arima.oos.RMSE,
                                                 arima.oos.MAPE   = arima.oos.MAPE))
  }
}

metrics.to.compare <- c("naive.oos.MAPE", "drift.oos.MAPE", "mean.oos.MAPE", "arima.oos.MAPE")
best <- apply(accuracy.df, MARGIN = 1, FUN=function(x) metrics.to.compare[which.min(x[metrics.to.compare])])
table(best)

#arima.oos.MAPE drift.oos.MAPE  mean.oos.MAPE naive.oos.MAPE 
#40             86              9             39 

#####

#####
# # Test on US to make sure loop is running as expected
# regression.accuracy.df <- data.frame(country    = character(), stringsAsFactors=FALSE)
# 
# country.name <- "United States"
# regression.accuracy.df[1, 1] <- country.name
# country.df <- as.data.frame(gdp[gdp$country == country.name,])
# country.df <- country.df[order(country.df$year),]
# response.ts.abbrev <- ts(country.df[country.df$year < end.yr-2, response.var], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
# 
# response.df.newdata <- country.df[,c("year", response.var)]
# response.df.newdata <- subset(response.df.newdata, year == 2013 | year == 2014 | year == 2015)
# response.df.newdata <- response.df.newdata[order(response.df.newdata$year),]
# 
# predictor <- 815
# 
# if (sum(is.na(country.df[,predictor])) == 0 && (!colnames(country.df)[predictor] %in% c("year","country.region", "iso2c", "country"))) {
#   pred.ind <- colnames(country.df)[predictor]
#   pred.var <- unlist(series_ref[series_ref$series.indicator == pred.ind, "series.name"]) 
# 
#   country.trimmed.df <- country.df[,c("iso2c", "country", "year", response.var, pred.ind)]
#   country.trimmed.df <- country.trimmed.df[order(country.trimmed.df$year),]
#   
#   pred.subset <- subset(country.trimmed.df, year == 2013 | year == 2014 | year == 2015)
#   pred.newdata <- ts(pred.subset[,pred.ind], start=c(end.yr-2,1), end=c(end.yr,1), frequency=1)
#   
#   pred.ts.abbrev <- ts(country.trimmed.df[country.trimmed.df$year < end.yr-2, pred.ind], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
#   intersect <- ts.intersect(response.ts.abbrev, pred.ts.abbrev)  
#   pred.tslm <- tslm(response.ts.abbrev ~ trend + pred.ts.abbrev, intersect)
#   
#   fcast <- as.data.frame(forecast(pred.tslm, newdata=data.frame(trend = c(23,24,25) ,pred.ts.abbrev = pred.newdata))$mean)
#   colnames(fcast) <- pred.ind
#   
#   pred.temp.df <- cbind(response.df.newdata, fcast)
#   pred.temp.df$perc.error <- (pred.temp.df[,response.var] - pred.temp.df[,pred.ind]) / pred.temp.df[,response.var]
#   
#   pred.MAPE <- mean(abs(pred.temp.df[,"perc.error"])) *100
#   regression.accuracy.df[regression.accuracy.df$country == country.name, paste(pred.ind,".MAPE", sep="")] <- pred.MAPE
#   
# } else {print("Predictor has NAs")}
#####

# Create accuracy metrics for simple regression
#####

regression.accuracy.df <- data.frame(country    = character(), stringsAsFactors=FALSE)

for (country in 1:length(country.list)) {
  country.name <- country.list[country]
  regression.accuracy.df[country, 1] <- country.name
  country.df <- as.data.frame(gdp[gdp$country == country.name,])
  country.df <- country.df[order(country.df$year),]
  print(paste("****Calculating for", country.name))
  
  response.ts.abbrev <- ts(country.df[country.df$year < end.yr-2, response.var], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
  response.df.newdata <- country.df[,c("year", response.var)]
  response.df.newdata <- subset(response.df.newdata, year == 2013 | year == 2014 | year == 2015)
  response.df.newdata <- response.df.newdata[order(response.df.newdata$year),]
  
  for (predictor in 1:length(country.df)){
    if (sum(is.na(country.df[,predictor])) == 0 && (!colnames(country.df)[predictor] %in% c("year","country.region", "iso2c", "country"))) {
      pred.ind <- colnames(country.df)[predictor]
      pred.var <- unlist(series_ref[series_ref$series.indicator == pred.ind, "series.name"]) 
      
      country.trimmed.df <- country.df[,c("iso2c", "country", "year", response.var, pred.ind)]
      country.trimmed.df <- country.trimmed.df[order(country.trimmed.df$year),]
      
      pred.subset <- subset(country.trimmed.df, year == 2013 | year == 2014 | year == 2015)
      pred.newdata <- ts(pred.subset[,pred.ind], start=c(end.yr-2,1), end=c(end.yr,1), frequency=1)
      
      pred.ts.abbrev <- ts(country.trimmed.df[country.trimmed.df$year < end.yr-2, pred.ind], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
      intersect <- ts.intersect(response.ts.abbrev, pred.ts.abbrev)
      pred.tslm <- tslm(response.ts.abbrev ~ trend + pred.ts.abbrev, intersect)
      
      fcast <- as.data.frame(forecast(pred.tslm, newdata=data.frame(trend = c(23,24,25) ,pred.ts.abbrev = pred.newdata))$mean)
      colnames(fcast) <- pred.ind
      
      pred.temp.df <- cbind(response.df.newdata, fcast)
#      pred.temp.df$error <- pred.temp.df[,response.var] - pred.temp.df[,pred.ind]
      pred.temp.df$perc.error <- (pred.temp.df[,response.var] - pred.temp.df[,pred.ind]) / pred.temp.df[,response.var]
      
#       pred.MAE <- mean(abs(pred.temp.df[,"error"]))
#       pred.RMSE <- sqrt(mean(as.vector(pred.temp.df[,"error"])^2))
      pred.MAPE <- mean(abs(pred.temp.df[,"perc.error"])) *100
      
#       regression.accuracy.df[regression.accuracy.df$country == country.name, paste(pred.ind,".MAE", sep="")] <- pred.MAE
#       regression.accuracy.df[regression.accuracy.df$country == country.name, paste(pred.ind,".RMSE", sep="")] <- pred.RMSE
      regression.accuracy.df[regression.accuracy.df$country == country.name, paste(pred.ind,".MAPE", sep="")] <- pred.MAPE
    }
  }
}

regression.accuracy.df <- regression.accuracy.df[,!colnames(regression.accuracy.df) %in% c("SL.GDP.PCAP.EM.KD.MAPE")]
regression.accuracy.df$num.models <- apply(regression.accuracy.df, 1, function(x) sum(!is.na(x)) - 1)

full.accuracy.df <- left_join(accuracy.df, regression.accuracy.df, by=c("country"))
full.accuracy.df$best <- apply(full.accuracy.df, MARGIN = 1, FUN=function(x) colnames(full.accuracy.df)[which.min(x[2:ncol(full.accuracy.df)]) + 1])
#p <- apply(full.accuracy.df, MARGIN = 1, FUN=function(x) unlist(series_ref[series_ref$series.indicator == x["best"], "series.name"]) 
table(full.accuracy.df$best)
full.accuracy.df[,c("country", "best")]

for (column in 1:ncol(full.accuracy.df)){
#for (column in 1:30){
  if (!colnames(full.accuracy.df)[column] %in% c("country","num.models", "best") && sum(!is.na(full.accuracy.df[,column])) > 20 && max(full.accuracy.df[,column], na.rm=T) < 100) {
    model.name <- colnames(full.accuracy.df)[column]
   print(histogram(~full.accuracy.df[,column], data=full.accuracy.df, breaks=c(0,1,2,5,10,15,20,25,50,75,100), type="count", xlab="MAPE", main=model.name, ylim=c(0,75)))
    }
}
    
    