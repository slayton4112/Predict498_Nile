setwd("C:/Users/Greg/Desktop/Classes/NU/Capstone")
library(forecast)
library(dplyr)
library(tseries)
library(hts)
library(reshape2)
library(dummies)

load("C:/Users/Greg/Desktop/Classes/NU/Capstone/Predict498_Nile/nile_data.RData")

response.var <- "SL.GDP.PCAP.EM.KD"
num.yrs <- 25
start.yr <- 1991
end.yr <- 2015

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

cluster.designation <- read.csv("hc3.csv")
cluster.designation <- cluster.designation[,c("V1","V2")]
colnames(cluster.designation) <- c("country", "cluster")
cluster.designation[,"cluster"] <- as.factor(cluster.designation[,"cluster"])
dummies <- dummy(cluster.designation[,"cluster"])
cluster.designation <- cbind(cluster.designation,dummies)
cluster.designation <- subset(cluster.designation, select = -cluster.designation1)



# Arimax Predictors for each cluster
#####
predictor.df <- data.frame(cluster = character(), var1 = character(), var2 = character(), var3 = character())
predictor.df <- rbind(predictor.df, data.frame(cluster = "1", var1 = "SP.RUR.TOTL.ZG", var2 = "SP.URB.GROW", var3 = "NY.ADJ.DMIN.CD"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "2", var1 = "SH.STA.MMRT", var2 = "SP.POP.TOTL", var3 = "SH.DYN.NMRT"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "3", var1 = "SH.MMR.RISK", var2 = "SH.DYN.NMRT", var3 = "AG.LND.FRST.ZS"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "4", var1 = "SP.POP.DPND", var2 = "SP.POP.1564.TO.ZS", var3 = "EN.POP.DNST"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "5", var1 = "SP.POP.TOTL.FE.ZS", var2 = "TM.VAL.MRCH.CD.WT", var3 = "SP.RUR.TOTL.ZS"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "6", var1 = "SP.RUR.TOTL.ZS", var2 = "SH.DYN.NMRT", var3 = "SP.POP.65UP.TO.ZS"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "7", var1 = "SH.MMR.RISK", var2 = "EN.URB.MCTY.TL.ZS", var3 = "SP.POP.0014.TO.ZS"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "8", var1 = "SH.MMR.RISK.ZS", var2 = "AG.LND.FRST.K2", var3 = "AG.LND.TOTL.K2"))
predictor.df <- rbind(predictor.df, data.frame(cluster = "9", var1 = "SP.RUR.TOTL", var2 = "FM.LBL.BMNY.CN", var3 = "SP.DYN.IMRT.IN"))

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


# Practice run with ARIMAX model - Afghanistan
##### 
# accuracy.df <- data.frame(country     = character(),
#                           naive.oos.MAPE   = numeric(),
#                           drift.oos.MAPE   = numeric(),
#                           mean.oos.MAPE   = numeric(),
#                           arima.oos.MAPE  = numeric(),
#                           arimax.oos.MAPE = numeric()
# )
# 
country.name <- country.list[country]
country.cluster <- cluster.designation[cluster.designation == country.name, "cluster"]
country.df <- response.df.dropna[response.df.dropna$country == country.name,]
country.df <- country.df[order(country.df$year),]
actual.response <- as.data.frame(country.df[country.df$year >= 2013, response.var])
colnames(actual.response) <- "actual"
# cluster.preds <- predictor.df[predictor.df$cluster == as.character(country.cluster), c("var1", "var2", "var3")]
# cluster.pred.var1 <- as.character(cluster.preds$var1)
# cluster.pred.var2 <- as.character(cluster.preds$var2)
# cluster.pred.var3 <- as.character(cluster.preds$var3)
# country.pred.df <- gdp[gdp$country == country.name, c("country", "year", cluster.pred.var1, cluster.pred.var2, cluster.pred.var3)]
# country.pred.df <- country.pred.df[order(country.pred.df$year),]
# country.pred.df.abbrev <- country.pred.df[country.pred.df$year < 2013,]
# country.pred.df.newdata <- country.pred.df[country.pred.df$year >= 2013,]

country.pred.df <- cluster.designation[cluster.designation$country == country.name,3:10]
for (row in 1:21) { country.pred.df <- rbind(country.pred.df, cluster.designation[cluster.designation$country == country.name,3:10])}

country.pred.df.newdata <- cluster.designation[cluster.designation$country == country.name,3:10]
for (row in 1:3) {country.pred.df.newdata <- rbind(country.pred.df.newdata,cluster.designation[cluster.designation$country == country.name,3:10])}
# var1.len <- length(unique(country.pred.df.abbrev[,cluster.pred.var1]))
# var2.len <- length(unique(country.pred.df.abbrev[,cluster.pred.var2]))
# var3.len <- length(unique(country.pred.df.abbrev[,cluster.pred.var3]))
# 
# country.xreg <- matrix(nrow=22)

country.xreg <- as.matrix(country.pred.df)

# if (var1.len == 22) {country.xreg <- cbind(country.xreg, var1 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var1])))}
# if (var2.len == 22) {country.xreg <- cbind(country.xreg, var2 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var2])))}
# if (var3.len == 22) {country.xreg <- cbind(country.xreg, var3 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var3])))}
# 
# country.xreg <- country.xreg[,-1]
# 
# # country.xreg <- cbind(var1 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var1])),
# #                       var2 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var2])), 
# #                       var3 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var3])))
# 
# var1.len.new <- length(unique(country.pred.df.newdata[,cluster.pred.var1]))
# var2.len.new <- length(unique(country.pred.df.newdata[,cluster.pred.var2]))
# var3.len.new <- length(unique(country.pred.df.newdata[,cluster.pred.var3]))
# 
# country.new.xreg <- matrix(nrow=3)

country.new.xreg <- as.matrix(country.pred.df.newdata)
# if (var1.len.new == 3) {country.new.xreg <- cbind(country.new.xreg, var1 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var1])))}
# if (var2.len.new == 3) {country.new.xreg <- cbind(country.new.xreg, var2 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var2])))}
# if (var3.len.new == 3) {country.new.xreg <- cbind(country.new.xreg, var3 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var3])))}
# 
# country.new.xreg <- country.new.xreg[,-1]
# 
# # <- cbind(var1 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var1])),
# #                           var2 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var2])),
# #                           var3 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var3])))
# 
country.df <- response.df.dropna[response.df.dropna$country == country.name,]
country.df <- country.df[order(country.df$year),]
actual.response <- as.data.frame(country.df[country.df$year >= 2013, response.var])
colnames(actual.response) <- "actual"

auto.arima.fit <- auto.arima(arima.abbrev.df[,country.name])
print(names(auto.arima.fit$coef))
ifelse("drift" %in% as.list(names(auto.arima.fit$coef)), inc.drift <- TRUE, inc.drift <- FALSE)
p <- auto.arima.fit$arma[1]
d <- auto.arima.fit$arma[6]
q <- auto.arima.fit$arma[2]


Arima.fit <- Arima(arima.abbrev.df[,country.name], order=c(p,d,q), include.drift = inc.drift, xreg = country.xreg)
print(auto.arima.fit)
print(Arima.fit)
# 
# arima.forecasted.response <-  as.data.frame(forecast(auto.arima.fit, h=3)$mean)
# colnames(arima.forecasted.response) <- "arima.forecasted"
# arimax.forecasted.response <- as.data.frame(forecast(Arima.fit, h=3, xreg = country.new.xreg)$mean)
# colnames(arimax.forecasted.response) <- "arimax.forecasted"
# 
# country.arima.df <- cbind(actual.response, arima.forecasted.response, arimax.forecasted.response)
# country.arima.df$arima.perc.error <- (country.arima.df[,"actual"] - country.arima.df[,"arima.forecasted"]) / country.arima.df$actual
# country.arima.df$arimax.perc.error <- (country.arima.df[,"actual"] - country.arima.df[,"arimax.forecasted"]) / country.arima.df$actual
# 
# # Now do forecasting models     
# country.ts.abbrev <- ts(country.df[country.df$year < 2013, response.var], start=c(1991,1), end=c(2012,1), frequency = 1)
# # Use full ts to determine the OOS error
# country.ts.full <- ts(country.df[, response.var], start=c(1991,1), end=c(2015,1), frequency = 1) 
# 
# 
# naive.oos.MAPE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
# drift.oos.MAPE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAPE"]
# mean.oos.MAPE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
# arima.oos.MAPE <- mean(abs(country.arima.df[,"arima.perc.error"])) * 100
# arimax.oos.MAPE <- mean(abs(country.arima.df[,"arimax.perc.error"])) * 100
# 
# accuracy.df <- rbind(accuracy.df, data.frame(country     = country.name,
#                                              naive.oos.MAPE   = naive.oos.MAPE,
#                                              drift.oos.MAPE   = drift.oos.MAPE,
#                                              mean.oos.MAPE    = mean.oos.MAPE,
#                                              arima.oos.MAPE   = arima.oos.MAPE,
#                                              arimax.oos.MAPE   = arimax.oos.MAPE))

# Create for all countries
#####

accuracy.df <- data.frame(country     = character(),
                         naive.oos.MAPE   = numeric(),
                         drift.oos.MAPE   = numeric(),
                         mean.oos.MAPE   = numeric(),
                         arima.oos.MAPE  = numeric(),
                         arimax.oos.MAPE = numeric()
)

for (country in 1:length(country.list)) {

  country.name <- country.list[country]
  print(paste("**Calculating for", country.name))
  country.cluster <- cluster.designation[cluster.designation == country.name, "cluster"]
  country.df <- response.df.dropna[response.df.dropna$country == country.name,]
  country.df <- country.df[order(country.df$year),]
  actual.response <- as.data.frame(country.df[country.df$year >= 2013, response.var])
  colnames(actual.response) <- "actual"
  cluster.preds <- predictor.df[predictor.df$cluster == as.character(country.cluster), c("var1", "var2", "var3")]
  cluster.pred.var1 <- as.character(cluster.preds$var1)
  cluster.pred.var2 <- as.character(cluster.preds$var2)
  cluster.pred.var3 <- as.character(cluster.preds$var3)
  country.pred.df <- gdp[gdp$country == country.name, c("country", "year", cluster.pred.var1, cluster.pred.var2, cluster.pred.var3)]
  country.pred.df <- country.pred.df[order(country.pred.df$year),]
  country.pred.df.abbrev <- country.pred.df[country.pred.df$year < 2013,]
  country.pred.df.newdata <- country.pred.df[country.pred.df$year >= 2013,]
  
  var1.len <- length(unique(country.pred.df.abbrev[,cluster.pred.var1]))
  var2.len <- length(unique(country.pred.df.abbrev[,cluster.pred.var2]))
  var3.len <- length(unique(country.pred.df.abbrev[,cluster.pred.var3]))

  var1.len.new <- length(unique(country.pred.df.newdata[,cluster.pred.var1]))
  var2.len.new <- length(unique(country.pred.df.newdata[,cluster.pred.var2]))
  var3.len.new <- length(unique(country.pred.df.newdata[,cluster.pred.var3]))
  
  country.xreg <- matrix(nrow=22)
  country.new.xreg <- matrix(nrow=3)
  
  if (var1.len > 5) {country.xreg <- cbind(country.xreg, var1 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var1])))
                     country.new.xreg <- cbind(country.new.xreg, var1 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var1])))
                    }
  if (var2.len > 5) {country.xreg <- cbind(country.xreg, var2 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var2])))
                     country.new.xreg <- cbind(country.new.xreg, var2 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var2])))
                    }
  if (var3.len > 5) {country.xreg <- cbind(country.xreg, var3 = as.matrix(as.ts(country.pred.df.abbrev[,cluster.pred.var3])))
                     country.new.xreg <- cbind(country.new.xreg, var3 = as.matrix(as.ts(country.pred.df.newdata[,cluster.pred.var3])))
                    }
  
  country.xreg <- country.xreg[,-1]
  country.new.xreg <- country.new.xreg[,-1]
  
  country.df <- response.df.dropna[response.df.dropna$country == country.name,]
  country.df <- country.df[order(country.df$year),]
  actual.response <- as.data.frame(country.df[country.df$year >= 2013, response.var])
  colnames(actual.response) <- "actual"

  #We need to extract include.drift and order args from auto.arima, then pass that into Arima() for each country
  auto.arima.fit <- auto.arima(arima.abbrev.df[,country.name])
  ifelse("drift" %in% as.list(names(auto.arima.fit$coef)), inc.drift <- TRUE, inc.drift <- FALSE)
  p <- auto.arima.fit$arma[1]
  d <- auto.arima.fit$arma[6]
  q <- auto.arima.fit$arma[2]
  
  Arima.err <- tryCatch(Arima.fit <- Arima(arima.abbrev.df[,country.name], order=c(p,d,q), include.drift = inc.drift, xreg = country.xreg), error=function(e) e)
  #Arima.fit <- Arima(arima.abbrev.df[,country.name], order=c(p,d,q), include.drift = inc.drift, xreg = country.xreg)
  if (inherits(Arima.err, "error")) {
    arima.forecasted.response <-  as.data.frame(forecast(auto.arima.fit, h=3)$mean)
    colnames(arima.forecasted.response) <- "arima.forecasted"
    
    country.arima.df <- cbind(actual.response, arima.forecasted.response, arimax.forecasted.response)
    country.arima.df$arima.perc.error <- (country.arima.df[,"actual"] - country.arima.df[,"arima.forecasted"]) / country.arima.df$actual

    # Now do forecasting models     
    country.ts.abbrev <- ts(country.df[country.df$year < 2013, response.var], start=c(1991,1), end=c(2012,1), frequency = 1)
    # Use full ts to determine the OOS error
    country.ts.full <- ts(country.df[, response.var], start=c(1991,1), end=c(2015,1), frequency = 1) 
    
    
    naive.oos.MAPE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
    drift.oos.MAPE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAPE"]
    mean.oos.MAPE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
    arima.oos.MAPE <- mean(abs(country.arima.df[,"arima.perc.error"])) * 100

    accuracy.df <- rbind(accuracy.df, data.frame(country     = country.name,
                                                 naive.oos.MAPE   = naive.oos.MAPE,
                                                 drift.oos.MAPE   = drift.oos.MAPE,
                                                 mean.oos.MAPE    = mean.oos.MAPE,
                                                 arima.oos.MAPE   = arima.oos.MAPE,
                                                 arimax.oos.MAPE   = NaN))
  } 
  else {
  
  arima.forecasted.response <-  as.data.frame(forecast(auto.arima.fit, h=3)$mean)
  colnames(arima.forecasted.response) <- "arima.forecasted"
  arimax.forecasted.response <- as.data.frame(forecast(Arima.fit, h=3, xreg = country.new.xreg)$mean)
  colnames(arimax.forecasted.response) <- "arimax.forecasted"
  
  country.arima.df <- cbind(actual.response, arima.forecasted.response, arimax.forecasted.response)
  country.arima.df$arima.perc.error <- (country.arima.df[,"actual"] - country.arima.df[,"arima.forecasted"]) / country.arima.df$actual
  country.arima.df$arimax.perc.error <- (country.arima.df[,"actual"] - country.arima.df[,"arimax.forecasted"]) / country.arima.df$actual
  
  # Now do forecasting models     
  country.ts.abbrev <- ts(country.df[country.df$year < 2013, response.var], start=c(1991,1), end=c(2012,1), frequency = 1)
  # Use full ts to determine the OOS error
  country.ts.full <- ts(country.df[, response.var], start=c(1991,1), end=c(2015,1), frequency = 1) 
  
  
  naive.oos.MAPE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
  drift.oos.MAPE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAPE"]
  mean.oos.MAPE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
  arima.oos.MAPE <- mean(abs(country.arima.df[,"arima.perc.error"])) * 100
  arimax.oos.MAPE <- mean(abs(country.arima.df[,"arimax.perc.error"])) * 100
  
  accuracy.df <- rbind(accuracy.df, data.frame(country     = country.name,
                                               naive.oos.MAPE   = naive.oos.MAPE,
                                               drift.oos.MAPE   = drift.oos.MAPE,
                                               mean.oos.MAPE    = mean.oos.MAPE,
                                               arima.oos.MAPE   = arima.oos.MAPE,
                                               arimax.oos.MAPE   = arimax.oos.MAPE))
}}

accuracy.df.w.clust <- left_join(cluster.designation[,c("country", "cluster")], accuracy.df, by=c("country"))


#Single Regressors
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

full.accuracy.df <- left_join(accuracy.df.w.clust, regression.accuracy.df, by=c("country"))
#full.accuracy.df <- left_join(cluster.designation[,c("country","cluster")], full.accuracy.df, by=c("country"))
full.accuracy.df$best <- apply(full.accuracy.df, MARGIN = 1, FUN=function(x) colnames(full.accuracy.df)[which.min(x[3:ncol(full.accuracy.df)]) + 2])
#p <- apply(full.accuracy.df, MARGIN = 1, FUN=function(x) unlist(series_ref[series_ref$series.indicator == x["best"], "series.name"]) 
table(full.accuracy.df$best)
table(full.accuracy.df$cluster, full.accuracy.df$best)
full.accuracy.df[,c("country", "best")]

#for (column in 1:ncol(full.accuracy.df)){
for (column in 1:8){
  if (!colnames(full.accuracy.df)[column] %in% c("country","num.models", "best","cluster") && sum(!is.na(full.accuracy.df[,column])) > 20 && max(full.accuracy.df[,column], na.rm=T) < 100) {
    model.name <- colnames(full.accuracy.df)[column]
    print(histogram(~full.accuracy.df[,column] | cluster, data=full.accuracy.df, breaks=c(0,1,2,5,10,15,20,25,50,75,100), type="count", xlab="MAPE", main=model.name, ylim=c(0,20)))
  }
}