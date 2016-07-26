setwd("C:/Users/Greg/Desktop/Classes/NU/Capstone")
library(forecast)
library(dplyr)

load("C:/Users/Greg/Desktop/Classes/NU/Capstone/Predict498_Nile/nile_data.RData")

#series_ref <- tbl_df(as.data.frame(WDI_data["series"])) # full list of WDI data series
#country_ref <- tbl_df(as.data.frame(WDI_data["country"])) # extra country information

#train.data <- read.csv("Data/data/TrainingSet.csv")

#gdp.ppe.colname <- unlist(series_ref[series_ref$series.name == "GDP per person employed (constant 1990 PPP $)", "series.indicator"])
colname <- "SL.GDP.PCAP.EM.KD"
num.yrs <- 25
start.yr <- 1991
end.yr <- 2015
metric.df <- gdp[,c("iso2c", "country", "country.region", "year", colname)]
unique(metric.df$country)

metric.df$missing = is.na(gdp$SL.GDP.PCAP.EM.KD)*1

# Group by country and sum missing GDP
num.missing = metric.df %>%
  group_by(country) %>%
  summarise(num_miss = sum(missing)) %>%
  arrange(desc(num_miss))

# Filter on countries missing GDP data
nonmiss <- filter(num.missing, num_miss == 0)
miss <- filter(num.missing, num_miss > 0)

# Drop the countries that are missing GDP data
metric.dropna.df <- metric.df[!(metric.df$country %in% miss$country),]

#### Using US as example to then include in loop for all countries
us.gdp.ppe <- metric.df[metric.df$country == "United States",]
us.gdp.ppe <- us.gdp.ppe[order(us.gdp.ppe$year),]
us.ts.abbrev <- ts(us.gdp.ppe[us.gdp.ppe$year < end.yr-2, colname], start=c(start.yr,1), end=c(end.yr-3,1), frequency = 1)
us.ts.full <- ts(us.gdp.ppe[,colname], start=c(start.yr,1), end=c(end.yr,1), frequency = 1)
accuracy(rwf(us.ts.abbrev, h = 3, drift = TRUE), us.ts.full)
accuracy(meanf(us.ts.abbrev, h=3), us.ts.full)

plot(us.ts.full, main = "United States", ylab = "GDP per person employed (constant 1990 PPP $)")
naive(us.ts.abbrev, h =3)
lines(rwf(us.ts.abbrev, h = 3, drift = TRUE)$mean, col = 130, lty = 2)
lines(naive(us.ts.abbrev, h=3)$mean, col = 3, lty = 2)
lines(meanf(us.ts.abbrev, h=3)$mean, col = 4, lty = 2)
legend("topleft", lty=2, col=c(130, 3, 4), legend=c("Drift method", "Naive method", "Mean method"))

naive.res <- residuals(naive(us.ts.full))
mean.res <- residuals(meanf(us.ts.full))
drift.res <- residuals(rwf(us.ts.full))
plot(naive.res, ylab = "Residual - Naive method", main = "United States")
plot(mean.res, ylab = "Residuals - Mean method", main = "United States")
plot(drift.res, ylab = "Residuals - Drift method", main = "United States")

hist(naive.res, nclass="FD", main = "US", xlab = "Residuals for Naive method")
hist(mean.res, nclass="FD", main = "US", xlab = "Residuals for Mean method")
hist(drift.res, nclass="FD", main="United States", xlab = "Residuals for Drift method")

#### End US example - Moving to all countries below

country.list <- as.vector(nonmiss[order(nonmiss$country), "country"]$country)

accuracy.df <- data.frame(country     = character(),
                          naive.train.MAE   = numeric(),
                          naive.test.MAE    = numeric(),
                          naive.train.RMSE  = numeric(),
                          naive.test.RMSE   = numeric(),
                          naive.train.MAPE  = numeric(),
                          naive.test.MAPE   = numeric(),
                          drift.train.MAE   = numeric(),
                          drift.test.MAE    = numeric(),
                          drift.train.RMSE  = numeric(),
                          drift.test.RMSE   = numeric(),
                          drift.train.MAPE  = numeric(),
                          drift.test.MAPE   = numeric(),
                          mean.train.MAE   = numeric(),
                          mean.test.MAE    = numeric(),
                          mean.train.RMSE  = numeric(),
                          mean.test.RMSE   = numeric(),
                          mean.train.MAPE  = numeric(),
                          mean.test.MAPE   = numeric()
                          )

for (country in 1:length(country.list)) {
  country.name <- country.list[country]
  print(country.name)
  country.df <- metric.df[metric.df$country == country.name,]
  country.df <- country.df[order(country.df$year),]
  if (length(unique(country.df[,colname])) > 1) {
  country.ts.abbrev <- ts(country.df[country.df$year < 2013, colname], start=c(1991,1), end=c(2012,1), frequency = 1)  
  country.ts.full <- ts(country.df[, colname], start=c(1991,1), end=c(2015,1), frequency = 1)  

  plot(country.ts.full, main = country.name, ylab = "GDP per person employed (constant 1990 PPP $)", xlab = "Year")
  lines(rwf(country.ts.abbrev, h=3, drift = TRUE)$mean, col = 130, lty=2)
  lines(naive(country.ts.abbrev, h=3)$mean, col = 3, lty=2)
  lines(meanf(country.ts.abbrev, h=3)$mean, col = 4, lty = 2)
  legend("topleft", lty = 2, col=c(130,3,4), legend=c("Drift Method", "Naive method", "Mean method")) 
  
  acf(country.ts.full, main = country.name)

  naive.res <- residuals(naive(country.ts.full))
  mean.res <- residuals(meanf(country.ts.full))
  drift.res <- residuals(rwf(country.ts.full, drift = TRUE))
  plot(naive.res, ylab = "Residual - Naive method", main = country.name)
  plot(mean.res, ylab = "Residuals - Mean method", main = country.name)
  plot(drift.res, ylab = "Residuals - Drift method", main = country.name)
  
  hist(naive.res, nclass="FD", main = country.name, xlab = "Residuals for Naive method")
  hist(mean.res, nclass="FD", main = country.name, xlab = "Residuals for Mean method")
  hist(drift.res, nclass="FD", main= country.name, xlab = "Residuals for Drift method")
  
  # populate df of errors based on various accuracy metrics - MAE, MPE, RMSE
  naive.train.MAE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Training set", "MAE"]
  naive.test.MAE    <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAE"]
  naive.train.RMSE  <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Training set", "RMSE"]
  naive.test.RMSE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "RMSE"]
  naive.train.MAPE  <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Training set", "MAPE"]
  naive.test.MAPE   <- accuracy(naive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
  drift.train.MAE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Training set", "MAE"]
  drift.test.MAE    <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAE"]
  drift.train.RMSE  <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Training set", "RMSE"]
  drift.test.RMSE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "RMSE"]
  drift.train.MAPE  <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Training set", "MAPE"]
  drift.test.MAPE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAPE"]
  mean.train.MAE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Training set", "MAE"]
  mean.test.MAE    <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAE"]
  mean.train.RMSE  <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Training set", "RMSE"]
  mean.test.RMSE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "RMSE"]
  mean.train.MAPE  <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Training set", "MAPE"]
  mean.test.MAPE   <- accuracy(meanf(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
  
  
  accuracy.df <- rbind(accuracy.df, data.frame(country     = country.name,
                                               naive.train.MAE   = naive.train.MAE,
                                               naive.test.MAE    = naive.test.MAE ,
                                               naive.train.RMSE  = naive.train.RMSE,
                                               naive.test.RMSE   = naive.test.RMSE,
                                               naive.train.MAPE  = naive.train.MAPE,
                                               naive.test.MAPE   = naive.test.MAPE,
                                               drift.train.MAE   = drift.train.MAE,
                                               drift.test.MAE    = drift.test.MAE,
                                               drift.train.RMSE  = drift.train.RMSE,
                                               drift.test.RMSE   = drift.test.RMSE,
                                               drift.train.MAPE  = drift.train.MAPE,
                                               drift.test.MAPE   = drift.test.MAPE))
  }
}

accuracy.df$train.MAE <- apply(accuracy.df, MARGIN = 1, FUN=function(x) ifelse(x["naive.train.MAE"] < x["drift.train.MAE"], "naive", "drift"))
accuracy.df$test.MAE  <- apply(accuracy.df, MARGIN = 1, FUN=function(x) ifelse(x["naive.test.MAE"] < x["drift.test.MAE"], "naive", "drift"))
accuracy.df$train.RMSE <- apply(accuracy.df, MARGIN = 1, FUN=function(x) ifelse(x["naive.train.RMSE"] < x["drift.train.RMSE"], "naive", "drift"))
accuracy.df$test.RMSE  <- apply(accuracy.df, MARGIN = 1, FUN=function(x) ifelse(x["naive.test.RMSE"] < x["drift.test.RMSE"], "naive", "drift"))
accuracy.df$train.MAPE <- apply(accuracy.df, MARGIN = 1, FUN=function(x) ifelse(x["naive.train.MAPE"] < x["drift.train.MAPE"], "naive", "drift"))
accuracy.df$test.MAPE  <- apply(accuracy.df, MARGIN = 1, FUN=function(x) ifelse(x["naive.test.MAPE"] < x["drift.test.MAPE"], "naive", "drift"))

# Let's see which of the two basic forecasts outperformed the other
table(accuracy.df$train.MAE)
table(accuracy.df$test.MAE)
table(accuracy.df$train.RMSE)
table(accuracy.df$test.RMSE)
table(accuracy.df$train.MAPE)
table(accuracy.df$test.MAPE)

# Let's just see if there are any differences between regions of the world
accuracy.df <- left_join(accuracy.df, country_ref[,c("country.country", "country.region")], by = c("country" = "country.country"))
table(accuracy.df$country.region,accuracy.df$train.MAE)
table(accuracy.df$country.region,accuracy.df$test.MAE)
table(accuracy.df$country.region,accuracy.df$train.RMSE)
table(accuracy.df$country.region,accuracy.df$test.RMSE)
table(accuracy.df$country.region,accuracy.df$train.MAPE)
table(accuracy.df$country.region,accuracy.df$test.MAPE)

#### Cross-sectional simple regression
us.df <- gdp[gdp$country == "United States",]
# Let's do a simple regression on one variable before expanding to all variables
head(us.df[0:8])
pred.ind <- "AG.AGR.TRAC.NO"
pred.var <- unlist(series_ref[series_ref$series.indicator == pred.ind, "series.name"])
print(as.character(pred.var))
trimmed.df <- us.df[,c("iso2c", "country", "year", colname, pred.ind)]
trimmed.df <- trimmed.df[order(trimmed.df$year),]
us.response.ts.abbrev <- ts(trimmed.df[trimmed.df$year < end.yr-2, colname], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
us.response.ts.full <- ts(trimmed.df[,colname], start=c(start.yr,1), end=c(end.yr,1), frequency=1)
us.pred.ts.abbrev <- ts(trimmed.df[trimmed.df$year < end.yr-2, pred.ind], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
us.pred.ts.full <- ts(trimmed.df[,pred.ind], start=c(start.yr,1), end=c(end.yr,1), frequency=1)
us.pred.ts.full <- na.omit(pred.ts.full)

# Using Augmented Dickey-Fuller test for unit root; basically is the given ts stationary? Higher p-val indicates non-stationarity
# Both response and pred variable are non-stationary
adf.test(us.response.ts.full, alternative=c("stationary"))
adf.test(us.pred.ts.full, alternative=c("stationary"))

us.ts.intersect <- ts.intersect(us.response.ts.full, us.pred.ts.full)
us.tslm <- tslm(us.response.ts.full ~ trend + us.pred.ts.full, us.ts.intersect)

# Extract p-value of predictor
pred.us.pval <- summary(test)$coefficients["us.pred.ts.full",4]

# Reproduce the US example to all countries
response.var <- "SL.GDP.PCAP.EM.KD"
pred.accuracy.df <- data.frame(
                      country = character(),
                      pred.ind = character(),
                      pred.var = character(),
                      num.years = integer(),
                      p.val = numeric()
                      )

for (country in 1:length(country.list)) {
  country.name <- country.list[country]
  country.df <- as.data.frame(gdp[gdp$country == country.name,])
  print(paste("****Calculating for", country.name))
  for (predictor in 1:length(country.df)){
    pred.ind <- colnames(country.df)[predictor]
    if (typeof(country.df[,predictor]) == "double" && !(pred.ind %in% c("year","country.region"))) {
      pred.var <- unlist(series_ref[series_ref$series.indicator == pred.ind, "series.name"]) 
      
      country.trimmed.df <- country.df[,c("iso2c", "country", "year", response.var, pred.ind)]
      country.trimmed.df <- country.trimmed.df[order(country.trimmed.df$year),]
      
      #This shouldn't be in this loop (should be in country loop), but it works so we'll keep as-is for the moment
      response.ts.abbrev <- ts(country.trimmed.df[country.trimmed.df$year < end.yr-2, response.var], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
      response.ts.full <- ts(country.trimmed.df[,response.var], start=c(start.yr,1), end=c(end.yr,1), frequency=1)
      
      pred.ts.abbrev <- ts(country.trimmed.df[country.trimmed.df$year < end.yr-2, pred.ind], start=c(start.yr,1), end=c(end.yr-3,1), frequency=1)
      pred.ts.full <- ts(country.trimmed.df[,pred.ind], start=c(start.yr,1), end=c(end.yr,1), frequency=1)
      
      if (all(is.na(pred.ts.full)) == FALSE){
        pred.ts.full <- na.remove(pred.ts.full)
      
        if (length(pred.ts.full) > 5) {
          err.intersect <- tryCatch(intersect <- ts.intersect(response.ts.full, pred.ts.full), error=function(e) e)
          
          if(inherits(err.intersect,"error")) next
          
          num.obs <- nrow(intersect)
          pred.tslm <- tslm(response.ts.full ~ trend + pred.ts.full, intersect)
          err.pval <- tryCatch(pred.pval <- summary(pred.tslm)$coefficients["pred.ts.full",4], error=function(e) e)
          
          if (inherits(err.pval, "error")) next
          
          pred.accuracy.df <- rbind(pred.accuracy.df, data.frame(
                                country = country.name,
                                pred.ind = pred.ind,
                                pred.var = pred.var,
                                num.years = num.obs,
                                p.val = pred.pval
            ))
        }
#      else print(paste(pred.var, "has less than 5 observations for", country.name))
      }
#    else print(paste(pred.var, "has 0 non-NA observations for", country.name)     
    }
  }
}

pred.accuracy.df <- pred.accuracy.df[,c("country", "pred.ind", "pred.var", "num.years", "p.val")]
pred.accuracy.df.cp <- pred.accuracy.df

pred.accuracy.df$country.rank <- NA

ranked.df <- data.frame(country = character(),
                        pred.ind = character(),
                        pred.var = character(),
                        num.years = numeric(),
                        p.val = numeric,
                        country.rank = numeric()
                        )

for (country in 1:length(country.list)) {
  temp.df <- pred.accuracy.df[pred.accuracy.df$country == country.list[country],]
  temp.df$country.rank <- rank(temp.df$p.val, na.last = TRUE)
  ranked.df <- rbind(ranked.df, temp.df)
}

count.df <- as.data.frame(aggregate(ranked.df$country.rank, by=list(ranked.df$pred.ind, ranked.df$pred.var), FUN = length))
colnames(count.df) <- c("pred.ind", "pred.var", "country.count")

rank.mean.df <- as.data.frame(aggregate(ranked.df$country.rank, by=list(ranked.df$pred.ind, ranked.df$pred.var), FUN = mean))
colnames(rank.mean.df) <- c("pred.ind", "pred.var", "avg.rank")

pval.mean.df <- as.data.frame(aggregate(ranked.df$p.val, by=list(ranked.df$pred.ind, ranked.df$pred.var), FUN = mean))
colnames(pval.mean.df) <- c("pred.ind", "pred.var", "avg.pval")

var.df <- left_join(rank.mean.df, pval.mean.df, by = c("pred.ind", "pred.var"))
var.df <- left_join(var.df, count.df, by = c("pred.ind", "pred.var"))
var.df <- var.df[order(var.df$avg.pval),]

# # Let's attempt to remove the stationarity of the response using a simple moving average over 5 years
# ma.us <- ma(us.response.ts.full, 5)
# us.rand.component <- na.omit(us.response.ts.full - ma.us)
# plot(us.rand.component)
# 
# ma.us.pred <- ma(us.pred.ts.full, 5)
# us.pred.rand.component <- na.omit(us.pred.ts.full - ma.us.pred)
# plot(us.pred.rand.component)
# 
# intersect <- ts.intersect(response.ts.full, us.pred.rand.component)
# pred.lm <- tslm(us.rand.component~us.pred.rand.component, intersect)