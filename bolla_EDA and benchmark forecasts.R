setwd("C:/Users/Greg/Desktop/Classes/NU/Capstone")
library(forecast)


train.data <- read.csv("Data/data/TrainingSet.csv")
gdp.ppe.df <- train.data[train.data$Series.Name == "GDP per person employed (constant 1990 PPP $)", ]
unique(gdp.ppe.df$Country.Name)

us.gdp.ppe <- unlist(gdp.ppe.df[gdp.ppe.df$Country.Name == "United States",])
us.ts.abbrev <- ts(us.gdp.ppe[2:34], start=c(1972,1), end=c(2004,1), frequency = 1)
us.ts.full <- ts(us.gdp.ppe[2:37], start=c(1972,1), end=c(2007,1), frequency = 1)
accuracy(rwf(us.ts.abbrev, h = 3, drift = TRUE), us.ts.full)

plot(us.ts.full, main = "US", ylab = "GDP per person employed (constant 1990 PPP $)")
snaive(us.ts.abbrev, h =3)
lines(rwf(us.ts.abbrev, h = 3, drift = TRUE)$mean, col = 130, lty = 2)
lines(snaive(us.ts.abbrev, h =3)$mean, col = 3, lty = 2)
legend("topleft", lty=2, col=c(130, 3), legend=c("Drift method", "(Seasonal) Naive"))

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
                          drift.test.MAPE   = numeric())

for (row in 1:nrow(gdp.ppe.df)) {
  country.name <- gdp.ppe.df[row, "Country.Name"]
  country.vec <-unlist(gdp.ppe.df[gdp.ppe.df$Country.Name == country.name,])
  country.ts.full <- ts(country.vec[2:37], start=c(1972,1), end=c(2007,1), frequency = 1)
  country.ts.abbrev <- ts(country.vec[2:34], start=c(1972,1), end=c(2004,1), frequency = 1)
  plot(country.ts.full, main = country.name, ylab = "GDP per person employed (constant 1990 PPP $)", xlab = "Year")
  lines(rwf(country.ts.abbrev, h=3, drift = TRUE)$mean, col = 130, lty=2)
  lines(snaive(country.ts.abbrev, h = 3)$mean, col = 3, lty=2)
  legend("topleft", lty = 2, col=c(130,3), legend=c("Drift Method", "(Seasonal) Naive")) 

  # populate df of errors based on various accuracy metrics - MAE, MPE, RMSE
  naive.train.MAE   <- accuracy(snaive(country.ts.abbrev, h = 3), country.ts.full)["Training set", "MAE"]
  naive.test.MAE    <- accuracy(snaive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAE"]
  naive.train.RMSE  <- accuracy(snaive(country.ts.abbrev, h = 3), country.ts.full)["Training set", "RMSE"]
  naive.test.RMSE   <- accuracy(snaive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "RMSE"]
  naive.train.MAPE  <- accuracy(snaive(country.ts.abbrev, h = 3), country.ts.full)["Training set", "MAPE"]
  naive.test.MAPE   <- accuracy(snaive(country.ts.abbrev, h = 3), country.ts.full)["Test set", "MAPE"]
  drift.train.MAE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Training set", "MAE"]
  drift.test.MAE    <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAE"]
  drift.train.RMSE  <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Training set", "RMSE"]
  drift.test.RMSE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "RMSE"]
  drift.train.MAPE  <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Training set", "MAPE"]
  drift.test.MAPE   <- accuracy(rwf(country.ts.abbrev, h = 3, drift = TRUE), country.ts.full)["Test set", "MAPE"]
  
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

# Next step is to subset the countries where the naive or drift forecast performed well (through MAPE?) and those that didn't. 
# Is it possible geography/region plays a role in whether naive/drift performs well?