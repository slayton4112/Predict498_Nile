# Arima Fitting

setwd('C:/Users/Scott Layton/Documents/Predict498_Nile')

library(reshape2)
load("C:/Users/Scott Layton/Documents/Predict498_Nile/nile_data.RData")

filt_gdp = subset(gdp, !(country %in% unique(subset(gdp,gdp_outliers == 1)$country)))

arm_dta = filt_gdp[,c('country', 'year', 'SL.GDP.PCAP.EM.KD')]

order_arm_dta = arm_dta[order(arm_dta$country, arm_dta$year),]

wide_dta = dcast(order_arm_dta, year ~ country, value.var="SL.GDP.PCAP.EM.KD")

names(wide_dta)


# try gts_fit

gts_fit = gts(ts(wide_dta, class='mts'))

plot.gts(gts_fit)

allts(gts_fit)


# Just fit 'em all with auto.arima()

h = 10
allf = matrix(NA,nrow=h, ncol=ncol(wide_dta))
res = matrix(NA, nrow=nrow(wide_dta), ncol=ncol(wide_dta))

for( i in 1:ncol(wide_dta))
{
  fit <- auto.arima(wide_dta[,i])
  allf[,i] = forecast(fit, h=h)$mean
  res[, i] = na.omit(wide_dta[,i]-fitted(fit))
}

colnames(res) <- names(wide_dta)

colnames(res)

saveRDS(res,file="residuals_by_country.rds")

res <- readRDS('residuals_by_country.rds')