########################################
########################################
# Packages
########################################
require(WDI)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lattice)
require(reshape2)
require(stringr)
require(forecast)
require(StatMeasures)
require(psych)
require(dtw)
require(mclust)
require(C50)
########################################
# Get Data
########################################
# Target Variable is "GDP per person employed (constant 1990 PPP $)"
# SL.GDP.PCAP.EM.KD
# Extract Raw Data & Variable Lists from WDI api
series_ref = tbl_df(as.data.frame(WDI_data[1])) # full list of WDI data series
country_ref = tbl_df(as.data.frame(WDI_data[2])) # extra country information
wdi = filter(series_ref, series.sourceDatabase == "World Development Indicators") # filter on WDI indicators
wdi_vars = as.vector(unique(wdi$series.indicator)) # create vector of all WDI indicator names
# Create master dataframe
gdp = tbl_df(WDI(indicator = wdi_vars, country = 'all', start=1991, end=2015)) # WDI indicator dataframe
gdp = merge(gdp, country_ref[, c(2,4)], by.x = "iso2c", by.y = "country.iso2c", all.x = TRUE) # add region type
gdp = filter(gdp, country.region != "Aggregates") # drop Aggregates type
gdp$country = as.factor(gdp$country) # convert country to factor from char
# Save RData file
# Extracting each time from the API is too slow
save(gdp, wdi, country_ref, series_ref, file = "nile_data.RData")
View(gdp)
####################################
# CLEAN
####################################
# Drop countries missing GDP data for our time frame
# How many are missing GDP?
gdp$missing = is.na(gdp$SL.GDP.PCAP.EM.KD)*1 # create missing flag
# Group by country and sum missing GDP
miss = gdp %>%
  group_by(country) %>%
  summarise(num_miss = sum(missing)) %>%
  arrange(desc(num_miss))
ggplot(filter(miss, num_miss > 0), aes(reorder(country, num_miss), num_miss)) +
  geom_bar(stat = "identity") + coord_flip()
## Smallest number of GDP missing is 22, Drop all countries >=22
# Filter on countries (43 of them) missing most GDP data
drop = filter(miss, num_miss >= 22)
# Drop the countries that are mostly missing GDP data
gdp <- gdp[!(gdp$country %in% drop$country),]
gdp$missing = NULL
length(unique(gdp$country)) # 174 countries leftover
# Flag Outliers in GDP as gdp_outliers
uq = quantile(gdp$SL.GDP.PCAP.EM.KD, .75) # upper quartile
lq = quantile(gdp$SL.GDP.PCAP.EM.KD, .25) # lower quartile
iqr.gdp = IQR(gdp$SL.GDP.PCAP.EM.KD) # inter quartile range
gdp$gdp_outliers = ((gdp$SL.GDP.PCAP.EM.KD > uq + 1.5 * iqr.gdp) | (gdp$SL.GDP.PCAP.EM.KD < lq - 1.5 * iqr.gdp)) * 1
# Group by country and sum missing GDP
gdp.out = gdp %>%
  group_by(country) %>%
  summarise(gdp.outliers = sum(gdp_outliers)) %>%
  arrange(desc(gdp.outliers))
View(gdp.out)
View(gdp)
gdp_HC = gdp[ ! gdp$year %in% c(2013,2014,2015), ]
gdp_HC1 = gdp_HC[ , colSums(is.na(gdp_HC)) == 0]
View(gdp_HC1)
HC1=gdp_HC[ , paste0( "M_",names(gdp_HC)[-1])] <- lapply(gdp_HC[-1], function(x) as.numeric(is.na(x)) )
View(gdp_HC1)
View(HC1)
HC.df = cbind(gdp_HC,HC1)
View(HC.df)
gdp_HC.df = HC.df[ , colSums(is.na(HC.df)) == 0]
View(gdp_HC.df)
gdp_HC.mat=as.matrix(gdp_HC.df)
gdp_HC.df2=sapply( gdp_HC.df, as.numeric )
View(gdp_HC.df2)
gdp_HC.df2 = gdp_HC.df[,c(4:25,26,29:2062)]
gdp_HC.df2=sapply( gdp_HC.df2, as.numeric )
HCScale = scale(gdp_HC.df2, center=TRUE, scale=TRUE)
HCScale=cbind(gdp_HC.df[,c(2:3)],HCScale)
View(HCScale)
HCScale=HCScale[,c(1:23)]
HCScale2=HCScale[order(HCScale[,1], HCScale[,2]),]
View(HCScale2)
HCDS=reshape(HCScale2, idvar="country", timevar="year", direction="wide")
View(HCDS)
HCDS.mod=t(HCDS)
View(HCDS.mod)
colnames(HCDS.mod) = HCDS.mod[1, ] # the first row will be the header
HCDS.mod = HCDS.mod[-1, ]          # removing the first row.
View(HCDS.mod)
HCDS.mod2=HCDS.mod[order(row.names(HCDS.mod)), ]
View(HCDS.mod2)
library(dtw)
HCDS.mod11=t(HCDS.mod2)
distMatrix2 <- dist(HCDS.mod11, method="DTW")
hc2 <- hclust(distMatrix2, method="average")
plot(hc2)
# Model Based Clustering
library(mclust)
fitMclust <- Mclust(distMatrix2)
plot(fitMclust, what="BIC") # plot results
summary(fitMclust) # display the best model 
hc2$MclustClass=fitMclust$classification
plot(hc2)
hc3.csv=cbind(row.names(HCDS.mod11), hc2$MclustClass)
View(hc3.csv)
write.csv(hc3.csv, file = "hc3.csv")
library(plyr)
colnames(hc3.csv)=c("country", "cluster")
hc3.csv=as.data.frame(hc3.csv)
library(data.table)
gdp_HC.df3=cbind(gdp_HC.df[,c(2:3)],gdp_HC.df2)
gdp_HC.df4=gdp_HC.df[,c(2:23)]
TS_DWT=gdp_HC.df4[order(gdp_HC.df4[,1], gdp_HC.df4[,2]),]
TD_DWT=reshape(TS_DWT, idvar="country", timevar="year", direction="wide")
TD_DWT.mod=t(TD_DWT)
colnames(TD_DWT.mod) = TD_DWT.mod[1, ] # the first row will be the header
TD_DWT.mod = TD_DWT.mod[-1, ]          # removing the first row.
TD_DWT.mod2=TD_DWT.mod[order(row.names(TD_DWT.mod)), ]
View(TD_DWT.mod2)
TD_DWT.mod11=t(TD_DWT.mod2)
TD_DWT.mod12=as.data.frame(TD_DWT.mod11)
TD_DWT.mod13=setDT(TD_DWT.mod12, keep.rownames = TRUE)[]
colnames(TD_DWT.mod13)[1] <- "country"
clusteredData <- join(TD_DWT.mod13, hc3.csv, by='country', type='left', match='all')
clusteredData.mod=as.data.frame(clusteredData)
clusteredData.mod[]=lapply(clusteredData.mod, function(x) as.numeric(as.character(x)))
clusteredData.mod=cbind(TD_DWT.mod13$country, clusteredData.mod[,c(2:442)])
clusteredData.mod$cluster=factor(clusteredData.mod$cluster)
library(C50)
x.tree <- clusteredData.mod[,2:441]
y.tree=clusteredData.mod[,442]
model <- C50::C5.0( x.tree, y.tree )
summary( model )
tapply(clusteredData.mod$SL.EMP.TOTL.SP.FE.ZS.1996, clusteredData.mod$cluster, mean)
tapply(clusteredData.mod$SL.UEM.1524.FE.ZS.2007, clusteredData.mod$cluster, mean)
tapply(clusteredData.mod$SL.GDP.PCAP.EM.KD.2003, clusteredData.mod$cluster, mean)
tapply(clusteredData.mod$SL.TLF.CACT.FE.ZS.1999, clusteredData.mod$cluster, mean)
tapply(clusteredData.mod$SP.ADO.TFRT.1996, clusteredData.mod$cluster, mean)
tapply(clusteredData.mod$SL.EMP.1524.SP.ZS.2012, clusteredData.mod$cluster, mean)
tapply(clusteredData.mod$SL.EMP.TOTL.SP.ZS.1996, clusteredData.mod$cluster, mean)
tapply(clusteredData.mod$SL.UEM.1524.ZS.1994, clusteredData.mod$cluster, mean)
