##############################
# PREDICT 498
# Joe Gwozdz
##############################


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
write.csv(gdp, file = "/Users/LosGuamuch/Desktop/gdp.csv") # double checking in Excel


########################################
# QA
########################################

# Loads Capstone RData file
load("~/Predict498_Nile/nile_data.RData")

summary(gdp)
head(gdp)
length(unique(gdp$country)) #217 countries in total

# print first 10 rows
head(gdp[, c(1:9)], n=10)

# print last 10 rows
tail(gdp[, c(1:9)], n=10)

# Check missingness for all variables
sapply(gdp, function(x) sum(is.na(x)))

# Summary of All Data
# StatsMeasures package
# includes variable, class, distinct values, missing, % miss, sample value
gdp_summary = contents(gdp)

# Summary of Categorical data
gdp_summary.cat = dqcategorical(gdp)
View(gdp_summary.cat)

# Summary of Numeric data
gdp_summary.num = dqcontinuous(gdp)
View(gdp_summary.num)

# Create Data Dictionary
gdp_dictionary = merge(gdp_summary, series_ref, by.x = "variable", by.y = "series.indicator",
                       all.x = TRUE) # merges variables with basic descriptives
gdp_dictionary = gdp_dictionary[,c(1:2, 7:10, 3:6)]

gdp_dictionary = merge(gdp_dictionary, gdp_summary.num, all.x = TRUE) # adds all numerical summary data
gdp_dictionary = gdp_dictionary[,c(1, 3:7, 2, 8:27)]

# Save Dictionary to Team Drive
write.csv(gdp_dictionary, file = "/Users/LosGuamuch/Predict498_Nile/gdp_dictionary.csv")

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


########################################
# Viz
########################################

# Line Chart
# Why is this not plotting all of the data points?
ggplot(subset(gdp, country == c("United States", "Mexico", "Canada", "United Kingdom")), aes(year, SL.GDP.PCAP.EM.KD, color=country)) +
  geom_line() + 
  xlab('Year') +
  ylab('GDP Per Capita')

# Histogram of GDP faceted by year
m <- ggplot(gdp, aes(SL.GDP.PCAP.EM.KD))
m <- m + geom_histogram(binwidth = 0.5)
m <- m + aes( y = ..density..)
m + facet_wrap(~ year)