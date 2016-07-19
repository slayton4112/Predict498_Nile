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
# EDA / QA
########################################

# Loads Capstone RData file
load("~/Predict498_Nile/nile_data.RData")

summary(gdp)
head(gdp)
length(unique(gdp$country)) #214 countries in total

# print first 10 rows
head(gdp[, c(1:9)], n=10)

# print last 10 rows
tail(gdp[, c(1:9)], n=10)

# How many are missing GDP?
gdp$missing = is.na(gdp$SL.GDP.PCAP.EM.KD)*1 # create missing flag

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

gdp_dictionary = merge(gdp_summary, gdp_summary.num, all.x = TRUE) # adds all numerical summary data

# Save Dictionary to Team Drive
write.csv(gdp_dictionary, file = "/Users/LosGuamuch/Predict498_Nile/gdp_dictionary.csv")

# Group by country and sum missing GDP
miss = gdp %>%
  group_by(country) %>%
  summarise(num_miss = sum(missing)) %>%
  arrange(desc(num_miss))

# Filter on countries missing GDP data
miss = filter(miss, num_miss == 25)

# Drop the countries that are missing GDP data
gdp <- gdp[!(gdp$country %in% miss$country),]
gdp$missing = NULL
length(unique(gdp$country)) # 174 countries leftover



########################################
# Viz
########################################

# Line Chart
ggplot(gdp, aes(year, SL.GDP.PCAP.EM.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')

# Boxplot of GDP
p <- ggplot(filter(train.data.wide, year >= 1991), aes(year, SL.GDP.PCAP.EM.KD))
p + geom_boxplot() + geom_jitter(width = 0.2)

p <- ggplot (filter(train.data.wide, year >= 1990), aes(year, SL.GDP.PCAP.EM.KD)) +
  p + geom_line(aes(group=country.name)) +
  facet_wrap(~ country.name)