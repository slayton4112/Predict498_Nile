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


########################################
# Load & Prep
########################################
# Target Variable is "GDP per person employed (constant 1990 PPP $)"
# SL.GDP.PCAP.EM.KD

series_ref = tbl_df(as.data.frame(WDI_data[1])) # full list of WDI data series
country_ref = tbl_df(as.data.frame(WDI_data[2])) # extra country information
wdi = filter(series_ref, series.sourceDatabase == "World Development Indicators") # filter on WDI indicators
wdi_vars = as.vector(unique(wdi$series.indicator)) # create vector of all WDI indicator names

gdp = tbl_df(WDI(indicator = wdi_vars, country = 'all', start=1991, end=2015)) # WDI indicator dataframe

gdp = merge(gdp, country_ref[, c(2,4)], by.x = "iso2c", by.y = "country.iso2c", all.x = TRUE) # add region type
gdp = filter(gdp, country.region != "Aggregates") # drop Aggregates type



########################################
# EDA
########################################


summary(gdp)
head(dat)
length(unique(gdp$country)) #214 countries in total

# How many are missing GDP?
gdp$missing = is.na(gdp$SL.GDP.PCAP.EM.KD)*1 # create missing flag

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





# Line Chart
ggplot(gdp, aes(year, SL.GDP.PCAP.EM.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')

# Boxplot of GDP
p <- ggplot(filter(train.data.wide, year >= 1990), aes(year, SL.GDP.PCAP.EM.KD))
p + geom_boxplot() + geom_jitter(width = 0.2)

p <- ggplot (filter(train.data.wide, year >= 1990), aes(year, SL.GDP.PCAP.EM.KD)) +
  p + geom_line(aes(group=country.name)) +
  facet_wrap(~ country.name)