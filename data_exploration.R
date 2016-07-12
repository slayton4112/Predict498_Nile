# Set working directory -- you'll need to change this on your machine
library(reshape2)
library(lattice)
library(rpart)
library(randomForest)
library(tree)
library(partykit)

setwd('C:/Users/Scott Layton/Documents/Predict498_Nile')

#pull in training data
training_dta = read.csv('TrainingSet.csv')

# Columns in File
names(training_dta)

names(training_dta) <- gsub("\\.","",names(training_dta))

names(training_dta)


# Transpose data into long format for ease of use
training_dta.t <- melt(training_dta[ , !(names(training_dta) %in% "index")],varying=2:37, sep="YR", direction='long', id=c("CountryName", "SeriesCode", "SeriesName"), na.rm=T)

head(training_dta.t)

unique(training_dta.t$variable)

training_dta.t$year <- as.numeric(substr(training_dta.t$variable,8,11))

unique(training_dta.t$year)

# create giant set of columns for different measures

unique(training_dta.t$SeriesName)

# yikes that's a lot of columns to be made

names(training_dta.t)

unique(training_dta.t$SeriesCode)

training_dta.t <- dcast(training_dta.t, CountryName + year ~ SeriesCode, value.var="value")


head(training_dta.t)


names(training_dta.t)


# response variable

# NY.GDP.PCAP.PP.KD

grep("^NY.GDP.PCAP.PP.KD$", colnames(training_dta.t))

attach(training_dta.t)

xyplot(NY.GDP.PCAP.PP.KD ~ year | CountryName, group = CountryName, training_dta.t, t='l')


form <- paste(" ~ ",paste(names(training_dta.t)[818], collapse = ' + '), sep=" ")

form

densityplot( ~NY.GDP.PCAP.PP.KD| CountryName, data=training_dta.t,plot.points=F, ref=T, auto.key=list(columns=3),scales=list(relation="free"))

#z <- cor(training_dta.t[,2:length(colnames(training_dta.t))],use="pairwise.complete.obs")
#levelplot(z)




# random forest for complete rows -- Not working

training_dta.t.comp <- training_dta.t[complete.cases(training_dta.t),]

drops = c("1.2","2.1","3.2","4.1","5.1","6.1","6.7","7.8","8.16")
training_dta.t.sel <-training_dta.t[ , !(names(training_dta.t) %in% drops)]

nrow(training_dta.t.sel)

unique(training_dta.t.sel$NY.GDP.PCAP.PP.KD)


training_dta.t$CountryName <- as.factor(training_dta.t$CountryName)

fit.tree = rpart(NY.GDP.PCAP.PP.KD ~ ., data=training_dta.t, method="anova")



plot(fit.tree, uniform=TRUE)
text(fit.tree)

unique(training_dta.t$CountryName)

fit.ctree = cforest(NY.GDP.PCAP.PP.KD ~ ., data=training_dta.t, na.action=na.pass)

plot(fit.ctree)
 