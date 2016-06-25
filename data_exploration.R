# Set working directory -- you'll need to change this on your machine
setwd('/Users/scottlayton/Predict498_Nile')

#pull in training data
training_dta = read.csv('Training_set_values.csv')

#pull in training data labels -- This is the classification mapping
training_labels = read.csv('Training_set_labels.csv')

# join status group into main dataframe
training_dta <- merge(training_dta,training_labels,by="id")

# Columns in File
names(training_dta)

# Note: Functional, needs repair is small portion of data and will be difficult to predict
table(training_dta$status_group)
