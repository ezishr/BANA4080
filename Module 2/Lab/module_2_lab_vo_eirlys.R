# Q1 ----------------------------------------------------------------------
library(readr)
library(here)
df <- readr::read_csv(here("Module 2/lab", "lab2_data/blood_transfusion.csv"))
sum(is.na(df))
dim(df)
head(df, 10)
tail(df, 10)
df[100, 'Monetary']
mean(df[['Monetary']])
above_avg <- df[['Monetary']] > mean(df[['Monetary']])
df[above_avg, 'Monetary']


# Q2 ----------------------------------------------------------------------
df <- readr::read_csv(here('Module 2/Lab/lab2_data/PDI__Police_Data_Initiative__Crime_Incidents.csv')) 
dim(df)
sum(is.na(df))
colSums(is.na(df))
range(df[['DATE_REPORTED']])
table(df[['SUSPECT_AGE']])
sort(table(df['ZIP']), decreasing = TRUE)
table(df[['DAYOFWEEK']]) / sum(table(df[['DAYOFWEEK']]))
