# Descriptive Statistics and Analysis of dataset


# importing the data set 
df = read.csv("C:\\Users\\utpal\\Desktop\\R PROJECT\\sucidedataset.csv")

# choosing rows concerning United States only
df = df[26849:27220,]


# descriptive statistics -----------------------------------------------> 

#average number of suicides in America
mean(df$suicides_no)

# simple function to get mean of number of suicides for a particular year. 
mean_getter = function(df_name, year, col_name){
  a = c()
  y = as.integer(year)
  c = col_name
  for (i in row.names(df_name)){
    if (df_name[as.character(i), 'year'] == y){
      a = append(a, df_name[as.character(i), col_name])
    }
  }
  mean(a)
}

# Average number of suicides in America in the year 1985
mean_getter(df, 1985, 'suicides_no')

# Average number of suicides in America in the year 1995
mean_getter(df, 1995, 'suicides_no')

# Average number of suicides in America in the year 2005
mean_getter(df, 2005, 'suicides_no')

# Average number of suicides in America in the year 2015
mean_getter(df, 2015, 'suicides_no')


# median suicide cases 

median_getter = function(df_name, year, col_name){
  a = c()
  y = as.integer(year)
  c = col_name
  for (i in row.names(df_name)){
    if (df_name[as.character(i), 'year'] == y){
      a = append(a, df_name[as.character(i), col_name])
    }
  }
  median(a)
}


# median suicide cases in the year 1985
median_getter(df, 1985, 'suicides_no')

# median suicide cases in the year 1995
median_getter(df, 1995, 'suicides_no')

# median suicide cases in the year 2005
median_getter(df, 2005, 'suicides_no')

# median suicide cases in the year 2015
median_getter(df, 2015, 'suicides_no')


# standard deviation 
std_getter = function(df_name, year, col_name){
  a = c()
  y = as.integer(year)
  c = col_name
  for (i in row.names(df_name)){
    if (df_name[as.character(i), 'year'] == y){
      a = append(a, df_name[as.character(i), col_name])
    }
  }
  sd(a)
}

# standard deviation for the year 1985
std_getter(df, 1985, 'suicides_no')


# standard deviation for the year 1995
std_getter(df, 1995, 'suicides_no')


# standard deviation for the year 2005
std_getter(df, 2005, 'suicides_no')


# standard deviation for the year 2015
std_getter(df, 2015, 'suicides_no')


# finding out if there is any relationship b/w Human development index and
# number of suicides

anyNA(df)
library(imputeTS)
df = na_mean(df)

# correlation

# a simple function to get desired dataframe for calculating correlation
correlation_dataframe_getter = function(age){
  suicides_per100k2 = c()
  HDI = c()
  for (f in c(1985: 2015)){
    suicides_per100k = c()
    counter = 0
    for (i in row.names(df)){
      if (df[i, 'year'] == as.integer(f)){
        if (df[i, 'age'] == as.character(age)){
          suicides_per100k = append(suicides_per100k, df[i, 'suicides.100k.pop'])
          suicides_per100k = sum(suicides_per100k)
          counter = counter + 1
          if (counter == 2){
            HDI = append(HDI, df[i, 'HDI.for.year'])
            suicides_per100k2 = append(suicides_per100k2, suicides_per100k)
            counter = 0
          }
        }
      }
    }
  }
  d = data.frame(suicides_per100k2, HDI)
  return(d)
}

# finding out correlation b/w HDI and suicides per 100k population among 
# different age groups

# 5-14 years
a = correlation_dataframe_getter('5-14 years')
cor(a$suicides_per100k2, a$HDI)
plot(a, xlab = 'suicides per 100k people', type = 'l')
hist(a$suicides_per100k2, xlab = 'suicides per 100k', 
     main = 'Histogram of suicides per 100k')

# for 15-24 years
a = correlation_dataframe_getter('15-24 years')
cor(a$suicides_per100k2, a$HDI)
plot(a, xlab = 'suicides per 100k people', type = 'l')
hist(a$suicides_per100k2, xlab = 'suicides per 100k', 
     main = 'Histogram of suicides per 100k')

# for 25-34 years
a = correlation_dataframe_getter('25-34 years')
cor(a$suicides_per100k2, a$HDI)
plot(a, xlab = 'suicides per 100k people', type = 'l')
hist(a$suicides_per100k2, xlab = 'suicides per 100k', 
     main = 'Histogram of suicides per 100k')

# for 35-54 years
a = correlation_dataframe_getter('35-54 years')
cor(a$suicides_per100k2, a$HDI)
plot(a, xlab = 'suicides per 100k people', type = 'l')
hist(a$suicides_per100k2, xlab = 'suicides per 100k', 
     main = 'Histogram of suicides per 100k')

# for 55-74 years age group
a = correlation_dataframe_getter('55-74 years')
cor(a$suicides_per100k2, a$HDI)
plot(a, xlab = 'suicides per 100k people', type = 'l')
hist(a$suicides_per100k2, xlab = 'suicides per 100k', 
     main = 'Histogram of suicides per 100k')

# for 75+ years age group
a = correlation_dataframe_getter('75+ years')
cor(a$suicides_per100k2, a$HDI)
plot(a, xlab = 'suicides per 100k people', type = 'l')
hist(a$suicides_per100k2, xlab = 'suicides per 100k', 
     main = 'Histogram of suicides per 100k')


# =======================================================================

# MAKING PREDICTIONS FOR SUICIDE IN AMERICA FOR DIFFERENT AGE GROUPS

# creating an appropriate data frame for making suicide/100k population 
# prediction

# a simple function for creating a data frame required for prediction.

pred_df_getter = function(){
  
  suicides = c()
  year = c()
  
  for (f in c(1985: 2015)){
    counter = 0
    suicides2 = c()
    for (i in row.names(df)){
      
      
      if (df[i, 'year'] == as.integer(f)){
        suicides2 = append(suicides2, df[i, 'suicides.100k.pop'])
        suicides2 = sum(suicides2)
        counter = counter + 1
        if (counter == 12){
          suicides = append(suicides, suicides2)
          year = append(year, df[i, 'year'])
        }
      }
    }
    a = data.frame(year, suicides)
    
  }
  return(a)
}

pred_data_frame = pred_df_getter()

# making predictions 

# a simple plot to see the suicides over time. 
plot(pred_data_frame$year, pred_data_frame$suicides, type = 'l', 
     xlab = 'Year', ylab = 'Suicides')
# There was a decline in suicides b/w year 2000-2008
# But after 2008 we see a sharp rise in suicides because of economic 
# crisis of 2008

# making a time series object
t = ts(pred_data_frame, frequency = 31, start = c(1, 1))
t2 = decompose(t)

# applying Arima model for predictions
library(forecast)
e = auto.arima(t[,2])
f = forecast(e, 10)
plot(f, ylab = 'Suicides')

print(f)

