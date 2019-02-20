rm(list = ls())
#set working directory
setwd("C:/Users/Subbu/Documents/EA")


x = c("ggplot2", "corrgram", "DMwR", "caret", "e1071")
#Used to read excel files
install.packages("xlsx")
library("xlsx")
#tibble is a modern dataframe
#library(tibble)

#load Packages
lapply(x, require, character.only = TRUE)

# Reading data
dfabsenteeism = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1)

dim(dfabsenteeism)


######################################### Missing Values Analysis##################################3
missing_value = (data.frame(colSums(is.na(dfabsenteeism)))*100/nrow(dfabsenteeism))
colnames(missing_value) = c("Missing_percentage")
missing_value$Missing_percentage = (missing_value$Missing_percentage/nrow(dfabsenteeism)) * 100
missing_value

### Imputing values in the dataset ### start


#null values of Reason.for.absence are put equal to 27 since Absenteeism.time.in.hours for the observations having null values is < 10 hrs.
dfabsenteeism$Reason.for.absence[is.na(dfabsenteeism$Reason.for.absence)] = 27
#Zero category of 'Reason for absence' column value to category 26(i.e. unjustified absence).
dfabsenteeism$Reason.for.absence[dfabsenteeism$Reason.for.absence==0] = 26
#Imputing Month.of.absence null value to 10.
dfabsenteeism$Month.of.absence[is.na(dfabsenteeism$Month.of.absence)] = 10
#Disciplinary.failure missing values put to 0 because mode() of absenteeism_data$Disciplinary.failure =0
dfabsenteeism$Disciplinary.failure[is.na(dfabsenteeism$Disciplinary.failure)] = 0

#Imputing missing values in Transportation.expense with employee id

for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Transportation.expense)]){
  
  dfabsenteeism$Transportation.expense[is.na(dfabsenteeism$Transportation.expense) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Transportation.expense[dfabsenteeism$ID==i],na.rm = T)
}
#Imputing missing values in Distance.from.Residence.to.Work with employee id

for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Distance.from.Residence.to.Work)]){
  dfabsenteeism$Distance.from.Residence.to.Work[is.na(dfabsenteeism$Distance.from.Residence.to.Work) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Distance.from.Residence.to.Work[dfabsenteeism$ID==i],na.rm = T)
}
#Imputing missing values in Service.time with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Service.time)]){
  dfabsenteeism$Service.time[is.na(dfabsenteeism$Service.time) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Service.time[dfabsenteeism$ID==i],na.rm = T)
}
#Imputing missing values in Age with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Age)]){
  dfabsenteeism$Age[is.na(dfabsenteeism$Age) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Age[dfabsenteeism$ID==i],na.rm = T)
}

#Converting the variable into numeric
dfabsenteeism$Work.load.Average.day = as.numeric(dfabsenteeism$Work.load.Average.day)

#making a dataframe having Month of absence and Hit target with null Work.load.Average.day
dfWLD = data.frame(dfabsenteeism$Month.of.absence[is.na(dfabsenteeism$Work.load.Average.day)],dfabsenteeism$Hit.target[is.na(dfabsenteeism$Work.load.Average.day)])
dfWLD

#Work.load.Average.day missing values are imputed using Month.of.absence and Work.load.Average.day

for (i in 1:10){
  dfabsenteeism$Work.load.Average.day[(is.na(dfabsenteeism$Work.load.Average.day) & 
                                         dfabsenteeism$Month.of.absence==dfWLD[i,1]) & dfabsenteeism$Hit.target==dfWLD[i,2]] = 
    mean(dfabsenteeism$Work.load.Average.day[dfabsenteeism$Month.of.absence==dfWLD[i,1] & dfabsenteeism$Hit.target==dfWLD[i,2]],na.rm = T)
}

#making a dataframe having Month of absence and wotk load  with null Hit target

dfHT = data.frame(m1=dfabsenteeism$Month.of.absence[is.na(dfabsenteeism$Hit.target)],w1=dfabsenteeism$Work.load.Average.day[is.na(dfabsenteeism$Hit.target)])
dfHT
#Hit.target missing values are imputed using Month.of.absence and Work.load.Average.day

for (i in 1:6){
  dfabsenteeism$Hit.target[(is.na(dfabsenteeism$Hit.target) & dfabsenteeism$Month.of.absence==dfHT[i,1]) & dfabsenteeism$Work.load.Average.day==dfHT[i,2]] = mean(dfabsenteeism$Hit.target[dfabsenteeism$Month.of.absence==dfHT[i,1] & dfabsenteeism$Work.load.Average.day==dfHT[i,2]],na.rm = T)
}

#Imputing missing values in Education with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Education)]){
  dfabsenteeism$Education[is.na(dfabsenteeism$Education) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Education[dfabsenteeism$ID==i],na.rm=T)
}

#Imputing missing values in Son with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Son)]){
  dfabsenteeism$Son[is.na(dfabsenteeism$Son) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Son[dfabsenteeism$ID==i],na.rm=T)
}

#Imputing missing values in Social.drinker with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Social.drinker)]){
  dfabsenteeism$Social.drinker[is.na(dfabsenteeism$Social.drinker) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Social.drinker[dfabsenteeism$ID==i],na.rm=T)
}

#Imputing missing values in Social.smoker with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Social.smoker)]){
  
  dfabsenteeism$Social.smoker[is.na(dfabsenteeism$Social.smoker) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Social.smoker[dfabsenteeism$ID==i],na.rm=T)
}

#Imputing missing values in Pet with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Pet)]){
  print(i)
  dfabsenteeism$Pet[is.na(dfabsenteeism$Pet) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Pet[dfabsenteeism$ID==i],na.rm=T)
}

#Imputing missing values in Weight with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Weight)]){
  dfabsenteeism$Weight[is.na(dfabsenteeism$Weight) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Weight[dfabsenteeism$ID==i],na.rm=T)
}

#Imputing missing values in Height with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Height)]){
  dfabsenteeism$Height[is.na(dfabsenteeism$Height) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Height[dfabsenteeism$ID==i],na.rm=T)
}

#Imputing missing values in Body.mass.index with employee id
for (i in dfabsenteeism$ID[is.na(dfabsenteeism$Body.mass.index)]){
  dfabsenteeism$Body.mass.index[is.na(dfabsenteeism$Body.mass.index) & dfabsenteeism$ID==i] = mean(dfabsenteeism$Body.mass.index[dfabsenteeism$ID==i],na.rm=T)
}

#Reason.for.absence column has been used to impute missing value for Absenteeism.time.in.hours 

for (i in dfabsenteeism$Reason.for.absence[is.na(dfabsenteeism$Absenteeism.time.in.hours)]){
  dfabsenteeism$Absenteeism.time.in.hours[is.na(dfabsenteeism$Absenteeism.time.in.hours) & dfabsenteeism$Reason.for.absence==i] = mean(dfabsenteeism$Absenteeism.time.in.hours[dfabsenteeism$Reason.for.absence==i],na.rm=T)
}
### Miising values imputed  ####

# density Plots for distribution
#Transportation.expense 
hist(dfabsenteeism$Transportation.expense,prob = TRUE,xlab = 'Transportation.expense')
lines(density(dfabsenteeism$Transportation.expense))
#Distance.from.Residence.to.Work
hist(dfabsenteeism$Distance.from.Residence.to.Work,prob = TRUE,xlab = 'Distance.from.Residence.to.Work')
lines(density(dfabsenteeism$Distance.from.Residence.to.Work))
#Service.time
hist(dfabsenteeism$Service.time,prob = TRUE,xlab = 'Service.time')
lines(density(dfabsenteeism$Service.time))
#Age
hist(dfabsenteeism$Age,prob = TRUE,xlab = 'Age')
lines(density(dfabsenteeism$Age))
#Work.load.Average.day
hist(dfabsenteeism$Work.load.Average.day,prob = TRUE,xlab = 'Work.load.Average.day')
lines(density(dfabsenteeism$Work.load.Average.day))
#Hit.target
hist(dfabsenteeism$Hit.target,prob = TRUE,xlab = 'Hit.target')
lines(density(dfabsenteeism$Hit.target))
#Weight
hist(dfabsenteeism$Weight,prob = TRUE,xlab = 'Weight')
lines(density(dfabsenteeism$Weight))
#Height
hist(dfabsenteeism$Height,prob = TRUE,xlab = 'Height')
lines(density(dfabsenteeism$Height))
#Body.mass.index
hist(dfabsenteeism$Body.mass.index,prob = TRUE,xlab = 'Body.mass.index')
lines(density(dfabsenteeism$Body.mass.index))
###    All continuous variables have skewed distribution.

##########################Outlier Analysis##########################
num_col =c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours','Transportation.expense',
           'Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target','Work.load.Average.day')
cat_col = c('')
for (i in 1:length(num_col))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (num_col[i]), x = 'Absenteeism.time.in.hours'),data = dfabsenteeism) +
           stat_boxplot(geom = "errorbar", width = 0.5) +geom_boxplot(outlier.colour="blue", fill = "skyblue",
                                                                      outlier.shape=18,outlier.size=1, notch=FALSE) +labs(y=num_col[i],x="Absenteeism in Hours")+
           ggtitle(paste("Box plot of ",num_col[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)

#Capping outliers

for (i in c('Transportation.expense','Service.time','Age','Work.load.Average.day','Hit.target','Height','Absenteeism.time.in.hours')){
  qnt = quantile(dfabsenteeism[,i], probs=c(.25, .75), na.rm = T)
  iqr1 = qnt[2]-qnt[1]
  min1 = qnt[1]-1.5*iqr1
  max1 = qnt[2]+1.5*iqr1
  dfabsenteeism[,i][dfabsenteeism[,i]<min1] = min1
  dfabsenteeism[,i][dfabsenteeism[,i]>max1] = max1
}

#Correlation Analysis, feature selection
#Converting catcols to factor as they are categorical
catcols = c('Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')

for (i in catcols){
  dfabsenteeism[,i] = as.factor(dfabsenteeism[,i])
}
for(i in catcols){
  print(i)
  print(chisq.test(table(dfabsenteeism$Absenteeism.time.in.hours,dfabsenteeism[,i]), correct = FALSE))
}

# Chi-square test for correlation between factors
pval = c()

#Calculating & storing p-values in vector pval from chisquare test

for(i in catcols){
  for(j in catcols){
    chi2 = chisq.test(dfabsenteeism[,i],dfabsenteeism[,j])
    pval = c(pval,chi2$p.value)
  }
}

#converting pval to matrix m1

mPval = matrix(pval,ncol=10)
mPval

#Converting m1 to dataframe chi_df
chi_df = data.frame(mPval)
#Setting row names to catcols
row.names(chi_df) = catcols
#Setting column names to catcols
colnames(chi_df) = catcols
chi_df


#categorical variables having p-values<0.05 have dependence on Reason.for.absence.
#So, all categorical variables except Reason.for.absence and Day.of.the.week will be dropped.

dfabsenteeism[,c('Month.of.absence','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')] = list(NULL)

#Correlation between continuous independent variables
#cor(dfabsenteeism[,4:13])

corrgram(dfabsenteeism[,4:13], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

dfabsenteeism[,c('Body.mass.index')] = list(NULL)
# Correlation amongst continuous independent variables < 0.95
# Correlation between every independent variable & dependent variable < 0.2
# This means that there is no relationship between any independent variable and dependent variable.
# Relationship between Reason.for.absence and Absenteeism.time.in.hours

# Aggregating Absenteeism.time.in.hours by Reason.for.absence

AbsReasons = aggregate(dfabsenteeism$Absenteeism.time.in.hours, by=list(Category=dfabsenteeism$Reason.for.absence), FUN=sum)
AbsReasons

#Calculating absenteeism_dataeeism time by category as percent of total time in column Absence
AbsReasons$Absence = (AbsReasons$x/sum(dfabsenteeism$Absenteeism.time.in.hours))*100
AbsReasons = AbsReasons[order(AbsReasons$Absence),]
AbsReasons

barplot(AbsReasons$Absence,names.arg=AbsReasons$Category,xlab="Reason.for.absence",ylab="Absence",col="blue")

#Top 3 categories in terms of Absence time are:
# 1. Category-13:Diseases of the musculoskeletal system and connective tissue - 12.79 % of total time
# 2. Category-23:medical consultation - 11.22 % of total time
# 3. Category-19:Injury, poisoning and certain other consequences of external causes - 10.63 % of total time
# 4. Category 28:dental consultation - 8.54 % 0f total time
# 5. Category 26:unjustified absence - 7.66 % of total time

#Forecasting absenteeism time in hours per month for 2011

# Reading data
dfForecast = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1)

dim(dfForecast)
#Imputing missing values for Month.of.absence,Reason.for.absence,Absenteeism.time.in.hours
dfForecast$Month.of.absence[is.na(dfForecast$Month.of.absence)] = 10
dfForecast$Reason.for.absence[is.na(dfForecast$Reason.for.absence)] = 27
dfForecast$Reason.for.absence[dfForecast$Reason.for.absence==0] = 26

for (i in dfForecast$Reason.for.absence[is.na(dfForecast$Absenteeism.time.in.hours)]){
  dfForecast$Absenteeism.time.in.hours[is.na(dfForecast$Absenteeism.time.in.hours) & dfForecast$Reason.for.absence==i] = median(dfForecast$Absenteeism.time.in.hours[dfForecast$Reason.for.absence==i],na.rm=T)
}


#Converting Month.of.absence to factor
dfForecast$Month.of.absence = as.factor(dfForecast$Month.of.absence)
#Making a timeseries aggregating Absenteeism.time.in.hours by Month.of.absence
monthly_absence = aggregate(dfForecast$Absenteeism.time.in.hours,by=list(Category=dfForecast$Month.of.absence),FUN=sum)
monthly_absence = monthly_absence[2:13,]
monthly_absence


#Calculating absenteeism_dataeeism time as percent of total time in column absenteeism_datahours
monthly_absence$absenteeism_datahours = monthly_absence$x/3
row.names(monthly_absence) = monthly_absence$Category
monthly_absence

# Modelling time series using arima
tsAbs = ts(monthly_absence$absenteeism_datahours)
class(tsAbs)
tsAbs

#Visualizing timeseries
plot(tsAbs)

#Checking stationarity - Augmented Dickey-Fuller Test
library(tseries)
adf.test(tsAbs, alternative="stationary", k=0)

# Augmented Dickey-Fuller Test
# data:  tsdata
# Dickey-Fuller = -3.3984, Lag order = 0, p-value = 0.078
# alternative hypothesis: stationary

#time series tsdata is not stationary as determined by p-value of 0.078(>0.05).

#We will be subtracting shifted(single lag) time series from original time series.

tsAbsLag = tsAbs - stats::lag((tsAbs),1)
plot(tsAbsLag)
#Doing Augmented Dickey-Fuller Test again
adf.test(tsAbsLag, alternative="stationary", k=0)
#ACF plot
acf(tsAbsLag)
#value of p should be 0.
#PACF plot
pacf(tsAbsLag)
#value of q should be 0.

library(forecast)
model = arima(tsAbsLag,c(4,0,9))
fit1 = fitted(model)
residuals1 = tsAbsLag - fit1
residuals1
sum(residuals1**2)


#Arima with order=(4,0,9) gives us lowest RSS of 2222.32 so we will order=(4,0,9)

plot(tsAbsLag)
lines(fit1)
dfabsenteeism2011 = predict(model,n.ahead = 12)
dfabsenteeism2011
#Scaling absenteeism_data2011 back to original
dfabsenteeism2011 = cumsum(dfabsenteeism2011$pred)
dfabsenteeism2011Temp = dfabsenteeism2011 + rep(tsAbs[4],12)
as.data.frame(dfabsenteeism2011Temp)
ts_2011 = ts(dfabsenteeism2011Temp)
df1 = as.data.frame(dfabsenteeism2011Temp)
df1
row.names(df1) = c(13:24)
ts_2011 = ts(df1$dfabsenteeism2011Temp,start=13)

ts_2011
#Plotting original timeseries & forecast values

plot(tsAbs,xlim=c(1,24))
lines(ts_2011)
