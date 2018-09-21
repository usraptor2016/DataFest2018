library(tidyverse)
library(corrgram)
#source('D:/R/DataFest/importData.R')

stateTitleStat = function(data, title, state){
  if(count(data[data$stateProvince == state & data$normTitle == title,])$n > 1000){
    mean = mean(data[data$normTitle == title & data$stateProvince == state,]$estimatedSalary)
    var = var(data[data$normTitle == title & data$stateProvince == state,]$estimatedSalary)
    return(c(mean, var**.5))
    
  }else {
    return (c(NULL, NULL))
  }
}

UStitleStat = function(data, title){
  mean = mean(data[data$normTitle == title,]$estimatedSalary)
  var = var(data[data$normTitle == title,]$estimatedSalary)
  return(c(mean, var**.5))
}

plotStateTitle = function(data, title){
  stateList = unique(data[data$normTitle == title,]$stateProvince)
  for(j in stateList){
    hist(data[data$normTitle == title & data$stateProvince == j,]$estimatedSalary,
         main = paste(title, " ", j), xlab = "salary")
  }
}

plotTitle = function(data, title){
  hist(data[data$normTitleCategory == title,]$estimatedSalary, main = title, xlab = "salary")
}
data.us.ca <- data %>% 
  filter(country == "US", stateProvince == "CA")
data.us.oh <- data %>% 
  filter(country == "US", stateProvince == "OH")
data.us.ny <- data %>% 
  filter(country == "US", stateProvince == "NY")
data.us.ca %>% 
  group_by(jobId) %>% # group by jobId (there are many rows with the same jobid)
  filter(row_number() == 1) %>% # pick the first row for each jobid
  ungroup() %>%
  select(estimatedSalary, avgOverallRating, numReviews, experienceRequired, clicks) %>%
  corrgram(order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, 
           main="Correlation Table of California")
data.us.ny %>% 
  group_by(jobId) %>% # group by jobId (there are many rows with the same jobid)
  filter(row_number() == 1) %>% # pick the first row for each jobid
  ungroup() %>%
  select(estimatedSalary, avgOverallRating, numReviews, experienceRequired, clicks) %>%
  corrgram(order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, 
           main="Correlation Table of New York")
data.us.oh %>% 
  group_by(jobId) %>% # group by jobId (there are many rows with the same jobid)
  filter(row_number() == 1) %>% # pick the first row for each jobid
  ungroup() %>%
  select(estimatedSalary, avgOverallRating, numReviews, experienceRequired, clicks) %>%
  corrgram(order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, 
           main="Correlation Table of Ohio")

#dataset = select(data.us.ca, estimatedSalary, avgOverallRating, numReviews,
#                 experienceRequired, jobAgeDays, clicks)

##

category.vs.salary = data %>% group_by(jobId) %>% filter(row_number() == 1) %>% 
  ungroup() %>% select(normTitleCategory, estimatedSalary)

category.vs.salary <- na.omit(category.vs.salary)
category.vs.salary = category.vs.salary %>% filter(normTitleCategory == 'techinfo')

category.vs.salary.byState = data %>% filter(row_number() == 1) %>% select( normTitleCategory,
                                                                            estimatedSalary,
                                                                            stateProvince) %>% group_by(stateProvince)
category.vs.salary.byState <- na.omit(category.vs.salary.byState)
category.vs.salary.byState = category.vs.salary.byState %>% filter(normTitleCategory == 'techinfo')
stateList = c('OH','CA','NY')
category.vs.salary.byState = category.vs.salary.byState %>% filter(stateProvince == stateList)


hist(category.vs.salary[category.vs.salary$normTitleCategory == 'techinfo',]$estimatedSalary, main = 'techinfo', xlab = "salary")
vecMean = mean(category.vs.salary[category.vs.salary$normTitleCategory == 'techinfo',]$estimatedSalary)
vecSD = var(category.vs.salary[category.vs.salary$normTitleCategory == 'techinfo',]$estimatedSalary)**.5

for(j in stateList){
  hist(category.vs.salary.byState[category.vs.salary.byState$normTitleCategory == 'techinfo' & category.vs.salary.byState$stateProvince == j,]$estimatedSalary,
       main = paste('techinfo', " ", j), xlab = "salary")
  vecMean = c(vecMean, mean(category.vs.salary.byState[category.vs.salary.byState$normTitleCategory == 'techinfo' & category.vs.salary.byState$stateProvince == j,]$estimatedSalary))
  vecSD = c(vecSD, var(category.vs.salary.byState[category.vs.salary.byState$normTitleCategory == 'techinfo' & category.vs.salary.byState$stateProvince == j,]$estimatedSalary))
}
print(c('US', stateList))
print(vecMean)
print(vecSD)