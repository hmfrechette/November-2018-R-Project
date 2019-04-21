#set working directory first

library(Hmisc)
library(stringr)
library(lubridate)
library(dplyr)
library(data.table)
library(microbenchmark)
library(ggplot2)

MN_data <- fread('MN Crime.csv')

#1. inspect data

glimpse(MN_data) #convert reporteddate and time of offense to dates

summary(MN_data)

sum(is.na(MN_data$ReportedDate)) #0 at this point

#2. do data cleansing 

#drop duplicates
cleaned_MN_data <- unique(MN_data)

#parse the reporteddate column into date and time; this will give you proper type
cleaned_MN_data$ReportedDate<- parse_date_time(cleaned_MN_data$ReportedDate, 'Ymd HMS')
class(cleaned_MN_data$ReportedDate) #"POSIXct" "POSIXt"
sum(is.na(cleaned_MN_data$ReportedDate)) #20,127 NAs now, not good

#separate date from reported date column to make its own column
cleaned_MN_data$incident_date_occurrence <- as.Date(cleaned_MN_data$ReportedDate)
class(cleaned_MN_data$incident_date_occurrence) #date

#now, superimpose the 'time of offense' column with the reported_incident_date column
cleaned_MN_data$incident_occurrence_date_time <- as.POSIXct(paste(cleaned_MN_data$incident_date_occurrence, cleaned_MN_data$'Time of Offense'), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
class(cleaned_MN_data$incident_occurrence_date_time) #"POSIXct" "POSIXt" 

#create new column so that reported incident and time of occurrence are near each other
cleaned_MN_data$ReportedDate2 <- cleaned_MN_data$ReportedDate

#see if the number of NAs has increased at all
sum(is.na(cleaned_MN_data$incident_occurrence_date_time)) #20127

#find the difference between when incident happened and when it was reported
cleaned_MN_data$time_differences <- difftime(cleaned_MN_data$ReportedDate2, cleaned_MN_data$incident_occurrence_date_time, units = 'hours')

#write a function to either add 24 hours or keep it the same

super_function <- function(offense_time, reported_time){
  if(is.na(offense_time)){
    return(NA)
  } else if(is.na(reported_time)){
    return(NA)
  } else if(offense_time > reported_time){
    return(offense_time - dhours(24))
  }else{
    return(offense_time)
  }
}

#super_function(MN_data$incident_occurrence_date_time[1], MN_data$ReportedDate2[1])
cleaned_MN_data$adjusted_incident_date <- mapply(super_function, cleaned_MN_data$incident_occurrence_date_time,cleaned_MN_data$ReportedDate2)


cleaned_MN_data$adjusted_incident_date <- as.POSIXct(cleaned_MN_data$adjusted_incident_date, origin = "1970-01-01", tz = "UTC")

#Part 2**************************************************************************

#Insights on Vehicle related incidents vs other incidents

#get days of the week from incidence_occurrence_date_time
cleaned_MN_data$day_of_week <- as.Date(cleaned_MN_data$adjusted_incident_date)
class(cleaned_MN_data$day_of_week) #Date

#show which day of the week it occurred on
cleaned_MN_data$day_of_week <- weekdays(as.Date(cleaned_MN_data$day_of_week))

#extract the month in which incident occurred
cleaned_MN_data$month_of_incident <- as.Date(cleaned_MN_data$adjusted_incident_date)
class(cleaned_MN_data$month_of_incident) #date
cleaned_MN_data$month_of_incident <- month(as.Date(cleaned_MN_data$month_of_incident))


#Insights on time taken to report incidents

#get difference between adjusted (correct) incident date & time it was actually reported
cleaned_MN_data$corrected_time_difference <- difftime(cleaned_MN_data$ReportedDate2, cleaned_MN_data$adjusted_incident_date, units = 'hours')
class(cleaned_MN_data$corrected_time_difference) #difftime

#find when time to report was longer than 30 minutes from the time incident occurred
longer_30_mins <- cleaned_MN_data%>%
  filter(corrected_time_difference >= "0.5")

#convert difftime column to numeric
cleaned_MN_data$corrected_time_difference <- as.numeric(cleaned_MN_data$corrected_time_difference)

#find average length of time it took to report a crime
mean(cleaned_MN_data$corrected_time_difference, na.rm = TRUE) #8.147708 hours

#number of incidents per day of the week?
incidents_by_weekday <- cleaned_MN_data%>%
  group_by(day_of_week)%>%
  summarise(n=n())

write.csv(cleaned_MN_data, file = 'MN_cleaned_data.csv')




