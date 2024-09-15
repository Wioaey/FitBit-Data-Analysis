install.packages("viridis")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(viridisLite)
library(viridis)

dailyActivity <- read_csv("C:/Users/yosup/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
View(dailyActivity)

weightLog <- read_csv("C:/Users/yosup/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
View(weightLog)

sleepDay <- read_csv("C:/Users/yosup/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
View(sleepDay)

heartrate_seconds <- read_csv("C:/Users/yosup/Downloads/archive (3)/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
View(heartrate_seconds)


glimpse(dailyActivity)
glimpse(heartrate_seconds)
glimpse(sleepDay)
glimpse(weightLog)



#number of distinct Id's
n_distinct(weightLog$Id)

#counts the number of TRUE in column 'IsManualReport'
length(which(weightLog$IsManualReport==TRUE))

View(weightLog)

#counts the total number of NA in column 'Fat'
sum(is.na(weightLog$Fat))
length(weightLog$Fat)

#graphs the the weight change by date for each Id(user)
ggplot(data = weightLog, aes(x = Date, y = WeightKg, group = Id, color = as.character(Id)))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_blank())
#work on making the key box pretty


#number of distinct users in sleepDay
n_distinct(sleepDay$Id)
#number of rows in 
length(sleepDay$Id)

#date_range_sleepDay contains the lowest and highest date recorded in sleepDay
range(sleepDay$SleepDay)
#Found the number of recording for each user Id and then found the Standard Deviation of it
sdEntries <- sleepDay %>% count(Id)
View(sdEntries)
sd(sdEntries$n)

View(sleepDay)

tsr <-sum(sleepDay$TotalSleepRecords)
tsr

#finding the mean of both TotalMinutesAsleep and TotalTimeInBed
mean(sleepDay$TotalMinutesAsleep)
mean(sleepDay$TotalTimeInBed)

#Added the time diff between the 2 columns as mentioned above as TimeDiff into sleepDay
sleepDay <- transform(sleepDay, TimeDiff = abs(sleepDay$TotalMinutesAsleep - sleepDay$TotalTimeInBed))
View(sleepDay)



View(heartrate_seconds)
#found the max/min of the time column and the Value column
max(heartrate_seconds$Time)
min(heartrate_seconds$Time)
max(heartrate_seconds$Value)
min(heartrate_seconds$Value)

#the number of distinct Id's in the heartrate_seconds dataset
n_distinct(heartrate_seconds$Id)

#contains all of the Ids in the heartrate_seconds dataset
Id_hold <- unique(heartrate_seconds$Id)


#max_min_heart8 is a simplified version of heartrate_second. Contains the max, min of both Time and Value of
#of each respective Id and the count of its appearances
max_min_heart8 <- heartrate_seconds %>% group_by(Id) %>% summarize(across(everything(), max))
max_min_heart8 <- max_min_heart8 %>% rename(maxTime = Time, maxValue = Value)
temp <- heartrate_seconds %>% group_by(Id) %>% summarize(across(everything(), min))
View(temp)
max_min_heart8 <- cbind(max_min_heart8, temp$Time, temp$Value)
max_min_heart8 <- max_min_heart8 %>% rename(minTime = `temp$Time`, minValue = `temp$Value`)
max_min_heart8 <- max_min_heart8 %>% relocate(minTime,  .before = maxValue)
View(max_min_heart8)
 
max_min_heart8 <- cbind(max_min_heart8, heartrate_seconds %>% count(Id))
max_min_heart8 <- max_min_heart8 %>% select(-6)
max_min_heart8 <- max_min_heart8 %>% rename(Count = n)

View(dailyActivity)

#found the total active minutes and the added it next to TotalDistance and it's saved into temp1
max(dailyActivity %>% count(TrackerDistance)) - min(dailyActivity %>% count(TrackerDistance))
temp1 <- dailyActivity %>% rowwise() %>% mutate(totalMin = sum(across(ends_with("Minutes")), na.rm = T))
View(temp1)
temp1 <- temp1 %>% relocate(totalMin, .before= TrackerDistance )
temp1 <- temp1 %>% rename(TotalMinutes = totalMin)
View(temp1)

#summarized th dailyActivity by finding the max for each respective Id
dailyActivity %>% group_by(Id) %>% summarize(across(everything(), max))
View(maxmin_dailyActivity)
glimpse(maxmin_dailyActivity)
maxmin_dailyActivity <- dailyActivity %>% rename(TotalStep = Time, maxValue = Value)

#filtering out all the unlogged days of activity
active_days <- filter(dailyActivity, dailyActivity$LoggedActivitiesDistance > 0)
View(active_days)
#only 4 unique users had LoggedActivityDistance > 0
n_distinct(active_days$Id)


View(heartrate_seconds)

#merging dailyActivity and sleepDay dataset by Id and Date
#(this was because it contains everything but the data regarding sleep as in sleepDay)
sleepTemp <- sleepDay %>% separate(SleepDay, into = c("NewDate", "Hour", "AM_or_PM"), sep = " ") %>% rename
View(sleepTemp)
merged_data <- left_join(dailyActivity, sleepTemp, by = c('Id' = 'Id','ActivityDate' = 'NewDate'), relationship = "many-to-many") 
merged_data <- merged_data %>% relocate(NewDate, .before = TotalSteps)
n_distinct(merged_data$Id)
summary(merged_data)
View(merged_data)

#got the ActivityDates and then made a new column which showed the days of the week
merged_data <- merged_data %>% mutate(Weekday = weekdays(mdy(ActivityDate)))
View(merged_data)

#found the number of occurence per day and then made a graph comparing the weekdays
df <- data.frame(table(merged_data$Weekday))
View(merged_data)
ggplot(df) + geom_col(mapping=aes(x = factor(Var1, level = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),, y = Freq, fill = Var1)) + labs(title = "Amount Activity Per Date")

#graphed the totalsteps per weekday and separated them out based on the Id
ggplot(merged_data,mapping=aes(x= factor(Weekday, level=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), y = TotalSteps, fill = Weekday)) + geom_col()+
  facet_wrap(~Id) + theme(axis.text.x = element_blank()) + labs(title = "Total Steps per Day for Each Id")


#tried to see if more steps were walked in specific days or Weekdays vs Weekends
ggplot(merged_data, mapping=aes(factor(Weekday, level=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), y = TotalSteps, fill = Weekday)) + geom_col()+labs(title = "Total Steps per Day")+xlab("Weekdays")
#No apparent trend but Tuesday is the most walked day and Sunday is the least walked day

glimpse(merged_data)
merged_data <- transform(merged_data, TotalMin=(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes))
merged_data <- merged_data %>% relocate(TotalMin, .before = TotalSteps)
View(dailyActivity)
View(merged_data)
#i dont know what im trying to do here
merged_data <- mutate(merged_data, Id = as.character(Id))
glimpse(merged_data)
ggplot(merged_data,mapping=aes(x = factor(Weekday, level=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), y = LoggedActivitiesDistance, color = as.character(Id))) + geom_point()

glimpse(merged_data)


#Graphed the relationship between the Time Awake in Bed and Total Steps
ggplot(merged_data, mapping=aes(x = TimeDiff, y = TotalSteps)) + geom_point()+ labs(title = "Relationship between Time Awake in Bed and Total Steps") + geom_smooth()
#Not really a relationship exist, the only thing you can get is that most people spend less than 60 minutes awake in bed
summary(merged_data$TotalSteps)
summary(merged_data$TimeDiff)


#checked is amount slept had a relationship with the TotalSteps (none found)
ggplot(merged_data, mapping=aes(x=TotalMinutesAsleep, y = TotalSteps)) + geom_point() + geom_smooth()
ggplot(merged_data, mapping=aes(x=TotalSteps, y = TotalDistance)) + geom_point() + labs(title = "TotalSteps vs TotalDistance")
#For most the graphs above regarding TotalSteps, we got to keep in mind of the intensity of minutes

#I want to see if people who logged their distance were more active than people who did not
active_by_log <- data.frame(merged_data$Id, merged_data$LoggedActivitiesDistance, merged_data$Weekday, merged_data$TotalSteps, merged_data$TotalDistance)
active_by_log <- active_by_log %>% rename(Id = merged_data.Id, LoggedActivitiesDistance = merged_data.LoggedActivitiesDistance, Weekday = merged_data.Weekday, TotalSteps = merged_data.TotalSteps, TotalDistance = merged_data.TotalDistance)
active_by_log <- active_by_log %>% mutate(LoggedActivitiesDistance = as.character(LoggedActivitiesDistance))
View(active_by_log)       
ggplot(active_by_log, mapping=aes(x = as.character(LoggedActivitiesDistance), y=TotalSteps, color = as.character(LoggedActivitiesDistance))) +geom_point()+xlab("Logged Activities Distance (small to large")+labs(title = "Do manually Logging Distance help you be more active?")+theme(axis.text.x = element_blank())


merged_data %>%count(LoggedActivitiesDistance)

#on what days do people log their activities on?
#first, might be easier to create another sub-dataset that filter's out all non-logged days
df2 <- active_by_log %>% filter(LoggedActivitiesDistance != 0)
View(df2)
#Table that shows the number of Logged Activity in Each Day (non-zero)
log_by_weekdays <- table(df2$Weekday)
#converted the table into a data frame to use ggplot to show the table visually
log_by_weekdays1 <- data.frame(log_by_weekdays)
glimpse(log_by_weekdays1)
log_by_weekdays_plot <- ggplot(log_by_weekdays1, mapping=aes(x=Var1, y=Freq, fill=Var1))+geom_col()+xlab("Weekday") + labs(title = "Manually Logged Per Day")
log_by_weekdays_plot

View(log_by_weekdays)
#This table includes all of the non-logged activities also
table(merged_data$Weekday)
View(table(merged_data$Weekday))
ggplot(merged_data, mapping=aes(x=factor(Weekday, level=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),y = LoggedActivitiesDistance, color=as.character(Id)))+geom_point()


#saved my "merged_data" dataset as a csv file into my working directory (in this case archive (3) in downloads)
write.csv(merged_data, file = "merged_data_bellabeat.csv")
#now the same for the rest of datasets that I need from here
write.csv(log_by_weekdays1, file = "log_by_weekdays1._bellabeat.csv")