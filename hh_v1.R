
library(sqldf)
library(dplyr)
library(data.table)
library(tcltk)
library(RH2)

date_adjust <- function(x){
  
  if (grepl('/',x)){
    
    return(as.Date(x,"%m/%d/%Y"))
  }
  if(grepl('-',x)){
    return(as.Date(x,"%m-%d-%Y"))
  }
  else{
    return(NA)
  }
  
}


contacts <- read.csv('.../Contact.csv')


cases <- read.csv('.../Case.csv')



contacts$Date_turned_green__c_new <- sapply(contacts$Date_turned_green__c,date_adjust)

contacts$Date_Turned_Blue__c_new <- sapply(contacts$Date_Turned_Blue__c,date_adjust)

contacts$Confirmed_Hired_Date__c_new <- sapply(contacts$Confirmed_Hired_Date__c,date_adjust)

contacts$Date_Turned_Blue__c_new <- as.Date(contacts$Date_Turned_Blue__c_new,origin = "1970-01-01")

contacts$Date_turned_green__c_new <- as.Date(contacts$Date_turned_green__c_new ,origin = "1970-01-01")

contacts$Confirmed_Hired_Date__c_new  <- as.Date(contacts$Confirmed_Hired_Date__c_new ,origin = "1970-01-01")

cases$CreatedDate_new <- sapply(cases$CreatedDate,date_adjust)

cases$CreatedDate_new <- as.Date(cases$CreatedDate_new ,origin = "1970-01-01")

cases$ClosedDate_new <- sapply(cases$ClosedDate,date_adjust)

cases$ClosedDate_new <- as.Date(cases$ClosedDate_new ,origin = "1970-01-01")

cases$case_time <- cases$ClosedDate_new - cases$CreatedDate_new

cases$case_time < as.integer(cases$case_time)


contacts$Time_to_hire <-  contacts$Confirmed_Hired_Date__c_new - contacts$Date_turned_green__c_new



contacts$Time_to_hire <- as.integer(contacts$Time_to_hire)


#Filter contacts which are hired

# hired_blue <- contacts[contacts$Active_Color__c =='Blue',]
# 
# hired_flag <-contacts[contacts$Hire_Heroes_USA_Confirmed_Hire__c ==1,]

hired<- contacts[contacts$Active_Color__c =='Blue'| contacts$Hire_Heroes_USA_Confirmed_Hire__c ==1,]
not_hired <- contacts[contacts$Active_Color__c !='Blue' & contacts$Hire_Heroes_USA_Confirmed_Hire__c!= 1,]

#hired_flag[,c("Date_Turned_Blue__c_new","Confirmed_Hired_Date__c","Previous_Confirmed_Hire_Date__c")]

#hired_blue[,c("Date_Turned_Blue__c","Confirmed_Hired_Date__c","Previous_Confirmed_Hire_Date__c")]
# 
# summary(hired_blue$Time_to_hire)
# 
# summary(hired_flag$Time_to_hire)
# 
# summary(hired_blue_and_flag$Time_to_hire)
# 
# summary(contacts$Date_turned_green__c_new)

cases$caseid <- cases$Id

cases <- subset(cases,select = -c(Id))

combined <- merge(hired,cases,by.x = "Id",by.y = "ContactId",all.x = TRUE)

# Code to get mean of the salary ranges:


# Remove comma and dollar sign
temp <- gsub("[,$]","", combined$Salary_Range__c)

# remove text
temp <- gsub("[[:alpha:]]","", temp)

# get average over range
combined$Salary_Range__c_new <- sapply(strsplit(temp , "-") , function(i) mean(as.numeric(i)))


not_hired_combined <- merge(not_hired,cases,by.x = "Id",by.y = "ContactId",all.x = TRUE)

Test<- not_hired_combined[,c("caseid","Id")]

Test_Not_hired_with_Completed_Case <- not_hired_combined[not_hired_combined$Status =='Completed',c("caseid","Id")]

Test_HIRED<- combined[,c("caseid","Id","Salary_Range__c_new")]

Test_HIRED_with_Completed_Case<- combined[combined$Status=='Completed',c("caseid","Id","Salary_Range__c_new")]

sqldf("SELECT  count(*) FROM Test GROUP BY CASE WHEN  caseid is null THEN 'NONE' ELSE 'VOLUNTEER' END ")

# %GE OF PEOPLE WHO WERE NOT HIRED AND HAD VOLUNTEER ASSOCIATED IS: 1063 /(1063+56240) = 0.01855051

sqldf("SELECT  count(*) FROM Test_HIRED GROUP BY CASE WHEN  caseid is null THEN 'NONE' ELSE 'VOLUNTEER' END ")

# %GE OF PEOPLE WHO WERE  HIRED AND HAD VOLUNTEER ASSOCIATED IS: 763/(763+6734) = 0.101774

sqldf("SELECT  count(*) FROM Test_Not_hired_with_Completed_Case GROUP BY CASE WHEN  caseid is null THEN 'NONE' ELSE 'VOLUNTEER' END ")

# %GE OF PEOPLE WHO WERE NOT HIRED AND HAD VOLUNTEER ASSOCIATED WITH COMPLETED ACTIVITY IS: 475 * 100 /(475+ 56240) = 0.8375209

sqldf("SELECT  count(*) FROM Test_HIRED_with_Completed_Case GROUP BY CASE WHEN  caseid is null THEN 'NONE' ELSE 'VOLUNTEER' END ")

# %GE OF PEOPLE WHO WERE  HIRED AND HAD VOLUNTEER ASSOCIATED WITH COMPLETED ACTIVITY  IS:  483* 100 /(483+6734) = 6.692532



Test_HIRED_with_Completed_Case<- combined[combined$Status=='Completed',c("caseid","Id","Salary_Range__c_new","Reason")]
Test_HIRED_with_Completed_Case<- combined[!is.na(combined$Salary_Range__c_new),c("caseid","Id","Salary_Range__c_new","Reason")]

Test_HIRED_with_Completed_Case$Salary_Range__c_new



# FOR HIRED CAONTACTS, AVERAGE SALARY  WITH  AND WITHOUT VOLUNTEER HELP(COMPLETED ACTIVITIES)
sqldf("SELECT  AVG(Salary_Range__c_new),CASE WHEN  caseid is null THEN 'NONE' ELSE 'VOLUNTEER' END FROM 
      
            Test_HIRED_with_Completed_Case GROUP BY CASE WHEN  caseid is null THEN 'NONE' ELSE 'VOLUNTEER' END ")
# Mean Salary with volunteer activity = $49878.61  without volunteer activity  = $46409.89



# VOLUNTEER ACTIVITY TYPE AGAINST AVERAGE SALARY:
sqldf("SELECT  AVG(Salary_Range__c_new),Reason FROM 
      
      Test_HIRED_with_Completed_Case GROUP by Reason ")

# AVG("Salary_Range__c_new")                Reason
# 1                   53778.58 Federal Resume Review
# 2                   49358.49        Mock Interview
# 3                   48124.52     Career Counseling
# 4                   46409.89                  <NA>

combined_no_case <- combined[is.na(combined$caseid),]

combined_with_case <- combined[!is.na(combined$caseid),]

summary(combined_no_case$Time_to_hire)

summary(combined_with_case$Time_to_hire)

combined_with_completed_case <- combined_with_case[combined_with_case$Status=='Completed',]

summary(combined_with_completed_case$Time_to_hire)

combined_with_completed_case_categories <- combined_with_completed_case[,c("Reason","Time_to_hire","case_time")]

sqldf("SELECT AVG(Time_to_hire),COUNT(*),Reason FROM combined_with_completed_case_categories GROUP BY Reason ")



combined_with_completed_case_categories$Time_to_hire <- as.integer(combined_with_completed_case_categories$Time_to_hire)

combined_with_completed_case_categories$Time_to_hire <- combined_with_completed_case_categories$Time_to_hire[!is.na(combined_with_completed_case_categories$Time_to_hire)]

combined_with_completed_case_categories$case_time <- as.integer(combined_with_completed_case_categories$case_time)

cormat<-combined_with_completed_case_categories[!is.na(combined_with_completed_case_categories$case_time) & !is.na(combined_with_completed_case_categories$Time_to_hire),c("case_time","Time_to_hire")]

cor(cormat$case_time,cormat$Time_to_hire)

plot(cormat$case_time,cormat$Time_to_hire)

write.csv(cormat,"corr.csv")

#write.table(contacts,file = "C:/Users/varun/Desktop/HH/Contact_PROCESSED3.csv",row.names = FALSE, quote = TRUE)


sqldf("SELECT '2017-01-01' AS DATE - '2017-07-01' AS DATE ")


