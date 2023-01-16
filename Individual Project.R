#import dataset
mydata <- read.csv("https://ximarketing.github.io/class/ABOM/TripAdvisor.csv", 
                   fileEncoding = "UTF-8-BOM")
summary(mydata)

#set 'Area' variable, to fix the Area effect
library(gsubfn) #the package 'gsubfn' is available in my version 4.2.2, in some versions it should use function in package 'gsub'
mydata$Area <- gsub('[^A-Z,^a-z]','',mydata$Name)

#set 'Period' variable, to control how long each review has been published
library(lubridate)
mydata$LatestDate <- as.Date('2016-03-27')
mydata$Date <- as.Date(mydata$Date)
mydata$TimePeriod <- mydata$LatestDate-mydata$Date
library(gsubfn)
mydata$Period <- as.numeric(gsub('[^0-9]','',mydata$TimePeriod))

#set 'Season' variable, to control the season in which each review was published
library(lubridate)
mydata$Date <- as.Date(mydata$Date)
mydata$PMonth <- month(mydata$Date)
mydata[which(mydata$PMonth<=5 & mydata$PMonth>=3),'Season'] <- 'Spring'
mydata[which(mydata$PMonth<=8 & mydata$PMonth>=6),'Season'] <- 'Summer'
mydata[which(mydata$PMonth<=11 & mydata$PMonth>=9),'Season'] <- 'Autumn'
mydata[which(mydata$PMonth==12 | mydata$PMonth==1 | mydata$PMonth==2),'Season'] <- 'Winter'

#set 'Weekday' variable, to control the weekday in which each review was published 
Sys.setlocale("LC_TIME", "English") #English output
mydata$Weekday = weekdays(mydata$Date)

#set 'Experienced' variable, which indicates the total number of restaurants the reviewer has been to 
mydata$Experienced <- I(mydata$CountRestaurant+1)

#set 'Expertise' variable, which indicates the average votes the reviewer get
mydata$Expertise <- I(mydata$CountVotes/(mydata$CountReview+1))

#set 'RatingMean' variable, to control the restaurants' average scores
GroupMean <- aggregate(mydata$Rating, list(mydata$Name),mean)
colnames(GroupMean) <- c("Name", "RatingMean")
mydata<- merge(mydata,GroupMean,by='Name')

#log
mydata$LogExperienced <- log(mydata$Experienced+1)
mydata$LogHelpful <- log(mydata$Helpful+1)
mydata$LogPeriod <- log(mydata$Period+1)

#mean center
mydata$TitleLength_mc <- I(mydata$TitleLength-mean(mydata$TitleLength))
mydata$Length_mc <- I(mydata$Length-mean(mydata$Length))
mydata$Rating_mc <- I(mydata$Rating-mean(mydata$Rating))
mydata$Subjectivity_mc <- I(mydata$Subjectivity-mean(mydata$Subjectivity))
mydata$Photo_mc <- I(mydata$Photo-mean(mydata$Photo))
mydata$Sentiment_mc <- I(mydata$Sentiment-mean(mydata$Sentiment))
mydata$LogExperienced_mc <- I(mydata$LogExperienced-mean(mydata$LogExperienced))
mydata$Expertise_mc <- I(mydata$Expertise-mean(mydata$Expertise))

#tobit regression
library(AER)
#control variables
tobit1 <- tobit(LogHelpful ~ RatingMean+LogPeriod+factor(Mobile)+factor(Area)+factor(Season)+factor(Weekday), left = 0, right = Inf, data = mydata)
summary(tobit1)
#control variables + explanatory variables
tobit2 <- tobit(LogHelpful ~ factor(Local)+LogExperienced+Expertise+Rating+TitleLength+Length+I(Length^2)+Sentiment+I(Sentiment^2)+Subjectivity+Photo+RatingMean+LogPeriod+factor(Mobile)+factor(Area)+factor(Season)+factor(Weekday), left = 0, right = Inf, data = mydata)
summary(tobit2)
#control variables + explanatory variables + 'Rating' moderating effect
tobit3 <- tobit(LogHelpful ~ factor(Local)*Rating+LogExperienced+Expertise+Rating+TitleLength+Length+I(Length^2)+Sentiment+I(Sentiment^2)+Subjectivity+Photo+I(Rating_mc*LogExperienced_mc)+I(Rating_mc*Expertise_mc)+I(Rating_mc*TitleLength_mc)+I(Rating_mc*Length_mc)+I(Rating_mc*Length_mc^2)+I(Rating_mc*Sentiment_mc)+I(Rating_mc*Sentiment_mc^2)+I(Rating_mc*Subjectivity_mc)+I(Rating_mc*Photo_mc)+RatingMean+LogPeriod+factor(Mobile)+factor(Area)+factor(Season)+factor(Weekday), left = 0, right = Inf, data = mydata)
summary(tobit3)
