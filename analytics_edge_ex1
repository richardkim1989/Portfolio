poll <- read.csv("C:/Users/Sung Min Kim/Desktop/Computer Things/R/analytics_edge/AnonymityPoll.csv", header = TRUE)

#Problem 1 - Summarizing the Dataset
str(poll) #1,002 people participated in the poll.
summary(poll)

table(poll$Smartphone, useNA = "ifany") #487 smartphones users, 472 non-smartphone users, 43 NA.

table(poll$State, poll$Region) #kansas, missouri, ohio are in the midwest census region.

table(poll$State, poll$Region) #texas was the south census region with the largest number of interviewees.


#Problem 2 - Internet and Smartphone Users
table(poll$Internet.Use, poll$Smartphone, useNA = "ifany") #186 no smartphone or internet, 470 smartphone and internet, 285 internet but no smartphone, 17 no internet but smartphone.

limited <- subset(poll, Internet.Use == 1 | Smartphone == 1)
str(limited) #792 interviewees in the new data frame.


#Problem 3 - Summarizing Opinions about Internet Privacy
summary(limited) #missing values for smartphone, age, conservativeness, worry.about.info, privacy.importance, anonymity.possible, tried.masking.identity, privacy.laws.effective.

mean(limited$Info.On.Internet) #3.795 average number of personal info on the internet

table(limited$Info.On.Internet) #105 interviewees reported 0 for info.on.internet, while 8 interviewees reported 11.

table(limited$Worry.About.Info)
table(limited$Worry.About.Info)[2]/sum(table(limited$Worry.About.Info)) #0.4886 proportion of the interviewees worry about their info available on the internet.

table(limited$Anonymity.Possible)
table(limited$Anonymity.Possible)[2]/sum(table(limited$Anonymity.Possible)) #0.3692 proportion of the interviewees think it is possible to be completely anonymous on the internet.

table(limited$Tried.Masking.Identity)
table(limited$Tried.Masking.Identity)[2]/sum(table(limited$Tried.Masking.Identity)) #0.1632 proportion of the interviewees have tried masking their identity on the internet.

table(limited$Privacy.Laws.Effective)
table(limited$Privacy.Laws.Effective)[2]/sum(table(limited$Privacy.Laws.Effective)) #0.2558 proportion of the interviewees found United States privacy laws effective.


#Problem 4 - Relating Demographics to Polling Results
hist(limited$Age) #people about 60 years old are best represented age group in the population.

max(table(limited$Age, limited$Info.On.Internet)) #6 interviewees have exactly the same value in their age variable AND the same value in their Info.On.Internet variable

plot(jitter(limited$Age), jitter(limited$Info.On.Internet)) #Older age seems moderately associated with a smaller value for info.on.internet.

tapply(limited$Info.On.Internet, limited$Smartphone, summary) #4.3676 was the average info.on.internet value for smartphone users, and 2.9228 for non-smartphone users.

tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary) #0.1925, 0.1174

