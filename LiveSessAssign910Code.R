##get the data
FilePath = 'C:\\RCode\\LiveSession10_9Assignment.git\\CHF-master\\CHF.csv'
Hdata = read.csv(FilePath)
head(Hdata)

##Set Gender variable
Hdata[data$Sex == 0, "Gender"] = 'Unknown'
Hdata[data$Sex == 1, "Gender"] = 'Male'
Hdata[data$Sex == 2, "Gender"] = 'Female'

##Create age category
Hdata$agecat[Hdata$Age ==1] = 'Less than 25'
Hdata$agecat[Hdata$Age ==2] = '25-44'
Hdata$agecat[Hdata$Age ==3] = '46-64'
Hdata$agecat[Hdata$Age ==4] = '65-69'
Hdata$agecat[Hdata$Age ==5] = '70-74'
Hdata$agecat[Hdata$Age ==6] = '75-79'
Hdata$agecat[Hdata$Age ==7] = '80-84'
Hdata$agecat[Hdata$Age ==8] = '85-89'
Hdata$agecat[Hdata$Age ==9] = '90 and over'

##Show the Mean
mean(Hdata$AmtReim)
mean(Hdata$TotDeptChg)
mean(Hdata$TotAccomChg)

##show the Standard deviation
sd(Hdata$AmtReim)
sd(Hdata$TotDeptChg)
sd(Hdata$TotAccomChg)


##Tapply mean
tapply(Hdata$AmtReim, Hdata$Gender, mean)
tapply(Hdata$TotAccomChg, Hdata$Gender, mean)
tapply(Hdata$TotDeptChg, Hdata$Gender, mean)

##boxplot
boxplot(Hdata$AmtReim~Hdata$Gender, data=Hdata, main="Amount Paid By Medicare", col=(c("yellow","blue")),xlab="Gender", ylab="Dollars")
boxplot(Hdata$TotAccomChg~Hdata$Gender, data=Hdata, main="Total Accomodation Charges", col=(c("yellow","blue")),xlab="Gender", ylab="Dollars")
boxplot(Hdata$TotDeptChg~Hdata$Gender, data=Hdata, main="Total Department Charges", col=(c("yellow","blue")),xlab="Gender", ylab="Dollars")

##histogram
hist(Hdata$Age)
hist(Hdata$Sex)
hist(Hdata$TotAccomChg)

##Barplots
cnt=table(Hdata)
