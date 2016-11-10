##get the data
FilePath = 'C:\\RProjects\\LiveSession10_9Assignment\\CHF.csv'
Hdata = read.csv(FilePath)
head(Hdata)

##Set Gender variable
Hdata$Gender[Hdata$Sex == 0] = 'Unknown'
Hdata$Gender[Hdata$Sex == 1] = 'Male'
Hdata$Gender[Hdata$Sex == 2] = 'Female'

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

##
Hdata$admsrcdesc[Hdata$admsrc == 1] = 'Physician referral'
Hdata$admsrcdesc[Hdata$admsrc == 2] = 'Clinic referral'
Hdata$admsrcdesc[Hdata$admsrc == 3] = 'HMO referral'
Hdata$admsrcdesc[Hdata$admsrc == 4] = 'Trans(Hospital)'
Hdata$admsrcdesc[Hdata$admsrc == 5] = 'Trans(Nurse Fac) '
Hdata$admsrcdesc[Hdata$admsrc == 6] = 'Trans(Healthcare fac)'
Hdata$admsrcdesc[Hdata$admsrc == 7] = 'Emergency room'
Hdata$admsrcdesc[Hdata$admsrc == 8] = 'Court/law enforcement'
Hdata$admsrcdesc[Hdata$admsrc == 9] = 'Info not avail'

##Show the Mean
mean(Hdata$AmtReim)
mean(Hdata$TotDeptChg)
mean(Hdata$TotAccomChg)

##show the Standard deviation
sd(Hdata$AmtReim)
sd(Hdata$TotDeptChg)
sd(Hdata$TotAccomChg)

##boxplot

##Tapply mean
tapply(Hdata$AmtReim, Hdata$Gender, mean)
tapply(Hdata$TotAccomChg, Hdata$Gender, mean)
tapply(Hdata$TotDeptChg, Hdata$Gender, mean)

boxplot(Hdata$AmtReim~Hdata$Gender, data=Hdata, main="Amount Paid By Medicare", col=(c("yellow","blue")),xlab="Gender", ylab="Dollars")
boxplot(Hdata$TotAccomChg~Hdata$Gender, data=Hdata, main="Total Accomodation Charges", col=(c("yellow","blue")),xlab="Gender", ylab="Dollars")
boxplot(Hdata$TotDeptChg~Hdata$Gender, data=Hdata, main="Total Department Charges", col=(c("yellow","blue")),xlab="Gender", ylab="Dollars")

##histogram
hist(Hdata$Age)
hist(Hdata$Sex)
hist(Hdata$TotAccomChg)

##Barplots
cnt=table(Hdata$Gender,Hdata$admsrcdesc)
par(mar=c(13.5, 2.1 ,2.1 ,2.1))
barplot(cnt, main='Admission Source By Gender', xlab='Admission Source', 
        col=c('blue', 'red'), legend = rownames(cnt), beside = TRUE, ylim = c(0,110), las=2)

##t test
Hdata291 = subset(Hdata, Hdata$drgcode ==291)
head(Hdata)
mean(Hdata291$LOS)
hist(Hdata291$LOS)
t.test(Hdata291$LOS, mu=6, alternative = 'two.sided')
