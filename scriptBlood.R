blood = read.csv(file = file.choose(), stringsAsFactors = F)
summary(blood)
#PLOTS
pairs(blood[,-1])
boxplot(blood$Months.since.Last.Donation~blood$Made.Donation.in.March.2007)
boxplot(blood$Number.of.Donations~blood$Made.Donation.in.March.2007)
boxplot(blood$Total.Volume.Donated..c.c..~blood$Made.Donation.in.March.2007)

#CORTEST
cor(blood[,-1])

#Hypothesis Test
#H0 = avg meses desde la ultima donacion es igual si hizo la donacion o no
#H1 = avg meses desde la ultima donacion es diferente si hizo la donacion o no

#h0 = avg months since last donations is equal if you didnt donate
#h1 = avg months since last donation is larger if you didnt donate

months0 = (subset(blood, select = Months.since.Last.Donation, Made.Donation.in.March.2007==0))
months1 = subset(blood, select = Months.since.Last.Donation, Made.Donation.in.March.2007==1)
t.test(months0$Months.since.Last.Donation, months1$Months.since.Last.Donation, alternative = "greater",conf.level = 0.95)

#The difference of months since last donation is greater if you didnt donate
#p-value < 0.05

#h0 = avg number of donations is equal if you didnt donate
#h1 = avg number of donations is larger if you didnt donate
number0 = (subset(blood, select = Number.of.Donations, Made.Donation.in.March.2007==0))
number1 = (subset(blood, select = Number.of.Donations, Made.Donation.in.March.2007==1))
t.test(number0$Number.of.Donations, number1$Number.of.Donations, alternative = "less" ,conf.level = 0.95)

#The difference of donations between the people that donated is statistically different from the people who didnt.

#Data Formatting and Cleaning
blood = blood[,-1]
names(blood) = c("MonthsLastDonation", "NoDonations", "TotalVolume", "MonthsFirstDonation", "MadeDonation")

#Linear Regression between Y and X

lm1 = lm(MadeDonation~MonthsLastDonation+NoDonations+TotalVolume+MonthsFirstDonation, data = blood)
summary(lm1)

# TotalVolume with NA since it's perfectly correlated with NoDonations, will create an interaction between them

blood$Volume = blood$TotalVolume/blood$NoDonations

#Remove the variable created since the volume is the number of donations * 250cc
blood =blood[,-6]
lm2 = lm(MadeDonation~MonthsLastDonation+NoDonations+MonthsFirstDonation, data = blood)
summary(lm2)

lm2$fitted.values

#NON linear regression since the response variable is categorical. Using Logistic Regression.

glm1 = glm(MadeDonation~MonthsLastDonation+NoDonations+MonthsFirstDonation, family=binomial(link=logit), data = blood)
summary(glm1)


library(RWeka)
blood[,5] = as.factor(blood[,5])
blood<-blood[,-3]  
smp_size <- floor(0.75 * nrow(blood))  ## set the seed to make your partition reproductible 
set.seed(123) 
train_ind <- sample(seq_len(nrow(blood)), size = smp_size)  
train <- blood[train_ind, ] 
test <- blood[-train_ind, ]
fit <- J48(MadeDonation~MonthsLastDonation+NoDonations+MonthsFirstDonation, data=train)
summary(fit)

library(partykit)
plot(fit)

pr = subset(train, MonthsLastDonation <=7 & NoDonations<=18 & MonthsFirstDonation>49)
summary(pr)
predict(fit, pr)

samNeg = blood[sample(row.names(subset(blood, MadeDonation == 0)),100,replace = T ),]
samPos = blood[sample(row.names(subset(blood, MadeDonation == 1)),100,replace = T ),]

train_b = samNeg
train_b = rbind(train_b, samPos)
summary(train_b)
fit <- J48(MadeDonation~MonthsLastDonation+NoDonations+MonthsFirstDonation, data=train_b)
summary(fit)
