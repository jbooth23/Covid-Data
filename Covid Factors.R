setwd("/Users/User/Desktop/Covid Databases")
getwd()

library("readxl")

covidWA_data <- read_excel("Covid Per County, WA.xlsx") #imports our desired excel file and data within

head(covidWA_data) #prints the first few lines of data

covidWA.df = covidWA_data

summary(covidWA.df)

plot(covidWA.df)

povertyInf.lm = lm(InfectionRate~PovertyRate, data=covidWA.df)

par(mfrow=c(2,2)) #creates 2 rows and 2 columns

plot(povertyInf.lm)
summary(povertyInf.lm)

#very low correlation between poverty rate and infection rate. R^2 = 0.1696

ageInf.lm = lm(InfectionRate~AverageAge, data=covidWA.df)

plot(ageInf.lm)
summary(ageInf.lm)

#there is a modest, negative correlation between average age and infection rate. R^2 = 0.4824

voteInf.lm = lm(InfectionRate~BidenLead, data=covidWA.df)

plot(voteInf.lm)
summary(voteInf.lm)

#low correlation between support for Trump/Biden and covid infection rate. R^2 = 0.2291

vaccineVote.lm = lm(VaxRate~BidenLead, data=covidWA.df)

plot(vaccineVote.lm)
summary(vaccineVote.lm)

#moderate to strong correlation between vaccination rate and 2020 vote. R^2 = 0.6451

fullvaxVote.lm = lm(FullVaxRate~BidenLead, data=covidWA.df)

plot(fullvaxVote.lm)
summary(fullvaxVote.lm)

#moderate to strong correlation between FULL vaccination rate and 2020 vote. R^2 = 0.681

povertyVote.lm = lm(PovertyRate~BidenLead, data=covidWA.df)

plot(povertyVote.lm)
summary(povertyVote.lm)

#extremely low correlation between poverty rate and 2020 vote. R^2 = 0.1184

povertyDeg.lm = lm(PovertyRate~PercBachDegree, data=covidWA.df)

plot(povertyDeg.lm)
summary(povertyDeg.lm)

#no observed correlation between a bachelor's degree and lower poverty however 1 outlier (obs. 38) was observed. Analysis
#or exclusion of this outlier will likely change results significantly

vaxVote.lm = lm(VaxRate~PercBachDegree, data=covidWA.df)

plot(vaxVote.lm)
summary(vaxVote.lm)

#low correlation between 2020 vote and vaccination rate. R^2 = 0.2843

par(mfrow=c(2,2)) #sets window to 1 row, 1 column

hist(covidWA.df$VaxRate, xlab = "First Shot Vaccination Rate", main="First Shot Vaccination Rate Frequencies")
hist(covidWA.df$FullVaxRate, xlab = "Fully Vaccinated Rate", main = "Fully Vaccinated Rate Frequencies")
hist(covidWA.df$InfectionRate, xlab = "Covid Infection Rate", main="Covid Infection Rate Frequencies")
hist(covidWA.df$AverageAge, xlab = "Average Age", main = "Average Age Frequencies")
