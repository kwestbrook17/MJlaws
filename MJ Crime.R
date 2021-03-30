
#Read in Data
read.csv(file= file.choose(), header= TRUE)-> mj_crime
as.factor(mj_crime$Level) -> mj_crime$Level
#look at data
str(mj_crime)

#Violent

#side by side box plot of the data
boxplot(as.integer(mj_crime$Violent) ~ as.factor(mj_crime$Level))

summary(mj_crime$Violent)
#Anova table
mj.aov <- aov(mj_crime$Violent ~ mj_crime$Level)
summary(mj.aov)

#Tukey multiple comparison procedure
TukeyHSD(mj.aov)

#Property
boxplot(as.integer(mj_crime$Property) ~ as.factor(mj_crime$Level))

summary(mj_crime$Property)

mj.aov <- aov(mj_crime$Property ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)

#Murder
boxplot(as.integer(mj_crime$Murder) ~ as.factor(mj_crime$Level))

summary(mj_crime$Murder)

mj.aov <- aov(mj_crime$Murder ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)

#Rape
boxplot(as.integer(mj_crime$Rape) ~ as.factor(mj_crime$Level))

summary(mj_crime$Rape)

mj.aov <- aov(mj_crime$Rape ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)

#Robbery
boxplot(as.integer(mj_crime$Robbery) ~ as.factor(mj_crime$Level))

summary(mj_crime$Robbery)

mj.aov <- aov(mj_crime$Robbery ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)

#Assault
boxplot(as.integer(mj_crime$assault) ~ as.factor(mj_crime$Level))

summary(mj_crime$assault)

mj.aov <- aov(mj_crime$assault ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)

#Burglary
boxplot(as.integer(mj_crime$Burglary) ~ as.factor(mj_crime$Level))

summary(mj_crime$Burglary)

mj.aov <- aov(mj_crime$Burglary ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)

#Larceny Theft
boxplot(as.integer(mj_crime$Larceny.Theft) ~ as.factor(mj_crime$Level))

summary(mj_crime$Larceny.Theft)

mj.aov <- aov(mj_crime$Larceny.Theft ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)

#Car Theft
boxplot(as.integer(mj_crime$CarTheft) ~ as.factor(mj_crime$Level))

summary(mj_crime$CarTheft)

mj.aov <- aov(mj_crime$CarTheft ~ mj_crime$Level)
summary(mj.aov)

TukeyHSD(mj.aov)


