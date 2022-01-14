#Read Dataset
mc = read.csv('/Users/leila/Documents/Stat Course/Medical Cost/Medical Cost.csv')
head(mc)  # first 6 observations
tail(mc)  #Last 6 Observations
tail(mc, n = 10) #Last 10 Observations

#Structure of each columns of dataset
str(mc) #structure of dataset

#Convert char to Factor(Categorical data)
mc$sex = factor(mc$sex)
levels(mc$sex)
mc$smoker = factor(mc$smoker)
levels(mc$smoker)
mc$region = factor(mc$region)
levels(mc$region)
str(mc)

#Read some parts of dataset
mc[3,'bmi']
mc[3,3]
mc$bmi[3]
mc[,'bmi']
mc[3,]

#Descriptive Statistics
summary(mc) 

library(pastecs) # Library for stat.desc()
stat.desc(mc)# More Statistics information
#The stat.desc function provides the total n, number of null values, 
#number of na values, min, max, range, sum, median, mean, SE of the mean, 95% CI of the mean, var, 
#standard deviation, and coeff of var. 

by(mc, mc$smoker, stat.desc) #stat.decs can be replaced with any function

#skewness & Kurtosis
library(moments)
skewness(mc$charges)
kurtosis(mc$charges)


#----------------------------Plots---------------------------------
library(ggplot2)

#----------------------------Bar Chart---------------------------------

ggplot(data = mc, aes(x = sex, fill = sex)) + geom_bar()

ggplot(data = mc, aes(x = smoker, fill = smoker)) + geom_bar()

ggplot(data = mc, aes(x = children, fill = children)) + geom_bar()

ggplot(data = mc, aes(x = region, fill = region)) + geom_bar()

ggplot(data = mc, aes(x = sex, fill = smoker )) + geom_bar() + ggtitle("Chart of Sex and Smoker")

ggplot(data = mc, aes(x = sex, fill = smoker )) + geom_bar(position=position_dodge()) + ggtitle("Chart of Sex and Smoker")

#----------------------------Pie Chart--------------------------------

#Frequency Tables
sex = mc$sex
smoker = mc$smoker
region = mc$region
df1 <- as.data.frame(table(sex))
df2 <- as.data.frame(table(smoker))
df3 <- as.data.frame(table(region))

library(ggpubr)
labs1 <- paste0(df1$sex, " (", round(df1$Freq/(df1[1,2]+df1[2,2])*100, 2) , "%)")#Calculate percentage of each category
ggpie(df1, "Freq", label = labs1, fill = "sex",lab.pos = "in", color = "white")

labs2 <- paste0(df1$sex, " (", round(df2$Freq/(df2[1,2]+df2[2,2])*100, 2) , "%)")
ggpie(df2, "Freq", label = labs2, fill = "smoker",lab.pos = "in", color = "white")

labs3 <- paste0(df1$sex, " (", round(df3$Freq/(df3[1,2]+df3[2,2]+df3[3,2]+df3[4,2])*100, 2) , "%)")
ggpie(df3, "Freq", label = labs3, fill = "region",lab.pos = "in", color = "white")


#-----------------------------------------Histogram--------------------------------------------------------------

ggplot(mc, aes(x=charges)) + geom_histogram(color="black", fill="white")

#Histogram With Density Line
ggplot(mc, aes(x=charges)) + geom_histogram(aes(y=..density..), color="black", fill="white")+ geom_density(alpha=.2, fill="#FF6666")

#Histogram of Two Variables
ggplot(mc, aes(x=charges, color=smoker))+ geom_histogram(fill="white", position="dodge")+theme(legend.position="top") + stat_bin(bins = 30)   

#-----------------------------------------Box Plot-------------------------------------------------------
ggplot(mc, aes(x='', y= charges)) + geom_boxplot(colour = 'blue')

ggplot(mc, aes(x=smoker, y= charges, color = smoker)) + geom_boxplot()

#-----------------------------------------Scatter Plot------------------------------------------------------

ggplot(mc, aes(x=bmi, y= charges)) + geom_point()

ggplot(mc, aes(x=age, y= charges, colour = smoker)) + geom_point()

#-----------------------------------------Marginal Plot--------------------------------------------------
library(ggExtra)
p = ggplot(mc, aes(age, charges)) + geom_point() + theme_bw()
ggMarginal(p, margins = "both", size = 2, type = "histogram", col = "blue", fill = "orange")

#-----------------------------------------Matrix Plot-------------------------------------------------

pairs(~age+sex+bmi+children+smoker+region+charges, data=mc)

pairs(~charges+age+bmi+children, data=mc)

#-----------------------------------------Another Plot-------------------------------------------------

#Convert bmi to a categorical variable with 2 levels: '0' for persons with bmi <= 30 and '1' for persons with bmi > 30
mc2 = mc
mc2$bmi2[mc$bmi <= 30 ] = 0
mc2$bmi2[mc$bmi > 30  ] = 1
mc2$bmi2 = factor(mc2$bmi2 , ordered =TRUE)
levels(mc2$bmi2)

ggplot(mc2, aes(x=bmi2, y= charges, colour = smoker)) + geom_boxplot()



mc3 = mc2
mc3$smokBmi[mc2$bmi2 == 0 & mc2$smoker == 'no'] = 0
mc3$smokBmi[mc2$bmi2 == 0 & mc2$smoker == 'yes'] = 1
mc3$smokBmi[mc2$bmi2 == 1 & mc2$smoker == 'no'] = 2
mc3$smokBmi[mc2$bmi2 == 1 & mc2$smoker == 'yes'] = 3
mc3$smokBmi = factor(mc3$smokBmi)
levels(mc3$smokBmi)

ggplot(mc3, aes(x=age, y= charges, colour = smokBmi)) + geom_point()














