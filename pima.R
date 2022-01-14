library(faraway)
data(pima)
d<-pima
summary(d)
head(d)
d$diastolic[d$diastolic==0]=NA
d$diastolic[d$diastolic==0]=NA
d$glucose[d$glucose==0]=NA
d$triceps[d$triceps==0]=NA
d$bmi[d$bmi==0]=NA
d$insulin[d$insulin==0]=NA

summary(d)


--visualization
hist(d$diastolic,xlab = "diastolic",ylab = "freq",main = "Histogram for Diastolic",border = "#AADDEE",col="#445533
     ")
