
library(EnvStats)
library(ggplot2)

#Generate Log Normal Data
set.seed(1) 
x <- rlnormAlt(30, mean = 10, cv = 2)

#qqplot 
qqPlot(x, add.line = TRUE)

#QQPlot with ggplot2
df = data.frame(x)
ggplot(df, aes(sample = x)) + stat_qq() + stat_qq_line()

#box-cox transformation
df$y = boxcoxTransform(df$x, lambda = 0.5)
ggplot(df, aes(sample = y)) + stat_qq() + stat_qq_line()
qqPlot(boxcoxTransform(x, lambda = 0.5), add.line = TRUE) 

df$z = boxcoxTransform(df$x, lambda = 0)
ggplot(df, aes(sample = z)) + stat_qq() + stat_qq_line()
qqPlot(boxcoxTransform(x, lambda = 0), add.line = TRUE) 

#another example
data = read.csv('/Volumes/Leila LaCie/Stat Data Course/Final/R code/Box-Cox/box cox.csv')

#calculate optimal lambda
b = boxcox(data$x, objective.name = "Log-Likelihood", optimize = TRUE)
#boxcox(data$x, objective.name = "Log-Likelihood", optimize = TRUE, lambda = c(-5, 5)) 
data$a = boxcoxTransform(data$x, lambda = b$lambda)
ggplot(data, aes(sample = a)) + stat_qq() + stat_qq_line()










