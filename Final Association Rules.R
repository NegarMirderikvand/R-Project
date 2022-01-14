library(plyr)
library(arulesViz)
library(arules)

#Read the dataset
df <- read.csv('/Volumes/Leila LaCie/Stat Data Course/Final/Association Rules/R/Groceries_dataset.csv')
head(df)
str(df)

#Construct transactions data frame 
Tran = df[order(df$Member_number),]
Tran$Member_number <- as.numeric(Tran$Member_number)
Tran <- ddply(Tran, c("Member_number","Date"), function(df1)paste(df1$itemDescription, collapse = ","))
head(Tran,15)
Tran$Member_number <- NULL
Tran$Date <- NULL
colnames(Tran) <- c("itemList")

#Save transactions data frame in your pc
write.csv(Tran, file = '/Volumes/Leila LaCie/Stat Data Course/Final/Association Rules/R/Tran.csv', quote = FALSE, row.names = TRUE)

#Construct transaction Object
Tobj = read.transactions(file='/Volumes/Leila LaCie/Stat Data Course/Final/Association Rules/R/Tran.csv', rm.duplicates= TRUE, format="basket",sep=",",cols=1)

#Apriori algorithm Implementation below
basket_rules <- apriori(Tobj, parameter = list(minlen=2, sup = 0.001, conf = 0.05))
length(basket_rules)
summary(basket_rules)

#Display the top 20 rules with the highest lift 
inspect(head(sort(basket_rules,by="lift"),20))

# Displays itemFrequency
itemFrequencyPlot(Tobj, topN = 10)

# Displays scatterplot
plot(basket_rules, method="scatterplot", jitter = 0)
# To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

#Visualize the top 20 rules with the highest lift and plot them
plot(head(sort(basket_rules,by="lift"),20), method="graph")

#Visualize the top 50 rules with the highest lift and plot them
plot(head(sort(basket_rules,by="lift"),50), method="graph")

#Visualize the top 20 rules with the highest lift and plot them
plot(head(sort(basket_rules,by="lift"),20), method="paracoord")


#Apriori algorithm Implementation below
basket_rules2 <- apriori(Tobj, parameter = list(minlen=3, sup = 0.001, conf = 0.1))
length(basket_rules2)
summary(basket_rules2)

#Display the rules 
inspect(basket_rules2)

# Plot rules
plot(basket_rules2, method="graph")
plot(basket_rules2, method="paracoord")





