#install.packages("arules")
#install.packages('knitr')
#install.packages("gridExtra")
#install.packages("devtools")
#install.packages("plyr")
library(tidyverse)
library(arules)
library(arulesViz)
library(knitr)
library(gridExtra)
library(lubridate)
library(plyr)
groceries <- read.csv("C:/Users/muham/Desktop/Groceries_dataset.csv")
head(groceries)
sum(is.na(groceries))

sorted <- groceries[order(groceries$Member_number),]
#member umberi numeric yapma
sorted$Member_number <- as.numeric(sorted$Member_number)
str(sorted)
#ayni gun ve ayni saatte alan kisilerin itemlerini gruplama
itemList <- ddply(sorted, c("Member_number","Date"), function(df1)paste(df1$itemDescription,collapse = ","))

head(itemList,15)

#member no ve dateleri silme islemi
itemList$Member_number <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("itemList")
head(itemList,15)
#csv olarak cikarma listeyi
write.csv(itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)
head(itemList)

#basket formatina cevirme
trans = read.transactions(file="C:/Users/muham/Desktop/ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
print(trans)
#tirnaklari silme
trans@itemInfo$labels <- gsub("\"","",trans@itemInfo$labels)
trans


basket_rules <- apriori(trans, parameter = list(minlen=2, sup = 0.001, conf = 0.05, target="rules"))


print(length(basket_rules))


summary(basket_rules)

inspect(basket_rules[1:20])


plot(basket_rules, jitter = 0)


plot(basket_rules, method = "grouped", control = list(k = 5))

plot(basket_rules[1:20], method="graph")


plot(basket_rules[1:20], method="paracoord")

itemFrequencyPlot(trans, topN = 10)

basket_rules2 <- apriori(trans, parameter = list(minlen=3, sup = 0.001, conf = 0.1, target="rules"))

print(length(basket_rules2))

summary(basket_rules2)

inspect(basket_rules2)

plot(basket_rules2, method="graph")

plot(basket_rules2, method="paracoord")














