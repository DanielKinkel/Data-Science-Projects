## Data Science for Consumer Behavior Case Study

setwd("C:/Users/Anwender/OneDrive/Studium/10. Semester/Data Science for Consumer Behavior/Case study")

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

bakery <- read_csv("bakery.csv")
bakery_clean <- bakery[!(bakery$Item %in% c("NONE", "Adjustment")),]
#write.csv(bakery_clean, file="bakery_clean.csv", row.names = F, quote = F)

####### 1 Investigate the data

### 1.1 Which products are bought most frequently?

n_products <- bakery_clean %>% group_by(Item) %>% summarise(frequency = n()) %>% arrange(desc(frequency))

#-> The most bought products are Coffee(n=5471), Bread(n=3325), Tea(n=1435) and Cake(n=1025), which is reasonable
#   considering that the company that hired us is a bakery.


### 1.2 How many products do consumers buy per purchase?

products_per_purchase <- bakery_clean %>% group_by(Transaction) %>% summarise(products_per_purchase = n())

# Histogram for the number of products bought per purchase in percent 
ggplot(products_per_purchase, aes(x=products_per_purchase)) + 
   geom_bar(aes(y = (..count..)/sum(..count..)), fill="deepskyblue2", color="black") +
   scale_x_discrete(name = "products per purchase", limits = c(1:11)) +
   scale_y_continuous(labels=scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.4)) +
   ylab("percentage of transactions") +
   ggtitle("How many products do consumers buy per purchase?")

#-> The distribution of products per purchase is positively skewed. Most customers buy one or two items. 
#   Buying three or four items is still common, while higher item counts are rare.
 

### 1.3 Exploratory Data Analysis

summary(bakery_clean)
#-> 20506 Items were bought in 9684 transactions from 30.10.2016 until 09.04.2017.

length(unique(bakery_clean$Item))
#-> There have been 93 different items purchased in this time period.

## Purchase behavior for different time dimensions

# Formating the date column with the help of the lubridate package
bakery_clean2<-bakery_clean %>%
   mutate(Date=as.Date(Date),
          Time=hms(Time))   # stores time stamps in unified format

# Adding columns for hour, day, weekday, week and month
bakery_clean2$Hour <- hour(bakery_clean2$Time)
bakery_clean2$Day <- day(bakery_clean2$Date)
bakery_clean2$Weekday <- wday(bakery_clean2$Date,label=T, week_start = 1) 
bakery_clean2$week <- week(bakery_clean2$Date)
bakery_clean2$Month <- month(bakery_clean2$Date,label=T)


# Sold items per hour
hour <- bakery_clean2 %>%
   group_by(Hour) %>% 
   summarise(Count=n())

ggplot(hour,aes(x=Hour,y=Count))+
   geom_bar(stat="identity", fill="deepskyblue2", color="black")+
   ggtitle("Sold Items per Hour") + 
   ylab("Number of Sales")
#-> The vast majority of items are sold within the time from 8am - 5pm
#   Especially in the period from 10am to 3pm a lot of products are sold

# Sold Items per day
day <- bakery_clean2 %>%
   group_by(Day) %>% 
   summarise(Count=n())

ggplot(day,aes(x=Day,y=Count))+
   geom_bar(stat="identity", fill="deepskyblue2", color="black")+
   ggtitle("Sold Items per Day") + 
   ylab("Number of Sales")
#-> Some variance in the numer of sold items per day, but no clear pattern

# Sold items per weekday
weekday <- bakery_clean2 %>% 
   group_by(Weekday) %>% 
   summarise(Count= n()) 

ggplot(weekday,aes(x=Weekday,y=Count))+
   geom_bar(stat="identity", fill="deepskyblue2", color="black")+
   ggtitle(" Sold items per weekday")+ 
   ylab("Number of Sales")
#-> It is noticeable that most items are sold on Saturdays. Slight increases compared to 
#   workdays can be observed for Friday and Sunday

#  Sold Items per month
month <- bakery_clean2 %>%
   group_by(Month) %>% 
   summarise(Count=n())

ggplot(month,aes(x=Month,y=Count))+
   geom_bar(stat="identity", fill="deepskyblue2", color="black")+
   ggtitle("Sold Items per Month")+ 
   ylab("Number of Sales")
#-> Data for April and October not fully included, which causes the discrepancy
#   Otherwise: Slightly smaller number of sales in Dezember and January


# Hourly differences for the 10 best-selling products
most_items <- sort(table(bakery_clean$Item),decreasing = T)
most_items <- names(most_items[1:10])

products_per_hour <- bakery_clean2 %>% 
   group_by(Item, Hour) %>%
   summarize(number = n()) %>% 
   filter(Item %in% most_items) %>%
   filter(Hour %in% c(8:16))

products_per_hour %>% 
   ggplot(aes(Item, number)) +
   geom_bar(aes(fill = as.factor(Hour)),
            position = "dodge",
            stat = "identity") + 
   scale_fill_brewer(palette = "Set1") +
   labs(fill = "Hours") +
   ylab("Number of Sales") +
   ggtitle("Hourly differences for Top10 products") +
   scale_x_discrete(guide = guide_axis(n.dodge=3))

#-> Pastry and medialuna show the stronges sales in the morning hours.
#   Bread and Coffee spike around 11am. Sandwiches are most popular at lunch time (12-14pm).
#   Tea and Cake are bought most often during 14pm-15pm.

# Weekday differences for the 10 best-selling products
products_per_weekday <- bakery_clean2 %>% 
   group_by(Item, Weekday) %>%
   summarize(number = n()) %>% 
   filter(Item %in% most_items)

products_per_weekday %>% 
   ggplot(aes(Item, number)) +
   geom_bar(aes(fill = Weekday),
            position = "dodge",
            stat = "identity") + 
   scale_fill_brewer(palette = "Set1") +
   ylab("Number of Sales") +
   ggtitle("Weekday differences for Top10 products") +
   scale_x_discrete(guide = guide_axis(n.dodge=3))
#-> It becomes apparent again that most items are bought more on Saturday. However, this
# is not the case for cookies. Additionally, the sales of brownies and hot chocoloade
# are lower at the start of the week

# Monthly differences for the 10 best-selling products
products_per_month <- bakery_clean2 %>% 
   group_by(Item, Month) %>%
   summarize(number = n()) %>% 
   arrange(Item,desc(number)) %>% 
   filter(Item %in% most_items) %>%
   filter(!(Month %in% c("Okt", "Apr")))

products_per_month %>% 
   ggplot(aes(Item, number)) +
   geom_bar(aes(fill = Month),
            position = "dodge",
            stat = "identity") + 
   scale_fill_brewer(palette = "Set1") +
   ggtitle("Monthly differences for Top10 products") +
   scale_x_discrete(guide = guide_axis(n.dodge=3))
#-> Cake, Sandwiches and Cookies are more popular early in the year. Medialuna is bought
#   more at the end of the year. Brownies show strong numbers for
#   November but low sales in December.


### 2
library(arules)
bakery_transactions <- read.transactions("bakery_clean.csv", format = "single", cols=c(3,4),sep=",", quote="\"", header=TRUE)

datamat <- coerce(bakery_transactions@data, signature(from = "ngCMatrix", to = "matrix"))
datamat <- matrix(as.logical(datamat),ncol=nrow(bakery_transactions@itemInfo),byrow=T)
colnames(datamat) <- bakery_transactions@itemInfo[,1]
datamat <- as.data.frame(datamat)

# library(fastDummies)
# bakery2 <- dummy_cols(bakery, select_columns = c("Item"), remove_selected_columns = T)
# bakery2 <- bakery2 %>% select(3, 6:98) %>% group_by(Transaction) %>% summarize(across(everything(), list(sum)))

# k means
  ssplot <- function(data, maxCluster = 30) {
   SSw <- vector()
   for (i in 2:maxCluster) {
     set.seed(42)
     SSw[i] <- sum(kmeans(data, centers = i)$withinss)
   }
   plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares") 
 }
 ssplot(datamat) 
 
 cl.kmeans.5 <- kmeans(datamat, 5)
 cl.kmeans.5$size
 
 cl.kmeans.8 <- kmeans(datamat, 8)
 cl.kmeans.8$size
 

# hirachical clustering
 cl.h <- hclust(dist(datamat))
 plot(cl.h) 
 clusterCut.5 <- cutree(cl.h, 5)
 clusterCut.8 <- cutree(cl.h, 8)
 table(clusterCut.5)
 table(clusterCut.8)
 
 # evaluation
 cl.eval <- function(x){
   clMeans <- matrix( nrow = max(x$cluster ), ncol = ncol(x))
   colnames(clMeans) <- colnames(x)
   clMax <- vector()
   clWhichMax <- vector()
   
   for (i in 1:max(x$cluster)) {
     clx <- x[x$cluster == i,]
     del <- which(colnames(clx) == "cluster")
     clx <- clx[,-del]
     
     clMeans[i,] <- c(as.vector(colMeans(clx)),i)
     clMax[i] <- max(colMeans(clx))
     clWhichMax[i] <- names(which.max(colMeans(clx)))
   }
   return(list(t(clMeans),clMax,clWhichMax))
 }
 
 
 datamat$cluster <- cl.kmeans.5$cluster
 kmeans.5.eval <- cl.eval(datamat)
 
 datamat$cluster <- cl.kmeans.8$cluster
 kmeans.8.eval <- cl.eval(datamat)

 datamat$cluster <- clusterCut.3  
 hclust.3.eval <- cl.eval(datamat)

 datamat$cluster <- clusterCut.8
 hclust.8.eval <- cl.eval(datamat)

## 3
 rules <-apriori(bakery_transactions, parameter = list(support = 0.001, conf = 0.01),
                appearance = list(default="lhs",rhs="Cookies"),
                control = list(verbose=F))

rules_lift <-sort(rules, decreasing = TRUE, by="lift")
inspect(rules_lift[1:5])


################################################################################
# Association Rules
# Most frequently bought products
itemFrequencyPlot(bakery_transactions, topN = 20, type = "absolute")

rules <- apriori(bakery_transactions, parameter = list(supp = 0.001, conf = 0.8))
inspect(rules)

summary(bakery_transactions)

rules2 <- apriori(data = bakery_transactions, parameter = list(supp = 0.001, conf = 0.001),
                  appearance = list(default = "lhs", rhs = "Cookies"),
                  control = list(verbose=F))

rules2 <- sort(rules2, decreasing = T, by = "confidence")
inspect(rules2[1:5])

plot(rules2[1:5])

plot(rules2[1:5], method = "graph", interactive = F, shading = NA)
# A promotion based on Juice and Cookies would be a good idea.







