getwd()
setwd("C:/Users/VSBAG/Desktop/DSE_Milan/3rd_sem_subject/ML&SL/Group_Project/Unsupervised")

library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(corrplot)
library(ggcorrplot)
library(stats)

# Clustering
library(cluster) 
library(factoextra)
library(NbClust)


data <- read.csv("https://raw.githubusercontent.com/vicky61992/StatisticsUnsupervisedlearning_project/main/Wholesale%20customers%20data.csv")
head(data)
summary(data)
sapply(data,function(x)sum(is.na(x))) # there is no null values
str(data)
summary(data)

View(data)

# Convert the frame to a table
customer <- data.table(data)

# Construct a data frame of totals using the data table
fresh_prop <- customer[, sum(Fresh), by=.(Channel, Region)]
fresh_prop$Category <- "Fresh"

milk_prop <- customer[, sum(Milk), by=.(Channel, Region)]
milk_prop$Category <- "Milk"

grocery_prop <- customer[, sum(Grocery), by=.(Channel, Region)]
grocery_prop$Category <- "Grocery"

frozen_prop <- customer[, sum(Frozen), by=.(Channel, Region)]
frozen_prop$Category <- "Frozen"

detergents_paper_prop <- customer[, sum(Detergents_Paper), by=.(Channel, Region)]
detergents_paper_prop$Category <- "Detergents_Paper"

delicassen_prop <- customer[, sum(Delicassen), by=.(Channel, Region)]
delicassen_prop$Category <- "Delicassen"


# Combine the data tables
customer_prop <- rbind(fresh_prop, milk_prop, grocery_prop, frozen_prop, detergents_paper_prop,  delicassen_prop)


# Change 'Channel' and 'Region' to factors
customer_prop$Channel <- as.factor(customer_prop$Channel)
customer_prop$Region <- as.factor(customer_prop$Region)



# Split the data table
customer_prop_region <- subset(customer_prop, select = -Channel)
customer_prop_channel <- subset(customer_prop, select = -Region)




# Get the Channel and Region totals 
channel_totals <- customer[, sum(Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen), by=.(Channel)]
region_totals <- customer[, sum(Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen), by=.(Region)]


# Calculate the amount spent per Channel/Region relative to the total
customer_prop_channel$Prop <- 0
customer_prop_channel[Channel == 1]$Prop <- customer_prop_channel[Channel == 1]$V1 / channel_totals[Channel == 1]$V1
customer_prop_channel[Channel == 2]$Prop <- customer_prop_channel[Channel == 2]$V1 / channel_totals[Channel == 2]$V1

customer_prop_region$Prop <- 0
customer_prop_region[Region == 1]$Prop <- customer_prop_region[Region == 1]$V1 / region_totals[Region == 1]$V1
customer_prop_region[Region == 2]$Prop <- customer_prop_region[Region == 2]$V1 / region_totals[Region == 2]$V1
customer_prop_region[Region == 3]$Prop <- customer_prop_region[Region == 3]$V1 / region_totals[Region == 3]$V1

ggplot(data = customer_prop_channel, aes(x = Category, y = Prop, fill = Channel)) + 
  geom_bar(stat = "identity")

ggplot(data = customer_prop_region, aes(x = Category, y = Prop, fill = Region)) + 
  geom_bar(stat = "identity")

# we use only one technique please comment which one is best
# once we finalize then i make some more changes if possible according on that technique only.



set.seed(108) #Ensure reproducable code 

df<- sample_frac(data,0.7) #split into test and train data by 7:3 ratio
df.index<- as.numeric(rownames(df))
df.test<- data[-df.index,]
#head(df)
#head(df.test)
#View(df)


#str(df)
#str(df.test)
#summary(df)
#summary(df.test)
#summary(is.na(df))
#sapply(df,function(x)sum(is.na(x))) # there is no null values as we checked previously

table(df$Channel)
# We can see channel 1 customers Channel - Horeca (Hotel/Restaurant/Cafe) there are 211 obs. and 97obs by channel 2 retail.


table(df$Region)
# customers Region - (1)Lisnon contain 56 obs , (2)Oporto contains 33 obs and (3) Other (Nominal) contains 219 obs


# we can make group of channel and region and view total transaction at each categories

df %>% 
  group_by(Channel,Region) %>%                           
  summarise(total_fresh = sum(df$Fresh), total_Milk = sum(df$Milk),
            total_Grocery= sum(df$Grocery),total_Frozen=sum(df$Frozen),
            total_Detergents_Paper=sum(df$Detergents_Paper), 
            total_Delicassen= sum(df$Delicassen)) 

# show the sum of cost transaction for different product categories. Fresh and Grocery categories are the top sellers


temp <- reshape(df, direction="long", varying=c("Fresh","Milk","Grocery","Frozen","Detergents_Paper", "Delicassen"), 
                v.names= "Total_price", timevar="Category", 
                time=c("Fresh", "Milk","Grocery","Frozen","Detergents_Paper", "Delicassen"))

ggplot(temp, aes(x=temp$Category, y =temp$Total_price)) +geom_boxplot() +stat_boxplot(geom ='errorbar')
+ theme(axis.text.x= element_text(angle=90,hjust=1))+ ggtitle("Product Category Distribution")


cor.result<- cor(df)
pairs(df[,-c(1:2)], col=df$Channel, pch=21, lower.panel = NULL)+title( main = "Category by Channel")

pairs(df[,-c(1:2)], col=df$Region, pch=19, lower.panel = NULL)  +title(main = "Category by Region")

corrplot(cor.result, method="ellipse") +title(main = "Corelation by category")

ggcorrplot(cor.result, hc.order = TRUE, type = "lower", lab = TRUE, insig = "b")



apply(X= df[,-c(1:2)],MARGIN=2,FUN = function(x)length(boxplot.stats(x)$out))
head(df)

sort(boxplot.stats(df$Grocery)$out)

quantile(df$Grocery, probs=seq(from =0.9, to=1,by=0.025))

# From above, 97.5% percentile is selected due to the increment difference. Next, 97.5th percentile will replace the remaining outlier.

grocery.max <- as.numeric(quantile(df$Grocery,probs=0.975))
df$Grocery[df$Grocery > grocery.max] <- grocery.max

# The same concept will be applied for detergents_paper category.

sort(boxplot.stats(df$Detergents_Paper)$out)

quantile(df$Detergents_Paper, probs=seq(from =0.9, to=1,by=0.025))

detergents_Paper.max <- as.numeric(quantile(df$Detergents_Paper,probs=0.975))
df$Detergents_Paper[df$Detergents_Paper > detergents_Paper.max] <- detergents_Paper.max

# The same concept will be applied for milk category.

sort(boxplot.stats(df$Milk)$out)

quantile(df$Milk, probs=seq(from =0.9, to=1,by=0.025))

milk.max <- as.numeric(quantile(df$Milk,probs=0.975))
df$Milk[df$Milk > milk.max] <- milk.max

# For this project, will select the above 3 variables for simplicity.


ggplot(data=df, aes(x=Grocery, y =Detergents_Paper)) + geom_point(shape=1) +geom_smooth(method="lm")

ggplot(data=df, aes(x=Grocery, y =Milk)) + geom_point(shape=1) +geom_smooth(method="lm")


# Create subset to run k means.

# There i also want to add milk kindly suggest

df.subset1<-as.data.frame(df[,c("Grocery","Detergents_Paper")])
summary(df.subset1)


df.subset1<- as.data.frame(scale(df.subset1))
summary(df.subset1)
head(df.subset1)



# Analysis & Modeling

# Tests to find the optimal number of clusters

set.seed(102)
# Elbow method
fviz_nbclust(df.subset1, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(df.subset1, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(df.subset1, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# as per gap statistic method  & Elbow method the optimal cluster are 2

#Kmeans Clustering

set.seed(111)
kmean2.simple <- kmeans(df.subset1,centers=2, iter.max = 25, nstart=100)
df.subset1$cluster <- factor(kmean2.simple$cluster)
summary(df.subset1)




ggplot(data=df.subset1, aes(x=Detergents_Paper, y=Grocery, colour=cluster))+geom_point()+geom_point(data=as.data.frame(kmean2.simple$centers),color ="black", size=4, shape =17)



D<- daisy(df.subset1)
plot(silhouette(kmean2.simple$cluster, D),col=1:2, border = NA)

View(df)
km.final <- kmeans(df, 2)
View(km.final)
str(km.final)
str(kmean2.simple)

## Total Within cluster sum of square
kmean2.simple$tot.withinss

## Cluster sizes
kmean2.simple$size
table(kmean2.simple$cluster,km.final$cluster)


df$cluster <- kmean2.simple$cluster
head(df, 6)


fviz_cluster(kmean2.simple, data=df)

clusplot(df, df$cluster, color=TRUE, shade = TRUE, label=2)



set.seed(111)

library("fpc")
# Compute cluster stats
species <- as.numeric(kmean2.simple$cluster)
clust_stats <- cluster.stats(d = dist(df), 
                             species, kmean2.simple$cluster)
# Corrected Rand index
clust_stats$corrected.rand

clust_stats$vi

# Rand index: 1 (towards 1 better) VI: 0 (lower better)

set.seed(111)
kmean2.simple

## checking betweenss i.e. the inter cluster distance between cluster
kmean2.simple$betweenss






































































