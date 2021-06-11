

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
# HeatMap
res<- cor(data)
col1<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col1, symm = TRUE, Colv = NA, Rowv = NA)

## Correlation for redwine

ggcorrplot(cor(data), hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank")


# Boxplot for each variables in df

oldpar = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(data[[i]])
  mtext(names(data)[i], cex = 0.8, side = 1, line = 2)
}
par(oldpar)

##  we are droping Region and channel variables 

df1 <- data[c(3,4,5,6,7,8)]
View(df1)
head(df1,5)


# Predictor values Histogram distribution of df

oldpar = par(mfrow = c(3,2))
for ( i in 1:6 ) {
  hist(df1[[i]], xlab = names(df1)[i],
           col = 'blue', main = paste("Average =", signif(mean(df1[[i]]),5)))
}

# We see in dataset all variables has left skewed except quality (normally distributed)

# Elbow and Silhouette methods are direct methods and gap statistic method is the statistics method.

silhouette_score <- function(k){
  km <- kmeans(df1, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df1))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

fviz_nbclust(df1, kmeans, method='silhouette')

# The optimal number of clusters is 2. as shown in fig

km.final <- kmeans(df1, 2)
## Total Within cluster sum of square
km.final$tot.withinss

## Cluster sizes
km.final$size

data$cluster <- km.final$cluster
head(data, 6)

fviz_cluster(km.final, data=df1)


clusplot(data, data$cluster, color=TRUE, shade = TRUE, label=2)





# Alternative method
# we use only one technique please comment which one is best
# once we finalize then i make some more changes if possible according on that technique only.



set.seed(1) #Ensure reproducable code 
df<- sample_frac(data,0.7) #split into test and train data by 7:3 ratio
df.index<- as.numeric(rownames(df))
df.test<- data[-df.index,]
head(df)
head(df.test)
View(df)

str(df)
str(df.test)
summary(df)
summary(df.test)
summary(is.na(df))
sapply(df,function(x)sum(is.na(x))) # there is no null values as we checked previously

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

quantile(df1$Grocery, probs=seq(from =0.9, to=1,by=0.025))

# From above, 95% percentile is selected due to the increment difference. Next, 95th percentile will replace the remaining outlier.

grocery.max <- as.numeric(quantile(df$Grocery,probs=0.95))
df1$Grocery[df$Grocery > grocery.max] <- grocery.max

# The same concept will be applied for detergents_paper category.

sort(boxplot.stats(df$Detergents_Paper)$out)

quantile(df$Detergents_Paper, probs=seq(from =0.9, to=1,by=0.025))

grocery.max <- as.numeric(quantile(df$Detergents_Paper,probs=0.925))
df$Detergents_Paper[df$Detergents_Paper > grocery.max] <- grocery.max

# The same concept will be applied for milk category.

sort(boxplot.stats(df$Milk)$out)

quantile(df$Milk, probs=seq(from =0.9, to=1,by=0.025))

grocery.max <- as.numeric(quantile(df$Milk,probs=0.925))
df$Milk[df$Milk > grocery.max] <- grocery.max

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

set.seed(123)
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






































































