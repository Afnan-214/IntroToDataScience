#Read data
path <- readline("Enter dataset path (.csv) :")
data_set <- read.csv(path)
data_set
#clean
data_set$count <- NULL
data_set

## data visualization
library(dplyr) #select,group_by,summarise
#1-Compare cash and credit totals
paymentmethod <- select(data_set, paymentType)
table(data_set$paymentType)
pie(
  main="Compare cash and credit totals",
  x= table(data_set$paymentType),
)
barplot(
  main="Compare cash and credit totals",
  height=table(data_set$paymentType),
  col="skyblue",
  xlab="paymentType",
  ylab="totals")

#### cash=4957 > credit=4878 ###

#2-Compare each age and sum of total spending
age_per_total <- group_by(data_set,age)
age_per_total <- summarise(age_per_total,totalspending=sum(total))
age_per_total

barplot(
  main="Compare each age and sum of total spending",
  height=age_per_total$totalspending,
  name=age_per_total$age,
  col="skyblue",
  xlab="age",
  ylab="total spending")

plot(
  main="Compare each age and sum of total spending",
  x=age_per_total$age,
  y=age_per_total$totalspending,
  xlab = "age",
  ylab="total spending"
)
## 23 < all  , 37 > all , "22,55" ##

#3-each city total spending 
city_per_total <- group_by(data_set,city)
city_per_total <- summarise(city_per_total,totalspend=sum(total))
city_per_total

city_arrange <- arrange(city_per_total,desc(totalspend)) #arrange it by total descending
city_arrange
#### Alexandria is the biggest total spending , Aswan is the smallest total spending

pie(
  main="each city total spending",
  x=city_arrange$totalspend,
  labels = city_arrange$city)
barplot(
  main="each city total spending",
  height=city_arrange$totalspend,
  name=city_arrange$city,
  col="skyblue",
  xlab="city",
  ylab = "total spending")


########### Dashboard ##########
par(mfcol=c(2,3))
    pie(
      main="Compare cash and credit totals",
      x= table(data_set$paymentType))
barplot(
      main="Compare cash and credit totals",
      height=table(data_set$paymentType),
      col="skyblue",
      xlab="paymentType",
      ylab="totals")
barplot(
      main="Compare each age and sum of total spending",
      height=age_per_total$totalspending,
      name=age_per_total$age,
      col="skyblue",
      xlab="age",
      ylab="total spending")
 plot(
      main="Compare each age and sum of total spending",
      x=age_per_total$age,
      y=age_per_total$totalspending,
      xlab = "age",
      ylab="total spending")
 pie( 
      main="each city total spending",
      x=city_arrange$totalspend,
      labels = city_arrange$city)
 barplot(
      main="each city total spending",
      height=city_arrange$totalspend,
      name=city_arrange$city,
      col="skyblue",
      xlab="total spending",
      horiz=TRUE,
      las=1)

#############################################################
 table(data_set$customer)
 table(data_set$rnd)
 names <- aggregate(total~customer+rnd,data_set,sum)
 names
## k-mean
install.packages("stats")
library(stats)
#grouping the data frame 
cus_data<-aggregate(total~customer+age,data_set,sum) 
age_total <- select(cus_data,age,total)
age_total  
rownames(age_total) <- cus_data$customer
age_total

k <- as.integer(readline(prompt ="Enter Numbers of clusters between (2:4):"))
if(k<2|k>4){
  print("Please Enter Numbers of clusters between (2:4)")
  k <- as.integer(readline(prompt ="Enter Numbers of clusters between (2:4):"))
  k
  kmeans_result <- kmeans(age_total, centers = k)
  kmeans_result
}else{
kmeans_result <- kmeans(age_total, centers = k)
kmeans_result
}

cus_clus<-table(cus_data$customer,kmeans_result$cluster)
cus_clus

#display cluster table
kmean_table <-cbind(cus_clus,age_total)
colnames(kmean_table) <- c("names","cluster","cluster2","age","total") 
kmean_table 
kmean_table<- filter(kmean_table,cluster2==1)
kmean_table$cluster2<-NULL
kmean_table
#############################################################

## association
install.packages("gtools")
library(gtools)
install.packages("arules")
library(arules)
library(Matrix)

##C:/Users/ELYOSR/Desktop/items.txt

trans_path <- readline("Enter the transaction data path (.txt):")
trans_data <-read.transactions(trans_path,sep = ",")
trans_data
inspect(head(trans_data))


sup <- as.numeric(readline("Enter the Minimum Apriori support between (0.001:1):"))
if(sup<0.001|sup>1){
  print("The Minimum support should be between (0.001:1)")
  sup <- as.numeric(readline("Enter the Minimum Apriori support between (0.001:1):"))
  conf <-as.numeric(readline("Enter the Minimum Apriori confidence between (0.001:1):"))
}else{
  conf <-as.numeric(readline("Enter the Minimum Apriori confidence between (0.001:1):"))
}
if(conf<0.001|conf>1){
  print("The Minimum confidence should be between (0.001:1)")
  conf <-as.numeric(readline("Enter the Minimum Apriori confidence between (0.001:1):"))
}


association_rules <-apriori(trans_data,
                    parameter = list(support=sup, 
                    confidence=conf,
                    minlen=2))
inspect(association_rules)

