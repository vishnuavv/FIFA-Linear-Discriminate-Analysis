###############################################################################################
library("MASS")
library(psych)
library(car)
library(leaps)
library(QuantPsyc)
library(Amelia)    # Has one "missmap" function for finding missing values
library(ggplot2)
library(GGally)    # For the ggpairs function
library(corrplot)
library(data.table)
library(mltools)
library(klaR)
library(dplyr)

data = read.csv("Fulldata.csv", header = T, sep = ",")
head(data)
describe(data)

#Removing Player names, Nationality, National_Position, National_Kit
#Removing Club Name, Club_Position, Club_Kit, Birth_Date, 
ds <- data[ -c(1,2,3,4,5,6,7,14)]
head(ds)
summary(ds)

#Converting Club Joining into Year and into no of years in the club
CYear <- as.Date(ds$Club_Joining, '%m/%d/%Y')
Club_Year <- as.numeric(format(CYear, '%Y'))
No_of_Years_In_Club <- 2017 - Club_Year

#Converting Contract_Expiry into no of years for contract expiry
No_of_Years_for_Contract_Expiry <- ds$Contract_Expiry - 2017

#Attaching No_of_Years_In_Club & No_of_Years_for_Contract_Expiry to dataframe
ds1 <- cbind(ds, No_of_Years_In_Club)
ds2 <- cbind(ds1, No_of_Years_for_Contract_Expiry)
ds3 <- ds2[-c(1,2)]

#Fill in the missing value for variables No_of_Years_In_Club, No_of_Years_for_Contract_Expiry
ds3$No_of_Years_In_Club[is.na(ds3$No_of_Years_In_Club)] <- median(ds3$No_of_Years_In_Club, na.rm = TRUE)
ds3$No_of_Years_for_Contract_Expiry[is.na(ds3$No_of_Years_for_Contract_Expiry)] <- median(ds3$No_of_Years_for_Contract_Expiry, na.rm = TRUE)

#Removing "cm" & "kg" strings from columns height & weight
Height = as.numeric(gsub("cm", "", ds3$Height))
Weight = as.numeric(gsub("kg", "", ds3$Weight))
ds4 <- ds3[-c(2,3)]

#Attaching Height & Weight to data frame
ds5 <- cbind(ds4, Height)
ds6 <- cbind(ds5, Weight)

#Convert Preffered_Foot
#ds6$Preffered_Foot <- as.factor(ds6$Preffered_Foot)
#PrefferedFoot <- model.matrix(~ Preffered_Foot - 1, data = ds6)
#ds6 <- cbind(ds6, PrefferedFoot)
#Convert Preffered_Position
#ds6$Preffered_Position <- as.factor(ds6$Preffered_Position)
#PrefferedPosition <- model.matrix(~ Preffered_Position - 1, data = ds6)
#ds6 <- cbind(ds6, PrefferedPosition)
#names(ds6)
#Convert Work_Rate
#WorkRate <- model.matrix(~ Work_Rate - 1, data = ds6)
#ds6 <- cbind(ds6, WorkRate)

#Convert Ratings
breaks <- c(45, 66, 71, 94)
labels <- c("45-66", "67-71", "72-94")
Ratingsgroup <- cut(ds6$Rating, breaks, labels)

#Adding Ratingsgroup to Dataframe
ds7 <- cbind(ds6,Ratingsgroup)
names(ds7)
summary(ds7)

ds20 <- ds7[grep("CB", ds7$Preffered_Position),]
summary(ds20)

#Removing variables without correlation
ds8 <- ds20[-c(2,4,5,6,7,10,11,12,22,23,26,27,28,37,38,39,40,41,42,43,44,45)]
names(ds8)


#Removing Dribbing & short Pass due to multicolinearity
ds9 <- ds8[-c(4,12)]
names(ds9)

ds9 <- ds9[2:22]
names(ds9)

ds10 <- ds9[sample(nrow(ds9),100),]
head(ds10)

RatingsLDA <- lda(Ratingsgroup ~ ., data = ds10)
RatingsLDA
summary(RatingsLDA)
plot(RatingsLDA, col=as.integer(ds10$Ratingsgroup), main="Linear Discriminate Analysis with Player Ratings")
plot(RatingsLDA, dimen=1, main="Linear Discriminate Analysis Claasification groups")

ds11 <- ds9[sample(nrow(ds9),100),]
head(ds11)

Predicted_Train <- predict(RatingsLDA, ds10)
Predicted_Test <- predict(RatingsLDA, ds11)

mean(Predicted_Train$class == ds10$Ratingsgroup)
mean(Predicted_Test$class == ds11$Ratingsgroup)

row.names(ds10) <- paste(ds10$Ratingsgroup,row.names(ds10))

library(cluster)
D=daisy(ds10[2:21], metric='gower')

H.fit <- hclust(D, method="ward.D2")
plot(H.fit)

groups <- cutree(H.fit, k=3)

rect.hclust(H.fit, k=3, border="red") 

clusplot(ds10, D, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


k.means.fit <- kmeans(ds10, 3) # k = 3
attributes(k.means.fit)
k.means.fit$centers
k.means.fit$cluster
k.means.fit$size

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(ds10, nc=3)

library(cluster)

row.names(ds10) <- paste(ds10$Ratingsgroup,row.names(ds10))

clusplot(ds10, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

