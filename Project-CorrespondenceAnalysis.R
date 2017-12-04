###############################################################################################
library("MASS")
library(psych)
library(car)
library(leaps)
library(QuantPsyc)
library(Amelia)    # Has one "missmap" function for finding missing values
library(ggplot)
library(GGally)    # For the ggpairs function
library(corrplot)
library(data.table)
library(mltools)
library(ca)

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

ds20 <- ds6[grep("CB", ds7$Preffered_Position),]
summary(ds20)

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
#names(ds6)

#Convert Freekick_Accuracy
breaks <- c(4, 31, 42, 57, 93)
labels <- c("4-31", "32-42", "43-57", "58-93")
FAgroup <- cut(ds20$Freekick_Accuracy, breaks, labels)

#Convert Rating
breaks1 <- c(45, 62, 66, 71, 94)
labels1 <- c("45-62", "63-66", "67-71", "72-94")
Ratinggroup <- cut(ds20$Rating, breaks1, labels1)


#Adding FAgroup & Ratinggroup to Dataframe
ds7 <- cbind(ds20,FAgroup)
ds8 <- cbind(ds20,Ratinggroup)

Correspondance <- data.frame(Ratinggroup, FAgroup)
head(Correspondance)
CA1 <- table(Correspondance)

fit = ca(CA1)
summary(fit)
plot(fit, main="Correspondence Analysis between Rating & Freekick_Accuracy")
legend("topleft",inset=.02, legend=c("FK Accuracy", "Rating"),
       col=c("red", "blue"), pch=17:16, cex=1)
plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T), main="Correspondence Analysis between Rating & Freekick_Accuracy")
legend("topleft",inset=.02, legend=c("FK Accuracy", "Rating"),
       col=c("red", "blue"), pch=17:19, cex=1)


