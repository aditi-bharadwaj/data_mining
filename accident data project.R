#reading the file
read.csv("D:/Minning project/Data Set/Accidents_categorical.csv")-> Accident_data
View(Accident_data)
colnames(Accident_data)
#checking the datatype of variable
str(Accident_data)
library(dplyr)
#Checking the data balance
prop.table(table(Accident_data$Urban_or_Rural_Area))
#checking for any null value
sapply(Accident_data, function(x) sum(grepl("NULL", x)))
#plotting the graph of accidents happening in rural or urban area
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
##########################################
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-8, 5), ylim = c(50, 57), asp = 1)
points(Accident_data$Longitude, Accident_data$Latitude, col = "red", cex = .6)
# Encoding the target feature as factor
Accident_data$Urban_or_Rural_Area = factor(Accident_data$Urban_or_Rural_Area, labels = c(0, 1), levels = c("Urban", "Rural"))
Accident_data
View(Accident_data$Urban_or_Rural_Area)
View(Accident_data)

#applying the chi sqaure test for checking the corelation of categorical value
chi<- names(which(sapply(Accident_data, class) == "factor"))

#Performing Chi-Square n categorical variables:

chi1=Accident_data[,chi]

for(i in 2:length(chi)-1){
  print(i)
  chi3=table(chi1[,5],chi1[,i])
  
  print(chi[i])
  print(chisq.test(chi3,simulate.p.value = TRUE))
}

#dropping the columns which are not usefull
Accident_data$Latitude<- NULL
Accident_data$Longitude<- NULL
Accident_data$Accident_Index<- NULL
Accident_data$Datetime<- NULL
Accident_data$Junction_Detail<- NULL
Accident_data$Junction_Location<- NULL
Accident_data$Year<- NULL
Accident_data$Month_of_Year<- NULL
Accident_data$Day_of_Week<- NULL
Accident_data$Day_of_Month<- NULL
Accident_data$X1st_Road_Class<- NULL
#dividing data into training and testing set
library(caTools)
set.seed(123)
split = sample.split(Accident_data$Urban_or_Rural_Area, SplitRatio = 0.8)
training_set = subset(Accident_data, split == TRUE)
test_set = subset(Accident_data, split == FALSE)

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Urban_or_Rural_Area ~ Weather+Road_Surface_Conditions+High_Wind+Lights+Road_Type,
                 family = binomial,
                 data = training_set)
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-2])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred
# Making the Confusion Matrix
cm = table(test_set$Urban_or_Rural_Area, y_pred)
cm
library(caret)
confusionMatrix(table(test_set$Urban_or_Rural_Area, y_pred), mode = 'everything')
#making Roc curve
library(pROC)
library(ggplot2)
rocobj <- roc(as.numeric(test_set$Urban_or_Rural_Area), as.numeric(y_pred))
g <- ggroc(rocobj)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve logistic regression") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")
#applying naive bayes
library(e1071)
classifier = naiveBayes(x = training_set[-5],
                        y = training_set$Urban_or_Rural_Area)
# Predicting the Test set results
y_pred_Naive_Bayes = predict(classifier, newdata = test_set[-2])
y_pred_Naive_Bayes
plot(y_pred)
# Making the Confusion Matrix
cm = table(test_set$Urban_or_Rural_Area, y_pred)
cm
plot(cm)
library(caret)
confusionMatrix(table(test_set$Urban_or_Rural_Area, y_pred), mode = 'everything')
#Plotting ROC curve
library(pROC)
library(ggplot2)
rocobj1 <- roc(as.numeric(test_set$Urban_or_Rural_Area), as.numeric(y_pred_Naive_Bayes))
g <- ggroc(rocobj)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Naive Bayes") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")
#comparison of ROC curve
g2 <- ggroc(list(y_pred=rocobj, y_pred_Naive_Bayes=rocobj1))
g2
g2 <- ggroc(list(logistic_Regression=rocobj, Naive_Bayes=rocobj1,),size=1.5)
g2  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Comparison") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")

