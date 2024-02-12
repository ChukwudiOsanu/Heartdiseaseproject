rm(list = ls())
library(class)
library(rpart)
library(rpart.plot)
library(rattle)
library(gmodels)
library(caret)
library(ipred)
library(ggplot2)
library(randomForest)
HD.df <- read.csv(file.choose())

head(HD.df)
#Finding percentage of Presence 
nrow(HD.df[which(HD.df$Heart.Disease=='Presence'), ])/nrow(HD.df)
#creating frequency table
system <- table(HD.df$Heart.Disease)
prop.table(system)
#contingency table
table(HD.df$FBS.over.120, HD.df$Heart.Disease)
#turning the completed column into numeric value
HD.df$isHeart.Disease <- ifelse(HD.df$Heart.Disease=='Presence',1, 0)

# Create a bar plot for the relationship between Sex and Heart Disease
data_table <- table(HD.df$Sex, HD.df$Heart.Disease)
barplot(data_table, beside = TRUE, col = c("lightcoral", "lightblue"), 
        main = "Heart Disease by Sex", 
        xlab = "Sex", ylab = "Count")
sex_labels <- c("Female", "Male")
bar_centers <- barplot(data_table, beside = TRUE, col = c("lightblue", "lightcoral"), plot = FALSE)
text(bar_centers, par("usr")[3] - 2, sex_labels, xpd = TRUE, pos = 1, cex = 1.2)

# scatter plot for Age vs. Cholesterol with a linear regression line
plot(HD.df$Age, HD.df$Cholesterol, 
     xlab = "Age", ylab = "Cholesterol",
     main = "Scatter Plot of Age vs. Cholesterol",
     pch = 19, col = "blue", 
     cex = 0.7)
lm_model <- lm(Cholesterol ~ Age, data = HD.df)
abline(lm_model, col = "red")
# Drop ID and zip code columns.
HD.df <- subset(HD.df, select=-c(Heart.Disease))
#training the data
train.index <- sample(c(1:dim(HD.df)[1]), dim(HD.df)[1]*0.6)
#train.index
train.df <- HD.df[train.index, ]
valid.df <- HD.df[-train.index, ]

#Creating logistic regression with the dataset
logistic_model <- glm(isHeart.Disease ~  Sex + Age, data = HD.df, family = "binomial")
options(scipen=999)
summary(logistic_model)

#The model suggests that Chest.pain.type, EKG.results, ST.depression, Number.of.vessels.fluro, and Thallium are significant predictors of heart disease.
#The coefficients give insights into the direction and strength of the relationship between each predictor and the log-odds of having heart disease.

#Classification tree
class.tree <- rpart(isHeart.Disease ~ ., data = HD.df, 
                    control = rpart.control(maxdepth = 6), 
                    method = "class")
summary(class.tree)
#plot tree
prp(class.tree, type = 4, extra = 101, box.palette = "GnYlRd", 
    fallen.leaves = TRUE, branch = .3, split.font = 1, varlen = -10, 
    under=TRUE)  
fancyRpartPlot(class.tree)
#decision trees
rpart.rules(class.tree, extra = 4, cover = TRUE)
#Confusion Matrix 
pred.HD <-predict(class.tree, HD.df,type ="class")
confusionMatrix(as.factor(pred.HD),  as.factor(HD.df$isHeart.Disease))

#The model has a good accuracy of 86.3%, significantly better than the no-information rate.
#Kappa indicates substantial agreement beyond chance.

#Random Forest 
rf <- randomForest(as.factor(isHeart.Disease) ~ ., data = HD.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  
#RF plot
varImpPlot(rf, type = 1)
summary(rf)
#rf predict
rf.pred <- predict(rf, valid.df)
confusionMatrix(as.factor(rf.pred), as.factor(valid.df$isHeart.Disease))
