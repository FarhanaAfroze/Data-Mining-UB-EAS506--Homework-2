setwd("/users/shifa/Downloads")
install.packages("DataExplorer")
install.packages("ggcorrplot")
install.packages("Metrics")
install.packages("leaps")
install.packages("olsrr")
install.packages("class")

library(DataExplorer)
library(ggcorrplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(moments)
library(caTools)
library(MASS)
library(Hmisc)
library(Metrics)
library(leaps)
library(olsrr)
library(sp)
library(class)



#HW-1
# exploring the data sets
data <- read.csv("cereal.csv", na.strings = " ")
View(data)
dim(data)
summary(data)
sum(is.na(data))
#with(data, table(mfr))
plot_histogram(data)
plot_boxplot(data, by = "rating")
#boxplot(data$rating)

# Data cleaning and pre processing, tranforming and more visulaization the data
#first rename potass column
data <- rename(data, potassium = potass)
#removing categorical value from data set
data <- data[c(-1,-2,-3)]
head(data)
dim(data)
#calculate corr-ealtion and plot to see
corr <- cor(data)
#corr
ggcorrplot(corr, method = 'circle', lab = TRUE)

#next we will normalize our data 
normalize <- function(x) 
  {
  return ((x - min(x)) / (max(x) - min(x)))
  }
data$calories<-normalize(data$calories)
data$protein<-normalize(data$protein)
data$fat<-normalize(data$fat)
data$sodium<-normalize(data$sodium)
data$carbo<-normalize(data$carbo)
data$sugars<-normalize(data$sugars)
data$vitamins<-normalize(data$vitamins)
data$shelf<-normalize(data$shelf)
data$potassium<-normalize(data$potassium)
data$weight<-normalize(data$weight)
data$cups<-normalize(data$cups)
data$rating<-normalize(data$rating)
View(data)
head(data)
 # next finding skewness and tranforming data 

#ggdensity(data, x = "calories", fill = "yellow", title = "miles per gallon") + scale_x_continuous() + stat_overlay_normal_density(color = 'red', linetype = "dashed")

#data$calories <- sqrt(data$calories)
#skewness(data$calories)

#ggdensity(data, x = "calories", fill = "yellow", title = "miles per gallon") + scale_x_continuous() + stat_overlay_normal_density(color = 'red', linetype = "dashed")


#skewness(data)
#data <- sqrt(data)
#skewness(data)


#splitting the dataset into training and testing
set.seed(1)
split = sample.split(data$rating, SplitRatio = 0.8)
training_data = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

View(training_data)
View(test_set)

# now fitting the linear model and calculate MSE
# first for training data
train_model = lm(rating ~., data= training_data)
summary(train_model)

predi = predict(train_model, training_data) #given the training data you predict
predi

difference = data.frame(pred = predi, actual = training_data$rating)
difference  #showing the diffrence between actual and predicted values

# MSE error for training data

mse_training = sum((training_data$rating - predi)^2)
mse_training


#now MSE error for testing data
test_model = lm(rating~., data = test_set)
summary(test_model)

predi_test = predict(test_model, test_set) #given the training data you predict
predi_test

difference = data.frame(pred = predi_test, actual = test_set$rating)
difference

mse_test = sum((test_set$rating - predi_test)^2)
mse_test

# forward subset selection
#doing forward subset selection on training set
set.seed(10)
forward_subset = regsubsets(rating~. ,data = training_data, nvmax = 13, method = "forward")
fsummary = summary(forward_subset) #here sugars and then fiber is the best
fsummary
#coef(forward_subset, 1)

plot(forward_subset, scale = "Cp")
plot(forward_subset, scale = "bic")
plot(forward_subset, scale = "bic")


#forward subset on testing data

forward_subset_test = regsubsets(rating~. ,data = test_set, nvmax = 13, method = "forward")
fsummary_test = summary(forward_subset_test)#here sugars and then fiber is the best 
fsummary_test
coef(forward_subset_test, 1)
coef(forward_subset_test, 11)
coef(forward_subset_test, 12)

plot(forward_subset_test, scale = "Cp")
plot(forward_subset_test, scale = "bic")

which.min(fsummary_test$bic)
which.min(fsummary_test$cp)

which.max(fsummary_test$bic)
which.max(fsummary_test$cp)

#Now perform exhaustive subset selection

best_training = regsubsets(rating~., data = training_data, nvmax = 13, method = "exhaustive")
summary_best_train = summary(best_training)
summary_best_train
which.min(summary_best_train$cp)
which.min(summary_best_train$bic)

coef(best_training, 7)

plot(best_training, scale = "Cp")
plot(best_training, scale = "bic")
plot(best_training, scale = "adjr2")

best_subset1 = lm(rating~., data = training_data)
best_subset1
p3 = ols_step_best_subset(best_subset1)
p3
plot(p3)

# now perform for testing
best_test = regsubsets(rating~., data = test_set, nvmax = 13, method = "exhaustive")
summary_best_test = summary(best_test)
summary_best_test
which.min(summary_best_test$cp)
which.min(summary_best_test$bic)
which.min(summary_best_test$adjr2)

coef(best_test, 7)

plot(best_test, scale = "Cp")
plot(best_test, scale = "bic")

best_subset2 = lm(rating~., data = test_set)
best_subset2
p4 = ols_step_best_subset(best_subset2)
p4
plot(p4)


##### second problem #####
#install.packages("ElemStatLearn")



#DATASET.train <- as.data.frame(zip.train)
#visualizing the data
library(dplyr)

train_table = read.table("zip.train")
train_d <- as.data.frame(train_table)

test_table = read.table("zip.test")
test_d <- as.data.frame(test_table)

#head(train_d)


# classification using Linear regression


linear_class = lm(V1~., data = train_d)

boundary <- function(x) { if (x > .5) 3 else 2 }
linear_test_class <- as.character(sapply(predict(linear_class, test_d), boundary))
linear_test_class
linear_train_class <- as.character(sapply(predict(linear_class, test_d), 
                                         boundary))

head(train_d[1:10])
dim(train_d)
View(train_d)
sum(is.na(train_d))


# Now going to transform labels as factor
is.factor(train_d$V1)
train_d$V1 = as.factor(train_d$V1)
is.factor(train_d$V1)
summary(train_d$V1)

par(mfrow = c(1, 1))
par(mar = c(6, 4, 4, 2) + 0.1)
plot <- plot(train_d$V1, main = " Number of Digits (Training Set)", 
             ylim = c(0, 1500), ylab = "Total numbers")
text(x = plot, y = table(train_d$V1) + 50, labels = table(train_d$V1))


#now select only 2 and 3
select1 = c(2, 3)
Two_and_three_train = train_d[train_d$V1 %in% select1,]
View(Two_and_three_train)


is.factor(Two_and_three_train$V1)
summary(Two_and_three_train$V1)

#explore test data set

head(test_d)
dim(test_d)
View(test_d)
#summary(test_d)
sum(is.na(test_d))

# Now going to transform labels as factor
is.factor(test_d$V1)
test_d$V1 = as.factor(test_d$V1)
is.factor(test_d$V1)
summary(test_d$V1)

#visalazing testing data
par(mfrow = c(1, 1))
par(mar = c(4, 4, 4, 2) + 0.1)
plot <- plot(test_d$V1, main = " Number of Digits (Test Set)", 
             ylim = c(0, 500), ylab = "Total numbers")

text(x = plot, y = table(test_d$V1) + 50, labels = table(test_d$V1), cex = 0.75)

#now select only 2 and 3
select2 = c(2, 3)
Two_and_three_test = test_d[test_d$V1 %in% select2,]
View(Two_and_three_test)


is.factor(Two_and_three_test$V1)
summary(Two_and_three_test$V1)


independent_data_train = subset(Two_and_three_train, select = -c(V1)) #drop v1 column as it's for dependent variable
View(independent_data_train)

independent_data_test = subset(Two_and_three_test, select = -c(V1)) #drop v1 column as it's for dependent variable
View(independent_data_test)

#View(Two_and_three_train$V1)


### RUN KNN ALGORITHM ###

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

k_vals <- c(1, 3, 5, 7, 9, 11, 13, 15)

list_train_error <- c()
list_test_error <- c()

list_train_accuracy = c()
list_test_accuracy = c()

for (i in 1:8)
{
  
  # select "k"
  kk <- k_vals[i]
  independent_data_train = subset(Two_and_three_train, select = -c(V1))
  independent_data_test = subset(Two_and_three_test, select = -c(V1))
  # apply the algorithm
  #get prediction on train data 
  train_pred <- knn(train = independent_data_train, test= independent_data_train, cl =Two_and_three_train$V1, k = kk)
  c_matrix1 <- table(train_pred,Two_and_three_train$V1)
  acc1 =accuracy(c_matrix1)
  list_train_accuracy[[i]] = acc1
  
  
  #get prediction on test data
  test_pred <- knn(train = independent_data_train, test= independent_data_test, cl =Two_and_three_train$V1, k = kk)
  
  c_matrix2 <- table(test_pred,Two_and_three_test$V1)
  acc2 =accuracy(c_matrix2)
  list_test_accuracy[[i]] = acc2
  
  # training error rate
  train_err <- mean(Two_and_three_train$V1 != train_pred)
  # training error rate
  test_err <- mean(Two_and_three_test$V1 != test_pred)
  
  list_train_error[[i]] = train_err         #list(train_error=train_err)
  
  list_test_error[[i]] = test_err        #list(test_error = test_err)
}

list_train_error
list_test_error

list_train_accuracy
list_test_accuracy

#knn_class <- knn(independent_data_train, independent_data_test, cl = Two_and_three_train$V1, k = 3)
#c_matrix <- table(knn_class,Two_and_three_test$V1)
#c_matrix


library(ISLR)
names(College)
## third probelm 
dim(College)
head(College)
sum(is.na(College))
summary(College)

College <- College[c(-1)]
head(data)

normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}
College$Apps<-normalize(College$Apps)
College$Accept<-normalize(College$Accept)
College$Enroll<-normalize(College$Enroll)
College$Top10perc<-normalize(College$Top10perc)
College$Top25perc<-normalize(College$Top25perc)
College$F.Undergrad<-normalize(College$F.Undergrad)
College$Outstate<-normalize(College$Outstate)
College$Personal<-normalize(College$Personal)
College$P.Undergrad<-normalize(College$P.Undergrad)
College$Room.Board<-normalize(College$Room.Board)
College$Books<-normalize(College$Books)
College$PhD<-normalize(College$PhD)
College$Terminal<-normalize(College$Terminal)
College$S.F.Ratio<-normalize(College$S.F.Ratio)
College$perc.alumni<-normalize(College$perc.alumni)
College$Expend<-normalize(College$Expend)
College$Grad.Rate<-normalize(College$Grad.Rate)
View(College)

set.seed(1)
split = sample.split(College$Apps, SplitRatio = 0.8)
college_data_train = subset(College, split == TRUE)
college_set_test = subset(College, split == FALSE)

View(college_data_train)
View(college_set_test)

college_test_model = lm(Apps~., data = college_data_train)
summary(college_test_model)

predi_test_c = predict(college_test_model, college_set_test) #given the training data you predict


mse_test_c = sum((college_set_test$Apps - predi_test_c)^2)
mse_test_c




