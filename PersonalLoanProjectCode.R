#===============================================================================
#1. Read-in the data ("Bank_Personal_Loan_Modelling.csv").
personal_loan <- read.csv("Bank_Personal_Loan_Original.csv")
View(personal_loan)

#Explore the data
str(personal_loan)

#there are 14 attributes and 5000 observations
#out of 14 attributes, 5 attributes are numerical
#and 9 attributes are categorical
#the target variable is personal loan

#what is the acceptance rate of the personal loan?
table(personal_loan$Personal.Loan)
prop.table(table(personal_loan$Personal.Loan)) *100
#Observation:
#out of 5000 customers, only 9.6% of customers accepted the personal loan offer 
#in the last campaign
#we see that there is a high class imbalance issue in target variable 

################################################################################
#2. Data Summary and EDA
################################################################################
# Summary of numeric variables
summary(personal_loan[c("Age", "Experience", "Income", "Family", 
                        "CCAvg","Mortgage")])

#Age
summary(personal_loan$Age)
hist(personal_loan$Age)
# Observation: 
#The minimum age is 23 and maximum age is 67.
#Majority of age group in the data are in between 25 and 65. 
#Mean and median age is approximately the same (~45)

#Experience:
summary(personal_loan$Experience)
hist(personal_loan$Experience, col = "light blue")
subset(personal_loan, personal_loan$Experience < 0)
#Observation:
#Some of the number of experience is negative. 
#We need to investigate further and preprocess the data 
#since the number of experience cannot be a negative value
#minimum experience is -3 and maximum experience is 43
#mean and median are the approximately the same (~20)

#Income
summary(personal_loan$Income)
hist(personal_loan$Income, nclass=20, col = "pink")
#Observation:
#Minimum income is 8k and maximum income is 224k 
#Median is smaller than mean, meaning the data is right skewed
#the majority of income of individuals are in between 10k and 90k 


#Family:
summary(personal_loan$Family)

table(personal_loan$Family)
#Observation:
#number of customer with family size of 1 is 1472, 2 is 1296, 4 is 1222
#followed by family size of 3 is 1010

#CCAvg:
summary(personal_loan$CCAvg)
hist(personal_loan$CCAvg, col = "orange")
#Observation:
#the minimum average spending on credit card is 0
#and the maximum is 10k
#the median is smaller than mean, meaning the data is right skewed
#the majority of people has the average spending on credit card is less than 3k

#Mortgage
summary(personal_loan$Mortgage)
hist(personal_loan$Mortgage, col = "lightgreen")
#Observation:
#the minimum of mortgage amount is 0
#and the maximum is 635k
#the median is 0 and we can see that the data is highly skewed to the right
#most of the people have the mortgage amount with less than 50k

# Summary of categorical variables
#ZIP.Code
table(personal_loan$ZIP.Code)
which.max(table(personal_loan$ZIP.Code))
length(unique(personal_loan$ZIP.Code))
#Observation:
#we can see that there are 467 distinct zip code in the data
#the zip code 94720 appears the most (368)

#Education
table(personal_loan$Education)
#Observation:
#the number of customer with undergrad degree (#1) is 2096,  Advanced/Professional (#3) is 1501, 
#followed by Graduate (#2) is 1403

#Securities.Account
table(personal_loan$Securities.Account)
#Observation:
#522 customers have security account with the bank

#CD.Account
table(personal_loan$CD.Account)
#Observation:
#302 customers have certificate of deposit account with the bank

#Online
table(personal_loan$Online)
#Observation:
#More than 50% of the customers are using internet banking facility 

#CreditCard
table(personal_loan$CreditCard)
#Observation:
#Only 1470 customers have credit cards with other banks

#Correlation visualization of numerical data
##############################################
library(psych)
pairs.panels(personal_loan[c("Age", "Experience", "Income", "Family", "CCAvg","Mortgage")], main = "Correlation")
#Observation:
#Age and Experience: a strong positive correlation --> the professional experience increases with age
#Income and CCAvg: a moderate positive correlation --> as income increases, the credit card spending also moderately increases 
#Age and Mortgage: a weak negative correlation -->  no matter what the age is, mortgage does not seem to have much of change
#Income and Mortgage: a weak positive correlation --> as income increases, the mortgage tends to increase slightly

#Income vs Mortgage by Personal Loan: 
ggplot(data = personal_loan)+ 
  geom_point(mapping = aes(x= Income, y = Mortgage, color=as.factor(Personal.Loan) )) 
#Observation:
#Higher income -> higher mortgage for most of the cases 

#Income and Personal Loan by Education: 
ggplot(data = personal_loan)+ 
  geom_point(mapping = aes(x= Personal.Loan, y = Income, color=as.factor(Personal.Loan))) +
  facet_grid(~Education)
#Observation:
#customers with education level 1 tends to not accepting the Personal Loan

#Binning based on Income: High (> 150), Medium (>90 and <150), Low(<90)
personal_loan$income_cat <- ifelse(personal_loan$Income > 150, "High",
                                   ifelse(personal_loan$Income > 90, "Medium", "Low"))

#histogram based on income_cat, Personal.Loan
ggplot(data = personal_loan)+ 
  geom_histogram(aes(x= Personal.Loan, fill=as.factor(Personal.Loan)), stat="count") +
  facet_grid(~income_cat)

#Observation:
#Higher number of customers with medium and high income accepted the personal loan during last campaign
#Compared to the low income customers
#Acceptance rate of low income group appears to be less

#histogram based on income_cat, Personal.Loan
ggplot(data = personal_loan)+ 
  geom_histogram(aes(x= Personal.Loan, fill=as.factor(Personal.Loan)), stat="count") +
  facet_grid(~Education)
#Observation:
#Higher number of customers with graduate degree and advanced/professional degree accepted the personal offer 
#during last campaign

#histogram based on income_cat, Personal.Loan
ggplot(data = personal_loan)+ 
  geom_histogram(aes(x= Personal.Loan,  fill=as.factor(Personal.Loan) ), stat="count") +
  facet_grid(~Family)
#Observation:
#It appears that there is not much relation between family size and personal acceptance rate

#Age and CCAvg: 
ggplot(data = personal_loan)+ 
  geom_point(mapping = aes(x= Age, y = CCAvg, color=as.factor(Personal.Loan))) 
#Observation:
#the ccavg is the same for all age, however, higher ccavg tends to say yes to Personal Loan

#CCAvg and Income by Personal Loan:
ggplot(data = personal_loan)+ 
  geom_point(mapping = aes(x= Income, y = CCAvg, color=as.factor(Personal.Loan))) 
#Observation: 
#Customer with higher income tends to spend more on credit card and tends to say yes to Personal Loan

################################################################################
#3.Data Preprocessing
################################################################################
#Check for the missing values if any
#####################################
num_of_records <- nrow(personal_loan)
num_of_Features <- ncol(personal_loan)
data.summary <- as.data.frame(matrix(nrow = num_of_Features, ncol = 5))

#Add table headings
colnames(data.summary) <- c("Features", "Missing Values", "% of Missing Values",
                            "Inconsistent Values", "% of Inconsistent Values")

#Add feature names in the first column
data.summary[,1] <- colnames(personal_loan)

#Compute missing values per feature
for(i in 1:num_of_Features) {
  data.summary[i,2] <- sum(is.na(personal_loan[,i]))
  data.summary[i,3] <- (sum(is.na(personal_loan[,i]))/num_of_records)*100
}


data.summary
#Observation:
#Data has no missing values

#Check for the inconsistent values if any
#########################################
table(personal_loan$ZIP.Code)
#Observation:
#We found only one inconsistent values in the ZIP.Code attribute (9307)
#Since there is only one row, we are going to ignore this

#Handle the inconsistent values for Experience
table(personal_loan$Experience)
#Observation:
#We found few negative values in Experience attribute
#We will change it to positive value

for (i in 1:num_of_records){
  if((personal_loan$Experience[i] < 0)){
    (personal_loan$Experience[i] = abs(personal_loan$Experience[i]))
  }
}

table(personal_loan$Experience) #check after fixing the inconsistencies

################################################################################
#4.Construct multiple modeling
################################################################################
#Divide into test and training data sets
set.seed(02468)
rowvec <- 1:nrow(personal_loan)

#Split train and test set by ~80 and ~20 percentage randomly
train_vec <- sample(rowvec, floor(nrow(personal_loan)* 0.80), replace = FALSE)
test_vec <- rowvec[!is.element(rowvec, train_vec)]

train_data <- personal_loan[train_vec,]
test_data <- personal_loan[test_vec,]

#PART A: PREDICTIVE ANALYSIS
######################---------1.--KNN------------##############################
modelLookup("knn")
#Customize the parameter tuning process using trainControl() function
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, 
                     selectionFunction = "oneSE")

#use expand.grid() to create grid of tuning parameters for knn method -> K value
knn_grid <- expand.grid(k = c(15, 21, 41))
knn_grid

#customize train() with 10-fold cv control list and knn_grid with parameter k
set.seed(02468)
model_knn <- train(as.factor(Personal.Loan) ~ ., data = train_data,
                   method = "knn", metric = "Kappa",
                   trControl = ctrl, tuneGrid = knn_grid)
model_knn
#The optimal model is the one with k = 41.

#Model performance evaluation on train data
pred_knn_check <- predict(model_knn, train_data, type = "raw")
confusionMatrix(pred_knn_check, as.factor(train_data$Personal.Loan), positive = '1')
#The accuracy rate on the training data is 90.3%.

#apply the best knn candidate model to make predictions
library(gmodels)
pred_knn <- predict(model_knn, test_data, type = "raw")
confusionMatrix(pred_knn, as.factor(test_data$Personal.Loan), positive = '1')
#The accuracy rate on the testing data is 90.8%.

##################---------2. Logistic Regression------------########################
library("glm2")
#create a model
mylogit <- glm(Personal.Loan ~ .- income_cat, data = train_data, family = "binomial")
summary(mylogit)

#use the model on training data
pred_logit_train <- predict(mylogit, newdata = train_data, type = "response")
pred_logit_train <- ifelse(pred_logit_train > 0.5, 1, 0)
pred_logit_train

#model evaluation for training data
confusionMatrix(as.factor(pred_logit_train), as.factor(train_data$Personal.Loan), positive = '1')
#The accuracy rate on the training data is 94.98%.

#use the model on testing data
pred_logit <- predict(mylogit, newdata = test_data, type = "response")
pred_logit <- ifelse(pred_logit > 0.5, 1, 0)
pred_logit

#model evaluation for testing data
confusionMatrix(as.factor(pred_logit), as.factor(test_data$Personal.Loan), positive = '1')
#The accuracy rate on the test data is 95.6%.


##################---------3.--Decision Tree------------########################
modelLookup("C5.0")
library(caret)
#Customize the parameter tuning process using trainControl() function
ctrl <- trainControl(method = "repeatedcv", number = 10, 
                     selectionFunction = "oneSE")

#use expand.grid() to create grid of tuning parameters for Decision Tree
dt_grid <- expand.grid(model = "tree",
                       trials = c(5,10,15,20,25),
                       winnow = FALSE)
dt_grid

#customize train() with 10-fold cv control list and dt_grid
set.seed(1234)
model_dt <- train(as.factor(Personal.Loan) ~ .-ID - income_cat -ZIP.Code, data = train_data,
                  method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = dt_grid)
model_dt
summary(model_dt)

#Optimal model is the one with trials = 15, model - tree and winnow = FALSE.

#Model performance evaluation on train data
pred_dt_check <- predict(model_dt, train_data, type = "raw")
confusionMatrix(pred_dt_check, as.factor(train_data$Personal.Loan), positive = '1')
#The accuracy rate on the training data is 98.95%.

#apply the best Decision Tree model to make predictions
pred_dt <- predict(model_dt, test_data, type = "raw")
confusionMatrix(pred_dt, as.factor(test_data$Personal.Loan), positive = '1')
#The accuracy rate on the test data is 98.5%.

##################---------4.Random Forest------------########################
library(caret)
library(randomForest)
modelLookup("randomForest")
#create a train control of 10 fold cross validation
set.seed(02468)
ctrl3 <- trainControl(method = "cv", 
                      number = 10,
                      selectionFunction = "best",
                      savePredictions = TRUE,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)

#Random forest will use square root of total number of features per tree
#hence, we create an expand grid with the values of 2, 4, 8, and 16
rf_grid <- expand.grid(mtry = c(2,4,8,16))

#fit the random forest model
set.seed(02468)

train_data$Personal.Loan<-ifelse(train_data$Personal.Loan==1, "Yes","No")
test_data$Personal.Loan<-ifelse(test_data$Personal.Loan==1, "Yes","No")

table(train_data$Personal.Loan)
table(test_data$Personal.Loan)
model_rf <- train(as.factor(Personal.Loan) ~ ., data = train_data, 
                  method = "rf", metric = "ROC",
                  trControl = ctrl3, tuneGrid = rf_grid)
model_rf
#optimal model is the one with mtry = 16.

#Model performance evaluation on train data
pred_rf_check <- predict(model_rf, train_data, type = "raw")
table(pred_rf_check, train_data$Personal.Loan)
#The accuracy rate on the training data is 100%.

#Prediction vs Observation for various values of mtry
r <- as.data.frame(model_rf$pred)
r2_pred <- r[r$mtry == 2,1] #mtry=2
r2_act <- r[r$mtry == 2,2] #mtry=2
r4_pred <- r[r$mtry == 4,1] #mtry=4
r4_act <- r[r$mtry == 4,2] #mtry=4
r8_pred <- r[r$mtry == 8,1]#mtry=8
r8_act <- r[r$mtry == 8,2] #mtry=8
r16_pred <- r[r$mtry == 16,1] #mtry=16
r16_act <- r[r$mtry == 16,2] #mtry=16

#install.packages("ROCR")
#ROC curve for different values of mtry
library(ROCR) 
pred <- prediction( predictions = as.numeric(r16_pred),
                    labels = as.numeric(r16_act))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot( perf, main = "ROC CURVE", col = "Green", lwd = 3)
abline( a = 0, b = 1, lwd = 2, lty = 2)

#apply the best random forest model to make predictions
pred_rf <- predict(model_rf, test_data[,-10], type = "raw")
confusionMatrix(pred_rf, as.factor(test_data$Personal.Loan), positive = "Yes")
#The accuracy rate on the testing data is 98.6%.

#PART B: DESCRIPTIVE ANALYSIS
##################---------5. k-means clustering------------########################
#remove the target feature from the data 
personal_loan_k <- personal_loan[, -10]

#normalize the data frame using lapply() and scale() functions
personal_loan_z <- as.data.frame(lapply(personal_loan_k[, c(3, 4, 7, 9, 10, 11, 12, 13)], scale))

#explore the data after normalization
str(personal_loan_z)

#training the model on the data
library(factoextra)
RNGversion("3.5.2")
set.seed(2345)
loan_cluster <- kmeans(personal_loan_z, 3)

#visualization for 3 groups of clusters
fviz_cluster(loan_cluster, personal_loan_z)
#we tested different values of k to build the model.
#and k=3 is the best split

#evaluate the model performance
###############################
#visualize SSE for different values of k 
mydata <- personal_loan_k[,-14]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#obtain the size of clusters
loan_cluster$size

#examine the coordinates of the cluster centroids
loan_cluster$centers

#improving model performance
personal_loan_k$cluster <- loan_cluster$cluster
personal_loan_k[1:10, c("cluster", "Age", "Family", "Education")]

#Distribution of age by cluster
aggregate(data = personal_loan_k, Age ~ cluster, mean)

ggplot(data = personal_loan_k)+ 
  geom_histogram(aes(x= Age, fill=as.factor(cluster)), stat="count") +
  facet_grid(~cluster) #histogram of Age

#Observation:
#Cluster 1 has majority  of customers with age distributed between 30 and 40 (younger population)
#Cluster 2 has majority of customers with age distributed between 50 and 60 (older population)
#Cluster 3 has majority  of customers with age distributed between 40 and 50 (middle age population)
#and it is more dispersed compared to other clusters


#Distribution of family by cluster
aggregate(data = personal_loan_k, Family ~ cluster, mean)

ggplot(data = personal_loan_k)+ 
  geom_histogram(aes(x= Family, fill=as.factor(cluster)), stat="count") +
  facet_grid(~cluster) #histogram of Family
#Observation:
#Average family is lower for cluster 3 compared to other clusters

#Distribution of Education level by cluster
aggregate(data = personal_loan_k, Education ~ cluster, mean)
#Observation:
#Since we know that the age is widely distributed, it seems likely that cluster 3
#has mean education level of 1.625138 (lower mean education level compared to other clusters)

#Distribution of Experience level by cluster
aggregate(data = personal_loan_k, Experience ~ cluster, mean)

ggplot(data = personal_loan_k)+ 
  geom_histogram(aes(x= Experience, fill =as.factor(cluster)), stat="count")  #histogram of Experience

#Observation:
#This visualization shows that as the age increases, the experience increases as well as expected

#Summarization for k-means clustering analysis using visualization
##################################################################
#Age and Income by cluster
#choosing color palette
library(RColorBrewer)
myColors <- brewer.pal(3,"Set2")
names(myColors) <- levels(personal_loan_k$cluster)
colScale <- scale_colour_manual(name = "Cluster",values = myColors) 

#Age and Income by cluster visualization 
########################################################################
p <- ggplot(personal_loan_k, aes(Age, Income,colour = as.factor(cluster))) + geom_point()
p1 <- p + colScale
p1
#Observation:
#Cluster 1 represents younger population with lower income compared to cluster 3
#Cluster 2 represents older population with lower income compared to cluster 3
#Cluster 3 represents middle age population with higher income compared to other clusters




