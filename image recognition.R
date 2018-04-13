
####Reading the sample data ########
df<- read.csv("C:/Users/Aditya/Desktop/train.csv")
df1<-read.csv("C:/Users/Aditya/Desktop/test.csv")


#checing the strucutre of df
str(df)
sum(is.na(df))


### diving df into train and test sets
x_train<- df[1:1000,]
y_train<- df[1:1000,1]
View(x_train)
x_test<-df[1001:1400, ]
View(x_test)
y_test <- df[1001:1400,1]
y_test

#################################
####implementing Naive Bayes#####
#################################

###installing package for implementing navieBayes
install.packages("e1071")
library("e1071")


#####implementing naiveBayes and generating predictions###
start_time <- Sys.time()   ###used for calculating function / algorithm execution time
model_naiveBayes <- naiveBayes(x_train,as.factor(y_train))
end_time <- Sys.time()     ###used for calculating function / algorithm execution time
end_time - start_time      ###used for calculating function / algorithm execution time


predict(model_naiveBayes,x_test[,-1])
t<-table(predict(model_naiveBayes,x_test[,-1]),as.factor(y_test))
sum(diag(t))/sum(t)      #### calculating the training accuracy


### predicting  the unkonwn labels in the test dataset
predict(model_naiveBayes,df1[,-1])





################################
###implementing SVM#############
#################################

x_train$label <- as.factor(x_train$label)

start_time <- Sys.time()     ###used for calculating function / algorithm execution time
model_svm <- svm(formula= x_train$label ~ .,data= x_train, cost=100, gamma=0.1,kernel="linear")

end_time <- Sys.time()      ###used for calculating function / algorithm execution time
end_time - start_time       ###used for calculating function / algorithm execution time

predict(model_svm,x_test[,-1])
t<-table(predict(model_svm,x_test[,-1]),y_test)
sum(diag(t))/sum(t)            #### calculating the training accuracy


### predicting  the unkonwn labels in the test dataset
predict(model_svm,df1[,-1])


################################
#########implementing KNN#######
################################

######writing a function to scale down the data###
#### taking squre root of each data point to reduce the magnitude ##

normalize <- function(x) {
  return(sqrt(x))
}



####diving into training and testing#####
x_train<- df[1:1000,]
y_train<- df[1:1000,1]   #####target variable while training#######




#####applying the normalize function to scale the data#########
new_x_train<-as.data.frame(lapply(x_train,normalize))

x_test<-df[1001:1400, ]
new_x_test <-as.data.frame(lapply(x_test,normalize))
new_x_test


y_test <- df[1001:1400,1]
y_test


#### installing package for knn####
install.packages("class")
library("class")


######### finding optimal k value#######

model_knn <- NULL
error.rate <- NULL

for(i in 1:10){
  model_knn <- knn(new_x_train,new_x_test,cl= y_train, k=i)
  error.rate[i]<- mean(y_test!=model_knn)
}
print(error.rate)

####### Visualizing elbow method to tune knn#####

install.packages("ggplot2")
library("ggplot2")

k.values<- 1:10
error.df <- data.frame(error.rate,k.values)

ggplot(error.df,aes(k.values,error.rate)) + geom_point()  ###k=6 gives lowest error

#### implementing knn with k=6

start_time <- Sys.time()    ###used for calculating function / algorithm execution time
model_knn <- knn(new_x_train,new_x_test,cl= y_train,k=6)
end_time <- Sys.time()     ###used for calculating function / algorithm execution time
end_time - start_time      ###used for calculating function / algorithm execution time


#####accuracy calculation###
t<-table(y_test,model_knn)
sum(diag(t))/sum(t)          ### accuracy


#### predciting the unkown labels in df1

df2<- as.data.frame(lapply(df1[,-1],normalize))  ##### normalizing df1(testing set)
df2
sum(is.na(df2))      ####### knn does not work if testing set has missing values
df3<-(na.omit(df2))  ###### removing all missing values from testing set

model_knn1 <- knn(new_x_train[,-1],df3 ,cl= y_train,k=6)  ###train and test should have same dimension (cols)
model_knn1          ##### predictions


