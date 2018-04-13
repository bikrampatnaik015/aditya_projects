
# installing the pacakages for implementing decision tree algorithm
install.packages("rpart")
library("rpart")
install.packages("rpart.plot")
library("rpart,plot")


# importing the file fedPapers85.csv
df<- read.csv("C:/Users/Aditya/Desktop/fedPapers85.csv")

# checking the structure of the dataframe
str(df)
View(df)

#removing the second column of the dataframe(filename)

df<- df[,-2]
View(df)

#subsetting the dataframe to create the test set
test<-df[c(1:11),]

View(test)
str(df)




# subsettting the dataframe with the rows which have either Madison or Hamilton as the author
df1<- df[c(13:62,71:85),]
View(df1)


# Using the above datasset (df1) and diving it into training and validation using random sampling
dt<-sample(nrow(df1), nrow(df1)*.7) #70 % of rows of df1 are assingned for training
training<-df1[dt,]
validation<-df1[-dt,]


#checking the number of rows
nrow(training)
nrow(validation)


# building decision tree using the rpart package
tree<- rpart(author~.,training,method = "class", minbucket=1)
summary(tree)

# plotting the decison tree

plot(tree)
text(tree,pretty=0)

#checing the accuracy of the model
t<- table(predict(tree,validation,type="class"),validation$author)
t
sum(diag(t))/sum(t)


#####################################
###PREDICTING THE UNKNOWN AUTHORS####
#####################################
predict(tree,test,type = "class")



#Pruning the tree
##Using plotcp function to find the cost for which error is minimum
plotcp(tree)

# generating pruned tree using the cp value which gives minimum error
prunedtree <- prune(tree,cp=.25)
summary(prunedtree)

#####################################################
######PREDICTING UNKONWN AUTHORS USING PRUNED TREE###
#####################################################
predict(prunedtree,test,type="class")
