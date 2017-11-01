library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
library(ggord)
library(ggplot2)
library(Hmisc)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
library(SDMTools)
library(dplyr)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)

setwd("/Users/pranavdar/Desktop/PGPBABI/Machine Learning")
UP.Analysis<-read.csv(file.choose(), header=T)
na.omit(UP.Analysis)

summary(UP.Analysis)
str(UP.Analysis)


#Define some dummies
UP.Analysis$Criminal <-as.numeric(UP.Analysis$Criminal.Case)
UP.Analysis$Crime<-ifelse(UP.Analysis$Criminal>0,1,0)
UP.Analysis$Crorepati<-ifelse(UP.Analysis$Total.Assets>10000000,1,0)
UP.Analysis$INC<-ifelse(UP.Analysis$Party=="INC",1,0)
UP.Analysis$BJP<-ifelse(UP.Analysis$Party=="BJP",1,0)
UP.Analysis$BSP<-ifelse(UP.Analysis$Party=="BSP",1,0)
UP.Analysis$CPI<-ifelse(UP.Analysis$Party=="CPI",1,0)
UP.Analysis$CPM<-ifelse(UP.Analysis$Party=="CPM",1,0)
UP.Analysis$SP<-ifelse(UP.Analysis$Party=="SP",1,0)

UP.Analysis$National<-UP.Analysis$INC+UP.Analysis$BJP+UP.Analysis$CPM+UP.Analysis$CPI+UP.Analysis$BSP+UP.Analysis$SP
UP.Analysis$Major<-UP.Analysis$INC+UP.Analysis$BJP+UP.Analysis$SP+UP.Analysis$BSP
UP.Analysis$Graduate<-ifelse(UP.Analysis$Education =="Graduate",1,0)
UP.Analysis$Doctorate<-ifelse(UP.Analysis$Education=="Doctorate",1,0)
UP.Analysis$Graduate.Professional<-ifelse(UP.Analysis$Education=="Graduate Professional",1,0)
UP.Analysis$Post.Graduate<-ifelse(UP.Analysis$Education=="Post Graduate",1,0)
UP.Analysis$Grad=UP.Analysis$Graduate+UP.Analysis$Doctorate+UP.Analysis$Graduate.Professional + UP.Analysis$Post.Graduate
UP.Analysis$GEN<-ifelse(UP.Analysis$AC_TYPE=="GEN",1,0)
UP.Analysis$Winner<-ifelse(UP.Analysis$POSITION=="1",1,0)
UP.Analysis$Serious<-ifelse(UP.Analysis$ser>0,1,0)
UP.Analysis$Female<-ifelse(UP.Analysis$Gender=="F",1,0)
UP.Analysis$Win.Party<-ifelse(UP.Analysis$Winner==1,UP.Analysis$Party,0)
UP.Analysis$Vote.Share<-(UP.Analysis$Votes)/UP.Analysis$Polled
UP.Analysis$Turnout<-UP.Analysis$Polled/UP.Analysis$Electors

levels(UP.Analysis$Education )

UP.Analysis$Qual<-as.character(UP.Analysis$Education)

UP.Analysis$Qual[UP.Analysis$Qual=="10th Pass"]<-"School.Complete"
UP.Analysis$Qual[UP.Analysis$Qual=="12th Pass"]<-"School.Complete"
UP.Analysis$Qual[UP.Analysis$Qual=="5th Pass"]<-"Secondary"
UP.Analysis$Qual[UP.Analysis$Qual=="8th Pass"]<-"Secondary"
UP.Analysis$Qual[UP.Analysis$Qual=="Doctorate"]<-"Graduate"
UP.Analysis$Qual[UP.Analysis$Qual=="Graduate"]<-"Graduate"
UP.Analysis$Qual[UP.Analysis$Qual=="Graduate Professional"]<-"Graduate"
UP.Analysis$Qual[UP.Analysis$Qual=="Iliterate"]<-"No.Schooling"
UP.Analysis$Qual[UP.Analysis$Qual=="Literate"]<-"No.Schooling"
UP.Analysis$Qual[UP.Analysis$Qual=="Others"]<-"School.Complete"
UP.Analysis$Qual[UP.Analysis$Qual=="Post Graduate"]<-"Graduate"
UP.Analysis$Qual<-as.factor((UP.Analysis$Qual))
UP.Analysis$years.studied<-ifelse(UP.Analysis$Education=="Illiterate",0,ifelse(UP.Analysis$Education=="Literate",1,
                                                                               ifelse(UP.Analysis$Education=="5th Pass",5,
                                                                                      ifelse(UP.Analysis$Education=="8th Pass",8,ifelse(UP.Analysis$Education=="10th Pass",10,                     
                                                                                                                                        ifelse(UP.Analysis$Education=="12th Pass",12,ifelse(UP.Analysis$Education=="Graduate",
                                                                                                                                                                                            15,ifelse(UP.Analysis$Education=="Graduate Professional",16,ifelse(UP.Analysis$Education=="Post Graduate",17,
                                                                                                                                                                                                                                                               ifelse(UP.Analysis$Education=="Doctorate",22,5))))))))))
sum(UP.Analysis$Winner)



#Create some variables and retain that is only essential
#First remove unnecessary data
#Constituencies without winners!



UP.Analysis<-transform(UP.Analysis, no.winner = ave(UP.Analysis$Winner, UP.Analysis$AC_NAME,
                                                    FUN = max))
UP.Analysis<-subset(UP.Analysis,UP.Analysis$no.winner!=0)
sum(UP.Analysis$Winner)



#Constituency Level data

UP.Analysis <- transform(UP.Analysis, 
                         rank = ave(UP.Analysis$Total.Assets, UP.Analysis$AC_NAME, 
                                    FUN = function(x) rank(-x, ties.method = "first")))
UP.Analysis$Asset.Rank<-UP.Analysis$rank

normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}


UP.Analysis<-transform(UP.Analysis, mean.assets=ave(UP.Analysis$Total.Assets, UP.Analysis$AC_NAME, 
                                                    FUN = normalize))

UP.Analysis<-transform(UP.Analysis, mean.age=ave(UP.Analysis$Age, UP.Analysis$AC_NAME, 
                                                 FUN = normalize))

UP.Analysis<-transform(UP.Analysis, mean.studied=ave(UP.Analysis$years.studied, UP.Analysis$AC_NAME, 
                                                     FUN = normalize))




UP.Analysis$Rich<-ifelse(UP.Analysis$Asset.Rank==1,"R1",
                         ifelse(UP.Analysis$Asset.Rank==2,"R2",
                                ifelse(UP.Analysis$Asset.Rank==3,"R3","NR")))

UP.Analysis$Rich<-as.factor(UP.Analysis$Rich)
UP.Analysis$Vote.Share<-UP.Analysis$Votes/UP.Analysis$Polled
UP.Analysis$Status<-as.factor(ifelse(UP.Analysis$Winner==1,"Win","No"))
UP.Analysis$Position<-as.factor(ifelse(UP.Analysis$POSITION==1,"P1",ifelse(UP.Analysis$POSITION==2,"P2",
                                                                           ifelse(UP.Analysis$POSITION==3,"P3","Rest"))))

#PLOTS


# qplot()
qplot(mean.assets,  Asset.Rank, colour = Status, data=UP.Analysis)
qplot(mean.assets,  Asset.Rank, colour = Position, data=UP.Analysis)

qplot(mean.assets,  mean.age, colour = Status, data=UP.Analysis)
qplot(mean.assets,  mean.age, colour = Position, data=UP.Analysis)
qplot(mean.assets,  mean.age, colour = Rich, data=UP.Analysis)
qplot(mean.assets,  Vote.Share, colour = Rich, data=UP.Analysis)
qplot(mean.age,  Vote.Share, colour = Rich, data=UP.Analysis)
qplot(mean.assets,  mean.studied, colour = Status, data=UP.Analysis)

qplot(Asset.Rank,  mean.studied, colour = Position, data=UP.Analysis)

#Scatter Plots and Correlation

UP.Scatter<-subset(UP.Analysis[,c(37:51,54)])

#pairs.panels(UP.Scatter[1:15], gap=0, bg=c("red", "blue","green",'orange')[UP.Scatter$Position], pch=21)



#What factors play role?




####variable Selection
#Variables...does being richer candidate help
boxplot(UP.Analysis$Vote.Share  ~UP.Analysis$Rich)
aov.Rich<-aov(UP.Analysis$Vote.Share  ~UP.Analysis$Rich)
summary(aov.Rich)
tk.1<-TukeyHSD(aov.Rich)
tk.1
plot(tk.1)



#Variables...does qualification help
boxplot(UP.Analysis$Vote.Share  ~UP.Analysis$Qual)
aov.Qualification<-aov(UP.Analysis$Vote.Share  ~UP.Analysis$Qual)
summary(aov.Qualification)
tk.3<-TukeyHSD(aov.Qualification)
tk.3
plot(tk.3)




#Now time for analysis

#How many Models?
#Districts and Clusters

UP.Analysis<-transform(UP.Analysis, dist.age=ave(UP.Analysis$Age, UP.Analysis$DIST_NAME
))
UP.Analysis<-transform(UP.Analysis, dist.TA=ave(UP.Analysis$Total.Assets, UP.Analysis$DIST_NAME))
UP.Analysis<-transform(UP.Analysis, dist.Grad=ave(UP.Analysis$Grad, UP.Analysis$DIST_NAME))
UP.Analysis<-transform(UP.Analysis, dist.Fem=ave(UP.Analysis$Female, UP.Analysis$DIST_NAME))

UP.Analysis<-transform(UP.Analysis, dist.Serious=ave(UP.Analysis$ser, UP.Analysis$DIST_NAME))
UP.Analysis<-transform(UP.Analysis, dist.Crime=ave(UP.Analysis$Crime, UP.Analysis$DIST_NAME))
UP.Analysis<-transform(UP.Analysis, dist.Incmbnt=ave(UP.Analysis$Incumbent.Party, UP.Analysis$DIST_NAME))
UP.Analysis<-transform(UP.Analysis, dist.gen=ave(UP.Analysis$GEN, UP.Analysis$DIST_NAME))
UP.Analysis<-transform(UP.Analysis, dist.electors=ave(UP.Analysis$Electors, UP.Analysis$DIST_NAME))
UP.Analysis<-transform(UP.Analysis, dist.polled=ave(UP.Analysis$Polled, UP.Analysis$DIST_NAME))

UP.Analysis$dist.turnout<-UP.Analysis$dist.polled/UP.Analysis$dist.electors
UP.Cluster<-UP.Analysis[,c(1,55:65)]
#UP.Cluster<-sort(UP.Cluster$DIST_NAME)
UP.Cluster<- UP.Cluster[!duplicated(UP.Cluster), ]

#Make the data in random order

randomorder<-runif(nrow(UP.Cluster))
UP.Cluster<-UP.Cluster[order(randomorder),]
#Define Clusters:
#Scatterplot with label
UP.Cluster$Category<-ifelse(UP.Cluster$dist.gen==1,"GEN", "RES")

qplot(dist.electors,dist.TA, colour = Category,data=UP.Cluster)


Cluster.dist.2<-UP.Cluster[,c(1:10,12)]

#Clustering all attributes
#Units Scaling
variables<-colnames(Cluster.dist.2)[-1]
nmatrix<-scale(Cluster.dist.2[,variables])
ncenter<-attr(nmatrix, "scaled:center")
nscale<-attr(nmatrix, "scaled:scale")

#Normalize
z<-Cluster.dist.2[,-c(1,1)]
z
m<-apply(z,2,mean)
m
s<-apply(z,2,sd)
s
z1<-scale(z,m,s)
z1
#Euceldian dist
distance<-dist(z1)
print(distance, digits=3)

# Alternate Distance function 
#d_Euc<-dist(nmatrix, method="euclidean")
#d_Mht<-dist(nmatrix, method="manhattan")
#Hierarchial clustering (ward method: each points are indiv clusters,
#then aggregate to minimize Within SS)

#Cluster Dendogram with Complete Linkage
hc.UP_c<-hclust(distance)
plot(hc.UP_c, labels=Cluster.dist.2$Dist.EEC)
print(hc.UP_c)

#Cluster Dendogram with Average linkage

hc.UP_a<-hclust(distance, method="average")
plot(hc.UP_a, labels=Cluster.dist.2$Dist.EEC)
print(hc.UP_a)
#Comparisons of two methods
#extract members

members_hc.UP.c<-cutree(hc.UP_c,k=2)
members_hc.UP.a<-cutree(hc.UP_a,k=2)

#make table
table(members_hc.UP.c,members_hc.UP.a)

#What variables are playing roles?
aggregate(z1,list(members_hc.UP.c),mean)

#We can repeat the same for Cluster by constituencies

#So?

#First, let's trim the data set
UP.work<-UP.Analysis[,c(2,18:23,31,36:45,48:54)]


#Partitioning Data Sets
#Partition train and val
#We will use this throughout so that samples are comparable
set.seed(1234)
pd<-sample(2,nrow(UP.work),replace=TRUE, prob=c(0.7,0.3))

0.7*374

train<-UP.work[pd==1,]
val<-UP.work[pd==2,]


sum(UP.Analysis$Winner)
sum(val$Winner)
sum(train$Winner)

##########################################
#So what does classification algorithms do really?

#####################

#####################
train.2fact<-train[,c(20,22,24)]

val.2fact<-val[,c(20,22,24)]


NB.1<-naiveBayes(x=train.2fact[-3], y=train.2fact$Status)
#pedict
y_pred<-predict(NB.1,newdata=val.2fact[-3])
#Confusion matrix



cm.NB.1=table(val.2fact[,3],y_pred)
cm.NB.1
# Visualising the Training set results
library(ElemStatLearn)
set = train.2fact
X1 = seq(min(set[, 1])-0.1 , max(set[, 1])+0.1 , by = 0.005)
X2 = seq(min(set[, 2]) -0.1, max(set[, 2])+0.1 , by = 0.005)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('mean.assets', 'mean.studied')
y_grid = predict(NB.1, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Training set)',
     xlab = 'mean.assets', ylab = 'mean.studied',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "Win", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "Win", 'green', 'black'))


#Now for test
set = val.2fact
X1 = seq(min(set[, 1])-0.1 , max(set[, 1])+0.1 , by = 0.005)
X2 = seq(min(set[, 2]) -0.1, max(set[, 2])+0.1 , by = 0.005)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('mean.assets', 'mean.studied')
y_grid = predict(NB.1, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Val set)',
     xlab = 'meanassets', ylab = 'mean.studied',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "Win", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "Win", 'green', 'black'))



##########################
#Knn frames


y_pred<-knn(train=train.2fact[,-3],test=val.2fact[-3], cl=train.2fact[,3],k=3)
cm.knn<-table(val.2fact[,3],y_pred)
cm.knn

#Visualizing Training set
set = train.2fact
X1 = seq(min(set[, 1]) , max(set[, 1]) , by = 0.001)
X2 = seq(min(set[, 2]), max(set[, 2]) , by = 0.001)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('mean.assets',  'mean.studied')
y_grid = knn(train=train.2fact[,-3],test=grid_set[-3], cl=train.2fact[,3],k=3)
plot(set[, -3],
     main = 'Knn (Training set)',
     xlab = 'mean.assets', ylab = 'mean.studied',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "Win", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "Win", 'black', 'green'))


#Now for test
set = val.2fact
X1 = seq(min(set[, 1]) , max(set[, 1]) , by = 0.001)
X2 = seq(min(set[, 2]), max(set[, 2]) , by = 0.001)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('mean.assets',  'mean.studied')
y_grid = knn(train=train.2fact[,-3],test=grid_set[-3], cl=train.2fact[,3],k=3)
plot(set[, -3],
     main = 'Knn (Val set)',
     xlab = 'mean.assets', ylab = 'mean.studied',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == "Win", 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == "Win", 'black', 'green'))







#Logistic
train.2fact$Win<-ifelse(train.2fact$Status=="Win",1,0)
val.2fact$Win<-ifelse(val.2fact$Status=="Win",1,0)

train.2fact<-train.2fact[,c(1,2,4)]
val.2fact<-val.2fact[,c(1,2,4)]



logit.1<-glm(Win~mean.assets+mean.studied, family=binomial,data=train.2fact)
logit.1
#pedict
y_pred<-predict(logit.1,type='response',newdata=val.2fact[-3])
#Confusion matrix

cm.logit=table(val.2fact[,3],y_pred>0.2)
cm.logit
# Visualising the Val set results
library(ElemStatLearn)
set = val.2fact
X1 = seq(min(set[, 1]) - 0.1, max(set[, 1]) + 0.1, by = 0.0051)
X2 = seq(min(set[, 2]) - 0.1, max(set[, 2]) + 0.1, by = 0.0051)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('mean.assets', 'mean.studied')


y_grid = predict(logit.1, type='response',newdata = grid_set)
plot(set[, -3],
     main = 'logit (Training set)',
     xlab = 'mean.assets', ylab = 'mean.studied',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green', 'black'))


#Now for test
set = test_set
X1 = seq(min(set[, 1]) -0.1, max(set[, 1]) + 0.1, by = 0.0051)
X2 = seq(min(set[, 2]) - 0.1, max(set[, 2]) + 0.1, by = 0.0051)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('norm.age', 'norm.Salary')
y_grid = predict(logit.1, newdata = grid_set)
plot(set[, -3],
     main = 'logit (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'blue', 'red'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'white', 'black'))
# Visualising the  results
library(ElemStatLearn)

#############
#First the Count Based Models: NB and KNN
#Data Frames
train.NB<-train[,c(4,6,7,12,13,20:22,24)]
val.NB<-val[,c(4,6,7,12,13,20:22,24)]


######
#NB


NB<-naiveBayes(x=train.NB[-9], y=train.NB$Status)
#pedict
y_pred.NB<-predict(NB,newdata=val.NB[-9])
y_pred.NB


#Confusion matrix

tab.NB=table(val.NB[,9],y_pred.NB)
tab.NB

tab.NB<-table(val.NB[,9],y_pred.3)
tab.NB

accuracy.NB<-sum(diag(tab.NB))/sum(tab.NB)
accuracy.NB
waste.NB<-tab.NB[1,2]/(tab.NB[1,2]+tab.NB[2,2])
waste.NB
lost.NB<-tab.NB[2,1]/(tab.NB[2,1]+tab.NB[2,2])
lost.NB
Loss.fn.NB<-0.6*(lost.NB)^2+0.4*(waste.NB)^2
Loss.fn.NB

####KNN
#knn3
y_pred.3<-knn(train=train.NB[,-9],test=val.NB[-9], cl=train.NB[,9],k=3)
tab.knn.3<-table(val.NB[,9],y_pred.3)
tab.knn.3

accuracy.knn.3<-sum(diag(tab.knn.3))/sum(tab.knn.3)
accuracy.knn.3
waste.knn.3<-tab.knn.3[1,2]/(tab.knn.3[1,2]+tab.knn.3[2,2])
waste.knn.3
lost.knn.3<-tab.knn.3[2,1]/(tab.knn.3[2,1]+tab.knn.3[2,2])
lost.knn.3
Loss.fn.knn.3<-0.6*(lost.knn.3)^2+0.4*(waste.knn.3)^2
Loss.fn.knn.3

#knn5
y_pred.5<-knn(train=train.NB[,-9],test=val.NB[-9], cl=train.NB[,9],k=5)
tab.knn.5<-table(val.NB[,9],y_pred.5)
tab.knn.5

accuracy.knn.5<-sum(diag(tab.knn.5))/sum(tab.knn.5)
accuracy.knn.5
waste.knn.5<-tab.knn.5[1,2]/(tab.knn.5[1,2]+tab.knn.5[2,2])
waste.knn.5
lost.knn.5<-tab.knn.5[2,1]/(tab.knn.5[2,1]+tab.knn.5[2,2])
lost.knn.5
Loss.fn.knn.5<-0.6*(lost.knn.5)^2+0.4*(waste.knn.5)^2
Loss.fn.knn.5

#knn7
y_pred.7<-knn(train=train.NB[,-9],test=val.NB[-9], cl=train.NB[,9],k=7)
tab.knn.7<-table(val.NB[,9],y_pred.7)
tab.knn.7

accuracy.knn.7<-sum(diag(tab.knn.7))/sum(tab.knn.7)
accuracy.knn.7
waste.knn.7<-tab.knn.7[1,2]/(tab.knn.7[1,2]+tab.knn.7[2,2])
waste.knn.7
lost.knn.7<-tab.knn.7[2,1]/(tab.knn.7[2,1]+tab.knn.7[2,2])
lost.knn.7
Loss.fn.knn.7<-0.6*(lost.knn.7)^2+0.4*(waste.knn.7)^2
Loss.fn.knn.7

#knn9
y_pred.9<-knn(train=train.NB[,-9],test=val.NB[-9], cl=train.NB[,9],k=9)
tab.knn.9<-table(val.NB[,9],y_pred.9)
tab.knn.9

accuracy.knn.9<-sum(diag(tab.knn.9))/sum(tab.knn.9)
accuracy.knn.9
waste.knn.9<-tab.knn.9[1,2]/(tab.knn.9[1,2]+tab.knn.9[2,2])
waste.knn.9
lost.knn.9<-tab.knn.9[2,1]/(tab.knn.9[2,1]+tab.knn.9[2,2])
lost.knn.9
Loss.fn.knn.9<-0.6*(lost.knn.9)^2+0.4*(waste.knn.9)^2
Loss.fn.knn.9

########

###Regression Based Models
#Data Frame for Linear, LPM and logit

train.reg<-train[,c(1,4,6:9,11:13,15,19:22)]
val.reg<-val[,c(1,4,6:9,11:13,15,19:22)]

str(train.reg)
###Now some basic commands

Linear<-Vote.Share~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied
LPM<-Winner~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied
logit<-Winner~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

#Linear Regression, Vote Share as Proxy



#Linear Regression


OLS<-lm(Linear, data=train.reg)
summary (OLS)
#VIF:1/(1-R^2)

vif(OLS)

#Retain Only Significant  ones
#Dropped: CAND_AGE, Grad,Female, Crime.reduce,Ser.Inc,Serious

Linear.1<-Vote.Share~Incumbent.Party +Crorepati+  Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

OLS.1<-lm(Linear.1, data=train.reg)
summary (OLS.1)
vif(OLS.1)

val$Pred_OLS.1 <- predict(OLS.1, val.reg)
Pred.win<-ddply(val, .(AC_NAME), transform, 
                max.share=max(Pred_OLS.1))
Pred.win$win<-ifelse(Pred.win$Pred_OLS.1==Pred.win$max.share,1,0)

tab.reg<-table(Pred.win$Winner, Pred.win$win > 0.5)
tab.reg

accuracy.reg<-sum(diag(tab.reg))/sum(tab.reg)
accuracy.reg
waste.reg<-tab.reg[1,2]/(tab.reg[1,2]+tab.reg[2,2])
waste.reg
lost.reg<-tab.reg[2,1]/(tab.reg[2,1]+tab.reg[2,2])
lost.reg
Loss.fn.reg<-0.6*(lost.reg)^2+0.4*(waste.reg)^2
Loss.fn.reg


######

#LPM
LPM<-lm(LPM,train.reg)
summary(LPM)
vif(LPM)
#Retain only the significant ones post VIF corection

LPM.1<-Winner~Incumbent.Party +Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

OLS.LPM1<-lm(LPM.1,train.reg)
summary(OLS.LPM1)
vif(OLS.LPM1)

#Further
LPM.2<-Winner~Incumbent.Party + Major+Female+  mean.age++mean.studied
OLS.LPM.2<-lm(LPM.2,train.reg)
summary(OLS.LPM.2)
Pred_LPM <- predict(OLS.LPM.2,val.reg)
#Confusionmatrix
tab.LPM<-table(val.reg$Winner, Pred_LPM > 0.25)
tab.LPM


accuracy.LPM<-sum(diag(tab.LPM))/sum(tab.LPM)
accuracy.LPM
waste.LPM<-tab.LPM[1,2]/(tab.LPM[1,2]+tab.LPM[2,2])
waste.LPM
lost.LPM<-tab.LPM[2,1]/(tab.LPM[2,1]+tab.LPM[2,2])
lost.LPM
Loss.fn.LPM<-0.6*(lost.LPM)^2+0.4*(waste.LPM)^2
Loss.fn.LPM


##############
#logit

#All Variables

logit.1 <- glm(logit   , train.reg, family = binomial)
summary(logit.1)
vif(logit.1)

#Remove insignificant variables post VIF
logit.2<-Winner~Incumbent.Party + Major+ Female+ mean.age++mean.studied
logit.2 <- glm(logit.2  , train.reg, family = binomial)
summary(logit.2)
vif(logit.2)


pred <- predict.glm(logit.2, newdata=val.reg, type="response")

tab.logit<-confusion.matrix(val.reg$Winner,pred,threshold = 0.24)
tab.logit
accuracy.logit<-sum(diag(tab.logit))/sum(tab.logit)
accuracy.logit
waste.logit<-tab.logit[2,1]/(tab.logit[2,1]+tab.logit[2,2])
waste.logit
lost.logit<-tab.logit[1,2]/(tab.logit[1,2]+tab.logit[2,2])
lost.logit
Loss.fn.logit<-0.6*(lost.logit)^2+0.4*(waste.logit)^2
Loss.fn.logit
#ROC
roc<-roc(val.reg$Winner,pred)
plot(roc)
auc(val.reg$Winner,pred)


#####################################

#Decision Tree and RFM
#Data frame
train.dt<-train[,c(4,6:9,11:13,15,19:22,24)]
val.dt<-val[,c(4,6:9,11:13,15,19:22,24)]


DT<-Status~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

RFM<-Status~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

###Now Decision Trees

DT<-rpart(DT, method="class",train.dt)
rpart.plot(DT)
rpart.plot(DT, type=3,extra=101,fallen.leaves = T)
summary(DT)


pred.DT= predict(DT, type = "class",val.dt)
tabDT<-table( val.dt$Status,pred.DT)
tabDT
accuracy.DT<-sum(diag(tabDT))/sum(tabDT)
accuracy.DT
waste.DT<-tabDT[1,2]/(tabDT[1,2]+tabDT[2,2])
waste.DT
lost.DT<-tabDT[2,1]/(tabDT[2,1]+tabDT[2,2])
lost.DT
Loss.fn.DT<-0.6*(lost.DT)^2+0.4*(waste.DT)^2
Loss.fn.DT
############NOW RANDOM FOREST


#Run RFM
rfm<-randomForest(RFM,train.dt)
rfm
predictrfm<-predict(rfm,val.dt)
tab.rfm<-table(val.dt$Status,predictrfm)
tab.rfm
accuracy.rfm<-sum(diag(tab.rfm))/sum(tab.rfm)
accuracy.rfm
waste.rfm<-tab.rfm[1,2]/(tab.rfm[1,2]+tab.rfm[2,2])
waste.rfm
lost.rfm<-tab.rfm[2,1]/(tab.rfm[2,1]+tab.rfm[2,2])
lost.rfm
Loss.fn.rfm<-0.6*(lost.rfm)^2+0.4*(waste.rfm)^2
Loss.fn.rfm

########### Linear Discriminant Analysis and SVM
#Data Frame

train.lda<-train[,c(1,4,6:9,11:13,15,19:22,24)]
val.lda<-val[,c(1,4,6:9,11:13,15,19:22,24)]

LDA<-Status~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied
SVM<-Status~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied


#Insample
lda<-lda(LDA,train.lda )
lda


#validation

lda_1<-lda(LDA,train.lda)
lda1.pred<-predict(lda_1, newdata=val.lda)
ldapredclass<-lda1.pred$class
tab.LDA<-table(ldapredclass,val.lda$Status)
print(tab.LDA)

accuracy.LDA<-sum(diag(tab.LDA))/sum(tab.LDA)
accuracy.LDA
waste.LDA<-tab.LDA[2,1]/(tab.LDA[2,1]+tab.LDA[2,2])
waste.LDA
lost.LDA<-tab.LDA[1,2]/(tab.LDA[1,2]+tab.LDA[2,2])
lost.LDA
Loss.fn.LDA<-0.6*(lost.LDA)^2+0.4*(waste.LDA)^2
Loss.fn.LDA
######
SVM.UP<-svm(SVM, data=train.lda, type='C-classification', kernel = 'linear')
SVM.UP
#pedict
y_pred.SVM<-predict(SVM.UP,newdata=val.lda)
y_pred.SVM
#Confusion matrix
tab.SVM=table(val.lda$Winner ,y_pred.SVM)
tab.SVM

accuracy.SVM<-sum(diag(tab.SVM))/sum(tab.SVM)
accuracy.SVM
waste.SVM<-tab.SVM[1,2]/(tab.SVM[1,2]+tab.SVM[2,2])
waste.SVM
lost.SVM<-tab.SVM[2,1]/(tab.SVM[2,1]+tab.SVM[2,2])
lost.SVM
Loss.fn.SVM<-0.6*(lost.SVM)^2+0.4*(waste.SVM)^2
Loss.fn.SVM
#############
#Neural Network

#Data frame
train.ann<-train[,c(4,6:9,12:13,19:22,11)]
val.ann<-val[,c(4,6:9,12:13,19:22,11)]

#######
ANN<-Winner~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

str(train.ann)
ANN.UP <- neuralnet(ANN,
                    data = train.ann,
                    hidden = 2,
                    err.fct = "ce",
                    linear.output = FALSE)
plot(ANN.UP)



# Confusion Matrix & Misclassification Error - training data

output.ANN <- compute(ANN.UP, val.ann[,-12])
p1 <- output.ANN$net.result
pred1 <- ifelse(p1>0.25, 1, 0)
tab.ANN <- table(pred1, val$Winner)
tab.ANN
print(tab.ANN)

accuracy.ANN<-sum(diag(tab.ANN))/sum(tab.ANN)
accuracy.ANN
waste.ANN<-tab.ANN[2,1]/(tab.ANN[2,1]+tab.ANN[2,2])
waste.ANN
lost.ANN<-tab.ANN[1,2]/(tab.ANN[1,2]+tab.ANN[2,2])
lost.ANN
Loss.fn.ANN<-0.6*(lost.ANN)^2+0.4*(waste.ANN)^2
Loss.fn.ANN
#### So what to do?

#######
#K FOLD VALIDATIONS



set.seed(123)
folds<-createFolds(train$Winner,k=10)
str(folds)
######
#10 Fold Validation with Linear regression

Linear.kval<-Vote.Share~Incumbent.Party +Crorepati+  Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied



cv_OLS<-lapply(folds,function(x){
  train.ols.kval<-train.reg[x,]
  test.ols.kval<-val.reg[-x,]
  OLS.kval<-lm(Linear.kval   , train.ols.kval)
  test.ols.kval$Pred_OLS.kval <- predict(OLS.kval, test.ols.kval)
  
  Pred.win.kval<-ddply(test.ols.kval, .(AC_NAME), transform, 
                       max.share=max(Pred_OLS.kval))
  Pred.win.kval$win<-ifelse(Pred.win.kval$Pred_OLS.kval==Pred.win.kval$max.share,1,0)
  tab.OLS.kval<-table(Pred.win.kval$Winner, Pred.win.kval$win > 0.5)
  sum(diag(tab.OLS.kval))/sum(tab.OLS.kval)
})

str(cv_OLS)
fit.OLS<-mean(unlist(cv_OLS))
fit.OLS
##########
#10 Fold validation with LPM

LPM.2<-Winner~Incumbent.Party + Major+Female+  mean.age++mean.studied

cv_LPM<-lapply(folds,function(x){
  train.lpm.kval<-train.reg[x,]
  test.lpm.kval<-val.reg[-x,]
  LPM.kval<-lm(LPM.2, train.lpm.kval)
  LPM.kval.pred<-predict(LPM.kval, test.lpm.kval)
  tab.LPM.kval<-table(test.lpm.kval$Winner, LPM.kval.pred>0.5)
  sum(diag(tab.LPM.kval))/sum(tab.LPM.kval)
})

str(cv_LPM)
fit.LPM<-mean(unlist(cv_LPM))
fit.LPM
##########
#10 Fold Validation with LDA

rfm.2<-Winner~Incumbent.Party + Major+ Female+ mean.age++mean.studied

cv_rfm<-lapply(folds,function(x){
  train.rfm.kval<-train.reg[x,]
  test.rfm.kval<-val.reg[-x,]
  rfm.kval<-glm(rfm.2   , train.rfm.kval, family = binomial)
  pred.rfm.kval <- predict.glm(rfm.kval, newdata=test.rfm.kval, type="response")
  tab.rfm.kval<-confusion.matrix(test.rfm.kval$Winner,pred.rfm.kval,threshold = 0.5)
  sum(diag(tab.rfm.kval))/sum(tab.rfm.kval)
})


str(cv_rfm)
fit.rfm.kval<-mean(unlist(cv_rfm))
fit.rfm.kval
####




#10 Vold Validation with KNN

normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
library(class)
#Knn frames
cv_KNN.5<-lapply(folds,function(x){
  train.NB.kval<-train.NB[x,]
  test.NB.kval<-val.NB[-x,]
  train.knn.kval<-as.data.frame(lapply(train.NB.kval[,c(1:8)],normalize))
  test.knn.kval<-as.data.frame(lapply(test.NB.kval[,c(1:8)],normalize))
  train_target.kval<-train.NB.kval[,9]
  test_target.kval<-test.NB.kval[,9]
  knn5.kval<-knn(train=train.knn.kval,test=test.knn.kval,cl=train_target.kval,k=5)
  tab.knn.5.kval<-table(test_target.kval,knn5.kval)
  sum(diag(tab.knn.5.kval))/sum(tab.knn.5.kval)
})
str(cv_KNN.5)
fit.KNN<-mean(unlist(cv_KNN.5))
fit.KNN
#####################
#10 Vold Validation with NB

cv_NB<-lapply(folds,function(x){
  train.NB.kval<-train.NB[x,]
  test.NB.kval<-val.NB[-x,]
  
  NB.kval<-naiveBayes(x=train.NB.kval[-9], y=train.NB.kval$Status)
  y_pred.NB.kval<-predict(NB,newdata=test.NB.kval[-9])
  cm.NB.kval=table(test.NB.kval[,9],y_pred.NB.kval)
  sum(diag(cm.NB.kval))/sum(cm.NB.kval)
})

str(cv_NB)
fit.NB<-mean(unlist(cv_NB))
fit.NB

#####################

#10 Fold with LDA and SVM

LDA<-Status~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

cv_LDA<-lapply(folds,function(x){
  train.lda.kval<-train.lda[x,]
  test.lda.kval<-val.lda[-x,]
  lda.kval<-lda(LDA   , train.lda.kval)
  lda.kval.pred<-predict(lda.kval, newdata=test.lda.kval)
  ldapredclass.kval<-lda.kval.pred$class
  tab.LDA.kval<-table(ldapredclass.kval,test.lda.kval$Winner)
  sum(diag(tab.LDA.kval))/sum(tab.LDA.kval)
})

str(cv_LDA)
fit.LDA<-mean(unlist(cv_LDA))
fit.LDA

#SVM  
SVM<-Status~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

cv_SVM<-lapply(folds,function(x){
  train.svm.kval<-train.lda[x,]
  test.svm.kval<-val.lda[-x,]
  svm.kval<-svm(SVM,    train.svm.kval, type='C-classification', kernel = 'linear')
  svm.kval.pred<-predict(svm.kval, newdata=test.svm.kval)
  cm.SVM.kval=table(test.svm.kval$Status ,svm.kval.pred)
  
  sum(diag(cm.SVM.kval))/sum(cm.SVM.kval)
})

str(cv_SVM)
fit.SVM<-mean(unlist(cv_SVM))
fit.SVM



#########
#10 Fold on Decision Trees and RFM
DT<-Winner~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

RFM<-Winner~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied


cv_DT<-lapply(folds,function(x){
  train.DT.kval<-train.dt[x,]
  test.DT.kval<-val.dt[-x,]
  DT.kval<-rpart(DT, method="class",train.DT.kval)
  pred.DT.kval = predict(DT.kval, type="class",newdata=test.DT.kval)
  tab.DT.kval<-table( pred.DT.kval,test.DT.kval$Winner)
  sum(diag(tab.DT.kval))/sum(tab.DT.kval)
})

str(cv_DT)
fit.DT<-mean(unlist(cv_DT))
fit.DT
####gain
set.seed(123)
folds.1<-createFolds(train$Winner,k=5)
str(folds.1)


cv_DT.gain<-lapply(folds.1,function(x){
  train.DT.kval<-train.dt[x,]
  test.DT.kval<-val.dt[-x,]
  DT.kval<-rpart(DT, method="class",train.DT.kval)
  pred.DT.kval = predict(DT.kval, type="class",newdata=test.DT.kval)
  tab.DT.kval<-table( pred.DT.kval,test.DT.kval$Winner)
  
  accuracy.DT.kval<-sum(diag(tab.DT.kval))/sum(tab.DT.kval)
  accuracy.DT.kval
  waste.DT.kval<-tab.DT.kval[1,2]/(tab.DT.kval[1,2]+tab.DT.kval[2,2])
  waste.DT.kval
  lost.DT.kval<-tab.DT.kval[2,1]/(tab.DT.kval[2,1]+tab.DT.kval[2,2])
  lost.DT.kval
  Loss.fn.DT.kval<-0.6*(lost.DT.kval)^2+0.4*(waste.DT.kval)^2
  gain.DT.kval<-Loss.fn.DT.kval
  
})
str(cv_DT.gain)
fit.DT.gain<-mean(unlist(cv_DT.gain))
fit.DT.gain

########
#xgboost
library(xgboost)
train.xg<-train.dt[,c(1:8,10:13)]
val.xg<-val.dt[,c(1:8,10:13)]

xg.b<-xgboost(data=as.matrix(train.xg[-6]), label=train.xg$Winner, nrounds=10)
xg.b
cv_xg<-lapply(folds,function(x){
  train.xg.kval<-train.xg[x,]
  test.xg.kval<-val.xg[-x,]
  #xg.kval<-xgboost(data=as.matrix(train.xg.kval[,-6]),label=train.xg$Winner,nrounds = 10)
  pred.xg.kval = predict(xg.b, newdata=as.matrix(test.xg.kval[,-6]))
  pred.xg.kval=(pred.xg.kval>=0.5)
  tab.xg.kval<-table( pred.xg.kval,test.xg.kval$Winner)
  #sum(diag(tab.xg.kval))/sum(tab.xg.kval)
  (tab.xg.kval[1,1]+tab.xg.kval[2,2])/(tab.xg.kval[1,1] +
                                         tab.xg.kval[2,2]+tab.xg.kval[1,2]+tab.xg.kval[2,1])
})

str(cv_xg)
fit.xg<-mean(unlist(cv_xg))
fit.xg




#10-Fold validation RFM


cv_RFM<-lapply(folds,function(x){
  train.RFM.kval<-train.dt[x,]
  test.RFM.kval<-val.dt[-x,]
  rfm.kval<-randomForest(RFM,train.RFM.kval)
  predict.rfm.kval<-predict(rfm.kval,test.RFM.kval)
  tab.RFM.kval<-table( test.RFM.kval$Winner,predict.rfm.kval)
  sum(diag(tab.RFM.kval))/sum(tab.RFM.kval)
})

str(cv_RFM)
fit.RFM<-mean(unlist(cv_RFM))
fit.RFM

##########
#10 Fold Validation ANN
ANN<-Winner~Incumbent.Party +Crime++Crorepati+ Major+ Grad+ Serious+Female+ Asset.Rank+mean.assets+ mean.age++mean.studied

cv_ANN<-lapply(folds,function(x){
  train.ANN.kval<-train.ann[x,]
  test.ANN.kval<-val.ann[-x,]
  ann.kval<-neuralnet(ANN,
                      data = train.ANN.kval,
                      hidden = 2,
                      err.fct = "ce",
                      linear.output = FALSE)
  output <- compute(ann.kval, test.ANN.kval[,-12])
  p1 <- output$net.result
  pred1 <- ifelse(p1>0.25, 1, 0)
  tab.ANN.kval <- table(pred1, test.ANN.kval$Winner)
  
  sum(diag(tab.ANN.kval))/sum(tab.ANN.kval)
})

str(cv_ANN)
fit.ANN<-mean(unlist(cv_ANN))
fit.ANN

#####
#XGBoost

train.xg<-train.dt[,c(1:8,10:13)]
val.xg<-val.dt[,c(1:8,10:13)]

#xgboost.iris<-xgboost(data=as.matrix(train.iris.DT[-3]), 
#label=train.iris.DT$Spec.1,nrounds = 10)
xg.b<-xgboost(data=as.matrix(train.xg[-6]), label=train.xg$Winner, nrounds=10)
xg.b
cv_xg<-lapply(folds,function(x){
  train.xg.kval<-train.xg[x,]
  test.xg.kval<-val.xg[-x,]
  #xg.kval<-xgboost(data=as.matrix(train.xg.kval[,-6]),label=train.xg$Winner,nrounds = 10)
  pred.xg.kval = predict(xg.b, newdata=as.matrix(test.xg.kval[,-6]))
  pred.xg.kval=(pred.xg.kval>=0.5)
  tab.xg.kval<-table( pred.xg.kval,test.xg.kval$Winner)
  #sum(diag(tab.xg.kval))/sum(tab.xg.kval)
  (tab.xg.kval[1,1]+tab.xg.kval[2,2])/(tab.xg.kval[1,1] +
                                         tab.xg.kval[2,2]+tab.xg.kval[1,2]+tab.xg.kval[2,1])
})

str(cv_xg)
fit.xg<-mean(unlist(cv_xg))
fit.xg
#xgb with gain
cv_xg.gain<-lapply(folds.1,function(x){
  train.xg.kval<-train.xg[x,]
  test.xg.kval<-val.xg[-x,]
  #xg.kval<-xgboost(data=as.matrix(train.xg.kval[,-6]),label=train.xg$Winner,nrounds = 10)
  pred.xg.kval = predict(xg.b, newdata=as.matrix(test.xg.kval[,-6]))
  pred.xg.kval=(pred.xg.kval>=0.5)
  tab.xg.kval<-table( pred.xg.kval,test.xg.kval$Winner)
  #sum(diag(tab.xg.kval))/sum(tab.xg.kval)
  
  tab.xg.kval
  accuracy..xg.kval<-sum(diag(tab.xg.kval))/sum(tab.xg.kval)
  accuracy..xg.kval
  waste..xg.kval<-tab.xg.kval[1,2]/(tab.xg.kval[1,2]+tab.xg.kval[2,2])
  waste..xg.kval
  lost..xg.kval<-tab.xg.kval[2,1]/(tab.xg.kval[2,1]+tab.xg.kval[2,2])
  lost..xg.kval
  Loss.fn..xg.kval<-0.6*(lost..xg.kval)^2+0.4*(waste..xg.kval)^2
  Gain.xg.kval<-Loss.fn..xg.kval
  
  
  
})

str(cv_xg.gain)
fit.xg.gain<-mean(unlist(cv_xg.gain))
fit.xg.gain

#################
###Variable Importance: Through Shuffling
######
outcomeName <- 'Winner'
predictorNames <- setdiff(names(train.xg),outcomeName)

# transform outcome variable to text as this is required in caret for classification 
train.xg[,outcomeName] <- ifelse(train.xg[,outcomeName]==1,'Win','others')

# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=3, returnResamp='none', 
                           summaryFunction = twoClassSummary, classProbs = TRUE)

# shuffling with GBM
objGBM <- train(train.xg[,predictorNames],  as.factor(train.xg[,outcomeName]),
                method='gbm',
                trControl=objControl,
                metric = "ROC"
                #tuneGrid = expand.grid(n.trees = 3, interaction.depth = 3, shrinkage = 0.1)
)

predictions <- predict(object=objGBM, val.xg[,predictorNames], type='prob')
predictions
GetROC_AUC = function(probs, true_Y){
  # AUC approximation
  # http://stackoverflow.com/questions/4903092/calculate-auc-in-r
  # ty AGS
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix) 
  
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)   
  
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(auc)
}

refAUC <- GetROC_AUC(predictions[[2]],val.xg[,outcomeName])
print(paste('AUC score:', refAUC))

# Shuffle predictions for variable importance
AUCShuffle <- NULL
shuffletimes <- 500

featuresMeanAUCs <- c()
for (feature in predictorNames) {
  featureAUCs <- c()
  shuffledData <- val.xg[,predictorNames]
  for (iter in 1:shuffletimes) {
    shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
    predictions <- predict(object=objGBM, shuffledData[,predictorNames], type='prob')
    featureAUCs <- c(featureAUCs,GetROC_AUC(predictions[[2]], val.xg[,outcomeName]))
  }
  featuresMeanAUCs <- c(featuresMeanAUCs, mean(featureAUCs < refAUC))
}
AUCShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanAUCs)
AUCShuffle <- AUCShuffle[order(AUCShuffle$importance, decreasing=TRUE),]
print(AUCShuffle)
head(featureAUCs)
feature

# shuffling with GLM
# change a few things for a linear model
objControl <- trainControl(method='cv', number=2)
train.xg[,outcomeName] <- ifelse(train.xg[,outcomeName]=='Win', 1, 0)

# GLM
objGLM <- train(train.xg[,predictorNames],  train.xg[,outcomeName],
                method='glm',
                trControl=objControl,
                preProc = c("center", "scale"))

predictions <- predict(object=objGLM, val.xg[,predictorNames])
refRMSE=sqrt((sum((val.xg[,outcomeName]-predictions[[2]])^2))/nrow(val.xg))

VariableImportanceShuffle <- NULL

print(paste('Reference RMSE:',refRMSE))

shuffletimes <- 500
featuresMeanRMSEs <- c()
for (feature in predictorNames) {
  featureRMSEs <- c()
  shuffledData <- val.xg[,predictorNames]
  for (iter in 1:shuffletimes) {
    shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
    predictions <- predict(object=objGLM, shuffledData[,predictorNames])
    featureRMSEs <- c(featureRMSEs, sqrt((sum((val.xg[,outcomeName]-predictions[[2]])^2))/nrow(val.xg)))
  }
  featuresMeanRMSEs <- c(featuresMeanRMSEs,  mean((featureRMSEs - refRMSE)/refRMSE))
}
VariableImportanceShuffle <- data.frame('feature'=predictorNames, 'RMSE_Importance'=featuresMeanRMSEs)
VariableImportanceShuffle <- 
  VariableImportanceShuffle[order(VariableImportanceShuffle$RMSE_Importance,decreasing=TRUE),]
print(VariableImportanceShuffle)



#Actionable Insights

set.seed(1234)
random_splits <- runif(nrow(train.xg))
train_df <- train.xg[random_splits < .5,]
dim(train_df)


test_df <- train.xg[random_splits >= .5,]
dim(test_df)

outcome_name <- outcomeName


#To simplify things and maybe make this more useful for your advanced-analytics pipeline, we???ll build our prediction insight program into a function. The modeling engine will use xgboost: Extreme Gradient Boosting because it is easy to use and fast. You will need to install xgboost and dplyr if you don???t already have them (both available on cran).

observation_level_variable_importance <- function(train_data, live_data, outcome_name, eta = 0.2, 
                                                  max_depth=4, max_rounds=3000, number_of_factors=2) {
  
  # install.packages('dplyr')
  require(dplyr)
  # install.packages('xgboost')
  require(xgboost)
  
  set.seed(1234)
  split <- sample(nrow(train_data), floor(0.9*nrow(train_data)))
  train_data_tmp <- train_data[split,]
  val_data_tmp <- train_data[-split,]
  
  feature_names <- setdiff(names(train_data_tmp), outcome_name)
  dtrain <- xgb.DMatrix(data.matrix(train_data_tmp[,feature_names]), 
                        label=train_data_tmp[,outcome_name], missing=NaN)
  dval <- xgb.DMatrix(data.matrix(val_data_tmp[,feature_names]), 
                      label=val_data_tmp[,outcome_name], missing=NaN)
  watchlist <- list(eval = dval, train = dtrain)
  param <- list(  objective = "binary:logistic",
                  eta = eta,
                  max_depth = max_depth,
                  subsample= 0.9,
                  colsample_bytree= 0.9
  )
  
  xgb_model <- xgb.train ( params = param,
                           data = dtrain,
                           eval_metric = "auc",
                           nrounds = max_rounds,
                           missing=NaN,
                           verbose = 1,
                           print.every.n = 10,
                           early.stop.round = 20,
                           watchlist = watchlist,
                           maximize = TRUE)
  
  original_predictions <- predict(xgb_model, 
                                  data.matrix(live_data[,feature_names]), 
                                  outputmargin=FALSE, missing=NaN)
  
  # strongest factors
  new_preds <- c()
  for (feature in feature_names) {
    print(feature)
    live_data_trsf <- live_data
    # neutralize feature to population mean
    if (sum(is.na(train_data[,feature])) > (nrow(train_data)/2)) {
      live_data_trsf[,feature] <- NA
    } else {
      live_data_trsf[,feature] <- mean(train_data[,feature], na.rm = TRUE)
    }
    predictions <- predict(object=xgb_model, data.matrix(live_data_trsf[,feature_names]),
                           outputmargin=FALSE, missing=NaN)
    new_preds <- cbind(new_preds, original_predictions - predictions)
  }
  
  positive_features <- c()
  negative_features <- c()
  
  feature_effect_df <- data.frame(new_preds)
  names(feature_effect_df) <- c(feature_names)
  
  for (pred_id in seq(nrow(feature_effect_df))) {
    vector_vals <- feature_effect_df[pred_id,]
    vector_vals <- vector_vals[,!is.na(vector_vals)]
    positive_features <- rbind(positive_features, 
                               c(colnames(vector_vals)[order(vector_vals, 
                                                             decreasing=TRUE)][1:number_of_factors]))
    negative_features <- rbind(negative_features, 
                               c(colnames(vector_vals)[order(vector_vals, 
                                                             decreasing=FALSE)][1:number_of_factors]))
  }
  
  positive_features <- data.frame(positive_features)
  names(positive_features) <- paste0('Pos_', names(positive_features))
  negative_features <- data.frame(negative_features)
  names(negative_features) <- paste0('Neg_', names(negative_features))
  
  return(data.frame(original_predictions, positive_features, negative_features))
  
} 


#The first half of the function is straight-forward xgboost classification (see XGBoost R Tutorial) and we get a vector of predictions for our test/live data. It is in the second half that things get more interesting - after the model has trained on the training data split and predicted on the testing split, we are left with the prediction vector - dubbed original predictions. It is a long series of probabilities, one for each observation. The model used here isn???t important and the above should work with most models.

#We then run through each feature a second time we reset the feature to the population mean. We feed that the new data set with its feature neutralized into the prediction function and compare the original prediction vector against this new one. Any time the prediction for an observation changes, we conclude it is important for that observation. We also record whether the original feature has a positive or negative influence on that prediction.

# get variable importance 

preds <- observation_level_variable_importance(train_data = train_df, 
                                               live_data = test_df, 
                                               outcome_name = outcome_name)
head(preds)
tail(preds)

preds <- preds[order(preds$original_predictions),]
head(preds)

tail(preds)



#To confirm this,
objControl <- trainControl(method='cv', number=3, returnResamp='none')

glm_caret_model <- train(train_df[,setdiff(names(train_df),outcome_name)], 
                         as.factor(ifelse(train_df[,outcome_name]==1,'yes','no')), 
                         method='glm', 
                         trControl=objControl,
                         preProc = c("center", "scale"))

summary(glm_caret_model)
