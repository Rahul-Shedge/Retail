library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)
library('caret')
library(gbm)



s_train=read.csv('store_train.csv',stringsAsFactors = F)
s_test=read.csv('store_test.csv',stringsAsFactors = F)


setdiff(names(s_train),names(s_test))
#store
s_test$store = NA

length(is.na(s_train$store))
s_test
s_test$data='test'
s_train$data='train'




dim(s_test)


s_all=rbind(s_train,s_test)


View(s_all)
library(dplyr)

glimpse(s_all)

table(s_all$CouSub)



CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
max(table(s_all$country))
s_all$sales1
glimpse(s_all) 
s_all[is.na(s_all$population),'population'] <- mean(s_train$population,na.rm = T)
glimpse(s_all)
s_all[is.na(s_all$country),'country'] <- 3
glimpse(s_all)
table(s_all$storecode)

#s_all$store=as.factor(s_all$store)
sapply(s_all,function(x)sum(is.na(x)))
r=table(s_all$CouSub)
s_all=CreateDummies(s_all,'countytownname',80)
s_all=CreateDummies(s_all,'CouSub',1000)


s_all=CreateDummies(s_all,'State',80)


s_all=CreateDummies(s_all,'store_Type',400)

table(s_all$storecode)
s_all=CreateDummies(s_all,'storecode',25)
glimpse(s_all)
#for_dummy_vars=c('country','State','store_Type','CouSub')

#for(var in for_dummy_vars){
#  s_all=CreateDummies(s_all,var,50)
#}
glimpse(s_all)
sum(is.na(s_all))

s_all[is.na(s_all$population),'population'] <- mean(s_train$population,na.rm = T)
glimpse(s_all)
table(s_all$country)

glimpse(s_all)

table(s_all$state_alpha)

s_all=s_all %>% select(-sales1,-sales2,-sales3,-sales4,-countyname,
                       -Areaname,-country,-Id,-state_alpha)

s_all=s_all %>% select(-state_alpha)

glimpse(s_train1)



s_train= s_all %>% filter(data == 'train') %>% select(-data)
s_test= s_all %>% filter(data == 'test') %>% select(-data,-store)
 




set.seed(2)
s=sample(1:nrow(s_train),0.7*nrow(s_train))
s_train1=s_train[s,]
s_train2=s_train[-s,]
s_train2

#----------------------------------------------------------------------------------------------------


















View(s_train)
table(s_train$CouSub)

library('car')
vif_train1=lm(store~.,data=s_train1)

glimpse(s_train1)



form <-
  paste0('store ~', paste0(setdiff(names(s_train1),  'store'
  ), collapse = ' + '))

form <- as.formula(form)


sort(vif(vif_train1),decreasing = T)
k=100000000
to_remove=c('store')

while(k>4){
  
  vif_train1=lm(form,data=s_train1)
  
   k= sort(vif(vif_train1),decreasing = T)[1]
   if(k>4){
   remove=names(k)
   print(k)
   to_remove=c(remove,to_remove)

       
   form <-
   as.formula(paste0('store ~', paste0(setdiff(names(s_train1),  c('store',to_remove)
   ), collapse = ' + ')))
   }
   
}

glimpse(s_train)
s_train1$store=as.factor(s_train1$store)
fit_log=glm(form,data=s_train1,family = 'binomial')
fit_log=step(fit_log)
fit_log$coefficients


form <- as.formula(paste0("store ~" ,
                          paste0(setdiff(names(fit_log$coefficients),
                                         c('store','(Intercept)')) , collapse="+")))
fit_log = glm(form,data=s_train2,family = 'binomial')



## predicting the response
score.train=predict(fit_log,s_train2,type = "response")
real=s_train2$store


cutoffs=round(seq(0,1,length=100),3)
cutoff_data=data.frame(cutoff=999,KS=999)

for(cutoff in cutoffs){
  pred=as.numeric(score.train>cutoff)
  TP=sum(real==pred & real==1)
  TN=sum(real==pred & real==0)
  FP=sum(real!=pred & real==0)
  FN=sum(real!=pred & real==1)
  P=TP+FN
  N=TN+FP
  KS=(TP/P)-(FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}

cutoff_data=cutoff_data[-1,]
KS.cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]


# performance of this logistic model on the test data
## test performance
s_train2$score=predict(fit_log,newdata=s_train2,type="response")
s_train2$predicted=as.numeric(s_train2$score>KS.cutoff)

## train performance
s_train1$score=predict(fit_log,newdata=s_train1,type="response")
s_train1$predicted=as.numeric(s_train1$score>KS.cutoff)



table(s_train2$store,s_train2$predicted)



caTools::colAUC(s_train2$predicted, s_train2$store, plotROC = TRUE)
caTools::colAUC(s_train1$predicted, s_train1$store, plotROC = TRUE)

test_log_prob = predict(fit_log,newdata = s_test,type = 'response')

test.predicted=as.numeric(test_log_prob>KS.cutoff)
write.csv(test.predicted,"Mysubmission.csv" , row.names = F)
table(test.predicted)

s.tree=tree(form , s_train1)

## Performance on validation set

val.score=predict(s.tree,newdata = s_train2,type='vector')[,2]
pROC::roc(s_train2$store,val.score)$auc

train1.score=predict(s.tree,newdata = s_train1,type='vector')[,2]

pROC::roc(s_train1$store,train1.score)$auc



s_train$store=as.factor(s_train$store)

s.tree.final=tree(form,s_train)

train.score=predict(s.tree,newdata = s_train,type='vector')[,1]

pROC::roc(s_train$store,train.score)$auc


dim(s_train$store)
test.score=predict(s.tree.final,newdata=s_test,type='vector')[,1]
write.csv(test.score,"mysubmission.csv",row.names = F)





param=list(mtry=c(2,4,5,6),
           ntree=c(50,100,200,500,1000,2000,5000),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
#id search to create randomized grid search
subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,store~., 
             data =s_train1,
             tuning =params,
             folds = cvFolds(nrow(s_train1), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

best_params


#best_params
     #mtry ntree maxnodes nodesize
#817    5   100      100       10
#> myauc
#[1] 0.7882889

#best_params
#     m try ntree maxnodes nodesize
#631    5  1500      200       10
#>     print(myauc)
#[1] 0.791025

s.rf.final=randomForest(form,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=s_train
)

s.rf.final
#Call:
 # randomForest(formula = form, data = s_train, mtry = best_params$mtry,      ntree = best_params$ntree, maxnodes = best_params$maxnodes,      nodesize = best_params$nodesize) 
#Type of random forest: classification
#Number of trees: 100
#No. of variables tried at each split: 5

#OOB estimate of  error rate: 24.63%
#Confusion matrix:
 # 0   1 class.error
#0 1562 313   0.1669333
#1  509 954   0.3479152

test.score=predict(s.rf.final,newdata = s_test,type='prob')[,2]
write.csv(test.score,'2nd_proj.rf.csv',row.names = F)     




#gbm:------------------------------------------------------------------------------

param=list(interaction.depth=c(1:12),
           n.trees=c(500,900,1000,5000,2500,3000,5000,6000,7000),
           shrinkage=c(.001,0.009,0.007,0.003,0.005,0.05,0.01),
           n.minobsinnode=c(1,2,5,10,7,8))



subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}


num_trials=100
my_params=subset_paras(param,num_trials)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 10

## ----this code will take too long to run--------------------------------------------------------------
myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(gbm,store~.,
             data =s_train1,
             tuning =params,
             args=list(distribution="bernoulli"),
             folds = cvFolds(nrow(s_train1), K=10, type ="random"),
             cost =mycost_auc, seed = 5,
             predictArgs = list(type="response",n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
     print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  # print('DONE')
  # uncomment the line above to keep track of progress
}

#[1] 0.8042372
#> 
 ## ------------------------------------------------------------------------
#> best_params
#interaction.depth n.trees shrinkage n.minobsinnode
#1998     6         2500     0.005              5



    #-------------------------------
myauc

## ------------------------------------------------------------------------
best_params

## ------------------------------------------------------------------------
ci.gbm.final=gbm(store~.,data=s_train,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsinnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "bernoulli")




test.score=predict(ci.gbm.final,newdata=s_test,type='response',
                         n.trees = best_params$n.trees)
write.csv(test.score,"Rahul_Shedge_P2_part2.csv",row.names=F)
                   

#----------------------------------------TRYING USING CARET LIB---------------------
install.packages('ranger')
library(caret);library(ranger)
s_train1$store = as.factor(s_train1$store)
s_train2$store = as.factor(s_train2$store)
rf.grid <- expand.grid(mtry=c(7,8),
                       splitrule=c('gini','extratree'),min.node.size=c(5,10))

fitControl <- trainControl(method = 'cv',
                           #repeats = 5,
                           number = 3,
                           verboseIter = TRUE  )

levels(s_train1$store) <- make.names(levels(s_train1$store))
levels(s_train2$store) <- make.names(levels(s_train2$store))
rf_fit <-  caret::train(store~.,data = s_train1,
                              method = 'ranger',
                              num.trees =1000,
                               trControl = rf.grid,
                                  metric = 'ROC')

caTools::colAUC(Predict(rf_fit,newdata= s_train1,type='prob')[,'X1'],s_trian1$store,plotROC = T)




#-----------------------------------------------------------------------------------------------


library(caret)


ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 1, 
                     search = "grid",
                     verboseIter = TRUE,
                     sampling = "up")


set.seed(42)

#tunegrid <- expand.grid(mtry=c(5,10,15,25),ntree=c(100,500,700,1000,2000,5000),
#                    maxnodes=c(5,10,15,20,30,50),
#                     nodesize=c(1,2,5,10))



model_rf_over <- caret::train(factor(store)~ .,
                              data = s_train1,
                              method = "rf",
                              preProcess = c("scale"),
                              trControl = ctrl,
                              tuneGrid=expand.grid(mtry=c(7,8,10,15,20)))


test_score1=predict(model_rf_over,newdata = s_train2,type = 'prob')
prdtns=as.vector(test_score1[2])
library(ROCR)

pred=prediction(prdtns,s_train2$store)
performance(pred,'auc')




p1=predict(model_rf_over,newdata = s_test,type = 'prob')[2]
write.csv(p1,'Rahul_Shedge_P2_part2.csv',row.names = F)



#An object of class "performance"
#Slot "x.name":
#  [1] "None"

#Slot "y.name":
 # [1] "Area under the ROC curve"

#Slot "alpha.name":
 # [1] "none"

#Slot "x.values":
 # list()

#Slot "y.values":
 # [[1]]
#[1] 0.8143568



