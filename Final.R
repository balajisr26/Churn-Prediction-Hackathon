gc()
rm(testds)
rm(trainds)
rm(train_test)
rm(train)
rm(test)
library(data.table)
library(InformationValue)
trainds<-fread('train.csv')
testds<-fread('test.csv')

summary(trainds$EOP_prev1)
summary(testds$EOP_prev1)


trainds[is.na(trainds)]<-0
testds[is.na(testds)]<-0

trainds$ind<-1
testds$ind<-2



table(trainds$Responders)

train_test<-rbind(trainds,testds,fill=T)


for (f in names(train_test)) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- sort(unique(train_test[[f]]))
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}

#train_test$EOP_prev1_avg<-(train_test$EOP_prev1+train_test$EOP_prev2
#                          +train_test$EOP_prev3+train_test$EOP_prev4
#                          +train_test$EOP_prev5+train_test$EOP_prev6)/6

#train_test$EOP_prev1<-ifelse(train_test$EOP_prev1>1000000,1000000,train_test$EOP_prev1)

train_test$eop_int1<-train_test$EOP_prev1/train_test$EOP_prev6
train_test$eop_int2<-train_test$EOP_prev1/train_test$EOP_prev2
train_test$eop_int4<-train_test$EOP_prev1/train_test$I_AQB_PrevQ1
train_test$eop_int5<-train_test$EOP_prev1/train_test$I_CR_AQB_PrevQ1
train_test$eop_int7<-train_test$EOP_prev1/train_test$CR_AMB_Prev2
train_test$eop_int10<-train_test$EOP_prev1/train_test$CR_AMB_Drop_Build_1


train_test$BAL_prev_int1<-train_test$BAL_prev3/train_test$BAL_prev1

train_test$BAL_prev_int2<-train_test$BAL_prev3/train_test$BAL_prev6

train_test$CR_AMB_Drop_Build_int2<-train_test$CR_AMB_Drop_Build_1+train_test$CR_AMB_Drop_Build_2

train_test[,":="(meanval=rowMeans(.SD)
                 ,maxval=apply(.SD,1,max,na.rm=T)
                 ,minval=apply(.SD,1,min,na.rm=T))
           ,.SDcols=c("EOP_prev1","EOP_prev2","EOP_prev3"
                      ,"EOP_prev4","EOP_prev5","EOP_prev6")]


train_test[,":="(I_CR_AQB_meanval=rowMeans(.SD)
                 ,I_CR_AQB_maxval=apply(.SD,1,max,na.rm=T)
                 ,I_CR_AQB_minval=apply(.SD,1,min,na.rm=T))
           ,.SDcols=c("I_CR_AQB_PrevQ1","I_CR_AQB_PrevQ2")]

#####################################################################

train<-subset(train_test,ind==1)
test<-subset(train_test,ind==2)


features<-setdiff(names(train_test),c('UCIC_ID','Responders','ind'
                                      ,'EOP_prev1_avg'))



lift<-function(preds,dtrain)
{
  labels<-getinfo(dtrain,"label")
  ks<-ks_stat(labels,preds,TRUE)
  list(metric="lift",value=ks$cum_perc_responders[2])
}


library(xgboost)
dtrainmat = xgb.DMatrix(as.matrix(train[,features,with=FALSE]), label=train$Responders)
dtestmat = xgb.DMatrix(as.matrix(test[,features,with=FALSE]))

xgb_params = list(
eta = 0.1,
objective = 'binary:logistic',
eval_metric='ndcg',
min_child_weight=10,
max_depth=6
)

pred_total<-0
seed_val<-sample(1:1000,15)       

for(seed in seed_val)
{

gbdt = xgb.train(xgb_params, dtrainmat, nrounds =145,verbose= 0,nthread=4
                 ,tree_method="hist")

pred<-predict(gbdt,dtestmat)

pred_total<-pred_total+pred
}
df<-data.frame(UCIC_ID=testds$UCIC_ID,Responders=pred_total/15)
write.csv(df,'4_8.csv',row.names = F)



