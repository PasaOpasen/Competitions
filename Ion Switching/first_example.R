
#example from https://www.kaggle.com/jpbeconne/beginner-notebook

library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(MLmetrics)
library(doSNOW)


f1 <- function (data, lev = NULL,model=NULL) {
  cm<-as.matrix(table(actual=data$obs,predicted=data$pred))
  diag<-diag(cm)
  rowsums<-apply(cm,1,sum)
  colsums<-apply(cm,2,sum)
  precision <- ifelse(colsums==0,0,diag/colsums)
  recall  <- ifelse(rowsums==0,0,diag/rowsums)
  f1<- ifelse(precision+recall==0,0,2*precision*recall/(precision+recall))
  f1_val <- mean(f1)
  names(f1_val) <- c("F1")
  f1_val
}




data.path='D:/liverpool-ion-switching/'

train.path=paste0(data.path,'train','.csv')
test.path=paste0(data.path,'test','.csv')
sample.path=paste0(data.path,'sample_submission','.csv')


train_data<-read_csv(train.path,col_types = 'ddf')
test_data<-read_csv(test.path,col_types = 'dd')


train_data$batches<- as.factor((train_data$time - 0.0001)%/%50)
train_data$time_batch <- ((train_data$time - 0.0001)%%50)+0.0001




ggplot(train_data %>% slice(seq(1,n(),400)),
       aes(x=time_batch,y=signal,col=open_channels))+geom_point()+facet_grid(vars(batches))





train_data$trend_res<-0
train_data$trend_spl<-0
train_data$piece<-NA
for(lBatch in c(0,2:5,6:9)){
  
  if(lBatch %in% c(6:9)){
    trend.lm<-lm(signal~poly(time_batch,2),data=subset(train_data,batches==lBatch))
  }else{
    trend.lm<-lm(signal~time_batch,data=subset(train_data,batches==lBatch))
  }
  
  cat("batch is",lBatch,'\n')
  summary(trend.lm) %>% print()
  
  train_data$trend_res[train_data$batches==lBatch] <- trend.lm$residuals
  train_data$trend_spl[train_data$batches==lBatch]<-trend.lm$fitted.values
  train_data$piece[train_data$batches==lBatch]<-paste(as.character(lBatch),"1")
}

inds=train_data$batches==1 & train_data$time_batch<=10
trend.lm<-lm(signal~poly(time_batch,2),data=subset(train_data,inds))
train_data$trend_res[inds]<-trend.lm$residuals
train_data$trend_spl[inds]<-trend.lm$fitted.values
train_data$piece[inds]<-"11"

summary(trend.lm) %>% print()

inds=train_data$batches==1 & train_data$time_batch>10
trend.lm<-lm(signal~time_batch,data=subset(train_data,inds))
train_data$trend_res[inds]<-trend.lm$residuals
train_data$trend_spl[inds]<-trend.lm$fitted.values
train_data$piece[inds]<-"12"

summary(trend.lm) %>% print()




ggplot(train_data %>% slice(seq(1,n(),400)),
       aes(x=time_batch,y=trend_res,col=open_channels))+geom_point()+facet_grid(vars(batches))




cv5<- trainControl(method="cv",number=5,summaryFunction = f1)
train_ix<-createDataPartition(train_data$signal,p = 0.8,list = F)[,1]
train<-train_data[train_ix,]
test<-train_data[-train_ix,]


lda.model02 <- train(open_channels~signal+batches,data=train,method="lda",family="binomial", trControl=cv5,verbosity=T,metric="F1")
lda.model02
lda.valid02<-predict(lda.model02,newdata = test)
f1(data.frame(obs=test$open_channels,pred=lda.valid02))

lda.model03 <- train(open_channels~trend_res+batches,data=train,method="lda",family="binomial", trControl=cv5,verbosity=T,metric="F1")
lda.model03
lda.valid03<-predict(lda.model03,newdata = test)
f1(data.frame(obs=test$open_channels,pred=lda.valid03))



# for predict test values u should compare
# test signal amplitude for each batch with test ones
# it's necessary for choosing optimal batch number 
#
# then u should subtract trend for each batch
# and use model
#
#

test_data$batches<- factor((test_data$time - 0.0001)%/%50-10,levels=levels(train_data$batches))
test_data$time_batch <- ((test_data$time - 0.0001)%%50)+0.0001

ggplot(test_data %>% slice(seq(1,n(),300)),
       aes(x=time_batch,y=signal))+geom_point()+facet_grid(vars(batches))


test_data$trend_res<-0

for(lbatch in c(0,1)){
  inds=test_data$batches==lbatch & test_data$time_batch<=30
trend.lm<-lm(signal~poly(time_batch,2),data=subset(test_data,inds))
test_data$trend_res[inds]<-trend.lm$residuals

inds=test_data$batches==lbatch & test_data$time_batch>30
trend.lm<-lm(signal~poly(time_batch,2),data=subset(test_data,inds))
test_data$trend_res[inds]<-trend.lm$residuals
}

inds=test_data$batches==2
trend.lm<-lm(signal~poly(time_batch,2),data=subset(test_data,inds))
test_data$trend_res[inds]<-trend.lm$residuals


inds=test_data$batches==3
trend.lm<-lm(signal~time_batch,data=subset(test_data,inds))
test_data$trend_res[inds]<-trend.lm$residuals



ggplot(test_data %>% slice(seq(1,n(),300)),
       aes(x=time_batch,y=trend_res))+geom_point()+facet_grid(vars(batches))


#find best combinations by plots
tr=train_data %>% slice(seq(1,n(),600))
te=test_data %>% slice(seq(1,n(),400))

for(i in 0:3){
  for(j in 0:9){
    tmp=rbind(
      tr %>% filter(batches==j) %>% select(time_batch,signal) %>% mutate(source='train'),
      te %>% filter(batches==i) %>% select(time_batch,signal) %>% mutate(source='test')
    )
   pl= ggplot(tmp %>% mutate(source=factor(source)),
           aes(x=time_batch,y=signal))+geom_point()+facet_wrap(source~.)
   ggsave(paste0('test batch = ',i,' train batch = ',j,'.png'),pl)
  }
}


# use optimal batches
test_data$batches[test_data$batches==0]=6
test_data$batches[test_data$batches==1]=4
test_data$batches[test_data$batches==2]=6
test_data$batches[test_data$batches==3]=0



#best model predictions

lda.model03 <- train(open_channels~trend_res+batches,data=train_data,method="lda",family="binomial", trControl=cv5,verbosity=T,metric="F1")

res=predict(lda.model03,newdata = test_data)


#writing_sample

answer=read_csv(sample.path)

answer$time=format(answer$time,nsmall = 4)
#answer$open_channels=sample(0:10,answer$time %>% length(),replace = T)
answer$open_channels=res


write_csv(answer,paste0(data.path,"result.csv"))














