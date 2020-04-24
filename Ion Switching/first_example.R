
#example from https://www.kaggle.com/jpbeconne/beginner-notebook
#https://www.kaggle.com/kittlein/clean-signal-and-naive-features

library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(MLmetrics)
library(doSNOW)
library(data.table)
library(readr)
library(tidyverse)
library(xgboost)
library(RcppRoll)
library(dse)
library(KFAS)
library(wmtsa)
library(onehot)
#library(devtools)
#install_github("andrewuhl/RollingWindow")
#library(RollingWindow)
library(tsfeatures)


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
  return(f1_val)
}


f1_ <- function (obs,pred, lev = NULL,model=NULL) {
  cm<-as.matrix(table(actual=obs,predicted=pred))
  diag<-diag(cm)
  rowsums<-apply(cm,1,sum)
  colsums<-apply(cm,2,sum)
  precision <- ifelse(colsums==0,0,diag/colsums)
  recall  <- ifelse(rowsums==0,0,diag/rowsums)
  return(mean(ifelse(precision+recall==0,0,2*precision*recall/(precision+recall)))) 
}


data.path='D:/liverpool-ion-switching/'

train.path=paste0(data.path,'train_clean','.csv')
test.path=paste0(data.path,'test_clean','.csv')
sample.path=paste0(data.path,'sample_submission','.csv')


train_data<-read_csv(train.path,col_types = 'ddf')
test_data<-read_csv(test.path,col_types = 'dd')


train_data$batches<- as.factor((train_data$time - 0.0001)%/%50)
train_data$time_batch <- ((train_data$time - 0.0001)%%50)+0.0001

test_data$batches<- factor((test_data$time - 0.0001)%/%50-10,levels=levels(train_data$batches))
test_data$time_batch <- ((test_data$time - 0.0001)%%50)+0.0001


#experiment predictions#####
number_peaks <- function(x, window){
  rollmax <- as.numeric(roll_max(x, window))
  npeak <- sum(x > lead(rollmax, n = window) & x > lag(rollmax), na.rm = T)
  return(npeak)
}

# fast removal of na values
Rep.Nas = function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), (i):=0]
}

decode=function(x){
  bre=quantile(x, seq(0.1, 0.9, by=0.05))
  clase=rep(NA, length(x))
  for(j in 1:length(bre)){
    indi= which(x >=  bre[j] & x < bre[j+1]) 
    clase[indi]=j
  }
  return(clase)
}

# Get count of signal values around mean of open channels values
msoc=as.vector(by(train_data$signal, train_data$open_channels, mean))

freChann=function(x, chann){
  indi= which(x >  msoc[chann] - 0.1 & x < msoc[chann] + 0.1)
  return(length(indi))
}

# Smooth signal
mkSmoo=function(x, bw){
  sk=ksmooth(seq(0, 10, length=length(x)), x, bandwidth = bw)
  return(sk$y)
}

train_data %<>% group_by(batches) %>% mutate(
  mKsmoo2 = mkSmoo(signal, bw=0.0025),
  cls1 = decode(signal), 
  signalSquare = signal^2,
  cls2=decode(signalSquare), 
  signale33 = sign(signal)*abs(signal)^(1/3),
  cls3= decode(signale33)
)

train_data %<>%
  mutate(
 # RollingMaxB10 = roll_max(signal, n = 10, fill=max(signal)),
  RollingMaxB20= roll_max(signal, n = 20, fill=max(signal))
) %>% 
  group_by(batches) %>% mutate(
   RollingMeanB10 = roll_mean(signal, n = 10, fill=mean(signal)),
   q4B = length(which(signal > 0 & signal < 1.5)==T), 
   #mB =mean(signal), 
   #maxB = max(signal), 
   #RollingMaxB10 = roll_max(signal, n = 10, fill=max(signal)), 
  #abs_avgBs2 = (abs(min(signal)) + abs(max(signal)))/2, 
   
   #abs_avgB = (abs(min(signal)) + abs(max(signal)))/2,
    
   #q5B = length(which(signal > 0 & signal < 1.5)==T),
   shiftM1 = shift(signal, n=1, fill=mean(signal), "lag"), 
   shiftM2 = shift(signal, n=2, fill=mean(signal), "lag"), 
   shiftP1 = shift(signal, n=1, fill=mean(signal), "lead"), 
  shiftP2 = shift(signal, n=2, fill=mean(signal), "lead")
)

for(name in colnames(train_data)){
  t = train_data[,name] %>% unlist()
  train_data[is.na(t) | is.nan(t),name]=0
}


#experiment https://www.kaggle.com/frankmollard/random-gbm-ion-shift-rfc####
CenteredRoll <- function(DF, lags){
  
  Features = NULL
  
  for (l in lags) {
    Start = Sys.time()
    mea <-
      RcppRoll::roll_mean(DF$signal,
                          n = l,
                          fill = NA,
                          align = "center")
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    sdev <-
      RcppRoll::roll_sd(DF$signal,
                        n = l,
                        fill = NA,
                        align = "center")
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    maxi <-
      RcppRoll::roll_max(DF$signal,
                         n = l,
                         fill = NA,
                         align = "center")
    
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    mini <-
      RcppRoll::roll_min(DF$signal,
                         n = l,
                         fill = NA,
                         align = "center")
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    Wmea <-
      RcppRoll::roll_mean(
        DF$signal,
        n = l,
        weights = c((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1)
                    -max((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1))/((l-1)/2), 
                    max((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1))*2,
                    (((l-1)/2):1) / ((l-1)/2 * ((l-1)/2 + 1) / 1)
                    -max((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1))/((l-1)/2)),
        fill = NA,
        align = "center"
      )
    
    End = Sys.time()
    print(End - Start)
    
    #medi,
    Features <- cbind(Features, mea, sdev, maxi, mini, Wmea)
    
    
    colnames(Features)[c(
      (ncol(Features) - 4),
      (ncol(Features) - 3),
      (ncol(Features) - 2),
      (ncol(Features) - 1),
      ncol(Features))] <-
      c(
        paste("mea", l, sep = "_"),
        paste("sdev", l, sep = "_"),
        paste("max", l, sep = "_"),
        paste("min", l, sep = "_"),
        paste("Wmea", l, sep = "_")
      )
    
    gc()
    print(l)
  }
  
  return(Features)
}

#right/left aligned stats

LRRoll <- function(DF){
  
  LR = NULL
  
  l=100
  right_W100 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (1:l) / (l * (l + 1) / 2),
      fill = NA,
      align = "right"
    )
  
  left_W100 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (l:1) / (l * (l + 1) / 2),
      fill = NA,
      align = "left"
    )
  
  l=1000
  right_W1000 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (1:l) / (l * (l + 1) / 2),
      fill = NA,
      align = "right"
    )
  
  left_W1000 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (l:1) / (l * (l + 1) / 2),
      fill = NA,
      align = "left"
    )
  
  LR <- cbind(LR, right_W100, right_W1000, left_W100, left_W1000)
  
  return(LR)
}

specialFeatures <- function(DF){
  
  SFeatures = NULL
  #Lags----------------------------------
  L1 <- c(NA, DF$signal[1:(nrow(DF)-1)])
  L2 <- c(c(NA,NA), DF$signal[1:(nrow(DF)-2)])
  F1 <- c(DF$signal[2:nrow(DF)], NA)
  F2 <- c(DF$signal[3:nrow(DF)], c(NA, NA))
  
  #specials----------------------------------------
  dL1 <- c(NA, diff(DF$signal))
  dF1 <- c(diff(DF$signal), NA)
  signalAbs <- abs(DF$signal)
  signalSquare <- DF$signal^2
  signaleSqRo <- sign(DF$signal)*abs(DF$signal)^(1/2)
  signale33 <- sign(DF$signal)*abs(DF$signal)^(1/3)
  e <- exp(DF$signal)
  
  SFeatures <- cbind(SFeatures, L1, L2, F1, F2, dL1, dF1, signalAbs, signalSquare, signaleSqRo, signale33, e)
  
  return(SFeatures)
}

lags = c(11, 51, 101, 1001, 10001)

###FE Train
RollFeat <- CenteredRoll(train_data, lags)
LRFeat <- LRRoll(train_data)
specials <- specialFeatures(train_data)
DF_tr <- cbind(train_data, RollFeat, LRFeat, specials)
rm(RollFeat, LRFeat, specials)
#mean center
#DF_tr$signal_M1001 <- DF_tr$signal - DF_tr$mea_1001#centered mean
DF_tr$signal_M101 <- DF_tr$signal - DF_tr$mea_101

#BATCH SLICES
DF_tr$batch <- DF_tr$time %/%10

batch75 <- aggregate(signal~batch, data = DF_tr, FUN = quantile, probs = .75)
colnames(batch75)[2] <- "signal75"
DF_tr <- merge(x = DF_tr, y = batch75, by = "batch", all.x = T)

batch25 <- aggregate(signal~batch, data = DF_tr, FUN = quantile, probs = .25)
colnames(batch25)[2] <- "signal25"
DF_tr <- merge(x = DF_tr, y = batch25, by = "batch", all.x = T)

batchMax <- aggregate(signal~batch, data = DF_tr, FUN = max)
colnames(batchMax)[2] <- "signalMax"
DF_tr <- merge(x = DF_tr, y = batchMax, by = "batch", all.x = T)

batchMin <- aggregate(signal~batch, data = DF_tr, FUN = min)
colnames(batchMin)[2] <- "signalMin"
DF_tr <- merge(x = DF_tr, y = batchMin, by = "batch", all.x = T)

DF_tr <- DF_tr[order(DF_tr$time),]

#Upper Lower Difference
DF_tr$UL <- DF_tr$max_1001 - DF_tr$min_1001


summary(DF_tr)

#Complete
#DF_tr <- DF_tr[complete.cases(DF_tr),]

rm(batch75, batch25, batchMax, batchMin, train)
gc()

##########Test
RollFeat <- CenteredRoll(test_data, lags)
LRFeat <- LRRoll(test_data)
specials <- specialFeatures(test_data)
DF_te <- cbind(test_data, RollFeat, LRFeat, specials)
rm(RollFeat, LRFeat, specials)
#mean center
DF_te$signal_M1001 <- DF_te$signal - DF_te$mea_1001#centered mean
DF_te$signal_M101 <- DF_te$signal - DF_te$mea_101

#BATCH SLICES

DF_te$batch <- DF_te$time %/%10
#DF_tr$batch <- round(DF_tr$time/10, digits = 0)  

batch75 <- aggregate(signal~batch, data = DF_te, FUN = quantile, probs = .75)
colnames(batch75)[2] <- "signal75"
DF_te <- merge(x = DF_te, y = batch75, by = "batch", all.x = T)

batch25 <- aggregate(signal~batch, data = DF_te, FUN = quantile, probs = .25)
colnames(batch25)[2] <- "signal25"
DF_te <- merge(x = DF_te, y = batch25, by = "batch", all.x = T)

batchMax <- aggregate(signal~batch, data = DF_te, FUN = max)
colnames(batchMax)[2] <- "signalMax"
DF_te <- merge(x = DF_te, y = batchMax, by = "batch", all.x = T)

batchMin <- aggregate(signal~batch, data = DF_te, FUN = min)
colnames(batchMin)[2] <- "signalMin"
DF_te <- merge(x = DF_te, y = batchMin, by = "batch", all.x = T)


DF_te <- DF_te[order(DF_te$time),]

#Upper Lower Difference
DF_te$UL <- DF_te$max_1001 - DF_te$min_1001

#Complete
#DF_te <- DF_te[complete.cases(DF_te),]
#DF_te %<>% mice::mice(method = "mean") %>% mice::complete()
rm(batch75, batch25, batchMax, batchMin, test)
gc()





DF_tr <- as.data.frame(DF_tr)
DF_te <- as.data.frame(DF_te)

DF_tr <- DF_tr[DF_tr$time > 0 & DF_tr$time <= 500,-which(colnames(DF_tr) %in% c("time", "batch"))]
DF_te <- DF_te[DF_te$time > 500 & DF_te$time <= 700,-which(colnames(DF_te) %in% c("time", "batch"))]
#DF_tr <- cbind(DF_tr, RFCTr)
#DF_te <- cbind(DF_te, RFCTe)
#rm(RFCTr, RFCTe)

gc()

DF_tr$open_channels <- as.factor(DF_tr$open_channels)



write_csv(DF_tr,"newtrain.csv")
write_csv(DF_te,"newtest.csv")


DF_te <- DF_tr[complete.cases(DF_tr),]
write_csv(DF_tr[sample(1:nrow(DF_tr),nrow(DF_tr)/100),],"newtrainsample(50).csv")


############

ggplot(train_data %>% dplyr::slice(seq(1,n(),400)),
       aes(x=time,y=signal,col=open_channels))+geom_point()

ggplot(train_data %>% dplyr::slice(seq(1,n(),400)),
       aes(x=time_batch,y=signal,col=open_channels))+geom_point()+facet_grid(vars(batches))



#estimate trends####

train_data$trend_res<-0
train_data$trend_spl<-0
train_data$piece<-NA
for(lBatch in c(0,2:5,6:9)){
  
  if(lBatch %in% c(6:9)){
    trend.lm<-lm(signal~poly(time_batch,2),data=subset(train_data,batches==lBatch))
  }else{
    trend.lm<-lm(signal~time_batch,data=subset(train_data,batches==lBatch))
  }
  
  #cat("batch is",lBatch,'\n')
  #summary(trend.lm) %>% print()
  
  train_data$trend_res[train_data$batches==lBatch] <- trend.lm$residuals
  train_data$trend_spl[train_data$batches==lBatch]<-trend.lm$fitted.values
  train_data$piece[train_data$batches==lBatch]<-paste(as.character(lBatch),"1")
}

inds=train_data$batches==1 & train_data$time_batch<=10
trend.lm<-lm(signal~poly(time_batch,2),data=subset(train_data,inds))
train_data$trend_res[inds]<-trend.lm$residuals
train_data$trend_spl[inds]<-trend.lm$fitted.values
train_data$piece[inds]<-"11"

#summary(trend.lm) %>% print()

inds=train_data$batches==1 & train_data$time_batch>10
trend.lm<-lm(signal~time_batch,data=subset(train_data,inds))
train_data$trend_res[inds]<-trend.lm$residuals
train_data$trend_spl[inds]<-trend.lm$fitted.values
train_data$piece[inds]<-"12"

#summary(trend.lm) %>% print()




ggplot(train_data %>% slice(seq(1,n(),400)),
       aes(x=time_batch,y=trend_res,col=open_channels))+geom_point()+facet_grid(vars(batches))


corrplot::corrplot(cor(train_data[,sapply(train_data,is.numeric)]))


#modeling####

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


lda.model03=train(open_channels~trend_res+time_batch+batches+
                    signale33+mKsmoo2+cls1+cls2+cls3+RollingMeanB10,
                  data=train,
                  method="lda",
                  family="binomial", 
                  trControl=cv5,
                  verbosity=T,
                  metric="F1")

lda.valid03<-predict(lda.model03,newdata = test)

table(test$open_channels, lda.valid03)

f1(data.frame(obs=test$open_channels,pred=lda.valid03))







#test predictions####

# for predict test values u should compare
# test signal amplitude for each batch with test ones
# it's necessary for choosing optimal batch number 
#
# then u should subtract trend for each batch
# and use model
#
#

######## default batches
#test_data$batches<- factor((test_data$time - 0.0001)%/%50-10,levels=levels(train_data$batches))

#test_data$time_batch <- ((test_data$time - 0.0001)%%50)+0.0001

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
test_data$batches[test_data$batches==0]=7
test_data$batches[test_data$batches==1]=4
test_data$batches[test_data$batches==2]=6
test_data$batches[test_data$batches==3]=1



#best model predictions












#как сделать retrain модели?

lda.model03 <- train(open_channels~trend_res+batches,data=train_data,method="lda",family="binomial", trControl=cv5,verbosity=T,metric="F1")

res=predict(lda.model03,newdata = test_data)


#writing_sample

answer=read_csv(sample.path)

answer$time=format(answer$time,nsmall = 4)
#answer$open_channels=sample(0:10,answer$time %>% length(),replace = T)
answer$open_channels=res


write_csv(answer,paste0(data.path,"result7461.csv"))














