
# обучается не сильно медленно
#
# но пиздец долго предсказывает
#
#
#


library(doParallel)

load("info for best lda and svms.rdata")

cv1=trainControl(method="cv",number=6,summaryFunction = f1)
cv2=trainControl(method = 'none',verboseIter = T)


id_train=numeric()
id_test=numeric()
als=1:5000000
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),3000+50*i)]
  id2<-createDataPartition(id,p = 0.3,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}


train<-trn[id_train,]
test<-trn[id_test,]


cl <- makePSOCKcluster(6)
registerDoParallel(cl)

t=proc.time()
svmRadialSigma=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                       sin(signal)+cos(signal)+
                       signal+
                       sin(2*signal)+cos(2*signal)+
                       #sinpi(signal)+
                       cospi(signal)+
                       #exp(signal)+
                       #exp(-signal^2)+
                       signal2#+
                     #time_batch
                     ,
                     data=train,
                     method='svmRadialSigma',
                     preProcess='BoxCox',
                     #family="binomial", 
                     trControl= cv1,
                     tuneGrid=expand.grid(C=c(0.1,0.3,0.5,0.9,1,5,8),sigma=seq(0.01,0.1,length.out = 7)),
                     #verbosity=T,
                     metric="F1")

t=proc.time()-t
print(t)
stopCluster(cl)


test_res=f1_(obs=test$open_channels,pred=predict(svmRadialSigma,newdata = test))
test_res

for(i in 1:5){
  accshow(svmRadialSigma, trn[sample(als,10000),])  
}

sm=numeric(50)
for(i in 1:length(sm)){
  id=sample(als,5000)
  sm[i]=f1_(obs=trn[id,'open_channels'],pred=predict(svmRadialSigma,newdata = trn[id,]))
}
summary(sm)


svmRadialSigma$bestTune






id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),11000)]) #+50*i
}

lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                sin(signal)+cos(signal)+
                signal+
                sin(2*signal)+cos(2*signal)+
                #sinpi(signal)+
                cospi(signal)+
                #exp(signal)+
                #exp(-signal^2)+
                signal2#+
              #time_batch
              ,
              #data=train,
              data=trn[id,],
              #data=trn,
              method='svmRadialSigma',
              #family="binomial", 
              tuneGrid=expand.grid(C=0.5,sigma=0.07),
              #trControl=control,
              trControl=cv2,
              preProcess='BoxCox',
              #verbosity=T,
              metric="F1")

system.time(
  lda.fit %>% predict(newdata = tst[1:10000,]) # 3 для модели по 1000, 4.66 для 2000, 4.95 для 3000, 8.8 для 5000, 32 для 20 000
)

for(i in 1:5){
  accshow(lda.fit, trn[sample(als,5000),]) 
}
if(F){
  res=numeric(length(als)/500)
  k=length(res)/8
  
  cl<-makePSOCKcluster(8)
  registerDoParallel(cl)
  system.time(
    foreach(i=1:8)%dopar%{
      dp=((i-1)*k):(i*k)
      res[dp]=predict(lda.fit,newdata = tst[dp,])
    }
    # 3 для модели по 1000, 4.66 для 2000, 4.95 для 3000, 8.8 для 5000, 32 для 20 000
  )
  stopCluster(cl) 
  
  
  cl<-makeCluster(6)
  registerDoParallel(cl)
  
  res=numeric(length(als))
  foreach(i = 1:200) %dopar% {
    dp=(1+(i-1)*10000):(i*10000)
    res[dp] = predict(lda.fit,newdata = tst[dp,])
    print(i)
  }
  
  stopCluster(cl)
}

#res = predict(lda.fit,newdata = tst)

res=numeric(2000000)
for(i in 3:200) {
  dp=(1+(i-1)*10000):(i*10000)
  res[dp] = predict(lda.fit,newdata = tst[dp,])
  print(i)
}
res=res-1

#writing_sample

answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'result_svmRadialSigma_box C 0 5 sigma 0 07 count = 11000.csv'))



save(svmRadialSigma,file="svmRadialSigma_box C 0 5 sigma 0 07 count = 11000.rdata")










