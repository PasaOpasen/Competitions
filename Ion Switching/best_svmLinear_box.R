# качество прогноза не супер высокое
# но очень быстрое предсказание и вполне быстрое обучение
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
svmLinear=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                  signal+signal2+
                 sin(signal)+cos(signal)+
                  sin(2*signal)+cos(2*signal)+
                  cospi(signal)
                ,
                data=train,
                method='svmLinear',
                #family="binomial", 
                trControl= cv1,
                tuneGrid=expand.grid(C=c(3,4,5,6,7,8,9,10)),
                #preProcess = c('BoxCox'),
                #verbosity=T,
                metric="F1")

t=proc.time()-t
print(t)
stopCluster(cl)


test_res=f1_(obs=test$open_channels,pred=predict(svmLinear,newdata = test))
test_res


for(i in 1:5){
  accshow(svmLinear, trn[sample(als,10000),])  
}

sm=numeric(50)
for(i in 1:length(sm)){
  id=sample(als,5000)
  sm[i]=f1_(obs=trn$open_channels[id],pred=predict(svmLinear2,newdata = trn[id,]))
}
summary(sm)


svmLinear$bestTune





id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),22000)]) #+50*i
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
              method='svmLinear',
              #family="binomial", 
              tuneGrid=expand.grid(C=10),
              #trControl=control,
              trControl=cv2,
              #verbosity=T,
              metric="F1")

system.time(
  lda.fit %>% predict(newdata = tst[1:10000,]) # 3 для модели по 1000, 4.66 для 2000, 4.95 для 3000, 8.8 для 5000, 32 для 20 000
)

#res = predict(lda.fit,newdata = tst)

res=numeric(2000000)
for(i in 1:200) {
  dp=(1+(i-1)*10000):(i*10000)
  res[dp] = predict(lda.fit,newdata = tst[dp,])
  print(i)
}


#writing_sample

answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'result_svmLinear C=10 n=22000.csv'))

svmLinear=lda.fit
save(svmLinear,file='svmLinear C=10 n=22000.radata')