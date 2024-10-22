
# на большом наборе данных пиздец как долго обучается
# даже если указать чисто обучение с известным тюном
#
# а предсказывает еще дольше:
# если на обучении брать по 1000 с каждого класса (что пиздец как мало)
# 2млн будет предсказывать почти полчаса
#
#
#
#
#
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
svmRadialCost=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                       sin(signal)+cos(signal)+
                       signal+
                       sin(2*signal)+cos(2*signal)+
                       #sinpi(signal)+
                       cospi(signal)+
                       #exp(signal)+
                       #exp(-signal^2)+
                       I(1/(1+signal^2))#+
                     #time_batch
                     ,
                     data=train,
                     method='svmRadialCost',
                     #family="binomial", 
                     trControl= cv1,
                     tuneGrid=expand.grid(C=c(0.1,0.3,0.5,0.9,0.999,1,3,5,8)),
                     #verbosity=T,
                     metric="F1")

t=proc.time()-t
print(t)
stopCluster(cl)


test_res=f1_(obs=test$open_channels,pred=predict(svmRadialCost,newdata = test))
test_res

for(i in 1:5){
accshow(svmRadialCost, trn[sample(als,10000),])  
}

sm=numeric(50)
for(i in 1:length(sm)){
  id=sample(als,5000)
  sm[i]=f1_(obs=trn[id,'open_channels'],pred=predict(svmRadialCost,newdata = trn[id,]))
}
summary(sm)


svmRadialCost$bestTune






id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),5000)]) #+50*i
}

lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                sin(signal)+cos(signal)+
                signal+
                sin(2*signal)+cos(2*signal)+
                #sinpi(signal)+
                cospi(signal)+
                #exp(signal)+
                #exp(-signal^2)+
                I(1/(1+signal^2))#+
              #time_batch
              ,
              #data=train,
              data=trn[id,],
              #data=trn,
              method='svmRadialCost',
              #family="binomial", 
              tuneGrid=expand.grid(C=0.5),
              #trControl=control,
              trControl=cv2,
              #verbosity=T,
              metric="F1")

system.time(
 lda.fit %>% predict(newdata = tst[1:10000,]) # 5.15 для модели по 1000, 7.42 для 2000, 9.5 для 3000, 13 для 5000
)



res=lda.fit %>% predict(newdata = tst)#[1:1000,]


#writing_sample

answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'result_svmRadialCost C 0 5.csv'))

