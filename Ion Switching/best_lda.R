
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


set.seed(1998)

cl <- makePSOCKcluster(6)
registerDoParallel(cl)

t=proc.time()
lda2=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
               signal+
               #sin(signal)+
               #cos(signal)+
               #sin(2*signal)+
               #cos(2*signal)+
               #cospi(signal)+
               #I(1/(1+signal^2))+
             #cos(PC1)+
             #sin(PC1)#+
             #cospi(PC1)+
             signal2
             ,
             data=train,
             method='mda',
             #family="binomial", 
             trControl= cv1,
             tuneGrid=expand.grid(subclasses=1:10),
             #verbosity=T,
            #preProcess = c('scale', 'center'),
            preProcess = c('BoxCox'),
             metric="F1")

t=proc.time()-t
print(t)
stopCluster(cl)


test_res=f1_(obs=test$open_channels,pred=predict(lda2,newdata = test))
test_res



# begin 0.9304943 with center+scale      0.9294031 without and with  YeoJohnson     0.9313163 with BoxCox
# choose BoxCox
# 0.9300557 with I(sign(signal)*sqrt(abs(signal)))
# 0.9305665 without sin cos
# 0.9335818 for  PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+signal+I(sign(signal)*sqrt(abs(signal)))
#
#


for(i in 1:5){
  accshow(lda2, trn[sample(als,10000),])  
}

sm=numeric(50)
for(i in 1:length(sm)){
  id=sample(als,5000)
  sm[i]=f1_(obs=trn$open_channels[id],pred=predict(svmLinear2,newdata = trn[id,]))
}
summary(sm)


lda2$bestTune





id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),16000)]) #+50*i
}

lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                signal+
                #sin(signal)+
                #cos(signal)+
                #sin(2*signal)+
                #cos(2*signal)+
                #cospi(signal)+
                #I(1/(1+signal^2))+
                #cos(PC1)+
                #sin(PC1)#+
                #cospi(PC1)+
                signal2
              ,
              data=trn[id,],
              method='mda',
              #family="binomial", 
              trControl= cv2,
              tuneGrid=expand.grid(subclasses=5),
              #verbosity=T,
              #preProcess = c('scale', 'center'),
              preProcess = c('BoxCox'),
              metric="F1")


system.time(
  lda.fit %>% predict(newdata = tst[1:10000,]) # 0.71 для модели по 1000, 4.66 для 2000, 4.95 для 3000, 8.8 для 5000, 32 для 20 000
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

write_csv(answer,paste0(path.dir,'result_mda subclasses = 5  n = 16000.csv'))

mda=lda.fit
save(mda,file="mda_box subclasses = 5  n = 16000.rdata")




