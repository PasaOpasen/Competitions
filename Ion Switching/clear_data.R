
library(tidyverse)
library(caret)
library(magrittr)


path.dir='./ignore_data/'
#test.dir=paste0(path.dir,'newtest.csv')
#train.dir=paste0(path.dir,'newtrain.csv')

test.dir=paste0(path.dir,'newtest_pca.csv')
train.dir=paste0(path.dir,'newtrain_pca.csv')

tst=read_csv(test.dir)
trn=read_csv(train.dir)


# delete bad predictors

inds=nearZeroVar(tst)
nms=colnames(trn)[inds]

trn=trn[,-inds]
tst=tst[,-(inds-1)]

summary(trn)
summary(tst)

trn=trn[,-24]
tst=tst[,-(24-1)]


write_csv(trn,paste0(path.dir,'newtrain.csv'))
write_csv(tst,paste0(path.dir,'newtest.csv'))


write_csv(trn,paste0(path.dir,'newtrain_pca.csv'))
write_csv(tst,paste0(path.dir,'newtest_pca.csv'))



# replace NA by median

mt=preProcess(trn[,-2],method='medianImpute',verbose = T)

gc()

trn[,-2]<-predict(mt,trn[,-2])
tst<-predict(mt,newdata=tst)


# pca

mt=preProcess(trn[,-c(1:4)],method='pca',verbose = T)

trn=cbind(trn[,1:4],predict(mt,trn[,-c(1:4)]))

tst=cbind(tst[,1:3],predict(mt,tst[,-c(1:3)]))



write_csv(trn,paste0(path.dir,'newtrain_pca.csv'))
write_csv(tst,paste0(path.dir,'newtest_pca.csv'))





id=sample(1:5000000,1000)
featurePlot(trn[id,-2],trn$open_channels[id])


for(i in c(300,200,100,50,25)){
  write_csv(trn[sample(1:5000000,5000000/i),],paste0(path.dir,'sample_train(',i,').csv'))
}


corrplot::corrplot(cor(trn),method = 'number')



# simple models


accshow=function(.fit, df){
  
  ldafit<-predict(.fit,newdata = df)
  
  table(df$open_channels, ldafit) %>% print()
  
  f1(data.frame(obs=df$open_channels,pred=ldafit)) %>% print()
}



trn %<>% mutate(open_channels=factor(open_channels))

id_train=numeric()
id_test=numeric()
als=1:5000000
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),1000)]
  id2<-createDataPartition(id,p = 0.8,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}

train<-trn[id_train,]
test<-trn[id_test,]


control <- trainControl(method="cv",number=5,summaryFunction = f1,verboseIter = T)


lda.fit=train(open_channels~PC1+PC2+PC3+PC4+PC5,
                  data=train,
                  method="lda",
                  family="binomial", 
                  trControl=control,
                  verbosity=T,
                  metric="F1")

accshow(lda.fit,test)


accshow(lda.fit,rbind(train,test))
accshow(lda.fit,trn)




lda.fit=train(open_channels~PC1+PC2+PC3+PC4+PC5,
              data=trn, #rbind(train,test),
              method="mda",
              family="binomial", 
              trControl=control,
              verbosity=T,
              metric="F1")


ggplot(varImp(lda.fit))




res=lda.fit %>% predict(newdata = tst)


#writing_sample

answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'result_svmLinear220000_7pc.csv'))



