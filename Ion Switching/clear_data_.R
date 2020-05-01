
library(tidyverse)
library(caret)
library(magrittr)


path.dir='./ignore_data/'
test.dir=paste0(path.dir,'newtest_.csv')
train.dir=paste0(path.dir,'newtrain_.csv')

#test.dir=paste0(path.dir,'newtest_pca.csv')
#train.dir=paste0(path.dir,'newtrain_pca.csv')

tst=read_csv(test.dir)
trn=read_csv(train.dir)


trn %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))
tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))

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

cv1=trainControl(method="cv",number=6,summaryFunction = f1,verboseIter = T)
cv2=trainControl(method = 'none',verboseIter = T)

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


# boxcox

#trn %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))
#tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))

#mt=preProcess(trn[,c(1,12)],method='BoxCox',verbose = T)

#trn[,-c(2:4)]=predict(mt,trn[,-c(2:4)])
#tst[,-c(2:3)]=predict(mt,tst[,-c(2:3)])


#write_csv(trn,paste0(path.dir,'newtrain_pca.csv'))
#write_csv(tst,paste0(path.dir,'newtest_pca.csv'))





id=sample(1:5000000,1000)
featurePlot(trn[id,-2],trn$open_channels[id])


for(i in c(300,200,100,50,25)){
  write_csv(trn[sample(1:5000000,5000000/i),],paste0(path.dir,'sample_train(',i,').csv'))
}


corrplot::corrplot(cor(trn),method = 'number')
