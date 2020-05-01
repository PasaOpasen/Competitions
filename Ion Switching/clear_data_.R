
library(tidyverse)
library(caret)
library(magrittr)


path.dir='./ignore_data/'
test.dir='newtest_.csv'
train.dir='newtrain_.csv'

#test.dir=paste0(path.dir,'newtest_pca.csv')
#train.dir=paste0(path.dir,'newtrain_pca.csv')

tst=read_csv(test.dir)
trn=read_csv(train.dir)


gd = sapply(trn, function(x){
  return(
    is.numeric(x) & sum(is.na(x))/5e6<0.05
  )
})

trn = trn[,gd]
tst=tst[,colnames(trn)[-2]]

# replace NA by median

mt=preProcess(trn[,-2],method='medianImpute',verbose = T)

gc()

trn[,-2]<-predict(mt,trn[,-2])
tst<-predict(mt,newdata=tst)

nms=c('signalAbs','signalSquare','signaleSqRo','signale33','mea_51','max_1001','signal_M101','F2','signalMax','signal75')

write_csv(trn,train.dir)
write_csv(tst,test.dir)


trn=trn %>% select(-all_of(nms))
tst=tst%>% select(-all_of(nms))

# pca

mt=preProcess(trn[,-c(2:4)],method='pca',verbose = T)

mt2=preProcess(trn[,-c(2:4)],method='ica',verbose = T,n.comp=11)

mt3=preProcess(trn[,1], method=c("YeoJohnson"),verbose = T)


trn[,1]=predict(mt3,trn[,1])
trn.pca=predict(mt,trn[,-c(2:4)])
trn.ica=predict(mt2,trn[,-c(2:4)])
trn=cbind(trn[,1:4],trn.pca,trn.ica)

write_csv(trn,paste0(path.dir,'train_best.csv'))


tst[,1]=predict(mt3,tst[,1])
tst.pca=predict(mt,tst[,-c(2:3)])
tst.ica=predict(mt2,tst[,-c(2:3)])
tst=cbind(tst[,1:3],tst.pca,tst.ica)

write_csv(tst,paste0(path.dir,'test_best.csv'))


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


corrplot::corrplot(cor(trn[,-c(1:4)]),method = 'number')
