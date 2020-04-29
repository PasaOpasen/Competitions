
library(tidyverse)
library(magrittr)
library(caret)
library(doParallel)


path.dir='./ignore_data/'


trn=read_csv(paste0(path.dir,'train_bad.csv'))
tst=read_csv(paste0(path.dir,'test_bad.csv'))

trn %<>% select(-batches) %>% mutate(open_channels=factor(open_channels)) 
tst %<>% select(-batches)



cv1=trainControl(method="cv",number=6,summaryFunction = f1,verboseIter = T)
cv2=trainControl(method = 'none',verboseIter = T,summaryFunction = f1)



als=1:nrow(trn)
id=numeric()
for(i in 1:10){
  s= als[trn$open_channels==i]
  
    id= c(id,s[sample(1:length(s),min(8000,length(s)))])
  
}

trn1=trn[id,]


gc()

id=createDataPartition(1:nrow(trn1),p = 0.2,list = F)[,1]

train1=trn1[id,]
test1=trn1[-id,]





models=c(
  'lda',
  'lda2',
  'sda',
  'naive_bayes',
  'treebag',
  'multinom',
  'svmLinear',
  'svmLinear3',
  'svmPoly',
  'svmLinear2',
  'svmRadial',
  'svmRadialCost',
  'parRF'
)

grids=list(
  'lda2'=expand.grid(dimen=0),
  'naive_bayes'=expand.grid(laplace=0,usekernel=T, adjust=seq(3,4,length.out = 12)),
  #'treebag'=expand.grid(parameter=none)
  'multinom'=expand.grid(decay=0),
  'svmLinear'=expand.grid(C=seq(0.1,5,length.out = 20)),
  'parRF'=expand.grid(mtry=2:5)
)



rs=list()
fits=list()
for(ft in models){
  
  cat('Now: ',ft,'\n')
  
  t=Sys.time()
  
  cl <- makePSOCKcluster(6)
  registerDoParallel(cl)
  
  .fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
               sin(signal)+signal2+time_batch+
               signal+q5+q15+q25+q35+q45+q55+q65+q75+q85+q95
             ,
             data=train1,
             method=ft,
             #family="binomial", 
             trControl=cv1,
             #tuneLength=4,
             #tuneGrid=grids[[ft]],
             #verbosity=T,
             metric="F1")
  
  t=Sys.time()-t
  
  stopCluster(cl)
  
  cat('fitting...\n')
  
  v=f1(data.frame(obs=test1$open_channels,pred=predict(.fit,newdata = test1)))
  
  rs[[ft]]=list(fit=ft,result=v,time=t, tune=.fit$bestTune)
  fits[[ft]]=.fit
  print(rs)
}






























