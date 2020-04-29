

library(tidyverse)
library(magrittr)
library(caret)

cv1=trainControl(method="cv",number=6,summaryFunction = f1,verboseIter = T)
cv2=trainControl(method = 'none',verboseIter = T,summaryFunction = f1)


path.dir='./ignore_data/'

#trn=read_csv(paste0(path.dir,'train_clean.csv'))
#tst=read_csv(paste0(path.dir,'newtest_pca.csv'))
#tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))

trn=read_csv(paste0(path.dir,'trainsuper.csv'))
tst=read_csv(paste0(path.dir,'testsuper.csv'))

trn %<>% mutate(signal2=sign(supersignal)*sqrt(abs(supersignal))) 
tst %<>% mutate(signal2=sign(supersignal)*sqrt(abs(supersignal))) 

###ggplot(trn,aes(x=time_batch,y=supersignal,col = open_channels))+geom_point(alpha=0.95)+
###  facet_wrap(level_type~.)+theme_bw()

#trn %>% group_by(factor(open_channels)) %>% summarise(count=n())
#trn %>% group_by(factor(level_type)) %>% summarise(count=n())
#trn %>% group_by(factor(level_type),factor(open_channels)) %>% summarise(count=n())

# test inds

  
#trn=data.table::fread(paste0(path.dir,'train_clean.csv')) %>% as.h2o()

trn= trn[,-c(1,3,4)]  %>% filter(level_type==1 & open_channels>0) %>% select(-level_type,-level1,-level2) 
tst1= tst[,-c(1:3)] %>%  filter(level_type==1) %>% select(-level_type,-level1,-level2)

trn %<>% mutate(open_channels=factor(open_channels)) 

als=1:nrow(trn)
id=numeric()
  for(i in 1:10){
    s= als[trn$open_channels==i]
    
    if(length(s)>0){
      id= c(id,s[sample(1:length(s),min(32000,length(s)))])
    }
    
  }

trn1=trn[id,]


gc()

id=createDataPartition(1:nrow(trn1),p = 0.2,list = F)[,1]

train1=trn1[id,]
test1=trn1[-id,]


models=c(
  #----'lda',
  'lda2',
  ###'sda',
  #'rda',
  #'loclda',
  'naive_bayes',
  #---'treebag',
  'multinom',
  'svmLinear',
  ##'svmRadialSigma',
  'parRF'#,
  ##'kknn',
  ###'knn'
  #'lvq',
  #'pam',
)

grids=list(
  'lda2'=expand.grid(dimen=0),
  'naive_bayes'=expand.grid(laplace=0,usekernel=T, adjust=seq(3,4,length.out = 12)),
  #'treebag'=expand.grid(parameter=none)
  'multinom'=expand.grid(decay=0),
  'svmLinear'=expand.grid(C=seq(0.1,5,length.out = 20)),
  'parRF'=expand.grid(mtry=2:5)
)

library(doParallel)

rs=list()
fits=list()
for(ft in models){
  
  cat('Now: ',ft,'\n')
  
  t=Sys.time()
  
  cl <- makePSOCKcluster(6)
  registerDoParallel(cl)
  
  .fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                  sin(supersignal)+
                  supersignal+signal2
                ,
                data=train1,
                method=ft,
                #family="binomial", 
                trControl=cv1,
             #tuneLength=4,
             tuneGrid=grids[[ft]],
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




#' svmLinear does not support probs predictions
#'
#' so I will use it as alone model
#'

svmLinear=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
                      sin(supersignal)+
                      supersignal+signal2
                    ,
                    data=trn1, #25000
                    method='svmLinear',
                    family="binomial", 
                    trControl=cv2,
                    #tuneLength=4,
                    tuneGrid=expand.grid(C=0.9),
                    #verbosity=T,
                    metric="F1")




#ggplot(trn,aes(x=supersignal))+geom_boxplot()+facet_wrap(.~open_channels)
#trn %>% filter(level_type==1) %>% filter(open_channels==0 | open_channels==1) %>% group_by(factor(open_channels)) %>% summarise(min=min(supersignal),max=max(supersignal))

ggplot(tst %>% group_by(factor(level_type)) %>% slice(sample(1:n(),min(5000,n()))),
       aes(x=time_batch,y=supersignal))+
  geom_point()+
facet_wrap(.~factor(level_type))+
    theme_bw()


res=rep(-1,2e6)

res %>% table()

res[tst$level_type==1]=predict(svmLinear,tst %>% filter(level_type==1))
res[tst$supersignal < -5.5 & tst$level_type==1]=0


save(res,file='svmLinear3000_l1.rdata')







