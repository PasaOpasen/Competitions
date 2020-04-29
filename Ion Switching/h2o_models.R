library(tidyverse)
library(magrittr)
library(zeallot)

path.dir='./ignore_data/'

#trn=read_csv(paste0(path.dir,'train_clean.csv'))
#tst=read_csv(paste0(path.dir,'newtest_pca.csv'))
#tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))

trn=read_csv(paste0(path.dir,'trainsuper.csv'))
tst=read_csv(paste0(path.dir,'testsuper.csv'))

als=1:nrow(trn)
trn=trn[,-c(1,3,4)] 
tst=tst[,-c(1:3)]

tst$level_type=levs

#trn %>% group_by(factor(open_channels)) %>% summarise(count=n())
#trn %>% group_by(factor(level_type)) %>% summarise(count=n())
#trn %>% group_by(factor(level_type),factor(open_channels)) %>% summarise(count=n())

# test inds
id=numeric()
for(lev in 1:2){
  
  for(i in 0:10){
  s= als[trn$open_channels==i & trn$level_type==lev]
  
  if(length(s)>0){
    id= c(id,s[sample(1:length(s),min(60000,length(s)))])
  }

}
}

if(F){
  ggplot(trn %>% filter(level_type==1) %>% 
         group_by(factor(open_channels)) %>% 
         slice(sample(1:n(),min(2000,n()))),
       aes(x=time_batch,y=supersignal,col=factor(open_channels)))+
  geom_point(alpha=0.9)+
  theme_bw()
}




library(h2o)
h2o.init(nthreads = 6,       
         max_mem_size = "9g") 


accshow=function(fit,df,get.matrix=F){
  
  plot(fit)
  
  h2o.varimp_plot(fit)
  
  if(get.matrix){
  h2o.performance(fit, df) %>% print()   
  }

  
  cat('F1 = ',f1_(
    ( df$open_channels %>% as.data.frame())$open_channels%>% as.numeric(),
    (predict(fit, newdata= df)$predict %>% as.data.frame())$predict %>% as.numeric()
  ) ,'\n') 
  
}


#trn=data.table::fread(paste0(path.dir,'train_clean.csv')) %>% as.h2o()


trn1= trn[id,] %>% filter(level_type==1) %>% select(-level_type,-level1,-level2) %>% as.h2o()
trn2= trn[id,] %>% filter(level_type==2) %>% select(-level_type,-level1,-level2) %>% as.h2o()
tst1=tst %>%  filter(level_type==1) %>% select(-level_type,-level1,-level2) %>% as.h2o()
tst2=tst %>%  filter(level_type==2) %>% select(-level_type,-level1,-level2) %>% as.h2o()

trn1[,1]=as.factor(trn1[,1]); trn2[,1]=as.factor(trn2[,1]);
#trn[,10]=as.factor(trn[,10])
#tst[,8]=as.factor(tst[,8])

#set.seed(1998)



c(train1,test1) %<-% h2o.splitFrame(data=trn1,ratios = 0.8)
c(train2,test2) %<-% h2o.splitFrame(data=trn2,ratios = 0.8)


#train %>% as.data.frame() %>% group_by(open_channels) %>% summarise(count=n())
#test %>% as.data.frame() %>% group_by(open_channels) %>% summarise(count=n())

#split <- h2o.runif(trn)
#train <- trn [split <= 0.95,]
#test <- trn [split > 0.95,]

#gbm_grid <- h2o.grid(
#  'gbm',
#  x = colnames(train)[c(1,5:12)] , 
#  y = 'open_channels', 
#  training_frame = train,
#  validation_frame = test,
#  #nfolds=5,
#  hyper_params = list(
#    balance_classes = T,
#    max_depth = 5,
#    min_rows = 10,
#    learn_rate = c(0.05,0.1,0.3),
#    col_sample_rate = 0.8,
#    sample_rate =  0.9,
#    ntrees = c(20,50,100)
#  )
#)




fit_gbm2 <- h2o.gbm(
  x = colnames(train2)[-c(1)] , 
  y = 'open_channels', 
  training_frame = train2,   #trn2,   
  validation_frame = test2,
  
  #nfolds = 5,
  #fold_assignment = 'Stratified',
  
  #verbose = T,
  balance_classes = T,
  #class_sampling_factors = c(1,1.25,2.25,1.85,3,4.5,6.6,4.6,5,9,35),
  max_depth = 6,
  min_rows = 10,
  learn_rate = 0.1,
  learn_rate_annealing = 0.95,
  col_sample_rate = 1,
  sample_rate = 0.9,
  ntrees = 30,
  score_tree_interval = 10#,
  #stopping_metric = 'misclassification',
 # stopping_tolerance = 0.005
  )

accshow(fit_gbm2,test2,T)



fit_gbm1 <- h2o.gbm(
  x = colnames(train2)[-c(1)] , 
  y = 'open_channels', 
  training_frame = trn1,  #  train1, #  
  validation_frame = test1,
  
  #nfolds = 5,
  #fold_assignment = 'Stratified',
  
  #verbose = T,
  balance_classes = T,
  #class_sampling_factors = c(1,1.25,2.25,1.85,3,4.5,6.6,4.6,5,9,35),
  max_depth = 5,
  min_rows = 10,
  learn_rate = 0.1,
  learn_rate_annealing = 1,
  col_sample_rate = 1,
  sample_rate = 0.9,
  ntrees = 60,
  score_tree_interval = 10#,
  #stopping_metric = 'misclassification',
  # stopping_tolerance = 0.005
)

accshow(fit_gbm1,test1,T)






gc()



fit_rf1 <- h2o.randomForest(
  x = colnames(train1)[-c(1)] , 
  y = 'open_channels', 
  training_frame = trn1,  #  train1,  
  validation_frame = test1,
  balance_classes = T,
  
  ntrees = 100,
  max_depth = 12,
  min_rows = 1,
  nbins = 20,
  #sample_rate = .6,
  col_sample_rate_per_tree = .9#,
  #min_split_improvement = .000015
)

accshow(fit_rf1,test1,T)







#sp=sort(sample(1:2000000,2000))
#res0=predict(fit_gbm, newdata= tst[sp,])$predict%>% as.data.frame()

res = (predict(fit_gbm, newdata= tst)$predict %>% as.data.frame())$predict

res=rep(-1,2e6)
res[levs==1] = as.numeric((predict(fit_rf1, newdata= tst1)$predict %>% as.data.frame.array() %>% tbl_df())$predict)
res[levs==2] = as.numeric((predict(fit_gbm2, newdata= tst2)$predict %>% as.data.frame.array() %>% tbl_df())$predict)


answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'l1=rf, l2=gbm.csv'))




save(res,file='result of l1=gbm, l2=gbm.rdata')



h2o.shutdown(prompt = FALSE)






ggplot(tst%>% filter(level_type==1) %>% slice(seq(1,n(),by=4)),
       aes(x=seq(0,1,length.out = length(supersignal)), y=supersignal))+
  geom_point()
















