
library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'


trn=read_csv(paste0(path.dir,'train_last_clean.csv'))
tst=read_csv(paste0(path.dir,'test_last.csv'))

trn %<>% mutate(signal2=sign(signal)*sqrt(abs(signal))) %>% select(-time,-batches,-time_batch)
tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))  %>% select(-time,-batches,-time_batch)

als=1:nrow(trn)


id=numeric()
for(lev in 0:1){
  
  for(i in 0:10){
    s= als[trn$open_channels==i & trn$level==lev]
    
    if(length(s)>0){
      id= c(id,s[sample(1:length(s),min(50000,length(s)))])
    }
    
  }
  
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


tr=trn[id,] %>% as.h2o()
te=tst %>% as.h2o()

tr[,2]=as.factor(tr[,2])
tr[,12]=as.factor(tr[,12])
te[,11]=as.factor(te[,11])


p = h2o.splitFrame(data=tr,ratios = 0.8)

train=p[[1]]
test=p[[2]]



fit_gbm <- h2o.gbm(
  x = colnames(train)[-c(2,3,13,4)] , 
  y = 'open_channels', 
  training_frame =  train,  #   tr,
  validation_frame = test,
  
  #nfolds = 5,
  #fold_assignment = 'Stratified',
  
  #verbose = T,
  balance_classes = T,
  #class_sampling_factors = c(1,1.25,2.25,1.85,3,4.5,6.6,4.6,5,9,35),
  max_depth = 5,
  min_rows = 10,
  learn_rate = 0.1,
  learn_rate_annealing = 0.99,
  col_sample_rate = 0.8,
  sample_rate = 0.9,
  ntrees = 40,
  score_tree_interval = 10#,
  #stopping_metric = 'misclassification',
  # stopping_tolerance = 0.005
)

accshow(fit_gbm,test,T)












res = (predict(fit_gbm, newdata= tst)$predict %>% as.data.frame())$predict

res[tst$level_type==2] = (predict(fit_gbm2, newdata= tst2)$predict %>% as.data.frame())$predict %>% as.numeric() -1



answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'fit_gbm_l2 svmLinear_l1.csv'))










