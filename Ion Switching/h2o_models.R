library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

trn=read_csv(paste0(path.dir,'train_clean.csv'))
tst=read_csv(paste0(path.dir,'newtest_pca.csv'))
tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))

als=1:nrow(trn)


#trn %>% group_by(factor(open_channels)) %>% summarise(count=n())

# test inds
id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),min(60000,length(s)))]) #+50*i
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
trn=trn[id,-c(3,4)] %>% as.h2o()
tst=tst[,c(1,4:11)] %>% as.h2o()
trn[,2]=as.factor(trn[,2])

#set.seed(1998)

p = h2o.splitFrame(data=trn,ratios = 0.9,seed=1)

train=p[[1]]
test=p[[2]]

#train %>% as.data.frame() %>% group_by(open_channels) %>% summarise(count=n())
#test %>% as.data.frame() %>% group_by(open_channels) %>% summarise(count=n())

#split <- h2o.runif(trn)
#train <- trn [split <= 0.95,]
#test <- trn [split > 0.95,]


gbm_grid <- h2o.grid(
  'gbm',
  x = colnames(train)[c(1,5:12)] , 
  y = 'open_channels', 
  training_frame = train,
  validation_frame = test,
  #nfolds=5,
  hyper_params = list(
    balance_classes = T,
    max_depth = 5,
    min_rows = 10,
    learn_rate = c(0.05,0.1,0.3),
    col_sample_rate = 0.8,
    sample_rate =  0.9,
    ntrees = c(20,50,100)
  )
)




fit_gbm <- h2o.gbm(
  x = colnames(train)[-c(2)] , 
  y = 'open_channels', 
  training_frame =  train, # trn,   
  validation_frame = test,
  
  #nfolds = 5,
  #fold_assignment = 'Stratified',
  
  #verbose = T,
  balance_classes = T,
  #class_sampling_factors = c(1,1.25,2.25,1.85,3,4.5,6.6,4.6,5,9,35),
  max_depth = 6,
  min_rows = 10,
  learn_rate = 0.1,
  learn_rate_annealing = 1,
  col_sample_rate = 0.8,
  sample_rate = 0.9,
  ntrees = 70,
  score_tree_interval = 10#,
  #stopping_metric = 'misclassification',
 # stopping_tolerance = 0.005
  )

accshow(fit_gbm,test,F)

gc()



fit_rf <- h2o.randomForest(
  x = colnames(train)[-c(2)] , 
  y = 'open_channels', 
  training_frame =  train, # trn,   
  validation_frame = test,
  balance_classes = T,
  
  ntrees = 130,
  max_depth = 18,
  min_rows = 1,
  nbins = 20,
  sample_rate = .6,
  col_sample_rate_per_tree = .7,
  min_split_improvement = .000015
)

accshow(fit_rf,test,F)







#sp=sort(sample(1:2000000,2000))
#res0=predict(fit_gbm, newdata= tst[sp,])$predict%>% as.data.frame()

res = (predict(fit_rf, newdata= tst)$predict %>% as.data.frame())$predict# %>% as.numeric() -1



answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'fit_rf 60000.csv'))








h2o.shutdown(prompt = FALSE)
