

library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

trn=read_csv(paste0(path.dir,'trainsuper.csv'))
tst=read_csv(paste0(path.dir,'testsuper.csv'))


how_many = trn %>% group_by(factor(batches),factor(open_channels)) %>% 
  summarise(count = n())


trn %<>% mutate(signal2=sign(signal)*sqrt(abs(signal))) %>% 
  select(-time_batch,-level2,-level_type,-level1,-supersignal)
tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))  %>% 
  select(-time_batch,-level2,-level_type,-level1,-supersignal)

als=1:nrow(trn)
inds=c(0,0,0,0,0,0,1,1,1,1,0)

id=numeric()
for(btc in 0:9){
  
  if(btc %in% c(4,9)){
    k=1
  }else{
    k=4
  }
  
  for(i in 0:10){
    s= als[trn$open_channels==i & trn$batches==btc]
    
    if(length(s)>0){
      id= c(id,
            s[sample(1:length(s),
                     min(1000*k,length(s)
                         ))])
    }
    
  }
  
}
id=sort(id)



library(h2o)
h2o.init(nthreads = 6,       
         max_mem_size = "9g") 


accshow=function(fit,df,get.matrix=F,plotting=T){
  
  if(plotting){
    plot(fit)
    
    h2o.varimp_plot(fit)
  }
  
  
  if(get.matrix){
    h2o.performance(fit, df) %>% print()   
  }
  
  
  cat('F1 = ',f1_(
    ( df$open_channels %>% as.data.frame())$open_channels%>% as.numeric(),
    (predict(fit, newdata= df)$predict %>% as.data.frame())$predict %>% as.numeric()
  ) ,'\n') 
  
}


tr=trn %>% select(-batches)%>% as.h2o()
te=tst %>% select(-batches)%>% as.h2o()

tr[,2]=as.factor(tr[,2])
#tr[,12]=as.factor(tr[,12])
#te[,11]=as.factor(te[,11])


p = h2o.splitFrame(data=tr[id,] ,ratios = 0.8)
train=p[[1]]
test=p[[2]]



gbm_grid <- h2o.grid(
  'gbm',
  grid_id = 'gr',
  x = colnames(train)[-c(2)] , 
  y = 'open_channels', 
  training_frame =  train, # trn1,  #  
  #validation_frame = test1,
  nfolds=8,
  hyper_params = list(
    balance_classes = c(T,F),
    max_depth = c(2,3,4,5,6,7,8,9,10,12),
    min_rows = c(10,20,30,40,50),
    learn_rate = c(0.05,0.1,0.2,0.3),
    col_sample_rate = c(0.2,0.5,0.65,0.8,1),
    sample_rate = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ,
    ntrees = c(10,15,20,30,40,50)
  ),
  search_criteria = list(
    strategy = "RandomDiscrete", 
    max_runtime_secs = 2000,
    max_models = 1000, seed = 1)
)




fit_gbm <- h2o.gbm(
  x = colnames(train)[-c(2)] , 
  y = 'open_channels', 
  training_frame =  train,  #   tr,
  validation_frame = test,
  
  #nfolds = 5,
  #fold_assignment = 'Stratified',
  
  #verbose = T,
  balance_classes = F,
  #class_sampling_factors = c(1,1.25,2.25,1.85,3,4.5,6.6,4.6,5,9,35),
  max_depth = 10,
  min_rows = 20,
  learn_rate = 0.2,
  learn_rate_annealing = 1,
  col_sample_rate = 0.2,
  sample_rate = 1.0,
  ntrees = 20,
  score_tree_interval = 10#,
  #stopping_metric = 'misclassification',
  # stopping_tolerance = 0.005
)

accshow(fit_gbm,test,T)






fit_rf <- h2o.randomForest(
  x = colnames(train)[-c(2,3,13,4)] , 
  y = 'open_channels', 
  training_frame =  train,  #   tr,
  validation_frame = test,
  balance_classes = T,
  
  ntrees = 100,
  max_depth = 12,
  min_rows = 2,
  nbins = 20,
  #sample_rate = .6,
  col_sample_rate_per_tree = .9#,
  #min_split_improvement = .000015
)

accshow(fit_rf,test,T)



nfolds <- 5

my_gbm <- h2o.gbm(  x = colnames(train)[-c(2,3,13,4)] , 
                    y = 'open_channels', 
                    training_frame =  train, 
                    distribution = "multinomial",
                    
                    balance_classes = T,
                    max_depth = 5,
                    min_rows = 20,
                    learn_rate = 0.1,
                    learn_rate_annealing = 1,
                    col_sample_rate = 0.9,
                    sample_rate = 0.5,
                    ntrees = 40,
                    
                    
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,
                    seed = 1)

accshow(my_gbm,test,F)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest( x = colnames(train)[-c(2,3,13,4)] , 
                           y = 'open_channels', 
                           training_frame =  train, 
                           balance_classes = T,
                           distribution = "multinomial",
                           
                           ntrees = 50,
                           nfolds = nfolds,
                           fold_assignment = "Modulo",
                           keep_cross_validation_predictions = TRUE,
                           seed = 1)

accshow(my_rf,test,F)

ensemble <- h2o.stackedEnsemble(x = colnames(train)[-c(2,3,13,4)] , 
                                y = 'open_channels', 
                                training_frame =  train, 
                                
                                model_id = "my_ensemble",
                                base_models = list(my_gbm, my_rf))


accshow(ensemble,test,T,F)



















