
library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'


trn=read_csv(paste0(path.dir,'trainsuper.csv'))
tst=read_csv(paste0(path.dir,'testsuper.csv'))

trn %<>% mutate(signal2=sign(signal)*sqrt(abs(signal))) %>% 
  select(-batches,-time_batch,-level2,-level_type)
tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))  %>% 
  select(-batches,-time_batch,-level2,-level_type)

als=1:nrow(trn)


id=numeric()
for(lev in 0:1){
  
  for(i in 0:10){
    s= als[trn$open_channels==i & trn$level1==lev]
    
    if(length(s)>0){
      id= c(id,s[sample(1:length(s),min(8000,length(s)))])
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


tr=trn%>% as.h2o()
te=tst %>% as.h2o()

tr[,2]=as.factor(tr[,2])
#tr[,12]=as.factor(tr[,12])
#te[,11]=as.factor(te[,11])


p = h2o.splitFrame(data=tr[id,] ,ratios = 0.8)

train=p[[1]]
test=p[[2]]



gbm_grid <- h2o.grid(
  'gbm',
  grid_id = 'gr',
  x = colnames(train)[-c(2,12)] , 
  y = 'open_channels', 
  training_frame =  train, # trn1,  #  
  #validation_frame = test1,
  nfolds=8,
  hyper_params = list(
    balance_classes = T,
    max_depth = c(3,4,5,6,7,8,9,10),
    min_rows = c(10,20,30,40,50),
    learn_rate = c(0.05,0.1,0.2,0.3),
    col_sample_rate = c(0.5,0.65,0.8,1),
    sample_rate = c(0.4,0.5,0.6,0.7,0.8,0.9) ,
    ntrees = c(15,20,30,40)
  ),
  search_criteria = list(
    strategy = "RandomDiscrete", 
    max_runtime_secs = 1800,
    max_models = 1000, seed = 1)
)




fit_gbm <- h2o.gbm(
  x = colnames(train)[-c(2,12)] , 
  y = 'open_channels', 
  training_frame =  train,  #   tr,
  validation_frame = test,
  
  #nfolds = 5,
  #fold_assignment = 'Stratified',
  
  #verbose = T,
  balance_classes = T,
  #class_sampling_factors = c(1,1.25,2.25,1.85,3,4.5,6.6,4.6,5,9,35),
  max_depth = 3,
  min_rows = 40,
  learn_rate = 0.2,
  learn_rate_annealing = 1,
  col_sample_rate = 1.0,
  sample_rate = 0.7,
  ntrees = 40,
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






fit_svm <- h2o.psvm(
  x = colnames(train)[-c(2,12)] , 
  y = 'open_channels', 
  training_frame =  train,  #   tr,
  validation_frame = test,
  
  hyper_param = 1,
  gamma = -1,
  
  rank_ratio = -1,
  positive_weight = 1,
  negative_weight = 1,
  disable_training_metrics = TRUE,
  sv_threshold = 1e-04,
  fact_threshold = 1e-05,
  feasible_threshold = 0.001,
  surrogate_gap_threshold = 0.001,
  
  mu_factor = 10,
  max_iterations = 200
)
















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











res = (predict(ensemble, newdata= te)$predict %>% as.data.frame())$predict

res[tst$level_type==2] = (predict(fit_gbm2, newdata= tst2)$predict %>% as.data.frame())$predict %>% as.numeric() -1



answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'bestpy.csv'))








