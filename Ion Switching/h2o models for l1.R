
library(tidyverse)
library(magrittr)
library(zeallot)

path.dir='./ignore_data/'

trn=read_csv(paste0(path.dir,'train_bad.csv'))
tst=read_csv(paste0(path.dir,'test_bad.csv'))

trn %<>% select(-batches) #%>% mutate(open_channels=factor(open_channels)) 
tst %<>% select(-batches)

als=1:nrow(trn)




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


trn1= trn %>% as.h2o()
tst1=tst %>%  as.h2o()

trn1[,2]=as.factor(trn1[,2]);


id=numeric()
for(i in 1:10){
  s= als[trn$open_channels==i]
  
  id= c(id,s[sample(1:length(s),min(10000,length(s)))])
  
}
id=sort(id)

c(train1,test1) %<-% h2o.splitFrame(data=trn1[id,],ratios = 0.8)





gbm_grid <- h2o.grid(
  'gbm',
  x = colnames(train1)[-c(2)] , 
  y = 'open_channels', 
  training_frame =  train1, # trn1,  #  
  #validation_frame = test1,
  nfolds=5,
  hyper_params = list(
    balance_classes = T,
    max_depth = c(3,4,5,6,7,8,9,10),
    min_rows = c(10,20,30,40),
    learn_rate = c(0.05,0.1,0.2,0.3),
    col_sample_rate = c(0.5,0.65,0.8,1),
    sample_rate = c(0.6,0.7,0.8,0.9) ,
    ntrees = c(15,20,30,40)
  ),
  search_criteria = list(
    strategy = "RandomDiscrete", 
    max_runtime_secs = 1800,
    max_models = 100, seed = 1)
)

gbm_gridperf2 <- h2o.getGrid(grid_id = "gbm_grid2",
                             sort_by = "auc",
                             decreasing = TRUE)







fit_gbm1 <- h2o.gbm(
  x = colnames(train1)[-c(2)] , 
  y = 'open_channels', 
  training_frame =  train1, # trn1,  #  
  validation_frame = test1,
  
  #nfolds = 5,
  #fold_assignment = 'Stratified',
  
  #verbose = T,
  balance_classes = T,
  #class_sampling_factors = c(1,1.25,2.25,1.85,3,4.5,6.6,4.6,5,9,35),
  max_depth = 5,
  min_rows = 40,
  learn_rate = 0.1,
  learn_rate_annealing = 1,
  col_sample_rate = 0.8,
  sample_rate = 0.7,
  ntrees = 40,
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



fit_rf1 <- h2o.randomForest(
  x = colnames(train1)[-c(2)] , 
  y = 'open_channels', 
  training_frame = train1,  #trn1,    
  validation_frame = test1,
  balance_classes = T,
  
  ntrees = 100,
  max_depth = 8,
  min_rows = 1,
  nbins = 20,
  sample_rate = .9,
  col_sample_rate_per_tree = .7#,
  #min_split_improvement = .000015
)

accshow(fit_rf1,test1,T)





dl <- h2o.deeplearning(
  x = colnames(train1)[-c(2,3,11,9,10)] , 
  y = 'open_channels', 
  training_frame = train1,  #trn1,    
  validation_frame = test1,
  balance_classes = T,
  
  shuffle_training_data=T,
  
                       distribution = "multinomial",
                       hidden = c(25,20),
                       epochs = 200,
  l1=0,
  l2=0.0001,
                       train_samples_per_iteration = -2,
                       activation = "Maxout",
                       single_node_mode = FALSE,
                       force_load_balance = T,
                       score_training_samples = 0,
                       score_validation_samples = 0,
  score_interval = 3,
                       stopping_rounds = 0)


accshow(dl,test1,T)




#  ensemble  ####

nfolds <- 5

my_gbm <- h2o.gbm(  x = colnames(train1)[-c(2)] , 
                    y = 'open_channels', 
                    training_frame =  trn1, 
                    distribution = "multinomial",
                    balance_classes = T,

                    max_depth = 5,
                    min_rows = 40,
                    learn_rate = 0.1,
                    learn_rate_annealing = 1,
                    col_sample_rate = 0.8,
                    sample_rate = 0.7,
                    ntrees = 40,
                    score_tree_interval = 10,
                    
                    
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,
                    seed = 1)

accshow(my_gbm,test1,F)



my_rf <- h2o.randomForest( x = colnames(train1)[-c(2)] , 
                           y = 'open_channels', 
                           training_frame =  trn1, 
                           distribution = "multinomial",
                           balance_classes = T,
                           
                           ntrees = 100,
                           max_depth = 8,
                           min_rows = 1,
                           nbins = 20,
                           sample_rate = .9,
                           col_sample_rate_per_tree = .7,
                           
                           nfolds = nfolds,
                           fold_assignment = "Modulo",
                           keep_cross_validation_predictions = TRUE,
                           seed = 1)

accshow(my_rf,test1,F)


my_dl <- h2o.randomForest( x = colnames(train1)[-c(2,3,11,9,10)] , 
                           y = 'open_channels', 
                           training_frame =  trn1, 
                           distribution = "multinomial",
                           balance_classes = T,
                           
                           shuffle_training_data=T,
                           hidden = c(25,20),
                           epochs = 200,
                           l1=0,
                           l2=0.0001,
                           train_samples_per_iteration = -2,
                           activation = "Maxout",
                           single_node_mode = FALSE,
                           force_load_balance = T,
                           score_training_samples = 0,
                           score_validation_samples = 0,
                           score_interval = 3,
                           stopping_rounds = 0,
                           
                           nfolds = nfolds,
                           fold_assignment = "Modulo",
                           keep_cross_validation_predictions = TRUE,
                           seed = 1)

accshow(my_dl,test1,F)


ensemble <- h2o.stackedEnsemble(x = colnames(train)[-c(2)] , 
                                y = 'open_channels', 
                                training_frame =  train, 
                                
                                model_id = "my_ensemble",
                                base_models = list(my_gbm, my_rf))


accshow(ensemble,test1,T,F)



#sp=sort(sample(1:2000000,2000))
#res0=predict(fit_gbm, newdata= tst[sp,])$predict%>% as.data.frame()


res[levs==1] = as.numeric((predict(fit_gbm1, newdata= tst1)$predict %>% as.data.frame.array() %>% tbl_df())$predict)


answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'l1=gbm.csv'))




save(res,file='result of l1=gbm, l2=gbm.rdata')

load('result of l1=gbm, l2=gbm.rdata')

load('levels for test.rdata')

h2o.shutdown(prompt = FALSE)






ggplot(tst%>% filter(level_type==1) %>% slice(seq(1,n(),by=4)),
       aes(x=seq(0,1,length.out = length(supersignal)), y=supersignal))+
  geom_point()












