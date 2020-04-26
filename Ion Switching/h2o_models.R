library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

trn=read_csv(paste0(path.dir,'train_clean.csv'))
tst=read_csv(paste0(path.dir,'newtest_pca.csv'))
tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))

als=1:nrow(trn)

# test inds
id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),min(90000,length(s)))]) #+50*i
}




library(h2o)
h2o.init(nthreads = 6,       
         max_mem_size = "9g") 



#trn=data.table::fread(paste0(path.dir,'train_clean.csv')) %>% as.h2o()
trn=trn[id,] %>% as.h2o()
tst=tst %>% as.h2o()
trn[,2]=as.factor(trn[,2])

set.seed(1998)
split <- h2o.runif(trn)
train <- trn [split <= 0.95,]
test <- trn [split > 0.95,]


fit_gbm <- h2o.gbm(
  x = colnames(train)[c(1,5:12)] , 
  y = 'open_channels', 
  training_frame = train,# trn,
  #validation_frame = test,
  #verbose = T,
  max_depth = 5,
  min_rows = 10,
  learn_rate = 0.1,
  col_sample_rate = 0.8,
  sample_rate = 0.9,
  ntrees = 140,
  seed = 100)


plot(fit_gbm)

h2o.varimp_plot(fit_gbm)

perf<-h2o.performance(fit_gbm, test)

perf

f1_(
   ( test$open_channels %>% as.data.frame())$open_channels%>% as.numeric() -1 ,
    (predict(fit_gbm, newdata= test)$predict %>% as.data.frame())$predict %>% as.numeric() -1
    )

#h2o.F1(perf)

gc()
res = predict(fit_gbm, newdata= tst)$predict %>% as.data.frame()

res=res$predict %>% as.numeric()-1


answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'fit_gbm 90000.csv'))








h2o.shutdown(prompt = FALSE)
