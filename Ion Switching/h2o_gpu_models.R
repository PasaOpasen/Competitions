library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

#trn=read_csv(paste0(path.dir,'train_clean.csv'))
#tst=read_csv(paste0(path.dir,'newtest_pca.csv'))
#tst %<>% mutate(signal2=sign(signal)*sqrt(abs(signal)))

trn=read_csv(paste0(path.dir,'trainsuper.csv'))
tst=read_csv(paste0(path.dir,'testsuper.csv'))

trn %<>% mutate(signal2=sign(supersignal)*sqrt(abs(supersignal))) %>% 
  select(-signal) %>% mutate(level_type=factor(level_type),open_channels=factor(open_channels))
tst %<>% mutate(signal2=sign(supersignal)*sqrt(abs(supersignal))) %>% 
  select(-signal)%>% mutate(level_type=factor(level_type,levels=levels(trn$level_type)))

als=1:nrow(trn)


#trn %>% group_by(factor(open_channels)) %>% summarise(count=n())


id_train=numeric()
id_test=numeric()
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),3000+50*i)]
  id2<-caret::createDataPartition(id,p = 0.8,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}


train<-trn[id_train,]
test<-trn[id_test,]


library(h2o4gpu)
library(reticulate)  # only needed if using a virtual Python environment
use_virtualenv("/home/ledell/venv/h2o4gpu")  # set this to the path of your venv


xtrain=train[,-c(1:3)]

model_enc <- h2o4gpu.elastic_net_classifier() %>% fit(xtrain, train$open_channels) 















