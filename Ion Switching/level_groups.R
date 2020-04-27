
library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

trn=read_csv(paste0(path.dir,'train_clean.csv'))
trn %<>% mutate(open_channels=factor(open_channels))


ggplot(trn %>% group_by(open_channels,batches) %>% slice(sample(1:n(),min(600,n()))),aes(x=time_batch,y=signal,col=open_channels))+geom_point()+
  facet_wrap(batches~.)+
  #facet_wrap(batches~.,scales = 'free')+
  theme_bw()



test.dir=paste0(path.dir,'newtest_pca.csv')
tst=read_csv(test.dir)


ggplot(tst %>% group_by(batches)%>% slice(sample(1:n(),min(4000,n()))),aes(x=time_batch,y=signal))+geom_point()+
  facet_wrap(batches~.)+
  #facet_wrap(batches~.,scales = 'free')+
  theme_bw()



trn %<>% mutate(level_type=ifelse(batches %in% c(4,9),1,2) %>% factor()) 
tst %<>% mutate(level_type=ifelse(batches==1 & (time_batch<=10 |(time_batch>20 & time_batch<=30) ),1,2) %>% factor()) 


ggplot(trn %>% group_by(open_channels,level_type) %>% slice(sample(1:n(),min(1000,n()))),aes(x=time_batch,y=signal,col=open_channels))+geom_point()+
  facet_wrap(level_type~.)+
  #facet_wrap(batches~.,scales = 'free')+
  theme_bw()



ggplot(tst %>% group_by(level_type)%>% slice(sample(1:n(),min(4000,n()))),aes(x=time_batch,y=signal))+geom_point()+
  facet_wrap(level_type~.)+
  #facet_wrap(batches~.,scales = 'free')+
  theme_bw()






# нужно узнать, какие более репрезентативные в плане медианы (по количеству)
trn %>% group_by(level_type, open_channels) %>% summarise(count = n())

l1_5=trn %>% filter(open_channels==5 & level_type==1) %>% select(signal) %$% signal
l2_5=trn %>% filter(open_channels==5& level_type==2) %>% select(signal) %$% signal

t.test(l1_5)

t.test(l2_5)

m1=mean(l1_5)
m2=mean(l2_5)


trn %<>% mutate(supersignal=ifelse(level_type==2,signal, signal+(m2-m1))) 
tst %<>% mutate(supersignal=ifelse(level_type==2,signal, signal+(m2-m1))) 



ggplot(trn %>% group_by(open_channels,level_type) %>% slice(sample(1:n(),min(1000,n()))),
       aes(x=time_batch,y=supersignal,col=open_channels))+geom_point()+
  facet_wrap(level_type~.)+
  #facet_wrap(batches~.,scales = 'free')+
  theme_bw()


ggplot(tst %>% group_by(level_type)%>% slice(sample(1:n(),min(4000,n()))),
       aes(x=time_batch,y=supersignal))+geom_point()+
  facet_wrap(level_type~.)+
  #facet_wrap(batches~.,scales = 'free')+
  theme_bw()





write_csv(trn,paste0(path.dir,'trainsuper.csv'))
write_csv(tst,paste0(path.dir,'testsuper.csv'))




