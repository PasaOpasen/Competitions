
library(tidyverse)
library(magrittr)
library(zeallot)

path.dir='./ignore_data/'


trn=read_csv(paste0(path.dir,'trainsuper.csv'))
tst=read_csv(paste0(path.dir,'testsuper.csv'))




# train dataset ####

trn %<>% filter(level_type==1, open_channels>0) %>% 
  select(-supersignal,-level1,-level2,-level_type) %>% 
  mutate(
    open_channels=factor(open_channels),
    batches=factor(batches),
    signal2=signal*signal
    ) %>% 
  group_by(batches) %>% 
  mutate(
    time_batch=time_batch/50,
    q5=rollq(signal,0.05,100),
    q15=rollq(signal,0.15,100),
    q25=rollq(signal,0.25,100),
    q35=rollq(signal,0.35,100),
    q45=rollq(signal,0.45,100),
    q55=rollq(signal,0.55,100),
    q65=rollq(signal,0.65,100),
    q75=rollq(signal,0.75,100),
    q85=rollq(signal,0.85,100),
    q95=rollq(signal,0.95,100)
    )


summary(trn)

write_csv(trn,paste0(path.dir,'train_bad.csv'))


ggplot(trn %>% group_by(open_channels) %>% slice(sample(1:n(),min(n(),800)  )),
       aes(x=time_batch,y=signal))+
  geom_point(aes(col=open_channels,shape=batches),alpha=0.8)+
  theme_bw()


ggplot(trn %>% group_by(open_channels) %>% slice(sample(1:n(),min(n(),800)  )),
       aes(x=time_batch,y=signal))+
  geom_point(aes(col=open_channels,shape=batches),alpha=0.8)+
  theme_bw()+
  geom_line(aes(y=q55),col='blue')+
  geom_line(aes(y=q25),col='grey')+
  geom_line(aes(y=q5),col='red')


ggplot(trn %>% group_by(factor(open_channels)) %>% slice(sample(1:n(),min(n(),800)  )),
       aes(x=signal2,y=signal))+
  geom_point(aes(col=factor(open_channels) ,shape=factor(batches)),alpha=0.8)+
  theme_bw()

# test dataset ####


tst$time=seq(500.0001,700,length.out = 2e6)
tst$batches<- factor((tst$time - 0.0001)%/%10-50)
tst$time_batch <- ((tst$time - 0.0001)%%10)+0.0001
tst$level_type=ifelse( tst$batches== 6-1 | tst$batches== 8-1,1,2) %>% factor()

ggplot(tst %>% group_by(batches) %>% slice(sample(1:n(),min(n(),800)  )),
       aes(x=time_batch,y=signal))+
  geom_point(aes(shape=level_type),alpha=0.8)+
  facet_wrap(.~batches)+
  theme_bw()


levs=as.numeric(tst$level_type)
save(levs,file='levels for test.rdata')

tst %>% group_by(batches, level_type) %>% summarise(count=n())


tst %<>% filter(level_type==1) %>% 
  select(-supersignal,-level1,-level2,-level_type,-time) %>% 
  mutate(
    batches=factor(batches),
    signal2=signal*signal
  ) %>% 
  mutate(
    time_batch=time_batch/10,
    q5=rollq(signal,0.05,100),
    q15=rollq(signal,0.15,100),
    q25=rollq(signal,0.25,100),
    q35=rollq(signal,0.35,100),
    q45=rollq(signal,0.45,100),
    q55=rollq(signal,0.55,100),
    q65=rollq(signal,0.65,100),
    q75=rollq(signal,0.75,100),
    q85=rollq(signal,0.85,100),
    q95=rollq(signal,0.95,100)
  )

summary(tst)

write_csv(tst,paste0(path.dir,'test_bad.csv'))











