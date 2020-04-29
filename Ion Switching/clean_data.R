
library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

test.dir=paste0(path.dir,'test_clean_.csv')
train.dir=paste0(path.dir,'train_clean_.csv')


train=read_csv(train.dir)
test=read_csv(test.dir)

train %<>% mutate(open_channels=factor(open_channels))


train$batches<- as.factor((train$time - 0.0001)%/%50)
train$time_batch <- ((train$time - 0.0001)%%50)+0.0001

test$batches<- factor((test$time - 0.0001)%/%10-50)
test$time_batch <- ((test$time - 0.0001)%%10)+0.0001


# simple plots

g=ggplot(train %>% 
           group_by(open_channels) %>% 
           slice(sample(1:n(),min(2000,n()))),
         aes(x=time,y=signal,col=open_channels))+
  geom_point(alpha=0.8)+
  theme_bw()
g

g=ggplot(train %>% 
         group_by(open_channels) %>% 
         slice(sample(1:n(),min(2000,n()))),
       aes(x=time_batch,y=signal,col=open_channels))+
  geom_point(alpha=0.8)+
  theme_bw()+facet_wrap(.~batches)
ggsave('new clean train by 10s batch',g)

g=ggplot(test %>% 
           group_by(batches) %>% 
           slice(sample(1:n(),min(2000,n()))),
         aes(x=time,y=signal))+
  geom_point(alpha=0.8)+
  theme_bw()
g

g=ggplot(test %>% 
         group_by(batches) %>% 
         slice(sample(1:n(),min(1200,n()))),
       aes(x=time_batch,y=signal))+
  geom_point(alpha=0.8)+
  theme_bw()+facet_wrap(.~batches)

ggsave('new clean test by 10s batch',g)


# count of points per batch (500000 for train batch and for test if test batch is 50s)

train %>% group_by(batches) %>% summarise(count=n())

test %>% group_by(batches) %>% summarise(count=n())


# my new predictors

train %<>% 
  group_by(batches) %>% 
  mutate(
    mean=mean(signal),
    iqr=IQR(signal),
    l005=predict(loess(signal~time_batch,span = 0.05))
    )

train %>% 
  group_by(batches) %>% 
  summarise(
    mean=mean(signal),
    iqr=IQR(signal)
    )

summary(train)

ggplot(train %>% filter(batches==3) %>% 
     slice(seq(1,n(),by=1000)),
    aes(x=time_batch,y=signal))+
  geom_line(col='grey')+
  geom_point(aes(col=open_channels))+
  geom_line(aes(y=l005))+
  theme_bw()



train %<>% 
  group_by(batches) %>% 
  mutate(
    mean50=rollmean(signal,50),
    mean100=rollmean(signal,100),
    iqr100=rollIQR(signal,100),
    min50=rollmin(signal,50),
    max50=rollmax(signal,50),
    range50=max50-min50
  )



test %<>% 
  group_by(batches) %>% 
  mutate(
    mean=mean(signal),
    iqr=IQR(signal),
    l005=predict(loess(signal~time_batch,span = 0.05)),
    mean50=rollmean(signal,50),
    mean100=rollmean(signal,100),
    iqr100=rollIQR(signal,100),
    min50=rollmin(signal,50),
    max50=rollmax(signal,50),
    range50=max50-min50
  )

test %>% 
  group_by(batches) %>% 
  summarise(
    mean=mean(signal),
    iqr=IQR(signal)
  )


train %<>% mutate(level=ifelse(iqr>2,1,0)) 
test %<>% mutate(level=ifelse(iqr>2,1,0)) 








write_csv(train,path=paste0(path.dir,'train_last.csv')) 
write_csv(test,path=paste0(path.dir,'test_last.csv')) 















