library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

trn=read_csv(paste0(path.dir,'train_best.csv'))


trn %<>% mutate(open_channels=factor(open_channels)) 



ggplot(trn %>% filter(batches==4 | batches==9) %>% 
         group_by(open_channels) %>% slice(sample(1:n(),min(1500,n()))))+
  geom_point(aes(x=time_batch, y=signal, col= open_channels))+
  theme_bw()


ggplot(trn %>% filter(batches==4 | batches==9) %>% 
         group_by(open_channels) %>% slice(sample(1:n(),min(1500,n()))))+
  geom_point(aes(x=PC7*PC2, y=signal, col= open_channels))+
  theme_bw()

ggplot(trn %>% filter(batches==4 | batches==9) %>% 
         group_by(open_channels) %>% slice(sample(1:n(),min(1500,n()))))+
  geom_point(aes(x=time_batch*signal, y=signal, col= open_channels))+
  theme_bw()


ggplot(trn %>% filter(batches==4 | batches==9) %>% 
         group_by(open_channels) %>% slice(sample(1:n(),min(1500,n()))))+
  geom_point(aes(x=time_batch, y=signal, col= open_channels))+
  geom_line(aes(x=time_batch, y= rollmin(signal,10000)),color='grey')+
  theme_bw()






ggplot(trn %>% filter(batches==4 | batches==9) %>% 
         group_by(open_channels) %>% slice(sample(1:n(),min(1500,n()))))+
  geom_point(aes(x=time_batch, y=signal, col= open_channels))+
  geom_line(aes(x=time_batch, y= predict(loess(signal~time_batch,span = 0.1)) ),color='black',size=2)+
  theme_bw()


ggplot(trn %>% filter(batches==4 | batches==9)  %>% 
         group_by(open_channels) %>% slice(sample(1:n(),min(1500,n()))))+
  geom_point(aes(x=predict(loess(signal~time_batch,span = 0.01)), y=signal, col= open_channels),size=2,alpha=0.9)+
  theme_bw()







ggplot(trn %>% filter(batches==4 | batches==9)  %>% mutate(step=signal-c(0,0,signal[1:(n()-2)])) %>% 
         group_by(open_channels) %>% slice(sample(1:n(),min(1500,n()))))+
  geom_point(aes(x=step, y=signal, col= open_channels),size=2,alpha=0.9)+
  theme_bw()





















