

library(tidyverse)
library(magrittr)

dt= read_csv('train for py.csv')

dt %<>% select(signal, open_channels, batches) %>% filter(batches == 4 | batches ==9) %>% 
  mutate(batches=factor(batches),  chan = open_channels, open_channels = factor(open_channels)) %>% 
  group_by(batches) %>% mutate(time = seq(0.0001,50,length.out = n()))



ggplot(dt %>% group_by(open_channels) %>% slice(sample(1:n(), min(n(),800)  )), 
       aes(x=time, y=signal,col=open_channels))+
  geom_point()+ theme_bw()+
  labs(title  = 'begin vers')


old = dt$signal

for(i in 1:9){
  a = old[dt$chan==i]
  b = old[dt$chan==i+1]
  
  q90 = quantile(a,0.8)
  q10 = quantile(b,0.2)
  
  old[dt$chan>i]=old[dt$chan>i]+(q10-q90)
}

dt$old=old


ggplot(dt %>% group_by(open_channels) %>% slice(sample(1:n(), min(n(),800)  )), 
       aes(x=time, y=old,col=open_channels))+
  geom_point()+theme_bw()+
  labs(title = 'old predictor')

summary(dt)

