
library(tidyverse)
library(magrittr)



dt= read_csv('train for py.csv')

dt %<>% select(signal, open_channels, batches) %>% filter(batches == 4 | batches ==9) %>% 
  mutate(batches=factor(batches),  chan = open_channels, open_channels = factor(open_channels)) %>% 
  group_by(batches) %>% mutate(time = seq(0.0001,50,length.out = n()))


f=dt$signal[1]

dt %>% filter(batches == 4) %>% slice(1:500) %>% 
  mutate(s = cumsum(signal-f)) %>% 
  ggplot(
    aes(x=signal, y=s))+
  geom_point(aes(col=open_channels))+ 
  theme_bw()







