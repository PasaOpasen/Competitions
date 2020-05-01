


ggplot(trn %>% filter(batches==4) %>% group_by(factor(open_channels)) %>% 
         slice(sample(1:n(),min(1000,n()))),
       aes(x=time_batch, y=signal))+
  geom_point(aes(col=factor(open_channels)))+
  geom_line(aes(y=rollq(signal,0.95,3000)))+
  theme_bw()




