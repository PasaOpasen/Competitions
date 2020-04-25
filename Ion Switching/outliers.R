
colnames(trn)

ggplot(trn,aes(y=signal,x=open_channels))+
  geom_boxplot()+facet_grid(vars(factor(batches)))+theme_bw()












