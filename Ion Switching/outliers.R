
colnames(trn)

ggplot(trn,aes(y=signal))+
  geom_boxplot()+facet_wrap(factor(open_channels)~factor(batches),scales = 'free')+
  theme_bw()+coord_flip()


trn %<>% mutate(outliers=F)
als=1:nrow(trn)

for(lv in 0:1){
  for(ch in 0:10){
    id = als[trn$open_channels == ch & trn$level == lv] 
    
    if(length(id)==0){
      next
    }
    
    ind <- which(trn$signal[id] %in% boxplot.stats(trn$signal[id])$out)
    
    if(length(ind)==0){
      next
    }
    
    trn$outliers[id[ind]]=T
    
    cat('level = ',lv,' chan = ',ch,' len = ',length(ind),'\n')
  }
}
sum(trn$outliers)/nrow(trn)

trn %>% group_by(open_channels) %>% summarise(count=sum(outliers))


ggplot()+
  geom_point(data=trn %>% filter(outliers==F) %>% group_by(open_channels) %>% slice(sample(1:n(),600)),
       aes(y=signal,x=time_batch,col=open_channels))+
  geom_point(data=trn %>% filter(outliers==T)%>% group_by(open_channels)%>% slice(sample(1:n(),220)),
            aes(y=signal,x=time_batch,col=open_channels),shape=4,size=2)+
  facet_wrap(batches~.,scales = 'free')+
  theme_bw()+theme(legend.position = 'bottom')



ggplot()+
  geom_point(data=trn  %>% slice(sample(1:n(),8000)),
             aes(y=signal,x=time_batch,col=open_channels,shape=factor(outliers)))+
  facet_wrap(batches~.,scales = 'free')+
  theme_bw()+theme(legend.position = 'bottom')





write_csv(trn %>% filter(outliers==F) %>% select(-outliers),path=paste0(path.dir,'train_last_clean.csv'))


