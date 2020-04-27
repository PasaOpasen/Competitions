
library(tidyverse)
library(magrittr)

path.dir='./ignore_data/'

trn=read_csv(paste0(path.dir,'train_clean.csv'))
trn %<>% mutate(open_channels=factor(open_channels))


separator.show=function(f){
  
  ggplot(trn %>% group_by(open_channels) %>% slice(sample(1:n(),800)),aes(x=time_batch,y=f(signal),col=open_channels))+geom_point()+
    facet_wrap(batches~.)+
    #facet_wrap(batches~.,scales = 'free')+
    theme_bw()
}


separator.show(function(x) x)

separator.show(function(x) sin(x))

separator.show(function(x) sinh(x))

separator.show(function(x) cosh(x))


separator.show(function(x) x^3)


separator.show(function(x) x^3+x^2)






















