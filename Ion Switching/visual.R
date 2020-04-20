library(tidyverse)
library(data.table)
library(magrittr)

data.path='D:/liverpool-ion-switching/'

train.path=paste0(data.path,'train','.csv')
test.path=paste0(data.path,'test','.csv')
sample.path=paste0(data.path,'sample_submission','.csv')

dt=read_csv(train.path)

dt %>% summary()

dt$open_channels %>% factor() %>% summary()



dt %<>% mutate(chan=factor(open_channels,ordered = T))


dt %>% slice(seq(1,n(),by=500)) %>% ggplot(aes(x=time,y=signal,col=chan))+geom_point()


simple.logres=glm(chan~signal,dt,family =gaussian )



test=read_csv(test.path)





answer=read_csv(sample.path)
answer$open_channels=sample(1:10,answer$time %>% length(),replace = T)
write_csv(answer,"result.csv")





















