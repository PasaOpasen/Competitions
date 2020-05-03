
library(tidyverse)
path.dir='./ignore_data/'


answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=read_csv('best py result.csv')[[2]]

write_csv(answer,paste0(path.dir,'bestpy.csv'))








