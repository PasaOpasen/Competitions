library(tidyverse)
library(data.table)
library(magrittr)
library(caret)
library(MLmetrics)
library(doSNOW)


data.path='D:/liverpool-ion-switching/'

train.path=paste0(data.path,'train','.csv')
test.path=paste0(data.path,'test','.csv')
sample.path=paste0(data.path,'sample_submission','.csv')


dt=read_csv(train.path)

#dt %>% summary()

dt %<>% mutate(chan=factor(open_channels,ordered = T),chan2=factor(paste0('l',open_channels)))

#dt$chan %>% summary()


data=dt %>% group_by(chan) %>% slice(sample(1:n(),40))
ggplot(data,aes(x=time,y=signal,fill=chan))+geom_hex()+facet_grid(vars(chan),scales = "free")


#dt %>% slice(seq(1,n(),by=200)) %>% ggplot(aes(x=time,y=signal,col=chan))+geom_point()


#simple.logres=lm(open_channels~signal+signal:time,dt)
#simple.logres %>% summary()




calcF1Scores=function(act,prd){
    #treats the vectors like classes
    #act and prd must be whole numbers
    df=data.frame(act=as.numeric(act),prd=as.numeric(prd));
    scores=numeric(11);
    for(i in 0:10){
      
      tp=nrow(df[df$prd==i & df$act==i,]);        
      fp=nrow(df[df$prd==i & df$act!=i,]);
      fn=nrow(df[df$prd!=i & df$act==i,]);

      scores[i+1]=(2*tp)/(2*tp+fp+fn)
    }      
    return(mean(scores))
}
calcF1Scores2=function(act,prd){

  scores=numeric(11);
  lv=levels(prd)
  for(i in 0:10){
    ii=lv[i+1]
    p= prd==ii
    a= act==ii
    
    tp=sum(p & a);        
    fp=sum(p & !a);
    fn=sum(!p & a);
    
    scores[i+1]=(2*tp)/(2*tp+fp+fn)
  }      
  #print(scores)
  return(mean(scores,na.rm = T))
}

f1 <- function (data, lev = NULL, model = NULL) {

  answ=calcF1Scores2(data$obs,data$pred)
  names(answ)="F1"
 return(answ)

} 

tr=trainControl(
  method = 'cv',
  number=10,
  summaryFunction = f1,
  classProbs = T,
  verboseIter = T
  )


#d=seq(1,nrow(dt),by=300)


al=getModelInfo()
md=c()
for(m in names(al)){
  mod=al[[m]]
  if(mod$type[1]=="Classification" & !any(mod$tags=="Two Class Only")){
    md=c(md,m)
  }
}

results=data.frame(name="id",res=0,time=0)

for(f in md[4:8]){
  
  tryCatch({
  
  t=Sys.time()
  fit=train(x=data[c(1,2)],y=data$chan2,#preProcess = c("center", "scale"),
              method=f,
              trControl = tr,
              maximize = T,
              metric = 'F1')
  
  t=as.integer(Sys.time()-t)
  
  fit$resample$F1 %>% mean() %>% print()
  
  results=rbind(results,
    data.frame(
    name=f,
    res=calcF1Scores2(dt$chan2,fit %>% predict(dt)),
    time=t
    ))
  }, error = function(e) e)
}



fit.svm$resample$F1 %>% mean()

#system.time(
 calcF1Scores2(dt$chan2,fit.svm %>% predict(dt)) 
#)






test=read_csv(test.path)

res=fit.svm %>% predict(test) %>% str_sub(2) %>% as.numeric()




#writing_sample

answer=read_csv(sample.path)

answer$time=format(answer$time,nsmall = 4)
#answer$open_channels=sample(0:10,answer$time %>% length(),replace = T)
answer$open_channels=res


write_csv(answer,paste0(data.path,"result.csv"))















