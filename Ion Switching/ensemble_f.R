


ensemble.predict=function(fits,coefs,df){
  result=predict(fits[[1]], df, type='prob')*coefs[1]
  
  for(i in 2:length(coefs)){
    result=result+predict(fits[[i]],df, type='prob')*coefs[i]
  }
  return(max.col(result,ties.method = 'first')-1)
}




# test lda ensemble ####

trn=read_csv(paste0(path.dir,'train_clean.csv'))
trn %<>% mutate(open_channels=factor(open_channels)) 

als=1:nrow(trn)

# test inds
id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),3000)]) #+50*i
}

test.inds=id


# models

models=c('lda2','rf','sda')

fits=list()
scores=numeric()

for(k in 3:length(models)){
  
  id=numeric()
  for(i in 0:10){
    s= als[trn$open_channels[-test.inds]==i]
    id= c(id,s[sample(1:length(s),3000)]) #+50*i
  }
  
  fits[[k]]=train(open_channels~PC1+PC2+PC3+PC4+PC5+PC6+PC7+signal+signal2+
                  sin(signal)+cos(signal)+
                  sin(PC1)+cos(PC1)+
                  sin(2*signal)+cos(2*signal)
                ,
                data=trn[id,],
                method=models[k],
                #family="binomial", 
                trControl=control,
                #verbosity=T,
                metric="F1")
  
  
  scores[k]=f1_(trn$open_channels[test.inds],predict( fits[[k]],trn[test.inds,]))
  
  scores[k] %>% print()
  
}


scores
summary(scores)


f1_(
  trn$open_channels[test.inds],
  ensemble.predict(fits,rep(1,15),trn[test.inds,])
  )


f1_(
  trn$open_channels[test.inds],
  ensemble.predict(fits,scores,trn[test.inds,])
)

f1_(
  trn$open_channels[test.inds],
  ensemble.predict(fits,scores-0.924,trn[test.inds,])
)


# test probs in predict.train ####


lda.fit=train(open_channels~PC1+PC2+PC3+PC4+PC5+PC6+PC7+signal+signal2+
                sin(signal)+cos(signal)+
                sin(PC1)+cos(PC1)+
                sin(2*signal)+cos(2*signal)
              ,
              data=train,
              method="lda",
              family="binomial", 
              trControl=control,
              verbosity=T,
              metric="F1")

accshow(lda.fit,test)


pred=predict(lda.fit,test,type = 'prob')
pred=pred[sample(1:nrow(pred),50),]


which.max(pred[1:2,])


a=apply(pred, 1, function(x) which.max(x))
b=predict(lda.fit,test)

sum(a-1==b)


a=max.col(pred,ties.method = 'first')


system.time(
  for(i in 1:100){
    apply(pred, 1, function(x) which.max(x))  
  }
)

system.time(
  for(i in 1:100){
  max.col(pred,ties.method = 'first')
  }
)









# first ensemble ####


id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),12000)]) #+50*i
}

lda2_=train(open_channels~PC1+PC2+PC3+PC4+PC5+PC6+PC7+signal+signal2+
                  sin(signal)+cos(signal)+
                  sin(PC1)+cos(PC1)+
                  sin(2*signal)+cos(2*signal)
                ,
                data=trn[id,],
                method='lda2',
                #family="binomial", 
                trControl=cv2,
                tuneGrid=expand.grid(dimen=6),
                #verbosity=T,
                metric="F1")

id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),12000)]) #+50*i
}

rf_=train(open_channels~PC1+PC2+PC3+PC4+PC5+PC6+PC7+signal+signal2+
             sin(signal)+cos(signal)+
             sin(PC1)+cos(PC1)+
             sin(2*signal)+cos(2*signal)
           ,
           data=trn[id,],
           method='rf',
           #family="binomial", 
           trControl=cv2,
           tuneGrid=expand.grid(mtry=2),
           #verbosity=T,
           metric="F1")


id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),12000)]) #+50*i
}

sda_=train(open_channels~PC1+PC2+PC3+PC4+PC5+PC6+PC7+signal+signal2+
           sin(signal)+cos(signal)+
           sin(PC1)+cos(PC1)+
           sin(2*signal)+cos(2*signal)
         ,
         data=trn[id,],
         method='sda',
         #family="binomial", 
         trControl=cv2,
         tuneGrid=expand.grid(lambda=0.5,diagonal=F),
         #verbosity=T,
         metric="F1")



res=ensemble.predict(list(lda2_,rf_,sda_),scores-0.924,tst)


answer=read_csv(paste0(path.dir,'sample_submission.csv'))

answer$time=format(answer$time,nsmall = 4)

answer$open_channels=res

write_csv(answer,paste0(path.dir,'lda2 rf sda count = 12000.csv'))


save(lda2_, rf_, sda_,file='lda2 rf sda count = 12000.csv')

