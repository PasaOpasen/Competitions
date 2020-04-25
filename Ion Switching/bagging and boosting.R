



cv1=trainControl(method="cv",number=5,summaryFunction = f1,verboseIter = T)
cv2=trainControl(method = 'none',verboseIter = T)

id_train=numeric()
id_test=numeric()
als=1:5000000
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),1000)]
  id2<-createDataPartition(id,p = 0.3,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}

train<-trn[id_train,]
test<-trn[id_test,]



models=c(
#'treebag',
#'bagFDA',
#'cforest',
#'parRF',
#'rFerns',
#'ordinalRF',
#'ranger',
#'Rborist',
#'rf',
#'extraTrees',
#'RRF',
#'wsrf',
#'bstTree',
#'C5.0',
#'xgbTree',
#'xgbDART',
#'xgbLinear',
#'gbm',
#'pam',
#'kernelpls',
)


#rs=data.frame(name="name",test_res=0,time=0)
for(ft in models){
  
  t=proc.time()
  
  lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+signal,
                data=train,
                method=ft,
                #family="binomial", 
                trControl=cv1,
                #verbosity=T,
                metric="F1")
  
  
  t=proc.time()-t
  
  cat('calculating........\n')
  
  nw=data.frame(
    name=ft,
    test_res=f1_(obs=test$open_channels,pred=predict(lda.fit,newdata = test)),
    time=as.numeric(t)[3])
  
  rs=rbind(rs,nw) 
  
  print(rs)
}





rs=rs %>% tbl_df() %>% arrange(-test_res)


#second generation####


id_train=numeric()
id_test=numeric()
als=1:5000000
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),2000)]
  id2<-createDataPartition(id,p = 0.3,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}

train<-trn[id_train,]
test<-trn[id_test,]

models=rs %>% filter(time<2000 & test_res>0.925) %>% select(name) %>% mutate(name=as.character(name))
models=models$name


#rs=data.frame(name="name",test_res=0,time=0)
for(ft in models){
  
  t=proc.time()
  
  lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+signal,
                data=train,
                method=ft,
                #family="binomial", 
                trControl=cv1,
                #verbosity=T,
                metric="F1")
  
  
  t=proc.time()-t
  
  cat('calculating........\n')
  
  nw=data.frame(
    name=ft,
    test_res=f1_(obs=test$open_channels,pred=predict(lda.fit,newdata = test)),
    time=as.numeric(t)[3])
  
  rs=rbind(rs,nw) 
  
  print(rs)
}



rs=rs %>% tbl_df() %>% arrange(-test_res)



#trird generation####


id_train=numeric()
id_test=numeric()
als=1:5000000
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),6000)]
  id2<-createDataPartition(id,p = 0.3,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}

train<-trn[id_train,]
test<-trn[id_test,]

models=rs %>% filter( test_res>0.935) %>% select(name) %>% mutate(name=as.character(name))
models=models$name


#rs=data.frame(name="name",test_res=0,time=0)
for(ft in models){
  
  t=proc.time()
  
  lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+signal,
                data=train,
                method=ft,
                #family="binomial", 
                trControl=cv1,
                #verbosity=T,
                metric="F1")
  
  
  t=proc.time()-t
  
  cat('calculating........\n')
  
  nw=data.frame(
    name=ft,
    test_res=f1_(obs=test$open_channels,pred=predict(lda.fit,newdata = test)),
    time=as.numeric(t)[3])
  
  rs=rbind(rs,nw) 
  
  print(rs)
}





