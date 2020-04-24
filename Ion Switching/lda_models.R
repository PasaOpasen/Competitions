library(doParallel)

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



models=c(
 # 'mda',
  #'pda',
  #'qda',
  #'rda',
  #'sda',
  #'Mlda',
  #'qda',
  'rmda',
  'smda',
  'slda'
)


#rs=data.frame(name="name",test_res=0,time=0)
for(ft in models){
  
  t=proc.time()
  
  cl <- makePSOCKcluster(6)
  registerDoParallel(cl)
  
  lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ #PC6+PC7+
                  sin(signal)+cos(signal)+
                  signal+
                  sin(2*signal)+cos(2*signal)+
                  #sinpi(signal)+
                  cospi(signal)+
                  #exp(signal)+
                  #exp(-signal^2)+
                  I(1/(1+signal^2))#+
                #time_batch
                ,
                data=train,
                method=ft,
                #family="binomial", 
                trControl=cv1,
                #verbosity=T,
                metric="F1")
  
  
  t=proc.time()-t
  stopCluster(cl)
  
  cat('calculating........\n')
  
  nw=data.frame(
    name=ft,
    test_res=f1_(obs=test$open_channels,pred=predict(lda.fit,newdata = test)),
    #big_res= f1_(obs=trn$open_channels,pred=predict(lda.fit,newdata = trn)),
    time=as.numeric(t)[3])
  
  rs=rbind(rs,nw) 
  
  print(rs)
}




