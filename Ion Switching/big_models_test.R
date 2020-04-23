
id_train=numeric()
id_test=numeric()
als=1:5000000
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),200)]
  id2<-createDataPartition(id,p = 0.8,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}

train<-trn[id_train,]
test<-trn[id_test,]





models=c(
 'lda'#,  just for comparation 0.938 0.914
  #'xgbDART',
  #'evtree',
  #'
  #'randomGLM',
  #'
  #'ordinalRF',
  #'rfRules',
  #'RRF', 0.9273437  0.9172741
  #'gbm_h2o',
 #'smda',
 # 'gaussprLinear',
  #'gaussprPoly',
   #'gaussprRadial',
  #'ORFsvm',
  #'ordinalNet',
  #'smda',
   #'ORFpls',
  #'ORFridge',
  #'FH.GBML',
  #'snn'
)


rs=list()
for(ft in models){
  
  t=Sys.time()
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
                trControl=control,
                #verbosity=T,
                metric="F1")
  
  t=Sys.time()-t
  
  v=f1_(obs=test$open_channels,pred=predict(lda.fit,newdata = test))
  
  rs[[ft]]=list(fit=ft,
                result=v,
                big_result=f1_(obs=trn$open_channels,pred=predict(lda.fit,newdata = trn)),
                time=t)
  print(rs)
}




