

models=c(
  #'bagEarthGCV',
  #'cforest',
  #'xgbDART',
  #'evtree',
  #'
  #'randomGLM',
  #'
  #'ordinalRF',
  #'rfRules',
  #'RRF',
  #'gbm_h2o',
  #'smda',
  #'gaussprLinear',
  #'gaussprPoly',
 # 'gaussprRadial',
  #'ORFsvm',
  #'ordinalNet',
  #'smda',
 # 'ORFpls',
  #'ORFridge',
  #'ORFsvm',
  #'FH.GBML',
  #'protoclass',
  'kknn',
  'knn',
  'lvq',
  'pam',
  'ownn',
  'snn'
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
  
  v=f1(data.frame(obs=test$open_channels,pred=predict(lda.fit,newdata = test)))
  
  rs[[ft]]=list(fit=ft,result=v,time=t)
  print(rs)
}









