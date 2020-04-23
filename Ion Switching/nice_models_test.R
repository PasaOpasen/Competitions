

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
# 'lda',#  just for comparation 
# 'lda2',
#'treebag',
#  'C5.0',
#  'rpart1SE',
#  'ctree',
#  'xgbTree',
#  'multinom',
 # 'ranger',
 # 'C5.0Rules',
 # 'C5.0Tree',
  #'gbm',
  #'parRF',
  #'rFerns',
  'RRFglobal',
  'wsrf',
  'naive_bayes',
  'hdda',
  'loclda',
  'mda',
  'pda',
  'qda',
  'rda',
  'sda',
  'lssvmRadial',
  'svmLinear',
  'svmLinear2',
  'svmPoly',
  'svmRadial',
  'svmRadialCost',
  'svmRadialSigma',
  'PART',
  'JRip'
)


#rs=data.frame(name="name",test_res=0,time=0)
for(ft in models){
  
  t=proc.time()
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
  
  t=proc.time()-t
  
  cat('calculating........\n')
  
  nw=data.frame(
    name=ft,
    test_res=f1_(obs=test$open_channels,pred=predict(lda.fit,newdata = test)),
    #big_res= f1_(obs=trn$open_channels,pred=predict(lda.fit,newdata = trn)),
    time=as.numeric(t)[3])
  
  rs=rbind(rs,nw) 
 
  print(rs)
}

rs=rs[2:35,] %>% tbl_df() %>% arrange(-test_res,-time)

write_csv(rs,"best models 6600 train 15400 test.csv")


rs %>% filter(test_res>0.927)







id=numeric()
for(i in 0:10){
  s= als[trn$open_channels==i]
  id= c(id,s[sample(1:length(s),20000)]) 
}

lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+
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
              #data=train,
              data=trn[id,],
              #data=trn,
              method='svmLinear',
              #family="binomial", 
              #tuneGrid=expand.grid(subclasses = 3),
              #trControl=control,
              trControl=trainControl(method = 'none',verboseIter = T),
              #verbosity=T,
              metric="F1")





