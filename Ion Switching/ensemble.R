


rs=read_csv("best models 6600 train 15400 test.csv")

fits=rs %>% filter(test_res>0.927) %>% select(name)

fits=fits$name




id_train=numeric()
id_test=numeric()
als=1:5000000
for(i in 0:10){
  id= als[trn$open_channels==i]
  id=id[sample(1:length(id),1500)]
  id2<-createDataPartition(id,p = 0.3,list = F)[,1]
  id_train=c(id_train,id[id2])
  id_test=c(id_test, id[-id2])
}


train<-trn[id_train,]
test<-trn[id_test,]


ensemble=list()
info=list()
ct=1

for(ft in fits){
  
  t=proc.time()
  ensemble[[ct]]=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ #PC6+PC7+
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
                tuneLength=5,
                #verbosity=T,
                metric="F1")
  
  t=proc.time()-t
  
  cat('calculating........\n')
  
  info[[ct]]=list(
    name=ft,
    test_res=f1_(obs=test$open_channels,pred=predict(ensemble[[ct]],newdata = test)),
    time=as.numeric(t)[3],
    tune=ensemble[[ct]]$bestTune
    )
  
  print(info)
  
  ct=ct+1
}









