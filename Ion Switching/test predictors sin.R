

#trn2=trn %>% mutate(sin=sin(signal),cos=cos(signal),sin2=sin(PC1))



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

train<-trn2[id_train,]
test<-trn2[id_test,]


set.seed(1998)
t=proc.time()

lda.fit=train(open_channels ~ PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+signal
              ,
              data=train, #%>% select(-sin,-cos,-sin2),
              method='rf',
              #family="binomial", 
              trControl=cv1,
              #verbosity=T,
              metric="F1")


t=proc.time()-t

cat('calculating........\n')

nw=data.frame(
  test_res=f1_(obs=test$open_channels,pred=predict(lda.fit,newdata = test)),
  time=as.numeric(t)[3])

print(nw)


# для rf
#
# сначала 0.9294919 38.98
# после закоментирования sin 0.9347708 27.48
# сначала + timebatch 0.9305652 41.03
# без всех левых, но с timebatch 0.9343903 29.78
# без всех левых, но с cos(PC1) 0.9334422 30.39
# без всех левых, но с sin(PC1) 0.9314119 29.25
# без всех левых, но с cospi(PC1) 0.9349154 29.53
#
#
# без всех левых и без signal 0.9163519 27.92
#
#
#
#
#



# signaleSqRo <- sign(DF$signal)*abs(DF$signal)^(1/2)
# preProcess = c('scale', 'center','YeoJohnson')
# preProcess = c('scale', 'center','BoxCox')
#
#




