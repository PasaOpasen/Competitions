
formas=c('PC1 + PC2 + PC3 + PC4 + PC5',
         'PC1 + PC2 + PC3 + PC4 + PC5 + PC6',
         'PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7',
         'PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+signal',
         'PC1 + PC2 + PC3 + PC4 + PC5+ PC6+PC7+sin(signal)')


vals=numeric()
times=numeric()

for(forma in formas){
  
  t=Sys.time()
  lda.fit=train(formula=as.formula(paste0('open_channels ~ ',forma)),
                data=train,
                method="lda",
                family="binomial", 
                trControl=control,
                verbosity=T,classProbs=F,
                metric="F1")
  
  t=Sys.time()-t
  times=c(times,t)
  
  v=f1(data.frame(obs=test$open_channels,pred=predict(lda.fit,newdata = test)))
  vals=c(vals,v)
}








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
              method="lda",
              family="binomial", 
              trControl=control,
              verbosity=T,
              metric="F1")

t=Sys.time()-t
times=c(times,t)

v=f1(data.frame(obs=test$open_channels,pred=predict(lda.fit,newdata = test)))
vals=c(vals,v)









