
library(pracma)
#выделение выбросов но критерию Шовене
Schovene=function(x){
  m=mean(x)
  s=sd(x)
  vec=erfc(abs(m-x)/s)<0.5/length(x)
  k=sum(vec)
  p=length(x)-k
  
  repeat{
    m=mean(x[!vec])
    s=sd(x[!vec])
    vc=erfc(abs(m-x)/s)<0.5/p
    #print(erfc(abs(m-x)/s))
    if(sum(vc)>k){
      vec=vc
      k=sum(vc)
      p=length(x)-k
    }else{
      break
    }
  }
  return(vec)
}

Schovene(c(8.02,8.16,3.97,8.64,0.84,4.46,0.81,7.74,8.78,9.26,20.46,29.87,10.38,25.71))




