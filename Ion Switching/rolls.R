

library(compiler)

roll=function(vector,f,window=100){
  
  len=length(vector)
  
  result=numeric(length = len)
  
  for(i in 1:(len-window+1)){
    result[i]=f(vector[i:(i+window-1)])
  }
  result[(len-window+2):len]=result[len-window+1]
  
  return(result)
  
}

roll.quantile=function(vector,per=0.5,window=100){
  
  len=length(vector)
  
  result=numeric(length = len)
  
  for(i in 1:(len-window+1)){
    result[i]=as.numeric(quantile(vector[i:(i+window-1)], probs = per)) 
  }
  result[(len-window+2):len]=result[len-window+1]
  
  return(result)
  
}


rollmean_=function(vector,window=100){
  
  
  len=length(vector)
  
  result=numeric(length = len)
  
  for(i in 1:(len-window+1)){
    result[i]=mean(vector[i:(i+window-1)])
  }
  result[(len-window+2):len]=result[len-window+1]
  
  return(result)
  
}

rollIQR_=function(vector,window=100){
  
  
  len=length(vector)
  
  result=numeric(length = len)
  
  for(i in 1:(len-window+1)){
    result[i]=IQR(vector[i:(i+window-1)])
  }
  result[(len-window+2):len]=result[len-window+1]
  
  return(result)
  
}


rollmin_=function(vector,window=100){
  
  
  len=length(vector)
  
  result=numeric(length = len)
  
  for(i in 1:(len-window+1)){
    result[i]=min(vector[i:(i+window-1)])
  }
  result[(len-window+2):len]=result[len-window+1]
  
  return(result)
  
}

rollmax_=function(vector,window=100){
  
  
  len=length(vector)
  
  result=numeric(length = len)
  
  for(i in 1:(len-window+1)){
    result[i]=max(vector[i:(i+window-1)])
  }
  result[(len-window+2):len]=result[len-window+1]
  
  return(result)
  
}



rollmean=cmpfun(rollmean_)
rollIQR=cmpfun(rollIQR_)
rollmin=cmpfun(rollmin_)
rollmax=cmpfun(rollmax_)
rollq=cmpfun(roll.quantile)
