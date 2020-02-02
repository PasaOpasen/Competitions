

sm=0
count.probs=1e3
count.votes=1e4

for(t in seq(count.probs)){
  
for(i in seq(count.votes)){
  if(sum(rbinom(50,1,0.55))/50>=0.5){
    pos=pos+1
  }else{
    neg=neg+1
  }
}
sm=sm+pos/(pos+neg)
}

cat(sm/count.probs)


p=0.55#вероятность 55%
sm=0#искомая сумма
for(i in 26:50){#проверка для случаев от 26 до 50
  sm=sm+choose(50,i)*p^i*(1-p)^(50-i)
}
sm=sm+choose(50,25)*p^25*(1-p)^25/2#равновероятный ответ для 25
cat(sm)
