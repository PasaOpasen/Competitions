

Names=c("a","b","c","d","e")
x=c(4,3,1,-1,0)
y=c(2,2,-1,1,4)

df=data.frame(Names,x,y)

df$Names[(df$x-1)^2+(df$y-2)^2<=2.5^2]



library(DescTools)
x <- c(0,2,2,5,3,-1,-1,-4, -6,-5)
ref <- c(1,2,3,4,5,-1,-2,-3,-4,-5)
MAE( x, ref)
MAPE( x, ref)



df=data.frame(y=c(0,1,0,3),x=c(0,1,2,3))
lm(y~x,df)$coefficients


#этот цикл работает почти в 2 раза быстрее, чем то же самое в numpy через n.dot(n)
library(tictoc)
tic("speed of sum of squares of seq from 1 to 1000")
for(i in 1:10000){sum((1:1000)^2)}
t=toc()
t$toc/10000


















