
library(tidyverse)
library(magrittr)
library(gganimate)

dt= read_csv('train for py.csv')

dt %<>% select(signal, open_channels, batches) %>% filter(batches == 4 | batches ==9) %>% 
  mutate(batches=factor(batches),  chan = open_channels, open_channels = factor(open_channels)) %>% 
  group_by(batches) %>% mutate(time = seq(0.0001,50,length.out = n()))


dt %>% filter(batches == 4) %>% slice(1:500) %>% 
ggplot(
       aes(x=time, y=signal))+
  geom_line()+
  geom_point(aes(col=open_channels))+ theme_bw()+
  labs(title  = 'begin vers')+
  transition_reveal(time)

anim_save("firstan.gif")






data=dt %>% filter(batches == 4) %>% slice(1:500) 

# library
library(rgl)

# Add a new column with color
mycolors <- c('royalblue1', 'darkcyan', 'oldlace','green','red','black','blue','pink','violet','brown','grey')
data$color <- mycolors[ data$chan + 1 ]

# Plot
par(mar=c(0,0,0,0))
plot3d( 
  x=data$time, y=data$signal, z=abs(fft(data$signal)), 
  col = data$color, 
  type = 's', 
  #radius = .1,
  xlab="time", ylab="signal", zlab="func")



library(signal)

ch <- cheby2(5, 20, 0.5) 
z <- filter(ch, data$signal)
t=data$time
x=data$signal

plot(t, x, type = "l")
lines(t, z, col = "red")



data %>% mutate(z=as.numeric(z)) %>% 
  ggplot(
    aes(x=time, y=z))+
  geom_line()+
  geom_point(aes(col=open_channels))+ theme_bw()



data %>% mutate(z=abs(fft(signal))) %>% slice(2:300) %>% 
  ggplot(
    aes(x=time, y=z))+
  geom_line()+
  geom_point(aes(col=open_channels))+ theme_bw()

