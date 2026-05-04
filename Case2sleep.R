sleep<-read.csv("sleep.csv")
View(read.csv("sleep.csv"))

sample_size<-length(sleep$gender)
sample_size
dim(sleep)
summary(sleep)
boxplot(sleep$age,horizontal =TRUE,col="lightblue",main="Age of sample")
?boxplot
plot.default(sleep$sleep_duration)
unique(sleep$age)
hist(sleep$age, freq=TRUE,col="lightgreen")
?hist
seq(0,65,5)




sleepocdu<-aggregate( sleep_duration ~ occupation, data = sleep, FUN = mean)

sleepocdu <- sleepocdu[order(-sleepocdu$sleep_duration),]


unique(sleep$occupation)
unique(sleep$sleep_disorder)
sort(unique(sleep$sleep_quality))


#names(sort(-table(sleep$sleep_quality)))[c(0:6)]

boxplot(sleep$age,horizontal =TRUE,col="lightblue",main="Age of sample")



sleepocdu<-aggregate( sleep_duration ~ occupation, data = sleep, FUN = mean)
sleepocdu <- sleepocdu[order(-sleepocdu$sleep_),]
print(sleepocdu)



c("#660033","#990000", "#ff6666", "#cc3300",  "#0000ff", "#336699","#006600" ,"#00994d", "#006666" ,  "#996633" , "#cc33ff")




table(sleep$sleep_disorder)

aggregate( sleep_duration ~ gender, data = sleep, FUN = mean)
  
  
  
c("#0099ff", "#00ff00","#ffcc00","#ffa366","#ff66a3","#cc66ff","#cc66ff", "#ffb366","#66ff66","#e085c2", "#0099ff",)
  


coljack<-c("#33ccff",  "#33ffbb", "#33ff33","#99ff33","#ff9933","#ff3333","#ff3399","#ff33ff","#7733ff","#3333ff","#ffff1a")
matrix(coljack, nrow=11)






text(
  x = bpsd, y = sleepocdu$sleep_duration + 0.3, labels = sleepocdu$occupation,
  srt = 90,adj = 0,cex = 1


  
  ggplot(
    sleepocdu, 
    aes(x= occupation, y= sleep_duration)) +
    geom_bar(stat= "identity"  ) + theme()
  





?cor
 



