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





not needed
```{r, fig.width=10, fig.height=8}
coljack<- matrix(c("#33ccff","#33ffbb","#33ff33","#99ff33","#ff9933","#ff3333","#ff3399","#ff33ff","#7733ff","#3333ff","#ffff1a"), nrow=11)

```

*Engineer is with the highest duration of sleep on average* 
  <br><br><br><br><br><br><br><br><br>
  



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
 

  
  
  
  ```{r}
  
  
  sleepocdu<-aggregate( sleep_duration ~ occupation, data = sleep, FUN = mean) 
  sleepocdu<-cbind(sleepocdu,coljack)
  sleepocdu <- sleepocdu[order(-sleepocdu$sleep_duration),]
  print(sleepocdu)
  
  par(mar = c(5, 4, 4, 2))  
  
  bpsd <- barplot(
    sleepocdu$sleep_duration, col = sleepocdu$coljack, main = "Sleep duration based on Profession", axis.lty = 0, ylim = c(0, 10), ylab = "Sleep (Hours)", names.arg = sleepocdu$occupation, las=2
  )
  
  ``` 
  
  
  
  ```{r}

  #Male
  table(sleep_male$sleep_disorder)
  
  ```
  
  
  
  
  
  
  
  
  
  
  
  
  ```{r}
  sleepocqw_male<-aggregate( sleep_quality ~ occupation, data = sleep_male, FUN = mean)
  sleepocqw_male <- sleepocqw_male[order(-sleepocqw_male$sleep_quality),]
  print(sleepocqw_male)
  
  
  ```
  
  
  ```{r}
  sleepocqw_female<-aggregate( sleep_quality ~ occupation, data = sleep_female, FUN = mean)
  sleepocqw_female <- sleepocqw_female[order(-sleepocqw_female$sleep_quality),]
  print(sleepocqw_female)
  
  
  
  ```
  
  
  
  ```{r}
  

  #Female
  table(sleep_female$sleep_disorder)
  
  ```
  

  sleep_insomnia<-sleep[sleep$sleep_disorder %in% c("Insomnia"),]
  sleep_apnea<-sleep[sleep$sleep_disorder %in% c("Sleep Apnea"),]
  sleep_healthy<-sleep[sleep$sleep_disorder %in% c("None"),]
  
  
  
  
  
  sleep_insomnia<-sleep[sleep$sleep_disorder %in% c("Insomnia"),]
  summary(sleep_insomnia$age)
  par(mfrow=c(1,2),mar= c(5,1,5,1))
  
  boxplot(sleep_insomnia$age, horizontal = TRUE, main = "Age of people with Insomnia", col= "lightblue")
  
  
  
  
  sleep_apnea<-sleep[sleep$sleep_disorder %in% c("Sleep Apnea"),]
  print(summary(sleep_apnea$age))
  
  boxplot(sleep_apnea$age, horizontal = TRUE,main = "Age of people with Sleep Apnea", col= "lightgreen")
  
  
  
  
  
  ```{r}
  sleep_insomnia<-sleep[sleep$sleep_disorder %in% c("Insomnia"),]
  summary(sleep_insomnia$sleep_duration)
  par(mfrow=c(1,2),mar= c(5,1,5,1))
  
  boxplot(sleep_insomnia$sleep_duration, horizontal = TRUE, main = "Duration of sleep with Insomnia",col="lightblue")
  
  
  
  
  sleep_apnea<-sleep[sleep$sleep_disorder %in% c("Sleep Apnea"),]
  print(summary(sleep_apnea$sleep_duration))
  
  boxplot(sleep_apnea$sleep_duration, horizontal = TRUE,main = "Duration of sleep with Sleep Apnea", col= "lightgreen")
  
  
  
  ```
  
  
  
  ```{r}
  
  ci<-function(vec,alpha){
    n<-length(vec)
    sigma<-sd(vec)
    z<-abs(qnorm((1-alpha/2),0,1))
    xlow<-mean(vec)-z*(sigma/sqrt(n))
    xhigh<-mean(vec)+z*(sigma/sqrt(n))
    print(n)
    print(sigma)
    print(z)
    return(c(xlow,xhigh))
  }
  
  ```
  
  
  
  
  
  
  <style>
    hr {
      border: none;
      height: 2px;
      background-color: #000;
        opacity: 1;
    }
  </style>
    
  
