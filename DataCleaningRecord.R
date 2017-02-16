data <- data2[!(data2$Subject %in% c(11, 42, 77, 43)),] #43 removed for >95% same response, others for notes in binder

data<- data[-7813,]

data$RT <- as.numeric(data$RT)
data$Response <- as.numeric(data$Response)
data$Answer <- as.numeric(data$Answer)

data <- data[data$RT<mean(data$RT)+5*sd(data$RT),]

data <- data[data$Experiment.Stage==1,]

k <- 0
for(i in unique(data$Subject)){
  test <- sum(data$Response[data$Subject==i])/length(data$Response[data$Subject==i])
  
  if(test<0.05 | test>0.95){
    k <- k+1
    print(i)
    }
  
}
print(k)


for(i in unique(dprimeMEAN$subject)){
  
  dprimeMEAN$dprime[dprimeMEAN$subject==i] <- dprimeMEAN$dprime[dprimeMEAN$subject==i] - mean(dprimeMEAN$dprime[dprimeMEAN$subject==i])
  
}

dprimeMEANs <- aggregate(dprime~emotion, data=dprimeMEAN, FUN = mean)
dprimeSDs <- aggregate(dprime~emotion, data=dprimeMEAN, FUN = sd)
dprimeMEANS <- cbind(dprimeMEANs, dprimeSDs[,2])
