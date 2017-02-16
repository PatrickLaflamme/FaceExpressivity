
#create a placeholder variable
datalist <- list()

#load the data for each subject into a seperate index in a list
for(i in unique(data$Subject)){
  datalist[[i]] <- data[data$Subject==i,]
}

#now create a placeholder data frame
dprime <- data.frame()
save <- data.frame()

for(i in unique(data$Subject)){
  
  print(i)
  #calculate the necessary info for each participant
  participant <- calc_dprime(datalist[[i]], "FaceIdentity", "no")
  #create an emotion factor variable
  emotion <- colnames(participant)
  save <- rbind(save, participant[1,])
  
  #rotate it for easier aggregation
  participant <- t(participant)
  
  #label the data by participant #
  subject <- rep(i, nrow(participant))
  
  #add this info to the placeholder DF.
  dprime <- rbind(dprime, cbind(participant, emotion, subject))
}

dprime$dprime<- as.numeric(as.character(dprime$dprime))

subjectmeans <- aggregate(dprime$dprime, by=list(dprime$subject), FUN=mean)
