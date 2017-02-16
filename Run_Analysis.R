#####
#Actually use these functions

#import the data we need

#data <- import_expressivity_data('~/Dropbox/Apps/Emotion_Sensitivity/Experiment_1/','[0-9]_ES.csv')

#data <- add_img_data('WSEFEP-norms&FACS.csv', data)

data$FaceByEmotion <- with(data, interaction(FaceIdentity, Emotion), drop=T)

dprime <- calc_dprime(data, "Emotion", "noise")

#now let's aggregate for inspection
dprimeMEAN <- ddply(dprime, .(Emotion), summarize, dprime = mean(dprime))


total.model <- aov(dprime ~ Emotion + Error(Subject/Emotion), data = dprime)
summary(total.model)

partAverage <- ddply(dprime, .(Subject), summarize, dprime = mean(dprime))
# 
# dprime$dprimeAdjusted <- NA
# 
# for(i in unique(dprime$subject)){
#   
#   dprime$dprimeAdjusted[dprime$subject==i] <- dprime$dprime[dprime$subject==i] - partAverage$x[partAverage$Group.1==i]
#   
# }


participant <- calc_dprime(data, "FaceByEmotion", "no.noise")


# dprime$intensity <- NA
# 
# for(i in 1:nrow(dprime)){
#   
#   dprime$intensity[i] <- info_base$Intensity..0.1.[as.character(dprime$ID)[i]==info_base$Displayer.ID & dprime$emotion[i]==info_base$Display]
#   
# }

par(mfrow=c(3,4))
for(i in keepers$Displayer.ID){
     data <- filter(info_base, Displayer.ID == i)
     data <- filter(data, Display != "neutral")
     plot(sort(data$`Intensity..0.1.`), main = i)
  }

