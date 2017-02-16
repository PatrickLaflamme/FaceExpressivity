library(labdsv)

#create the FACS rating matrix
dims <- convert_FACS_to_dim(info_base)

colnames(dims) <- 0:(ncol(dims)-1)

#name each FACS row
rownames(dims) <- info_base$Picture.ID

dims <- dims[,c(apply(dims, 2, sd)!=0)]

dealwith0 <- dist(dims)
dealwith0[which(dealwith0==0)]<-1e-10

#check the stress to find the best number of dimensions for MDS
stress <- c()

for(i in 1:7){
  stress[i] <- bestnmds(dealwith0, k=i)$stress
}

plot(stress, type = 'l', xlab = "Number of Dims", main= "Stress vs # of Dims")
points(stress, pch=4)

#it seems as though 3 dimensions is best here, but let's do 2 dimensions first

MDS2d <- bestnmds(dealwith0, k=2)

plot(MDS2d$points, pch = 16, col = palette()[c(1:6,8)][as.factor(info_base$Display[match(rownames(MDS2d$points), info_base$Picture.ID)])], xlab = "", ylab = '')
legend("topright", legend = levels(as.factor(info_base$Display[match(rownames(MDS2d$points), info_base$Picture.ID)])), pch = 16, col = palette()[c(1:6,8)], cex=1, inset=c(0.02))

#cool. Now let's find the centroid of each emotion, and define the distances from each other.
emotionCentroids <- aggregate(MDS2d$points, by= info_base$Display[match(rownames(MDS2d$points), info_base$Picture.ID)], FUN=colMeans)

polarMDS2dPoint <- cart2pol(MDS2d$points[,1], MDS2d$points[,2])

#now let's take a peak at 3 dimensions

MDS3d <- bestnmds(dealwith0, k=3)

rgl::plot3d(MDS3d$points, col = palette()[c(1:6,8)][as.factor(info_base$Display[match(rownames(MDS3d$points), info_base$Picture.ID)])], xlab = "", ylab = '', zlab='')
rgl::legend3d("topright", legend = levels(as.factor(info_base$Display[match(rownames(MDS3d$points), info_base$Picture.ID)])), pch = 16, col = palette()[c(1:6,8)], cex=1, inset=c(0.02))
rgl::text3d(MDS3d$points[,1],MDS3d$points[,2],MDS3d$points[,3], texts = rownames(MDS3d$points), color= palette()[c(1:6,8)][as.factor(info_base$Display[match(rownames(MDS3d$points), info_base$Picture.ID)])])



#calculate a global dprime for each image across all participants.

dprimes <- calc_dprime(data, "Target_name","no_img")

order <- c()
#make sure that the dprimes and dims are in the same order!
for(i in colnames(dprime)){
  order <- c(order, which(rownames(dims)==i))
}

dims <- cbind(dims, t(dprime[4,order]))


plot(MDS2d$points, pch = 16, col = as.factor(dims[rownames(unique(dims[,-c(28)]))]), ylim = c(-2,2), xlim = c(-2,4), xlab = "", ylab = '')
legend("topright", legend = paste(unique(info_base$Display)), pch = 16, col = palette()[1:7], cex=1, inset=c(0.02))

moviefunc3d <- function(time){
  plot3d(MDS3d$points, 
         col = palette()[-7][as.factor(dims[rownames(unique(dims[,-c(28,29)])),29])], 
         xlab = '',
         ylab = '', 
         zlab = '',
         xlim = c(-3,3),
         ylim = c(-3,3),
         zlim = c(-3,3),
         box = F,
         axes = F)
  legend3d("topright", legend = paste(unique(info_base$Display)), pch = 16, col = palette()[c(1:6,8)], cex=1, inset=c(0.02))
}


#now let's do it with phits
stress <- c()

idx <- c()
for(i in 1:ncol(dprime)){
  idx <- c(idx, which(info_base$Picture.ID %in% rownames(unique(t(dprime[4,])))[i]))
}

relDrpime <- unique(t(dprime[4,]))
relDrpime <- cbind(relDrpime, info_base$Display[idx])

for(i in 1:20){
  stress[i] <- isoMDS(dist(as.numeric(relDrpime[,1])), k=i)$stress
}

plot(stress, type = 'l', xlab = "Number of Dims", main= "Stress vs # of Dims")
points(stress, pch=4)

MDS2d <- isoMDS(dist(unique(t(dprime[4,]))), k=1)

MDS3d <- isoMDS(dist(unique(t(dprime[4,]))), k=3)

plot(MDS2d$points, pch = 16, col = as.factor(relDrpime[,2]), xlab = "", ylab = '')
legend("topright", legend = paste(unique(relDrpime[,2])), pch = 16, col = palette()[1:7], cex=1, inset=c(0.02))


#now let's do it with phits for each emotion once.
stress <- c()

dprimeEmots <- dprimeMEAN
for(i in 1:length(unique(dprimeMEAN$subject))){
  dprimeEmots$dprime[dprimeMEAN$subject==unique(dprimeMEAN$subject)[i]] <- dprimeMEAN$dprime[dprimeMEAN$subject==unique(dprimeMEAN$subject)[i]] - mean(dprimeMEAN$dprime[dprimeMEAN$subject==unique(dprimeMEAN$subject)[i]])
}

for(i in 1:20){
  stress <- c(stress, isoMDS(dist(unique(dprimeEmots$dprime)))$stress)
}

plot(stress, type = 'l', xlab = "Number of Dims", main= "Stress vs # of Dims")
points(stress, pch=4)

MDS2d <- isoMDS(dist(unique(dprimeEmots$dprime)), k=1)

MDS3d <- isoMDS(dist(t(dprimeEmots[4,])), k=3)

plot(MDS2d$points, pch = 16, col = as.factor(rownames(MDS2d$points)), xlab = "", ylab = '')
legend("topright", legend = rownames(MDS2d$points), pch = 16, col = palette()[1:7], cex=1, inset=c(0.02))

rgl::plot3d(MDS3d$points, col = palette()[c(1:6,8)][as.factor(info_base$Display[match(rownames(MDS3d$points), info_base$Picture.ID)])], xlab = "", ylab = '', zlab='')
rgl::legend3d("topright", legend = levels(as.factor(info_base$Display[match(rownames(MDS3d$points), info_base$Picture.ID)])), pch = 16, col = palette()[c(1:6,8)], cex=1, inset=c(0.02))
rgl::text3d(MDS3d$points[,1],MDS3d$points[,2],MDS3d$points[,3], texts = rownames(MDS3d$points), color= palette()[c(1:6,8)][as.factor(info_base$Display[match(rownames(MDS3d$points), info_base$Picture.ID)])])

centroidData <- cbind(MDS3d$points, as.factor(info_base$Display[match(rownames(MDS3d$points), info_base$Picture.ID)]))



dprimes <- calc_dprime(data, "Target_name","no_img")
 stress <- c()
   
for(i in 1:20){
   stress <- c(stress, isoMDS(dist(unique(t(dprimes[c(4),]))), k=i)$stress)
 }

 mds2d <- isoMDS(dist(t(dprimes[c(2,4),])), k=1)

 
plot(test, col=as.factor(info_base$Display[match(rownames(test$points), info_base$Picture.ID)]), pch=16)
 legend("topright", legend = levels(as.factor(info_base$Display[match(rownames(test$points), info_base$Picture.ID)])), pch = 16, col = palette()[1:7], cex=1, inset=c(0.02))
 
 dealwith0 <- dist(t(dprimes[c(4),]))
 dealwith0[dealwith0==0]<- 0.00000000000000000001
 
 vals <- bestnmds(dealwith0,k=1)
 
par(mfrow=c(7,1), mar=c(1,0,2,.25))

for( i in unique(info_base$Display)){
  stripchart(-vals$points[info_base$Display[match(rownames(vals$points), info_base$Picture.ID)]==i,], xlim = round(range(vals$points),-1), main=i, pch=16, col = )
  }

par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
 

vals <- bestnmds(dealwith0,k=2)

plot(vals$points, col=palette()[c(1:6,8)][as.factor(info_base$Display[match(rownames(vals$points), info_base$Picture.ID)])], pch=16, xlim = c(-25,25), ylim = round(range(vals$points[,2]), -1))
legend("topright", legend = levels(as.factor(info_base$Display[match(rownames(vals$points), info_base$Picture.ID)])), pch = 16, col = palette()[c(1:6,8)], cex=1, inset=c(-0.05,-0.02))

#let's compare the mean intensity of each emotion across faces
meanInt <- aggregate(emotiveInfo_base$Intensity..0.1., by= list(emotion = emotiveInfo_base$Display), FUN=mean)
barplot(meanInt$x, names.arg = meanInt$emotion)

cor.test(meanInt$x, aggregate(dprimeMEAN$dprime, list(dprimeMEAN$emotion), mean)$x[c(1:4,6:7)])

#now let's confirm - is there a correlation between face-level dprime and rated intensity?
emotivedprime <- dprime[, info_base$Display[match(colnames(dprime), info_base$Picture.ID)]!='neutral']
emotiveInfo_base <- info_base[info_base$Display!='neutral',]

emotiveInfo_base <- emotiveInfo_base[match(colnames(emotivedprime), emotiveInfo_base$Picture.ID),]

cor.test(t(emotivedprime[1,]), emotiveInfo_base$Intensity..0.1.)


