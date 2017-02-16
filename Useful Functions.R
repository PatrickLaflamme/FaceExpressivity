#######
#import data
#######

import_expressivity_data <- function(directory, filepattern){
  ##
  ##  directory     ::  location of the data files
  ##
  ##  filepattern   ::  Pattern to accept as datafiles. Uses regex
  ##
  ##  loads all the data files in a directory and saves them as a single dataframe

  names <- list.files(path = directory, pattern = filepattern, full.names = T)
  
  output <- data.frame()
  
  for(i in names){
    
    newdata <- read.csv(i, header=TRUE, row.names=NULL)
    
    output <- rbind(output, newdata)
    
  }
  
  return(output)
}

add_img_data <- function(image_db_name, data){
  
  #load the image data
  image_data <- read.csv(image_db_name, row.names = NULL)
  
  #make some corrections for how CSVs are read into R.
  data$Target_name <- strsplit(as.character(data$Target_name),'.png')
  image_data$Picture.ID <- strsplit(as.character(image_data$Picture.ID), '.jpg')
  image_data$Display <- as.character(image_data$Display)
  image_data$Male..Female <- as.character(image_data$Male..Female)
  image_data$Intensity..0.1.[image_data$Intensity..0.1.=='x'] <- NA
  image_data$Intensity..0.1. <- as.double(image_data$Intensity..0.1.)
  
  
  #create the new storage variable for the extra data
  extradata <- data.frame(matrix(ncol=3, nrow = nrow(data)))
  colnames(extradata) <- c('img_sex','Emotion', 'intensity')
  
  data$FaceIdentity <- NA
  
  pb <- progress::progress_bar$new(total = nrow(data))
  #for each image in the dataframe, do this
  for(i in 1:nrow(data)){
    
    data$FaceIdentity[i]<- strsplit(data$Target_name[i][[1]],"_")[[1]][1]
    
    if(length(grep(data$Target_name[i], image_data$Picture.ID))>0){
      extradata[i,] <- image_data[grep(data$Target_name[i], image_data$Picture.ID),c(3,4,7)]
    }
    else{
      if(data$Target_name[i]=='no_img'){
        extradata[i,] <-c(NA, 'noise', NA)
      }
      else{
        extradata[i,] <- c(NA, NA, NA)
        print(paste("caution: row",i,'has a filename that is not recognized'))
      }
    }
    pb$tick()
  }
  
  data <- cbind(data, extradata)
  
  return(data)

}

calc_dprime <- function(data, by, noiseval,Grouping=NULL){
  
  grouping <- c("Subject", Grouping)
  
  pFAs <- ddply(data[data[,by]==noiseval,], grouping , summarize, pFAs=mean(Response))
  
  pFAs <- rep(pFAs$pFAs, each=7*30)
  
  pHIT <- ddply(data[data[,by]!=noiseval,], c(grouping,by), summarize, pHIT=mean(Response))
  
  output <- cbind(pHIT, pFAs)
  
  output$pHIT <- mapvalues(output$pHIT, c(0, 1), c(1/4, 3/4))
  
  output <- mutate(output, dprime = qnorm(pHIT) - qnorm(pFAs))
  
  return(output)
}

convert_FACS_to_dim <- function(info_base){
  
  intensity <- list(A = 0.1, B = 0.3, C = 0.525, D = 0.775, E = .9)
  #intensity <- list(A = 1, B = 1, C = 1, D = 1, E = 1)
  FACS <- strsplit(info_base$FACS, split = "+", fixed = T)
  
  dimensions = matrix(data = 0, nrow = nrow(info_base), ncol = 70)
  
  for(i in 1:nrow(info_base)){
    intensities <- as.matrix(intensity[gsub("L","",gsub("[[:digit:]]","", FACS[[i]]))])
    groups <- as.numeric(gsub("[^[:digit:]]","", FACS[[i]]))
    for(j in 1:length(groups)){
      if(!is.null(intensities[[j]])){
        dimensions[i,groups[j]+1] <- intensities[[j]]
      }
    }
  }
  
  return(dimensions)
}

cart2pol <- Vectorize(function(x, y){
  r <- sqrt(x^2 + y^2)
  t <- atan(y/x)
  
  c(r,t)
}, vectorize.args = c('x','y'))
