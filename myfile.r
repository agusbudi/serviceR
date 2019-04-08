# myfile.R

#* @get /mean
normalMean <- function(samples=10){
  data <- rnorm(samples)
  mean(data)
}


#* @get /mean2
normalMean <- function(){
  data <- 5
  mean(data)
}

#* @post /sum
addTwo <- function(a, b, c){
  x = as.numeric(a) + as.numeric(b) + as.numeric(c[2])
  x
}


#* @post /sum2
addTwo <- function(a, b){
  x = as.numeric(a) + as.numeric(b)
  x
}


# Return "hello world"
#* @get /hello
function(){
  "hello world"
}

#* @post /extractData
extractData <- function(annotname,songname, rawDoc){
  
  annotname = "agus"
  songname = "result"
  
  annotfolder <- paste("PFE/",annotname,"_",songname, sep="")
#  subDir <- "outputDirectory"

  dir.create(file.path(annotfolder), showWarnings = TRUE)
#  setwd(file.path(mainDir, subDir))

  position = c("TP9","TP10", "AF7", "AF8")
  signal = c("delta","theta", "alpha", "beta","gamma")

  sink(paste(annotfolder,"/",annotname,"_result", ".csv", sep=""))
  cat(paste("",signal,sep=","))
  cat("\n")
  cat("\n")

  Results=matrix(NA, nrow = 7*4, ncol=5)
  # Results[1,]=c("TP9",signal)
  # Results[2,1]="min"
  # Results[3,1]="Q1"
  # Results[4,1]="Q2"
  # Results[5,1]="Q3"
  # Results[6,1]="max"
  # Results[7,1]="mean"
  # Results[8,1]="Std Dev"
  minimaxi=matrix(0,nrow = 2, ncol = 5)
  minimaxi[1,]=1000


  k=0
  rawDoc <- read.csv(textConnection(xxx),sep=",",row.names = NULL, header=FALSE)
  #rawDocX = rawDocX[complete.cases(rawDocX), ]
  rawDoc = subset(rawDoc, select = -V39 )
  rawDoc= rawDoc[-1, ]
  rawDoc= rawDoc[-2, ]
  #(rawDocX[3,2]=="")
#  rawDoc = matrix(scan(text = rawDoc),nrow = 39,byrow = TRUE)
 # doc=read.table(paste(annotfolder,"/",songname,".csv", sep=""),header=TRUE, fill = TRUE,sep = ",", row.names = NULL)
  doc=as.matrix(rawDoc)
  doc = doc[!is.na(doc[,2]),]
  linenumber = nrow(doc)
  for(infLoop in 1:ncol(doc)){
    doc[doc[,infLoop]== "-Inf", infLoop] <- -10
  }
  TP9=matrix(NA,nrow = linenumber, ncol = 6)
  TP9[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d %H:%M:%S")
  firstnum = as.numeric(TP9[1,1])

  TP9[1,1] = 0
  TP9[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d %H:%M:%S")
  TP9[2:linenumber,1] = as.numeric(TP9[2:linenumber,1]) - firstnum

  TP9[,2]=as.numeric(doc[,2])
  TP9[,3]=as.numeric(doc[,6])
  TP9[,4]=as.numeric(doc[,10])
  TP9[,5]=as.numeric(doc[,14])
  TP9[,6]=as.numeric(doc[,18])

  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(TP9[,t+1])
    Results[2+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(TP9[,t+1])
    Results[6+idxRes,t]=mean(TP9[,t+1])
    Results[7+idxRes,t]=sd(TP9[,t+1])
  }
  k=k+1

  cat(paste("Left Ear_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(TP9[,2:6])
  maxval= max(TP9[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_TP9", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(TP9[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Left Ear Signal")
  lines(rep(0, max(TP9[,1])), type = "l", col = "black")
  lines(TP9[,2], type = "l", col = "red")
  lines(TP9[,3], type = "l", col = "blue")
  lines(TP9[,4], type = "l", col = "green")
  lines(TP9[,5], type = "l", col = "orange")
  lines(TP9[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()

  ##=============================================##
  AF7=matrix(NA,nrow = linenumber, ncol = 6)
  AF7[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(AF7[1,1])

  AF7[1,1] = 0
  AF7[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  AF7[2:linenumber,1] = as.numeric(AF7[2:linenumber,1]) - firstnum

  AF7[,2]=as.numeric(doc[,3])
  AF7[,3]=as.numeric(doc[,7])
  AF7[,4]=as.numeric(doc[,11])
  AF7[,5]=as.numeric(doc[,15])
  AF7[,6]=as.numeric(doc[,19])

  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(AF7[,t+1])
    Results[2+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(AF7[,t+1])
    Results[6+idxRes,t]=mean(AF7[,t+1])
    Results[7+idxRes,t]=sd(AF7[,t+1])
  }
  k=k+1
  cat(paste("Left Forehead_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(AF7[,2:6])
  maxval= max(AF7[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_AF7", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(AF7[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Left Forehead Signal")
  lines(rep(0, max(AF7[,1])), type = "l", col = "black")
  lines(AF7[,2], type = "l", col = "red")
  lines(AF7[,3], type = "l", col = "blue")
  lines(AF7[,4], type = "l", col = "green")
  lines(AF7[,5], type = "l", col = "orange")
  lines(AF7[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()

  ##=============================================##
  AF8=matrix(NA,nrow = linenumber, ncol = 6)
  AF8[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(AF8[1,1])
  AF8[1,1] = 0
  AF8[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  AF8[2:linenumber,1] = as.numeric(AF8[2:linenumber,1]) - firstnum

  AF8[,2]=as.numeric(doc[,4])
  AF8[,3]=as.numeric(doc[,8])
  AF8[,4]=as.numeric(doc[,12])
  AF8[,5]=as.numeric(doc[,16])
  AF8[,6]=as.numeric(doc[,20])

  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(AF8[,t+1])
    Results[2+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(AF8[,t+1])
    Results[6+idxRes,t]=mean(AF8[,t+1])
    Results[7+idxRes,t]=sd(AF8[,t+1])
  }
  k=k+1
  cat(paste("Right Forehead_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(AF8[,2:6])
  maxval= max(AF8[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_AF8", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(AF8[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Right Forehead Signal")
  lines(rep(0, max(AF8[,1])), type = "l", col = "black")
  lines(AF8[,2], type = "l", col = "red")
  lines(AF8[,3], type = "l", col = "blue")
  lines(AF8[,4], type = "l", col = "green")
  lines(AF8[,5], type = "l", col = "orange")
  lines(AF8[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()

  ##=====================================#
  TP10=matrix(NA,nrow = linenumber, ncol = 6)
  TP10[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(TP10[1,1])
  TP10[1,1] = 0
  TP10[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  TP10[2:linenumber,1] = as.numeric(TP10[2:linenumber,1]) - firstnum

  TP10[,2]=as.numeric(doc[,5])
  TP10[,3]=as.numeric(doc[,9])
  TP10[,4]=as.numeric(doc[,13])
  TP10[,5]=as.numeric(doc[,17])
  TP10[,6]=as.numeric(doc[,21])

  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(TP10[,t+1])
    Results[2+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(TP10[,t+1])
    Results[6+idxRes,t]=mean(TP10[,t+1])
    Results[7+idxRes,t]=sd(TP10[,t+1])
  }
  k=k+1
  cat(paste("Right Ear_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(TP10[,2:6])
  maxval= max(TP10[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_TP10", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(TP10[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Right Ear Signal")
  lines(rep(0, max(TP10[,1])), type = "l", col = "black")
  lines(TP10[,2], type = "l", col = "red")
  lines(TP10[,3], type = "l", col = "blue")
  lines(TP10[,4], type = "l", col = "green")
  lines(TP10[,5], type = "l", col = "orange")
  lines(TP10[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()

  for(z in 1:5){
    if(min(TP9[,z+1]) < minimaxi[1,z] & min(TP9[,z+1])!="-Inf"  & min(TP9[,z+1])> -10 )
      minimaxi[1,z]=min(TP9[,z+1])
    if(max(TP9[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(TP9[,z+1])

    if(min(TP10[,z+1]) < minimaxi[1,z] & min(TP10[,z+1])!="-Inf"  & min(TP10[,z+1])> -10 )
      minimaxi[1,z]=min(TP10[,z+1])
    if(max(TP10[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(TP10[,z+1])

    if(min(AF7[,z+1]) < minimaxi[1,z] & min(AF7[,z+1])!="-Inf"  & min(AF7[,z+1])> -10 )
      minimaxi[1,z]=min(AF7[,z+1])
    if(max(AF7[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(AF7[,z+1])

    if(min(AF8[,z+1]) < minimaxi[1,z] & min(AF8[,z+1])!="-Inf"  & min(AF8[,z+1])> -10 )
      minimaxi[1,z]=min(AF8[,z+1])
    if(max(AF8[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(AF8[,z+1])
  }
  #write.table(Results,paste(annotname,"_result", ".csv", sep=""), sep=",",row.names=FALSE,col.names=FALSE)
  sink()
  #file.show(paste(annotname,"_result", ".csv", sep=""))













  ###################################normalization and histogram############
  i=1
  sink(paste(annotfolder,"/",annotname,"_result", "-norm.csv", sep=""))
  cat(paste("",signal,sep=","))
  cat("\n")
  cat("\n")

  numBins=8
  histoInfo=matrix(NA,nrow = 1, ncol = numBins*4)
  k=0

 # doc=read.table(paste(annotfolder,"/",songname,".csv", sep=""),header=TRUE, fill = TRUE,sep = ",", row.names = NULL)
  doc=as.matrix(rawDoc)
  doc = doc[!is.na(doc[,2]),]
  linenumber = nrow(doc)
  for(infLoop in 1:ncol(doc)){
    doc[doc[,infLoop]== "-Inf", infLoop] <- -10
  }
  TP9=matrix(NA,nrow = linenumber, ncol = 6)
  TP9[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(TP9[1,1])

  TP9[1,1] = 0
  TP9[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  TP9[2:linenumber,1] = as.numeric(TP9[2:linenumber,1]) - firstnum


  TP9[,2]=(as.numeric(doc[,2])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
  TP9[,3]=(as.numeric(doc[,6])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
  TP9[,4]=(as.numeric(doc[,10])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
  TP9[,5]=(as.numeric(doc[,14])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
  TP9[,6]=(as.numeric(doc[,18])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))


  down=0
  deltaTop=1/numBins
  hi=1
  for(lop in 1:numBins){
    if(lop==numBins){
      histoInfo[hi]= length(TP9[TP9[,2]>= down & TP9[,2]<= (down+deltaTop),2])
      histoInfo[hi+8*1]= length(TP9[TP9[,3]>= down & TP9[,3]<= (down+deltaTop),3])
      histoInfo[hi+8*2]= length(TP9[TP9[,4]>= down & TP9[,4]<= (down+deltaTop),4])
      histoInfo[hi+8*3]= length(TP9[TP9[,5]>= down & TP9[,5]<= (down+deltaTop),5])
      histoInfo[hi+8*4]= length(TP9[TP9[,6]>= down & TP9[,6]<= (down+deltaTop),6])
    }else{
      histoInfo[hi]= length(TP9[TP9[,2]>= down & TP9[,2]< (down+deltaTop),2])
      histoInfo[hi+8*1]= length(TP9[TP9[,3]>= down & TP9[,3]< (down+deltaTop),3])
      histoInfo[hi+8*2]= length(TP9[TP9[,4]>= down & TP9[,4]< (down+deltaTop),4])
      histoInfo[hi+8*3]= length(TP9[TP9[,5]>= down & TP9[,5]< (down+deltaTop),5])
      histoInfo[hi+8*4]= length(TP9[TP9[,6]>= down & TP9[,6]< (down+deltaTop),6])

      down = down + deltaTop
    }
    hi=hi+1
  }

  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(TP9[,t+1])
    Results[2+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(TP9[,t+1])
    Results[6+idxRes,t]=mean(TP9[,t+1])
    Results[7+idxRes,t]=sd(TP9[,t+1])
  }
  k=k+1

  cat(paste("Left Ear_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(TP9[,2:6])
  maxval= max(TP9[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_TP9", "-norm.png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(TP9[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Left Ear Signal")
  lines(rep(0, max(TP9[,1])), type = "l", col = "black")
  lines(TP9[,2], type = "l", col = "red")
  lines(TP9[,3], type = "l", col = "blue")
  lines(TP9[,4], type = "l", col = "green")
  lines(TP9[,5], type = "l", col = "orange")
  lines(TP9[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()

  ##=============================================##
  AF7=matrix(NA,nrow = linenumber, ncol = 6)
  AF7[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(AF7[1,1])

  AF7[1,1] = 0
  AF7[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  AF7[2:linenumber,1] = as.numeric(AF7[2:linenumber,1]) - firstnum

  AF7[,2]=(as.numeric(doc[,3])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
  AF7[,3]=(as.numeric(doc[,7])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
  AF7[,4]=(as.numeric(doc[,11])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
  AF7[,5]=(as.numeric(doc[,15])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
  AF7[,6]=(as.numeric(doc[,19])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))

  down=0
  hi=1
  for(lop in 1:numBins){
    if(lop==numBins){
      histoInfo[hi+8*5]= length(AF7[AF7[,2]>= down & AF7[,2]<= (down+deltaTop),2])
      histoInfo[hi+8*6]= length(AF7[AF7[,3]>= down & AF7[,3]<= (down+deltaTop),3])
      histoInfo[hi+8*7]= length(AF7[AF7[,4]>= down & AF7[,4]<= (down+deltaTop),4])
      histoInfo[hi+8*8]= length(AF7[AF7[,5]>= down & AF7[,5]<= (down+deltaTop),5])
      histoInfo[hi+8*9]= length(AF7[AF7[,6]>= down & AF7[,6]<= (down+deltaTop),6])
    }else{
      histoInfo[hi+8*5]= length(AF7[AF7[,2]>= down & AF7[,2]< (down+deltaTop),2])
      histoInfo[hi+8*6]= length(AF7[AF7[,3]>= down & AF7[,3]< (down+deltaTop),3])
      histoInfo[hi+8*7]= length(AF7[AF7[,4]>= down & AF7[,4]< (down+deltaTop),4])
      histoInfo[hi+8*8]= length(AF7[AF7[,5]>= down & AF7[,5]< (down+deltaTop),5])
      histoInfo[hi+8*9]= length(AF7[AF7[,6]>= down & AF7[,6]< (down+deltaTop),6])

      down = down + deltaTop
    }
    hi=hi+1
  }

  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(AF7[,t+1])
    Results[2+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(AF7[,t+1])
    Results[6+idxRes,t]=mean(AF7[,t+1])
    Results[7+idxRes,t]=sd(AF7[,t+1])
  }
  k=k+1
  cat(paste("Left Forehead_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(AF7[,2:6])
  maxval= max(AF7[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_AF7", "-norm.png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(AF7[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Left Forehead Signal")
  lines(rep(0, max(AF7[,1])), type = "l", col = "black")
  lines(AF7[,2], type = "l", col = "red")
  lines(AF7[,3], type = "l", col = "blue")
  lines(AF7[,4], type = "l", col = "green")
  lines(AF7[,5], type = "l", col = "orange")
  lines(AF7[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()

  ##=============================================##
  AF8=matrix(NA,nrow = linenumber, ncol = 6)
  AF8[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(AF8[1,1])
  AF8[1,1] = 0
  AF8[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  AF8[2:linenumber,1] = as.numeric(AF8[2:linenumber,1]) - firstnum

  AF8[,2]=(as.numeric(doc[,4])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
  AF8[,3]=(as.numeric(doc[,8])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
  AF8[,4]=(as.numeric(doc[,12])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
  AF8[,5]=(as.numeric(doc[,16])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
  AF8[,6]=(as.numeric(doc[,20])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))

  down=0
  hi=1
  for(lop in 1:numBins){
    if(lop==numBins){
      histoInfo[hi+8*10]= length(AF8[AF8[,2]>= down & AF8[,2]<= (down+deltaTop),2])
      histoInfo[hi+8*11]= length(AF8[AF8[,3]>= down & AF8[,3]<= (down+deltaTop),3])
      histoInfo[hi+8*12]= length(AF8[AF8[,4]>= down & AF8[,4]<= (down+deltaTop),4])
      histoInfo[hi+8*13]= length(AF8[AF8[,5]>= down & AF8[,5]<= (down+deltaTop),5])
      histoInfo[hi+8*14]= length(AF8[AF8[,6]>= down & AF8[,6]<= (down+deltaTop),6])
    }else{
      histoInfo[hi+8*10]= length(AF8[AF8[,2]>= down & AF8[,2]< (down+deltaTop),2])
      histoInfo[hi+8*11]= length(AF8[AF8[,3]>= down & AF8[,3]< (down+deltaTop),3])
      histoInfo[hi+8*12]= length(AF8[AF8[,4]>= down & AF8[,4]< (down+deltaTop),4])
      histoInfo[hi+8*13]= length(AF8[AF8[,5]>= down & AF8[,5]< (down+deltaTop),5])
      histoInfo[hi+8*14]= length(AF8[AF8[,6]>= down & AF8[,6]< (down+deltaTop),6])

      down = down + deltaTop
    }
    hi=hi+1
  }


  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(AF8[,t+1])
    Results[2+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(AF8[,t+1])
    Results[6+idxRes,t]=mean(AF8[,t+1])
    Results[7+idxRes,t]=sd(AF8[,t+1])
  }
  k=k+1
  cat(paste("Right Forehead_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(AF8[,2:6])
  maxval= max(AF8[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_AF8", "-norm.png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(AF8[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Right Forehead Signal")
  lines(rep(0, max(AF8[,1])), type = "l", col = "black")
  lines(AF8[,2], type = "l", col = "red")
  lines(AF8[,3], type = "l", col = "blue")
  lines(AF8[,4], type = "l", col = "green")
  lines(AF8[,5], type = "l", col = "orange")
  lines(AF8[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()

  ##=====================================#
  TP10=matrix(NA,nrow = linenumber, ncol = 6)
  TP10[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(TP10[1,1])
  TP10[1,1] = 0
  TP10[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  TP10[2:linenumber,1] = as.numeric(TP10[2:linenumber,1]) - firstnum

  TP10[,2]=(as.numeric(doc[,5])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
  TP10[,3]=(as.numeric(doc[,9])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
  TP10[,4]=(as.numeric(doc[,13])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
  TP10[,5]=(as.numeric(doc[,17])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
  TP10[,6]=(as.numeric(doc[,21])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))

  down=0
  hi=1
  for(lop in 1:numBins){
    if(lop==numBins){
      histoInfo[hi+8*15]= length(TP10[TP10[,2]>= down & TP10[,2]<= (down+deltaTop),2])
      histoInfo[hi+8*16]= length(TP10[TP10[,3]>= down & TP10[,3]<= (down+deltaTop),3])
      histoInfo[hi+8*17]= length(TP10[TP10[,4]>= down & TP10[,4]<= (down+deltaTop),4])
      histoInfo[hi+8*18]= length(TP10[TP10[,5]>= down & TP10[,5]<= (down+deltaTop),5])
      histoInfo[hi+8*19]= length(TP10[TP10[,6]>= down & TP10[,6]<= (down+deltaTop),6])
    }else{
      histoInfo[hi+8*15]= length(TP10[TP10[,2]>= down & TP10[,2]< (down+deltaTop),2])
      histoInfo[hi+8*16]= length(TP10[TP10[,3]>= down & TP10[,3]< (down+deltaTop),3])
      histoInfo[hi+8*17]= length(TP10[TP10[,4]>= down & TP10[,4]< (down+deltaTop),4])
      histoInfo[hi+8*18]= length(TP10[TP10[,5]>= down & TP10[,5]< (down+deltaTop),5])
      histoInfo[hi+8*19]= length(TP10[TP10[,6]>= down & TP10[,6]< (down+deltaTop),6])

      down = down + deltaTop
    }
    hi=hi+1
  }

  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(TP10[,t+1])
    Results[2+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(TP10[,t+1])
    Results[6+idxRes,t]=mean(TP10[,t+1])
    Results[7+idxRes,t]=sd(TP10[,t+1])
  }
  k=k+1
  cat(paste("Right Ear_", annotname,"_",songname,"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")

  minval= min(TP10[,2:6])
  maxval= max(TP10[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_TP10", "-norm.png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(TP10[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Right Ear Signal")
  lines(rep(0, max(TP10[,1])), type = "l", col = "black")
  lines(TP10[,2], type = "l", col = "red")
  lines(TP10[,3], type = "l", col = "blue")
  lines(TP10[,4], type = "l", col = "green")
  lines(TP10[,5], type = "l", col = "orange")
  lines(TP10[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.

  area = "TP9"
  signallop=1
  for(lop in 1:19){
    if(lop==6){
      area="AF7"
      signallop=1
    } else if (lop==11){
      area="AF8"
      signallop=1
    } else if (lop==16){
      area="TP10"
      signallop=1
    }

    png(file = paste(annotfolder,"/",annotname,"_",songname,"_", area ,"_", signal[signallop], "-histo-norm.png", sep=""))
    B = histoInfo[(8*(lop-1)+1):(8*(lop-1)+8)]
    names(B) <- 1:8
    mp <- barplot(B,main=paste(area, " - ", signal[signallop]), xlab="interval", ylab="the number of signal",
                  border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
    text(mp, B, labels = B, pos = 3)
    dev.off()

    signallop= signallop +1
  }

  ##draw for all histogram
  area = "TP9"
  signallop=1
  B2=NULL
  png(file = paste(annotfolder,"/",annotname,"_",songname,"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
  for(lop in 1:20){
    if(lop==6){
      names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
      mp <- barplot(B2,main="Left Ear Signal", xlab="interval", ylab="the number of signal",
                    border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
      text(mp, B2, labels = B2, pos = 3)
      abline(h=30, col=1)
      abline(v=0, col=1)
      abline(v=9.65, col=1)
      abline(v=9.65*2, col=1)
      abline(v=9.65*3, col=1)
      abline(v=9.65*4, col=1)
      abline(v=9.65*5, col=1)
      dev.off()
      B2=NULL
      area="AF7"
      signallop=1
      png(file = paste(annotfolder,"/",annotname,"_",songname,"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
    } else if (lop==11){
      names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
      mp <- barplot(B2,main="Left Forehead Signal", xlab="interval", ylab="the number of signal",
                    border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
      text(mp, B2, labels = B2, pos = 3)
      abline(h=30, col=1)
      abline(v=0, col=1)
      abline(v=9.65, col=1)
      abline(v=9.65*2, col=1)
      abline(v=9.65*3, col=1)
      abline(v=9.65*4, col=1)
      abline(v=9.65*5, col=1)
      dev.off()
      B2=NULL
      area="AF8"
      signallop=1
      png(file = paste(annotfolder,"/",annotname,"_",songname,"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
    } else if (lop==16){
      names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
      mp <- barplot(B2,main="Right Forehead Signal", xlab="interval (Delta-Theta-Alpha-Beta-Gamma)", ylab="the number of signal",
                    border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
      text(mp, B2, labels = B2, pos = 3)
      abline(h=30, col=1)
      abline(v=0, col=1)
      abline(v=9.65, col=1)
      abline(v=9.65*2, col=1)
      abline(v=9.65*3, col=1)
      abline(v=9.65*4, col=1)
      abline(v=9.65*5, col=1)
      dev.off()
      B2=NULL
      area="TP10"
      signallop=1
      png(file = paste(annotfolder,"/",annotname,"_",songname,"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
    }

    B = histoInfo[(8*(lop-1)+1):(8*(lop-1)+8)]
    B2 = c(B2, B)
    signallop= signallop +1
  }
  names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
  mp <- barplot(B2,main="Right Ear Signal", xlab="interval", ylab="the number of signal",
                border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
  text(mp, B2, labels = B2, pos = 3)
  abline(h=30, col=1)
  abline(v=0, col=1)
  abline(v=9.65, col=1)
  abline(v=9.65*2, col=1)
  abline(v=9.65*3, col=1)
  abline(v=9.65*4, col=1)
  abline(v=9.65*5, col=1)
  dev.off()
  
  write.table(histoInfo,paste(annotfolder,"/histoInfo.txt", sep=""),sep=",",row.names=FALSE,col.names=FALSE)
  histoInfoname= cbind(histoInfo,annotname)
  write.table(histoInfoname,paste(annotfolder,"/histoInfo.name.txt", sep=""),sep=",",row.names=FALSE,col.names=FALSE)

  sink()
  histoInfo
}



#* @post /CRA
CRA <- function(folder, conv, thres, ent){
  threshold= as.double(thres)
  entro= as.double(ent)
  #select the number of reliable annotator exist
  convergence.Limit = as.double(conv) #0.01
  ##define the spammer score threshold, see (Raykar, 2012)
  epsilon.S = ((1- threshold) + (1- threshold) -1)^2

  ptm2=NULL
  
  nb.S=1
  ptm2 <- proc.time()
  X.vali=read.table(paste(folder,"X-vali.txt", sep=""),header=FALSE)
  X.vali=as.matrix(X.vali)
  Y.entropy = as.matrix(read.table(paste(folder,"entropy_input_EM.txt", sep=""),header=FALSE))
  
  
  Yname= paste(folder,"Y.txt", sep="")
  Y=read.table(Yname,header=FALSE)
  Y=as.matrix(Y)
  colnames(Y)=seq(1,ncol(Y),1)

  ##### calculate entropy: see (Zighed et al, 2010)
  f=matrix(0,nrow=2,ncol=ncol(Y))
  lamb=matrix(0,nrow=2,ncol=ncol(Y))
  w=NULL
  res.ent=NULL
  
  for (t in 1:ncol(Y)){
    for (i in 1:2){
      w[i]=length(which(Y.entropy==(i-1)))/length(Y.entropy)
      
      f[i,t]=length(which(Y[,t]==(i-1)))/length(Y.entropy) #frequency of class 0 and 1 prediction for each annotator
      lamb[i,t]=(nrow(X.vali)*f[i,t]+1)/(nrow(X.vali)+2) #define lambda, 6.13
    }
    res.ent[t]=sum((lamb[,t]*(1-lamb[,t]))/((-2*w+1)*lamb[,t]+w^2)) #calculate entropy, 6.12
  }
  
  
  ##################################################################################################
  ##choose with the default threshold (0.6 of sensitivity and specificity based on the reference distribution)
  thres=NULL
  for (i in 1:2){
    thres[i]=threshold*length(which(Y.entropy==(i-1)))
    f[i,t]=thres[i]/length(Y.entropy) #frequency of class 0 and 1 prediction for each annotator
    lamb[i,t]=(nrow(X.vali)*f[i,t]+1)/(nrow(X.vali)+2) #define lambda, 6.13
  }
  thres.res.ent=sum((lamb[,t]*(1-lamb[,t]))/((-2*w+1)*lamb[,t]+w^2)) #calculate entropy, 6.12
  nb.annot = length(res.ent[which(res.ent>=thres.res.ent)])
  
  
  ###recent editing, in order to limit the group number, if top-K has >50% of annotators, select only top 30%
  #  if(nb.annot > 0.5*ncol(Y))
  nb.annot=entro*nb.annot
  
  ##choose the 30% from all annotators based on the entropy, higher is better
  ordre.ent=order(res.ent, na.last = TRUE, decreasing = TRUE)
  annot=ordre.ent[1:nb.annot]

  annot.exp=matrix(0,nrow=ncol(Y),ncol=ncol(Y))
  
  #initiate Y with only the selected annotators
  Y=Y[,annot]
  Y.corr=matrix(0,nrow=2,ncol=ncol(Y))
  Y.corr[1,]=annot
  Y.corr[2,]=1:ncol(Y.corr)
  colnames(Y)=seq(1,ncol(Y),1)
  
  good.a=1:ncol(Y) #only the order
  
  library(polynom)
  
  # Initialization of mu
  R = ncol(Y)  #the number of annotators
  mu = NULL
  for (i in (1:nrow(X.vali)))
  {
    mu[i] = (1/R)*sum(Y[i,])    #Algorithm 9. Initialization of \mu value, Wolley's dissertation page 131, line 6
  }
  
  eta  = 1
  g = matrix(0,nrow=nrow(X.vali),ncol=ncol(X.vali))
  a = NULL
  b = NULL
  p = NULL
  diff1= 10
  H=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
  
  #calculate the Alpha1 and Beta1 :
  alpha = matrix(0,nrow=R,ncol=10000)
  beta = matrix(0,nrow=R,ncol=10000)
  c=1   #
  library(MASS)
  
  for (j in 1:R)
  {
    alpha[j,c] = (sum(mu*Y[,j]))/(sum(mu))
    beta[j,c] = (sum((1-mu)*(1-Y[,j])))/(sum(1-mu))
  }
  
  #### Calculate w, the parameter that has to be estimated by using Newton-Raphson method ######
  y2=NULL
  for ( i in 1:nrow(X.vali))
  {
    if (mu[i] < 0.5){
      y2[i] = 0
    }else {
      y2[i]=1
    }
  }
  w = matrix(0,nrow=ncol(X.vali),ncol=10000)
  w[,c] = ginv(X.vali)%*%y2
  
  
  # Calculate p, the probability fo giving true value, z=1
  p = 1/(1+exp(-(t(w[,c])%*%t(X.vali))))
  #Calculate g, gradient vector:
  for (i in 1:nrow(X.vali)){
    g[i,] = (mu[i]-p[i])*X.vali[i,]
  }
  
  g_new=NULL
  g_new=colSums(g)
  
  # Calcul H^-1, H is a Hessian Matrix:
  H = array(0,dim=c(nrow(X.vali),ncol(X.vali),ncol(X.vali)))
  for (i in 1:nrow(X.vali)){
    H[i,,] = p[i]*(1-p[i])*(X.vali[i,]%*%t(X.vali[i,]))
  }
  
  H_new=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
  H_new=H[1,,]
  for (i in 2:nrow(X.vali)){
    H_new=H_new+H[i,,]
  }
  H_new=-H_new

  # Calculate the new w
  w[,c+1] = w[,c] - eta*ginv(H_new)%*%g_new
  
  # Calculate the difference
  diff1=sum(sqrt((w[,c+1]-w[,c])^2))

  
  ########### E-STEP ###################
  # Calculate a and b: a is the probability of true positive, and b is the probability of true negative
  
  while (diff1 > convergence.Limit)
  {
    for (i in 1:nrow(X.vali))
    {
      a[i] = prod((alpha[,c]^(Y[i,]))*((1-alpha[,c])^(1-Y[i,])))
      b[i] = prod((beta[,c]^(1-Y[i,]))*((1-beta[,c])^(Y[i,])))
      
    }
    
    for (i in 1:nrow(X.vali)){
      if (a[i]==0) a[i]=10^{-10}
      if (b[i]==0) b[i]=10^{-10}
    }
    
    # Calcul de p:
    p = 1/(1+exp(-(t(w[,c+1])%*%t(X.vali))))

    # Calcul du nouveau mu
    for (i in 1:nrow(X.vali))
    {
      mu[i] = (a[i]*p[i])/(a[i]*p[i]+b[i]*(1-p[i]))
    }

    c=c+1
    for (j in 1:R)
    {
      alpha[j,c] = (sum(mu*Y[,j]))/(sum(mu))
      beta[j,c] = (sum((1-mu)*(1-Y[,j])))/(sum(1-mu))
    }
    
    #### Calculate w:
    
    # Calculate g:
    for (i in 1:nrow(X.vali))
    {
      g[i,] = (mu[i]-p[i])*X.vali[i,]
    }
    
    g_new=NULL
    g_new=colSums(g)
    
    # Calculate H^-1:
    H = array(0,dim=c(nrow(X.vali),ncol(X.vali),ncol(X.vali)))
    dim(H)
    for (i in 1:nrow(X.vali)){
      H[i,,] = p[i]*(1-p[i])*(X.vali[i,]%*%t(X.vali[i,]))
    }
    
    H_new=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
    H_new=H[1,,]
    for (i in 2:nrow(X.vali)){
      H_new=H_new+H[i,,]
    }
    H_new=-H_new
    
    # Calculate the new w
    w[,c+1] = w[,c] - eta*ginv(H_new)%*%g_new
    
    # Calculate the difference
    diff1=sum(sqrt((w[,c+1]-w[,c])^2))
    
    if(c==9999) diff1=0
    
  }
  ##################################################
  if(is.nan(beta[,c]))
    c=c-1

  
  ####Calculate the score, higher is better
  
  S=NULL
  for (j in 1:ncol(Y)) S[j]=(alpha[j,c]+beta[j,c]-1)^2

  c3=1
  annot.exp[c3,1:length(good.a)]=good.a
  c3=c3+1
  
  #inspired from Condorcet Jury's theorem
  if(length(which(S>epsilon.S)) > 0){
    annot.exp[c3,1:length(which(S>epsilon.S))]=which(S>epsilon.S)
    annot.exp[c3,1:length(which(S>epsilon.S))]=Y.corr[1,which(S>epsilon.S)]
  }else{
    ordre.S=order(S, na.last = TRUE, decreasing = TRUE)
    temp.epsilon.S=S[ordre.S[nb.S]]
    annot.exp[c3,1:length(which(S>=temp.epsilon.S))]=which(S>=temp.epsilon.S)
    annot.exp[c3,1:length(which(S>=temp.epsilon.S))]=Y.corr[1,which(S>=temp.epsilon.S)]
    
  }
  
  ###iterate until convergent
  while (sum((annot.exp[c3,]-annot.exp[c3-1,]))!=0){
    ##merge previous potential reliable annotator with the new group
    annot=ordre.ent[(((c3-1)*(nb.annot)+1):(c3*nb.annot))]
    annot=na.omit(annot)
    
    
    ###re-initiate the annotators only with the selected group
    
    Y=read.table(Yname,header=FALSE)
    Y=as.matrix(Y)
    colnames(Y)=seq(1,ncol(Y),1)

    Y=Y[,c(annot,annot.exp[c3,(1:length(which(S>epsilon.S)))])]
    
    #if there is still available group
    if(!is.null(ncol(Y))){
      Y.corr=matrix(0,nrow=2,ncol=ncol(Y))
      Y.corr[1,]=c(annot,annot.exp[c3,(1:length(which(S>epsilon.S)))])
      Y.corr[2,]=1:ncol(Y.corr)
      colnames(Y)=seq(1,ncol(Y),1)
      
      library(polynom)
      
      good.a=1:ncol(Y)
      
      # Initialization of mu:
      R = ncol(Y)
      mu = NULL
      for (i in (1:nrow(X.vali)))
      {
        mu[i] = (1/R)*sum(Y[i,])
      }

      eta  = 1
      g = matrix(0,nrow=nrow(X.vali),ncol=ncol(X.vali))
      a = NULL
      b = NULL
      p = NULL
      diff1= 10
      H=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
      
      
      # Calculate alpha1 & beta1, alpha1 is P(y=1|z=1), beta1 is P(y=0|z=0)
      alpha = matrix(0,nrow=R,ncol=10000)
      beta = matrix(0,nrow=R,ncol=10000)
      c=1
      library(MASS)
      
      for (j in 1:R)
      {
        alpha[j,c] = (sum(mu*Y[,j]))/(sum(mu))
        beta[j,c] = (sum((1-mu)*(1-Y[,j])))/(sum(1-mu))
      }

      #Calculate w
      y2=NULL
      for ( i in 1:nrow(X.vali))
      {
        if (mu[i] < 0.5)
        {
          y2[i] = 0
        }else
        {
          y2[i]=1
        }
      }
      w = matrix(0,nrow=ncol(X.vali),ncol=30000)
      w[,c] = ginv(X.vali)%*%y2
      
      
      # Calculate p:
      p = 1/(1+exp(-(t(w[,c])%*%t(X.vali))))
      #Calculate g:
      for (i in 1:nrow(X.vali))
      {
        g[i,] = (mu[i]-p[i])*X.vali[i,]
      }
      
      g_new=NULL
      g_new=colSums(g)
      
      # Calculate H^-1:
      H = array(0,dim=c(nrow(X.vali),ncol(X.vali),ncol(X.vali)))
      for (i in 1:nrow(X.vali))
      {
        H[i,,] = p[i]*(1-p[i])*(X.vali[i,]%*%t(X.vali[i,]))
      }
      
      H_new=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
      H_new=H[1,,]
      for (i in 2:nrow(X.vali))
      {
        H_new=H_new+H[i,,]
      }
      H_new=-H_new

      # Calculate the new w
      w[,c+1] = w[,c] - eta*ginv(H_new)%*%g_new
      
      # Calculate the difference
      diff1=sum(sqrt((w[,c+1]-w[,c])^2))

      ########### E-STEP ###################
      # Calculate a & b:
      while (diff1 > convergence.Limit){
        for (i in 1:nrow(X.vali))
        {
          a[i] = prod((alpha[,c]^(Y[i,]))*((1-alpha[,c])^(1-Y[i,])))
          b[i] = prod((beta[,c]^(1-Y[i,]))*((1-beta[,c])^(Y[i,])))
        }
        
        for (i in 1:nrow(X.vali)){
          if (a[i]==0) a[i]=10^{-1}
          if (b[i]==0) b[i]=10^{-1}
        }
        
        # Calculate p:
        p = 1/(1+exp(-(t(w[,c+1])%*%t(X.vali))))
        
        # Calculate the new mu
        for (i in 1:nrow(X.vali)){
          mu[i] = (a[i]*p[i])/(a[i]*p[i]+b[i]*(1-p[i]))
        }
        
        c=c+1
        for (j in 1:R)
        {
          alpha[j,c] = (sum(mu*Y[,j]))/(sum(mu))
          beta[j,c] = (sum((1-mu)*(1-Y[,j])))/(sum(1-mu))
        }

        # Calculate g:
        for (i in 1:nrow(X.vali))
        {
          g[i,] = (mu[i]-p[i])*X.vali[i,]
        }
        
        g_new=NULL
        g_new=colSums(g)
        
        # Calculate H^-1:
        H = array(0,dim=c(nrow(X.vali),ncol(X.vali),ncol(X.vali)))
        dim(H)
        for (i in 1:nrow(X.vali))
        {
          H[i,,] = p[i]*(1-p[i])*(X.vali[i,]%*%t(X.vali[i,]))
        }
        
        H_new=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
        H_new=H[1,,]
        for (i in 2:nrow(X.vali))
        {
          H_new=H_new+H[i,,]
        }
        H_new=-H_new
        
        # Calculate w
        w[,c+1] = w[,c] - eta*ginv(H_new)%*%g_new
        
        # Calculate the difference
        diff1=sum(sqrt((w[,c+1]-w[,c])^2))
        if(is.nan(diff1))
          diff1=0
        
        ##if the iteration reach the limit, the break
        if(c==9999) diff1=0
      }
      
      ####calculate Spammer score, higher is better
      S=NULL
      for (j in 1:ncol(Y)) S[j]=(alpha[j,c]+beta[j,c]-1)^2
      
      ##if there is no more group
    } else{
      Y.corr=matrix(0,nrow=2,ncol=1)
      Y.corr[1,]=c(annot,annot.exp[c3,(1:length(which(S>epsilon.S)))])
      Y.corr[2,]=1:1
      
      library(polynom)
      
      good.a=1
      
      # Initialization of mu:
      R = 1
      mu = Y
      
      eta  = 1
      g = matrix(0,nrow=nrow(X.vali),ncol=ncol(X.vali))
      a = NULL
      b = NULL
      p = NULL
      diff1= 10
      H=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
      
      
      # Calculate alpha1 & beta1:
      alpha = matrix(0,nrow=R,ncol=10000)
      beta = matrix(0,nrow=R,ncol=10000)
      c=1
      library(MASS)
      
      alpha[1,c] = (sum(mu*Y))/(sum(mu))
      beta[1,c] = (sum((1-mu)*(1-Y)))/(sum(1-mu))
      
      #Calculate w
      y2=NULL
      for ( i in 1:nrow(X.vali))
      {
        if (mu[i] < 0.5)
        {
          y2[i] = 0
        }else
        {
          y2[i]=1
        }
      }
      w = matrix(0,nrow=ncol(X.vali),ncol=30000)
      w[,c] = ginv(X.vali)%*%y2
      
      
      # Calculate p:
      p = 1/(1+exp(-(t(w[,c])%*%t(X.vali))))
      #Calculate g:
      for (i in 1:nrow(X.vali))
      {
        g[i,] = (mu[i]-p[i])*X.vali[i,]
      }
      
      g_new=NULL
      g_new=colSums(g)
      
      # Calculate H^-1:
      H = array(0,dim=c(nrow(X.vali),ncol(X.vali),ncol(X.vali)))
      for (i in 1:nrow(X.vali))
      {
        H[i,,] = p[i]*(1-p[i])*(X.vali[i,]%*%t(X.vali[i,]))
      }
      
      H_new=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
      H_new=H[1,,]
      for (i in 2:nrow(X.vali))
      {
        H_new=H_new+H[i,,]
      }
      H_new=-H_new
      
      # Calculate the new w
      w[,c+1] = w[,c] - eta*ginv(H_new)%*%g_new
      
      # Calculate the difference
      diff1=sum(sqrt((w[,c+1]-w[,c])^2))
      
      ########### E-STEP : ###################
      
      # Calculate a & b:
      while (diff1 > convergence.Limit){
        for (i in 1:nrow(X.vali))
        {
          a[i] = prod((alpha[,c]^(Y[i,]))*((1-alpha[,c])^(1-Y[i,])))
          b[i] = prod((beta[,c]^(1-Y[i,]))*((1-beta[,c])^(Y[i,])))
          
        }
        
        for (i in 1:nrow(X.vali)){
          if (a[i]==0) a[i]=10^{-1}
          if (b[i]==0) b[i]=10^{-1}
        }
        
        # Calculate p:
        p = 1/(1+exp(-(t(w[,c+1])%*%t(X.vali))))
        
        # Calculate the new mu
        for (i in 1:nrow(X.vali))
        {
          mu[i] = (a[i]*p[i])/(a[i]*p[i]+b[i]*(1-p[i]))
        }
        
        c=c+1
        alpha[1,c] = (sum(mu*Y))/(sum(mu))
        
        for (i in 1:nrow(X.vali))
        {
          g[i,] = (mu[i]-p[i])*X.vali[i,]
        }
        
        g_new=NULL
        g_new=colSums(g)
        
        H = array(0,dim=c(nrow(X.vali),ncol(X.vali),ncol(X.vali)))
        for (i in 1:nrow(X.vali))
        {
          H[i,,] = p[i]*(1-p[i])*(X.vali[i,]%*%t(X.vali[i,]))
        }
        
        H_new=matrix(0,nrow=ncol(X.vali),ncol=ncol(X.vali))
        H_new=H[1,,]
        for (i in 2:nrow(X.vali))
        {
          H_new=H_new+H[i,,]
        }
        H_new=-H_new
        
        w[,c+1] = w[,c] - eta*ginv(H_new)%*%g_new
        
        diff1=sum(sqrt((w[,c+1]-w[,c])^2))
        if(is.nan(diff1))
          diff1=0
        if(c==9999) diff1=0
      }

      #### Calculate spammer score
      
      S=NULL
      S=(alpha[1,c]+beta[1,c]-1)^2
    }
    
    c3=c3+1
    #inspired from Condorcet Jury's theorem
    if(length(which(S>epsilon.S)) > 0){
      annot.exp[c3,1:length(which(S>epsilon.S))]=which(S>epsilon.S)
      annot.exp[c3,1:length(which(S>epsilon.S))]=Y.corr[1,which(S>epsilon.S)]
    }else{
      #  annot.exp[c3,1:length(which(S==max(S)))]=which(S==max(S))
      #  annot.exp[c3,1:length(which(S==max(S)))]=Y.corr[1,which(S==max(S))]
      
      ordre.S=order(S, na.last = TRUE, decreasing = TRUE)
      temp.epsilon.S=S[ordre.S[nb.S]]
      annot.exp[c3,1:length(which(S>=temp.epsilon.S))]=which(S>=temp.epsilon.S)
      annot.exp[c3,1:length(which(S>=temp.epsilon.S))]=Y.corr[1,which(S>=temp.epsilon.S)]
    }
    
    if(c3==5)
      break
  }
  ###################the iteration is finished########
  
  
  Y=read.table(Yname,header=FALSE)
  Y=as.matrix(Y)
  colnames(Y)=seq(1,ncol(Y),1)

  RC.f = NULL
  ##if there are several final reliable annotators
  if(length(which(S>epsilon.S)) > 1){
    round(S,5)
    #rank:
    Maxconf= max(S)
    Maxaa=annot.exp[c3,which(S==max(S))]
    RC.f = annot.exp[c3,1:length(which(S>epsilon.S))]
    Y=Y[,annot.exp[c3,1:length(which(S>epsilon.S))]]
    
    R = ncol(Y)
    mu = NULL
    for (i in (1:nrow(X.vali)))
    {
      mu[i] = (1/R)*sum(Y[i,])  #define the final prediction by applying Majority Voting
    }
    ##if no reliable annotator, then select only one  
  }else{
    RC.f = annot.exp[c3,1:length(which(S==max(S)))]
    Y=Y[,annot.exp[c3,1:length(which(S==max(S)))]]
    mu <- Y
  }
  
  #compare and print the result
  
  Yb.vali=read.table(paste(folder,"Yb-vali.txt", sep=""),header=FALSE)
  Yb.vali=Yb.vali[,1]
  
  #        folder= paste(filename,expnum,threshold, sep = "/")  # paste(filename,expnum, sep="/")
  
  # write.table(mu,paste(folder,"/mu-CondexpertEM","_", entro,"_",threshold,".txt",sep=""),row.names=FALSE,col.names=TRUE)
  
  library(PresenceAbsence)
  result2<-matrix(0,nrow=nrow(X.vali),ncol=3)
  result2[,1]<-1:nrow(X.vali)
  result2[,2]<-as.numeric(Yb.vali)
  result2[,3]=mu
  result2<-as.data.frame(result2)
  dimnames(result2)[[2]]<-c("ID" ,"Observed","CRA")
  dimnames(result2)[[2]]
  is.data.frame(result2)
  
  
  #########################computation time
  proc.time() - ptm2
  time.result= proc.time() - ptm2
  
  # header = paste("ROC curve of RASA for ", filename ," data \n based on 40 Annotators", sep = "")
  # png(filename=paste(folder,"/ROC_CRA40.png", sep=""))
  # auc.roc.plot(result2,col=TRUE,line.type=TRUE,main=header)
  # dev.off()
  
  auc.result = auc(result2, st.dev=FALSE)
  acc.result = 0
  conf.matrix = NULL
  conf.matrix[1]=0  #TP
  conf.matrix[2]=0  #TN
  conf.matrix[3]=0  #FP
  conf.matrix[4]=0  #FN
  
  mu2 = as.numeric(mu)
  for (i in 1:nrow(X.vali)){
    pred=0
    if(mu2[i]>0.5)
      pred=1
    
    if(pred==Yb.vali[i]){
      acc.result= acc.result + 1
      if(pred==1){conf.matrix[1]=conf.matrix[1]+1}
      else{conf.matrix[2]=conf.matrix[2]+1}
    }else{
      if(pred==1){conf.matrix[3]=conf.matrix[3]+1}
      else{conf.matrix[4]=conf.matrix[4]+1}
    }    
  }
  acc.result= acc.result/nrow(X.vali)
  expertS_result = paste(time.result["elapsed"], " ",auc.result, " ",acc.result, " ",threshold, " ",convergence.Limit  )
  
  matrix = paste(conf.matrix[1],conf.matrix[2],conf.matrix[4],conf.matrix[3],sep = ";")
  paste(matrix,"@@", sep="")
  
  # write.table(RC.f,paste(folder,"/RC.f","_", entro,"_",threshold,".txt", sep=""),row.names=FALSE,col.names=FALSE)
  
  # write.table(expertS_result,paste(folder,"/CRA_result","_", entro,"_",threshold,".txt", sep=""),row.names=FALSE,col.names=FALSE)
  

  ############################## List of reliable annotators
  # annot.exp[c3,1:length(which(S>epsilon.S))]
  
  
  ###############################################################
  ##...
}
