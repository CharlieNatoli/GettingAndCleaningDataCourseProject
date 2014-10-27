
#1: download and unzip file

 url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 download.file(url = url, destfile = "data", method = "curl")
unzip(zipfile = "data", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".", unzip = "internal",setTimes = FALSE)

#2: read files into R

setwd("UCI HAR Dataset")
files =list.files(all.files = FALSE, recursive = TRUE)
for (i in 1:length(files)){
  table = read.table(file = files[i], header = FALSE, sep = "", fill = TRUE);
  assign(x = gsub("/" ,".", paste ("", files[i], sep = "", collapse = NULL)), value = table) ;
  }


#3: merge all of the files together into a dataframe called "Combined Data"

test = cbind(test.subject_test.txt,test.y_test.txt,test.X_test.txt)
train = cbind(train.subject_train.txt,train.y_train.txt,train.X_train.txt)

CombinedData = rbind(test,train)

names(CombinedData)[1]<-paste("Subject")
names(CombinedData)[2]<-paste("ActivityNumber")
CombinedData = cbind(CombinedData[,1:2],ActivityName = 1,CombinedData[3:ncol(CombinedData)]) 
for (i in 1:nrow(CombinedData)){
  CombinedData[i,3] =  as.character(activity_labels.txt[activity_labels.txt$V1 == CombinedData[i,2] ,2])
  }
for (i in 4:ncol(CombinedData)){
   names(CombinedData)[i]<-paste(features.txt[i-3,2] )
  }

#4: make a vector with all of the names variables that are a mean or a standard deviation titled "MeanStdVariables"

MeanStdVariables = c(as.character(features.txt[grep("mean", features.txt$V2),2]) ,as.character(features.txt[grep("std", features.txt$V2),2]) )

#5:  use "MeanStdVariables" to make a data frame that contains all of the variables that are a mean or a standard deviation  

MeanStdOnly = data.frame(Subject = CombinedData$Subject,ActivityNumber = CombinedData$ActivityNumber,ActivityName = CombinedData$ActivityName)

for (i in 1:length(MeanStdVariables)){
  MeanStdOnly = cbind(MeanStdOnly,NewColumn = subset(CombinedData,select = MeanStdVariables[i]))
  colnames (MeanStdOnly)[i+3] = MeanStdVariables[i]
  }
 
#6: Make a dataframe with means of each of those variables

Means = data.frame(Subject  = rep(1:30, times = 1, each = 6), Activity = rep(1:6, times = 30, each = 1),ActvityName = 1)
 for (i in 1:nrow(Means)){
   Means[i,3] =  as.character(activity_labels.txt[activity_labels.txt$V1 == Means[i,2] ,2])
 }
 
#7: populate it with means i = rows, j = colums

for (j in 1:length(MeanStdVariables)){
  Means$NewColumn = 1
  colnames (Means)[j+3] = MeanStdVariables[j]
  for (i in 1: nrow(Means)){
    Means[i,j+3] = mean(MeanStdOnly[MeanStdOnly$Subject == Means[i,1] & MeanStdOnly$ActivityNumber == Means[i,2] ,j+3])
    }
  }

 #8: export the dataset
 
 write.table(Means, file = "CourseProject", append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
