proppattern<-function(directory,property){
  ##Directory name of csv containing data, only in cwd
  ##csv file should have titles of the format p1.q1 etc, read in excel as p1 q1
  ##property number 1-16 of property desired, no default set.
  require(ggplot2)
  if(missing(directory)|missing(property))
    return("Please enter both directory and property parameters")
  else{
  
  filename<-paste(getwd(),"/",directory,".csv",sep="")
  ##Set path variable to current wd, name csv of data file

  data<-read.csv(filename,header=TRUE)
  ##Read in your csv, ensure that it knows there is a header here.
  
  data<-as.data.frame(data)
  ##Ensures that data is a dataframe, rather than matrix or anything else.
  ##While this should already be the case, just double checking that we 
  ##have what we need here.
  
 # sapply(property,
 propno<-paste("^p",property,".q",sep="")#)
  ##set the property number desired, ensure it's in the format of the csv's head

  propqs<-grep(propno,names(data))
  ##search for the property number in the head of the csv
  propdata<-data[propqs]
  ##Subset the initial data set (csv) to only be a dataframe of your desired
  ##property's three questions.
  ##At this point we have 1000ish obs of 3 vars, propdata is our desired property
  ## 's data set of all data in that property, over all three questions.
  
  rows<-nrow(propdata)
  ##countsnumber of rows in propdata
  propvector<-vector()#length=3*rows
  ##creates empty propvector for filling
  statsvector<-vector()#length=20
  countvector<-vector()#length=20
  ##Creates empty statsvector also for filling
  
  
  for(i in 1:rows){
    rowi<-paste(propdata[i,],sep="",collapse="")
    propvector<-c(propvector,rowi)
  }
 ##goes through each row of data for selected prpoerty, and collapeses
 ##the questions worth of data into a question number long string of 0/1.
 ##Populates propvector with a string element for each row of data.
  
  for (i in 1:length(propvector)){
    ##read first value
    ##Check in stats vector for its equal
    ##if found add 1 to stats vector count of value
    ##Else create new entry, value, count = 1
    index<-propvector[i]
    ##set the index to something shorter and more understandable
    loc<-ifelse(sapply(statsvector[grep(index,statsvector)],identical,index),
                grep(index,statsvector),NA)
    ##search for the value of the row value (index) in the statvector 
    ##Set position of identical strings in statvector to loc and anything else to NA
    loc<-na.omit(loc)
    ##remove NA from loc (AKA cleanup time)
    ifelse(identical(length(loc),as.integer(0)),
           {statsvector<-c(statsvector,index); countvector<-c(countvector,1)},
          countvector[loc]<-countvector[loc]+1
          )
   ##if loc is empty (value of index not found in statvector)
   ##append value of index to statvector and append 1 to countvector
   ##else, set countvector at loc to countvector at loc + 1 -- increase
   ##the value of countvector by 1.
   
  }

 resultantcount<-data.frame(statsvector,countvector)
 print(resultantcount)
 qplot(statsvector,countvector,geom="bar",stat="identity")
}
}


