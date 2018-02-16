suppressMessages(library(tidyverse))
suppressMessages(library(forcats))

#opening file
re<-read_csv('kaggle_data.csv',col_names = TRUE)
pre_process<function(dataframe){
  #Removing id variable as it doesnt have any information
  dataframe<-dataframe[,2:81]
  #MSSubClass-recoding this to categories as its not ordinal Not sure what to do here atm
  #MSZoning dummy coding
  dataframe$MSZoning<-as.numeric(parse_factor(dataframe$MSZoning, unique(dataframe$MSZoning)))
  #Street Dummy Coding
  dataframe$Street<-as.numeric(parse_factor(dataframe$Street, unique(dataframe$Street)))
  #Alley Dummy Coding
  dataframe$Alley<-as.numeric(parse_factor(dataframe$Alley, unique(dataframe$Alley)))
  #Lotshape Dummy Coding
  dataframe$Lotshape<-as.numeric(parse_factor(dataframe$Lotshape, unique(dataframe$Lotshape)))
  #LandContour
  dataframe$LandContour<-as.numeric(parse_factor(dataframe$LandContour, unique(dataframe$LandContour)))
  #Utilities
  
  
}