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
  
  #LotFrontage Rescale Replacing NA with means
  dataframe$LotFrontage<-ifelse(is.na(dataframe$LotFrontage)==TRUE,mean(dataframe$LotFrontage, na.rm=TRUE),dataframe$LotFrontage)
  dataframe$LotFrontage<-scale(dataframe$LotFrontage, center=TRUE, scale=TRUE)
  
  #LotArea Rescale Replacing NA with means
  dataframe$LotArea<-ifelse(is.na(dataframe$LotArea)==TRUE,mean(dataframe$LotArea, na.rm=TRUE),dataframe$LotArea)
  dataframe$LotArea<-scale(dataframe$LotArea, center=TRUE, scale=TRUE)
  
  #Street Dummy Coding
  dataframe$Street<-as.numeric(parse_factor(dataframe$Street, unique(dataframe$Street)))
  
  #Alley Dummy Coding
  #dataframe$Alley<-as.numeric(parse_factor(dataframe$Alley, unique(dataframe$Alley)))
  #Killing Alley
  dataframe$Alley<-NULL
  
  #Lotshape Dummy Coding
  dataframe$Lotshape<-as.numeric(parse_factor(dataframe$Lotshape, unique(dataframe$Lotshape)))
 
  #LandContour
  dataframe$LandContour<-as.numeric(parse_factor(dataframe$LandContour, unique(dataframe$LandContour)))
  
  #Utilities
  dataframe$Utilities<-as.numeric(parse_factor(dataframe$Utilities, unique(dataframe$Utilities)))

  #LotConfig
  dataframe$LotConfig<-as.numeric(parse_factor(dataframe$LotConfig, unique(dataframe$LotConfig)))

  #LandSlope
  dataframe$LandSlope<-as.numeric(parse_factor(dataframe$LandSlope, unique(dataframe$LamdSlope)))

  #Classifying the neighborhoods by low medium and high and creating new column (Doesn't this artificially create an interaction w/price?
  #nbhdprice <- summarize(group_by(dataframe, Neighborhood), mean(SalePrice, na.rm=T)) 
  #nbhdprice[order(nbhdprice$`mean(SalePrice, na.rm = T)`),]
  #nbhdprice_lo <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 140000)
  #nbhdprice_med <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 200000 &
   #                       nbhdprice$`mean(SalePrice, na.rm = T)` >= 140000 )
                          
  #nbhdprice_hi <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 200000)
  #dataframe$nbhd_price_level[dataframe$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1
  #dataframe$nbhd_price_level[dataframe$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2
  #dataframe$nbhd_price_level[dataframe$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3```
  
  #Condition1
  dataframe$Condition1<-as.numeric(parse_factor(dataframe$Condition1, unique(dataframe$Condition1)))
  
  #Condition2
  dataframe$Condition2<-as.numeric(parse_factor(dataframe$Condition2, unique(dataframe$Condition2)))
  
  #BldgType
  dataframe$BldgType<-as.numeric(parse_factor(dataframe$BldgType, unique(dataframe$BldgType)))
  
  #HouseStyle
  dataframe$HouseStyle<-as.numeric(parse_factor(dataframe$HouseStyle, unique(dataframe$HouseStyle)))
  
  #OverallQuality & OverallCond already coded
  
  #YearBuilt / YearRemodAdd already coded
  
  #Roofstyle
  dataframe$RoofStyle<-as.numeric(parse_factor(dataframe$RoofStyle, unique(dataframe$RoofStyle)))
  
  #RoofMatl
  dataframe$RoofMatl<-as.numeric(parse_factor(dataframe$RoofMatl, unique(dataframe$RoofMatl)))
  
  #Exterior1s
  dataframe$Exterior1st<-as.numeric(parse_factor(dataframe$Exterior1st, unique(dataframe$Exterior1st)))
  
  #Exterior2nd
  dataframe$Exterior2nd<-as.numeric(parse_factor(dataframe$Exterior2nd, unique(dataframe$Exterior2nd)))
  
  #MasVnrType
  dataframe$MasVnrType<-as.numeric(parse_factor(dataframe$MasVnrType, unique(dataframe$MasVnrType)))
  
  #MasVnrArea_rescale putting zeros into NA's
  dataframe$MasVnrArea<-ifelse(is.na(dataframe$MasVnrArea)==TRUE, 0, dataframe$MasVnrArea)
  dataframe$MasVnrArea<-scale((dataframe$MasVnrArea, center=TRUE, scale=TRUE)
  
  #ExterQual
  dataframe$ExterQual<-as.numeric(parse_factor(dataframe$ExterQual, unique(dataframe$ExterQual)))
  
  #ExterCond
  dataframe$ExterCond<-as.numeric(parse_factor(dataframe$ExterCond, unique(dataframe$ExterCond)))
  
  #Foundation
  dataframe$Foundation<-as.numeric(parse_factor(dataframe$Foundation, unique(dataframe$Foundation)))
  
  #BsmtQual
  dataframe$BsmtQual<-as.numeric(parse_factor(dataframe$BsmtQual, unique(dataframe$BsmtQual)))
  
  #BsmntCond
  dataframe$BsmtCond<-as.numeric(parse_factor(dataframe$BsmtCond, unique(dataframe$BsmtCond)))

  #BsmtExposure
  dataframe$BsmtExposure<-as.numeric(parse_factor(dataframe$BsmtExposure, unique(dataframe$BsmtExposure)))
  
  #BsmtFinType1
  dataframe$BsmtFinType1<-as.numeric(parse_factor(dataframe$BsmtFinType1, unique(dataframe$BsmtFinType1)))
  
  #BsmtFinSF1 rescale
  dataframe$BsmtFinSF1<-scale(dataframe$BsmtFinSF1, center=TRUE, scale=TRUE)
  
  #BsmtFinType2
  dataframe$BsmtFinType2<-as.numeric(parse_factor(dataframe$BsmtFinType2, unique(dataframe$BsmtFinType2)))
  
  #BsmtFinSF1 rescale
  dataframe$BsmtFinSF2<-scale(dataframe$BsmtFinSF2, center=TRUE, scale=TRUE)
  
  




  
  
  
  

  




}