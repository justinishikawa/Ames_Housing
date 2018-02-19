suppressMessages(library(tidyverse))
suppressMessages(library(forcats))
pre_process<-function(dataframe){
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
  dataframe$LotShape<-as.numeric(parse_factor(dataframe$LotShape, unique(dataframe$LotShape)))
 
  #LandContour
  dataframe$LandContour<-as.numeric(parse_factor(dataframe$LandContour, unique(dataframe$LandContour)))
  
  #Utilities
  dataframe$Utilities<-as.numeric(parse_factor(dataframe$Utilities, unique(dataframe$Utilities)))

  #LotConfig
  dataframe$LotConfig<-as.numeric(parse_factor(dataframe$LotConfig, unique(dataframe$LotConfig)))

  #LandSlope
  dataframe$LandSlope<-as.numeric(parse_factor(dataframe$LandSlope, unique(dataframe$LandSlope)))

  #Neighborhood
  dataframe$Neighborhood<-as.numeric(parse_factor(dataframe$Neighborhood, unique(dataframe$Neighborhood)))
  
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
  dataframe$MasVnrArea<-scale(dataframe$MasVnrArea, center=TRUE, scale=TRUE)
  
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
  #dataframe$BsmtFinSF1<-scale(dataframe$BsmtFinSF1, center=TRUE, scale=TRUE)
  
  #BsmtFinType2
  dataframe$BsmtFinType2<-as.numeric(parse_factor(dataframe$BsmtFinType2, unique(dataframe$BsmtFinType2)))
  
  #BsmtFinSF2 rescale
  #dataframe$BsmtFinSF2<-scale(dataframe$BsmtFinSF2, center=TRUE, scale=TRUE)
  
  #Finished basement SF, unfinished and Total are all the rehashes of the same information.  I'm creating a new column that is finished ratio which is 
  #SF1+SF2/Total rescaled of course
  dataframe$FinishedRatio<-(dataframe$BsmtFinSF1+dataframe$BsmtFinSF2)/dataframe$TotalBsmtSF
  dataframe$FinishedRatio<-scale(dataframe$FinishedRatio, center=TRUE, scale=TRUE)
  
  #Killing finished SF 1 & 2, BsmtUnfSF, and TotalBsmt
  dataframe$BsmtFinSF1<-NULL; dataframe$BsmtFinSF2<-NULL; dataframe$BsmtFinSF2<-NULL; dataframe$BsmtUnfSF<-NULL; dataframe$TotalBsmtSF<-NULL
  
  #Heating
  dataframe$Heating<-as.numeric(parse_factor(dataframe$Heating, unique(dataframe$Heating)))
  
  #HeatingQC
  dataframe$HeatingQC<-as.numeric(parse_factor(dataframe$HeatingQC, unique(dataframe$HeatingQC)))
  
  #Central Air
  dataframe$CentralAir<-as.numeric(parse_factor(dataframe$CentralAir, unique(dataframe$CentralAir)))
  
  #Electrical
  dataframe$Electrical<-as.numeric(parse_factor(dataframe$Electrical, unique(dataframe$Electrical)))
  
  #1stFlrSF, 2nd FlrSF, LowQualFinSF and GrLivArea are all the same information.  Going to change column information into ratio then scale
  dataframe$'1stFlrSF'<-dataframe$'1stFlrSF'/dataframe$GrLivArea
  dataframe$'2ndFlrSF'<-dataframe$'2ndFlrSF'/dataframe$GrLivArea
  dataframe$LowQualFinSF<-dataframe$LowQualFinSF/dataframe$GrLivArea
  dataframe$'1stFlrSF'<-scale(dataframe$'1stFlrSF', center=TRUE, scale=TRUE)
  dataframe$'2ndFlrSF'<-scale(dataframe$'2ndFlrSF', center=TRUE, scale=TRUE)
  dataframe$LowQualFinSF<-scale(dataframe$LowQualFinSF, center=TRUE, scale=TRUE)
  dataframe$GrLivArea<-scale(dataframe$GrLivArea, center=TRUE, scale=TRUE)
  
  #BsmtFullBath
  dataframe$BsmtFullBath<-as.numeric(parse_factor(dataframe$BsmtFullBath, unique(dataframe$BsmtFullBath)))
  
  #BsmtHalfBath
  dataframe$BsmtHalfBath<-as.numeric(parse_factor(dataframe$BsmtHalfBath, unique(dataframe$BsmtHalfBath)))
  
  #HalfBath
  dataframe$HalfBath<-dataframe$HalfBath+1
  
 
   #KitchenAbvGr
  dataframe$KitchenAbvGr<-as.numeric(parse_factor(dataframe$KitchenAbvGr, unique(dataframe$KitchenAbvGr)))
  
  #KitchenQual
  dataframe$KitchenQual<-as.numeric(parse_factor(dataframe$KitchenQual, unique(dataframe$KitchenQual)))
  
  #TotRmsAbvGrd
  dataframe$TotRmsAbvGrd<-as.numeric(parse_factor(dataframe$TotRmsAbvGrd, unique(dataframe$TotRmsAbvGrd)))
  
  #Functional
  dataframe$Functional<-as.numeric(parse_factor(dataframe$Functional, unique(dataframe$Functional)))
  
  #Fireplaces
  dataframe$Fireplaces<-as.numeric(parse_factor(dataframe$Fireplaces, unique(dataframe$Fireplaces)))
  
  #FireplaceQu
  dataframe$FireplaceQu<-as.numeric(parse_factor(dataframe$FireplaceQu, unique(dataframe$FireplaceQu)))
  
  #GarageType
  dataframe$GarageType<-as.numeric(parse_factor(dataframe$GarageType, unique(dataframe$GarageType)))
  
  #Garage Year Already category
  
  #GarageFinish
  dataframe$GarageFinish<-as.numeric(parse_factor(dataframe$GarageFinish, unique(dataframe$GarageFinish)))
  
  #GarageCars Already category
  
  #GarageArea rescale
  dataframe$GarageArea<-scale(dataframe$GarageArea, center=TRUE, scale=TRUE)
  
  #GarageQual
  dataframe$GarageQual<-as.numeric(parse_factor(dataframe$GarageQual, unique(dataframe$GarageQual)))
  
  #GarageCond
  dataframe$GarageCond<-as.numeric(parse_factor(dataframe$GarageCond, unique(dataframe$GarageCond)))
  
  #PavedDrive
  dataframe$PavedDrive<-as.numeric(parse_factor(dataframe$PavedDrive, unique(dataframe$PavedDrive)))
  
  #WoodDeckSF
  dataframe$WoodDeckSF<-scale(dataframe$WoodDeckSF, center=TRUE, scale=TRUE)
  
  #OpenPorchSF
  dataframe$OpenPorchSF<-scale(dataframe$OpenPorchSF, center=TRUE, scale=TRUE)
  
  #EnclosedPorch
  dataframe$EnclosedPorch<-scale(dataframe$EnclosedPorch, center=TRUE, scale=TRUE)
  
  #3SsnPorch
  dataframe$'3SsnPorch'<-scale(dataframe$'3SsnPorch', center=TRUE, scale=TRUE)
  
  #ScreenPorch
  dataframe$ScreenPorch<-scale(dataframe$ScreenPorch, center=TRUE, scale=TRUE)
  
  #PoolArea
  dataframe$PoolArea<-scale(dataframe$PoolArea, center=TRUE, scale=TRUE)
  
  #PoolQC  
  dataframe$PoolQC<-as.numeric(parse_factor(dataframe$PoolQC, unique(dataframe$PoolQC)))
  
  #Fence
  dataframe$Fence<-as.numeric(parse_factor(dataframe$Fence, unique(dataframe$Fence)))
  
  #MiscFeature & Misc Value Remove until we know what do with it 
  #dataframe$MiscFeature<-as.numeric(parse_factor(dataframe$MiscFeature, unique(dataframe$MiscFeature)))
  #dataframe$MiscVal<-scale(dataframe$MiscVal, center=TRUE, scale=TRUE)
  dataframe$MiscFeature<-NULL; 
  dataframe$MiscVal<-NULL
  
  #Month and Year Sold -Leaving for now
  
  #SaleType
  dataframe$SaleType<-as.numeric(parse_factor(dataframe$SaleType, unique(dataframe$SaleType)))
  
  #SaleCondition
  dataframe$SaleCondition<-as.numeric(parse_factor(dataframe$SaleCondition, unique(dataframe$SaleCondition)))
  
  #SalesPrice Dependent Variable not rescaling
  return(dataframe)

  }