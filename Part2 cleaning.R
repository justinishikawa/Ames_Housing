## Upload Cleaned Data
  final_proj_data <- readRDS("~/Downloads/ames.RDS")
  head(final_proj_data)
  
  numeric_names <- c("SalePrice", "LotFrontage", "LotArea", "MasVnrArea", "1stFlrSF", "2ndFlrSF", "LowQualFinSF", "GrLivArea", "GarageArea", "WoodDeckSF",
                     "OpenPorchSF", "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea", "FinishedRatio", "YearBuilt", "YearRemodAdd",
                     "GarageYrBlt", "YrSold")
  numeric_data <- final_proj_data[,numeric_names]
  head(numeric_data)
  cat_data <- final_proj_data[, -which(names(final_proj_data) %in% numeric_names)]
  head(cat_data)
  cat_data_factor <- as.data.frame(sapply(cat_data, as.factor))
  head(cat_data_factor)
  
  # Merge data sets together
  final_data <- as.data.frame(c(numeric_data, cat_data_factor))
  head(final_data)
  
  # Get rid of NaN in FinishedRatio
  final_data$FinishedRatio<-ifelse(is.nan(final_data$FinishedRatio), 0,final_data$FinishedRatio)
  head(final_data)
  
  # Get rid of Na in GarageYrBuilt
  final_data$GarageYrBlt<-ifelse(is.na(final_data$GarageYrBlt),mean(final_data$GarageYrBlt, na.rm=TRUE) ,final_data$GarageYrBlt)
  
  
  # Split Train and Test
  set.seed(1234)
  training.set.rows <- sample(x = 1460, size = 1000, replace = FALSE)
  training.set <- final_data[training.set.rows, ]
  test.set <- final_data[-training.set.rows, ]
  
  # Get Variables to be used in model: Sale price is y, numerics are predictors
  # Split Train and Test
  set.seed(1234)
  training.set.rows <- sample(x = 1460, size = 1000, replace = FALSE)
  num.training.set <- numeric_data[training.set.rows, ]
  num.test.set <- numeric_data[-training.set.rows, ]