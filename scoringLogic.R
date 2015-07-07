## Scoring Logic
#setwd ("/stuartillson/python_projects/final_cheese")
train <- read.csv ('updated_set_run.csv', header=T, stringsAsFactors = F)

###

library (data.table)
library (reshape2)
library (ggplot2)
library (Metrics)
library (randomForest)
library (RRF)
library (nnet)
library (brnn)
library (glmnet)
library (forecast)
library (ggplot2)
library (TTR)
library (Metrics)



igNms <- grep ("ig", names (train))
train <- train [,-igNms]
setDT (train)
#train [,Date:=as.POSIXct (Date, format = "%d/%m/%Y", tz = "UTC", origin = "1970-1-1")]
myTgt <- "Dairy.Products_NASS_Total_Cheese_1k_lbs_production"
train [,flag_predict := ifelse (is.na(Dairy.Products_NASS_Total_Cheese_1k_lbs_production), 0, 1)]
train_orig <- copy(train)
myTgt <- c(myTgt, "Dairy.Products_NASS___Nonfat_dry_milk._human_1k_lbs_production", "Dairy.Products_NASS___Skim_milk_powder._total_1k_lbs_production")
train [,Slaughter_Slaughter_SLGHTR_Cattle_1k_head := as.numeric(Slaughter_Slaughter_SLGHTR_Cattle_1k_head)]
train [,Slaughter_Slaughter_Steers_1k_head := as.numeric (Slaughter_Slaughter_Steers_1k_head)]
train [,Dairy.Products_NASS___Dry_skim_milk._animal_1k_lbs_production := as.numeric (Dairy.Products_NASS___Dry_skim_milk._animal_1k_lbs_production)]
train [,Dairy.Products_NASS___Dry_whole_milk_1k_lbs_production := as.numeric (Dairy.Products_NASS___Dry_whole_milk_1k_lbs_production)]


sumNms <- unique (c(names (train) [grep ("Slaughter", names (train))], names (train) [grep ("Dairy", names (train))]))
sumNms <- sumNms [!(sumNms %in% myTgt)]

# aggregate at the point of predict
train [,id:=1:nrow(train)]
temp <- train[train$flag_predict==1,]
temp [,flag_predict := 1:nrow(temp)]
temp <- temp [,c("id", "flag_predict"),with=F]
train [,id:=1:nrow(train)]
train <- merge (train, temp, by = "id", all.x=TRUE)
train [,flag_predict.x:=NULL]
setnames (train, "flag_predict.y", "flag_predict")
train [,flag_predict := ifelse (is.na(flag_predict), 0, flag_predict)]

train_agg_mean <- train [,names (train) [!(names (train) %in% c("id", myTgt, "Date", sumNms))],with=F] [,lapply(.SD,mean,na.rm=TRUE ), by = flag_predict]
train_agg_sum <- train [,names (train) [!(names (train) %in% c("id", myTgt, "Date"))],with=F][,c(sumNms,"flag_predict"),with=F] [,lapply(.SD,sum,na.rm=TRUE ), by = flag_predict]
train_agg <- merge (train_agg_mean, train_agg_sum, by = "flag_predict")
temp <- train [train$flag_predict != 0,]
temp <- temp [,list(flag_predict, Date)]
train_agg <- merge (train_agg, temp, by = "flag_predict", all.x=TRUE)
train_agg <- train_agg [train_agg$flag_predict >0,]

stockNms <- names (train) [grep ("Stock", names (train))]
ignoreNms <- c(myTgt, "flag_predict", "id", "Date")
train_agg_model <- copy (train_agg)
train_model <- copy (train)

train <- train_model
train_agg <- train_agg_model

temp <- train [,c(myTgt, "flag_predict"), with=F]
train_agg <- merge (train_agg, temp, by = "flag_predict")

# We will build for 1 - month in advance, 2 - month in advance, 3 - month - 4 - month 5-month and 6-month in advance
myTgt <- "Dairy.Products_NASS_Total_Cheese_1k_lbs_production"
setnames (train_agg, myTgt, "actual")
train_agg [,Date_posix := as.POSIXct (Date, format = "%d/%m/%Y", tz = "UTC", origin="1970-1-1")]
train_agg [,mth := month (Date_posix)]
train_agg [,yr := year (Date_posix)]
############# Build a random forest model
train_agg_rf <- copy (train_agg)
for (nm in names (train_agg_rf)) {
	setnames (train_agg_rf, nm, "temp")
	idx <- which (names (train_agg_rf) %in% "temp")
	mank <- train_agg_rf$temp
	mank <- ifelse (is.na(mank), -999, mank)
	set (train_agg_rf, j=idx, value = mank)
	setnames (train_agg_rf, "temp", nm)
}

load ('trained_models_yequalsfx.rData')

train_agg_rf <- train_agg_rf[which.max(train_agg_rf$flag_predict),]

# for fold-1
myNms <- unique (c(stockNms, sumNms, "actual"))
pred_df <- NULL
for (k in c(1:12)) {
	print (k)
	temp <- copy (train_agg_rf)


myTS <- ts (train_agg$actual, start = c(1994, 1), frequency =12)

	# Holt Winters method
	myTS_hw <- HoltWinters(myTS, beta=FALSE, gamma=FALSE)
	pred_df <- rbind (pred_df, data.frame (method = "HoltWinters", fold = k, pred = paste ( forecast (myTS_hw, h=6)$mean, collapse = " ")))

	# ARIMA method
	myTS_arima <-  auto.arima (myTS)
	pred_df <- rbind (pred_df, data.frame (method = "ARIMA", fold = k, pred = paste ( forecast (myTS_arima, h=6)$mean, collapse = " ")))

	# Naive Method
	pred_df <- rbind (pred_df, data.frame (method = "naive", fold = k, pred = paste ( snaive (myTS, h=6)$mean, collapse = " ")))
	
	# Random Walk
	pred_df <- rbind (pred_df, data.frame (method = "naive", fold = k, pred = paste ( rwf (myTS, h=6)$mean, collapse = " ")))
	
	# ETS
	myTS_arima <-  ets  (myTS)
	pred_df <- rbind (pred_df, data.frame (method = "ETS", fold = k, pred = paste ( forecast (myTS_arima, h=6)$mean, collapse = " ")))

	# Exponential Damped Trend
	myTS_hwexpdamped<- holt (myTS, exponential = TRUE, damped = TRUE)
	pred_df <- rbind (pred_df, data.frame (method = "hwexpdamped", fold = k, pred = paste ( forecast (myTS_hwexpdamped, h=6)$mean, collapse = " ")))

	# Damped Trend
	myTS_hwdamped<- holt (myTS, damped = TRUE)
	pred_df <- rbind (pred_df, data.frame (method = "hwdamped", fold = k, pred = paste ( forecast (myTS_hwdamped, h=6)$mean, collapse = " ")))

	# Holt Winter Seasonal Trend
	myTS_holtwinterseasonal<- ets (myTS, lambda = NULL, opt.crit = "mae")
	pred_df <- rbind (pred_df, data.frame (method = "holtwinterseasonal", fold = k, pred = paste ( forecast (myTS_holtwinterseasonal, h=6)$mean, collapse = " ")))

	# Holt Winter Seasonal BoxCox Trend
	myTS_holtwinterseasonalboxcox <- ets (myTS, lambda = NULL, opt.crit = "mae")
	pred_df <- rbind (pred_df, data.frame (method = "holtwinterseasonalboxcox", fold = k, pred = paste ( forecast (myTS_holtwinterseasonal, h=6)$mean, collapse = " ")))


	# Random Forest Predictions
	pred_array <- NULL
	opList <- rfList [[k]]
	for (j in c(1:6)) {
		myRF <- opList [[j]]
		pred <- predict (myRF, as.data.frame (temp[,myNms,with=F]), type = "response")
		pred_array <- c(pred_array, pred)
	}
	pred_df <- rbind (pred_df, data.frame (method = "randomforest", fold = k, pred = paste (pred_array, collapse = " ")))

	# Random Forest No Act Predictions
	pred_array <- NULL
	opList <- rfListNoAct [[k]]
	for (j in c(1:6)) {
		myRF <- opList [[j]]
		pred <- predict (myRF, as.data.frame (temp[,myNms,with=F]), type = "response")
		pred_array <- c(pred_array, pred)
	}
	pred_df <- rbind (pred_df, data.frame (method = "randomforestnoact", fold = k, pred = paste (pred_array, collapse = " ")))

	# RRF No Act Predictions
	pred_array <- NULL
	opList <- rrfList [[k]]
	for (j in c(1:6)) {
		myRF <- opList [[j]]
		pred <- predict (myRF, as.data.frame (temp[,myNms,with=F]), type = "response")
		pred_array <- c(pred_array, pred)
	}
	pred_df <- rbind (pred_df, data.frame (method = "RRF", fold = k, pred = paste (pred_array, collapse = " ")))

	# BRNN Predictions
	pred_array <- NULL
	opList <- brnnList [[k]]
	for (j in c(1:6)) {
		myBRNN <- opList [[j]]
		pred <- predict (myBRNN, as.matrix( as.data.frame (temp[,myNms,with=F])), type = "response")
		pred_array <- c(pred_array, pred)
	}
	pred_df <- rbind (pred_df, data.frame (method = "BRNN", fold = k, pred = paste (pred_array, collapse = " ")))

	# BRNN3 Predictions
	pred_array <- NULL
	opList <- brnn3List [[k]]
	for (j in c(1:6)) {
		myBRNN <- opList [[j]]
		pred <- predict (myBRNN, as.matrix( as.data.frame (temp[,myNms,with=F])), type = "response")
		pred_array <- c(pred_array, pred)
	}
	pred_df <- rbind (pred_df, data.frame (method = "BRNN3", fold = k, pred = paste (pred_array, collapse = " ")))

	# BRNN4 Predictions
	pred_array <- NULL
	opList <- brnn4List [[k]]
	for (j in c(1:6)) {
		myBRNN <- opList [[j]]
		pred <- predict (myBRNN, as.matrix( as.data.frame (temp[,myNms,with=F])), type = "response")
		pred_array <- c(pred_array, pred)
	}
	pred_df <- rbind (pred_df, data.frame (method = "BRNN4", fold = k, pred = paste (pred_array, collapse = " ")))


	# GLMNET Predictions
	pred_array <- NULL
	opList <- glmnetList [[k]]
	for (j in c(1:6)) {
		myGLMNET <- opList [[j]]
		predict (myGLMNET, as.matrix( as.data.frame (rbind (temp[,myNms,with=F],temp[,myNms,with=F]))), type = "response", s=min(myGLMNET$lambda))[1,1]
		pred_array <- c(pred_array, pred)
	}
	pred_df <- rbind (pred_df, data.frame (method = "LinearRegressorRegularized", fold = k, pred = paste (pred_array, collapse = " ")))


	
}



write.csv (pred_df, file = 'pred_df.csv', row.names = F)


