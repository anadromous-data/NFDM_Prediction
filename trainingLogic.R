setwd ("/home/kiran/milk/cme/modifieddatasets")
library (data.table)
library (reshape2)
library (ggplot2)
library (Metrics)
library (randomForest)
library (RRF)
library (nnet)
library (brnn)


train <- read.csv ('test_set_modified_v6.csv', header=T, stringsAsFactors = F)
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
train_agg [,actual_1 := c(train_agg$actual [2:nrow(train_agg)], rep(-1,1))]
train_agg [,actual_2 := c(train_agg$actual [3:nrow(train_agg)], rep(-1,2))]
train_agg [,actual_3 := c(train_agg$actual [4:nrow(train_agg)], rep(-1,3))]
train_agg [,actual_4 := c(train_agg$actual [5:nrow(train_agg)], rep(-1,4))]
train_agg [,actual_5 := c(train_agg$actual [6:nrow(train_agg)], rep(-1,5))]
train_agg [,actual_6 := c(train_agg$actual [7:nrow(train_agg)], rep(-1,6))]

train_agg [,lapply(.SD, function (x) length (which(is.na(x)))/length(x))]
train_agg [,Date_posix := as.POSIXct (Date, format = "%d/%m/%Y", tz = "UTC", origin="1970-1-1")]
train_agg [,mth := month (Date_posix)]
train_agg [,yr := year (Date_posix)]

train_agg [,fold_1 := ifelse (Date_posix <= as.POSIXct ("30/11/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("31/12/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_2 := ifelse (Date_posix <= as.POSIXct ("31/10/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("30/11/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_3 := ifelse (Date_posix <= as.POSIXct ("30/9/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("31/10/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_4 := ifelse (Date_posix <= as.POSIXct ("31/8/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("30/9/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_5 := ifelse (Date_posix <= as.POSIXct ("31/7/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("31/8/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_6 := ifelse (Date_posix <= as.POSIXct ("30/6/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("31/7/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_7 := ifelse (Date_posix <= as.POSIXct ("31/5/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("30/6/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_8 := ifelse (Date_posix <= as.POSIXct ("30/4/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("31/5/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_9 := ifelse (Date_posix <= as.POSIXct ("31/3/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("30/4/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_10 := ifelse (Date_posix <= as.POSIXct ("28/2/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("31/3/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_11 := ifelse (Date_posix <= as.POSIXct ("31/1/2013", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("28/2/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]
train_agg [,fold_12 := ifelse (Date_posix <= as.POSIXct ("31/12/2012", tz = "UTC", format = "%d/%m/%Y", origin="1970-1-1"), 1, ifelse (Date_posix == as.POSIXct ("31/1/2013", tz = "UTC",  format = "%d/%m/%Y", origin="1970-1-1"), 0, -1))]




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

###
model_df <- NULL
##


# for fold-1
myNms <- unique (c(stockNms, sumNms, "actual"))

############################################################
###############RANDOM FOREST ###############################
###############################################################
rfList <- list ()
for (k in c(1:12)) {
	print (k)
	foldName <- paste ("fold", k, sep="_")

	temp <- copy (train_agg_rf)
	setnames (temp, foldName, "fold")
	temp$actual <- NULL
	actual_array <- NULL
	pred_array <- NULL
	opList <- list (); 
	for (j in c(1:6)) {
		actualName <- paste ("actual", j, sep="_")
		setnames (temp, actualName, "actual")
		tempTrain <- temp[temp$fold == 1, ]
		myRF <- randomForest (as.data.frame (tempTrain[,myNms,with=F]), tempTrain$actual, ntree = 250, do.trace=F, nodesize = 1)
		opList [[j]] <- myRF
		pred <- predict (myRF, as.data.frame (temp[temp$fold ==0,myNms,with=F]), type = "response")
		actual_array <- c(actual_array, temp$actual[temp$fold ==0])
		pred_array <- c(pred_array, pred)
		setnames (temp, "actual", actualName)
	}
	rfList [[k]] <- opList
	setnames (temp, "fold", foldName)
	myMAE <- mae (actual_array, pred_array)
	myMSE <- mse (actual_array, pred_array)
	myRMSE <- rmse (actual_array, pred_array)
	myRMSLE <- rmsle (actual_array, pred_array)
	myMAE_avg <- myMAE /mean (actual_array)
	model_df <- rbind (model_df, data.frame (method = "randomforest", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual_array, collapse=" "), pred = paste (pred_array, collapse = " ")))
}


############################################################
###############RANDOM FOREST WITHOUT ACTUAL ###############################
###############################################################
myNms <- myNms [!(myNms %in% "actual")]
rfListNoAct <- list ()
for (k in c(1:12)) {
	print (k)
	foldName <- paste ("fold", k, sep="_")

	temp <- copy (train_agg_rf)
	setnames (temp, foldName, "fold")
	temp$actual <- NULL
	actual_array <- NULL
	pred_array <- NULL
	opList <- list (); 
	for (j in c(1:6)) {
		actualName <- paste ("actual", j, sep="_")
		setnames (temp, actualName, "actual")
		tempTrain <- temp[temp$fold == 1, ]
		myRF <- randomForest (as.data.frame (tempTrain[,myNms,with=F]), tempTrain$actual, ntree = 250, do.trace=F, nodesize = 1)
		opList [[j]] <- myRF
		pred <- predict (myRF, as.data.frame (temp[temp$fold ==0,myNms,with=F]), type = "response")
		actual_array <- c(actual_array, temp$actual[temp$fold ==0])
		pred_array <- c(pred_array, pred)
		setnames (temp, "actual", actualName)
	}
	rfListNoAct [[k]] <- opList
	setnames (temp, "fold", foldName)
	myMAE <- mae (actual_array, pred_array)
	myMSE <- mse (actual_array, pred_array)
	myRMSE <- rmse (actual_array, pred_array)
	myRMSLE <- rmsle (actual_array, pred_array)
	myMAE_avg <- myMAE /mean (actual_array)
	model_df <- rbind (model_df, data.frame (method = "randomforestnoact", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual_array, collapse=" "), pred = paste (pred_array, collapse = " ")))
}
myNms <- c(myNms, "actual")

############################################################
###############REGULARIZED RANDOM FOREST ###############################
###############################################################
library (RRF)
library (Metrics)
rrfList <- list ()
for (k in c(1:12)) {
	print (k)
	foldName <- paste ("fold", k, sep="_")

	temp <- copy (train_agg_rf)
	setnames (temp, foldName, "fold")
	temp$actual <- NULL
	actual_array <- NULL
	pred_array <- NULL
	opList <- list (); 
	for (j in c(1:6)) {
		actualName <- paste ("actual", j, sep="_")
		setnames (temp, actualName, "actual")
		tempTrain <- temp[temp$fold == 1, ]
		myRF <- RRF (as.data.frame (tempTrain[,myNms,with=F]), tempTrain$actual, ntree = 250, do.trace=F, nodesize = 1)
		opList [[j]] <- myRF
		pred <- predict (myRF, as.data.frame (temp[temp$fold ==0,myNms,with=F]), type = "response")
		actual_array <- c(actual_array, temp$actual[temp$fold ==0])
		pred_array <- c(pred_array, pred)
		setnames (temp, "actual", actualName)
	}
	rrfList [[k]] <- opList
	setnames (temp, "fold", foldName)
	myMAE <- mae (actual_array, pred_array)
	myMSE <- mse (actual_array, pred_array)
	myRMSE <- rmse (actual_array, pred_array)
	myRMSLE <- rmsle (actual_array, pred_array)
	myMAE_avg <- myMAE /mean (actual_array)
	model_df <- rbind (model_df, data.frame (method = "RRF", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual_array, collapse=" "), pred = paste (pred_array, collapse = " ")))
}
# save.image ('modeledRF.rData')

############################################################
###############BAYESIAN REGULARIZED NEURAL NETWORK###############################
###############################################################
library (brnn)

brnnList <- list ()
for (k in c(1:12)) {
	print (k)
	foldName <- paste ("fold", k, sep="_")

	temp <- copy (train_agg_rf)
	setnames (temp, foldName, "fold")
	temp$actual <- NULL
	actual_array <- NULL
	pred_array <- NULL
	opList <- list ();
	 for (j in c(1:6)) {
		actualName <- paste ("actual", j, sep="_")
		setnames (temp, actualName, "actual")
		tempTrain <- temp[temp$fold == 1, ]
		myBRNN <- brnn (x=as.matrix (as.data.frame (tempTrain[,myNms,with=F])), y=tempTrain$actual, neurons = 2, normalize = TRUE, cores = 2)
		opList [[j]] <- myBRNN
		pred <- predict (myBRNN, as.matrix( as.data.frame (temp[temp$fold ==0,myNms,with=F])), type = "response")
		actual_array <- c(actual_array, temp$actual[temp$fold ==0])
		pred_array <- c(pred_array, pred)
		setnames (temp, "actual", actualName)
	}
	brnnList [[k]] <- opList
	setnames (temp, "fold", foldName)
	myMAE <- mae (actual_array, pred_array)
	myMSE <- mse (actual_array, pred_array)
	myRMSE <- rmse (actual_array, pred_array)
	myRMSLE <- rmsle (actual_array, pred_array)
	myMAE_avg <- myMAE /mean (actual_array)
	model_df <- rbind (model_df, data.frame (method = "BRNN", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual_array, collapse=" "), pred = paste (pred_array, collapse = " ")))
}

############################################################
###############BAYESIAN REGULARIZED NEURAL NETWORK-neurons = 3#########
###############################################################
library (brnn)

brnn3List <- list ()
for (k in c(1:12)) {
	print (k)
	foldName <- paste ("fold", k, sep="_")

	temp <- copy (train_agg_rf)
	setnames (temp, foldName, "fold")
	temp$actual <- NULL
	actual_array <- NULL
	pred_array <- NULL
	opList <- list (); for (j in c(1:6)) {
		actualName <- paste ("actual", j, sep="_")
		setnames (temp, actualName, "actual")
		tempTrain <- temp[temp$fold == 1, ]
		myBRNN <- brnn (x=as.matrix (as.data.frame (tempTrain[,myNms,with=F])), y=tempTrain$actual, neurons = 3, normalize = TRUE, cores = 2)
		opList [[j]] <- myBRNN
		pred <- predict (myBRNN, as.matrix( as.data.frame (temp[temp$fold ==0,myNms,with=F])), type = "response")
		actual_array <- c(actual_array, temp$actual[temp$fold ==0])
		pred_array <- c(pred_array, pred)
		setnames (temp, "actual", actualName)
	}
	brnn3List [[k]] <- opList
	setnames (temp, "fold", foldName)
	myMAE <- mae (actual_array, pred_array)
	myMSE <- mse (actual_array, pred_array)
	myRMSE <- rmse (actual_array, pred_array)
	myRMSLE <- rmsle (actual_array, pred_array)
	myMAE_avg <- myMAE /mean (actual_array)
	model_df <- rbind (model_df, data.frame (method = "BRNN3", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual_array, collapse=" "), pred = paste (pred_array, collapse = " ")))
}

# save.image ('modeledBRNN.rData')



############################################################
###############BAYESIAN REGULARIZED NEURAL NETWORK-neurons =4#########
###############################################################
library (brnn)

brnn4List <- list ()
for (k in c(1:12)) {
	print (k)
	foldName <- paste ("fold", k, sep="_")

	temp <- copy (train_agg_rf)
	setnames (temp, foldName, "fold")
	temp$actual <- NULL
	actual_array <- NULL
	pred_array <- NULL
	opList <- list (); for (j in c(1:6)) {
		actualName <- paste ("actual", j, sep="_")
		setnames (temp, actualName, "actual")
		tempTrain <- temp[temp$fold == 1, ]
		myBRNN <- brnn (x=as.matrix (as.data.frame (tempTrain[,myNms,with=F])), y=tempTrain$actual, neurons = 4, normalize = TRUE, cores = 2)
		opList [[j]] <- myBRNN
		pred <- predict (myBRNN, as.matrix( as.data.frame (temp[temp$fold ==0,myNms,with=F])), type = "response")
		actual_array <- c(actual_array, temp$actual[temp$fold ==0])
		pred_array <- c(pred_array, pred)
		setnames (temp, "actual", actualName)
	}
	brnn4List [[k]] <- opList
	setnames (temp, "fold", foldName)
	myMAE <- mae (actual_array, pred_array)
	myMSE <- mse (actual_array, pred_array)
	myRMSE <- rmse (actual_array, pred_array)
	myRMSLE <- rmsle (actual_array, pred_array)
	myMAE_avg <- myMAE /mean (actual_array)
	model_df <- rbind (model_df, data.frame (method = "BRNN4", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual_array, collapse=" "), pred = paste (pred_array, collapse = " ")))
}


############################################################
###############TRY GLMNET#########
###############################################################
library (glmnet)

glmnetList <- list ()
for (k in c(1:12)) {
	print (k)
	foldName <- paste ("fold", k, sep="_")

	temp <- copy (train_agg_rf)
	setnames (temp, foldName, "fold")
	temp$actual <- NULL
	actual_array <- NULL
	pred_array <- NULL
	opList <- list ()
	 for (j in c(1:6)) {
		actualName <- paste ("actual", j, sep="_")
		setnames (temp, actualName, "actual")
		tempTrain <- temp[temp$fold == 1, ]
		myGLMNET <- glmnet (x=as.matrix (as.data.frame (tempTrain[,myNms,with=F])), y=tempTrain$actual, family="gaussian")
		opList [[j]] <- myGLMNET
		pred <- predict (myGLMNET, as.matrix( as.data.frame (temp[temp$fold !=1,myNms,with=F])), type = "response", s=min(myGLMNET$lambda))[1,1]
		actual_array <- c(actual_array, temp$actual[temp$fold ==0])
		pred_array <- c(pred_array, pred)
		setnames (temp, "actual", actualName)
	}
	glmnetList [[k]] <- opList
	setnames (temp, "fold", foldName)
	myMAE <- mae (actual_array, pred_array)
	myMSE <- mse (actual_array, pred_array)
	myRMSE <- rmse (actual_array, pred_array)
	myRMSLE <- rmsle (actual_array, pred_array)
	myMAE_avg <- myMAE /mean (actual_array)
	model_df <- rbind (model_df, data.frame (method = "LinearRegressorRegularized", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual_array, collapse=" "), pred = paste (pred_array, collapse = " ")))
}

# save.image ('modeledglmnet.rData')


save (rfList, rfListNoAct, rrfList, brnnList, brnn3List, brnn4List, glmnetList, file = 'trained_models_yequalsfx.rData')


#########################################################################################
#################TIME SERIES METHODS######################################################
###########################################################################################
myTS <- ts (train_agg$actual, start = c(1994, 1), end = c(2014, 6), frequency =12)
myTS <- window (myTS, start = c(1994, 1), end = c(2014, 5))

myTS_train <- list ()
myTS_test <- list ()
# fold 1 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-11
myTS_train [[1]] <- window (myTS, start = c(1994,1), end = c(2013,11))
myTS_test [[1]] <- window (myTS, start = c(2013,12), end = c(2014,5))
# fold 2 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-10
myTS_train [[2]] <- window (myTS, start = c(1994,1), end = c(2013,10))
myTS_test [[2]] <- window (myTS, start = c(2013,11), end = c(2014,4))
# fold 3 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-9
myTS_train [[3]] <- window (myTS, start = c(1994,1), end = c(2013,9))
myTS_test [[3]] <- window (myTS, start = c(2013,10), end = c(2014,3))
# fold 4 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-8
myTS_train [[4]] <- window (myTS, start = c(1994,1), end = c(2013,8))
myTS_test [[4]] <- window (myTS, start = c(2013,9), end = c(2014,2))
# fold 5 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-7
myTS_train [[5]] <- window (myTS, start = c(1994,1), end = c(2013,7))
myTS_test [[5]] <- window (myTS, start = c(2013,8), end = c(2014,1))
# fold 6 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-6
myTS_train [[6]] <- window (myTS, start = c(1994,1), end = c(2013,6))
myTS_test [[6]] <- window (myTS, start = c(2013,7), end = c(2013,12))
# fold 7 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-5
myTS_train [[7]] <- window (myTS, start = c(1994,1), end = c(2013,5))
myTS_test [[7]] <- window (myTS, start = c(2013,6), end = c(2013,11))
# fold 8 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-4
myTS_train [[8]] <- window (myTS, start = c(1994,1), end = c(2013,4))
myTS_test [[8]] <- window (myTS, start = c(2013,5), end = c(2013,10))
# fold 9 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-3
myTS_train [[9]] <- window (myTS, start = c(1994,1), end = c(2013,3))
myTS_test [[9]] <- window (myTS, start = c(2013,4), end = c(2013,9))
# fold 10 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-2
myTS_train [[10]] <- window (myTS, start = c(1994,1), end = c(2013,2))
myTS_test [[10]] <- window (myTS, start = c(2013,3), end = c(2013,8))
# fold 11 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2013-1
myTS_train [[11]] <- window (myTS, start = c(1994,1), end = c(2013,1))
myTS_test [[11]] <- window (myTS, start = c(2013,2), end = c(2013,7))
# fold 12 - Validation: 2013-12 to 2014-5; Training: 1994-1 to 2012-12
myTS_train [[12]] <- window (myTS, start = c(1994,1), end = c(2012,12))
myTS_test [[12]] <- window (myTS, start = c(2013,1), end = c(2013,6))



########################################################################
########HOLT WINTERS METHOD ##################################
#########################################################################
myTS_hw <- list ()
myTS_hw_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_hw [[k]] <- HoltWinters(myTS_train[[k]], beta=FALSE, gamma=FALSE)
	myTS_hw_pred [[k]] <- forecast (myTS_hw[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_hw_pred [[k]])
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "HoltWinters", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('holtwinters.rData')

########################################################################
########ARIMA METHOD ##################################
#########################################################################


myTS_arima <- list ()
myTS_arima_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_arima [[k]] <- auto.arima (myTS_train[[k]])
	myTS_arima_pred [[k]] <- forecast (myTS_arima[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_arima_pred [[k]])
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "ARIMA", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('arima.rData')


########################################################################
########NAIVE METHOD ##################################
#########################################################################

myTS_naive <- list ()
myTS_naive_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_naive_pred [[k]] <- snaive (myTS_train[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_naive_pred [[k]])
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "naive", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('naive.rData')


########################################################################
########randomwalk METHOD ##################################
#########################################################################

myTS_randomwalk <- list ()
myTS_randomwalk_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_randomwalk_pred [[k]] <- rwf (myTS_train[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_randomwalk_pred [[k]])
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "randomwalk", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('randomwalk.rData')


########################################################################
########SIMPLE EXPONENTIAL SMOOTHING METHOD ##################################
#########################################################################
myTS_ses <- list ()
myTS_ses_pred <- list ()

# find optimal smoothing parameter
bestMAE <- 100000
for (myAlpha in seq (from = 0.05,to= 0.95, by = 0.05)) {
	print (myAlpha)
	prediction <- NULL
	actual <- NULL
	for (k in c(1:12)) {
		temp <- ets (myTS_train[[k]], alpha = myAlpha)
		prediction <- c(prediction, forecast (temp, h=6)$mean)
		actual <- c(actual, as.numeric (myTS_test [[k]]))
	}
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	if (myMAE < bestMAE) {
		bestMAE <- myMAE
		bestAlpha <- myAlpha
	}
	print (paste (myAlpha, myMAE))
}

for (k in c(1:12)) {
	print (k)
	myTS_ses [[k]] <- ets (myTS_train[[k]], alpha = bestAlpha)
	myTS_ses_pred [[k]] <- forecast (myTS_ses[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_ses_pred [[k]])
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "ets", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('ses.rData')



########################################################################
########EXPONENTIAL DAMPED TREND ##################################
#########################################################################


myTS_hwexpdamped <- list ()
myTS_hwexpdamped_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_hwexpdamped [[k]] <- holt (myTS_train[[k]], exponential = TRUE, damped = TRUE)
	myTS_hwexpdamped_pred [[k]] <- forecast (myTS_hwexpdamped[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_hwexpdamped_pred [[k]])[1:length(actual)]
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "hwexpdamped", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('hwexpdamped.rData')


########################################################################
######## DAMPED TREND ##################################
#########################################################################


myTS_hwdamped <- list ()
myTS_hwdamped_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_hwdamped [[k]] <- holt (myTS_train[[k]], damped = TRUE)
	myTS_hwdamped_pred [[k]] <- forecast (myTS_hwdamped[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_hwdamped_pred [[k]])[1:length(actual)]
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "hwdamped", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('hwdamped.rData')


########################################################################
######## HOLT WINTER SEASONAL TREND ##################################
#########################################################################


myTS_holtwinterseasonal <- list ()
myTS_holtwinterseasonal_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_holtwinterseasonal [[k]] <- ets (myTS_train[[k]], lambda = NULL, opt.crit = "mae")
	myTS_holtwinterseasonal_pred [[k]] <- forecast (myTS_holtwinterseasonal[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_holtwinterseasonal_pred [[k]])[1:length(actual)]
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "holtwinterseasonal", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('holtwinterseasonal.rData')

########################################################################
######## HOLT WINTER SEASONAL BOX COX ##################################
#########################################################################


myTS_holtwinterseasonalboxcox <- list ()
myTS_holtwinterseasonalboxcox_pred <- list ()


for (k in c(1:12)) {
	print (k)
	myTS_holtwinterseasonalboxcox [[k]] <- ets (myTS_train[[k]], lambda = 0, opt.crit = "mae")
	myTS_holtwinterseasonalboxcox_pred [[k]] <- forecast (myTS_holtwinterseasonalboxcox[[k]], h=6)$mean
	actual <- as.numeric (myTS_test [[k]])
	pred <- as.numeric (myTS_holtwinterseasonalboxcox_pred [[k]])[1:length(actual)]
	myMAE <- mae (actual, pred)
	myMSE <- mse (actual, pred)
	myRMSE <- rmse (actual, pred)
	myRMSLE <- rmsle (actual, pred)
	myMAE_avg <- myMAE /mean (actual)
	model_df <- rbind (model_df, data.frame (method = "holtwinterseasonalboxcox", myMAE = myMAE, myMSE = myMSE, myRMSE = myRMSE, myRMSLE = myRMSLE, myMAE_pct = myMAE_avg, fold = k, actual = paste (actual, collapse=" "), pred = paste (pred, collapse = " ")))
}

save.image ('holtwinterseasonalboxcox.rData')











############################################################################
############TIME SERIES END ##############################################
#############################################################################

print ("Saved all models")

library (data.table)
library (stringr)

model_df_t <- copy (setDT( model_df))
model_df_t [,method := as.factor (method)]
model_df_req <- model_df_t [,list (method, actual, fold, pred)]
act_df <- t (sapply (model_df_req$actual, function (x) as.numeric (unlist (str_split (x, pattern = " ")))))
pred_df <- t (sapply (model_df_req$pred, function (x) as.numeric (unlist (str_split (x, pattern = " ")))))
colnames (act_df) <- paste ("actual", 1:6, sep="_")
colnames (pred_df) <- paste ("pred", 1:6, sep="_")
model_df_req <- cbind (as.data.frame (model_df_req), act_df, pred_df)
model_df_req$actual <- NULL
model_df_req$pred <- NULL

setDT (model_df_req)
library (reshape2)
temp <- melt (as.data.frame (model_df_req), id.vars = c("method", "fold"), measure.vars = c(paste ("actual", 1:6, sep="_"), paste ("pred", 1:6, sep="_")) )
setDT (temp)
model_df_wide <- copy (temp)
rm (temp)

model_df_wide [,mth := sapply (variable, function (x) unlist (str_split (x, "_"))[[2]])]
model_df_wide [,variable := sapply (variable, function (x) unlist (str_split (x, "_"))[[1]])]

actual_df <- model_df_wide [model_df_wide$variable == "actual",]
model_df_wide <- model_df_wide [model_df_wide$variable != "actual",]

actual_df <- actual_df [actual_df$method == "HoltWinters",]
actual_df [,method := NULL]
actual_df [,variable := NULL]

model_df_wide <- merge (model_df_wide, actual_df, by = c("fold", "mth"), all.x=TRUE)
setnames (model_df_wide, "value.x", "pred")
setnames (model_df_wide, "value.y", "actual")

to_model_df <- copy (model_df_wide)
for_model_df <- dcast.data.table (to_model_df, fold + mth ~ method, value.var = "pred", fun.aggregate = mean)
for_model_df <- merge (for_model_df, actual_df, by = c("fold", "mth"), all.x=TRUE)

setnames (for_model_df, "value", "actual")



##############################################################################
# Summarize results of various methods#
##############################################################################

model_df_summary <- cbind (as.data.frame (model_df), act_df, pred_df)
model_df_summary <- as.data.frame (model_df_summary)
mae_df <- cbind (act_df, pred_df)
myMAE1 <- apply (mae_df, 1, function (x) mae (x[1], x[7]))
myMAE2 <- apply (mae_df, 1, function (x) mae (x[2], x[8]))
myMAE3 <- apply (mae_df, 1, function (x) mae (x[3], x[9]))
myMAE4 <- apply (mae_df, 1, function (x) mae (x[4], x[10]))
myMAE5 <- apply (mae_df, 1, function (x) mae (x[5], x[11]))
myMAE6 <- apply (mae_df, 1, function (x) mae (x[6], x[12]))

model_df_summary$myMAE1 <- myMAE1
model_df_summary$myMAE2 <- myMAE2
model_df_summary$myMAE3 <- myMAE3
model_df_summary$myMAE4 <- myMAE4
model_df_summary$myMAE5 <- myMAE5
model_df_summary$myMAE6 <- myMAE6

model_df_summary$pctMAE1 <- model_df_summary$myMAE1/model_df_summary$actual_1
model_df_summary$pctMAE2 <- model_df_summary$myMAE2/model_df_summary$actual_2
model_df_summary$pctMAE3 <- model_df_summary$myMAE3/model_df_summary$actual_3
model_df_summary$pctMAE4 <- model_df_summary$myMAE4/model_df_summary$actual_4
model_df_summary$pctMAE5 <- model_df_summary$myMAE5/model_df_summary$actual_5
model_df_summary$pctMAE6 <- model_df_summary$myMAE6/model_df_summary$actual_6


 
write.csv (model_df_summary, file = 'model_df_summary.csv', row.names = F)


for_model_df [,mae_HoltWinters := abs (actual - HoltWinters)/actual]
for_model_df [,mae_ARIMA := abs (actual - ARIMA)/actual]
for_model_df [,mae_naive := abs (actual - naive)/actual]
for_model_df [,mae_randomwalk := abs (actual - randomwalk)/actual]
for_model_df [,mae_ets := abs (actual - ets)/actual]
for_model_df [,mae_hwexpdamped := abs (actual - hwexpdamped)/actual]
for_model_df [,mae_hwdamped := abs (actual - hwdamped)/actual]
for_model_df [,mae_holtwinterseasonal := abs (actual - holtwinterseasonal)/actual]
for_model_df [,mae_holtwinterseasonalboxcox := abs (actual - holtwinterseasonalboxcox)/actual]
for_model_df [,ensemble := (1/mae_HoltWinters) * HoltWinters +
			(1/mae_ARIMA) * ARIMA +
			(1/mae_naive) * naive +
			(1/mae_randomwalk) * randomwalk +
			(1/mae_ets) * ets +
			(1/mae_hwexpdamped) * hwexpdamped +
			(1/mae_hwdamped) * hwdamped +
			(1/mae_holtwinterseasonal) * holtwinterseasonal +
			(1/mae_holtwinterseasonalboxcox) *  holtwinterseasonalboxcox]
for_model_df [,ensemble := ensemble /(1/mae_HoltWinters + 1/mae_ARIMA + 1/mae_naive +
1/mae_randomwalk +1/mae_ets +1/mae_hwexpdamped +1/mae_hwdamped + 1/mae_holtwinterseasonal
+  1/mae_holtwinterseasonalboxcox )]




