run_analysis<-function(){
  
  test_x<-read.table("test/X_test.txt")
  train_x<-read.table("train/X_train.txt")
  test<-test_x[,c(1 , 2 , 3 , 4 , 5 , 6 , 41 , 42 , 43 , 44 , 45 , 46 , 81 , 82 , 83 , 84 , 85 , 86 , 121 , 122 , 123 , 124 , 125 , 126 , 161 , 162 , 163 , 164 , 165 , 166 , 201 , 202 , 214 , 215 , 227 , 228 , 240 , 241 , 253 , 254 , 266 , 267 , 268 , 269 , 270 , 271 , 294 , 295 , 296 , 345 , 346 , 347 , 348 , 349 , 350 , 373 , 374 , 375 , 424 , 425 , 426 , 427 , 428 , 429 , 452 , 453 , 454 , 503 , 504 , 513 , 516 , 517 , 526 , 529 , 530 , 539 , 542 , 543 , 552)]
  train<-train_x[,c(1 , 2 , 3 , 4 , 5 , 6 , 41 , 42 , 43 , 44 , 45 , 46 , 81 , 82 , 83 , 84 , 85 , 86 , 121 , 122 , 123 , 124 , 125 , 126 , 161 , 162 , 163 , 164 , 165 , 166 , 201 , 202 , 214 , 215 , 227 , 228 , 240 , 241 , 253 , 254 , 266 , 267 , 268 , 269 , 270 , 271 , 294 , 295 , 296 , 345 , 346 , 347 , 348 , 349 , 350 , 373 , 374 , 375 , 424 , 425 , 426 , 427 , 428 , 429 , 452 , 453 , 454 , 503 , 504 , 513 , 516 , 517 , 526 , 529 , 530 , 539 , 542 , 543 , 552)]
  train_y<-read.table("train/y_train.txt")
  test_y<-read.table("test/y_test.txt")
  test<-cbind(test_y,test)
  train<-cbind(train_y,train)
  test[,1][test[,1]==1]<-"WALKING"
  test[,1][test[,1]==5]<-"STANDING"
  test[,1][test[,1]==2]<-"WALKING_UPSTAIRS"
  test[,1][test[,1]==3]<-"WALKING_DOWNSTAIRS"
  test[,1][test[,1]==4]<-"SITTING"
  test[,1][test[,1]==6]<-"LAYING"
  train[,1][train[,1]==6]<-"LAYING"
  train[,1][train[,1]==4]<-"SITTING"
  train[,1][train[,1]==3]<-"WALKING_DOWNSTAIRS"
  train[,1][train[,1]==2]<-"WALKING_UPSTAIRS"
  train[,1][train[,1]==5]<-"STANDING"
  train[,1][train[,1]==1]<-"WALKING"
  s_test<-read.table("test/subject_test.txt")
  s_train<-read.table("train/subject_train.txt")
  train<-cbind(s_train,train)
  test<-cbind(s_test,test)
  colnames(train)<-c("Subject", "Activity", "tBodyAcc_mean_X", "tBodyAcc_mean_Y", "tBodyAcc_mean_Z", "tBodyAcc_std_X", "tBodyAcc_std_Y", "tBodyAcc_std_Z", "tGravityAcc_mean_X", "tGravityAcc_mean_Y", "tGravityAcc_mean_Z", "tGravityAcc_std_X", "tGravityAcc_std_Y", "tGravityAcc_std_Z", "tBodyAccJerk_mean_X", "tBodyAccJerk_mean_Y", "tBodyAccJerk_mean_Z", "tBodyAccJerk_std_X", "tBodyAccJerk_std_Y", "tBodyAccJerk_std_Z", "tBodyGyro_mean_X", "tBodyGyro_mean_Y", "tBodyGyro_mean_Z", "tBodyGyro_std_X", "tBodyGyro_std_Y", "tBodyGyro_std_Z", "tBodyGyroJerk_mean_X", "tBodyGyroJerk_mean_Y", "tBodyGyroJerk_mean_Z", "tBodyGyroJerk_std_X", "tBodyGyroJerk_std_Y", "tBodyGyroJerk_std_Z", "tBodyAccMag_mean", "tBodyAccMag_std", "tGravityAccMag_mean", "tGravityAccMag_std", "tBodyAccJerkMag_mean", "tBodyAccJerkMag_std", "tBodyGyroMag_mean", "tBodyGyroMag_std", "tBodyGyroJerkMag_mean", "tBodyGyroJerkMag_std", "fBodyAcc_mean_X", "fBodyAcc_mean_Y", "fBodyAcc_mean_Z", "fBodyAcc_std_X", "fBodyAcc_std_Y", "fBodyAcc_std_Z", "fBodyAcc_meanFreq_X", "fBodyAcc_meanFreq_Y", "fBodyAcc_meanFreq_Z", "fBodyAccJerk_mean_X", "fBodyAccJerk_mean_Y", "fBodyAccJerk_mean_Z", "fBodyAccJerk_std_X", "fBodyAccJerk_std_Y", "fBodyAccJerk_std_Z", "fBodyAccJerk_meanFreq_X", "fBodyAccJerk_meanFreq_Y", "fBodyAccJerk_meanFreq_Z", "fBodyGyro_mean_X", "fBodyGyro_mean_Y", "fBodyGyro_mean_Z", "fBodyGyro_std_X", "fBodyGyro_std_Y", "fBodyGyro_std_Z", "fBodyGyro_meanFreq_X", "fBodyGyro_meanFreq_Y", "fBodyGyro_meanFreq_Z", "fBodyAccMag_mean", "fBodyAccMag_std", "fBodyAccMag_meanFreq", "fBodyBodyAccJerkMag_mean", "fBodyBodyAccJerkMag_std", "fBodyBodyAccJerkMag_meanFreq", "fBodyBodyGyroMag_mean", "fBodyBodyGyroMag_std", "fBodyBodyGyroMag_meanFreq", "fBodyBodyGyroJerkMag_mean", "fBodyBodyGyroJerkMag_std", "fBodyBodyGyroJerkMag_meanFreq")
  colnames(test)<-c("Subject", "Activity", "tBodyAcc_mean_X", "tBodyAcc_mean_Y", "tBodyAcc_mean_Z", "tBodyAcc_std_X", "tBodyAcc_std_Y", "tBodyAcc_std_Z", "tGravityAcc_mean_X", "tGravityAcc_mean_Y", "tGravityAcc_mean_Z", "tGravityAcc_std_X", "tGravityAcc_std_Y", "tGravityAcc_std_Z", "tBodyAccJerk_mean_X", "tBodyAccJerk_mean_Y", "tBodyAccJerk_mean_Z", "tBodyAccJerk_std_X", "tBodyAccJerk_std_Y", "tBodyAccJerk_std_Z", "tBodyGyro_mean_X", "tBodyGyro_mean_Y", "tBodyGyro_mean_Z", "tBodyGyro_std_X", "tBodyGyro_std_Y", "tBodyGyro_std_Z", "tBodyGyroJerk_mean_X", "tBodyGyroJerk_mean_Y", "tBodyGyroJerk_mean_Z", "tBodyGyroJerk_std_X", "tBodyGyroJerk_std_Y", "tBodyGyroJerk_std_Z", "tBodyAccMag_mean", "tBodyAccMag_std", "tGravityAccMag_mean", "tGravityAccMag_std", "tBodyAccJerkMag_mean", "tBodyAccJerkMag_std", "tBodyGyroMag_mean", "tBodyGyroMag_std", "tBodyGyroJerkMag_mean", "tBodyGyroJerkMag_std", "fBodyAcc_mean_X", "fBodyAcc_mean_Y", "fBodyAcc_mean_Z", "fBodyAcc_std_X", "fBodyAcc_std_Y", "fBodyAcc_std_Z", "fBodyAcc_meanFreq_X", "fBodyAcc_meanFreq_Y", "fBodyAcc_meanFreq_Z", "fBodyAccJerk_mean_X", "fBodyAccJerk_mean_Y", "fBodyAccJerk_mean_Z", "fBodyAccJerk_std_X", "fBodyAccJerk_std_Y", "fBodyAccJerk_std_Z", "fBodyAccJerk_meanFreq_X", "fBodyAccJerk_meanFreq_Y", "fBodyAccJerk_meanFreq_Z", "fBodyGyro_mean_X", "fBodyGyro_mean_Y", "fBodyGyro_mean_Z", "fBodyGyro_std_X", "fBodyGyro_std_Y", "fBodyGyro_std_Z", "fBodyGyro_meanFreq_X", "fBodyGyro_meanFreq_Y", "fBodyGyro_meanFreq_Z", "fBodyAccMag_mean", "fBodyAccMag_std", "fBodyAccMag_meanFreq", "fBodyBodyAccJerkMag_mean", "fBodyBodyAccJerkMag_std", "fBodyBodyAccJerkMag_meanFreq", "fBodyBodyGyroMag_mean", "fBodyBodyGyroMag_std", "fBodyBodyGyroMag_meanFreq", "fBodyBodyGyroJerkMag_mean", "fBodyBodyGyroJerkMag_std", "fBodyBodyGyroJerkMag_meanFreq")
  combined<-merge(train,test, all=TRUE)
  #write.table(combined,file="combined_oryg.txt")
  tidy_list<-split(combined[3:81], list(combined$Activity, combined$Subject))
 
mean_lis<-lapply(tidy_list, colMeans)  
df_means<-data.frame(mean_lis)
trans<-t(df_means)
names<- rownames(trans) 
Subject_ID<-lapply(strsplit(as.character(names), "\\."), "[", 2)
 Activity_Label<-lapply(strsplit(as.character(names), "\\."), "[", 1)
trans<-cbind(Activity_Label,Subject_ID,trans)
write.table(trans,file="pt5_data.csv", sep=";", row.name=FALSE)
}