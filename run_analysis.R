library(dplyr)


read_file <- function(data_set_id) {
  old_dir <- getwd()
  setwd("./Data")

  data_set <- read.table(eval(paste("./",data_set_id,"/X_",data_set_id,".txt", sep="")))
  data_set_col_labels <- read.table(eval(paste("./features.txt", sep="")))
  names(data_set) <- data_set_col_labels[,2]

  data_set_subject_ID <- read.table(eval(paste("./",data_set_id,"/subject_",data_set_id,".txt", sep="")))
  names(data_set_subject_ID) <- c("subjectID")
  data_set <- cbind(data_set_subject_ID, data_set)
  
  data_set_activity_ID <- read.table(eval(paste("./",data_set_id,"/y_",data_set_id,".txt", sep="")))
  names(data_set_activity_ID) <- c("activityID")
  data_set <- cbind(data_set_activity_ID, data_set)
  
  setwd(old_dir)
  data_set
}

step_1 <- function() {
  test <- read_file("test")
  train <- read_file("train")
  total <- rbind(test, train)
}

step_2 <- function() {
  full_file <- step_1()
  to_match <- c("mean\\(\\)", "std\\(\\)")
  mean_stdev <- full_file[, grep(paste(to_match, collapse="|"), colnames(full_file))]
}

step_3 <- function() {
  full_file <- step_1()
  actLabel <- read.table("./Data/activity_labels.txt")
  names(actLabel) <- c("activityID", "activityDesc")
  activityDescription <- merge(full_file$activityID, actLabel, by.x = 1, by.y = "activityID", all = FALSE)[,2]
  cbind(activityDescription, full_file)
}

step_4 <- function() {
  full_file <- step_3()
  initial_col_names <- names(full_file)
  final_col_names <- gsub("\\(\\)", "", initial_col_names)
  final_col_names <- gsub("Body", "", final_col_names)
  final_col_names <- gsub("\\-std", "StdDev", final_col_names)
  final_col_names <- gsub("\\-mean", "Mean", final_col_names)
  final_col_names <- gsub("\\-mad", "MedAbsDev", final_col_names)
  final_col_names <- gsub("\\-max", "Max", final_col_names)
  final_col_names <- gsub("\\-min", "Min", final_col_names)
  final_col_names <- gsub("\\-sma", "SignalMagnitudeArea", final_col_names)
  final_col_names <- gsub("\\-energy", "Energy", final_col_names)
  final_col_names <- gsub("\\-iqr", "InterquartileRange", final_col_names)
  final_col_names <- gsub("\\-entropy", "Entropy", final_col_names)
  final_col_names <- gsub("\\-arCoeff", "AutoregressionCoeff", final_col_names)
  final_col_names <- gsub("\\-maxInds", "MaxFreqIndex", final_col_names)
  final_col_names <- gsub("\\-meanFreq", "WtdAvgFreq", final_col_names)
  final_col_names <- gsub("\\-skewness", "SkewnessFreq", final_col_names)
  final_col_names <- gsub("\\-kurtosis", "KurtosisFreq", final_col_names)
  final_col_names <- gsub("\\-bandsEnergy", "EnergyFFTBands", final_col_names)
  final_col_names <- gsub("\\-angle", "AngleBetweenVectors", final_col_names)
  final_col_names <- gsub("\\-correlation", "Correlation", final_col_names)
  final_col_names <- gsub("\\Acc", "Acceleration", final_col_names)
  final_col_names <- gsub("\\Gyro", "Gyroscope", final_col_names)
  final_col_names <- gsub("\\Mag", "Magnitude", final_col_names)
  final_col_names <- gsub("\\-X", "X", final_col_names)
  final_col_names <- gsub("\\-Y", "Y", final_col_names)
  final_col_names <- gsub("\\-Z", "Z", final_col_names)
  final_col_names <- gsub("\\,", "\\_", final_col_names)
  final_col_names <- gsub("\\(", "", final_col_names)
  final_col_names <- gsub("\\)", "", final_col_names)
  final_col_names <- gsub("\\-", "", final_col_names)
  
  names(full_file) <- final_col_names
  full_file
}

step_5 <- function() {
  full_file <- step_4()
  output <- full_file[, !(colnames(full_file) == "activityID")] %>% group_by(subjectID, activityDescription) %>% summarise_each(funs(mean))
  write.table(output, row.names = FALSE)
  write.table(output, row.names = FALSE, file = "tidy_data.txt")
}
