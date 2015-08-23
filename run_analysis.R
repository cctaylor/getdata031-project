library(dplyr)

# read_file() loads either the test or train data, depending on which is passed in the "data_set_id" argument
# fully assembles the data set, including adding variable (column) labels and subject & activity identifiers
read_file <- function(data_set_id) {
  old_dir <- getwd()    # store the current working directory
  setwd("./Data")

  data_set <- read.table(eval(paste("./",data_set_id,"/X_",data_set_id,".txt", sep="")))  # read the data file
  data_set_col_labels <- read.table(eval(paste("./features.txt", sep="")))  # read the variable names
  names(data_set) <- data_set_col_labels[,2]  # attach names to data file

  data_set_subject_ID <- read.table(eval(paste("./",data_set_id,"/subject_",data_set_id,".txt", sep=""))) # read the subject identifiers
  names(data_set_subject_ID) <- c("subjectID")  # name the column
  data_set <- cbind(data_set_subject_ID, data_set)  # attach subject IDs to data set
  
  data_set_activity_ID <- read.table(eval(paste("./",data_set_id,"/y_",data_set_id,".txt", sep="")))  # read the activity identifiers
  names(data_set_activity_ID) <- c("activityID")  # name the column
  data_set <- cbind(data_set_activity_ID, data_set) # attach activity IDs to data set
  
  setwd(old_dir)  # return to original working directory
  data_set    # return the full data set
}


# step_1() returns the output specified in step 1 of the project instuctions: a single data frame containing test and train data
step_1 <- function() {
  test <- read_file("test")
  train <- read_file("train")
  total <- rbind(test, train)
}


# step_2() returns a data frame that only contains the mean and standard deviation measures from the full data file
step_2 <- function() {
  full_file <- step_1()
  to_match <- c("mean\\(\\)", "std\\(\\)")
  mean_stdev <- full_file[, grep(paste(to_match, collapse="|"), colnames(full_file))]
}


# step_3() returns a data fram with the activity descriptions added
step_3 <- function() {
  full_file <- step_1()
  actLabel <- read.table("./Data/activity_labels.txt")  # load the activity descriptions
  names(actLabel) <- c("activityID", "activityDesc")  # name the columns
  activityDescription <- merge(full_file$activityID, actLabel, by.x = 1, by.y = "activityID", all = FALSE)[,2]  # create a data frame with the activity description for each row in the full data set
  cbind(activityDescription, full_file) # attach the activity names to the full data set
}


# step_4() improves the names of the columns in the data set to they are clearer and more descriptive and all camelCase
step_4 <- function() {
  full_file <- step_3()   # load the full file
  initial_col_names <- names(full_file)   # create a vector of the variable names
  final_col_names <- gsub("\\(\\)", "", initial_col_names)  # remove any parentheses ()
  final_col_names <- gsub("Body", "", final_col_names)      # remove the term "Body", which is unneccesary 
  final_col_names <- gsub("\\-std", "StdDev", final_col_names)  # change the extension "-std", for standard deviation, to "StdDev"
  final_col_names <- gsub("\\-mean", "Mean", final_col_names)   # change the extension "-mean" to "Mean"
  final_col_names <- gsub("\\-mad", "MedAbsDev", final_col_names)   # change the extension "-mad", for median absolute deviation, to "MedAbsDev"
  final_col_names <- gsub("\\-max", "Max", final_col_names)         # change the extension "-max" to "Max"
  final_col_names <- gsub("\\-min", "Min", final_col_names)         # change the extension "-min" to "Min"
  final_col_names <- gsub("\\-sma", "SignalMagnitudeArea", final_col_names)   # change the extension "-sma" to "SignalMagnitudeArea"
  final_col_names <- gsub("\\-energy", "Energy", final_col_names)   # change the extension "-energy" to "Energy"
  final_col_names <- gsub("\\-iqr", "InterquartileRange", final_col_names)    # change the extension "-iqr" to "InterquartileRange"
  final_col_names <- gsub("\\-entropy", "Entropy", final_col_names)           # change the extension "-entropy" to "Entropy"
  final_col_names <- gsub("\\-arCoeff", "AutoregressionCoeff", final_col_names)   # change the extension "-arCoeff" to "AutoregressionCoefficient"
  final_col_names <- gsub("\\-maxInds", "MaxFreqIndex", final_col_names)      # change the extension "maxInds" to "MaxFreqIndex"
  final_col_names <- gsub("\\-meanFreq", "WtdAvgFreq", final_col_names)       # change the extension "-meanFreq" to "WtdAvgFreq"
  final_col_names <- gsub("\\-skewness", "SkewnessFreq", final_col_names)     # change the extension "-skewness" to "SkewnessFreq"
  final_col_names <- gsub("\\-kurtosis", "KurtosisFreq", final_col_names)     # change the extension "-kurtosis" to "KurtosisFreq"
  final_col_names <- gsub("\\-bandsEnergy", "EnergyFFTBands", final_col_names)    # change the extension "-bandsEnergy", referring to the energy bands in the Fast Fourier Transform, to "EnergyFFTBands"
  final_col_names <- gsub("\\-angle", "AngleBetweenVectors", final_col_names)     # change the extension "-angle" to "AngleBetweenVectors"
  final_col_names <- gsub("\\-correlation", "Correlation", final_col_names)   # change the extension "-correlation" to "Correlation"
  final_col_names <- gsub("Acc", "Acceleration", final_col_names)   # change the extension "Acc" to "Acceleration"
  final_col_names <- gsub("Gyro", "Gyroscope", final_col_names)     # change the extension "Gyro" to "Gyroscope"
  final_col_names <- gsub("Mag", "Magnitude", final_col_names)      # change the extension "Mag" to "Magnitude
  final_col_names <- gsub("\\-X", "X", final_col_names)             # remove the hyphen before the axes
  final_col_names <- gsub("\\-Y", "Y", final_col_names)
  final_col_names <- gsub("\\-Z", "Z", final_col_names)
  final_col_names <- gsub("\\,", "\\_", final_col_names)            # replace all commas with underscores
  final_col_names <- gsub("\\(", "", final_col_names)               #remove any remaining parentheses
  final_col_names <- gsub("\\)", "", final_col_names)
  final_col_names <- gsub("\\-", "", final_col_names)               # remove any remaining hyphens
  
  names(full_file) <- final_col_names     # update the variable names in the data set
  full_file     # return the file with updated variable names
}


# step_5() returns the mean of each variable, grouped by subject and activity
step_5 <- function() {
  full_file <- step_4()
  output <- full_file[, !(colnames(full_file) == "activityID")] %>% group_by(subjectID, activityDescription) %>% summarise_each(funs(mean))
  write.table(output, row.names = FALSE, file = "tidy_data.txt")
  output
}
