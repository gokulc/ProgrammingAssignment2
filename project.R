##This script file reads the training data X into dfx_train, training data y into
## dfy_train, test data x into dfx_test and test data y into dfy_test

dfx_test <- read.table("test\\X_test.txt",header=FALSE)
dfx_train <- read.table("train\\X_train.txt",header=FALSE)

dfy_test <- read.table("test\\y_test.txt",header=FALSE)
dfy_train <- read.table("train\\Y_train.txt",header=FALSE)

df_subject_test <- read.table("test\\subject_test.txt",header=FALSE)
df_subject_train <- read.table("train\\subject_train.txt",header=FALSE)

## Merge tables X & Y for test and train

df_test <- cbind(dfx_test,dfy_test,df_subject_test)
df_train <- cbind(dfx_train,dfy_train,df_subject_train)

#Now combine these two data frames into one data set/frame
df <- rbind(df_test, df_train)

#read features.txt to df_features. Features.txt contains the features list and we
## need to pick the ones we are interested in.. i.e means and stds

df_features<- read.table("features.txt",header=FALSE)

## Since there are two columns in df_features, Number of rows and feature names
## let us set the column names to df_features as No. and feature
colnames(df_features) <- c("No.","Feature")

## There are open-close parentheses in feature names which we can clean up by:
df_features$Feature<-sub("mean()","mean",df_features$Feature,fixed=TRUE)
df_features$Feature<-sub("Mean()","mean",df_features$Feature,fixed=TRUE)
df_features$Feature<-sub("std()","std",df_features$Feature,fixed=TRUE)
df_features$Feature<-sub("Std()","std",df_features$Feature,fixed=TRUE)

##Renaming activity numbers with names as required in step 3


## The last column in our main data is 'Y' so lets add it to df_features
df_features <- rbind(df_features,c(561,"Activity"),c(562,"Subject"))

## set the cleaned up df_feature$Feature as column names for our main data frame df
colnames(df) <- (df_features$Feature)

## lets get a list of features with mean
df_list_mean <- grep("mean",df_features$Feature)
df_list_Mean <- grep("Mean",df_features$Feature)

## and with std
df_list_std <- grep("std",df_features$Feature)
df_list_Std <- grep("Std",df_features$Feature)

df_list_activity <- grep("Activity",df_features$Feature)
df_list_subject <- grep("Subject",df_features$Feature)

## combining all of them into one list of needed features
df_list_needed <- rbind(df_features[df_list_mean,], 
                        df_features[df_list_Mean,], 
                        df_features[df_list_std,],
                        df_features[df_list_Std,],
                        df_features[df_list_activity,],
                        df_features[df_list_subject,])

##Extracting the needed/required variables from the main data set df
df_tidy <- df[,df_list_needed$Feature]

##Naming activities
##     1 WALKING
##     2 WALKING_UPSTAIRS
##     3 WALKING_DOWNSTAIRS
##     4 SITTING
##     5 STANDING
##     6 LAYING

df_tidy$Activity<-sub(1,'WALKING',df_tidy$Activity)
df_tidy$Activity <- sub(2,'WALKING_UPSTAIRS',df_tidy$Activity)
df_tidy$Activity <- sub(3,'WALKING_DOWNSTAIRS',df_tidy$Activity)
df_tidy$Activity <- sub(4,'SITTING',df_tidy$Activity)
df_tidy$Activity <- sub(5,'STANDING',df_tidy$Activity)
df_tidy$Activity <- sub(6,'LAYING',df_tidy$Activity)

#Extracting means of all columns over the aggregates of subjects and activity
df_aggregate_means = aggregate(df_tidy[,1:86], by = list(Activity = df_tidy$Activity,Subject = df$Subject ), FUN = "mean", na.rm = T)

##save the data frame generated as 'aggregate.txt' file
write.table(df_aggregate_means,file="aggregate.txt",row.name=FALSE)