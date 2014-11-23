get_clean_data_xyz2009ee
========================
#In the script, the following lines read in the training and testing dataset from the folder.
x_train = read.table("./train/X_train.txt")

x_test = read.table("./test/X_test.txt")

y_test = read.table("./test/Y_test.txt")

y_train = read.table("./train/Y_train.txt")
subject_train = read.table("./train/subject_train.txt")
subject_test = read.table("./test/subject_test.txt")
features = read.table("features.txt")

#Then the following lines merge the training and testing dataset into one dataframe
x1_train <- cbind(x_train,y_train)
x2_train <- cbind(x1_train,subject_train)
x1_test <- cbind(x_test,y_test)
x2_test <- cbind(x1_test,subject_test)
total2 <- rbind(x2_train,x2_test)

#Now the whole data frame is total2, Then use the descriptive activity names to name the activities in the data set
features[,2] <- as.character(features[,2])
for(i in 1:561){
colnames(total2)[i] <- features[i,2]} 
colnames(total2)[562] <- "activity labels"
colnames(total2)[563] <- "subject labels"


#Then extracts only the measurements on the mean and standard deviation for each measurement. The new dataframe is total3. 
total3 <- total2[,grep("mean|std|activity labels|subject labels",colnames(total2))]

#Now labels the activity variable in the dataframe with descriptive variable names
rownumber <- dim(total3)[1]
for (i in 1:rownumber){
if(total3[i,80]==1){total3[i,80] <- "WALKING"}
else if(total3[i,80]==2){total3[i,80] <- "WALKING_UPSTAIRS"}
else if(total3[i,80]==3){total3[i,80] <- "WALKING_DOWNSTAIRS"}
else if(total3[i,80]==4){total3[i,80] <- "SITTING"}
else if(total3[i,80]==5){total3[i,80] <- "STANDING"}
else {total3[i,80] <- "LAYING"}
}

# Creates a second, independent data set with the average of each variable for each activity and each subject. Column 80 is activity and column 81 is subject
spins = split(total3[,1:79],total3[,80:81])
vname <- names(spins)
x5 = data.frame(vname)
lens <- length(vname)
for (j in 1:79){
for (i in 1:lens){
v1[i] <- mean(spins[[i]][,j])}
x5 <- cbind(x5,v1)
}
for(i in 2:80){
colnames(x5)[i] <- colnames(total3)[i-1]}

#Finally write the output
write.table(x5,file="outputdata.txt",row.names= FALSE)







