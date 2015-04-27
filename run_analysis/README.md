Getting and Cleaning Data Course Project

How the script works:

It:
1.    Downloads data sets. 
2.    Stores the data into the directory `./data` in the working directory.
3.    Merges the trainig and testing data sets.
4.    Exteacts only the mean and standard deviation variables.
5.    Replaces the activity codes with descriptive names.
6.    Creates a data set with the average of each variable extracted in step 4.
7.    Stores the resulting data set in the text file `./tidy_data.txt`. 