# Getting and Cleaning Data - Course Project
Final Project for Coursera Getting and Cleaning Data by Johns Hopkins University

In this project, data collected from <a href="http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones">Human Activity Recognition Using Smartphones</a>, 
worked with, and cleaned, to prepare a data that ready to used for later analysis.

This repository contains the following files:
<li>
<b>README.md</b>, which provides an overview of the data set and introduction.</li>
<li><b>tidy_data.txt</b>, which contains the final output of data set.</li>
<li><b>CodeBook.md</b>, the code book, which describes the contents of the data set 
(data, variables and transformations used to generate the data).</li>
<li><b>run_analysis.R</b>, the R script that was used to create the data set.
</li>


## Study design
<a href="http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones">Human Activity Recognition Using Smartphones</a>
is our data source, which describes how the data was initially collected as follows:

>The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

>The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Training and test data were first merged together to create one data set, 
then the measurements on the mean and standard deviation were extracted for each measurement 
(79 variables extracted from the original 561).
Finally, the measurements were averaged for each subject and activity, resulting in the final data set.

## Creating data set
un_analysis.R can be used to create the data set.
It retrieves the source data set and transforms it to produce the final data set by implementing the following steps 
:

* Unzip source data if it doesn't exist.
* Read data.
* Merge the training and the test sets to create one data set.
* Extract only the measurements on the mean and standard deviation for each measurement.
* Use descriptive activity names to name the activities in dataset.
* Appropriately label the data set with descriptive variable names.
* Create a tidy set with the average of each variable for each activity and each subject.

Write the data set to the tidy_data.txt file.
The tidy_data.txt in this repository was created by running the run_analysis.R
