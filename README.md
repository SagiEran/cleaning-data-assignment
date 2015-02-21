# cleaning-data-assignment
cleaning data assignment project for the course

Code book
The tidy data in tidy_data_set.txt can be read into R with the following code:
read.table("tidy_data_set.txt", header=TRUE, colClasses=c('factor', 'factor', rep('numeric', 66)))



Overview
The tidy_data_set.txt file is a tidy subset of the data provided in the Human Activity Recognition Using Smartphones Data Set. The source data is available from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones# and it's also included in the UCI HAR Dataset directory in this repo. 

tidy_data_set.txt includes the combined test and training data sets. README.md includes a walk through of the data cleansing and transformation steps used to create tidy_data_set.txt.

The tidy data set is a subset of this combined data that includes only measurements on the mean and standard deviation for each measure. This reduces the data to 68 columns (68 feature variable measurements, plus the TestSubject and Activity columns). The size of the tidy data set was further reduced by averaging each variable for each activity and each subject. This resulted in 180 rows of data, with a unique combination of TestSubject and Activity values in each row. 



Data dictionary
The variables in this tidy data set are a subset of the variables described in the features_info.txt file in the original data set. features_info.txt provides a more in-depth overview of the original values and how they were calculated.

1.TestSubject - A factor that identifies the volunteer participant.


Values: integer from 1 to 30


2.Activity - A factor that identifies the activity being performed


Values: Walking, WalkingUpStairs, WalkingDownStairs, Sitting, Standing, Lying

The feature variables below (#3 - #68) are each an average of the values collected for the test subject and activity specified in the data row. For each, the value is a numeric normalized and bounded within [-1, 1]


3.MeanBodyAccelerationXAxis - The mean of the body acceleration on the X axis. 


4.MeanBodyAccelerationYAxis - The mean of the body acceleration on the Y axis.


5.MeanBodyAccelerationZAxis - The mean of the body acceleration on the Z axis.


6.MeanGravityAccelerationXAxis - The mean of the gravity acceleration on the X axis. 


7.MeanGravityAccelerationYAxis - The mean of the gravity acceleration on the Y axis.


8.MeanGravityAccelerationZAxis - The mean of the gravity acceleration on the Z axis.


9.MeanBodyAccelerationJerkXAxis - The mean of the body acceleration on the X axis, derived in time to obtain Jerk signals. 


10.MeanBodyAccelerationJerkYAxis - The mean of the body acceleration on the Y axis, derived in time to obtain Jerk signals.


11.MeanBodyAccelerationJerkZAxis - The mean of the body acceleration on the Z axis, derived in time to obtain Jerk signals.


12.MeanBodyAngularVelocityXAxis - The mean of the body angular velocity on the X axis.


13.MeanBodyAngularVelocityYAxis - The mean of the body angular velocity on the Y axis.


14.MeanBodyAngularVelocityZAxis - The mean of the body angular velocity on the Z axis.


15.MeanBodyAngularVelocityJerkXAxis - The mean of the body angular velocity on the X axis, derived in time to obtain Jerk signals.


16.MeanBodyAngularVelocityJerkYAxis - The mean of the body angular velocity on the Y axis, derived in time to obtain Jerk signals.


17.MeanBodyAngularVelocityJerkZAxis - The mean of the body angular velocity on the Z axis, derived in time to obtain Jerk signals.


18.MeanBodyAccelerationMagnitude - The mean of the body acceleration magnitude, calculated using the Euclidean norm.


19.MeanGravityAccelerationMagnitude - The mean of the gravity acceleration magnitude.


20.MeanBodyAccelerationJerkMagnitude - The mean of the body acceleration magnitude derived in time to obtain Jerk signals.


21.MeanBodyAngularVelocityMagnitude - The mean of the angular velocity magnitude.


22.MeanBodyAngularVelocityJerkMagnitude - The mean of the angular velocity magnitude derived in time to obtain Jerk signals.


23.MeanFFTBodyAccelerationXAxis - The mean of the body acceleration on the X axis, with a Fast Fourier Transform (FFT) applied. 


24.MeanFFTBodyAccelerationYAxis - The mean of the body acceleration on the Y axis, with a Fast Fourier Transform (FFT) applied.


25.MeanFFTBodyAccelerationZAxis - The mean of the body acceleration on the Z axis, with a Fast Fourier Transform (FFT) applied.


26.MeanFFTBodyAccelerationJerkXAxis - The mean of the body acceleration on the X axis, derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied. 


27.MeanFFTBodyAccelerationJerkYAxis - The mean of the body acceleration on the Y axis, derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.


28.MeanFFTBodyAccelerationJerkZAxis - The mean of the body acceleration on the Z axis, derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.


29.MeanFFTBodyAngularVelocityXAxis - The mean of the body angular velocity on the X axis, with a Fast Fourier Transform (FFT) applied.


30.MeanFFTBodyAngularVelocityYAxis - The mean of the body angular velocity on the Y axis, with a Fast Fourier Transform (FFT) applied.


31.MeanFFTBodyAngularVelocityZAxis - The mean of the body angular velocity on the Z axis, with a Fast Fourier Transform (FFT) applied.


32.MeanFFTBodyAccelerationMagnitude - The mean of the body acceleration magnitude, with a Fast Fourier Transform (FFT) applied.


33.MeanFFTBodyAccelerationJerkMagnitude - The mean of the body acceleration magnitude derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.


34.MeanFFTBodyAngularVelocityMagnitude - The mean of the angular velocity magnitude, with a Fast Fourier Transform (FFT) applied.


35.MeanFFTBodyAngularVelocityJerkMagnitude - The mean of the angular velocity magnitude derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.


36.StandardDeviationBodyAccelerationXAxis - The standard deviation of the body acceleration on the X axis. 


37.StandardDeviationBodyAccelerationYAxis - The standard deviation of the body acceleration on the Y axis.


38.StandardDeviationBodyAccelerationZAxis - The standard deviation of the body acceleration on the Z axis.


39.StandardDeviationGravityAccelerationXAxis - The standard deviation of the gravity acceleration on the X axis. 


40.StandardDeviationGravityAccelerationYAxis - The standard deviation of the gravity acceleration on the Y axis.


41.StandardDeviationGravityAccelerationZAxis - The standard deviation of the gravity acceleration on the Z axis.


42.StandardDeviationBodyAccelerationJerkXAxis - The standard deviation of the body acceleration on the X axis, derived in time to obtain Jerk signals. 


43.StandardDeviationBodyAccelerationJerkYAxis - The standard deviation of the body acceleration on the Y axis, derived in time to obtain Jerk signals.


44.StandardDeviationBodyAccelerationJerkZAxis - The standard deviation of the body acceleration on the Z axis, derived in time to obtain Jerk signals.


45.StandardDeviationBodyAngularVelocityXAxis - The standard deviation of the body angular velocity on the X axis. 


46.StandardDeviationBodyAngularVelocityYAxis - The standard deviation of the body angular velocity on the Y axis.


47.StandardDeviationBodyAngularVelocityZAxis - The standard deviation of the body angular velocity on the Z axis.


48.StandardDeviationBodyAngularVelocityJerkXAxis - The standard deviation of the body angular velocity on the X axis, derived in time to obtain Jerk signals. 


49.StandardDeviationBodyAngularVelocityJerkYAxis - The standard deviation of the body angular velocity on the Y axis, derived in time to obtain Jerk signals.


50.StandardDeviationBodyAngularVelocityJerkZAxis - The standard deviation of the body angular velocity on the Z axis, derived in time to obtain Jerk signals.


51.StandardDeviationBodyAccelerationMagnitude - The standard deviation of the body acceleration magnitude, calculated using the Euclidean norm.


52.StandardDeviationGravityAccelerationMagnitude - The standard deviation of the gravity acceleration magnitude.


53.StandardDeviationBodyAccelerationJerkMagnitude - The standard deviation of the body acceleration magnitude derived in time to obtain Jerk signals.


54.StandardDeviationBodyAngularVelocityMagnitude - The standard deviation of the angular velocity magnitude.


55.StandardDeviationBodyAngularVelocityJerkMagnitude - The standard deviation of the angular velocity magnitude derived in time to obtain Jerk signals.


56.StandardDeviationFFTBodyAccelerationXAxis - The standard deviation of the body acceleration on the X axis, with a Fast Fourier Transform (FFT) applied. 


57.StandardDeviationFFTBodyAccelerationYAxis - The standard deviation of the body acceleration on the Y axis, with a Fast Fourier Transform (FFT) applied.


58.StandardDeviationFFTBodyAccelerationZAxis - The standard deviation of the body acceleration on the Z axis, with a Fast Fourier Transform (FFT) applied.


59.StandardDeviationFFTBodyAngularVelocityJerkXAxis - The standard deviation of the body angular velocity on the X axis, derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied. 


60.StandardDeviationFFTBodyAngularVelocityJerkYAxis - The standard deviation of the body angular velocity on the Y axis, derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.


61.StandardDeviationFFTBodyAngularVelocityJerkZAxis - The standard deviation of the body angular velocity on the Z axis, derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.


62.StandardDeviationFFTBodyAngularVelocityXAxis - The standard deviation of the body angular velocity on the X axis, with a Fast Fourier Transform (FFT) applied. 


63.StandardDeviationFFTBodyAngularVelocityYAxis - The standard deviation of the body angular velocity on the Y axis, with a Fast Fourier Transform (FFT) applied.


64.StandardDeviationFFTBodyAngularVelocityZAxis - The standard deviation of the body angular velocity on the Z axis, with a Fast Fourier Transform (FFT) applied.


65.StandardDeviationFFTBodyAccelerationMagnitude - The standard deviation of the body acceleration magnitude, with a Fast Fourier Transform (FFT) applied.


66.StandardDeviationFFTBodyAccelerationJerkMagnitude - The standard deviation of the body acceleration magnitude derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.


67.StandardDeviationFFTBodyAngularVelocityMagnitude - The standard deviation of the angular velocity magnitude, with a Fast Fourier Transform (FFT) applied.


68.StandardDeviationFFTBodyAngularVelocityJerkMagnitude - The standard deviation of the angular velocity magnitude derived in time to obtain Jerk signals, with a Fast Fourier Transform (FFT) applied.




Note - Variable naming
varible names had been updated in the tidy data (vs. the names in the source data described in features_info.txt) to make it easier for consumers of the data to understand the measurement of each column. The names use the following conventions:
•The first word(s) indicates the type of metric, Mean or StandardDeviation 
•Next, FFT is included, if applicable, to indicate that a Fast Fourier Transform (FFT) had been applied. 
•Either Body or Gravity is included to indicate the type of signal
•Either Acceleration or AngularVelocity is included to indicate the type of measurement.
• Jerk indicates a measure where the body linear acceleration and angular velocity were derived in time to obtain
  Jerk signals
• Magnitude indicates a magnitude calculation
• XAxis, YAxis, and ZAxis indicate a measurement for the X, Y, or Z axis.

Here are some example mappings of old to new column names:
•Old: tBodyAcc-mean()-X
•New: MeanBodyAccelerationXAxis


•Old: tGravityAcc-std()-Y
•New: StandarddeviationGravityAccelerationYAxis


•Old: fBodyGyroMag-std()
•New: StandardDeviationFFTBodyAngularVelocityMagnitude
