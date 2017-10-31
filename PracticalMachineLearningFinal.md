Backgroung
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The data for this project come from this source: <http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>.

Goals
=====

The goal of the project is to predict the manner in which they did the exercise. This is the ***classe*** variable in the training set.

Things to clarify:

-   how the model is built;
-   how cross validation is used;
-   determine the expected out of sample error;
-   explain the choices which have been made.

Exploratory data analysis
=========================

The data set is already slpitted in testing and training ones. So we will work in this part of the work oly on the training data set; let's start loading the data:

``` r
har <- read.csv("./data/pml-training.csv", na.strings=c("","NA"))
```

Note that we are trnsforming blank cells in NA.
We look now for some basic information:

``` r
dim(har)
```

    ## [1] 19622   160

So we have 160 variables. The partecipants use accelerometers on the belt, forearm, arm, and dumbell. Each of them brings 38 variables, therefore 4x38=152 variables come from there. For example for the variable *belt* we have:

``` r
grep("belt",names(har), value = T)
```

    ##  [1] "roll_belt"            "pitch_belt"           "yaw_belt"            
    ##  [4] "total_accel_belt"     "kurtosis_roll_belt"   "kurtosis_picth_belt" 
    ##  [7] "kurtosis_yaw_belt"    "skewness_roll_belt"   "skewness_roll_belt.1"
    ## [10] "skewness_yaw_belt"    "max_roll_belt"        "max_picth_belt"      
    ## [13] "max_yaw_belt"         "min_roll_belt"        "min_pitch_belt"      
    ## [16] "min_yaw_belt"         "amplitude_roll_belt"  "amplitude_pitch_belt"
    ## [19] "amplitude_yaw_belt"   "var_total_accel_belt" "avg_roll_belt"       
    ## [22] "stddev_roll_belt"     "var_roll_belt"        "avg_pitch_belt"      
    ## [25] "stddev_pitch_belt"    "var_pitch_belt"       "avg_yaw_belt"        
    ## [28] "stddev_yaw_belt"      "var_yaw_belt"         "gyros_belt_x"        
    ## [31] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
    ## [34] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
    ## [37] "magnet_belt_y"        "magnet_belt_z"

The remain variables are the first 7:

``` r
names(har)[1:7]
```

    ## [1] "X"                    "user_name"            "raw_timestamp_part_1"
    ## [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
    ## [7] "num_window"

and the last one, which is the outsome of our analysis:

``` r
names(har)[160]
```

    ## [1] "classe"

Let's give a look at the first 7 variables:

``` r
head(har[,1:7])
```

    ##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
    ## 1 1  carlitos           1323084231               788290 05/12/2011 11:23
    ## 2 2  carlitos           1323084231               808298 05/12/2011 11:23
    ## 3 3  carlitos           1323084231               820366 05/12/2011 11:23
    ## 4 4  carlitos           1323084232               120339 05/12/2011 11:23
    ## 5 5  carlitos           1323084232               196328 05/12/2011 11:23
    ## 6 6  carlitos           1323084232               304277 05/12/2011 11:23
    ##   new_window num_window
    ## 1         no         11
    ## 2         no         11
    ## 3         no         11
    ## 4         no         12
    ## 5         no         12
    ## 6         no         12

So we see the first is just a progressive number, than the name of the partecipant, the next 6 are time/date related.

We can see there are 6 partecipants:

``` r
table(har$user_name)
```

    ## 
    ##   adelmo carlitos  charles   eurico   jeremy    pedro 
    ##     3892     3112     3536     3070     3402     2610

We can see moreover that the outcome can take 5 different values:

``` r
table(har$classe)
```

    ## 
    ##    A    B    C    D    E 
    ## 5580 3797 3422 3216 3607

The letter A means the exercise was done correctly, the other letters mean a common exercise mistake was done.

For the goal of this project we do not need the first 7 variables, so I remove them:

``` r
myData<-har[8:160]
```

Let's check if there are missing data:

``` r
infoNa<-apply(myData, 2, function(x){z<-sum(is.na(x))})
listNa<-infoNa[infoNa>0]
print(listNa)
```

    ##       kurtosis_roll_belt      kurtosis_picth_belt        kurtosis_yaw_belt 
    ##                    19216                    19216                    19216 
    ##       skewness_roll_belt     skewness_roll_belt.1        skewness_yaw_belt 
    ##                    19216                    19216                    19216 
    ##            max_roll_belt           max_picth_belt             max_yaw_belt 
    ##                    19216                    19216                    19216 
    ##            min_roll_belt           min_pitch_belt             min_yaw_belt 
    ##                    19216                    19216                    19216 
    ##      amplitude_roll_belt     amplitude_pitch_belt       amplitude_yaw_belt 
    ##                    19216                    19216                    19216 
    ##     var_total_accel_belt            avg_roll_belt         stddev_roll_belt 
    ##                    19216                    19216                    19216 
    ##            var_roll_belt           avg_pitch_belt        stddev_pitch_belt 
    ##                    19216                    19216                    19216 
    ##           var_pitch_belt             avg_yaw_belt          stddev_yaw_belt 
    ##                    19216                    19216                    19216 
    ##             var_yaw_belt            var_accel_arm             avg_roll_arm 
    ##                    19216                    19216                    19216 
    ##          stddev_roll_arm             var_roll_arm            avg_pitch_arm 
    ##                    19216                    19216                    19216 
    ##         stddev_pitch_arm            var_pitch_arm              avg_yaw_arm 
    ##                    19216                    19216                    19216 
    ##           stddev_yaw_arm              var_yaw_arm        kurtosis_roll_arm 
    ##                    19216                    19216                    19216 
    ##       kurtosis_picth_arm         kurtosis_yaw_arm        skewness_roll_arm 
    ##                    19216                    19216                    19216 
    ##       skewness_pitch_arm         skewness_yaw_arm             max_roll_arm 
    ##                    19216                    19216                    19216 
    ##            max_picth_arm              max_yaw_arm             min_roll_arm 
    ##                    19216                    19216                    19216 
    ##            min_pitch_arm              min_yaw_arm       amplitude_roll_arm 
    ##                    19216                    19216                    19216 
    ##      amplitude_pitch_arm        amplitude_yaw_arm   kurtosis_roll_dumbbell 
    ##                    19216                    19216                    19216 
    ##  kurtosis_picth_dumbbell    kurtosis_yaw_dumbbell   skewness_roll_dumbbell 
    ##                    19216                    19216                    19216 
    ##  skewness_pitch_dumbbell    skewness_yaw_dumbbell        max_roll_dumbbell 
    ##                    19216                    19216                    19216 
    ##       max_picth_dumbbell         max_yaw_dumbbell        min_roll_dumbbell 
    ##                    19216                    19216                    19216 
    ##       min_pitch_dumbbell         min_yaw_dumbbell  amplitude_roll_dumbbell 
    ##                    19216                    19216                    19216 
    ## amplitude_pitch_dumbbell   amplitude_yaw_dumbbell       var_accel_dumbbell 
    ##                    19216                    19216                    19216 
    ##        avg_roll_dumbbell     stddev_roll_dumbbell        var_roll_dumbbell 
    ##                    19216                    19216                    19216 
    ##       avg_pitch_dumbbell    stddev_pitch_dumbbell       var_pitch_dumbbell 
    ##                    19216                    19216                    19216 
    ##         avg_yaw_dumbbell      stddev_yaw_dumbbell         var_yaw_dumbbell 
    ##                    19216                    19216                    19216 
    ##    kurtosis_roll_forearm   kurtosis_picth_forearm     kurtosis_yaw_forearm 
    ##                    19216                    19216                    19216 
    ##    skewness_roll_forearm   skewness_pitch_forearm     skewness_yaw_forearm 
    ##                    19216                    19216                    19216 
    ##         max_roll_forearm        max_picth_forearm          max_yaw_forearm 
    ##                    19216                    19216                    19216 
    ##         min_roll_forearm        min_pitch_forearm          min_yaw_forearm 
    ##                    19216                    19216                    19216 
    ##   amplitude_roll_forearm  amplitude_pitch_forearm    amplitude_yaw_forearm 
    ##                    19216                    19216                    19216 
    ##        var_accel_forearm         avg_roll_forearm      stddev_roll_forearm 
    ##                    19216                    19216                    19216 
    ##         var_roll_forearm        avg_pitch_forearm     stddev_pitch_forearm 
    ##                    19216                    19216                    19216 
    ##        var_pitch_forearm          avg_yaw_forearm       stddev_yaw_forearm 
    ##                    19216                    19216                    19216 
    ##          var_yaw_forearm 
    ##                    19216

We see there are 100 variables which contain ***NA*** in 19216 rows, on a total of 19622, which correspond to 97.9%. We can therefore safely remove these variables from our data set:

``` r
namesListNa<-names(listNa)
myData[,namesListNa] <-list(NULL)
```

Finally my data set has the following dimensions:

``` r
dim(myData)
```

    ## [1] 19622    53

Moreover, a part the last memmber ***classe*** which is a factor, we can see that there are only member of the classes:

``` r
unique(sapply(myData[-length(myData)],class))
```

    ## [1] "numeric" "integer"

Slicing Data for Cross Validation
=================================

To have a model evaluation, We use a random subsampling to create from our (training) data the sub-training and a sub-test sets:

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
set.seed(123)
inTrain <- createDataPartition(y=myData$classe, p=0.75, list=FALSE)
training <- myData[inTrain,]
testing <- myData[-inTrain,]
```

We will use them in the next section.

Model building
==============

We are now ready to build a model. Because the outcome is a factor which can takes 5 values, we use the *random forest* approach. Note that in this case we do not need a preprocessing of the data:

``` r
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
set.seed(457)
system.time(modelFit <-randomForest(classe ~.,data=training))
```

    ##    user  system elapsed 
    ## 127.824   0.872 128.873

At this point we can check how good is our model, comparing the prediction with the test set:

``` r
pred<-predict(modelFit, testing)
confusionMatrix(pred,testing$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1394    1    0    0    0
    ##          B    1  946    7    0    0
    ##          C    0    2  848    9    0
    ##          D    0    0    0  793    2
    ##          E    0    0    0    2  899
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9951          
    ##                  95% CI : (0.9927, 0.9969)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9938          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9993   0.9968   0.9918   0.9863   0.9978
    ## Specificity            0.9997   0.9980   0.9973   0.9995   0.9995
    ## Pos Pred Value         0.9993   0.9916   0.9872   0.9975   0.9978
    ## Neg Pred Value         0.9997   0.9992   0.9983   0.9973   0.9995
    ## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
    ## Detection Rate         0.2843   0.1929   0.1729   0.1617   0.1833
    ## Detection Prevalence   0.2845   0.1945   0.1752   0.1621   0.1837
    ## Balanced Accuracy      0.9995   0.9974   0.9945   0.9929   0.9986

We see we have *accuracy*, *Sensitivity* and *Specificity* all above 99%, so we can conclude the model works very well.

Summary
=======

We have build a *random forest* model to predict the manner in which a people do an exercise. This choice comes mainly from the categorical nature of the outcome variable. To cross validate the model we splitted the original training data in two sub data, i.e. a training and a testing one. According to this we have verified our model has an accuracy above 99%.
