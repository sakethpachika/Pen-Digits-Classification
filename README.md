# Pen-Digits-Classification


**Objective :**

To build a multi-class classification model which can accurately classify the numeric digits written on a pressure sensitive tablet.

**Data Overview:**

The database created has 250 samples for each of 44 writers, 30 writers are used for training and 14 writers are used for testing purpose, The data-set is normalized and re-sampled such that each digit is represented by a sequence of 8 points as (x,y) coordinates, the data is scaled between 0-100

The scaled data has no missing values and both the training and test data has the least imbalance.


Multiple models are built to find the best possible classification model

## Conclusion:

The classification accuracy for most of the models constructed were above ~95%, among these Ensemble (XGBoost + KNN) and KNN performed best.Random Forests and SVM have also performed good with efficiencies of 96% and 95% respectively. As expected multi-class logistic regression, naive-bayes did not produce desirable results for multi-class classification. 
