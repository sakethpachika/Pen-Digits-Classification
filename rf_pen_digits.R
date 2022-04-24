require(randomForest)
require(caret)
require(ggplot2)
pen_digits = read.csv("pendigits_training_f.csv")

pen_digits$Digit <- as.factor(pen_digits$Digit)
set.seed(1286)
train = sample(1:nrow(pen_digits), floor(0.8 * nrow(pen_digits)))

rf.pen_digits = randomForest(Digit~., data=pen_digits[train, ],ntree=1000)
rf.predict = predict(rf.pen_digits, pen_digits[-train, ], type="class")
#with(pen_digits[-train, ], table(rf.predict, Digit))
rf.cm = confusionMatrix(table(rf.predict, pen_digits[-train, ]$Digit))
rf.cm
#rf.pen_digits
#plot(rf.predict)
##mtry=6
rf.pen_digits_6 = randomForest(Digit~., data=pen_digits[train, ],ntree=1000,mtry=6)
rf.predict_6 = predict(rf.pen_digits_6, pen_digits[-train, ], type="class")
#with(pen_digits[-train, ], table(rf.predict, Digit))
rf.cm_6 = confusionMatrix(table(rf.predict_6, pen_digits[-train, ]$Digit))
rf.cm_6
#rf.pen_digits
#plot(rf.predict)
##mtry=9
rf.pen_digits_9 = randomForest(Digit~., data=pen_digits[train, ],ntree=1000,mtry=16)
rf.predict_9 = predict(rf.pen_digits_9, pen_digits[-train, ], type="class")
with(pen_digits[-train, ], table(rf.predict, Digit))
rf.cm_9 = confusionMatrix(table(rf.predict_9, pen_digits[-train, ]$Digit))
rf.cm_9
rf.pen_digits
plot(rf.predict)



##Test data 
pen_digits_testdata=read.csv("pen_digits_tesfile.csv")
set.seed(128)
pen_digits_testdata$Digit <- as.factor(pen_digits_testdata$Digit)
rf.predict_test_data = predict(rf.pen_digits, pen_digits_testdata, type="class")
with(pen_digits_testdata, table(rf.predict_test_data, Digit))
rf.cm = confusionMatrix(table(rf.predict_test_data,pen_digits_testdata$Digit))
rf.cm





#Testing 14260
pen_digits_pincode=read.csv("pen_digits_predict_f.csv")
pen_digits_pincode$Digit <- as.factor(pen_digits_pincode$Digit)
pendigits_pincode_pred = predict(rf.pen_digits, pen_digits_pincode, type="class")
pendigits_pincode_pred



##Heat-map CM
#generate random data 
data = data.frame(pen_digits_testdata$Digit,rf.predict_test_data)
names(data) = c("Actual", "Predicted")

#compute frequency of actual categories
actual = as.data.frame(table(data$Actual))
names(actual) = c("Actual","ActualFreq")

#build confusion matrix
confusion = as.data.frame(table(data$Actual, data$Predicted))
names(confusion) = c("Actual","Predicted","Freq")

#calculate percentage of test cases based on actual frequency
confusion = merge(confusion, actual)
confusion$Percent = confusion$Freq/confusion$ActualFreq*100

#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +
  geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
  labs(x="Actual",y="Predicted")
tile = tile + 
  geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
  scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile





