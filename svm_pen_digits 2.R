require(e1071)
require(caret)
pen_digits = read.csv("pendigits_training_f.csv")

pen_digits$Digit <- as.factor(pen_digits$Digit)
set.seed(1286)
train = sample(1:nrow(pen_digits), floor(0.8 * nrow(pen_digits)))

svm.pen_digits = svm(Digit~., data=pen_digits[train, ],kernel="linear",cost=10)
svm.predict = predict(svm.pen_digits, pen_digits[-train, ], type="class")
with(pen_digits[-train, ], table(svm.predict,Digit ))
svm.cm = confusionMatrix(table(svm.predict, pen_digits[-train, ]$Digit))
svm.cm

##Test data 
pen_digits_testdata=read.csv("pen_digits_tesfile.csv")
pen_digits_testdata$Digit <- as.factor(pen_digits_testdata$Digit)
svm.predict_test_data = predict(svm.pen_digits, pen_digits_testdata, type="class")
#with(pen_digits_testdata, table(rf.predict_test_data, Digit))
svm.cm = confusionMatrix(table(svm.predict_test_data,pen_digits_testdata$Digit))
svm.cm
plot(svm.predict_test_data)

#Pincode test
pen_digits_pincode=read.csv("pen_digits_predict_f.csv")
pen_digits_pincode$Digit <- as.factor(pen_digits_pincode$Digit)
pendigits_pincode_pred_svm = predict(svm.pen_digits, pen_digits_pincode, type="class")
pendigits_pincode_pred_svm

##Heat-map CM
#generate random data 
data = data.frame(pen_digits_testdata$Digit,svm.predict_test_data)
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
