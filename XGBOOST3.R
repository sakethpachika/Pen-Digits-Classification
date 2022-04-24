library(data.table) 
library(dplyr)      
library(ggplot2)  
library(caret)     
library(xgboost)    
library(e1071)      
library(cowplot)    
library(Matrix)
library(magrittr)
library(DataExplorer)

install.packages("png")
data <- read.csv("train.csv")

actual_test <- read.csv("test.csv")

plot_missing(data)

a <- table(data$digit)/nrow(data)

a_arr <- as.array(a)

b <- as.data.frame(a)

c <- as.data.frame(table(actual_test$digit)/(nrow(actual_test)))


write_xlsx(b,'/Users/sakethpachika/Documents/train_dist.xlsx')

write_xlsx(c,'/Users/sakethpachika/Documents/test_dist.xlsx')


library(caTools)
set.seed(112345)
split = sample.split(data$digit, SplitRatio = 0.8)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

training <- sparse.model.matrix(digit ~ .-1, data = train)     #independent variable
head(training)
train_label <- train[,"digit"]                      #dependent variable
train_matrix <- xgb.DMatrix(data = as.matrix(training), label = train_label)
testing <- sparse.model.matrix(digit~.-1, data = test)
test_label <- test[,"digit"]
test_matrix <- xgb.DMatrix(data = as.matrix(testing), label = test_label)

nc <- length(unique(train_label))

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)

nround = 50

watchlist <- list(train = train_matrix, test = test_matrix)

best_model <- xgb.train(params = xgb_params,
                        data = train_matrix,
                        watchlist = watchlist,
                        nrounds = nround,
                        early_stopping_rounds = 20,
                        eta = 0.1
                        )
xgbcv = xgb.cv(params=xgb_params,data = train_matrix,nrounds=100,nfold=5,showsd=T,stratified=T)



e <- data.frame(best_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')



imp_fearture <- xgb.importance(colnames(train_matrix), model = best_model)
print(imp)

xgb.plot.importance(imp_fearture)



pred <- predict(best_model, newdata = test_matrix)

pred$data

pred_prob <-predict.proba(best_model,newdata=test_matrix)

prediction <- matrix(pred, nrow = nc, ncol = length(pred)/nc) %>%
t() %>%
data.frame() %>%
mutate(label = test_label, max_prob = max.col(., "last")-1)

table(Prediction = prediction$max_prob, Actual = prediction$label)




### Actual test data



testing_actual <- sparse.model.matrix(digit~.-1, data = actual_test)
test_label_actual <- actual_test[,"digit"]
test_matrix_actual <- xgb.DMatrix(data = as.matrix(testing_actual), label = test_label_actual)

pred_actual <- predict(best_model, newdata = test_matrix_actual)
prediction <- matrix(pred_actual, nrow = nc, ncol = length(pred_actual)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label_actual, max_prob = max.col(., "last")-1)

cm <- table(Prediction = prediction$max_prob, Actual = prediction$label)

n  <-  sum(cm) # number of instances
nc  <-  nrow(cm) # number of classes
diag  <-  diag(cm) # number of correctly classified instances per class 
rowsums  <-  apply(cm, 1, sum) # number of instances per class
colsums  <-  apply(cm, 2, sum) # number of predictions per class
p  <-  rowsums / n # distribution of instances over the actual classes
q  <-  colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 

prediction_table = as.data.frame(pred_actual)

cm_df <- as.data.frame.matrix(cm) 

install.packages("writexl")

library("writexl")

image(matrix(cm))

write_xlsx(cm_df,'/Users/sakethpachika/Documents/cm_df.xlsx')

write_xlsx(prediction,'/Users/sakethpachika/Documents/prediction.xlsx')

write_xlsx(actual_test,'/Users/sakethpachika/Documents/actual_test.xlsx')

b <- as.data.frame(pred)



cm <- table(Prediction = prediction$max_prob, Actual = prediction$label)




#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +
  geom_tile(aes(x=prediction$label, y=prediction$max_prob,fill=Percent),data=confusion, color="black",size=0.1) +
  labs(x="Actual",y="Predicted")
tile = tile + 
  geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
  scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile



#generate random data 
data = data.frame(prediction$label,prediction$max_prob)
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


x=c("KNN","Logistic reg","RF","SVM","NB","XGBoost","Ensemble(XGBoost+KNN)")
x = c(1,2,3,4,5,6,7)
y=c(97.6,89.7,96.46,95.63,81.96,96.369,97.59)


y1 =c(99.2,94.6,99.33,98.87,87.38,97.5,99.1)

df <- data.frame(index=c(1,2,3,4,5,6,7),y_test=c(97.6,89.7,96.46,95.63,81.96,96.369,97.59),y_train=c(99.2,94.6,99.33,98.87,87.38,97.5,99.1))

df <- melt(df,id.vars="index",variable.name='series')

ggplot(df,aes(index,value))+geom_line(aes(colour=series))


#load necessary libraries
library(ggplot2)
library(reshape2)

#create data frame 
df <- data.frame(index=c(1, 2, 3, 4, 5, 6),
                 var1=c(4, 4, 5, 4, 3, 2),
                 var2=c(1, 2, 4, 4, 6, 9),
                 var3=c(9, 9, 9, 5, 5, 3))

#melt data frame into long format
df <- melt(df ,  id.vars = 'index', variable.name = 'series')

#create line plot for each column in data frame
ggplot(df, aes(index, value)) +
  geom_line(aes(colour = series))


df <- data.frame(index=c("KNN","Logistic reg","RF","SVM","NB","XGBoost","XGBoost+KNN"),y_test=c(97.6,89.7,96.46,95.63,81.96,96.369,97.59),y_train=c(99.2,94.6,99.33,98.87,87.38,97.5,99.1))

m_data <- melt(df,id.vars=c("index"))
p1 <- ggplot(m_data,aes(x=index,y=value,group=variable,colour=variable))+geom_line()+xlab("Model Type")+ylab("Accuracy")+ggtitle("Accuracy Vs Model Type")+expand_limits(y=0)+geom_point()
p1







