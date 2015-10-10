wbcd <- read.csv("C:/Users/Jasmeet/Documents/R/wisc_bc_data.csv", stringsAsFactors= FALSE)
str(wbcd)
wbcd <- wbcd[-1]
str(wbcd)
table(wbcd$diagnosis)
factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Maligant"))
prop.table(table(wbcd$diagnosis))
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n[c("radius_mean","area_mean","smoothness_mean")])
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]
install.packages("class")
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
#install.packages("gmodels")
#library("gmodels")
#CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

table(wbcd_test_pred, wbcd_test_labels)  
knnTab <- table( wbcd_test_pred, wbcd_test_labels) 
sum(knnTab[row(knnTab)==col(knnTab)])/sum(knnTab)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
