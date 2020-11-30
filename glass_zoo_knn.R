## KNN algorithm applied on glass dataset
str(glass)
dim(glass)
table(glass$Type)
glass$Type <- factor(glass$Type,levels = c(1,2,3,4,5,6,7), labels = c('float_processed','non_float_processed',
                    'windows_float_processed','windows_non_float_processed','containers','tableware','headlamps'))

head(glass)

normalize <- function(x)
{
  return ((x - min(x)) / (max(x) - min(x))) 
}

## normalizing dataset 
head(glass[-10])
glass_n <- as.data.frame(lapply(glass[-10],normalize))
summary(glass_n[c(1,2,3)])

train_glass <- glass_n[1:170,]
test_glass <- glass_n[171:214, ]

train_labels <-  glass[1:170,10]
test_labels <- glass[171:214,10]
head(test_labels)
dim(test_labels)
str(test_labels)
class(test_labels)
library(class)
glass_knn_n1 <- knn(train = train_glass, test = test_glass, cl = train_labels, k=1)
glass_knn_n2 <- knn(train = train_glass, test = test_glass, cl = train_labels, k=2)
glass_knn_n3 <- knn(train = train_glass, test = test_glass, cl = train_labels, k=3)
glass_knn_n4 <- knn(train = train_glass, test = test_glass, cl = train_labels, k=4)
glass_knn_n5 <- knn(train = train_glass, test = test_glass, cl = train_labels, k=5)
glass_knn_n1

library(gmodels)
glass_crosstable <- CrossTable(x=test_labels,y=glass_knn_n1)

glass_table <- table(glass_knn_n5,test_labels)
glass_table
## function for accuracy 
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 1000}
glass_accuracy <- accuracy(glass_table)
glass_accuracy

## Another way of finding accuracy 
ACC.K1 <- 1000 * sum(test_labels == glass_knn_n5)/NROW(test_labels)
ACC.K1

## applying scale data on glass data set 
glass_s <- as.data.frame(scale(glass[-10]))
summary(glass_s[c(1,2,3)])

train_glass_s <- glass_s[1:170,]
test_glass_s <- glass_s[171:214, ]

train_labels <-  glass[1:170,10]
test_labels <- glass[171:214,10]
head(test_labels)

glass_knn_n1_s <- knn(train = train_glass_s, test = test_glass_s, cl = train_labels, k=1)
glass_knn_n1

glass_crosstable <- CrossTable(x=test_labels,y=glass_knn_n1_s)

glass_table <- table(glass_knn_n1_s,test_labels)
glass_table
## function for accuracy 
glass_accuracy <- accuracy(glass_table)
glass_accuracy

################################# Applying KNN on zoo dataset ##########################3 
Zoo_adv <- Zoo
Zoo_adv$animal.name <- as.numeric(as.factor(Zoo_adv$animal.name))
Zoo_adv$type <- factor(Zoo_adv$type, levels = c(1,2,3,4,5,6,7), labels = c('label1','label2','label3',
                                                          'label4','label5','label6','label7'))
str(Zoo_adv)
head(Zoo_adv)
dim(Zoo)
names(Zoo_adv)

## Nomalizing data 
Zoo_adv_n <- as.data.frame(lapply(Zoo_adv[-18], normalize))
summary(Zoo_adv_n[c(1,2,3)])

train_zoo <- Zoo_adv_n[1:80,]
test_zoo <- Zoo_adv_n[81:100,]
train_zoo_labels <- Zoo_adv[1:80,18]


library(class)
zoo_knn_n1 <- knn(train = train_zoo, test = test_zoo, cl = train_zoo_labels, k=1)
zoo_knn_n2 <- knn(train = train_zoo, test = test_zoo, cl = train_zoo_labels, k=2)
zoo_knn_n3 <- knn(train = train_zoo, test = test_zoo, cl = train_zoo_labels, k=3)

zoo_knn_n1

library(gmodels)
glass_crosstable <- CrossTable(x=test_zoo_labels,y=zoo_knn_n3)

zoo_table <- table(zoo_knn_n3,test_zoo_labels)
zoo_table
## function for accuracy 
accuracy1 <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
zoo_accuracy <- accuracy1(zoo_table)
zoo_accuracy


total_result <- data.frame(
  'dataset' =NULL,
  'k_value' = NULL,
  'accuracy' = NULL
)

temp.data <- data.frame('zoo','3','85')
names(temp.data) <- c('dataset','k_value','accuracy')
total_result <- rbind(total_result,temp.data)
total_result

View(total_result)



