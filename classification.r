dat = read.csv("ENEM_LEVE_READY.csv", header = TRUE)

# Separa em dois grupos: training_dat e test_dat
set.seed(4) # https://xkcd.com/221/
idx <- sample(2, nrow(dat), replace=TRUE, prob=c(0.67, 0.33))

# Datasets: dados removendo a label
training_dat <- dat[idx==1, 1:ncol(dat)]
test_dat <- dat[idx==2, 1:ncol(dat)]
training_dat <- subset(training_dat, select=-c(NOTA_RED_LABEL))
test_dat <- subset(test_dat, select=-c(NOTA_RED_LABEL))

# Respostas: apenas a label
training_ans <- dat[idx==1, "NOTA_RED_LABEL"]
training_ans <- data.frame(training_ans)$training_ans
test_ans <- dat[idx==2, "NOTA_RED_LABEL"]
test_ans <- data.frame(test_ans)$test_ans

# Classificação utilizando KNN
library(class)
pred <- knn(train = training_dat, test = test_dat, cl = training_ans, k=3)

# Exibe os resultados
library(gmodels)
CrossTable(x = test_ans, y = pred)