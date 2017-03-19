dat = read.csv("ENEM_LEVE_READY.csv", header = TRUE)
#dat <- subset(dat, select=+c(IDADE, IN_DEFICIENCIA_MENTAL, NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, ESCOLA_PRIVADA, LOCALIZACAO_RURAL, NOTA_RED_LABEL)) # 2 (melhorou)
#dat <- subset(dat, select=+c(IDADE, IN_DEFICIENCIA_MENTAL, NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, ESCOLA_PRIVADA, NOTA_RED_LABEL)) # 3 (melhorou)
#dat <- subset(dat, select=+c(IDADE, NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, ESCOLA_PRIVADA, NOTA_RED_LABEL)) # 4 (piorou, desfazendo)
#dat <- subset(dat, select=+c(IN_DEFICIENCIA_MENTAL, NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, ESCOLA_PRIVADA, NOTA_RED_LABEL)) # 5 (piorou, desfazendo)
dat <- subset(dat, select=+c(GENERO_HOMEM, IDADE, IN_DEFICIENCIA_MENTAL, NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, ESCOLA_PRIVADA, NOTA_RED_LABEL)) # 6 (melhorou um pouco)
#dat <- subset(dat, select=+c(ENSINO_MEDIO_CONCLUIDO, GENERO_HOMEM, IDADE, IN_DEFICIENCIA_MENTAL, NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, ESCOLA_PRIVADA, NOTA_RED_LABEL)) # 7 (pouca diferença, desfazendo)
#dat <- subset(dat, select=+c(ENSINO_MEDIO_NAO_FAZ, GENERO_HOMEM, IDADE, IN_DEFICIENCIA_MENTAL, NOTA_CN, NOTA_CH, NOTA_LC, NOTA_MT, ESCOLA_PRIVADA, NOTA_RED_LABEL)) # 8 (piorou um pouco, desfazendo)

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
