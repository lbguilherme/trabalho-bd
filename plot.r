dat = read.csv("ENEM_LEVE_READY.csv", header = TRUE)
dat$x <- dat$NOTA_CH*500 + dat$NOTA_LC*500
dat$y <- dat$NOTA_CN*500 + dat$NOTA_MT*500
train <- subset(dat, select=+c(x, y))
cl <- subset(dat, select=+c(NOTA_RED_LABEL))[,1]
train <- data.frame(train)

require(MASS)
test <- expand.grid(x=seq(min(0), max(1000), by=25), y=seq(min(0), max(1000), by=25))

require(class)
classif1 <- knn(train, test, cl, k=1)
classif5 <- knn(train, test, cl, k=5)
classif10 <- knn(train, test, cl, k=10)
classif100 <- knn(train, test, cl, k=100)
classif300 <- knn(train, test, cl, k=300)
classif <- classif100

require(dplyr)
dataf1 <- bind_rows(
  mutate(test, grupo="otimo",   prob_grupo=ifelse(classif1==grupo, 1, 0)),
  mutate(test, grupo="regular", prob_grupo=ifelse(classif1==grupo, 1, 0)),
  mutate(test, grupo="ruim",    prob_grupo=ifelse(classif1==grupo, 1, 0)),
  mutate(test, grupo="bom",     prob_grupo=ifelse(classif1==grupo, 1, 0))
)
dataf5 <- bind_rows(
  mutate(test, grupo="otimo",   prob_grupo=ifelse(classif5==grupo, 1, 0)),
  mutate(test, grupo="regular", prob_grupo=ifelse(classif5==grupo, 1, 0)),
  mutate(test, grupo="ruim",    prob_grupo=ifelse(classif5==grupo, 1, 0)),
  mutate(test, grupo="bom",     prob_grupo=ifelse(classif5==grupo, 1, 0))
)
dataf10 <- bind_rows(
  mutate(test, grupo="otimo",   prob_grupo=ifelse(classif10==grupo, 1, 0)),
  mutate(test, grupo="regular", prob_grupo=ifelse(classif10==grupo, 1, 0)),
  mutate(test, grupo="ruim",    prob_grupo=ifelse(classif10==grupo, 1, 0)),
  mutate(test, grupo="bom",     prob_grupo=ifelse(classif10==grupo, 1, 0))
)
dataf100 <- bind_rows(
  mutate(test, grupo="otimo",   prob_grupo=ifelse(classif100==grupo, 1, 0)),
  mutate(test, grupo="regular", prob_grupo=ifelse(classif100==grupo, 1, 0)),
  mutate(test, grupo="ruim",    prob_grupo=ifelse(classif100==grupo, 1, 0)),
  mutate(test, grupo="bom",     prob_grupo=ifelse(classif100==grupo, 1, 0))
)
dataf300 <- bind_rows(
  mutate(test, grupo="otimo",   prob_grupo=ifelse(classif300==grupo, 1, 0)),
  mutate(test, grupo="regular", prob_grupo=ifelse(classif300==grupo, 1, 0)),
  mutate(test, grupo="ruim",    prob_grupo=ifelse(classif300==grupo, 1, 0)),
  mutate(test, grupo="bom",     prob_grupo=ifelse(classif300==grupo, 1, 0))
)
dataf <- dataf100

points <- data.frame(x=train[,1], y=train[,2], grupo=cl)
set.seed(4)
take <- sample(2, nrow(dat), replace=TRUE, prob=c(0.05, 0.95))
points_sampled <- points[take==1, 1:3]

require(ggplot2)

ggplot(dataf) + ggtitle("Notas de redação") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=2.5, data=points, alpha=0.3)
ggsave("plots/01-notas em redação.png")

ggplot(dataf) + ggtitle("Notas de redação (amostra 5%)") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=2.5, data=points_sampled, alpha=0.3)
ggsave("plots/02-notas em redação (amostra 5).png")

ggplot(dataf) + ggtitle("Grade de possíveis notas para previsão") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_point(aes(x=x, y=y), data=test, size=0.9)
ggsave("plots/03-grid.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif), size=0.9)
ggsave("plots/04-grid knn.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN com contorno") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_contour(aes(x=x, y=y, z=prob_grupo, group=grupo, color=grupo), bins=2, data=dataf) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif), size=0.9)
ggsave("plots/05-grid knn com contorno.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN com contorno (k=1)") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_contour(aes(x=x, y=y, z=prob_grupo, group=grupo, color=grupo), bins=2, data=dataf1) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif1), size=0.9)
ggsave("plots/06-grid knn com contorno k=1.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN com contorno (k=5)") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_contour(aes(x=x, y=y, z=prob_grupo, group=grupo, color=grupo), bins=2, data=dataf5) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif5), size=0.9)
ggsave("plots/07-grid knn com contorno k=5.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN com contorno (k=10)") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_contour(aes(x=x, y=y, z=prob_grupo, group=grupo, color=grupo), bins=2, data=dataf10) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif10), size=0.9)
ggsave("plots/08-grid knn com contorno k=10.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN com contorno (k=100)") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_contour(aes(x=x, y=y, z=prob_grupo, group=grupo, color=grupo), bins=2, data=dataf100) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif100), size=0.9)
ggsave("plots/09-grid knn com contorno k=100.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN com contorno (k=300)") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=0, data=points, alpha=0) +
  geom_contour(aes(x=x, y=y, z=prob_grupo, group=grupo, color=grupo), bins=2, data=dataf300) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif300), size=0.9)
ggsave("plots/10-grid knn com contorno k=300.png")

ggplot(dataf) + ggtitle("Classificação da nota de redação utilizando KNN com dados de treinamento") + xlab("Nota em Humanidades") + ylab("Nota em Exatas") + xlim(0, 1000) + ylim(0, 1000) +
  geom_point(aes(x=x, y=y, col=grupo), size=2.5, data=points, alpha=0.3) +
  geom_contour(aes(x=x, y=y, z=prob_grupo, group=grupo, color=grupo), bins=2, data=dataf) +
  geom_point(aes(x=x, y=y, col=grupo), data=mutate(test, grupo=classif), size=0.9)
ggsave("plots/11-grid knn com contorno e treinamento.png")
