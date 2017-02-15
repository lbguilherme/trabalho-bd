#dat = read.csv("/home/john/Temp/ENEM_LEVE.csv", header = TRUE)

# Remove candidatos que faltaram a prova de redação
filtered_dat1 = subset(dat, IN_STATUS_REDACAO != "F")
cat("Candidados removidos por faltar a prova de redação:", nrow(dat)-nrow(filtered_dat1), "dentre", nrow(dat), "\n")
# Corrige as notas de redação: se um aluno não faltou e mesmo assim está sem nota, é porque ele tirou zero
filtered_dat1$NU_NOTA_REDACAO[filtered_dat1$NU_NOTA_REDACAO=="."]=0
# Remove dois outliers: um candidato com 7 anos de idade outro com 115 anos de idade
filtered_dat2 = subset(filtered_dat1, IDADE>=8 & IDADE<=114)
cat("Outliers removidos de acordo com a idade:", nrow(filtered_dat1)-nrow(filtered_dat2), "\n")
cat("\n")

# Lista final de candidatos
filtered_dat = filtered_dat2

# Estatísticas básicas
cat("Idade mínima:", min(filtered_dat$IDADE), "\n")
cat("Idade máxima:", max(filtered_dat$IDADE), "\n")
cat("Total de candidatos:", nrow(filtered_dat), "\n")
cat("Candidatos com até 20 anos:", nrow(subset(filtered_dat, IDADE <= 20)), "\n")
cat("Candidatos com até 25 anos:", nrow(subset(filtered_dat, IDADE <= 25)), "\n")
cat("Candidatos com nota abaixo de 200:", nrow(subset(filtered_dat, as.numeric(as.character(NU_NOTA_REDACAO)) < 200)), "\n")
cat("Candidatos com nota acima de 800:", nrow(subset(filtered_dat, as.numeric(as.character(NU_NOTA_REDACAO)) > 800)), "\n")
cat("Candidatos do sexo masculino:", nrow(subset(filtered_dat, TP_SEXO == 0)), "\n")
cat("Candidatos do sexo feminino:", nrow(subset(filtered_dat, TP_SEXO == 1)), "\n")
cat("\n")

# Adiciona a coluna "NEW_REGIAO" à base
get_region <- function(entry) {
    return(substr(as.character(entry$COD_MUNICIPIO_INSC), 0, 1))
}
filtered_dat$NEW_REGIAO = "???"
filtered_dat$NEW_REGIAO[get_region(filtered_dat)=="1"] = "Norte"
filtered_dat$NEW_REGIAO[get_region(filtered_dat)=="2"] = "Nordeste"
filtered_dat$NEW_REGIAO[get_region(filtered_dat)=="3"] = "Sudeste"
filtered_dat$NEW_REGIAO[get_region(filtered_dat)=="4"] = "Sul"
filtered_dat$NEW_REGIAO[get_region(filtered_dat)=="5"] = "Centro-Oeste"
stopifnot(nrow(subset(filtered_dat, NEW_REGIAO=="???"))==0)

# Adiciona a coluna "NEW_SEXO" à base (textual)
filtered_dat$NEW_SEXO = "???"
filtered_dat$NEW_SEXO[filtered_dat$TP_SEXO==0] = "Masculino"
filtered_dat$NEW_SEXO[filtered_dat$TP_SEXO==1] = "Feminino"
stopifnot(nrow(subset(filtered_dat, NEW_SEXO=="???"))==0)

# Adiciona a coluna "NEW_ESCOLA" à base (textual)
filtered_dat$NEW_ESCOLA = "???"
filtered_dat$NEW_ESCOLA[filtered_dat$TP_ESCOLA==1] = "Pública"
filtered_dat$NEW_ESCOLA[filtered_dat$TP_ESCOLA==2] = "Privada"
filtered_dat$NEW_ESCOLA[filtered_dat$TP_ESCOLA=="."] = "Não informado"

# Adiciona a coluna "NEW_LOCALIZACAO" à base (textual)
filtered_dat$NEW_LOCALIZACAO = "???"
filtered_dat$NEW_LOCALIZACAO[filtered_dat$ID_LOCALIZACAO==1] = "Urbana"
filtered_dat$NEW_LOCALIZACAO[filtered_dat$ID_LOCALIZACAO==2] = "Rural"
filtered_dat$NEW_LOCALIZACAO[filtered_dat$ID_LOCALIZACAO=="."] = "Não informado"

# Histograma de algumas variáveis isoladas
png(filename="geral_candidatos-por-idade.png")
attach(filtered_dat)
hist(IDADE, xlab="Idade", ylab="Candidatos", breaks=40, main="")
dev.off()

png(filename="geral_candidatos-por-sexo.png")
barplot(table(filtered_dat$TP_SEXO), names.arg=c("Masculino", "Feminino"), xlab="Sexo", ylab="Candidatos")
dev.off()

png(filename="geral_candidatos-por-escola.png")
barplot(table(filtered_dat$NEW_ESCOLA), xlab="Tipo de escola", ylab="Candidatos")
dev.off()

png(filename="geral_candidatos-por-localizacao.png")
barplot(table(filtered_dat$NEW_LOCALIZACAO), xlab="Localização", ylab="Candidatos")
dev.off()

png(filename="geral_candidatos-por-regiao.png")
barplot(table(filtered_dat$NEW_REGIAO), xlab="Região", ylab="Candidatos")
dev.off()

png(filename="geral_candidatos-por-nota.png")
attach(filtered_dat)
hist(as.numeric(as.character(NU_NOTA_REDACAO)), xlab="Idade", ylab="Candidatos", breaks=40, main="")
dev.off()

# Sumário por região
display_summary <- function(region_name, column_name, vec) {
    cat("***", column_name, "\n")
    cat("average: ", mean(vec), "\n")
    cat("median: ", median(vec), "\n")
    cat("stddev: ", sd(vec), "\n")
}

display_region_summary <- function(region_name, vec) {
    cat(">>>>>>>>>>>>>>>>>>>>", region_name, "\n")
    cat("*** Candidatos:", nrow(vec), "\n")
    display_summary(region_name, "Idade", vec$IDADE)
    display_summary(region_name, "Nota de redação", as.numeric(as.character(vec$NU_NOTA_REDACAO)))
}

filter_by_region <- function(vec, digit) {
    return(subset(vec, substr(as.character(COD_MUNICIPIO_INSC), 0, 1) == digit))
}

display_region_summary("Brasil", filtered_dat)
display_region_summary("Norte", filter_by_region(filtered_dat, "1"))
display_region_summary("Nordeste", filter_by_region(filtered_dat, "2"))
display_region_summary("Sudeste", filter_by_region(filtered_dat, "3"))
display_region_summary("Sul", filter_by_region(filtered_dat, "4"))
display_region_summary("Centro-Oeste", filter_by_region(filtered_dat, "5"))
erro

# Boxplots por região
png(filename="regiao_idade.png")
attach(filtered_dat)
boxplot(IDADE~NEW_REGIAO, xlab="Região", ylab="Idade")
dev.off()

png(filename="regiao_nota.png")
boxplot(as.numeric(as.character(NU_NOTA_REDACAO))~NEW_REGIAO, xlab="Região", ylab="Nota de redação")
attach(filtered_dat)
dev.off()

# Correlação entre nota de redação e outras variáveis
png(filename="correlacao_idade.png")
attach(filtered_dat)
plot(IDADE, as.numeric(as.character(NU_NOTA_REDACAO)), xlab="Idade", ylab="Nota de redação")
dev.off()

png(filename="correlacao_sexo.png")
attach(filtered_dat)
boxplot(as.numeric(as.character(NU_NOTA_REDACAO))~NEW_SEXO, xlab="Sexo", ylab="Nota de redação")
dev.off()

png(filename="correlacao_escola.png")
attach(filtered_dat)
boxplot(as.numeric(as.character(NU_NOTA_REDACAO))~NEW_ESCOLA, xlab="Tipo de escola", ylab="Nota de redação")
dev.off()

png(filename="correlacao_localizacao.png")
attach(filtered_dat)
boxplot(as.numeric(as.character(NU_NOTA_REDACAO))~NEW_LOCALIZACAO, xlab="Localização", ylab="Nota de redação")
dev.off()

png(filename="correlacao_ano_concluiu.png")
attach(filtered_dat)
plot(ANO_CONCLUIU, as.numeric(as.character(NU_NOTA_REDACAO)), xlab="Ano de conclusão do Ensino Médio", ylab="Nota de redação")
dev.off()

png(filename="correlacao_nota_ch.png")
attach(filtered_dat)
plot(as.numeric(as.character(NU_NT_CH)), as.numeric(as.character(NU_NOTA_REDACAO)), xlab="Nota em Ciências Humanas", ylab="Nota de redação")
dev.off()

png(filename="correlacao_nota_cn.png")
attach(filtered_dat)
plot(as.numeric(as.character(NU_NT_CN)), as.numeric(as.character(NU_NOTA_REDACAO)), xlab="Nota em Ciências da Natureza", ylab="Nota de redação")
dev.off()

png(filename="correlacao_nota_lc.png")
attach(filtered_dat)
plot(as.numeric(as.character(NU_NT_LC)), as.numeric(as.character(NU_NOTA_REDACAO)), xlab="Nota em Linguagens e Códigos", ylab="Nota de redação")
dev.off()

png(filename="correlacao_nota_mt.png")
attach(filtered_dat)
plot(as.numeric(as.character(NU_NT_MT)), as.numeric(as.character(NU_NOTA_REDACAO)), xlab="Nota em Matemática", ylab="Nota de redação")
dev.off()

png(filename="correlacao_deficiencia_mental.png")
attach(filtered_dat)
boxplot(as.numeric(as.character(NU_NOTA_REDACAO))~IN_DEFICIENCIA_MENTAL, xlab="Deficiência mental", ylab="Nota de redação")
dev.off()

png(filename="correlacao_deficiencia_fisica.png")
attach(filtered_dat)
boxplot(as.numeric(as.character(NU_NOTA_REDACAO))~IN_DEFICIENCIA_FISICA, xlab="Deficiência física", ylab="Nota de redação")
dev.off()

cat("Coeficiente de correlação entre idade e nota de redação:", cor(filtered_dat$IDADE, as.numeric(as.character(filtered_dat$NU_NOTA_REDACAO)), method="pearson"), "\n")
cat("Coeficiente de correlação entre nota em Ciências Humanas e nota de redação:", cor(as.numeric(as.character(filtered_dat$NU_NT_CH)), as.numeric(as.character(filtered_dat$NU_NOTA_REDACAO)), use="complete", method="pearson"), "\n")
cat("Coeficiente de correlação entre nota em Ciências da Natureza e nota de redação:", cor(as.numeric(as.character(filtered_dat$NU_NT_CN)), as.numeric(as.character(filtered_dat$NU_NOTA_REDACAO)), use="complete", method="pearson"), "\n")
cat("Coeficiente de correlação entre nota em Linguagens e Códigos e nota de redação:", cor(as.numeric(as.character(filtered_dat$NU_NT_LC)), as.numeric(as.character(filtered_dat$NU_NOTA_REDACAO)), use="complete", method="pearson"), "\n")
cat("Coeficiente de correlação entre nota em Matemática e nota de redação:", cor(as.numeric(as.character(filtered_dat$NU_NT_MT)), as.numeric(as.character(filtered_dat$NU_NOTA_REDACAO)), use="complete"), "\n", method="pearson")

# source("script.r")
