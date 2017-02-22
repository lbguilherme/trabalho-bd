dat = read.csv("ENEM_LEVE.csv", header = TRUE)

# Remove candidatos que faltaram a prova de redação
dat = subset(dat, IN_STATUS_REDACAO != "F")

# Remove dois outliers: um candidato com 7 anos de idade outro com 115 anos de idade
dat = subset(dat, IDADE>=8 & IDADE<=114)

# Marca se fez a prova no mesmo estado em que se inscreveu
dat$UF_INSC_IGUAL_PROVA <- ifelse(dat$UF_INSC==dat$UF_MUNICIPIO_PROVA, 1, 0)

# Normaliza UF_INSC em colunas de boleanos
mm <- model.matrix(~UF_INSC-1, dat)
colnames(mm) <- gsub("UF_INSC","UF_INSC_",colnames(mm))
dat <- cbind(dat, mm)
dat <- subset(dat, select=-c(UF_INSC))

# Remove informações adicionais de localização
dat <- subset(dat, select=-c(COD_MUNICIPIO_PROVA, NO_MUNICIPIO_PROVA, UF_MUNICIPIO_PROVA))
dat <- subset(dat, select=-c(COD_MUNICIPIO_INSC, NO_MUNICIPIO_INSC))

# Normaliza TP_SEXO em colunas de boleanos
dat$GENERO_HOMEM = ifelse(dat$TP_SEXO==0, 1, 0)
dat$GENERO_MULHER = ifelse(dat$TP_SEXO==1, 1, 0)
dat <- subset(dat, select=-c(TP_SEXO))

# Normaliza notas em valores numéricos 0..1
dat$NOTA_RED <- ifelse(dat$NU_NOTA_REDACAO==".", 0, as.numeric(as.character(dat$NU_NOTA_REDACAO))/1000)
dat$NOTA_CN <- ifelse(dat$NU_NT_CN==".", 0, as.numeric(as.character(dat$NU_NT_CN))/1000)
dat$NOTA_CH <- ifelse(dat$NU_NT_CH==".", 0, as.numeric(as.character(dat$NU_NT_CH))/1000)
dat$NOTA_LC <- ifelse(dat$NU_NT_LC==".", 0, as.numeric(as.character(dat$NU_NT_LC))/1000)
dat$NOTA_MT <- ifelse(dat$NU_NT_MT==".", 0, as.numeric(as.character(dat$NU_NT_MT))/1000)
dat <- subset(dat, select=-c(NU_NOTA_REDACAO, NU_NT_CN, NU_NT_CH, NU_NT_LC, NU_NT_MT))

# Remove gabaritos e ID das provas
dat <- subset(dat, select=-c(DS_GABARITO_CN, DS_GABARITO_CH, DS_GABARITO_LC, DS_GABARITO_MT))
dat <- subset(dat, select=-c(TX_RESPOSTAS_CN, TX_RESPOSTAS_CH, TX_RESPOSTAS_LC, TX_RESPOSTAS_MT))
dat <- subset(dat, select=-c(ID_PROVA_CN, ID_PROVA_CH, ID_PROVA_LC, ID_PROVA_MT))
dat <- subset(dat, select=-c(IN_PRESENCA_CN, IN_PRESENCA_CH, IN_PRESENCA_LC, IN_PRESENCA_MT))
dat <- subset(dat, select=-c(NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4, NU_NOTA_COMP5))
dat <- subset(dat, select=-c(IN_STATUS_REDACAO))

# Remove opção de idioma
dat <- subset(dat, select=-c(TP_LINGUA))

# Remove número de inscrição e ano de realização da prova
dat <- subset(dat, select=-c(X, NU_INSCRICAO, NU_ANO))

# Normaliza TP_ESCOLA em colunas de booleanos
dat$ESCOLA_PUBLICA = ifelse(dat$TP_ESCOLA==1, 1, 0)
dat$ESCOLA_PRIVADA = ifelse(dat$TP_ESCOLA==2, 1, 0)
dat <- subset(dat, select=-c(TP_ESCOLA))

# Normaliza TP_LOCALIZACAO em colunas de booleanos
dat$LOCALIZACAO_URBANA = ifelse(dat$ID_LOCALIZACAO==1, 1, 0)
dat$LOCALIZACAO_RURAL = ifelse(dat$ID_LOCALIZACAO==2, 1, 0)
dat <- subset(dat, select=-c(ID_LOCALIZACAO))

# Remove demais informações sobre a escola e sobre a entidade certificadora
dat <- subset(dat, select=-c(NO_ENTIDADE_CERTIFICACAO, UF_ENTIDADE_CERTIFICACAO))
dat <- subset(dat, select=-c(PK_COD_ENTIDADE, COD_MUNICIPIO_ESC, NO_MUNICIPIO_ESC, UF_ESC))
dat <- subset(dat, select=-c(ID_DEPENDENCIA_ADM, SIT_FUNC, IN_TP_ENSINO))

# Normaliza idade: 0..100 => 0..1
dat$IDADE <- dat$IDADE/100

# Normaliza TP_COR_RACA em colunas de boleanos
dat$COR_RACA_BRANCA = ifelse(dat$TP_COR_RACA==1, 1, 0)
dat$COR_RACA_PRETA = ifelse(dat$TP_COR_RACA==2, 1, 0)
dat$COR_RACA_PARDA = ifelse(dat$TP_COR_RACA==3, 1, 0)
dat$COR_RACA_AMARELA = ifelse(dat$TP_COR_RACA==4, 1, 0)
dat$COR_RACA_INDIGENA = ifelse(dat$TP_COR_RACA==5, 1, 0)
dat <- subset(dat, select=-c(TP_COR_RACA))

# Normaliza o estado civil
dat$ESTADO_CIVIL_SOLTEIRO = ifelse(dat$TP_ESTADO_CIVIL==0, 1, 0)
dat$ESTADO_CIVIL_CASADO = ifelse(dat$TP_ESTADO_CIVIL==1, 1, 0)
dat$ESTADO_CIVIL_DIVORCIADO = ifelse(dat$TP_ESTADO_CIVIL==2, 1, 0)
dat$ESTADO_CIVIL_VIUVO = ifelse(dat$TP_ESTADO_CIVIL==3, 1, 0)
dat <- subset(dat, select=-c(TP_ESTADO_CIVIL))

# Normaliza ST_CONCLUSAO em colunas de boleanos
dat$ENSINO_MEDIO_CONCLUIDO = ifelse(dat$ST_CONCLUSAO==1, 1, 0)
dat$ENSINO_MEDIO_EM_2012 = ifelse(dat$ST_CONCLUSAO==2, 1, 0)
dat$ENSINO_MEDIO_DEPOIS_2012 = ifelse(dat$ST_CONCLUSAO==3, 1, 0)
dat$ENSINO_MEDIO_NAO_FAZ = ifelse(dat$ST_CONCLUSAO==4, 1, 0)
dat <- subset(dat, select=-c(ST_CONCLUSAO, ANO_CONCLUIU))

# Transformar nota de redação em 4 grupos nomeados
q <- quantile(dat$NOTA_RED)
dat$NOTA_RED_LABEL = ifelse(
  dat$NOTA_RED < q[2],
  "ruim",
  ifelse(
    dat$NOTA_RED < q[3],
    "regular",
    ifelse(
      dat$NOTA_RED < q[4],
      "bom",
      "otimo"
    )
  )
)
dat <- subset(dat, select=-c(NOTA_RED))

# Escreve arquivo final com os dados prontos para classificação
write.csv(dat, file="ENEM_LEVE_READY.csv")
