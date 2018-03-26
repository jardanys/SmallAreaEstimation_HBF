rm(list = ls())
library(survey)
library(sae)
library(TeachingSampling)
library(dplyr)
library(nlme)
options(survey.lonely.psu="adjust")

#**************************************
#******* MUESTRA PRUEBAS SABER ********
#**************************************


est <- readRDS("estudiantes.rds")
muestra3etapas <- readRDS("muestra3etapas.rds")

# Dominio Depto(Cod depto)
# y_est: Puntaje Matematicas
# x1: puntaje materias (una materia)
# x2: Estrato eneriga
# x3: Variable colegio (ej: jornada, ofc/priv, calendario)
# x4: ? la que se le pague la gana

# Transformar variables, quitar nulos

# Variables para x2:
est$FINS_ESTRATOVIVIENDAENERGIA[is.na(est$FINS_ESTRATOVIVIENDAENERGIA)] <- 1

# Variables para x4:
est$PERS_GENERO[is.na(est$PERS_GENERO)] <- names(table(est$PERS_GENERO)[which.max(table(est$PERS_GENERO))])
est$FINS_PERSONASHOGARACTUAL[is.na(est$FINS_PERSONASHOGARACTUAL)] <- 5
est$FINS_CUARTOSHOGARACTUAL[is.na(est$FINS_CUARTOSHOGARACTUAL)] <- 3
est$FINS_PISOSHOGAR[is.na(est$FINS_PISOSHOGAR)] <- names(table(est$FINS_PISOSHOGAR)[which.max(table(est$FINS_PISOSHOGAR))])
est$FINS_TIENEINTERNET[is.na(est$FINS_TIENEINTERNET)] <- names(table(est$FINS_TIENEINTERNET)[which.max(table(est$FINS_TIENEINTERNET))])
est$FINS_TIENECOMPUTADOR[is.na(est$FINS_TIENECOMPUTADOR)] <- names(table(est$FINS_TIENECOMPUTADOR)[which.max(table(est$FINS_TIENECOMPUTADOR))])
est$FINS_TIENEAUTOMOVILPARTICULAR[is.na(est$FINS_TIENEAUTOMOVILPARTICULAR)] <- names(table(est$FINS_TIENEAUTOMOVILPARTICULAR)[which.max(table(est$FINS_TIENEAUTOMOVILPARTICULAR))])

# Crear Dumies

Dummies_estrato_energia  <- as.data.frame(Domains(est$FINS_ESTRATOVIVIENDAENERGIA))
est <- cbind(est,Dummies_estrato_energia)

unique(est$CALENDARIO)
Dummies_calendario  <- as.data.frame(Domains(est$CALENDARIO))
est <- cbind(est,Dummies_calendario)

unique(est$PERS_GENERO)
Dummies_Pers_Genero  <- as.data.frame(Domains(est$PERS_GENERO))
est <- cbind(est,Dummies_Pers_Genero)

names(est)

Infoaux <- est %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Prom_LECTURA_CRITICA_PUNT = mean(LECTURA_CRITICA_PUNT),
            Prop_Estrato1 = mean(`1`),
            Prop_Estrato2 = mean(`2`),
            Prop_Estrato3 = mean(`3`),
            Prop_Estrato4 = mean(`4`),
            Prop_Estrato5 = mean(`5`),
            Prop_Estrato6 = mean(`6`),
            Prop_Calendario_A = mean(`Calendario A`),
            Prop_Calendario_B = mean(`Calendario B`),
            Prop_Calendario_Flexible = mean(`Calendario flexible`),
            Prop_Genero_F = mean(M),
            Prop_Genero_M = mean(`F`),
            N_d = n() )

# Convertir en factores las categoricas de la muestra

muestra3etapas$CALENDARIO <- as.factor(muestra3etapas$CALENDARIO)
muestra3etapas$NATURALEZA <- as.factor(muestra3etapas$NATURALEZA)
muestra3etapas$JORNADA <- as.factor(muestra3etapas$JORNADA)
muestra3etapas$PERS_GENERO <- as.factor(muestra3etapas$PERS_GENERO)
muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA <- as.factor(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA)
muestra3etapas$FINS_PISOSHOGAR <- as.factor(muestra3etapas$FINS_PISOSHOGAR)
muestra3etapas$FINS_TIENEINTERNET <- as.factor(muestra3etapas$FINS_TIENEINTERNET)
muestra3etapas$FINS_TIENECOMPUTADOR <- as.factor(muestra3etapas$FINS_TIENECOMPUTADOR)
muestra3etapas$FINS_TIENEAUTOMOVILPARTICULAR <- as.factor(muestra3etapas$FINS_TIENEAUTOMOVILPARTICULAR)
muestra3etapas$INGLES_DESEM <- as.factor(muestra3etapas$INGLES_DESEM)

Tamanos <- Infoaux[,c("CODIGOMUNICIPIO", "N_d")]
names(Infoaux)
Medias <- Infoaux[,c("CODIGOMUNICIPIO", "Prom_LECTURA_CRITICA_PUNT",
                     "Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6", 
                     "Prop_Calendario_B", "Prop_Calendario_Flexible", 
                     "Prop_Genero_F")]

# Convertir a caracter
Tamanos$CODIGOMUNICIPIO <- as.character(Tamanos$CODIGOMUNICIPIO)
Medias$CODIGOMUNICIPIO <- as.character(Medias$CODIGOMUNICIPIO)
muestra3etapas$CODIGOMUNICIPIO <- as.character(muestra3etapas$CODIGOMUNICIPIO)

# Modelo
BHF <- pbmseBHF(MATEMATICAS_PUNT ~ LECTURA_CRITICA_PUNT + FINS_ESTRATOVIVIENDAENERGIA + CALENDARIO + PERS_GENERO, 
                dom = CODIGOMUNICIPIO, 
                meanxpop = Medias,
                popnsize = Tamanos,
                B = 200, data = muestra3etapas)


# Estimaci�n para dominios observados
BHF$est$eblup

# Estimaci�n del error cuadr�tico medio
BHF$mse

# cv
sqrt(BHF$mse$mse) / BHF$est$eblup$eblup * 100


# Estimaciones para dominios externos (que no salieron en la muestra)
Beta_est <- BHF$est$fit$fixed
names(Beta_est) <- gsub("XsXs", "", names(Beta_est) )
names(Beta_est)[1] <-" Intercepto" 
Beta_est <- as.matrix(Beta_est)

# Totales por dominio
x_1 <- "Prom_LECTURA_CRITICA_PUNT"
x_2 <- c("Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6")
x_3 <- c("Prop_Calendario_B", "Prop_Calendario_Flexible")
x_4 <- "Prop_Genero_F"

Xbar_d <- Infoaux[ c(x_1, x_2, x_3, x_4)]
Unos <- as.data.frame(as.matrix(rep(1, nrow(Infoaux))))
Xbar_d <- cbind(Unos, Xbar_d)
Xbar_d <- as.matrix(Xbar_d)
rownames(Xbar_d) <- Infoaux$CODIGOMUNICIPIO
head(Xbar_d)

Prom_dominios <- Xbar_d %*% Beta_est
rownames(Prom_dominios) <- Infoaux$CODIGOMUNICIPIO
Prom_dominios <- as.data.frame(Prom_dominios)
Prom_dominios$domain <- row.names(Prom_dominios)
colnames(Prom_dominios)[1] <- "Ybar_efectosfijos" 
head(Prom_dominios)


# Conservar los dominios no observados 
Prom_dominios_observados <- BHF$est$eblup
Prom_dominios <- merge(Prom_dominios, Prom_dominios_observados, by = "domain", all.x = T)
names(Prom_dominios)[1] <- "MUNICIPIO"
head(Prom_dominios)
############ Estimaci�n MSE para dominios no observados ###########

library(nlme)
modelo_mixto <- lme(MATEMATICAS_PUNT ~ LECTURA_CRITICA_PUNT + FINS_ESTRATOVIVIENDAENERGIA + CALENDARIO + PERS_GENERO, random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestra3etapas)


#\hat{V}(\hat{\bodysymbol{\beta}}):
Varest_betaest <- vcov(modelo_mixto)

# \hat{sigma}^2_u
sigma2est_u <- BHF[[1]]$fit$refvar #29.66217^2 #EN modelo_mixto observese que es la misma estimacion

# Identificar los dominios no observados
dominios_noobservados <- unique(est$CODIGOMUNICIPIO)[!(unique(est$CODIGOMUNICIPIO) %in% unique(muestra3etapas$CODIGOMUNICIPIO))]
Xbar_d_noobs <- Xbar_d[row.names(Xbar_d) %in% dominios_noobservados,]
MSE_DominiosNoobservados <- diag((Xbar_d_noobs %*% Varest_betaest %*% t(Xbar_d_noobs)) + sigma2est_u)
MSE_DominiosNoobservados <- as.table(MSE_DominiosNoobservados)
df_MSE_DominiosNoobservados <- as.data.frame(MSE_DominiosNoobservados)
names(df_MSE_DominiosNoobservados) <- c("MUNICIPIO", "MSE")
df_MSE_DominiosNoobservados$ClaseDominio <- "No observado"

head(df_MSE_DominiosNoobservados)

df_MSE_Dominiosobservados <- BHF$mse
names(df_MSE_Dominiosobservados) <- c("MUNICIPIO", "MSE")
df_MSE_Dominiosobservados$ClaseDominio <- "Observado"

head(df_MSE_Dominiosobservados)

df_MSE_Dominios <- bind_rows(df_MSE_DominiosNoobservados, df_MSE_Dominiosobservados)
df_MSE_Dominios <- df_MSE_Dominios[order(df_MSE_Dominios$MUNICIPIO),]

# Tienden a dar m�s MSE los dominios no obsevados
boxplot(MSE ~ ClaseDominio, data = df_MSE_Dominios)

# Resultados finales

Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF

head(Resultados)

mean(Resultados$cve)


