rm(list = ls())
library(survey)
library(sae)
library(TeachingSampling)
library(dplyr)
data(BigLucy)
# Dominio: Zonas
# Y: Income
# Info Auxiliar: Income, Employees, Level
mue <- readRDS("muestra.rds")
table(mue$Zone); table(BigLucy$Zone)
table(table(mue$Zone)>0)

# INFO AUXILIAR
Dummies_Level  <- Domains(BigLucy$Level)
BigLucy <- cbind(BigLucy,Dummies_Level )
Infoaux <- BigLucy %>% group_by(Zone) %>% 
  summarise(Prom_Impue = mean(Taxes),
            Prom_Empleados = mean(Employees),
            Prop_Grandes = mean(Big),
            Prop_Medianas = mean(Medium),
            Prop_Pequenas = mean( Small),
            N_d = n() )



mue$Zone <- as.character(mue$Zone)
BigLucy$Zone <- as.character(BigLucy$Zone)
Infoaux$Zone <- as.character(Infoaux$Zone)

#mue$IndiceCondado <- as.numeric(gsub("County","",mue$Zone))
#BigLucy$IndiceCondado <- as.numeric(gsub("County","",BigLucy$Zone))
#Infoaux$IndiceCondado <- as.numeric(gsub("County","",Infoaux$Zone))


Tamanos <- Infoaux[,
                   c("Zone", "N_d")]
Medias <-Infoaux[,
                 c("Zone", "Prom_Impue","Prom_Empleados",
                   "Prop_Medianas", "Prop_Pequenas")]

set.seed(12345)
BHF <- pbmseBHF(Income ~ Taxes + Employees  + Level, 
                dom = Zone, 
                meanxpop = Medias,
                popnsize = Tamanos,
                B = 200, data = mue)

# Estimaci�n para dominios observados
BHF$est$eblup

# Estimaci�n del error cuadr�tico medio
BHF$mse

# CVE
sqrt(BHF$mse$mse) / BHF$est$eblup$eblup * 100


# Estimaciones para dominios externos (61 que no salieron en la muestra)
Beta_est <- BHF$est$fit$fixed
names(Beta_est) <- gsub("XsXs", "", names(Beta_est) )
names(Beta_est)[1] <-" Intercepto" 
Beta_est <- as.matrix(Beta_est)

# Totales por dominio
Xbar_d <- Infoaux[ c("Prom_Impue", "Prom_Empleados", "Prop_Medianas", "Prop_Pequenas")]
Unos <- as.data.frame(as.matrix(rep(1, nrow(Infoaux))))
Xbar_d <- cbind(Unos, Xbar_d)
Xbar_d <- as.matrix(Xbar_d)
rownames(Xbar_d) <- Infoaux$Zone

Prom_dominios <- Xbar_d %*% Beta_est
rownames(Prom_dominios) <- Infoaux$Zone 
Prom_dominios <- as.data.frame(Prom_dominios)
Prom_dominios$domain <- row.names(Prom_dominios)
colnames(Prom_dominios)[1] <- "Ybar_efectosfijos" 

# Conservar los dominios no observados 
Prom_dominios_observados <- BHF$est$eblup
Prom_dominios <- merge(Prom_dominios, Prom_dominios_observados, by = "domain", all.x = T)
names(Prom_dominios)[1] <- "Zone"
head(Prom_dominios)
############ Estimaci�n MSE para dominios no observados ###########
library(nlme)
modelo_mixto <- lme(Income ~  Taxes + Employees  + Level, random = ~1 | as.factor(Zone), data = mue)
#\hat{V}(\hat{\bodysymbol{\beta}}):
Varest_betaest <- vcov(modelo_mixto)

# \hat{sigma}^2_u
sigma2est_u <- BHF[[1]]$fit$refvar #29.66217^2 #EN modelo_mixto observese que es la misma estimacion

# Identificar los dominios no observados
dominios_noobservados <- unique(BigLucy$Zone)[!(unique(BigLucy$Zone) %in% unique(mue$Zone))]
Xbar_d_noobs <- Xbar_d[row.names(Xbar_d) %in% dominios_noobservados,]
MSE_DominiosNoobservados <- diag((Xbar_d_noobs %*% Varest_betaest %*% t(Xbar_d_noobs)) + sigma2est_u)
MSE_DominiosNoobservados <- as.table(MSE_DominiosNoobservados)
df_MSE_DominiosNoobservados <- as.data.frame(MSE_DominiosNoobservados)
names(df_MSE_DominiosNoobservados) <- c("Zone", "MSE")
df_MSE_DominiosNoobservados$ClaseDominio <- "No observado"

df_MSE_Dominiosobservados <- BHF$mse
names(df_MSE_Dominiosobservados) <- c("Zone", "MSE")
df_MSE_Dominiosobservados$ClaseDominio <- "Observado"


df_MSE_Dominios<- bind_rows(df_MSE_DominiosNoobservados, df_MSE_Dominiosobservados)
df_MSE_Dominios <- df_MSE_Dominios[order(df_MSE_Dominios$Zone),]

# Tienden a dar m�s MSE los dominios no obsevados
boxplot(MSE ~ ClaseDominio, data = df_MSE_Dominios)

# Resultados finales

Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "Zone")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF

mean(Resultados$cve)
