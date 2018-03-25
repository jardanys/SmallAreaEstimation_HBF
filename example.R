rm(list = ls())
library(survey)
library(sae)
library(TeachingSampling)
library(dplyr)
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


# primera etapa NII / nII
# segunda es NI / nI
# tercera es N_i / n_i
# La multiplicación son los factores de expasión

#*******************
# INFO AUXILIAR
#*******************

# Variables para X1: 

x1 <- c("LECTURA_CRITICA_PUNT", "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT", 
       "CIENCIAS_NATURALES_PUNT", "INGLES_PUNT")

x1 <- c("LECTURA_CRITICA_PUNT", "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT", 
        "CIENCIAS_NATURALES_PUNT", "INGLES_PUNT")

x3 <- c("CALENDARIO", "NATURALEZA", "JORNADA", "EVALUADOS")

x4 <- c("PERS_GENERO", "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", 
        "FINS_PISOSHOGAR", "FINS_TIENEINTERNET", "FINS_TIENECOMPUTADOR", 
        "FINS_TIENEAUTOMOVILPARTICULAR", "INGLES_DESEM")


# Variables para x2:
unique(est$FINS_ESTRATOVIVIENDAENERGIA) # Categorica 
est$FINS_ESTRATOVIVIENDAENERGIA[is.na(est$FINS_ESTRATOVIVIENDAENERGIA)] <- 1

# Variables para x3:
unique(est$CALENDARIO) # Categorica
unique(est$NATURALEZA) # Categorica
unique(est$JORNADA) # Categorica
summary(est$EVALUADOS) # Numerica

x3 <- c("CALENDARIO", "NATURALEZA", "JORNADA", "EVALUADOS")

# Variables para x4:
unique(est$PERS_GENERO) # Categorica
table(est$PERS_GENERO)
est$PERS_GENERO[is.na(est$PERS_GENERO)] <- names(table(est$PERS_GENERO)[which.max(table(est$PERS_GENERO))])

summary(est$FINS_PERSONASHOGARACTUAL) # Numerica
est$FINS_PERSONASHOGARACTUAL[is.na(est$FINS_PERSONASHOGARACTUAL)] <- 5

summary(est$FINS_CUARTOSHOGARACTUAL) # Numerica
est$FINS_CUARTOSHOGARACTUAL[is.na(est$FINS_CUARTOSHOGARACTUAL)] <- 3

unique(est$FINS_PISOSHOGAR) # Categorica
est$FINS_PISOSHOGAR[is.na(est$FINS_PISOSHOGAR)] <- names(table(est$FINS_PISOSHOGAR
                                                               )[which.max(table(est$FINS_PISOSHOGAR))])

unique(est$FINS_TIENEINTERNET) # Categorica
est$FINS_TIENEINTERNET[is.na(est$FINS_TIENEINTERNET)] <- names(table(est$FINS_TIENEINTERNET
                                                               )[which.max(table(est$FINS_TIENEINTERNET))])

unique(est$FINS_TIENECOMPUTADOR) # Categorica
est$FINS_TIENECOMPUTADOR[is.na(est$FINS_TIENECOMPUTADOR)] <- names(table(est$FINS_TIENECOMPUTADOR
                                                                     )[which.max(table(est$FINS_TIENECOMPUTADOR))])

unique(est$FINS_TIENEAUTOMOVILPARTICULAR) # Categorica
est$FINS_TIENEAUTOMOVILPARTICULAR[is.na(est$FINS_TIENEAUTOMOVILPARTICULAR)] <- names(table(est$FINS_TIENEAUTOMOVILPARTICULAR
                                                                         )[which.max(table(est$FINS_TIENEAUTOMOVILPARTICULAR))])


unique(est$INGLES_DESEM) # Categorica

x4 <- c("PERS_GENERO", "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", 
        "FINS_PISOSHOGAR", "FINS_TIENEINTERNET", "FINS_TIENECOMPUTADOR", 
        "FINS_TIENEAUTOMOVILPARTICULAR","INGLES_DESEM")

#****************
# Desarrollo 
#****************

# x1 todas numericas

# x2: Estrato Energia
est$FINS_ESTRATOVIVIENDAENERGIA <- ifelse(est$FINS_ESTRATOVIVIENDAENERGIA==1, "Estrato1",
                                          ifelse(est$FINS_ESTRATOVIVIENDAENERGIA==2, "Estrato2",
                                                 ifelse(est$FINS_ESTRATOVIVIENDAENERGIA==3, "Estrato3",
                                                        ifelse(est$FINS_ESTRATOVIVIENDAENERGIA==4, "Estrato4",
                                                               ifelse(est$FINS_ESTRATOVIVIENDAENERGIA==5, "Estrato5",
                                                                      ifelse(est$FINS_ESTRATOVIVIENDAENERGIA==6, "Estrato6","SinEstrato"))))))
muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA <- ifelse(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA==1, "Estrato1",
                                          ifelse(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA==2, "Estrato2",
                                                 ifelse(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA==3, "Estrato3",
                                                        ifelse(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA==4, "Estrato4",
                                                               ifelse(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA==5, "Estrato5",
                                                                      ifelse(muestra3etapas$FINS_ESTRATOVIVIENDAENERGIA==6, "Estrato6","SinEstrato"))))))
Dummies_estrato_energia  <- as.data.frame(Domains(est$FINS_ESTRATOVIVIENDAENERGIA))
est <- cbind(est,Dummies_estrato_energia)
head(est)
# x3: Variable colegio: Naturaleza

  # si es Evaluados pasa

  # si no
x3 <- c("CALENDARIO", "NATURALEZA", "JORNADA", "EVALUADOS")

    # Si Calendario
unique(est$CALENDARIO)
est$CALENDARIO <- ifelse(est$CALENDARIO=="Calendario A", "Calendario_A",
                         ifelse(est$CALENDARIO=="Calendario B", "Calendario_B",
                                ifelse(est$CALENDARIO=="Calendario flexible", "Calendario_flexible","Sin_Calendario")))
muestra3etapas$CALENDARIO <- ifelse(muestra3etapas$CALENDARIO=="Calendario A", "Calendario_A",
                         ifelse(muestra3etapas$CALENDARIO=="Calendario B", "Calendario_B",
                                ifelse(muestra3etapas$CALENDARIO=="Calendario flexible", "Calendario_flexible","Sin_Calendario")))
Dummies_calendario  <- as.data.frame(Domains(est$CALENDARIO))
est <- cbind(est,Dummies_calendario)
head(est)
    # si Naturaleza
unique(est$NATURALEZA)
est$NATURALEZA <- ifelse(est$NATURALEZA=="No oficial", "Naturaleza_No_Oficial", "Naturaleza_Oficial")
muestra3etapas$NATURALEZA <- ifelse(muestra3etapas$NATURALEZA=="No oficial", "Naturaleza_No_Oficial", "Naturaleza_Oficial")
Dummies_Naturaleza  <- as.data.frame(Domains(est$NATURALEZA))
est <- cbind(est,Dummies_Naturaleza)

    # si Jornada
unique(est$JORNADA)
table(est$JORNADA)
est$JORNADA <- ifelse(est$JORNADA=="Completa u ordinaria", "Jornada_Completa", 
                      ifelse(est$JORNADA=="Noche", "Jornada_Noche",
                             ifelse(est$JORNADA=="Tarde", "Jornada_Tarde", 
                                    ifelse(est$JORNADA=="Mañana", "Jornada_Manana", "Jornada_SabDom"))))
muestra3etapas$JORNADA <- ifelse(muestra3etapas$JORNADA=="Completa u ordinaria", "Jornada_Completa", 
                      ifelse(muestra3etapas$JORNADA=="Noche", "Jornada_Noche",
                             ifelse(muestra3etapas$JORNADA=="Tarde", "Jornada_Tarde", 
                                    ifelse(muestra3etapas$JORNADA=="Mañana", "Jornada_Manana", "Jornada_SabDom"))))
Dummies_Jornada  <- as.data.frame(Domains(est$JORNADA))
est <- cbind(est,Dummies_Jornada)

# x4: Otras

x4 <- c("PERS_GENERO", "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", 
        "FINS_PISOSHOGAR", "FINS_TIENEINTERNET", "FINS_TIENECOMPUTADOR", 
        "FINS_TIENEAUTOMOVILPARTICULAR", "INGLES_DESEM")

  # si FINS_PERSONASHOGARACTUAL o FINS_CUARTOSHOGARACTUAL pasa derechi

  # si no

    # si PERS_GENERO
unique(est$PERS_GENERO)
est$PERS_GENERO <- ifelse(est$PERS_GENERO=="F", "Genero_F", "Genero_M")
muestra3etapas$PERS_GENERO <- ifelse(muestra3etapas$PERS_GENERO=="F", "Genero_F", "Genero_M")
Dummies_Pers_Genero  <- as.data.frame(Domains(est$PERS_GENERO))
est <- cbind(est,Dummies_Pers_Genero)

    # si FINS_PISOSHOGAR
unique(est$FINS_PISOSHOGAR)
est$FINS_PISOSHOGAR <- ifelse(est$FINS_PISOSHOGAR=="1", "Pisos_Tierra", 
                              ifelse(est$FINS_PISOSHOGAR=="2", "Pisos_Cemento",
                                     ifelse(est$FINS_PISOSHOGAR=="3", "Pisos_Madera", "Pisos_Baldosa_Marmol")))
muestra3etapas$FINS_PISOSHOGAR <- ifelse(muestra3etapas$FINS_PISOSHOGAR=="1", "Pisos_Tierra", 
                              ifelse(muestra3etapas$FINS_PISOSHOGAR=="2", "Pisos_Cemento",
                                     ifelse(muestra3etapas$FINS_PISOSHOGAR=="3", "Pisos_Madera", "Pisos_Baldosa_Marmol")))
Dummies_Pisos  <- as.data.frame(Domains(est$FINS_PISOSHOGAR))
est <- cbind(est,Dummies_Pisos)

# si FINS_TIENEINTERNET
unique(est$FINS_TIENEINTERNET)
est$FINS_TIENEINTERNET <- ifelse(est$FINS_TIENEINTERNET=="S", "Internet_Si", "Internet_No")
muestra3etapas$FINS_TIENEINTERNET <- ifelse(muestra3etapas$FINS_TIENEINTERNET=="S", "Internet_Si", "Internet_No")
Dummies_Fins_Internet  <- as.data.frame(Domains(est$FINS_TIENEINTERNET))
est <- cbind(est,Dummies_Fins_Internet)

    # si FINS_TIENECOMPUTADOR
unique(est$FINS_TIENECOMPUTADOR)
est$FINS_TIENECOMPUTADOR <- ifelse(est$FINS_TIENECOMPUTADOR=="S", "Computador_Si", "Computador_No")
muestra3etapas$FINS_TIENECOMPUTADOR <- ifelse(muestra3etapas$FINS_TIENECOMPUTADOR=="S", "Computador_Si", "Computador_No")
Dummies_Computador  <- as.data.frame(Domains(est$FINS_TIENECOMPUTADOR))
est <- cbind(est,Dummies_Computador)

    # si FINS_TIENEAUTOMOVILPARTICULAR
unique(est$FINS_TIENEAUTOMOVILPARTICULAR)
est$FINS_TIENEAUTOMOVILPARTICULAR <- ifelse(est$FINS_TIENEAUTOMOVILPARTICULAR=="S", "Auto_Si", "Auto_No")
muestra3etapas$FINS_TIENEAUTOMOVILPARTICULAR <- ifelse(muestra3etapas$FINS_TIENEAUTOMOVILPARTICULAR=="S", "Auto_Si", "Auto_No")
Dummies_movil  <- as.data.frame(Domains(est$FINS_TIENEAUTOMOVILPARTICULAR))
est <- cbind(est,Dummies_movil)

# si INGLES_DESEM
unique(est$INGLES_DESEM)
est$INGLES_DESEM <- ifelse(est$INGLES_DESEM=="A2", "Ingles_A2",
                           ifelse(est$INGLES_DESEM=="A1", "Ingles_A1",
                                  ifelse(est$INGLES_DESEM=="B+", "Ingles_B_mas",
                                         ifelse(est$INGLES_DESEM=="B1", "Ingles_B1", "Ingles_A_menos"))))
muestra3etapas$INGLES_DESEM <- ifelse(muestra3etapas$INGLES_DESEM=="A2", "Ingles_A2",
                           ifelse(muestra3etapas$INGLES_DESEM=="A1", "Ingles_A1",
                                  ifelse(muestra3etapas$INGLES_DESEM=="B+", "Ingles_B_mas",
                                         ifelse(muestra3etapas$INGLES_DESEM=="B1", "Ingles_B1", "Ingles_A_menos"))))
Dummies_ingles  <- as.data.frame(Domains(est$INGLES_DESEM))
est <- cbind(est,Dummies_ingles)

names(est)

x1 <- c("LECTURA_CRITICA_PUNT", "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT", 
        "CIENCIAS_NATURALES_PUNT", "INGLES_PUNT")

x3 <- c("CALENDARIO", "NATURALEZA", "JORNADA", "EVALUADOS")

x4 <- c("PERS_GENERO", "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", 
        "FINS_PISOSHOGAR", "FINS_TIENEINTERNET", "FINS_TIENECOMPUTADOR", 
        "FINS_TIENEAUTOMOVILPARTICULAR", "INGLES_DESEM")

Infoaux <- est %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Prom_LECTURA_CRITICA_PUNT = mean(LECTURA_CRITICA_PUNT),
            Prom_MATEMATICAS_PUNT = mean(MATEMATICAS_PUNT),
            Prom_SOCIALES_CIUDADANAS_PUNT = mean(SOCIALES_CIUDADANAS_PUNT),
            Prom_CIENCIAS_NATURALES_PUNT = mean(CIENCIAS_NATURALES_PUNT),
            Prom_INGLES_PUNT = mean(INGLES_PUNT),
            Prop_Estrato1 = mean(Estrato1),
            Prop_Estrato2 = mean(Estrato2),
            Prop_Estrato3 = mean(Estrato3),
            Prop_Estrato4 = mean(Estrato4),
            Prop_Estrato5 = mean(Estrato5),
            Prop_Estrato6 = mean(Estrato6),
            Prop_Calendario_A = mean(Calendario_A),
            Prop_Calendario_B = mean(Calendario_B),
            Prop_Calendario_Flexible = mean(Calendario_flexible),
            Prop_Naturaleza_Oficial = mean(Naturaleza_Oficial),
            Prop_Naturaleza_No_Oficial = mean(Naturaleza_No_Oficial),
            Prop_Jornada_Completa = mean(Jornada_Completa),
            Prop_Jornada_Manana = mean(Jornada_Manana),
            Prop_Jornada_Tarde = mean(Jornada_Tarde),
            Prop_Jornada_Noche = mean(Jornada_Noche),
            Prop_Jornada_SabatinaDom = mean(Jornada_SabDom),
            Prom_Evaluados = mean(EVALUADOS),
            Prop_Genero_F = mean(Genero_F),
            Prop_Genero_M = mean(Genero_M),
            Prom_Pers_Hogar = mean(FINS_PERSONASHOGARACTUAL),
            Prom_Cuartos_Hogar = mean(FINS_CUARTOSHOGARACTUAL),
            Prop_Pisos_Tierra = mean(Pisos_Tierra),
            Prop_Pisos_Cemento = mean(Pisos_Cemento),
            Prop_Pisos_Madera = mean(Pisos_Madera),
            Prop_Pisos_Baldosa_Marmol = mean(Pisos_Baldosa_Marmol),
            Prop_Internet_Si = mean(Internet_No),
            Prop_Internet_No = mean(Internet_Si),
            Prop_Computador_Si = mean(Computador_No),
            Prop_Computador_No = mean(Computador_Si),
            Prop_Auto_Si = mean(Auto_No),
            Prop_Auto_No = mean(Auto_Si),
            Prop_Ingles_A_ = mean(Ingles_A_menos),
            Prop_Ingles_A1 = mean(Ingles_A1),
            Prop_Ingles_A2 = mean(Ingles_A2),
            Prop_Ingles_B = mean(Ingles_B_mas),
            Prop_Ingles_A1 = mean(Ingles_B1),
            N_d = n() )

x1 <- c("LECTURA_CRITICA_PUNT", "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT", 
        "CIENCIAS_NATURALES_PUNT", "INGLES_PUNT")

x3 <- c("CALENDARIO", "NATURALEZA", "JORNADA", "EVALUADOS")

x4 <- c("PERS_GENERO", "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", 
        "FINS_PISOSHOGAR", "FINS_TIENEINTERNET", "FINS_TIENECOMPUTADOR", 
        "FINS_TIENEAUTOMOVILPARTICULAR", "INGLES_DESEM")

names(est)
names(Infoaux)

x1 <- "LECTURA_CRITICA_PUNT"
x_1 <- paste0("Prom_",x1)
x2 <- "FINS_ESTRATOVIVIENDAENERGIA"
x_2 <- c("Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6")
x3 <- "CALENDARIO"
x_3 <- c("Prop_Naturaleza_Oficial")
x4 <- "PERS_GENERO"
x_4 <- c("Prom_Pers_Hogar")

ifelse(x3=="CALENDARIO", x_3<-c("Prop_Calendario_B", "Prop_Calendario_Flexible"),
       ifelse(x3=="NATURALEZA", x_3<-c("Prop_Naturaleza_Oficial"),
              ifelse(x3=="JORNADA", x_3<-c("Prop_Jornada_Completa", "Prop_Jornada_Manana",
                                           "Prop_Jornada_Tarde", "Prop_Jornada_Noche"),
                     ifelse(x3=="EVALUADOS", x_3<-c("Prom_Evaluados")))))
       

ifelse(x4=="PERS_GENERO", x_4<-c("Prop_Genero_F"),
       ifelse(x4=="FINS_PERSONASHOGARACTUAL", x_4<-c("Prom_Pers_Hogar"),
              ifelse(x4=="FINS_CUARTOSHOGARACTUAL", x_4<-c("Prom_Cuartos_Hogar"),
                     ifelse(x4=="FINS_PISOSHOGAR", x_4<-c("Prop_Pisos_Cemento", "Prop_Pisos_Madera","Prop_Pisos_Baldosa_Marmol"),
                            ifelse(x4=="FINS_TIENEINTERNET", x_4<-c("Prop_Internet_Si"),
                                   ifelse(x4=="FINS_TIENECOMPUTADOR", x_4<-c("Prop_Computador_Si"),
                                          ifelse(x4=="FINS_TIENEAUTOMOVILPARTICULAR", x_4<-c("Prop_Auto_Si"),
                                                 ifelse(x4=="INGLES_DESEM", x_4<-c("Prop_Ingles_A1", "Prop_Ingles_A2",
                                                                                   "Prop_Ingles_B")))))))))

x_1
x_2
x_3
x_4




set.seed(12345)


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
Medias <- Infoaux[,c("CODIGOMUNICIPIO", paste0("Prom_",x1),
                     "Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6", 
                     x_3, 
                     x_4)]

Tamanos$CODIGOMUNICIPIO <- as.character(Tamanos$CODIGOMUNICIPIO)
Medias$CODIGOMUNICIPIO <- as.character(Medias$CODIGOMUNICIPIO)
muestra3etapas$CODIGOMUNICIPIO <- as.character(muestra3etapas$CODIGOMUNICIPIO)

formula <- as.formula(paste0("MATEMATICAS_PUNT", "~", x1, "+", "FINS_ESTRATOVIVIENDAENERGIA", "+", x3, "+", x4))

BHF <- pbmseBHF(formula, 
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
x_1
x_2
x_3
x_4

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
modelo_mixto <- lme(formula, random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestra3etapas)


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

