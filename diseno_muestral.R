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




diseno_muestral <- svydesign(ids = ~CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = muestra3etapas,
                             nest = T)

svymean(~INGLES_PUNT, diseno_muestral)
100 * cv(svymean(~INGLES_PUNT, diseno_muestral))
mean(est$INGLES_PUNT)
svytotal(~EVALUADOS, diseno_muestral)
100 * cv(svytotal(~EVALUADOS, diseno_muestral))
sum(est$EVALUADOS)
sum(weights(diseno_muestral))
nrow(est)

# Dominio Depto(Cod depto)
# y_est: Puntaje Matematicas
# x1: puntaje materias (una materia)
# x2: Estrato eneriga
# x3: Variable colegio (ej: jornada, ofc/priv, calendario)
# x4: ? la que se le pague la gana

# Estimaciones por departamento para MATEMATICAS_PUNT

svymean(~MATEMATICAS_PUNT, diseno_muestral)
100 * cv(svymean(~MATEMATICAS_PUNT, diseno_muestral))
mean(est$MATEMATICAS_PUNT)

svyby(~MATEMATICAS_PUNT, ~DEPARTAMENTO, diseno_muestral, svymean)
100 * cv(svyby(~MATEMATICAS_PUNT, ~DEPARTAMENTO, diseno_muestral, svymean))
mean(est$MATEMATICAS_PUNT)


# primera etapa NII / nII
# segunda es NI / nI
# tercera es N_i / n_i
# La multiplicación son los factores de expasión
