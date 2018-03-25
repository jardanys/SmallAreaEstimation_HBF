
rm(list = ls())
options(survey.lonely.psu="adjust")

library(dplyr)
library(stratification)

est <- readRDS("estudiantes.rds")
est <- est[c("CODIGOMUNICIPIO", "NOMBREMUNICIPIO" ,"CODIGO_ICFES","ID_estud",
             "AGSB_NOMBREINSTITUCION", "DEPARTAMENTO", "CALENDARIO",
             "NATURALEZA", "JORNADA", "PERS_GENERO", "FINS_ESTRATOVIVIENDAENERGIA", "FINS_PERSONASHOGARACTUAL", 
             "FINS_CUARTOSHOGARACTUAL", "FINS_PISOSHOGAR", "FINS_TIENEINTERNET", 
             "FINS_TIENECOMPUTADOR", "FINS_TIENEAUTOMOVILPARTICULAR", "LECTURA_CRITICA_PUNT", 
             "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT", "CIENCIAS_NATURALES_PUNT", 
             "INGLES_PUNT", "INGLES_DESEM", "EVALUADOS")]
est$CALENDARIO <- as.character(est$CALENDARIO)
est$NATURALEZA <- as.character(est$NATURALEZA)
est$JORNADA <- as.character(est$JORNADA)
# UPM: Municipio (tx: Numero de estudiantes)
# USM: Colegio (tx: Numero de estudiantes)
# UTM: Estudiante
# Dise�o ESTMAS - ESTMAS CONGL 

#prueba <-  est %>% group_by(CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
#  summarise(neval = n(), EVALUADOS = max(EVALUADOS))
#table(prueba$neval - prueba$EVALUADOS)

cons_mpio <- est %>% group_by(CODIGOMUNICIPIO) %>% summarise(tx = n())
set.seed(12345)
numestratos <- 8
LH <- strata.LH(x = cons_mpio$tx, CV = 0.03, Ls = numestratos, takeall = T)
cortes <- c(min(cons_mpio$tx), LH$bh, max(cons_mpio$tx))
cons_mpio$estrato_mpio <- cut(cons_mpio$tx, breaks = cortes, include.lowest = T, right = F,
                              label = paste0("Estrato", 1:numestratos))

cons_mpio <- arrange(cons_mpio, tx)
set.seed(12345)
indica_estratifMpio <- sampling::strata(cons_mpio, "estrato_mpio", size = LH$nh,
                                        method = "srswor", description = T)
mue_mpios <- sampling::getdata(cons_mpio, indica_estratifMpio)
mue_mpios <- mue_mpios[c("estrato_mpio", "CODIGOMUNICIPIO")]
# Selecci�n de colegios 
# Variale auxiliar de estratificaci�n, el total

Tamanos <- data.frame(estrato_mpio = paste0("Estrato",1:numestratos), NI = LH$Nh, nI = LH$nh)

marco_mpio <- merge(est, mue_mpios, all.y = T, by = "CODIGOMUNICIPIO") 
marco_mpio <- merge(marco_mpio, Tamanos, by = "estrato_mpio" )

################################### Selecci�n de colegios ######################
cons_colegio <- marco_mpio %>% group_by(estrato_mpio,CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
  summarise(tx = n())

cons_temp <- cons_colegio %>% group_by(estrato_mpio,CODIGOMUNICIPIO) %>% 
  summarise(numColegios = n()) %>% arrange(-numColegios)

# Menos  o igual de 3 colegios hacerlos todos
# De 4 a 12 colegios hacer la mitad con MAS
# De 13 a 29 colegios hacer la tercera parte con MAS
# DE 30 Colegios en adelante crear tres estratos con un
# nivel de precisi�n del 3%

# Muncipios en donde se censar�
mpios_grupo1 <- cons_temp$CODIGOMUNICIPIO[cons_temp$numColegios <= 3]
mpios_grupo2 <- cons_temp$CODIGOMUNICIPIO[cons_temp$numColegios >= 4 &
                                            cons_temp$numColegios <= 12 ]
mpios_grupo3 <- cons_temp$CODIGOMUNICIPIO[cons_temp$numColegios >= 13 &
                                            cons_temp$numColegios <= 29 ]
mpios_grupo4 <- cons_temp$CODIGOMUNICIPIO[cons_temp$numColegios >= 30]

# Selecci�n de municipios del grupo 1
MueColegios_mpiosgrupo1 <- subset(marco_mpio, 
                                  marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo1)
#table(MueColegios_mpiosgrupo1$NOMBREMUNICIPIO)
mue_ColA <- cons_temp[cons_temp$CODIGOMUNICIPIO %in% mpios_grupo1, ]
mue_ColA$NII <- mue_ColA$numColegios
mue_ColA$nII <- mue_ColA$numColegios
mue_ColA$numColegios <- NULL
mue_ColA$EstratoColegio <- "GRUPO1"
mue_colgrupo1 <- merge(marco_mpio, mue_ColA, 
                       by = c("estrato_mpio","CODIGOMUNICIPIO"), all.y = T)

# Selecci�n de municipios del grupo 2
MarcoColegios_mpiosgrupo2 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo2)
cons_colegA_grupo2 <- MarcoColegios_mpiosgrupo2 %>% 
  group_by(CODIGOMUNICIPIO,CODIGO_ICFES) %>% 
  summarise(temp = n())
cons_colB_grupo2 <- cons_colegA_grupo2 %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Num_col = n())
cons_colB_grupo2$nNum_col <- ceiling(cons_colB_grupo2$Num_col * 0.5)

cons_colegA_grupo2 <- cons_colegA_grupo2[order(cons_colegA_grupo2$CODIGOMUNICIPIO),]
cons_colB_grupo2 <- cons_colB_grupo2[order(cons_colB_grupo2$CODIGOMUNICIPIO),]

set.seed(12345)
indicamue_grup2 <- sampling::strata(data = cons_colegA_grupo2, stratanames = "CODIGOMUNICIPIO",
                                    size = cons_colB_grupo2$nNum_col, method="srswor", 
                                    description = T)
mue_Col2 <- cons_colegA_grupo2[indicamue_grup2$ID_unit,] 
mue_Col2$temp <- NULL; 
mue_Col2 <- merge(mue_Col2, cons_colB_grupo2, by = "CODIGOMUNICIPIO")
names(mue_Col2)[3:4] <- c("NII", "nII")
mue_Col2$EstratoColegio <- "GRUPO2"
mue_colgrupo2 <- merge(marco_mpio, mue_Col2, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))
# Prueba 
prue <- mue_colgrupo2 %>% group_by(CODIGOMUNICIPIO, CODIGO_ICFES) %>%
  summarise(temp = n(), nII = max(nII), NII = max(NII))
prue2 <- prue %>% group_by(CODIGOMUNICIPIO) %>%
  summarise(n_col = n(), nII = max(nII), NII = max(NII))
length(unique(prue2$CODIGOMUNICIPIO))
dim(prue2)


# Municipios del grupo 3
# Selecci�n de municipios del grupo 3
MarcoColegios_mpiosgrupo3 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo3)
cons_colegA_grupo3 <- MarcoColegios_mpiosgrupo3 %>% 
  group_by(CODIGOMUNICIPIO,CODIGO_ICFES) %>% 
  summarise(temp = n())
cons_colB_grupo3 <- cons_colegA_grupo3 %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Num_col = n())
cons_colB_grupo3$nNum_col <- ceiling(cons_colB_grupo3$Num_col * (1/3))

cons_colegA_grupo3 <- cons_colegA_grupo3[order(cons_colegA_grupo3$CODIGOMUNICIPIO),]
cons_colB_grupo3 <- cons_colB_grupo3[order(cons_colB_grupo3$CODIGOMUNICIPIO),]

set.seed(12345)
indicamue_grup3 <- sampling::strata(data = cons_colegA_grupo3, stratanames = "CODIGOMUNICIPIO",
                                    size = cons_colB_grupo3$nNum_col, method="srswor", 
                                    description = T)
mue_Col3 <- cons_colegA_grupo3[indicamue_grup3$ID_unit,] 
mue_Col3$temp <- NULL; 
mue_Col3 <- merge(mue_Col3, cons_colB_grupo3, by = "CODIGOMUNICIPIO")
names(mue_Col3)[3:4] <- c("NII", "nII")
mue_Col3$EstratoColegio <- "GRUPO3"

mue_colgrupo3 <- merge(marco_mpio, mue_Col3, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))
# Prueba 
prue_grupo3 <- mue_colgrupo3 %>% group_by(estrato_mpio, CODIGOMUNICIPIO, CODIGO_ICFES) %>%
  summarise(temp = n(), nII = max(nII), NII = max(NII))
prue2_grupo3 <- prue_grupo3 %>% group_by(estrato_mpio, CODIGOMUNICIPIO) %>%
  summarise(n_col = n(), nII = max(nII), NII = max(NII))
length(unique(prue2_grupo3$CODIGOMUNICIPIO))
dim(prue2_grupo3)

############################# Grupo 4 de municipios ##################################
# Municipios del grupo 4
MarcoColegios_mpiosgrupo4 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo4)

cons_colegA_grupo4 <- MarcoColegios_mpiosgrupo4 %>% 
  group_by(CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
  summarise(tx = n())
# length(unique(cons_colegA_grupo4$CODIGOMUNICIPIO))
#cons_temp
# Sobre ese hacer programci�n funcional
#DATOS <- cons_colegA_grupo4[cons_colegA_grupo4$CODIGOMUNICIPIO == "00152", ]

f_MuestraLH <- function(DATOS){ 
  DATOS2 <- DATOS
  set.seed(12345)
  LHCOL <- strata.LH(x = DATOS2$tx, CV = 0.05, Ls = 4, takeall = 0, algo = "Kozak")
  cortesCOL <- c(min(DATOS$tx), LHCOL$bh, max(DATOS$tx))
  DATOS2$EstratoColegio <- cut(DATOS2$tx, breaks = cortesCOL, include.lowest = T, right = F,
                               label = paste0("Estrato", 1:4))
  
  DATOS2 <- arrange(DATOS2, tx)
  set.seed(12345)
  indica_estratifCOl <- sampling::strata(DATOS2, stratanames = "EstratoColegio", size = LHCOL$nh,
                                         method = "srswor", description = T)
  mue_colgrupo4 <- sampling::getdata(DATOS2, indica_estratifCOl)
  mue_colgrupo4 <- mue_colgrupo4[c("EstratoColegio", "CODIGOMUNICIPIO", "CODIGO_ICFES")]
  
  TamanosCol <- data.frame(EstratoColegio = paste0("Estrato", 1:4), NII = LHCOL$Nh, nII = LHCOL$nh)
  mue_colgrupo4 <- merge(mue_colgrupo4, TamanosCol, by = "EstratoColegio" )
  # Que la salida genre la muestra de colegios para los del grupo de municipio 4
  return(mue_colgrupo4)  
}


Muestras <- cons_colegA_grupo4 %>% 
  split(.$CODIGOMUNICIPIO) %>% 
  map(~f_MuestraLH(DATOS = .))
#length(Muestras);     class(Muestras)
#dim(Muestras$"00152") # 27 colegios

mue_Col4 <- do.call(rbind, Muestras)
#nrow(mue_Col4[substr(row.names(mue_Col4), 1, 5) == "00152",]) # 27 colegios
row.names(mue_Col4) <- 1:nrow(mue_Col4)

# table(mue_Col4$nII == 1) # Tratar muestras de tama�o 1
mue_Col4$nII <- ifelse(mue_Col4$nII == 1, 2, mue_Col4$nII)
mue_colgrupo4 <- merge(marco_mpio, mue_Col4, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"), all.y = T) 
# length(unique(paste0(mue_colgrupo4$CODIGOMUNICIPIO, mue_colgrupo4$CODIGO_ICFES)))
# 483 colegios
mue_colgrupo4$EstratoColegio <- as.character(mue_colgrupo4$EstratoColegio)
muestra <- bind_rows(mue_colgrupo1, mue_colgrupo2,mue_colgrupo3, mue_colgrupo4)
muestra <- muestra[c("estrato_mpio", "CODIGOMUNICIPIO", "NOMBREMUNICIPIO",  
                     "EstratoColegio", "CODIGO_ICFES", "ID_estud",
                     "AGSB_NOMBREINSTITUCION", "DEPARTAMENTO", "CALENDARIO", 
                     "NATURALEZA",  "JORNADA", "PERS_GENERO", "FINS_ESTRATOVIVIENDAENERGIA",
                     "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", "FINS_PISOSHOGAR", 
                     "FINS_TIENEINTERNET",  "FINS_TIENECOMPUTADOR", "FINS_TIENEAUTOMOVILPARTICULAR", 
                     "LECTURA_CRITICA_PUNT", "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT",
                     "CIENCIAS_NATURALES_PUNT",  "INGLES_PUNT", "INGLES_DESEM", "EVALUADOS", "NI", "nI", 
                     "NII", "nII")]
# Cuantos municipios
length(unique(muestra$CODIGOMUNICIPIO))
# Cuantos colegios (628)
length(unique(muestra$CODIGO_ICFES))
length(unique(paste(muestra$CODIGOMUNICIPIO,muestra$CODIGO_ICFES, sep = "_")))

# Seleccionar estudiantes (MAS)
consulta_estud <- muestra %>% group_by(CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
  summarise(Num_est = n()) %>% arrange(-Num_est)
summary(consulta_estud$Num_est)
# Sacar el 30% de los estudiantes
# table(duplicated(muestra$CODIGO_ICFES))
consulta_estud$n_i <- ceiling(consulta_estud$Num_est * 0.2) 

consulta_estud <- consulta_estud %>% arrange(CODIGO_ICFES)
names(consulta_estud)[3] <- c("N_i")
muestra <- muestra %>% arrange(CODIGO_ICFES)

indica_mueestu <- sampling::strata(muestra, stratanames = "CODIGO_ICFES",
                                   size = consulta_estud$n_i,
                                   method = "srswor", description = T)

muestra3etapas <- muestra[indica_mueestu$ID_unit,]  
muestra3etapas <- merge(muestra3etapas,  consulta_estud)
dim(muestra3etapas) # 5556 encuestas
#table(muestra3etapas$CODIGO_ICFES)
#table(muestra3etapas$nII == 1); muestra3etapas[muestra3etapas$nII == 1,]
saveRDS(muestra3etapas, "muestra3etapas.RDS")
# muestra3etapas <- readRDS("muestra3etapas.rds")
#for(i in 1:ncol(muestra3etapas)){
#  print(paste(colnames(muestra3etapas)[i],table(muestra3etapas[,i] == "")))
#}

library(survey)
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













