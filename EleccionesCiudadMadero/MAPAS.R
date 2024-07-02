library(writexl)
library(corrplot)
library(sf)
library(ggplot2)
library(readxl)
library(dplyr)


mexico_sf <- st_read("C:/Users/acer/Downloads/SECCION/SECCION.shp") #Datos del INE para hacer un mapa

CIUDAD_MADERO <- mexico_sf %>%
  filter(ENTIDAD == "28",
         MUNICIPIO == "9") %>%
  mutate(SECCION = as.character(SECCION))

DATA <- read_xlsx("C:/Users/Acer/Downloads/CiudadMadero2021y2024.xlsx",
                  sheet = "2021y2024",
                  skip = 1)

# Renaming specific columns in the tibble
DATA <- DATA %>%
  rename(
    `TOTAL DE VOTOS 2024` = `TOTAL DE VOTOS`,
    `TOTAL DE VOTOS 2021` = `...5`,
    `PVEM pct` = `...8`,
    `PT + MORENA pct` = `...12`,
    `SIGAMOS HACIENDO HISTORIA pct` = `...16`,
    `PAN pct` = `...20`,
    `PRI pct` = `...22`,
    `FUERZA Y CORAZON POR MEXICO pct` = `...24`,
    `MC` = `...25`,
    `MC pct` = `...26`,
    `MOVIMIENTO CIUDADANO pct` = `...28`,
    `PRD 2021` = `PRD...29`,
    `PRD 2021 pct` = `...30`,
    `PRD 2024` = `PRD...31`,
    `PRD 2024 pct` = `...32`,
    `PES pct` = `...34`,
    `RSP pct` = `...36`,
    `FXM pct` = `...38`,
    `MIGUEL RODRÍGUEZ SALAZAR pct` = `...40`,
    `VOTOS NULOS pct` = `...42`,
    `CANDIDATOS NO REGISTRADOS pct` = `...44`
  )




DATA2021 <- DATA %>% 
  select(
    -`TOTAL DE VOTOS 2024`,
    -`PARTICIPACION CIUDADANA`,
    -`SIGAMOS HACIENDO HISTORIA`,
    -`SIGAMOS HACIENDO HISTORIA pct`,
    -`MORENA GANADOR 2024`,
    -`DIFERENCIA 2024`,
    -`FUERZA Y CORAZON POR MEXICO`,
    -`FUERZA Y CORAZON POR MEXICO pct`,
    -`MOVIMIENTO CIUDADANO`,
    -`MOVIMIENTO CIUDADANO pct`,
    -`PRD 2024`,
    -`PRD 2024 pct`
  )


DATA2021 <- left_join(DATA2021, CIUDAD_MADERO, by = "SECCION")
DATA2021 <- st_sf(DATA2021)
DATA2021 <- DATA2021 %>% 
  mutate(
    DIFERENCIA = `PT + MORENA pct` - `PAN pct`
  )

DATA2024 <- DATA %>% 
  select(
    SECCION,
    `TOTAL DE VOTOS 2024`,
    `PARTICIPACION CIUDADANA`,
    `SIGAMOS HACIENDO HISTORIA`,
    `SIGAMOS HACIENDO HISTORIA pct`,
    `MORENA GANADOR 2024`,
    `DIFERENCIA 2024`,
    `FUERZA Y CORAZON POR MEXICO`,
    `FUERZA Y CORAZON POR MEXICO pct`,
    `MOVIMIENTO CIUDADANO`,
    `MOVIMIENTO CIUDADANO pct`,
    `PRD 2024`,
    `PRD 2024 pct`
  )

DATA2024 <- left_join(DATA2024, CIUDAD_MADERO, by = "SECCION")
DATA2024 <- st_sf(DATA2024)
DATA2024 <- DATA2024 %>% 
  mutate(
    DIFERENCIA = `SIGAMOS HACIENDO HISTORIA pct` - `FUERZA Y CORAZON POR MEXICO pct`
  )

# DIFERENCIA PORCENTUAL DE VOTOS DE MORENA DE 2021 a 2024

DATA <- DATA %>%
  mutate(
    `DIFERENCIA MORENA pct` = `SIGAMOS HACIENDO HISTORIA pct` - `PT + MORENA pct`,
    `DIFERENCIA MORENA` = `SIGAMOS HACIENDO HISTORIA` - `PT + MORENA`
  )

DATAdif <- left_join(DATA %>% select(`DIFERENCIA MORENA pct`,`DIFERENCIA MORENA` , SECCION), CIUDAD_MADERO, by = "SECCION")
DATAdif <- st_sf(DATAdif)

#|MAPS

#2021


DATA2021$DIFERENCIA_CATEGORIA <- cut(DATA2021$DIFERENCIA,
                                     breaks = c(-Inf, -0.13, 0, 0.13, Inf),
                                     labels = c("MORENA-PT perdió con + de 13%", 
                                                "MORENA-PT perdió con - de 13%", 
                                                "MORENA-PT ganó con - de 17%", 
                                                "MORENA-PT ganó con + de 17%"))


ggplot(data = DATA2021) +
  geom_sf(aes(fill = DIFERENCIA_CATEGORIA), color = "white", size = 0.2) +
  scale_fill_manual(name = NULL,
                    values = c("MORENA-PT perdió con + de 13%" = "darkblue",
                               "MORENA-PT perdió con - de 13%" = "lightblue",
                               "MORENA-PT ganó con - de 17%" = "pink",
                               "MORENA-PT ganó con + de 17%" = "darkred")) +
  labs(title = "Diferencia porcentual de votos entre la coalición MORENA-PT y el PAN en 2021")

#2024


DATA2024$DIFERENCIA_CATEGORIA <- cut(DATA2024$DIFERENCIA,
                                     breaks = c(-Inf, -0.13, 0, 0.13, Inf),
                                     labels = c("SHH perdió con + de 13%", 
                                                "SHH perdió con - de 13%", 
                                                "SHH ganó con - de 17%", 
                                                "SHH ganó con + de 17%"))


ggplot(data = DATA2024) +
  geom_sf(aes(fill = DIFERENCIA_CATEGORIA), color = "white", size = 0.2) +
  scale_fill_manual(name = NULL,
                    values = c("SHH perdió con + de 13%" = "darkblue",
                               "SHH perdió con - de 13%" = "lightblue",
                               "SHH ganó con - de 17%" = "pink",
                               "SHH ganó con + de 17%" = "darkred")) +
  labs(title = "Diferencia porcentual de votos entre la coalición Sigamos Haciendo Historia (SHH) \n y la coalición Fuerza y Corazón por México en 2024") 
#Diferencia porcentual de votos entre años



DATAdif$DIFERENCIA_CATEGORIA <- cut(DATAdif$`DIFERENCIA MORENA pct`,
                                     breaks = c(-Inf, 0.1, 0.2, 0.3, Inf),
                                     labels = c("Entre 0% y 10%", 
                                                "Entre 10% y 20%", 
                                                "Entre 20% y 30%", 
                                                "Entre 30% y 37%"))


ggplot(data = DATAdif) +
  geom_sf(aes(fill = DIFERENCIA_CATEGORIA), color = "white", size = 0.2) +
  scale_fill_manual(name = "Diferencias",
                    values = c("Entre 0% y 10%" = "darkblue",
                               "Entre 10% y 20%" = "blue1",
                               "Entre 20% y 30%" = "red1",
                               "Entre 30% y 37%" = "darkred")) +
  labs(title = "Diferencia porcentual de votos de las coaliciones \n de MORENA entre 2021 y 2024") 

#Diferencia absoluta de votos entre años



DATAdif$DIFERENCIA_CATEGORIA_abs <- cut(DATAdif$`DIFERENCIA MORENA`,
                                    breaks = c(-Inf, -100, 0, 100, 200, 300, Inf),
                                    labels = c("Menos de -100", 
                                               "Entre -100 y 0", 
                                               "Entre 0 y 100", 
                                               "Entre 100 y 200",
                                               "Entre 200 y 251",
                                               "701"))


ggplot(data = DATAdif) +
  geom_sf(aes(fill = DIFERENCIA_CATEGORIA_abs), color = "white", size = 0.2) +
  scale_fill_manual(name = "Diferencias",
                    values = c("Menos de -100" = "darkblue",
                               "Entre -100 y 0" = "blue1",
                               "Entre 0 y 100" = "red3",
                               "Entre 100 y 200" = "darkred",
                               "Entre 200 y 251" = "gray43",
                               "701" = "black")) +
  labs(title = "Diferencia absoluta de votos de las coaliciones \n de MORENA entre 2021 y 2024") 

