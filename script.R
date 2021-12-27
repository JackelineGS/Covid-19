
library(tidyverse)
library(gapminder)

fv_bd <- read_csv("Base_departamentos.csv")
openxlsx::write.xlsx(fv_bd, file = "Base_departamentos.xlsx")

library(readxl)
BD <- read_excel("Base_departamentos.xlsx")
View(BD)

#######################################################
################ GRÁFICO POR REGIONES #################

#AMAZONAS

AMA <- BD %>% 
ggplot(mapping = aes(x = epi_week, 
                     y = log(AMAZONAS), 
                     colour = ESTADO_1)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "AMAZONAS", 
       subtitle = "(r= -.21; p = .04)") + 
  theme(legend.title = element_blank(), legend.position = "left")

#ANCASH

ANC <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(ANCASH), 
                       colour = ESTADO_2)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "ANCASH", 
       subtitle = "(r= -.32; p < .01)") + 
  theme(legend.position = "none")

#APURIMAC

APU <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(APURIMAC), 
                       colour = ESTADO_3)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "APURIMAC", 
       subtitle = "(r = -.09; p = .37)") + 
  theme(legend.position = "none")

#AREQUIPA

ARE <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(AREQUIPA), 
                       colour = ESTADO_4)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "AREQUIPA", 
       subtitle = "(r = -.15; p = .12)") + 
  theme(legend.position = "none")

#AYACUCHO

AYA <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(AYACUCHO),
                       colour = ESTADO_5)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "AYACUCHO", 
       subtitle = "(r= -.12; p = .23)") + 
  theme(legend.position = "none")

#CAJAMARCA

CAJ <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(CAJAMARCA),
                       colour = ESTADO_6)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "CAJAMARCA", 
       subtitle = "(r = -.20; p = .04)") + 
  theme(legend.position = "none")

#CALLAO 

CAL <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(CALLAO),
                       colour = ESTADO_7)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "CALLAO", 
       subtitle = "(r = -.46; p < .01)") + 
  theme(legend.position = "none")


#CUSCO 

CUS <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(CUSCO), 
                       colour = ESTADO_8)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "CUSCO", 
       subtitle = "(r = .01; p = .96)") + 
  theme(legend.position = "none")

#HUANCAVELICA

HUA <- BD %>% 
  ggplot(mapping = aes(x = epi_week,
                       y = log(HUANCAVELICA), 
                       colour = ESTADO_9)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "HUANCAVELICA", 
       subtitle = "(r = -.07 ; p = .44)") + 
  theme(legend.position = "none")


#HUANUCO

HUAN <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(HUANUCO), 
                       colour = ESTADO_10)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "HUANUCO", 
       subtitle = " (r = -.34; p < .01 )") + 
  theme(legend.position = "none")


#ICA

ICA <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(ICA), 
                       colour = ESTADO_11)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "ICA", 
       subtitle = "(r = -.34 ; p < .01)") + 
  theme(legend.position = "none")


#JUNIN

JUN <- BD %>% 
  ggplot(mapping = aes(x = epi_week,
                       y = log(JUNIN),
                       colour = ESTADO_12)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "JUNIN", 
       subtitle = "(r = -.20; p = .04)") + 
  theme(legend.position = "none")


#LALIBERTAD

LAL <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(LALIBERTAD), 
                       colour = ESTADO_13)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LA LIBERTAD", 
       subtitle = "(r = -.31; p < .01)") + 
  theme(legend.position = "none")

#LAMBAYEQUE

LAM <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(LAMBAYEQUE),
                       colour = ESTADO_14)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LAMBAYEQUE", 
       subtitle = "(r = -.44; p < .01)") + 
  theme(legend.position = "none")


#LIMA

LIM <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(LIMA),
                       colour = ESTADO_15)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LIMA", 
       subtitle = "(r = -.37; p < .01)") + 
  theme(legend.position = "none")


#LORETO

LOR <- BD %>% 
  ggplot(mapping = aes(x = epi_week,
                       y = log(LORETO),
                       colour = ESTADO_16)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LORETO", 
       subtitle = "(r = -.41; p < .01)") + 
  theme(legend.position = "none")


#MADREDEDIOS

MAD <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(MADREDEDIOS),
                       colour = ESTADO_17)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "MADRE DE DIOS", 
       subtitle = "(r = -.30; p < .01)") + 
  theme(legend.position = "none")


#MOQUEGUA

MOQ <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(MOQUEGUA),
                       colour = ESTADO_18)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "MOQUEGUA", 
       subtitle = "(r = -.21; p = .03)") + 
  theme(legend.position = "none")


#PASCO

PAS <- BD %>%  
  ggplot(mapping = aes(x = epi_week,
                       y = log(PASCO),
                       colour = ESTADO_19)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "PASCO", 
       subtitle = "(r = -.19; p = .06)") + 
  theme(legend.position = "none")


#PIURA

PIU <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(PIURA),
                       colour = ESTADO_20)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "PIURA", 
       subtitle = "(r = -.40; p < .01)") + 
  theme(legend.position = "none")


#PUNO

PUN <- BD %>% 
  ggplot(mapping = aes(x = epi_week,
                       y = log(PUNO),
                       colour = ESTADO_21)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "PUNO", 
       subtitle = "(r = .03; p = .71)") + 
  theme(legend.position = "none")


#SANMARTIN

SAN <- BD %>% 
  ggplot(mapping = aes(x = epi_week,
                       y = log(SANMARTIN),
                       colour = ESTADO_22)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "SAN MARTIN", 
       subtitle = "(r = -.23; p = .02)") + 
  theme(legend.position = "none")


#TACNA

TAC <- BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(TACNA),
                       colour = ESTADO_23)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "TACNA", 
       subtitle = "(r = -.12; p = .23)") + 
  theme(legend.position = "none")


#TUMBES

TUM <- BD %>% 
  ggplot(mapping = aes(x = epi_week,
                       y = log(TUMBES),
                       colour = ESTADO_24)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "TUMBES", 
       subtitle = "(r = -.36;p < .01)") + 
  theme(legend.position = "none")


#UCAYALI

UCA <- BD %>% 
  ggplot(mapping = aes(x = epi_week,
                       y = log(UCAYALI),
                       colour = ESTADO_25)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "UCAYALI", 
       subtitle = "(r = -.43; p < .01)") + 
  theme(legend.position = "none")



######################################################
################### CORRELACION ######################

BD2 <- read_csv("DP1_vacunados_y_fallecidos_x_semanaEpi.csv")
names(BD2)

library(MVN)          # Análisis de normalidad multivariada
library(corrr)        # Análisis de correlación
library(effectsize)   # Tamaño del efecto
library(psych)        # Estadisticos descriptivos


## ANALISIS DE NORMALIDAD
shapiro.test(BD2$AMAZONAS_fal)
shapiro.test(BD2$AMAZONAS_vac)
shapiro.test(BD2$ANCASH_fal)
shapiro.test(BD2$ANCASH_vac)
shapiro.test(BD2$APURIMAC_fal)
shapiro.test(BD2$APURIMAC_vac)
shapiro.test(BD2$AREQUIPA_fal)
shapiro.test(BD2$AREQUIPA_vac)
shapiro.test(BD2$AYACUCHO_fal)
shapiro.test(BD2$AYACUCHO_vac)
shapiro.test(BD2$CAJAMARCA_fal)
shapiro.test(BD2$CAJAMARCA_vac)
shapiro.test(BD2$CALLAO_fal)
shapiro.test(BD2$CALLAO_vac)
shapiro.test(BD2$CUSCO_fal)
shapiro.test(BD2$CUSCO_vac)
shapiro.test(BD2$HUANCAVELICA_vac)
shapiro.test(BD2$HUANCAVELICA_fal)
shapiro.test(BD2$HUANUCO_vac)
shapiro.test(BD2$HUANUCO_fal)
shapiro.test(BD2$ICA_vac)
shapiro.test(BD2$ICA_fal)
shapiro.test(BD2$JUNIN_fal)
shapiro.test(BD2$JUNIN_vac)
shapiro.test(BD2$`LA LIBERTAD_fal`)
shapiro.test(BD2$`LA LIBERTAD_vac`)
shapiro.test(BD2$LAMBAYEQUE_fal)
shapiro.test(BD2$LAMBAYEQUE_vac)
shapiro.test(BD2$LIMA_fal)
shapiro.test(BD2$LIMA_vac)
shapiro.test(BD2$LORETO_fal)
shapiro.test(BD2$LORETO_vac)
shapiro.test(BD2$`MADRE DE DIOS_fal`)
shapiro.test(BD2$`MADRE DE DIOS_vac`)
shapiro.test(BD2$MOQUEGUA_fal)
shapiro.test(BD2$MOQUEGUA_vac)
shapiro.test(BD2$PASCO_fal)
shapiro.test(BD2$PASCO_vac)
shapiro.test(BD2$PIURA_fal)
shapiro.test(BD2$PIURA_vac)
shapiro.test(BD2$PUNO_fal)
shapiro.test(BD2$PUNO_vac)
shapiro.test(BD2$`SAN MARTIN_fal`)
shapiro.test(BD2$`SAN MARTIN_vac`)
shapiro.test(BD2$TACNA_fal)
shapiro.test(BD2$TACNA_vac)
shapiro.test(BD2$TUMBES_fal)
shapiro.test(BD2$TUMBES_vac)
shapiro.test(BD2$UCAYALI_fal)
shapiro.test(BD2$UCAYALI_vac)


## Análisis de correlación (Pearson o Spearman) -----
# H0: Correlación igual a cero (no existe)
# H1: Correlación diferente a cero (existe)

# paquete {stats}
cor.test(BD2$AMAZONAS_fal, 
         BD2$AMAZONAS_vac,
         method = "spearman")


cor.test(BD2$ANCASH_fal, 
         BD2$ANCASH_vac,
         method = "spearman")

cor.test(BD2$APURIMAC_fal, 
         BD2$APURIMAC_vac,
         method = "spearman")

cor.test(BD2$AREQUIPA_fal, 
         BD2$AREQUIPA_vac,
         method = "spearman")

cor.test(BD2$AYACUCHO_fal, 
         BD2$AYACUCHO_vac,
         method = "spearman")

cor.test(BD2$CAJAMARCA_fal, 
         BD2$CAJAMARCA_vac,
         method = "spearman")

cor.test(BD2$CALLAO_fal, 
         BD2$CALLAO_vac,
         method = "spearman")


cor.test(BD2$CUSCO_fal, 
         BD2$CUSCO_vac,
         method = "spearman")


cor.test(BD2$HUANCAVELICA_fal, 
         BD2$HUANCAVELICA_vac,
         method = "spearman")

cor.test(BD2$HUANUCO_fal, 
         BD2$HUANUCO_vac,
         method = "spearman")

cor.test(BD2$ICA_fal, 
         BD2$ICA_vac,
         method = "spearman")

cor.test(BD2$JUNIN_fal, 
         BD2$JUNIN_vac,
         method = "spearman")

cor.test(BD2$`LA LIBERTAD_fal`,
         BD2$ `LA LIBERTAD_vac`,
         method = "spearman")

cor.test(BD2$LAMBAYEQUE_fal, 
         BD2$LAMBAYEQUE_vac,
         method = "spearman")

cor.test(BD2$LIMA_fal, 
         BD2$LIMA_vac,
         method = "spearman")

cor.test(BD2$LORETO_fal, 
         BD2$LORETO_vac,
         method = "spearman")

cor.test(BD2$`MADRE DE DIOS_fal`, 
         BD2$`MADRE DE DIOS_vac`,
         method = "spearman")

cor.test(BD2$MOQUEGUA_fal, 
         BD2$MOQUEGUA_vac,
         method = "spearman")

cor.test(BD2$PASCO_fal, 
         BD2$PASCO_vac,
         method = "spearman")

cor.test(BD2$PIURA_fal, 
         BD2$PIURA_vac,
         method = "spearman")

cor.test(BD2$PUNO_fal, 
         BD2$PUNO_vac,
         method = "spearman")

cor.test(BD2$`SAN MARTIN_fal`, 
         BD2$`SAN MARTIN_vac`,
         method = "spearman")

cor.test(BD2$TACNA_fal, 
         BD2$TACNA_vac,
         method = "spearman")

cor.test(BD2$TUMBES_fal, 
         BD2$TUMBES_vac,
         method = "spearman")

cor.test(BD2$UCAYALI_fal, 
         BD2$UCAYALI_vac,
         method = "spearman")


######################################################
################ UNIR LOS GRAFICOS ###################

library(patchwork)

correlacion <- AMA + ANC + APU + ARE + AYA + CAJ + 
               CAL + CUS + HUA + HUAN + ICA + JUN + 
               LAL + LAM + LIM + LOR + MAD + MOQ +
               PAS + PIU + PUN + SAN + TAC + TUM + 
               UCA

ggsave(filename = "regiones.png",
       plot = correlacion,
       height = 9,
       width = 12,
       scale = 1.5,
       dpi = 300)







