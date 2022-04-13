library(tidyverse)
library(gapminder)
library(readxl)

BD <- read_excel("data.xlsx")
names(BD)

0########################################################################
##2.#### GR?FICO DE FALLECIDOS, VACUNADOS Y CASOS POR REGION #############
########################################################################

BD %>% 
  ggplot(mapping = aes(x = epi_week, 
                       y = log(total), 
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Total de casos", 
       subtitle = "(r= -.21; p = .04)") + 
  theme(legend.title = element_blank(), legend.position = "left")



#AMAZONAS

AMA <- BD %>% 
ggplot(mapping = aes(x = epi_week, 
                     y = log(AMAZONAS), 
                     colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "AMAZONAS", 
       subtitle = "(r= -.22; p = .02)") + 
  theme(legend.title = element_blank(), legend.position = "left")

#ANCASH

ANC <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(ANCASH), 
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "ANCASH", 
       subtitle = "(r= -.36; p < .01)") + 
  theme(legend.position = "none")

#APURIMAC

APU <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(APURIMAC), 
                       colour = ESTADO)) + 
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
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(AREQUIPA), 
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "AREQUIPA", 
       subtitle = "(r = -.19; p = .05)") + 
  theme(legend.position = "none")

#AYACUCHO

AYA <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(AYACUCHO),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "AYACUCHO", 
       subtitle = "(r= -.14; p = .14)") + 
  theme(legend.position = "none")

#CAJAMARCA

CAJ <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(CAJAMARCA),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "CAJAMARCA", 
       subtitle = "(r = -.20; p = .03)") + 
  theme(legend.position = "none")

#CALLAO 

CAL <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(CALLAO),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "CALLAO", 
       subtitle = "(r = -.48; p < .01)") + 
  theme(legend.position = "none")


#CUSCO 

CUS <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(CUSCO), 
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "CUSCO", 
       subtitle = "(r = -.04; p = .67)") + 
  theme(legend.position = "none")

#HUANCAVELICA

HUA <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(HUANCAVELICA), 
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "HUANCAVELICA", 
       subtitle = "(r = -.08 ; p = .42)") + 
  theme(legend.position = "none")


#HUANUCO

HUAN <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(HUANUCO), 
                       colour = ESTADO)) + 
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
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(ICA), 
                       colour = ESTADO)) + 
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
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(JUNIN),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "JUNIN", 
       subtitle = "(r = -.21; p = .03)") + 
  theme(legend.position = "none")


#LALIBERTAD

LAL <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(LA.LIBERTAD), 
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LA LIBERTAD", 
       subtitle = "(r = -.34; p < .01)") + 
  theme(legend.position = "none")

#LAMBAYEQUE

LAM <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(LAMBAYEQUE),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LAMBAYEQUE", 
       subtitle = "(r = -.46; p < .01)") + 
  theme(legend.position = "none")


#LIMA

LIM <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(LIMA),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LIMA", 
       subtitle = "(r = -.46; p < .01)") + 
  theme(legend.position = "none")


#LORETO

LOR <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(LORETO),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "LORETO", 
       subtitle = "(r = -.43; p < .01)") + 
  theme(legend.position = "none")


#MADREDEDIOS

MAD <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(MADRE.DE.DIOS),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "MADRE DE DIOS", 
       subtitle = "(r = -.29; p < .01)") + 
  theme(legend.position = "none")


#MOQUEGUA

MOQ <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(MOQUEGUA),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "MOQUEGUA", 
       subtitle = "(r = -.19; p = .05)") + 
  theme(legend.position = "none")


#PASCO

PAS <- BD %>%  
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(PASCO),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "PASCO", 
       subtitle = "(r = -.18; p = .06)") + 
  theme(legend.position = "none")


#PIURA

PIU <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(PIURA),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "PIURA", 
       subtitle = "(r = -.43; p < .01)") + 
  theme(legend.position = "none")


#PUNO

PUN <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(PUNO),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "PUNO", 
       subtitle = "(r = .05; p = .60)") + 
  theme(legend.position = "none")


#SANMARTIN

SAN <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(SAN.MARTIN),
                       colour = ESTADO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "SAN MARTIN", 
       subtitle = "(r = -.26; p < .01)") + 
  theme(legend.position = "none")


#TACNA

TAC <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week, 
                       y = log(TACNA),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "TACNA", 
       subtitle = "(r = -.06; p = .50)") + 
  theme(legend.position = "none")


#TUMBES

TUM <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(TUMBES),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "TUMBES", 
       subtitle = "(r = -.40;p < .01)") + 
  theme(legend.position = "none")


#UCAYALI

UCA <- BD %>% 
  filter(ESTADO != "caso") %>%
  ggplot(mapping = aes(x = epi_week,
                       y = log(UCAYALI),
                       colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "UCAYALI", 
       subtitle = "(r = -.44; p < .01)") + 
  theme(legend.position = "none")



######################################################
################### CORRELACION ######################

BD2 <- read_csv("DP1_covid19-peru_x_semanaEpi.csv")
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

####################################################################
############ SCATERPLOT DE VACUNADOS Y MORTALIDAD ##################
####################################################################

install.packages("ggrepel")

library(tidyverse)
library(gapminder)
library(readxl)
library(ggrepel)

departamentos <- read_csv("DP2_covid19-peru_resumen_x_departamentos_V1.csv")
semana_BD <- read_excel("Semana_EPI.xlsx")
names(semana_BD)

departamentos <- 

sp <- ggplot(departamentos, aes(x= vac_porcentaje, 
                                y= tasa_mortalidad,
                                label = departamento)) + 
  geom_point() + 
  stat_smooth(method = lm) +
  labs(x = "Porcentaje de vacunados",
       y = "Tasa de mortalidad") + 
  geom_label_repel(fill = "white", xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))


ggsave(filename = "vacunados_mortalidad.png",
       plot = sp,
       height = 6,
       width = 12,
       scale = 1.5,
       dpi = 300)


















