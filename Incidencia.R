library(pacman)
p_load(tidyverse, psych, epiR, scales, 
       lubridate, patchwork, openxlsx)


base <- read_csv("fallecidos_departamentos_semanasEpi.csv")
GDB <- read_csv("vacunados_x_departamento_x_semanaEpi_11.csv")
vac <- read.csv("vac_fal_x_departamento.csv")

names(BASE)


#Categorización por macroregiones

data <- data %>% 
  mutate(MACRO = 
           case_when(departamento == "TUMBES" ~ "NORTE",
                     departamento == "PIURA" ~ "NORTE",
                     departamento == "LAMBAYEQUE" ~ "NORTE",
                     departamento == "CAJAMARCA" ~ "NORTE",
                     departamento == "LA LIBERTAD" ~ "NORTE",
                     departamento == "ANCASH"~ "NORTE",
                     departamento == "JUNIN" ~ "CENTRO",
                     departamento == "PASCO" ~ "CENTRO",
                     departamento == "HUANUCO" ~ "CENTRO",
                     departamento == "HUANCAVELICA" ~ "CENTRO",
                     departamento == "AYACUCHO" ~ "CENTRO",
                     departamento == "ICA" ~  "CENTRO",
                     departamento == "AREQUIPA" ~ "SUR",
                     departamento == "MOQUEGUA" ~ "SUR",
                     departamento == "TACNA" ~ "SUR",
                     departamento == "CUSCO" ~ "SUR",
                     departamento == "APURIMAC" ~ "SUR",
                     departamento == "PUNO" ~ "SUR",
                     departamento == "MADRE DE DIOS" ~ "ORIENTE",
                     departamento == "LORETO" ~ "ORIENTE",
                     departamento == "UCAYALI" ~ "ORIENTE",
                     departamento == "AMAZONAS" ~ "ORIENTE",
                     departamento == "SAN MARTIN" ~ "ORIENTE",
                     departamento == "CALLAO" ~ "LIMAMETRO",
                     departamento == "LIMA" ~ "LIMAMETRO"))



###########################################################
################### GRAFICOS LINEAS #######################

data %>%
  filter(fallecido == "1") %>% 
  filter(epi_year == "2021") %>%
  group_by(epi_week, MACRO) %>% 
  count()  %>% 
  ggplot(aes(x = epi_week,
             y = n,
             color = MACRO)) + 
  geom_line() + 
  labs(
    title = "Fallecidos",
    subtitle = "Macroregiones",
    x = "Semana epidemiológica",
    y = "")


VACUNADO <- BASE %>%
  select(vacunado, epi_year, epi_week, DEPARTAMENTO) %>%
  filter(vacunado == "1") %>%
  filter(epi_year == "2021") %>%
  group_by(epi_week,DEPARTAMENTO) %>% 
  count() %>% 
  ggplot(aes(x = epi_week,
             y = n, 
             color = DEPARTAMENTO)) + 
  geom_line()


#######################################################################
####################### BASE ACTUALIZADA ##############################
#######################################################################

SEMANA_BD <- read_csv("semanas_epi.csv")

FIRST <- SEMANA_BD %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2020) %>% 
  ggplot(aes(y = fallecidos, x = epi_week)) +
  geom_col(fill = "#8B2323") + labs(
    title = "Primera ola - semana 10-53 / 2020",
    subtitle = "",
    x = "",
    y = "") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5))

SECOND <- SEMANA_BD %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2021) %>% 
  ggplot(aes(y = fallecidos, x = epi_week)) +
  geom_col(fill = "#8B2323") + labs(
    title = "Segunda ola - semana 1 - 41 / 2021",
    subtitle = "",
    x = "",
    y = "") + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5))

(FIRST + SECOND) + plot_annotation(
  title    = "Fallecidos por COVID-19 confirmados",
  subtitle = "",
  theme = theme(plot.title = element_text(size = 25, hjust = 0.6),
                plot.subtitle = element_text(size = 18, hjust = 0.5)))

REGIONES <- read_csv("TOTAL_fallecidosxciudades.csv")

REG_BD <- REGIONES %>% 
  ggplot(aes(y = DEPARTAMENTO, x = tasa_mortalidad)) +
  geom_col(fill = "#7AC5CD") + labs(
    title = "Fallecidos por departamento del Peru",
    subtitle = "",
    x = "Tasa de mortalidad",
    y = "")

VACUNAS <- SEMANA_BD %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2021) %>% 
  ggplot(aes(y = vacunados, x = epi_week)) +
  geom_col(fill = "#43CD80") + labs(
    title = "Vacunación contra el COVID 19",
    subtitle = "",
    x = "",
    y = "") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5))

##################################################################
############### FALLECIDOS MACROREGIONES #########################

# La macroregión está comprendida por: Ancash, La libertad, Piura,
# Cajamarca, Lambayeque, Tumbes

MACROREGIONES <- read_csv("fallecidosXciudadesXsemanasEpi.csv")
names(MACROREGIONES)

MACROREGIONES <- MACROREGIONES %>%
  mutate(NORTE = ANCASH + LALIBERTAD + 
                 PIURA + CAJAMARCA +
                 LAMBAYEQUE + TUMBES) %>% 
  mutate(CENTRO = ICA + JUNIN + 
           AYACUCHO + PASCO +
           HUANCAVELICA + HUANUCO) %>% 
  mutate(SUR = AREQUIPA + APURIMAC + CUSCO +  
           MOQUEGUA + PUNO + TACNA) %>% 
  mutate(ORIENTE = MADREDEDIOS + LORETO + 
           SANMARTIN + AMAZONAS + 
           UCAYALI) %>% 
  mutate(LIMAM = LIMA + CALLAO)
  
names(MACROREGIONES)

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2020) %>% 
  ggplot(aes(y = NORTE, x = epi_week )) +
  geom_col(fill = "#66CDAA") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Norte",
    x = "Semana epidemiológica",
    y = "")

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2021) %>% 
  ggplot(aes(y = NORTE, x = epi_week )) +
  geom_col(fill = "#66CDAA") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Norte",
    x = "Semana epidemiológica",
    y = "")
  
# La macroregión está comprendido por: Ica, Junín, Ayacucho
# Pasco, Huancavelica y Huánuco

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2020) %>% 
  ggplot(aes(y = CENTRO, x = epi_week )) +
  geom_col(fill = "#FF7256") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Centro",
    x = "Semana epidemiológica",
    y = "")

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2021) %>% 
  ggplot(aes(y = CENTRO, x = epi_week )) +
  geom_col(fill = "#FF7256") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Centro",
    x = "Semana epidemiológica",
    y = "")

# La macroregión sur está comprendida por: Arequipa, Apurimac, 
# Cusco, Moquegua, Puno y Tacna

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2020) %>% 
  ggplot(aes(y = SUR, x = epi_week )) +
  geom_col(fill = "#EE6363") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Sur",
    x = "Semana epidemiológica",
    y = "")

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2021) %>% 
  ggplot(aes(y = SUR, x = epi_week )) +
  geom_col(fill = "#EE6363") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Sur",
    x = "Semana epidemiológica",
    y = "")

# La macroregión está oriente está conformada por las regiones:
# Madre de Dios, Loreto, San Martín, Amazonas, Ucayali

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2020) %>% 
  ggplot(aes(y = ORIENTE, x = epi_week )) +
  geom_col(fill = "#7AC5CD") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Oriente",
    x = "Semana epidemiológica",
    y = "")

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2021) %>% 
  ggplot(aes(y = ORIENTE, x = epi_week )) +
  geom_col(fill = "#7AC5CD") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Oriente",
    x = "Semana epidemiológica",
    y = "")

# La macroregión está oriente está conformada por las regiones:
# Lima y Callao

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2020) %>% 
  ggplot(aes(y = LIMAM, x = epi_week )) +
  geom_col(fill = "#7A67EE") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Lima-Callao",
    x = "Semana epidemiológica",
    y = "")

MACROREGIONES %>% 
  mutate(epi_week = as_factor(epi_week)) %>% 
  filter(epi_year == 2021) %>% 
  ggplot(aes(y = LIMAM, x = epi_week )) +
  geom_col(fill = "#7A67EE") + labs(
    title = "Fallecidos",
    subtitle = "Macroregión Lima-Callao",
    x = "Semana epidemiológica",
    y = "")

macroregiones <- NORTE + CENTRO + 
  SUR + ORIENTE + 
  LIMAM

ggsave(filename = "fallecidomacro.png",
       plot = fallecido_macro,
       height = 5,
       width = 8,
       scale = 1.5,
       dpi = 300)

#######################################################
############ ANALISIS DE 

library(pacman)
p_load(tidyverse, psych, epiR, scales,lubridate, patchwork, openxlsx)

base <- read_csv("fallecidos_departamentos_semanasEpi.csv")
GDB <- read_csv("vacunados_x_departamento_x_semanaEpi_11.csv")
vac <- read.csv("vac_fal_x_departamento.csv")

openxlsx::write.xlsx(base, file = "FALLECIDOS.xlsx")
openxlsx::write.xlsx(GDB, file = "VACUNADOS.xlsx")

###########################################################
############  FALLECIDOS Y VACUNADO######################## 
## AMAZONAS
base <- readxl::read_excel("FALLECIDOS.xlsx")
GDB <- readxl::read_excel("VACUNADOS.xlsx")


AMF <- base %>%
  filter(epi_year == 2021) %>% 
ggplot(aes(epi_week, AMAZONAS)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "AMAZONAS") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

AMV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, AMAZONAS)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "AMAZONAS") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#ANCASH

ANF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, ANCASH)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "ANCASH") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

ANV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, ANCASH)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "ANCASH") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#APURIMAC

APF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, APURIMAC)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "APURIMAC") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

APV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, APURIMAC)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "APURIMAC") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#AREQUIPA

ARF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, AREQUIPA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "AREQUIPA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

ARV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, AREQUIPA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "AREQUIPA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#AYACUCHO

AYF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, AYACUCHO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "AYACUCHO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

AYV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, AYACUCHO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "AYACUCHO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#CAJAMARCA

CAF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, CAJAMARCA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "CAJAMARCA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

CAV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, CAJAMARCA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "CAJAMARCA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#CALLAO 

CALF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, CALLAO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "CALLAO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

CALV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, CALLAO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "CALLAO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#CUSCO 

CUF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, CUSCO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "CUSCO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

CUV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, CUSCO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "CUSCO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#HUANCAVELICA

HUF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, HUANCAVELICA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "HUANCAVELICA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

HUV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, HUANCAVELICA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "HUANCAVELICA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#HUANUCO

HF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, HUANUCO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "HUANUCO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

HV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, HUANUCO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "HUANUCO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#ICA

IF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, ICA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "ICA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

IV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, ICA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "ICA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()


#JUNIN

JUF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, JUNIN)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "JUNIN") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

JUV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, JUNIN)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "JUNIN") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

# LA LIBERTAD

LF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LALIBERTAD)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LA LIBERTAD") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

LV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LALIBERTAD)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LA LIBERTAD") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#LAMBAYEQUE #####################################

LAMF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LAMBAYEQUE)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LAMBAYEQUE") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

LAMV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LAMBAYEQUE)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LAMBAYEQUE") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()


#LIMA

LIMF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LIMA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LIMA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

LIMV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LIMA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LIMA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

# LORETO

LORF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LORETO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LORETO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

LORV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, LORETO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "LORETO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#MADRE DE DIOS

MADF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, MADREDEDIOS)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "MADRE DE DIOS") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

MADV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, MADREDEDIOS)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "MADRE DE DIOS") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

# MOQUEGUA

MOQF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, MOQUEGUA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "MOQUEGUA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

MOQV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, MOQUEGUA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "MOQUEGUA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#PASCO

PASF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, PASCO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "PASCO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

PASV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, PASCO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "PASCO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

# PIURA
PIUF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, PIURA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "PIURA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

PIUV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, PIURA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "PIURA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#PUNO
PUNF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, PUNO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "PUNO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

PUNV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, PUNO)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "PUNO") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#SAN MARTIN

SAMF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, SANMARTIN)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "SAN MARTIN") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

SAMV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, SANMARTIN)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "SANMARTIN") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#TACNA

TACF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, TACNA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "TACNA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

TACV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, TACNA)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "TACNA") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#TUMBES
TUMF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, TUMBES)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "TUMBES") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

TUMV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, TUMBES)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "TUMBES") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

#UCAYALI

UCF <- base %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, UCAYALI)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "UCAYALI") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

UCV <- GDB %>%
  filter(epi_year == 2021) %>% 
  ggplot(aes(epi_week, UCAYALI)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(
    title = "UCAYALI") + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()

library(patchwork)

fallecido_region <- AMF + ANF + APF + ARF + AYF + CAF + 
  CALF + CUF + HUF + HF + IF + JUF + LF + LAMF + LIMF +
  LORF + MADF + MOQF + PASF + PIUF + PUNF + SAMF + TACF + 
  TUMF + UCF


vacunado_region <- AMV + ANV + APV + ARV + AYV + CAV + 
  CALF + CUV + HUV + HV + IV + JUV + LV + LAMV + LIMV + 
  LORV + MADV + MOQV + PASV + PIUV + PUNV + SAMV + TACV +
  TUMV + UCV

ggsave(filename = "fAllecido_region.png",
       plot = fallecido_region,
       height = 5,
       width = 8,
       scale = 1.5,
       dpi = 300)

ggsave(filename = "vacunado_region.png",
       plot = vacunado_region,
       height = 5,
       width = 8,
       scale = 1.5,
       dpi = 300)


library(pacman)
p_load(tidyverse, psych, epiR, scales,lubridate, patchwork, openxlsx)

base <- read_csv("fallecidos_departamentos_semanasEpi.csv")
GDB <- read_csv("vacunados_x_departamento_x_semanaEpi_11.csv")
vac <- read.csv("vac_fal_x_departamento.csv")

######################################################
################### CORRELACION ######################

library(tidyverse)    # Manipulación y visualización de datos
library(openxlsx)     # Importar archivos excel
library(MVN)          # Análisis de normalidad multivariada
library(corrr)        # Análisis de correlación
library(effectsize)   # Tamaño del efecto
library(psych)        # Estadisticos descriptivos
library(car)          # Análisis de homocedasticidad
library(performance)  # Supuestos de la regresión lineal
library(MASS)         # Supuestos de la regresión lineal


## ANALISIS DE NORMALIDAD
shapiro.test(DATA$V_AMAZONAS)
shapiro.test(DATA$F_AMAZONAS)
shapiro.test(DATA$V_ANCASH)
shapiro.test(DATA$F_ANCASH)
shapiro.test(DATA$V_APURIMAC)
shapiro.test(DATA$F_APURIMAC)
shapiro.test(DATA$V_AREQUIPA)
shapiro.test(DATA$F_AREQUIPA)
shapiro.test(DATA$V_AYACUCHO)
shapiro.test(DATA$F_AYACUCHO)
shapiro.test(DATA$V_CAJAMARCA)
shapiro.test(DATA$F_CAJAMARCA)
shapiro.test(DATA$V_CALLAO)
shapiro.test(DATA$F_CALLAO)
shapiro.test(DATA$V_CUSCO)
shapiro.test(DATA$F_CUSCO)
shapiro.test(DATA$V_HUANCAVELICA)
shapiro.test(DATA$F_HUANCAVELICA)
shapiro.test(DATA$V_HUANUCO)
shapiro.test(DATA$F_HUANUCO)
shapiro.test(DATA$V_ICA)
shapiro.test(DATA$F_ICA)
shapiro.test(DATA$V_JUNIN)
shapiro.test(DATA$F_JUNIN)
shapiro.test(DATA$V_LALIBERTAD)
shapiro.test(DATA$F_LALIBERTAD)
shapiro.test(DATA$V_LAMBAYEQUE)
shapiro.test(DATA$F_LAMBAYEQUE)
shapiro.test(DATA$V_LIMA)
shapiro.test(DATA$F_LIMA)
shapiro.test(DATA$V_LORETO)
shapiro.test(DATA$F_LORETO)
shapiro.test(DATA$V_MADREDEDIOS)
shapiro.test(DATA$F_MADREDEDIOS)
shapiro.test(DATA$V_MOQUEGUA)
shapiro.test(DATA$F_MOQUEGUA)
shapiro.test(DATA$V_PASCO)
shapiro.test(DATA$F_PASCO)
shapiro.test(DATA$V_PIURA)
shapiro.test(DATA$F_PIURA)
shapiro.test(DATA$V_PUNO)
shapiro.test(DATA$F_PUNO)
shapiro.test(DATA$V_SANMARTIN)
shapiro.test(DATA$F_SANMARTIN)
shapiro.test(DATA$V_TACNA)
shapiro.test(DATA$F_TACNA)
shapiro.test(DATA$V_TUMBES)
shapiro.test(DATA$F_TUMBES)
shapiro.test(DATA$V_UCAYALI)
shapiro.test(DATA$F_UCAYALI)


## Análisis de correlación (Pearson o Spearman) -----
# H0: Correlación igual a cero (no existe)
# H1: Correlación diferente a cero (existe)

# paquete {stats}
cor.test(DATA$V_UCAYALI, 
         DATA$F_UCAYALI,
         method = "spearman")



















