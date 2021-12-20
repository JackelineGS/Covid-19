
library(tidyverse)
library(gapminder)

fv_bd <- read_csv("departamentos.csv")
openxlsx::write.xlsx(fv_bd, file = "BD2.xlsx")

library(readxl)
BD <- read_excel("BD.xlsx")
View(BD)


BD %>% 
ggplot(mapping = aes(x = epi_week, y = log(AMAZONAS), colour = ESTADO)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita on Five Continents", 
       subtitle = "Individual countries shown in gray, trend in blue.")




