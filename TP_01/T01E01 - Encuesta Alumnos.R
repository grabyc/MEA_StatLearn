ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

##packages <- c("MASS", "caret", "effects", "e1071", "rpart", "Epi")
packages <- c("tidyverse", "knitr", "hrbrthemes", "magrittr")
ipak(packages)

## ej 1.1

# carga de dataset encuesta 
casos <- read_csv2("data/encuesta.csv") 

casos_1_1 <- casos %>%
  select(c(8:11, 47:51))

as_tibble(casos_1_1)
as.data.frame(casos_1_1)

## ej 1.2
casos_1_2 <- casos_1_1 %>%
  filter(consumiste_alcohol == 'Sí') 

casos_1_2 %>%
  summarize(porc_consumen_alcohol = (n() / count(casos_1_1)) * 100)

casos_1_2_M <- casos_1_2 %>%
  filter(Sexo == 'Masculino') 

casos_1_2_F <- casos_1_2 %>%
  filter(Sexo == 'Femenino') 

(casos_1_2_M %>%
    summarize((n() / count(casos_1_2)) * 100))[[1]]

(casos_1_2_F %>%
    summarize((n() / count(casos_1_2)) * 100))[[1]]


## ej 1.3
casos_1_2 %>% 
  drop_na(Sexo) %>%
  ggplot(aes(x=Edad, fill=Sexo)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') + 
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="") + 
  facet_wrap(~ Sexo)

## ej 1.4
casos_1_4 <- casos_1_2 %>%
  drop_na(Sexo) %>%
  filter(tuviste_un_exceso_de_alcohol == 'Sí') 

(casos_1_4 %>%
    summarize((n() / count(casos_1_2)) * 100))[[1]]


casos_1_4 %<>%
  mutate(ciclo = case_when(
    Año %in% (1:3) ~ "Básico",
    Año %in% (4:6) ~ "Superior"
  ))

casos_1_4 %>%
  group_by(Sexo, ciclo) %>%
  summarise( (n() / count(casos_1_4)) * 100)
