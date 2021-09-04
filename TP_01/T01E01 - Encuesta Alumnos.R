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
  summarize(porcentaje = (n() / count(casos_1_4)) * 100) %>%
  ggplot(aes(x=Sexo, y=porcentaje$n, fill=Sexo)) +
    geom_bar(stat="identity", alpha=.6) +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    facet_wrap( ~ ciclo) + 
    theme_ipsum() +
    labs(fill="") 

## ej 1.5
casos_1_2 %>%
  drop_na(Sexo) %>%
  mutate(ciclo = case_when(
    Año %in% (1:3) ~ "Básico",
    Año %in% (4:6) ~ "Superior"
  )) %>%
  group_by(Sexo, ciclo) %>%
  summarize(alc_exs = sum(tuviste_un_exceso_de_alcohol == 'Sí') / count(casos_1_2),
            alc_anio = sum(consumiste_alcohol_este_anio == 'Sí') / count(casos_1_2),
            alc_mes = sum(consumiste_alcohol_este_mes == 'Sí') / count(casos_1_2),
            alc_semana = sum(consumiste_alcohol_esta_semana == 'Sí') / count(casos_1_2)) %>%
  pivot_longer(!c("Sexo", "ciclo"), names_to = "grupo", values_to = "porcentaje") %>%
  ggplot(aes(x=reorder(grupo, porcentaje$n), y=porcentaje$n, fill=Sexo)) +
    geom_segment(aes(x = reorder(grupo, porcentaje$n), 
                     xend = reorder(grupo, porcentaje$n), 
                     y = 0, yend = porcentaje$n),
                 color = "gray", lwd = 1.5) +
    geom_point(size = 4, pch = 21, bg = 4, col = "gray") +
    coord_flip() +
    facet_wrap(Sexo ~ ciclo) + 
    theme_ipsum() +
    labs(fill="") + xlab("Variable") + ylab("Porcentaje")
