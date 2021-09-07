ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

##packages <- c("MASS", "caret", "effects", "e1071", "rpart", "Epi")
packages <- c("tidyverse", "knitr", "hrbrthemes", "magrittr")
ipak(packages)

## ej 2.2

# carga de dataset pesticidas 
muestras <- read_csv2("data/PESTICIDA.csv") 

## ej 2.3
ggplot(muestras, aes(x=TIEMPO, y=CONC)) + 
  geom_point(size=6) + 
  theme_ipsum()

## ej 2.4
ggplot(muestras, aes(x=TIEMPO, y=CONC, color=as.factor(PESTICIDA)  )) + 
  geom_point(size=6) + 
  labs(color = "Pesticida") +
  theme_ipsum()

## ej 2.5
ggplot(muestras, aes(x=TIEMPO, y=CONC, color=as.factor(PESTICIDA)  )) + 
  geom_point(size=6) + 
  scale_y_log10() +
  labs(color = "Pesticida (log 10)") +
  theme_ipsum()

## ej 2.6
plaguicidas <- read_csv2("data/plaguicidas_nombres.csv") 

muestras_2_6 <- muestras %>% 
  inner_join(plaguicidas, by = c("PESTICIDA" = "Numero"))

muestras_2_6

## ej 2.7
ggplot(muestras_2_6, aes(x=TIEMPO, y=CONC, color=Nombre)) + 
  geom_point(size=4) + 
  scale_y_log10() +
  labs(color = "Pesticida (log 10)") +
  facet_wrap(vars(Nombre), dir = "v") +
  theme_ipsum()
