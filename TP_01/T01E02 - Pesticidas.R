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
  theme_ipsum() +
  stat_smooth(method = "lm", col = "grey")

## ej 2.10

muestras_2_10 <- muestras_2_6 %>%
  group_by(ANIO, CHACRA, Nombre, ARBOL, FRUTO) %>%
  summarise(deposito_inicial = CONC[TIEMPO==0],
            tasa_degradacion = (max(CONC)-min(CONC))/max(TIEMPO)) 

muestras_2_10 %>%
  ggplot( aes(x=Nombre, y=deposito_inicial, fill=Nombre)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("Pesticida") + ylab("Deposito Inicial")

muestras_2_10 %>%
  ggplot( aes(x=Nombre, y=tasa_degradacion, fill=Nombre)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("Pesticida") + ylab("Tasa Degradación")


muestras_2_10 %>%
  group_by(Nombre) %>%
  summarise(prom_deposito_inicial = mean(deposito_inicial),
            prom_tasa_degradacion = mean(tasa_degradacion))
