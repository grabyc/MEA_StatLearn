---
title: "TP Preprocesamiento con Tidyverse"
subtitle: "Aprendizaje Estadístico"
author: "Laura Pérez - Gabriel Raby"
date: "9/2/2021"
output:
html_document:
df_print: paged
---

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Cargar los paquetes que vamos a utilizar
```{r}
packages <- c("tidyverse", "knitr", "hrbrthemes", "magrittr", "lubridate", 
              "ggplot2", "stringr", "fs", "magick", "tidymodels", "vip", 
              "rpart.plot", "imager")
ipak(packages)
```
######################################################################
###
### 1) Preproc
###     - cargar imagenes  -- type: cimg [x,y,z,c]
###     - uniformar tamaño de imgs -- [200,200,z,c]
###     - eliminar canales de color -- [x,y,1,1]
###     - armar matriz de trabajo [img_label, v1, ..., v_i, ... , v_(x*y)]
###
######################################################################

```{r}
imgList <- load.dir("./letras/letras") %>%
  map(resize, 100, 100) %>%
  map(function(x) as.cimg(as.vector(round(x, digits = 0)))) %>%
  map(grayscale)

plot(imgList[[1]])

# leer clases/etiquetas a partir de los nombre de archivos
getLabelName <- function(input) {
  ret <- str_split(input, "/")
  str_sub(ret[[1]][length(ret[[1]])], 1, 1)
}

getBinaryLabel <- function(input) {
  ret <- str_split(input, "/")
  ifelse(str_sub(ret[[1]][length(ret[[1]])], 1, 1) == "a", 1, 2 )
}

allLabels <- names(imgList) %>% map(getLabelName) %>% unlist()
binLabels <- names(imgList) %>% map(getBinaryLabel)
imgMatrix <- as_tibble(cbind(unlist(binLabels), 
                             matrix(unlist(imgList), nrow=length(imgList), byrow=TRUE)))
imgMatrix <- rename(imgMatrix, letra = V1) %>% 
        mutate(letra = if_else(letra==1, "aes", "noaes"),
          letra = factor(letra, levels = "aes", "noaes"))
dim(imgMatrix)
head(imgMatrix, 1)

```

######################################################################
###
### 1) Analisis de los datos
###
######################################################################

# graph of allLabels and binLabels to see the distribution of training examples

g <- as.data.frame(allLabels) %>%
  ggplot( aes(x=allLabels)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9)

#posibles analisis

pca.m = prcomp(imgMatrix[,-1], scale=TRUE)

modelo_logistico <- glm(letra ~ ., data = imgMatrix, family = "binomial")


######################################################################
###
### 1) Algoritmo de clasificacion
###
######################################################################

letras_split <- initial_split(imgMatrix, strata = letra)
letras_train <- training(letras_split)
letras_test <- testing(letras_split)
letras_kfold <- vfold_cv(letras_train, v = 5)

fitted_logistic_model <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification") %>%
    fit(letras ~., data = letras_train)

