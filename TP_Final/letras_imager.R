ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","imager", "magrittr", "tidymodels")
ipak(packages)

######################################################################
###
### 1) Preproc
###     - cargar imagenes  -- type: cimg [x,y,z,c]
###     - uniformar y reducir tamaño de imgs -- [100,100,z,c]
###     - eliminar canales de color (grayscale) -- [x,y,1,1]
###     - generar tags de letras a partir de nombres de archivos 
###     - armar matriz de trabajo [img, v1, ..., v_i, ... , v_(x*y)]
###
######################################################################
imgList <- load.dir("./letras/letras") %>%
  map(resize, 100, 100) %>%
  map(function(x) as.cimg(as.vector(round(x, digits = 0)))) %>%
  map(grayscale)

plot(imgList[[1]])

# leer clases/etiquetas a partir de los nombre de archivos

##getLabelName <- function(input) {
##  ret <- str_split(input, "/")
##  str_sub(ret[[1]][length(ret[[1]])], 1, 1)
##}

getBinaryLabel <- function(input) {
  ret <- str_split(input, "/")
  ifelse(str_sub(ret[[1]][length(ret[[1]])], 1, 1) == "a", 1, 2 )
}

##allLabels <- names(imgList) %>% map(getLabelName) %>% unlist()
binLabels <- names(imgList) %>% map(getBinaryLabel) 

imgMatrix <- as_tibble(cbind(unlist(binLabels), 
                             matrix(unlist(imgList), nrow=length(imgList), byrow=TRUE)))

imgMatrix <- rename(imgMatrix, letra = V1) %>% 
  mutate(letra = if_else(letra==1, "aes", "noaes"),
         letra = factor(letra, levels = c("aes", "noaes")))

##imgMatrix %<>% rename(letra = V1) %>% 
##                mutate( letra = if_else(letra==1, "aes", "noaes"),
##                        letra = factor(letra, levels = "aes", "noaes"))

dim(imgMatrix)
head(imgMatrix, 1)


######################################################################
###
### 2) Analisis de los datos
###
######################################################################

# graph of allLabels and binLabels to see the distribution of training examples

#posibles analisis

##TODO: no pude hacerlo correr
pca.m = prcomp(imgMatrix[,-1], scale=TRUE)

modelo_logistico <- glm(letra ~ ., data = imgMatrix, family = "binomial")


######################################################################
###
### 3) Algoritmo de clasificacion
###
######################################################################

letras_split <- initial_split(imgMatrix, strata = letra)
letras_train <- training(letras_split)
letras_test <- testing(letras_split)
letras_kfold <- vfold_cv(letras_train, v = 5)

fitted_logistic_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(letra ~., data = letras_train)

