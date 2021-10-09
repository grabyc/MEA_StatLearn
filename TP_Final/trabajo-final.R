ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","imager", "magrittr", "tidymodels", "kknn")
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

# Objetivo binario
imgMatrix <- as_tibble(cbind(unlist(binLabels),
                             matrix(unlist(imgList), nrow=length(imgList), byrow=TRUE)))

imgMatrix <- rename(imgMatrix, letra = V1) %>%
  mutate(letra = if_else(letra==1, "aes", "noaes"),
         letra = factor(letra, levels = c("aes", "noaes")))

# objetivo multiclase knn
imgMatrixMulti <- as_tibble(cbind(unlist(allLabels),
                             matrix(unlist(imgList), nrow=length(imgList), byrow=TRUE)))

imgMatrixMulti <- rename(imgMatrixMulti, letra = V1) %>% mutate(letra = factor(letra))


t <- as_tibble(t(imgMatrix[, -1]))
print(t)
t %<>%
  rowSums()

t <- as.data.frame(t)
t <- as_tibble(t)
t[t[1]==86]  

t %>% filter(t <= 85)
presente <- (t %>% mutate(p = t <= 85) %>% select(p))$p

redux_matrix <- imgMatrix[,as.vector(c(TRUE,presente)) ==TRUE]

##imgMatrix %<>% rename(letra = V1) %>%
##                mutate( letra = if_else(letra==1, "aes", "noaes"),
##                        letra = factor(letra, levels = "aes", "noaes"))

dim(imgMatrix)
dim(redux_matrix)
head(imgMatrix, 1)

# Seteamos semilla para futuras particiones de datos, etc., de aca en adelante
set.seed(1)

######################################################################
###
### 2) Analisis de los datos
###
######################################################################

# graph of allLabels and binLabels to see the distribution of training examples
g <- as.data.frame(allLabels) %>%
  ggplot( aes(x=allLabels)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9)
plot(g)

#posibles analisis

pca.m = prcomp(imgMatrix[,-1], scale=TRUE)  ###podemos reducir los vectores de imagenes a 2 dimensiones y graficar los puntos?

modelo_logistico <- glm(letra ~ ., data = redux_matrix, family=binomial(link='logit'))  ## family = "binomial")


######################################################################
###
### 3) Algoritmo de clasificacion, objetivo clasificacion binaria en letra 'a' u otras.
###
######################################################################


# Division de datos de entrenamiento y testeo

letras_split <- initial_split(imgMatrix, strata = letra)
letras_train <- training(letras_split)
letras_test <- testing(letras_split)

# Remuestreo para seleccion de modelos.
# En nuestro caso puede ser muy util, ya que los datos son dificiles de graficar de ante-mano.
# Con la validacion podemos estimar el error de testeo y comparar modelos.

#### TODO:
#### separar datos de entrenamiento en validacion?? puedo usar los datos de validacion para comprar varios modelos.
#### el modelo que mejor funcione en el subconjunto de validacion es el que se elije y luego evalua en los datos de testeo.
#### aqui podemos hacer validacion simple.


fitted_logistic_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(letra ~., data = letras_train)

# Visualisar coeficientes

tidy(fitted_logistic_model) %>% filter(!is.na(estimate))

# Visualisar coeficientes y estadisticas (calculando odds con exponentiate)

tidy(fitted_logistic_model, exponentiate=TRUE) %>% filter(!is.na(estimate))
# NAs aparecen cuando la variable es linealmente dependiente de las otras
# Este thread tiene un ejemplo muy similar a nuestro caso con multiples variables explicativas y logistic regresion:
# https://community.rstudio.com/t/multinomial-logistic-regression/47550/6


# visualisar filtrando coefs no significativos %>% filter(p.value < 0.5)



classif <- predict(fitted_logistic_model, letras_test, type="class")
# una prueba que se puede hacer es correr la predicion en el conjunto de entrenamiento, si esto da mal quiere decir que el algoritmo no esta aprendiendo!!!

## podemos tambien pedir las probabilidadees type="probs"


# preparar marco de datos () para hacer evaluaciones

# matriz de confucion, accuracy, precicion, f_score, kappa
# cuvas roc auc no se si tengan mucho sentido despues de ver que los estadisticos son malos
# kappa puede ser interesante xq compara las predicciones del modelo contra prediciones al azhar, es el modelo peor que al azahar??



### KNN

muestra <- sample(1:86, 65)
train <- imgMatrix[muestra,]
test <- imgMatrix[-muestra,]

model <- train.kknn(letra ~ . , data = train[,1:1000], kmax=7) # esta funcion usa cross-validation para elegit el k
model    #visualise model details, this selects k=1


# ver como clasifica datos de entrenamiento
classif_train <- predict(model, train[,1:1000])
classif_train
tt <- table(bind_cols(train[,1:1], classif_train)) # matriz de confucion
tt #visualiza

accuracy <- (sum(diag(tt)))/sum(tt)
accuracy  ## 70% deberia ser mejor en el conjunto de testeo....

# ver como clasifica datos de testeo
classif_test <- predict(model, test[,1:1000])
classif_test
tt <- table(bind_cols(test[,1:1], classif_test)) # matriz de confucion
tt #visualiza
accuracy <- (sum(diag(tt)))/sum(tt)
accuracy  
## Esta accuracy es de 81% pero no es buena en realidad, clasifico todas las noaes
## correctamente pero las aes las hizo todas mal! Es un problema de desbalance de datos?


#####
##### Clasificacion multiclase
#####


muestra <- sample(1:86, 65)
train <- imgMatrixMulti[muestra,]
test <- imgMatrixMulti[-muestra,]

model <- train.kknn(letra ~ . , data = train[,1:1000], kmax=7)

head(imgMatrixMulti, 1)
