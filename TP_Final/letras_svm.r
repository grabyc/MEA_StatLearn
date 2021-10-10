ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","imager", "dplyr", "magrittr", "kernlab", "e1071")
ipak(packages)

######################################################################
###
### 1) Preproc
###     - cargar imagenes  -- type: cimg [x,y,z,c]
###     - uniformar tamaño de imgs -- [200,200,z,c]
###     - eliminar canales de color -- [x,y,1,1]
###     - armar matriz de trabajo [img, v1, ..., v_i, ... , v_(x*y)]
###
######################################################################
imgList <- load.dir("./letras2") %>%
  map(resize, 100, 100) %>%
  map(function(x) as.cimg(as.vector(round(x, digits = 0)))) %>%
  map(grayscale)

getBinaryLabel <- function(input) {
  ret <- str_split(input, "/")
  ifelse(str_sub(ret[[1]][length(ret[[1]])], 1, 1) == "a", 1, 2 )
}
binLabels <- names(imgList) %>% map(getBinaryLabel)

# Objetivo binario
imgMatrix <- as_tibble(cbind(unlist(binLabels),
                             matrix(unlist(imgList), nrow=length(imgList), byrow=TRUE)))

imgMatrix <- rename(imgMatrix, letra = V1) %>%
  mutate(letra = if_else(letra==1, "aes", "noaes"),
         letra = factor(letra, levels = c("aes", "noaes")))

# Division de datos de entrenamiento y testeo
letras_split <- initial_split(imgMatrix, strata = letra)
letras_train <- training(letras_split)
letras_test <- testing(letras_split)

names(letras_train)
kernfit <- ksvm(letra~., data=letras_train[,1:10])
kernfit <- ksvm(letra~., data=letras_train[,1:10], type = "C-svc", kernel = 'vanilladot', prob.model = FALSE)
plot(kernfit, data = letras_train)

##########################################################
##########################################################
## ejemplos svm (e1071) y ksvm (kernlab)
##########################################################
##########################################################

x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 3/2
dat <- data.frame(x=x, y=as.factor(y))

ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)
plot(svmfit, dat)

kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot')
plot(kernfit, data = x)


x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
dat <- data.frame(x=x, y=as.factor(y))

ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10)
plot(svmfit, dat)

kernfit <- ksvm(x,y, type = "C-svc", kernel = 'vanilladot', C = 100)
plot(kernfit, data = x)


tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
(bestmod <- tune.out$best.model)

ypred <- predict(bestmod, dat)
(misclass <- table(predict = ypred, truth = dat$y))
