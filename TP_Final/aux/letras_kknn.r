ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","imager", "dplyr", "magrittr", "kknn")
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
  map(resize, 200, 200) %>%
  map(function(x) as.cimg(as.vector(round(x, digits = 0)))) %>%
  map(grayscale)

imgMatrix <- as_tibble(cbind(names(imgList), matrix(unlist(imgList), nrow=length(imgList), byrow=TRUE)))

letters_lvls <- c("a", "b", "c", "e", "i", "o", "u")

imgMatrix %<>%
  mutate_all(type.convert) %>%
  mutate(V1 = str_extract(string = V1, pattern = "[^-]*(?=\\.)")) %>%
  mutate(V1 = factor(V1, levels = letters_lvls)) %>%
  rename(letra = V1)

glimpse(imgMatrix)


set.seed(2020)
muestra <- sample(1:86, 65)
training <- imgMatrix[muestra,]
test <- imgMatrix[-muestra,]

model <- train.kknn(letra ~ ., data = training[,1:1000], kmax = 9) 

######################################################################
###
### train.kknn function
###
######################################################################
formula = V1 ~ .
data = training[,1:1000]
kmax = 11
ks = NULL
distance = 2
kernel = "optimal"
ykernel = NULL
scale = TRUE
contrasts = c(unordered = "contr.dummy",ordered = "contr.ordinal")

  if (is.null(ykernel)) 
    ykernel = 0

  optKernel <- function(k, d=1){
    1/k*(1 + d/2 - d/(2*k^(2/d)) * ( (1:k)^(1+2/d) - (0:(k-1))^(1+2/d)  ))
  }
  weight.y = function(l = 1, diff = 0) {
    k = diff + 1
    result = matrix(0, l, l)
    diag(result) = k
    for (i in 1:(k - 1)) {
      for (j in 1:(l - i)) {
        result[j, j + i] = k - i
        result[j + i, j] = k - i
      }
    }
    result
  }

  kernel <- match.arg(kernel, c("rectangular", "triangular", 
                                "epanechnikov", "biweight", "triweight", "cos", "inv", 
                                "gaussian", "rank", "optimal"), TRUE)
  if (is.null(ks)) {
    ks <- 1:kmax
    nk <- kmax
  } else {
    ks <- sort(ks)
    nk <- length(ks)
    kmax <- max(ks)
  }
  call <- match.call()
  mf <- model.frame(formula, data = data)
  mt <- attr(mf, "terms")
  y <- model.response(mf)
  cl <- model.response(mf)
  old.contrasts <- getOption("contrasts")
  options(contrasts = contrasts)
  mm.data <- model.matrix(mt, mf)
  d <- sum(attr(mt, "order"))
  r <- length(kernel)
  MISCLASS <- matrix(nrow = nk, ncol = r, dimnames = list(ks, kernel))
  MEAN.ABS <- matrix(nrow = nk, ncol = r, dimnames = list(ks, kernel))
  MEAN.SQU <- matrix(nrow = nk, ncol = r, dimnames = list(ks, kernel))
  P <- list(nk * r)
  m <- dim(mm.data)[1]
  q <- dim(mm.data)[2]
  p <- m
  ind <- attributes(mm.data)$assign
  d.sd <- numeric(length(ind)) + 1
  we <- numeric(length(ind)) + 1
  d.sd = apply(mm.data, 2, stats::var)
  for (i in unique(ind)) {
    d.sd[ind == i] = sqrt(mean(d.sd[ind == i]))
    we[ind == i] = 1/sum(ind == i)
  }
  we[d.sd == 0] = 0
  d.sd[d.sd == 0] = 1
  if (scale) 
    mm.data <- sweep(mm.data, 2L, d.sd, "/", check.margin = FALSE)
  ord = order(we * apply(mm.data, 2, sd), decreasing = TRUE)
  mm.data <- mm.data[, ord, drop = FALSE]
  we <- we[ord]
  Euclid <- FALSE
  if (distance == 2) 
    Euclid <- TRUE
  if (Euclid) {
    dmtmp <- .C("dmEuclid", as.double(mm.data), as.double(mm.data), 
                as.integer(m), as.integer(p), as.integer(q), dm = double((kmax + 2L) * p), 
                cl = integer((kmax + 2L) * p), k = as.integer(kmax + 2), 
                as.double(distance), as.double(we), PACKAGE = "kknn")
    } else {
    dmtmp <- .C("dm", as.double(mm.data), as.double(mm.data), 
                   as.integer(m), as.integer(p), as.integer(q), dm = double((kmax + 2L) * p), 
                   cl = integer((kmax + 2L) * p), k = as.integer(kmax + 2), 
                   as.double(distance), as.double(we), PACKAGE = "kknn")}
  D <- matrix(dmtmp$dm, nrow = p, ncol = kmax + 2)
  C <- matrix(dmtmp$cl, nrow = p, ncol = kmax + 2)
  C <- C + 1
  CL <- matrix(cl[C], nrow = p, ncol = kmax + 2)
  D <- D[, -1]
  C <- C[, -1]
  CL <- CL[, -1]
  if (is.ordered(y)) {
    response <- "ordinal"
    lev <- levels(y)
    l <- length(lev)
    weightClass <- matrix(0, m, l)
  }
  if (is.numeric(y)) {
    response <- "continuous"
    weightClass <- NULL
  }
  if (is.factor(y) & !is.ordered(y)) {
    response <- "nominal"
    lev <- levels(y)
    l <- length(lev)
    weightClass <- matrix(0, m, l)
  }
  for (k_i in 1:nk) {
    j <- ks[k_i]
    maxdist <- D[, j + 1]
    maxdist[maxdist < 1e-06] = 1e-06
    V <- D[, 1:j]/maxdist
    V <- pmin(V, 1 - (1e-06))
    V <- pmax(V, 1e-06)
    for (s in 1:r) {
      if (kernel[s] == "rank") 
        W <- (j + 1) - t(apply(as.matrix(V), 1, rank))
      if (kernel[s] == "inv") 
        W <- 1/V
      if (kernel[s] == "rectangular") 
        W <- matrix(1, nrow = m, ncol = j)
      if (kernel[s] == "triangular") 
        W <- 1 - V
      if (kernel[s] == "epanechnikov") 
        W <- 0.75 * (1 - V^2)
      if (kernel[s] == "biweight") 
        W <- dbeta((V + 1)/2, 3, 3)
      if (kernel[s] == "triweight") 
        W <- dbeta((V + 1)/2, 4, 4)
      if (kernel[s] == "cos") 
        W <- cos(V * pi/2)
      if (kernel[s] == "gaussian") {
        v <- j + 1
        alpha = 1/(2 * v)
        qua = abs(qnorm(alpha))
        W = V * qua
        W = apply(as.matrix(W), 2, dnorm)
      }
      if (kernel[s] == "optimal") {
        W = rep(optKernel(j, d), each = m)
      }
      W <- matrix(W, m, j)
      if (response != "continuous") {
        for (i in 1:l) {
          weightClass[, i] <- rowSums(W * (matrix(CL[, 1:j], m, j) == lev[i]))
        }
        weightClass <- weightClass/rowSums(weightClass)
        colnames(weightClass) <- lev
      }
      if (response == "ordinal") {
        blub = length(lev)
        weightClass = weightClass %*% weight.y(blub, ykernel)
        weightClass <- weightClass/rowSums(weightClass)
        weightClass <- t(apply(weightClass, 1, cumsum))
        colnames(weightClass) <- lev
        fit <- numeric(m)
        fit <- ((l + 1) - (weightClass >= 0.5) %*% (numeric(l) + 1))
        fit <- ordered(fit, levels = 1:l, labels = lev)
      }
      if (response == "nominal") {
        lwc = length(weightClass)
        fit <- apply(weightClass, 1, order, decreasing = TRUE)[1, ]
        fit <- factor(fit, levels = 1:l, labels = lev)
      }
      if (response == "continuous") {
        fit <- rowSums(W * (matrix(CL[, 1:j], m, j)))/pmax(rowSums(matrix(W, m, j)), 1e-06)
        weightClass = fit
      }
      attr(fit, "kernel") = kernel[s]
      attr(fit, "k") = j
      P[[k_i + (s - 1) * nk]] = fit
    }
  }
  for (k_i in 1:nk) {
    j <- ks[k_i]
    for (s in 1:r) {
      if (is.factor(y)) 
        MISCLASS[k_i, s] <- sum(y != P[[k_i + (s - 1) * 
                                          nk]])/m
      if (is.numeric(y) | is.ordered(y)) 
        MEAN.ABS[k_i, s] <- sum(abs(as.numeric(y) - 
                                      as.numeric(P[[k_i + (s - 1) * nk]])))/m
      if (is.numeric(y) | is.ordered(y)) 
        MEAN.SQU[k_i, s] <- sum((as.numeric(y) - as.numeric(P[[k_i + 
                                                                 (s - 1) * nk]]))^2)/m
    }
  }
  if (response == "nominal") 
    best <- which(MISCLASS == min(MISCLASS), arr.ind = TRUE)
  if (response == "ordinal") 
    best <- which(MEAN.ABS == min(MEAN.ABS), arr.ind = TRUE)
  if (response == "continuous") 
    best <- which(MEAN.SQU == min(MEAN.SQU), arr.ind = TRUE)
  best.parameters = list(kernel = kernel[best[1, 2]], k = ks[best[1, 
                                                                  1]])
  options(contrasts = old.contrasts)
  result = list(MISCLASS = MISCLASS, MEAN.ABS = MEAN.ABS, 
                MEAN.SQU = MEAN.SQU, fitted.values = P, best.parameters = best.parameters, 
                response = response, distance = distance, call = call, 
                terms = mt, data = data)
  class(result) = c("train.kknn", "kknn")
  result
