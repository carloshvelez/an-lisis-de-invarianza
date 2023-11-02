#cargar paquetes
packages <- c("readxl", "lavaan", "ggplot2", "semTools", "dplyr", "semPlot", "tidyr")
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}


#cargar archivo:

archivo <- "Base_final.csv"
folder <- "data"
df <- read.csv(file.path(folder,archivo), sep = ";")


#definir modelos
modelo_erq <- 'reevaluación =~ X.1.R + X.3.R + X.5.R + X.7.R + X.8.R + X.10.R
              supresión =~ X.2.S+X.4.S+X.6.S+X.9.S
              X.1.R~~X.3.R'
              

modelo_erq2 <- 'reevaluación =~ X.1.R + X.3.R + X.5.R + X.7.R + X.8.R + X.10.R
              supresión =~ X.2.S+X.4.S+X.6.S+X.9.S
              total =~ X.1.R + X.3.R + X.5.R + X.7.R + X.8.R + X.10.R + X.2.S+X.4.S+X.6.S+X.9.S'

#Correr CFA para modelos. 
fit <- cfa(modelo_erq2, data = df, estimator = "DWLS")

fitmeasures(fit)
standardizedsolution(fit)

summary(fit)

resid(fit)


fit <- cfa(modelo_erq, data = df, estimator = "DWLS", ordered = T)
summary(fit, fit.measures = TRUE)
fitmeasures(fit)
semPaths(fit, whatLabels = "std", rotation = 2, color = "grey")

#Correr CFA's para invarianza (ordenados)

fit_1 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo", ordered = T )
fit_2 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo", group.equal = "loadings", ordered = T)
fit_3 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo", group.equal = c("intercepts", "loadings"), ordered = T)
fit_4 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo", group.equal = c("intercepts", "loadings", "residuals"), ordered = T)
invarianza_erq_a <- compareFit(fit_1, fit_2, fit_3, fit_4, moreIndices = T)

summary(invarianza_erq)



#Correr CFA's para invarianza (No ordenados) por sexo
fit <- cfa(modelo_erq, data = df, estimator = "DWLS")
summary(fit, fit.measures = TRUE)
fitmeasures(fit)
semPaths(fit, whatLabels = "std", rotation = 2, color = "grey")

fit_1 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo")
fit_2 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo", group.equal = "loadings")
fit_3 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo", group.equal = c("intercepts", "loadings"))
fit_4 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "Sexo", group.equal = c("intercepts", "loadings", "residuals"))
invarianza_erq_b <- compareFit(fit_1, fit_2, fit_3, fit_4, moreIndices = T)

summary(invarianza_erq)


#Correr CFA's para invarianza (No ordenados) por estrato

fit_1 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "EstratoSocioeconómico")
fit_2 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "EstratoSocioeconómico", group.equal = "loadings")
fit_3 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "EstratoSocioeconómico", group.equal = c("intercepts", "loadings"))
fit_4 <- cfa(modelo_erq, data = df, estimator = "DWLS", group = "EstratoSocioeconómico", group.equal = c("intercepts", "loadings", "residuals"))
invarianza_erq_c <- compareFit(fit_1, fit_2, fit_3, fit_4, moreIndices = T)

summary(invarianza_erq)







