suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(stringr))
suppressMessages(library(data.table))
suppressMessages(library(gdata))
suppressMessages(library(rdevs))
suppressMessages(library(randomForest))
suppressMessages(library(ada))

# ########### Entrenamiento Modelo 1
source("./basic_functions.R")
paths <- readRDS("./paths.rds")


modelName <- "modelo_x"
modelCode <- "gbm"
total_vars_modelo <- 12


data_input <- paste0("data_", modelName)
input_ranking <- paste0("ranking_", modelName)
output_modelo <- paste(modelName, modelCode, total_vars_modelo, "variables", sep="_")

message("Cargando datos y ranking")
data <- readRDS(file.path(paths[, "data"], paste0(data_input, ".rds")))
rf_ranking <- readRDS(file.path(paths[,"multivariadoRDS"],paste0(input_ranking,".rds")))


data <- char_to_factor(data)

set.seed(1)
#Conservar un 70% de datos para el proceso de entrenamiento y dejar 30% para tests futuros
muestra_modelamiento <- sample(1:dim(data)[1],0.7*dim(data)[1],rep=F)
datamod1 <- data[muestra_modelamiento, ]
datos_sin_ocupar <- readRDS(file.path(paths[,"datos_run_models"], "datos_sin_ocupar.rds"))

datamod1 <- na.roughfix(datamod1)
datamod1$DESERCION <- factor(datamod1$DESERCION)
levels(datamod1$DESERCION) <- c("F", "T")

muestra_entrenamiento <- sample(1:dim(datamod1)[1],0.7*dim(datamod1)[1],rep=F)

training <- readRDS(file.path(paths[,"datos_run_models"], "training.rds"))
testing <- readRDS(file.path(paths[,"datos_run_models"], "testing.rds"))

ordervars_m1 <- rf_ranking[1:30,1]

message("Entrenando Modelo Seleccionado")

f <- as.formula(paste("DESERCION", paste(ordervars_m1[1:total_vars_modelo], collapse="+"), sep="~"))

fitter <- model_fitter(modelCode)
modelo1 <- fitter(f, training)

saveRDS(modelo1, file=file.path(paths[,"modelos"], paste0(output_modelo,".rds")))


#### Para testear con los datos que se dejaron sin participar en el proceso
# Si se desea utilizar otro set de prueba, se debe reemplazar "datos_sin_ocupar"
# por "testing", "training", o algun otro sample de "data"

datos_sin_ocupar <- na.roughfix(datos_sin_ocupar)
datos_sin_ocupar$DESERCION <- factor(datos_sin_ocupar$DESERCION)
levels(datos_sin_ocupar$DESERCION) <- c("F", "T")

data_output <- datos_sin_ocupar
data_output$prob <- predict(modelo1, newdata = datos_sin_ocupar, type="prob")[,2]
# jpeg("../BOXPLOT.jpg")
# boxplot(data_output$prob)
# dev.off()
datos_sin_ocupar$PROBABILIDAD_DESERCION <- data_output$prob
datos_sin_ocupar$DECIL_RIESGO_INSTITUCION <- cut(data_output$prob, 
    breaks=unique(quantile(data_output$prob, probs=0:10/10)),
    labels=FALSE, 
    include.lowest=TRUE)
datos_sin_ocupar$DESERCION_REAL <- ifelse(datos_sin_ocupar$DESERCION == "F", 0, 1)

goodness_of_fit <- datos_sin_ocupar[, c(
        "DECIL_RIESGO_INSTITUCION",
        "PROBABILIDAD_DESERCION",
        "DESERCION_REAL"
    )]

goodness_of_fit <- goodness_of_fit %>% group_by(
        DECIL_RIESGO_INSTITUCION
    )%>% summarise(
        PROBABILIDAD = mean(PROBABILIDAD_DESERCION),
        REAL = mean(DESERCION_REAL)
    )

goodness_of_fit <- data.frame(goodness_of_fit)
message("#####  Bondad de ajuste  #####")
goodness_of_fit

write.table(goodness_of_fit, file.path(paths[, "modelos_resultados"], paste0("BONDAD_AJUSTE_ ", output_modelo, ".csv")), 
            dec = ",", sep = ";", col.names = TRUE, quote = FALSE, row.names = FALSE , na='')

write.table(datos_sin_ocupar, file.path(paths[, "modelos_resultados"], paste0(output_modelo, ".csv")), 
            dec = ",", sep = ";", col.names = TRUE, quote = FALSE, row.names = FALSE , na='')
