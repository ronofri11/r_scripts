##### Modelamiento  ######

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

source("./basic_functions.R")
paths <- readRDS("./paths.rds")

###################################################

#modelo x
modelName <- "modelo_x"

max_variables <- 30
models <- c("rf", "ctree","LogitBoost","ada", "gbm", "nnet")

###################################################
data_input <- paste0("data_", modelName)
ranking_input <- paste0("ranking_", modelName)
output <- paste0("plot_clasificadores_", modelName)

message("Cargando datos y ranking multivariado")
data <- readRDS(file.path(paths[,"data"],paste0(data_input, ".rds")))
rf_rankmod1 <- readRDS(file.path(paths[,"multivariadoRDS"],paste0(ranking_input,".rds")))

data <- char_to_factor(data)

message("Definiendo muestra para entrenamiento y testing")

set.seed(1)
#Conservar un 70% de datos para el proceso de entrenamiento y dejar 30% para tests futuros
muestra_modelamiento <- sample(1:dim(data)[1],0.7*dim(data)[1],rep=F)
datamod1 <- data[muestra_modelamiento, ]
datos_sin_ocupar <- data[-muestra_modelamiento, ]

ordervars_m1 <- rf_rankmod1[1:max_variables, 1]

datamod1 <- na.roughfix(datamod1)
datamod1$DESERCION <- factor(datamod1$DESERCION)
levels(datamod1$DESERCION) <- c("F", "T")

saveRDS(datamod1, file=file.path(paths[,"datos_run_models"], "datos_run_models.rds"))
saveRDS(datos_sin_ocupar, file=file.path(paths[,"datos_run_models"], "datos_sin_ocupar.rds"))

muestra_entrenamiento <- sample(1:dim(datamod1)[1],0.7*dim(datamod1)[1],rep=F)

training <- datamod1[muestra_entrenamiento, ]
testing <- datamod1[-muestra_entrenamiento, ]

saveRDS(training, file=file.path(paths[,"datos_run_models"], "training.rds"))
saveRDS(testing, file=file.path(paths[,"datos_run_models"], "testing.rds"))

message("total registros training y testing")
print(dim(training))
print(dim(testing))

message("Entrenando candidatos a modelo 1")
set.seed(1)
# models <- c("rf","ada", "plr", "LogitBoost")
model1 <- run_models(training, testing, response.name = "DESERCION",pred.names = ordervars_m1,
                     models=models)

message("Guardando candidatos modelo 1")
saveRDS(model1, file=file.path(paths[, "candidatos"], "candidatos_modelo_1.rds"))

res <- model1

model_name_orig <- c("ada","bayesglm","ctree","gbm","knn","LogitBoost","nnet","plr","rf","rl","knn")
model_name_new <- c("Adaboost","Bayesian GLM","Decision Tree","Stochastic Gradient Boosting","K-Nearest Neighbor Algorithm","LogitBoost","Neural Nets","Penalized Logistic Regression","Random Forest","Logistic Regression","K-nearest Neighbour")

for(ind in 1:length(model_name_new)){
  res$model[res$model==model_name_orig[ind]]=model_name_new[ind]
}
message("Preparando plot comparativo entre modelos")
p <- ggplot(res) +
  geom_point(aes(x=namevar, y=aucroc, color=model)) +
  geom_line(aes(x=namevar, y=aucroc, color=model, group=model), size = 1.5) +
  scale_color_manual(values=c('#7cb5ec',  '#90ed7d', '#f7a35c', '#8085e9', 
                              '#f15c80', '#e4d354','#434348', 'darkred', '#91e8e1',"cyan","blue"))+
  scale_y_continuous("Área bajo curva ROC", labels = percent_format()) +
  geom_hline(aes(yintercept=.5), size = 0.8, colour = "red") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("") + ggtitle("")
p <- p + theme_hc() + theme(legend.position = "top")+theme(legend.text=element_text(size=10),axis.text.y=element_text(size=10))
p

pdf(file = file.path(paths[,"plots"], paste0(output ,".pdf")),width = 12,height = 8)
print(p)
dev.off()
