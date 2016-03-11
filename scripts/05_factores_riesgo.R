suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(caret))
suppressMessages(library(rdevs))
suppressMessages(library(randomForest))
suppressMessages(library(ada))
suppressMessages(library(party))

source("./basic_functions.R")
paths <- readRDS("./paths.rds")

modelName <- "modelo_x"
modelCode <- "gbm"
total_vars_modelo <- 12


excluded_vars <- c(

)

input_modelo <- paste(modelName, modelCode, total_vars_modelo, "variables", sep="_")
input_ranking <- paste0("ranking_", modelName)
# input_training <- paste0("training_", input_modelo)
##### Carga de datos #####
datamod1 <- readRDS(file.path(paths[,"datos_run_models"], "training.rds"))
rf_ranking <- readRDS(file.path(paths[,"multivariadoRDS"],paste0(input_ranking,".rds")))
modelo1 <- readRDS(file.path(paths[, "modelos"], paste0(input_modelo, ".rds")))

datamod1$prob <- predict(modelo1, newdata = datamod1, type="prob")[,2]

vars <- rf_ranking[1:total_vars_modelo, 1]

variables <- vars[which(!vars%in%excluded_vars)]

tr.control <- ctree_control(maxdepth=5)
ft1 <- as.formula(paste("prob", paste(variables, collapse="+"), sep="~"))
tree1 <- ctree(formula = ft1, data = datamod1, controls = tr.control)

capture.output(tree1, file=file.path(paths[, "factores_riesgo"],paste0("factor_riesgo_tree_", input_modelo,".txt")))
pdf(file=file.path(paths[, "factores_riesgo"], paste0("plot_factor_riesgo_tree_", input_modelo, ".pdf")), width = 70, height = 30)
tree_par <- list(type="extended",
                 inner_panel = node_inner(tree1, abbreviate = F, pval = FALSE, id = T, fill="white"),
                 terminal_panel = node_boxplot(tree1, col = "black",fill="#9400D350"))
plot(tree1, type = tree_par$type , inner_panel = tree_par$inner_panel, terminal_panel = tree_par$terminal_panel)
dev.off()
