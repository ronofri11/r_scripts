
suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(rdevs))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(randomForest))
suppressMessages(library(RRF))

source("./basic_functions.R")

paths <- readRDS("./paths.rds")

##############################################################

# modelo x
modelName <- "modelo_x"


input <- paste0("data_", modelName)
output <- paste0("ranking_", modelName)

##############################################################

data <- readRDS(file.path(paths[, "data"], paste0(input, ".rds")))

data <- char_to_factor(data)

# imprime variables que no se encuentran como factor ni numeric
non_factor_nor_numeric(data)
# imprime variables que contienen NA
has_na(data)

# imprime factors con mas de 32 categorias
factor_many_levels(data, colnames(data), 32)

set.seed(1)
# modificar response.name de acuerdo al nombre de la marca de desercion
rf_ranking_modelo_1 <- pred_ranking_rrf(data, response.name = "DESERCION")

saveRDS(rf_ranking_modelo_1, file=file.path(paths[,"multivariadoRDS"], paste0(output, ".rds")))
write.table(rf_ranking_modelo_1, file.path(paths[, "multivariado"], paste0(output, ".csv")), sep=";", dec=",", row.names=FALSE)
