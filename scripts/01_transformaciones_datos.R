suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(rdevs))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))

rm(list=ls())
options(stringsAsFactors=FALSE)

source("./basic_functions.R")
paths <- readRDS("./paths.rds")

# Se requiere data_proc creado a partir de una fuente como un csv, o una base de datos
input <- "data_proc"

modelName <- "modelo_x"


output <- paste0("data_", modelName)

data <- readRDS(file.path(paths[, "data"], paste0(input, ".rds")))

# Listar variables que se eliminaran
excluded_vars <- c(

)

# Eliminar variables
data <- data[, setdiff(colnames(data), excluded_vars)]

# Cambiar a mayusculas los nombres de las columnas y modificar camelcase a underscore.
# Estandarizar de forma sencilla el contenido de cada variable.

#### A partir de este punto los nombres de las variables pueden haber sido alterados
#### y deberan ser considerados los nuevos nombres para continuar realizando modificaciones
data <- str_standar_df(data)


# Listar variables para ser ingresadas como numeric
numeric_vars <- c(

)

# Hacer cast a numeric de las variables seleccionadas
data <- cast_to_numerics(data, numeric_vars)

saveRDS(data, file.path(paths[, "data"], paste0(output, ".rds")))

