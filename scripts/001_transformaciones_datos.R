suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(rdevs))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(foreign))

rm(list=ls())

source("./basic_functions.R")

filename <- "../IPCHILE_MODELO_NUEVOS.sav"

data <- read.spss(file=filename, use.value.labels=FALSE, to.data.frame=TRUE)

data$DESERCION <- data$Marca_Desercion
data$DESERCION_REINCORPORACION <- data$Marca_Desercion_Reincorporacion
data$Marca_Desercion <- NULL
data$Marca_Desercion_Reincorporacion <- NULL

data$DESERCION_ALTERNATIVA <- ifelse(data$MATRICULA_PERIODO_SIGUIENTE == "NO",1,0)

excluded_vars <- c(
	"rut",
	"rut_md5",
	"alumnoCodigoCarrera",
	"colegioCodigo",
	"colegioNombre",
	"alumnoFechaMatricula"
)
contingency_tables(data, setdiff(colnames(data), excluded_vars), "DESERCION", "../NUEVOS_TABLAS_TODAS_LAS_VARIABLES.csv")
contingency_tables(data, setdiff(colnames(data), excluded_vars), "DESERCION_REINCORPORACION", "../NUEVOS_TABLAS_TODAS_LAS_VARIABLES_REINCORPORACION.csv")
contingency_tables(data, setdiff(colnames(data), excluded_vars), "DESERCION_ALTERNATIVA", "../NUEVOS_TABLAS_TODAS_LAS_VARIABLES_ALTERNATIVA.csv")