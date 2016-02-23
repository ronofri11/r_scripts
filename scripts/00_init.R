mainDir <- file.path(getwd())

paths <- data.frame(
    plots = "../plots",
    presentacion = "../plots/presentacion",
    univariado = "../rankings",
    multivariado = "../rankings/multivariado",
    multivariadoRDS = "../rankings/multivariado/rds",
    modelos = "../modelos",
    candidatos = "../modelos/seleccion",
    data = "../data",
    datos_run_models = "../data/datos_run_models",
    dict = "../dict"
)

message("Creando directorios de trabajo")

for(col in colnames(paths)){
    folder <- paths[, col]
    path <- file.path(mainDir, folder)
    if(!file.exists(path)){
        message(paste("Creando:", folder))
        dir.create(path)
    }
    else{
        message(paste("Directorio ya existe:", folder))
    }
}

saveRDS(paths, file.path(getwd(), "paths.rds"))
