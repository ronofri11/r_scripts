
#These functions require some libraries to exist in global scope
#like 'stringr'

str_remove_tilde <- function (string){
  string <- gsub("Ä|Á","A",
                 gsub("Ë|É", "E", 
                      gsub("Ï|Í", "I",
                           gsub("Ö|Ó", "O", 
                                gsub("Ü|Ú", "U", string)))))
  string <- gsub("ä|á","a",
                 gsub("ë|é", "e", 
                      gsub("ï|í", "i",
                           gsub("ö|ó", "o", 
                                gsub("ü|ú", "u", string)))))
  string <- gsub("Ñ", "N", gsub("ñ", "n", string))
  string
}

str_standar_colnames <- function(text){
  text <- str_trim(text)
  text <- camelcase_to_underscore(text)
  text <- str_remove_tilde(toupper(str_replace_all(text,"\\s+","_")))
  str_replace_all(text,"\\.+","_")
}

str_standar_df <- function(data){
  message("Estandarizando formato de los strings")
  colnames(data) <- str_standar_colnames(colnames(data))
  for(var in colnames(data)){
    if(is.character(data[[var]])){
      message(var)
      data[[var]]<-str_remove_tilde(toupper(str_trim(data[[var]])))
      data[[var]] <- str_replace_all(data[[var]],"\\s+"," ")
      data[[var]][data[[var]]==""] <- NA
    }
  }
  message("Estandarización de strings completa")
  data
}

group_factor_levels <- function(data, var, upper_limit=32){
    t <- table(data[, var])
    sorted_t <- sort(t, decreasing=TRUE)
    sorted_names_t <- names(sorted_t)
    aux_levels <- levels(data[, var])
    if(length(sorted_names_t) > upper_limit - 1){
        low_freq_names <- sorted_names_t[upper_limit:length(sorted_names_t)]
        # aux_levels[which(aux_levels%in%c("NULL"))] <- "S/I"
        aux_levels[which(aux_levels%in%low_freq_names)] <- "OTRA"

        # if(!(c("S/I")%in%aux_levels)){
        #   aux_levels <- c(aux_levels, "S/I")
        # }
    }
    aux_levels
}

trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

print_numerics <- function(data){
  message("Numeric variables")
  i <- 0
  invisible(sapply(colnames(data), function(col){
      if(is.numeric(data[,col])){
        print(col)
        i <<- i + 1
      }
    }))
  i
}

print_characters <- function(data){
  message("Character variables")
  i <- 0
  invisible(sapply(colnames(data), function(col){
      if(is.character(data[,col])){
        print(col)
        i <<- i + 1
      }
    }))
  i
}

print_factors <- function(data){
  message("Factor variables")
  i <- 0
  invisible(sapply(colnames(data), function(col){
      if(is.factor(data[,col])){
        print(col)
        i <<- i + 1
      }
    }))
  i
}

camelcase_to_underscore <- function(text){
  gsub("([a-z])([A-Z])", "\\1_\\L\\2", text, perl = TRUE)
}

char_to_factor <- function(df) { data.frame(lapply(df, function (v) { if (is.character(v)) factor(v) else v })) }

all_to_factor <- function(data){
  invisible(sapply(colnames(data), function(col){
      data[,col] <<- factor(data[,col])
    }))
}

cast_to_numerics <- function(data, varlist){
  message("Convirtiendo variables a numeric")
  daux <- data
  for(var in varlist){
    message(var)
    daux[, var] <- as.numeric(daux[, var])
  }
  daux
}

replace_values <- function(data, varlist, oldValue, newValue){
  daux <- data
  for(var in varlist){
    daux[daux[, var] == oldValue, var] <- newValue
  }
  daux
}


factor_many_levels <- function(data, varlist, upper_limit=32){
    for(var in varlist){
      if(is.factor(data[, var])){
        if(length(levels(data[, var])) > upper_limit){
          message(paste("var: ",var))
        }
      }
    }
}

non_factor_nor_numeric <- function(data){
  message("Columnas que no son factor ni numeric")
  invisible(sapply(colnames(data), function(x){
    if(!is.factor(data[, x]) & !is.numeric(data[, x])){
      print(x)
      data[, x] <<- as.factor(as.character(data[, x]))
    }
  }))
}

#Filtro de columnas con NA
has_na <- function(data){
  message("Columnas con NA")
  invisible(sapply(colnames(data), function(x){
    if(length(data[is.na(data[,x]), x]) > 0 ){
      print(x)
    }
  }))
}

pred_ranking_rrf <- function (data, response.name, pred.names = setdiff(names(data), response.name), ...){
  message(paste("Proceso Iniciado en:", Sys.time()))
  formula <- as.formula(paste(response.name, paste(pred.names, collapse = "+"), sep = " ~ "))
  daux <- subset(data, select = c(response.name, pred.names))
  message("STEP 1: Convirtiendo Strings a Factores y agregando corrección del missing...")
  daux <- char2factor(daux)
  daux <- na.roughfix(daux)
  daux[[response.name]] <- factor(daux[[response.name]])
  message("STEP 2: Cargando modelo RRF...")
  rrf <- RRF(formula, data = daux, ...)
  message("STEP 3: Estimando importancia de las variables...")
  imp <- data.frame(variable = rownames(RRF::importance(rrf)),mdg = as.numeric((RRF::importance(rrf))))
  imp <- imp[order(imp$mdg, decreasing = TRUE), ]
  message(paste("Proceso Acabado en:", Sys.time()))
  imp
}

df_ranking <- function(var1,var2,var3){
  #Ejemplo: var1-Cohorte, var2-carrera, var3-puntaje
  data.aux <- data.frame(v1=var1,v2=var2,v3=var3)
  ecdfs <- dlply(data.aux, .(v1, v2), function(df){
    ecdf(subset(data.aux, v1==v1 & v2 == v2)$v3)
  })
  
  ranking<-apply(data.aux, 1, function(x){
    ecdfs[[paste(x[1:2],collapse=".")]](x[3])
  })  
  ranking
}

tcount <- function(data, var, response){
  total <- dim(data)[1]
  t <- data %>% group_by_(var, response) %>% summarise(n=n()) %>% mutate(
      percent=round(n/sum(n)*100, 2),
      total_percent= round(n/total*100, 2)
    )
  data.frame(t)
}

contingency_tables <- function(data, varlist, response, output){
  if(file.exists(output)){
    file.remove(output)
  }
  varlist <- setdiff(varlist, response)
  for(i in 1:length(varlist)){
    t <- tcount(data, varlist[i], response)
    suppressWarnings(write.table(t, output, sep=";",dec=",",row.names=FALSE, append=TRUE))
  }
}