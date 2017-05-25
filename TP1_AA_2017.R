# Universidad de Buenos Aires. 
# Maestria en Explotacion de Datos y Descubrimiento de la Información
# Aprendizaje Automático
# Trabajo Práctivo Nro 1
# Alumnos:  Ferrari, Mauricio
#           Flecha, Rubén
#           Gil, Myriam

# Elimino todo lo que haya en memoria
rm(list=ls()) 
# Defino el directoria de trabajo
setwd("C:/Users/e_flecha/Documents/GitHub/TP1AA2017") 
# C:\Users\e_flecha\Documents\GitHub\TP1AA2017
# C:\Users\e_flecha\Desktop\TPDM2017

#Confirmo mi directorio de trabajo
getwd()

# Importo librerías a utilizar
if(!require("sqldf")) install.packages("sqldf")
library(sqldf)

if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if(!require("RgoogleMaps")) install.packages("RgoogleMaps")
library(RgoogleMaps)

if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

if(!require("dtplyr")) install.packages("dtplyr")
library(dtplyr)

if(!require("grid")) install.packages("grid")
library(grid)

if(!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)



# Carga de datos version csv
message("Cargando datos...")
datos_orig  <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition_V0.csv", sep = ",", header = T)
datos_train <- read.csv("WA_Fn-UseC_-_TRAIN.csv", sep = ",", header = T)
datos_test  <- read.csv("WA_Fn-UseC_-_TEST.csv", sep = ",", header = T)
message("Carga de datos --> LISTO!")


# Análisis Exploratorio de datos
str(datos_orig)
head(datos_orig)

hist(datos_orig$Attrition)
plot(datos_orig$Attrition)

# Gráficos de torta de los set para comparar distribucion de clase
pie1 <- ggplot(datos_orig, aes(x = factor(2), fill = datos_orig$Attrition)) +
  geom_bar(width=1) + 
  coord_polar(theta = "y")
pie2 <- ggplot(datos_train, aes(x = factor(2), fill = datos_train$Attrition)) +
  geom_bar(width=1) + 
  coord_polar(theta = "y")
pie3 <- ggplot(datos_test, aes(x = factor(2), fill = datos_test$Attrition)) +
  geom_bar(width=1) + 
  coord_polar(theta = "y")

# Funcion multiplot que permite graficar 2 o mas gráficos en uno
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Los 3 gráficos de torta en uno solo
multiplot(pie1, pie2, pie3, cols = 3)

# Cálculo numérico de las clases en los sets de datos original, train y test
tot_orig  <- nrow(datos_orig)
yes_orig  <- nrow(filter(datos_orig, datos_orig$Attrition == 'Yes'))
no_orig   <- nrow(filter(datos_orig, datos_orig$Attrition == 'No'))
datos_orig_perc_y <- (yes_orig*100)/tot_orig
datos_orig_perc_n <- (no_orig*100)/tot_orig

tot_train  <- nrow(datos_train)
yes_train  <- nrow(filter(datos_train, datos_train$Attrition == 'Yes'))
no_train   <- nrow(filter(datos_train, datos_train$Attrition == 'No'))
datos_train_perc_y <- (yes_train*100)/tot_train
datos_train_perc_n <- (no_train*100)/tot_train

tot_test  <- nrow(datos_test)
yes_test  <- nrow(filter(datos_test, datos_test$Attrition == 'Yes'))
no_test   <- nrow(filter(datos_test, datos_test$Attrition == 'No'))
datos_test_perc_y <- (yes_test*100)/tot_test
datos_test_perc_n <- (no_test*100)/tot_test

message("Distribucion Datos Set original")
datos_orig_perc_y
datos_orig_perc_n
message("Distribucion Datos TRAIN")
datos_train_perc_y
datos_train_perc_n
message("Distribucion Datos TEST")
datos_test_perc_y
datos_test_perc_n

#-------------------------------------------------------------------


