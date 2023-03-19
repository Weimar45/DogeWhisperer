# ----- Descripción del Proyecto ----
#
# Aplicación en Shiny para el estudio de criptomonedas. 
# Fecha: 22/02/2022
# Última vez editado por: Alejandro Navas González
#

# ----- Zona General ----

# Carga de los paquetes a utilizar en la aplicación. 
library(DT)
library(grt)
library(xlsx)
library(rlang)
library(shiny)
library(knitr)
library(skimr)
library(fresh)
library(psych)
library(ggsci)
library(viridis)
library(magrittr)
library(PMCMRplus)
library(d3heatmap)
library(shinymeta)
library(tidyquant)
library(tidyverse)
library(ggcorrplot)
library(data.table)
library(shinyalert)
library(palettetown)
library(ggstatsplot)
library(googledrive)
library(RColorBrewer)
library(htmlwidgets)
library(berryFunctions)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)

# Paquetes relacionados con Python. 
library(reticulate)
library(tensorflow)
library(keras)


# Establecer un tema común para los gráficos.
theme_set(theme_light() + theme(text = element_text(size = 16, family = "serif")))

# Creación de un tema propio para la Dashboard (paquete fresh).
mytheme <- create_theme(
  
  adminlte_color(
    
    light_blue = "#434C5E"
    
  ),
  
  adminlte_sidebar(
    
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440",
    dark_submenu_color = "#2E3440"
    
  ),
  
  adminlte_global(
    
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
    
  )
)



# Carga del marco de datos. 
# Datos extraídos de: https://www.cryptodatadownload.com/data/binance/
# Función de carga de los marcos de datos sobre criptotransacciones. 
loadDrive <- function(id){
  
  temp <- tempfile(fileext = ".csv")
  
  downloadcsv <- drive_download(
    as_id(id), path = temp, overwrite = TRUE)
  
  crypto <- fread(downloadcsv$local_path, data.table = FALSE)
  
  return(crypto)
  
}

# Marcos de datos originales. 
cardano   <- fread("data/ADA.csv")
solana    <- fread("data/SOL.csv")
binance   <- fread("data/BNB.csv")
polkadot  <- fread("data/DOT.csv")
ethereum  <- fread("data/ETH.csv")
chainlink <- fread("data/LINK.csv")
litecoin  <- fread("data/LTC.csv")
uniswap   <- fread("data/UNI.csv")
ripple    <- fread("data/XRP.csv")
bitcoin   <- fread("data/BTC.csv")


# Marcos de datos con pocos días para hacer testeos rápidos. 
# Se establece conexión con la carpeta Drive de nuestro proyecto.
# Hay que dar permisos a la API desde nuestra cuenta de correo de la Universidad. 
# cardano   <- loadDrive("1rXjxW-RcpOpFFMxXd6tSgydAztyAQVNq")
# solana    <- loadDrive("16ohfVGLh1RGV_K8mRYyfqrSSqzQC3XOz")
# binance   <- loadDrive("13uDzLSxW_ToRjHapUev0t-RYvVdysIN9")
# polkadot  <- loadDrive("1qPL2wfdSSN4ntGxfwHPgM0PeHB3zSb20")
# ethereum  <- loadDrive("1ebw2wTZ7xC3H0ShG6mQn85XXE2O1Up0a")
# chainlink <- loadDrive("1UKvHL3-1qKkiHPYs3c5NGoZmNf8Rv7jF")
# litecoin  <- loadDrive("1CNMlgUYhG08PPLktoiGouGR7RYa2jQet")
# uniswap   <- loadDrive("1p2hwa_YlAo7mT-0vqMGsXo7q5dRz33tC")
# ripple    <- loadDrive("110ZYEkG8xmTdZikZco0E7eY5CxNTU8hv")
# bitcoin   <- loadDrive("1wvpVg0NLgaXuSQ2krB1o-kSNRUH8w0Iu")

# Cambio de los nombres para un único formato.
names(cardano) <- c("unix", "date", "symbol", "open", "high",
                    "low", "close", "volume", "USDT", "tradecount")
names(solana) <- c("unix", "date", "symbol", "open", "high",
                   "low", "close", "volume", "USDT", "tradecount")
names(binance) <- c("unix", "date", "symbol", "open", "high",
                    "low", "close", "volume", "USDT", "tradecount") 
names(polkadot) <- c("unix", "date", "symbol", "open", "high",
                     "low", "close", "volume", "USDT", "tradecount") 
names(ethereum) <- c("unix", "date", "symbol", "open", "high",
                     "low", "close", "volume", "USDT", "tradecount") 
names(chainlink) <- c("unix", "date", "symbol", "open", "high",
                      "low", "close", "volume", "USDT", "tradecount") 
names(litecoin) <- c("unix", "date", "symbol", "open", "high",
                     "low", "close", "volume", "USDT", "tradecount") 
names(uniswap) <- c("unix", "date", "symbol", "open", "high",
                    "low", "close", "volume", "USDT", "tradecount") 
names(ripple) <- c("unix", "date", "symbol", "open", "high",
                   "low", "close", "volume", "USDT", "tradecount") 
names(bitcoin) <- c("unix", "date", "symbol", "open", "high",
                    "low", "close", "volume", "USDT", "tradecount") 

# Unir los marcos de datos. 
crypto <- rbind(solana, cardano, binance, polkadot, ethereum, 
                chainlink, litecoin, uniswap, ripple, bitcoin) %>% dplyr::select(-unix)

# Lista de los nombres de los modelos de regresión a seleccionar. 
models <- list("Regresión Lineal Binance"   = "models/modeloBinance.rds", 
               "Regresión Lineal Ripple"    = "models/modeloRipple.rds",
               "Regresión Lineal Bitcoin"   = "models/modeloBitcoin.rds",
               "Regresión Lineal Cardano"   = "models/modeloCardano.rds",
               "Regresión Lineal Chainlink" = "models/modeloChainlink.rds",
               "Regresión Lineal Ethereum"  = "models/modeloEthereum.rds",
               "Regresión Lineal Litecoin"  = "models/modeloLitecoin.rds",
               "Regresión Lineal Polkadot"  = "models/modeloPolkadot.rds",
               "Regresión Lineal Solana"    = "models/modeloSolana.rds",
               "Regresión Lineal Uniswap"   = "models/modeloUniswap.rds")

# Lista de los nombres de los modelos LSTM a seleccionar. 
modelsKeras <- list("LSTM Solana"   = "models/LSTMSolana.h5",
                    "LSTM Cardano"  = "models/LSTMCardano.h5",
                    "LSTM Ethereum" = "models/LSTMEthereum.h5",
                    "LSTM Bitcoin"  = "models/LSTMBitcoin.h5")

# Lista de las variables del marco de datos a seleccionar. 
varquant <- list("Valor de Apertura" = "open",
                 "Valor Máximo" = "high",
                 "Valor Mínimo" = "low",
                 "Valor de Cierre" = "close",
                 "Volumen" = "volume",
                 "Volumen de USDT" = "USDT", 
                 "Número de Transacciones"= "tradecount")

# Salto temporal para los modelos LSTM.
datalags <- 3


