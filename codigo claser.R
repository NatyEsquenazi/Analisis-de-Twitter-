install.packages("rtweet")
install.packages("tidyverse")
install.packages("quanteda")

library(rtweet)
library(tidyverse)
library(quanteda)

## Comprobamos que funcione rtweet pidiéndole a la API las tendencias de AR
trendsAR <- get_trends("argentina")
View(trendsAR)

write_as_csv(trendsAR, "trendsAR.csv") #Lo guardamos en un CSV que tiene datos estructurado
View(trendsAR)

## Capturamos los últimos 3200 tuits de una cuenta

usuario <- get_timeline("msalnacion", n = 3200)
write_as_csv(usuario, "usuario.csv") 
View(usuario)
dim(usuario)
ts_plot(usuario, by = "days") #Lo ploteamos por dia 

## Capturamos los ultimos 3200 tuits de tres cuentas en un solo dataframe

timelines <- get_timelines(c("alferdez", "horaciorlarreta", "kicillofok"), n = 3200)

write_as_csv(timelines, "timelines.csv")

## Graficamos una linea de tiempo con los tweets de las tres cuentas

timelines %>%
  dplyr::filter(created_at > "2020-01-01") %>%  #Toma los posteriores al 01 de enero
  dplyr::group_by(screen_name) %>%  #agrupa por usuario
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "Tuits publicados por cada cuenta"
  )



#Lineas utiles si hay otro encoding
timelines <- read.csv("https://github.com/labpoliticasuba/Taller-de-Twitter/raw/master/archivos/timelines.csv", fileEncoding = "UTF-8")
usuario <- read.csv("https://raw.githubusercontent.com/labpoliticasuba/Taller-de-Twitter/master/archivos/usuario.csv", fileEncoding = "UTF-8")
trendsAR <- read.csv("https://raw.githubusercontent.com/labpoliticasuba/Taller-de-Twitter/master/archivos/trendsAR.csv", fileEncoding = "UTF-8")



######## ANALISIS DEL TEXTO ######## 

#Previamente se instalo la libreria quanteda 

#Vemos como queda el dataset que descargamos recientemente
View(timelines)

#Vamos a transformar ese dataset en un corpus que podamos trabajar: aca se podria contar palabras, etc
corpus_timelines <- corpus(timelines)
head(corpus_timelines)

#Ahora vamos a transformalo en una matriz de palabras y tweets
dfm_timelines <- dfm(corpus_timelines, remove_punct = TRUE, remove = stopwords("spa"), 
                     groups = "screen_name") #agrupa por el usuario que emitio el tweet
head(dfm_timelines)

#RTA: tenemos 26.000 palabras, fernandez dijo 1 vez facundo, los demas 0. La /U0001 es un emoji

#Nube de palabras
textplot_wordcloud(dfm_timelines, rotation = 0.25, #rotacion de las palabras
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu"))) #paleta de colores

#Nube de palabras comparacion: comparacion discursiva entre diferentes usuarios 
textplot_wordcloud(dfm_timelines, comparison = TRUE, max_words = 300,
                   color = c("blue", "red", "green"))

#Top Users
user_timelines <- dfm_select(dfm_timelines, pattern = "@*") #Selecciono solo las palabras que comienzan con @ osea son usuarios 
top_user_timelines <- names(topfeatures(user_timelines, 50)) #Elejimos usuarios que aparecen mas de 50 veces
View(top_user_timelines)

#Hacemos una matriz de palabras (palabras que aparecen juntas en el mismo texto/tweet): fcm hace una matriz de correlacion de palabras
user_timelines_fcm <- fcm(user_timelines)
head(user_timelines_fcm) 

#La Tv publica aparece muy frecuentemente cercana a mau albornoz

#Visualizamos las relaciones
user_fcm <- fcm_select(user_timelines_fcm, pattern = top_user_timelines) #Me quedo con las palabras que estan en el top 50
textplot_network(user_fcm, min_freq = 0.1, edge_color = "orange", edge_alpha = 0.8, edge_size = 5)

#OJO: aca la red esta tomando datos de las elecciones inclusive 











