library(tidyverse)
library(tm)# para el text mining
library(wordcloud) # para el wordcloud
library(SnowballC) # para quitar las stopwords
library(RColorBrewer) #Colores para el primer wordcloud
library(wordcloud2) #wordcloud con mejores interacciones


# funcion nube genera df con el speech de cada candidato
nube = function(x,iniciales){
  
  x = Corpus(VectorSource(x))# El texto es un speech la data debe subirse como un corpus
  # inspect(vm) ## revisando
  
  ## Cleaning los stopwords, numeros y puntacion (no hay caracteres especiales &/?@)
  x = x %>% 
    tm_map(removeNumbers)%>% # eliminar los minutos
    tm_map(removePunctuation)%>% # eliminar puntuacion o dospuntos
    tm_map(stripWhitespace) # quitar dobles espacios o enter
  
  x = tm_map(x, content_transformer(tolower)) # aplicamos minusculas
  x = tm_map(x, removeWords, stopwords("Spanish")) # quitar stopwords
  x = tm_map(x, removeWords, 
            c("entonces","aquí","el","de","en","que","por","los","las","para","una","con","este")) # quitar mis propias stopwords
  
  dtm = TermDocumentMatrix(x)# crear una taba con la frecuencia de repeticion 
  matriz = as.matrix(dtm) # crear una matriz
  palabras = sort(rowSums(matriz), decreasing = TRUE) # sumo las filas en la matriz
  x_df = data.frame(row.names = NULL, candidato = iniciales, word = names(palabras), freq = palabras) # creo dataframe
  return(x_df)
}

# cargo los speechs de cada uno
yl = readLines("YL politicas educativas.txt", encoding="UTF-8") # extrae el texto con tildes
vm = readLines("VM politicas educativas.txt", encoding="UTF-8") 
hds = readLines("HdS politicas educativas.txt", encoding="UTF-8") 
rla = readLines("RLA politicas educativas.txt", encoding="UTF-8") 
gf = readLines("GF politicas educativas.txt", encoding="UTF-8") 
fc = readLines("FC politicas educativas.txt", encoding="UTF-8") 
ma = readLines("MA politicas educativas.txt", encoding="UTF-8") 

# genero df de cada speech
df_yl = nube(yl,"YL")
df_vm = nube(vm,"VM")
df_hds = nube(hds,"HdS")
df_rla = nube(rla,"RLA")
df_gf = nube(gf,"GF")
df_fc = nube(fc,"FC")
df_ma = nube(ma,"MA")

# uno todos los dfs en uno solo 
df  = df_yl %>%
    union_all(df_vm) %>%
    union_all(df_hds) %>%
    union_all(df_rla) %>%
    union_all(df_gf) %>%
    union_all(df_fc) %>%
    union_all(df_ma)

# wordcloud de cada candidato
wordcloud2(data = df_gf[2:3], size = 0.6, color = 'random-dark')



#### GRAFICA FINAL
  ## primera alternativa
#  wordcloud(words = df_gf$word, freq = df_gf$freq, 
#            min.freq = 2, max.words = 200, 
#            random.order = FALSE, rot.per =  0.35,
#            colors = brewer.pal(8,"Dark"))
  


  
  