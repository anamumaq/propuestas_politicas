library(tidyverse)
library(tm)# para el text mining
library(wordcloud) # para el wordcloud
library(SnowballC)
library(RColorBrewer)
library(wordcloud2) 

VM = readLines("VM politicas educativas.txt", encoding="UTF-8") # extrae el texto con tildes
VM = Corpus(VectorSource(VM))# El texto es un speech la data debe subirse como un corpus
inspect(VM) ## revisando

## Cleaning los stopwords, numeros y puntacion (no hay caracteres especiales &/?@)
  VM = VM %>% 
    tm_map(removeNumbers)%>% # eliminar los minutos
    tm_map(removePunctuation)%>% # eliminar puntuacion o dospuntos
    tm_map(stripWhitespace) # quitar dobles espacios o enter

  VM = tm_map(VM, content_transformer(tolower)) # aplicamos minusculas
  VM = tm_map(VM, removeWords, stopwords("Spanish")) # quitar stopwords
  VM = tm_map(VM, removeWords, 
              c("el","de","en","que","por","los","las","para","una","con","este")) # quitar mis propias stopwords
##
  
dtm = TermDocumentMatrix(VM)# crear una taba con la frecuencia de repeticion 
matriz = as.matrix(dtm) # crear una matriz
palabras = sort(rowSums(matriz), decreasing = TRUE) # sumo las filas en la matriz
VM_df = data.frame(word = names(palabras), freq = palabras) # creo dataframe

#### GRAFICA FINAL
  ## primera alternativa
  wordcloud(words = VM_df$word, freq = VM_df$freq, 
            min.freq = 2, max.words = 200, 
            random.order = FALSE, rot.per =  0.35,
            colors = brewer.pal(8,"Dark"))
  
  ## segunda alternativa
  wordcloud2(data = VM_df, size = 1, color = 'random-dark')

  count(VM_df)  #numero de palabras
  
  

  
  