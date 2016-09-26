library(tm)
library(FastKNN)
library(fields)
############

tratamento <- function(){
  my.corpus <- Corpus(DirSource("Documents/Rec Inf/Dois/LegendasTexto/", encoding = "ISO-8859-14"))
  label <- read.csv("Documents/Rec Inf/consultaFilme/Categorias Filmes - P??gina1.csv")
  label = label[c(-173,-615), ]
  
  
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, removeNumbers)
  my.corpus <- tm_map(my.corpus, removeWords, stopwords('portuguese'))
  
  library(SnowballC)
  my.corpus <- tm_map(my.corpus, stemDocument, language= "portuguese")
  
  dtm.matrix.stm <- DocumentTermMatrix(my.corpus)
  
  tfidf.matrix = weightTfIdf(dtm.matrix.stm)
  
  tfidf.matrix <- as.matrix(tfidf.matrix)
}


menu <- function(){
  cont = 1
  print("Escolha 6 filmes que voc?? gosta da lista abaixo:")
  for (filme in row.names(tfidf.matrix)){
    print(paste(cont, ":", filme))
    cont = cont + 1
    
  }
  entrada <- readline(prompt="Entre com o n??mero de 6 filmes que voc?? gosta (ex.: 1,2,3,4,5, 6): ")
  
  entrada = as.integer(unlist(strsplit(entrada, ",")))
  return(entrada)
}

#media para as colunas que nao tem linha 0
media.colunas.nao.zero <- function(tabela){
  for (i in 1:ncol(tabela)){
    if (min(tabela[,i]) > 0){
      tabela[1,i] = mean(tabela[,i]) #escolher minimo? media? maximo? moda?
    } else{
      tabela[1,i] = 0
    }
  }
  return(tabela)
}

recomendacao <- function(k.parametro){
  print("Pegando e tratandos os dados ...")
  tratamento()
  entrada = menu()
  escolhas.usuario = tfidf.matrix[entrada,]
  escolhas.usuario = media.colunas.nao.zero(escolhas.usuario)
  perfil_usuario = t(as.matrix(escolhas.usuario[1,]))
  distancia = rdist(perfil_usuario, tfidf.matrix[-entrada,])
  recomendacoes = k.nearest.neighbors(1, cbind(distancia, 0), k = k.parametro) #O zero pois o k.nearest.neighbors assume que a coluna com a menor distancia eh o proprio elemento (ele x ele), e no codigo nao temos um elemento e sim a juncao de varios
  row.names(tfidf.matrix[recomendacoes,])
}

print(recomendacao(k.parametro = 8))


