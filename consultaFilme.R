library(tm)
library(FastKNN)

############

my.corpus <- Corpus(DirSource("Documents/Rec Inf/Dois/LegendasTexto/", encoding = "ISO-8859-14"))
label <- read.csv("Documents/Rec Inf/consultaFilme/Categorias Filmes - Página1.csv")
label = label[c(-173,-615), ]


my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, removeWords, stopwords('portuguese'))

library(SnowballC)
my.corpus <- tm_map(my.corpus, stemDocument, language= "portuguese")


dtm.matrix.stm <- DocumentTermMatrix(my.corpus)

tfidf.matrix = weightTfIdf(dtm.matrix.stm)

tfidf.matrix <- as.matrix(tfidf.matrix)


euclidean = as.matrix(dist(tfidf.matrix, method = "euclidian"))


k.nearest.neighbors(32,euclidean, 5)
k.nearest.neighbors(370,euclidean, 5)
k.nearest.neighbors(444,euclidean, 5)
k.nearest.neighbors(227,euclidean, 5)
k.nearest.neighbors(248,euclidean, 5)



#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


teste = c(145, 157, 245, 259, 325, 443, 455, 488, 450, 232)


df.teste = tfidf.matrix[teste, ]
df.train = tfidf.matrix[-teste,]
label.train = label[-teste, ]
label.teste = label[teste,]


teste.2 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 2)
teste.3 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 3)
teste.4 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 4)
teste.5 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 5)
teste.6 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 6)
teste.7 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 7)
teste.8 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 8)

teste.15 = knn_training_function(df.train, euclidean, label.train$Categoria, k = 15)

label.treinando = cbind(label.train, teste.1, teste.2, teste.3, teste.4, teste.5, teste.6, teste.7, teste.8, teste.15)

count = 1
for(i in names(label.treinando)){
  if (count > 2){
    quantidade = nrow( label.treinando[as.character(label.treinando$Categoria)==as.character(label.treinando[[i]]),])
    print(paste("Para k = ", count - 1, " numero de acertos = ",  quantidade) )
    
  }
  count = count + 1
  
}



results = knn_test_function(df.train, df.teste, euclidean, label.train$Categoria, 6)
results



