library(tm)
library(FastKNN)

############

my.corpus <- Corpus(DirSource("Documents/Rec Inf/Dois/LegendasTexto/"))
label <- read.csv("Documents/Rec Inf/consultaFilme/Categorias Filmes - Página1.csv")
label = label[c(-173,-615), ]


my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, removeNumbers)

library(SnowballC)
my.corpus <- tm_map(my.corpus, stemDocument, language= "portuguese")


dtm.matrix.stm <- DocumentTermMatrix(my.corpus)

tfidf.matrix = weightTfIdf(dtm.matrix.stm)

tfidf.matrix <- as.matrix(tfidf.matrix)


euclidean = as.matrix(dist(tfidf.matrix, method = "euclidian"))
manhattan = as.matrix(dist(tfidf.matrix, method = "manhattan"))


k.nearest.neighbors(32,euclidean, 5)
k.nearest.neighbors(370,euclidean, 5)
k.nearest.neighbors(444,euclidean, 5)
k.nearest.neighbors(227,euclidean, 5)
k.nearest.neighbors(248,euclidean, 5)

k.nearest.neighbors(32,manhattan, 5)
k.nearest.neighbors(370,manhattan, 5)
k.nearest.neighbors(444,manhattan, 5)
k.nearest.neighbors(227,manhattan, 5)
k.nearest.neighbors(248,manhattan, 5)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
teste = c(145, 157, 246, 260, 326, 443, 456, 489, 450, 233)

teste = c(145, 157, 245, 259, 325, 442, 455, 488, 449, 232)

View(tfidf.matrix[145,1])
df.teste = tfidf.matrix[teste, ]
df.train = tfidf.matrix[-teste,]
label.train = label[-teste, ]
label.teste = label[teste,]


nrow(df.train)
results = knn_test_function(df.train, df.teste, euclidean, label.train$Categoria, 5)




