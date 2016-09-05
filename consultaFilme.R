library(tm)
library(FastKNN)

############

my.corpus <- Corpus(DirSource("Documents/Rec Inf/Dois/LegendasTexto/"))

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

