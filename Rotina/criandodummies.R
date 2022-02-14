#Generate example dataframe with character column
example <- as.data.frame(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))
names(example) <- "strcol"

#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
  example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}


##Esse código substitui o for e tudo o que está dentro dele. Mas não consigo controlar o nome das
#dummies pelo jeito. Ai depois teria que alterar colname. NO for talvez eu já criaria
#o colname na hora. Mas da para identificar o ano, mesmo que não fique esteticamente bonito
#acho que esse é o comando. model.matrix(). Muito superior ao de cima
example<-cbind(example, model.matrix(~example$strcol-1))

