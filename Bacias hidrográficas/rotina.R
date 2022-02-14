
###carrega dados climáticos
#carrega dados anuais
load("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Dados climáticos/climafinalanosp.RData")

ibgemun=climafinalanosp[climafinalanosp$Ano==2014,c(1,2,4)]
ibgemun=ibgemun[order(ibgemun$Municipio),]

#Vou precisar fundir vculttemp com as bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/BaciasHidrograficasSP.csv",
                header=T)
bacias=bacias[order(bacias$nomemunic),]


bacias=cbind(bacias,ibgemun)
bacias=bacias[,c(4,1,2,3)]

setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas")
write.csv(bacias, file="baciamun", row.names=F)
