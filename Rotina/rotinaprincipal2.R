
#1) Tabela com os principais produtos agrícolas de agricultura temporária e permanente (em
#valores da produção, não quantidade), para o primeiro e última ano da série;


library(Hmisc)

####Principais produtos da agricultura temporária
#Vou deixar valores reais. Para isso multiplico os valores de 1995 por 3.753606 .Esse valor de ipca
# foi calculado na etapa seguinte dos mapas.


principaistemporarias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura temporária/principais_produtos_lavoura_temporaria_1995_2014.csv",header=T,na.strings=c("-","NA"))
princptemp=aggregate(principaistemporarias[,c(4)],by=list(Produto=principaistemporarias$Lavoura.temporária,Ano=principaistemporarias$Ano),sum,na.rm=T)

#1995
princptemp1995=princptemp[c(1:31),]
princptemp1995$x=princptemp1995$x*3.753606
princptemp1995=princptemp1995[order(-princptemp1995$x),]
colnames(princptemp1995)[3]="Valor"
#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas temporárias")
#write.csv(princptemp1995,file="Culturas temporárias mais importantes - 1995",row.names=F)
#latex(head(princptemp1995,15),rowname=NULL)

#2014
princptemp2014=princptemp[c(32:62),]
princptemp2014=princptemp2014[order(-princptemp2014$x),]
colnames(princptemp2014)[3]="Valor"
#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas temporárias")
#write.csv(princptemp2014,file="Culturas temporárias mais importantes - 2014",row.names=F)
#latex(head(princptemp2014,15),rowname=NULL)

princptemp=cbind(princptemp1995,princptemp2014)
princptemp=princptemp[,c(2,1,3,5,4,6)]
colnames(princptemp)=c("Year","Main products","Value","Year","Main products","Value" )
#latex(head(princptemp,20),rowname=NULL)

#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/Resultados artigo/Principais produtos")
#write.csv(princptemp,file="Principais produtos da lavoura temporária",row.names = F)

#####Principais produtos da agricultura permanente
principaispermanentes=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura permanente/principais_produtos_lavoura_permanente_1995_2014.csv",header=T,na.strings=c("-","NA"))
princperm=aggregate(principaispermanentes[,c(4)],by=list(Produtor=principaispermanentes$Lavoura.permanente,Ano=principaispermanentes$Ano),sum,na.rm=T)

#1995
princperm1995=princperm[c(1:35),]
princperm1995$x=princperm1995$x*3.753606
princperm1995=princperm1995[order(-princperm1995$x),]
colnames(princperm1995)[3]="Valor"
#Deixar somente café total
princperm1995=princperm1995[c(1:6,9:35),]
#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas permanentes")
#write.csv(princperm1995,file="Culturas permanetes mais importantes 1994",row.names=F)
#latex(head(princperm1995,15),rowname=NULL)

#2014
princperm2014=princperm[c(36:70),]
princperm2014=princperm2014[order(-princperm2014$x),]
colnames(princperm2014)[3]="Valor"
#Deixar somente café total
princperm2014=princperm2014[c(1:2,4:23,25:35),]
#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas permanentes")
#write.csv(princperm2014,file="Culturas permanetes mais importantes 2014",row.names=F)
#latex(head(princperm2014,15),rowname=NULL)

princperm=cbind(princperm1995,princperm2014)
princperm=princperm[,c(2,1,3,5,4,6)]
colnames(princperm)=c("Year","Main products","Value","Year","Main products","Value" )
#latex(head(princperm,15),rowname=NULL)

#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/Resultados artigo/Principais produtos")
#write.csv(princperm,file="Principais produtos da lavoura permanente",row.names=F)

rm(list=ls())

#2) Mapa coroplético com distriuição relativa do VBP temporária e permanente no estado de SP,
#para o primeiro e último ano da série;
#Vão ser quatro mapas então com participação relativa de cada município

###Tratar os dados para mostrarem a participação dos municípios
#Culturas permanentes
#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[19]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}



vculttemp=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura temporária/vptemporario.csv",
                   header=T, na.strings = c("-","NA"))



###Padronização dos períodos da base de dados
vculttemp=vculttemp[vculttemp$Ano>1994,]

#######Deflacionando série de dados - Valores de 2014
lvculttemp2=list()
lvculttemp=split(vculttemp, paste(vculttemp$Municipio))

for (i in 1:length(lvculttemp)){
  temp=cbind(lvculttemp[[i]]$Municipio,lvculttemp[[i]]$Ano,lvculttemp[[i]]$VPTemporario*ipca$V3)
  colnames(temp)=c("Municipio","Ano","VTReal")
  lvculttemp2[[i]]=temp
}

vculttemp=do.call(rbind.data.frame,lvculttemp2)


#######################Permanente
#carrega dados de culturas permanentes
vcultperm=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura permanente/vppermanente.csv",
                   header=T,na.strings=c("-","NA"))

###Padronização dos períodos da base de dados
vcultperm=vcultperm[vcultperm$Ano>1994,]

#######Deflacionando série de dados
lvcultperm2=list()
lvcultperm=split(vcultperm, paste(vcultperm$Município))

for (i in 1:length(lvcultperm)){
  perm=cbind(lvcultperm[[i]]$Município,lvcultperm[[i]]$Ano,lvcultperm[[i]]$VPPermanente*ipca$V3)
  colnames(perm)=c("Municipio","Ano","VPReal")
  lvcultperm2[[i]]=perm
}

vcultperm=do.call(rbind.data.frame,lvcultperm2)



###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Shapefiles/municipios_2010/municipios_2010.shp")
#Para adicionar contornos no mapa final preciso adicionar o shape file dos estados
estados=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Shapefiles/estados_2010/estados_2010.shp")

#Selecionar apenas os municípios que pertencem ao estado de SP
spmunicipios=municipios
spmunicipios=spmunicipios[order(spmunicipios@data$uf),]
spmunicipios@data$Cod=1:length(municipios@data$id)
spmunicipios=spmunicipios[c(4781:5425),]

#Selecionar apenas o shape do estado de SP
spestado=estados
spestado=spestado[c(26),]


###########################################################

#mapa de círculos proporcionais
#Culturas permanentes 1995
vcultperm1995=vcultperm[vcultperm$Ano==1995,]
spmunperm1995=spmunicipios
spcoordsmun=as.data.frame(coordinates(spmunperm1995))
dados=spmunperm1995@data
dados=cbind(dados,spcoordsmun)
dados$ordem=1:dim(dados)[1]
dadosp1995=merge(dados,vcultperm1995,by.x=c('codigo_ibg'),by.y=c('Municipio'),
             all.x=T,all.y=T)
#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
dadosp1995=merge(dadosp1995,bacias, by.x=c('codigo_ibg'),by.y=c('Municipio'))


cor=c("gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54")
      
cores=1:length(cor)
cores=as.data.frame(cbind(cores,cor))
colnames(cores)=c("Codigobacia","Cores")
dadosp1995=merge(dadosp1995,cores, by.x=c('Codigobacia'),by.y=c('Codigobacia'))
dadosp1995$Cores=as.character(dadosp1995$Cores)

dadosp1995=dadosp1995[order(dadosp1995$ordem),]
spmunperm1995@data=dadosp1995
#####################################################
plot(spmunperm1995, border=F,lwd=2, axes=F,las=0.01,col=dadosp1995$Cores)
plot(spestado,add=TRUE,lwd=1)
points(spcoordsmun$V1,spcoordsmun$V2,pch=21,col="darkgreen",
       bg=adjustcolor("darkgreen",0.5),cex=dadosp1995$VPReal/16000,lwd=0.9)

#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.51,-24.55))


####################################
#Culturas permanentes 2014
vcultperm2014=vcultperm[vcultperm$Ano==2014,]
spmunperm2014=spmunicipios
spcoordsmun=as.data.frame(coordinates(spmunperm2014))
dados=spmunperm2014@data
dados=cbind(dados,spcoordsmun)
dados$ordem=1:dim(dados)[1]
dadosp2014=merge(dados,vcultperm2014,by.x=c('codigo_ibg'),by.y=c('Municipio'),
                 all.x=T,all.y=T)
#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
dadosp2014=merge(dadosp2014,bacias, by.x=c('codigo_ibg'),by.y=c('Municipio'))


cor=c("gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54")

cores=1:length(cor)
cores=as.data.frame(cbind(cores,cor))
colnames(cores)=c("Codigobacia","Cores")
dadosp2014=merge(dadosp2014,cores, by.x=c('Codigobacia'),by.y=c('Codigobacia'))
dadosp2014$Cores=as.character(dadosp2014$Cores)

dadosp2014=dadosp2014[order(dadosp2014$ordem),]
spmunperm2014@data=dadosp2014
#####################################################
plot(spmunperm2014, border=F,lwd=2, axes=F,las=0.01,col=dadosp2014$Cores)
plot(spestado,add=TRUE,lwd=1)
points(spcoordsmun$V1,spcoordsmun$V2,pch=21,col="darkgreen",
       bg=adjustcolor("darkgreen",0.5),cex=dadosp2014$VPReal/16000,lwd=0.9)

#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.51,-24.55))

###########################
legend(c("47", "94","141","188"),x=-44,y=-20, pt.bg=adjustcolor("darkgreen",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.5,title="Million R$",
       pt.cex =c(187577/16000/4,187577/16000/2,187577/16000*3/4,187577/16000))



####################################
#Culturas temporárias 1995
vculttemp1995=vculttemp[vculttemp$Ano==1995,]
spmuntemp1995=spmunicipios
spcoordsmun=as.data.frame(coordinates(spmuntemp1995))
dados=spmuntemp1995@data
dados=cbind(dados,spcoordsmun)
dados$ordem=1:dim(dados)[1]
dadost1995=merge(dados,vculttemp1995,by.x=c('codigo_ibg'),by.y=c('Municipio'),
                 all.x=T,all.y=T)
#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
dadost1995=merge(dadost1995,bacias, by.x=c('codigo_ibg'),by.y=c('Municipio'))


cor=c("gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54")

cores=1:length(cor)
cores=as.data.frame(cbind(cores,cor))
colnames(cores)=c("Codigobacia","Cores")
dadost1995=merge(dadost1995,cores, by.x=c('Codigobacia'),by.y=c('Codigobacia'))
dadost1995$Cores=as.character(dadost1995$Cores)

dadost1995=dadost1995[order(dadost1995$ordem),]
spmuntemp1995@data=dadost1995
#####################################################
plot(spmuntemp1995, border=F,lwd=2, axes=F,las=0.01,col=dadost1995$Cores)
plot(spestado,add=TRUE,lwd=1)
points(spcoordsmun$V1,spcoordsmun$V2,pch=21,col="darkgreen",
       bg=adjustcolor("darkgreen",0.5),cex=dadost1995$VTReal/40000,lwd=0.9)

#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.51,-24.55))

####################################
#Culturas temporárias 2014
vculttemp2014=vculttemp[vculttemp$Ano==2014,]
spmuntemp2014=spmunicipios
spcoordsmun=as.data.frame(coordinates(spmuntemp2014))
dados=spmuntemp2014@data
dados=cbind(dados,spcoordsmun)
dados$ordem=1:dim(dados)[1]
dadost2014=merge(dados,vculttemp2014,by.x=c('codigo_ibg'),by.y=c('Municipio'),
                 all.x=T,all.y=T)
#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
dadost2014=merge(dadost2014,bacias, by.x=c('codigo_ibg'),by.y=c('Municipio'))


cor=c("gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54")

cores=1:length(cor)
cores=as.data.frame(cbind(cores,cor))
colnames(cores)=c("Codigobacia","Cores")
dadost2014=merge(dadost2014,cores, by.x=c('Codigobacia'),by.y=c('Codigobacia'))
dadost2014$Cores=as.character(dadost2014$Cores)

dadost2014=dadost2014[order(dadost2014$ordem),]
spmuntemp2014@data=dadost2014
#####################################################
plot(spmuntemp2014, border=F,lwd=2, axes=F,las=0.01,col=dadost2014$Cores)
plot(spestado,add=TRUE,lwd=1)
points(spcoordsmun$V1,spcoordsmun$V2,pch=21,col="darkgreen",
       bg=adjustcolor("darkgreen",0.5),cex=dadost2014$VTReal/40000,lwd=0.9)

#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.51,-24.55))

###########################
#Definir legenda e escala
#Vou plotar legenda a parte
legend(c("150", "300","450","600"),x=-44,y=-20, pt.bg=adjustcolor("darkgreen",0.5),
       col="darkgreen",pch=c(21,21,21,21),bty="n",cex=1.5,title="Million R$",
       pt.cex =c(593502/40000/4,593502/40000/2,593502/40000*3/4,593502/40000))


rm(list=ls())

#4) #############################################################################
###############################################################
#Estatísticas descritivas básicas
#Valor de Produção e Bacias hidrográficas
#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[19]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}

#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
###carrega dados climáticos
#carrega dados anuais
load("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Dados climáticos/climafinalanosp.RData")

#carrega dados por estação do ano
load("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Dados climáticos/climafinalestsp.RData")

climafinalanosp=climafinalanosp[climafinalanosp$Ano>1994,]
climafinalanosp=climafinalanosp[,c(1,4:18)]
climafinalestsp=climafinalestsp[climafinalestsp$Ano>1994,]
climafinalestsp=climafinalestsp[,c(1,3:59)]


###Carrega dados de valor da produção de culturas temporárias

vculttemp=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura temporária/vptemporario.csv",
                   header=T, na.strings = c("-","NA"))
vculttemp=vculttemp[vculttemp$Ano>1994,]


#######Deflacionando série de dados
lvculttemp2=list()
lvculttemp=split(vculttemp, paste(vculttemp$Municipio))

for (i in 1:length(lvculttemp)){
  temp=cbind(lvculttemp[[i]]$Municipio,lvculttemp[[i]]$Ano,lvculttemp[[i]]$VPTemporario*ipca$V3)
  colnames(temp)=c("Municipio","Ano","VTReal")
  lvculttemp2[[i]]=temp
}

vculttemp=do.call(rbind.data.frame,lvculttemp2)

#merge tables
vculttemp=merge(vculttemp,climafinalanosp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)
vculttemp=merge(vculttemp,climafinalestsp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)
vculttemp=merge(vculttemp,bacias, by.x=c('Municipio'), by.y=c('Municipio'),all.x=T,all.y=T)


#############################################

#carrega dados de culturas permanentes
vcultperm=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura permanente/vppermanente.csv",
                   header=T,na.strings=c("-","NA"))

vcultperm=vcultperm[vcultperm$Ano>1994,]


#######Deflacionando série de dados
lvcultperm2=list()
lvcultperm=split(vcultperm, paste(vcultperm$Município))

for (i in 1:length(lvcultperm)){
  perm=cbind(lvcultperm[[i]]$Município,lvcultperm[[i]]$Ano,lvcultperm[[i]]$VPPermanente*ipca$V3)
  colnames(perm)=c("Municipio","Ano","VPReal")
  lvcultperm2[[i]]=perm
}

vcultperm=do.call(rbind.data.frame,lvcultperm2)

#merge tables
vcultperm=merge(vcultperm,climafinalanosp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)
vcultperm=merge(vcultperm,climafinalestsp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)
vcultperm=merge(vcultperm,bacias, by.x=c('Municipio'), by.y=c('Municipio'),all.x=T,all.y=T)

##################################
#Estatísticas climáticas

#Exportar a tabela clima e fazer no excel o gráfico de tendência das variáveis climáticas
clima=aggregate(vcultperm[,c(5,7,10,15)],by=list(Ano=vcultperm$Ano),mean,na.rm=T)

#Agora preciso obter a taxa de crescimento linear das variáveis por bacia
#Para isso preciso obter primeiro a série das variáveis por bacia
climabacia=aggregate(vcultperm[,c(5,7,10,15)],by=list(Bacia=vcultperm$Bacia,
                          Ano=vcultperm$Ano),mean,na.rm=T)

climabacia$`Temperatura Media`=log(climabacia$`Temperatura Media`)
climabacia$`Temperatura Media(sd)`=log(climabacia$`Temperatura Media(sd)`)
climabacia$entre0e1=log(climabacia$entre0e1)
climabacia$maisde25mm=log(climabacia$maisde25mm)

climabacial2=list()
climabacial=split(climabacia,paste(climabacia$Bacia))

for(i in 1: length(climabacial)){

TMED=lm(climabacial[[i]]$`Temperatura Media` ~ climabacial[[i]]$Ano,data=climabacial[[i]])
TMEDSD=lm(climabacial[[i]]$`Temperatura Media(sd)`~ climabacial[[i]]$Ano,data=climabacial[[i]])
poucachuva=lm(climabacial[[i]]$entre0e1~ climabacial[[i]]$Ano,data=climabacial[[i]])
muitachuva=lm(climabacial[[i]]$maisde25mm~ climabacial[[i]]$Ano,data=climabacial[[i]])

#Falta só multipliplicar por 100 para as taxas de crescimento ficarem em percentual
climabacia2=as.data.frame(cbind(as.character(climabacial[[i]]$Bacia[1]),(exp(as.numeric(TMED$coefficients[2]))-1),
              (exp(as.numeric(TMEDSD$coefficients[2]))-1),(exp(as.numeric(poucachuva$coefficients[2]))-1),
                (exp(as.numeric(muitachuva$coefficients[2]))-1)))
colnames(climabacia2)=c("Bacia","Temp","Tempsd","menos1mm","mais25mm")

climabacial2[[i]]=climabacia2
}

#Exportar essa tabela e editá-la em excel
climabacia=do.call(rbind.data.frame,climabacial2)

#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/Resultados artigo/Clima")
#write.csv(clima,file="Tendência do clima",row.names=F)
#write.csv(climabacia,file="Taxa de crescimento variáveis climáticas por bacia",row.names=F)

############################################################################
############################################################################
#Exportar a tabela com os valores da produção agregados por ano e fazer o gráfico no excel
vt=aggregate(vculttemp[,c(3)],by=list(Ano=vculttemp$Ano),sum,na.rm=T)
vp=aggregate(vcultperm[,c(3)],by=list(Ano=vcultperm$Ano),sum,na.rm=T)
vproducao=cbind(vt,vp[,c(2)])
colnames(vproducao)=c("Ano","VTemporarias","VPermanente")


###################################################
###Valor da produção por bacia
vprodbaciap=aggregate(vcultperm[,c(3)],by=list(Bacia=vcultperm$Bacia,Ano=vcultperm$Ano),sum,na.rm=T)
vprodbaciap$x=log(vprodbaciap$x)
vprodbaciap$x[!is.finite(vprodbaciap$x)]=NA

vprodbaciat=aggregate(vculttemp[,c(3)],by=list(Bacia=vculttemp$Bacia,Ano=vculttemp$Ano),sum,na.rm=T)
vprodbaciat$x=log(vprodbaciat$x)
vprodbaciat$x[!is.finite(vprodbaciat$x)]=NA


vprodbacia=merge(vprodbaciap,vprodbaciat,by.x=c("Bacia","Ano"),by.y=c("Bacia","Ano"))
colnames(vprodbacia)=c("Bacia","Ano","Vperm","Vtemp")

vprodbacial2=list()
vprodbacial=split(vprodbacia,paste(vprodbacia$Bacia))

for(i in 1:length(vprodbacial)){
vprop=lm(vprodbacial[[i]]$Vperm ~ vprodbacial[[i]]$Ano,data=vprodbacial[[i]])
vprot=lm(vprodbacial[[i]]$Vtemp ~ vprodbacial[[i]]$Ano,data=vprodbacial[[i]])

vpro=as.data.frame(cbind(as.character(vprodbacial[[i]]$Bacia[1]),(exp(as.numeric(vprop$coefficients[2]))-1),
                         (exp(as.numeric(vprot$coefficients[2]))-1)))
colnames(vpro)=c("Bacia","Vperm","Vtemp")

vprodbacial2[[i]]=vpro
}


#Exportar essa tabela e editá-la em excel
vprodbacia=do.call(rbind.data.frame,vprodbacial2)

#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/Resultados artigo/Valor da produção")
#write.csv(vproducao,file="Tendência do valor da produção",row.names=F)
#write.csv(vprodbacia,file="Taxa de crescimento do valor da produção por bacias",row.names=F)

rm(list=ls())

#5) Modelo de dados em painel:

 # VBP temporária(it) = area temporária (it) + variáveis climáticas (it) + i + t + erro(it)
  #VBP permanente(it) = area temporária (it) + variáveis climáticas (it) + i + t + erro(it)
  
  #O ideal seria um modelo espacial. Não sei se vai dar tempo:
    
   # VBP temporária(it) = area temporária (it) + variáveis climáticas (it) + i + t +erro(i-1,t) +erro(it)
  #VBP permanente(it) = area temporária (it) + variáveis climáticas (it) + i + t + erro(i-1,t) + erro(it)

######################################################################
#####################################################################
#Modelo de dados em painel para culturas temporárias

#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[19]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}



vculttemp=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura temporária/vptemporario.csv",
                   header=T, na.strings = c("-","NA"))
vculttemp=vculttemp[vculttemp$Ano>1994,]


###carrega dados climáticos
#carrega dados anuais
load("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Dados climáticos/climafinalanosp.RData")

#carrega dados por estação do ano
load("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Dados climáticos/climafinalestsp.RData")

###Carrega dados de área e de valor da produção de culturas temporárias
areaculttemp=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura temporária/area_colhida_lavoura_temporaria.csv",
                      header=T,na.strings = c("-","NA"))

vculttemp=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura temporária/vptemporario.csv",
                  header=T, na.strings = c("-","NA"))



###Padronização dos períodos da base de dados
vculttemp=vculttemp[vculttemp$Ano>1994,]
climafinalanosp=climafinalanosp[climafinalanosp$Ano>1994,]
climafinalanosp=climafinalanosp[,c(1,4:18)]
climafinalestsp=climafinalestsp[climafinalestsp$Ano>1994,]
climafinalestsp=climafinalestsp[,c(1,3:59)]

#######Deflacionando série de dados
lvculttemp2=list()
lvculttemp=split(vculttemp, paste(vculttemp$Municipio))

for (i in 1:length(lvculttemp)){
  temp=cbind(lvculttemp[[i]]$Municipio,lvculttemp[[i]]$Ano,lvculttemp[[i]]$VPTemporario*ipca$V3)
  colnames(temp)=c("Municipio","Ano","VTReal")
  lvculttemp2[[i]]=temp
}

vculttemp=do.call(rbind.data.frame,lvculttemp2)
vculttemp$logreal=log(vculttemp$VTReal)
vculttemp$logreal[!is.finite(vculttemp$logreal)]=NA

#Padronizando data de área colhida e aplicando log
areaculttemp=areaculttemp[areaculttemp$Ano>1994,]
areaculttemp$logarea=log(areaculttemp$Area.Colhida)
areaculttemp$logarea[!is.finite(areaculttemp$logarea)]=NA

#merge tables
vculttemp=merge(vculttemp,areaculttemp,by.x=c('Municipio','Ano'),by.y=c('Município','Ano'))
vculttemp=merge(vculttemp,climafinalanosp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)
vculttemp=merge(vculttemp,climafinalestsp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)

#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
vculttemp=merge(vculttemp,bacias, by.x=c('Municipio'), by.y=c('Municipio'),all.x=T,all.y=T)

#O painel de dados tem que necessariamente ser balanceado para que eu consiga fazer regressão espacial.
#o que eu posso fazer é deixar apenas os municípios presentes em todos os anos da série
lvculttemp=split(vculttemp,paste(vculttemp$Municipio))
lvculttemp2=list()
j=1
for (i in 1:length(lvculttemp)){
lvculttemp[[i]]$TesteNA=0
lvculttemp[[i]]$TesteNA[is.na(lvculttemp[[i]]$logreal)]=1
if(sum(lvculttemp[[i]]$TesteNA)==0){
  lvculttemp2[j]=lvculttemp[i]
  j=j+1
  }
}

vculttemp=do.call(rbind.data.frame,lvculttemp2)

###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Shapefiles/municipios_2010/municipios_2010.shp")

#Selecionar apenas os municípios que pertencem ao estado de SP
spmunicipios=municipios
spmunicipios=spmunicipios[order(spmunicipios@data$uf),]
spmunicipios@data$Cod=1:length(municipios@data$id)
spmunicipios=spmunicipios[c(4781:5425),]

#Exlui Ilha Bela do litoral de São Paulo. Com nã havia conexões com o continente
#estava dando problema na hora de criar a matrix de pesos. 
spmunicipios2014=spmunicipios[c(1:12,14:645),]
munpanel=vculttemp[vculttemp$Ano==2014,]

#Não vai ser só ilha bela
#Tenho que excluir todos os municípios que não estão presentes no painel de dados
#Atribuindo 1 a variável bin para os elementos que estão presentes em munpanel e spmunicipios2014 simultaneamente
spmunicipios2014$bin=0
for (i in 1:length(spmunicipios2014$codigo_ibg)){
  for (j in 1:length(munpanel$Municipio)){
    if (spmunicipios2014$codigo_ibg[[i]]==munpanel$Municipio[[j]]){
      spmunicipios2014$bin[[i]]=1
    }
    
  }
  
}

#Pegando em spmunicipios2014 apenas os elementos com valor de bin =1  e atribuindo novamente a spmunicipios2014
spmunicipios2014=spmunicipios2014[spmunicipios2014$bin==1,]

library(spdep)
#Cria neighbors por contiguidade
spneighbors=poly2nb(spmunicipios2014)
coords=coordinates(spmunicipios2014)
#Plotanto mapa de vizinhança
plot(spneighbors,coords,col="black")


#####Spatial weight matrix based on contiguity
weigthmatrix=nb2listw(spneighbors)
summary(weigthmatrix)

#Lagrange multiplier test for spatial lag and spatial error dependencies
#De acordo com os p-values nós temos dependência espacial e também podemos 
#usar modelo com lag e com erro espacial
#lm.LMtests(logareal[[1]]$logreal,weigthmatrix,test=c("LMlag","LMerr"))


library(plm)
library(splm)



#Preciso incluir as binárias para as bacias 
vculttemp$Codigobacia=as.character(vculttemp$Codigobacia)
vculttemp=cbind(vculttemp,model.matrix(~vculttemp$Codigobacia-1))

#Binárias para o ano
vculttemp$Ano2=as.character(vculttemp$Ano)
vculttemp=cbind(vculttemp,model.matrix(~vculttemp$Ano2-1))


vculttemp=plm.data(vculttemp)
vculttemp$Ano=as.numeric(vculttemp$Ano)

Y=cbind(vculttemp[,c(4)])
#melhor ajuste até o momento
#X=as.matrix(vculttemp[,c(11,8,10,13)])
#Problema com a binária 78
X=as.matrix(vculttemp[,c(8,10,13,32,81:96,98:100,2)])

fm=Y~X


######################
#Testes Lagrange multiplier

LM1=bsktest(x=fm,data=vculttemp,listw=weigthmatrix,test="LM1")
LM1

LM2=bsktest(x=fm,data=vculttemp,listw=weigthmatrix,test="LM2")
LM2

LMH=bsktest(x=fm,data=vculttemp,listw=weigthmatrix,test="LMH")
LMH

CLMlambda=bsktest(x=fm,data=vculttemp,listw=weigthmatrix,test="CLMlambda")
CLMlambda



##################
#Efeitos fixos e aleatórios não espacial + teste de hausmann
fe=plm(formula=fm,data=vculttemp,model="within")
summary(fe)

re=plm(formula=fm,data=vculttemp,model="random")
summary(re)

phtest(re, fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios sem erro espacial e
#sem lag +  teste de hausman
fe=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="within")
summary(fe)

re=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="random")
summary(re)

sphtest(x = re, x2 = fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios com erro espacial e
#sem lag +  teste de hausman

fe=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="within",spatial.error = T,lag=F)
summary(fe)

re=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="random",spatial.error = T,lag=F)
summary(re)

sphtest(x = re, x2 = fe)

#Modelos espaciais de efeitos fixo e efeitos aleatórios sem erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="within",lag=T,spatial.error = F)
summary(fe)

re=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="random",lag=T,spatial.error = F)
summary(re)

sphtest(x = re, x2 = fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios com erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="within",lag=T, spatial.error = T)
summary(fe)

re=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="random",lag=T,spatial.error = T)
summary(re)

sphtest(x = re, x2 = fe)



rm(list=ls())

######################################################################
#####################################################################
#Modelo de dados em painel para culturas permanentes

#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/IPCA/IPCA.csv",header=T)
ipca=ipca[c(16:35),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1995. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[20]=1
ipca$V3[19]=ipca$V1[19]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}



###carrega dados climáticos
#carrega dados anuais
load("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Dados climáticos/climafinalanosp.RData")

#carrega dados por estação do ano
load("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Dados climáticos/climafinalestsp.RData")


#carrega dados de culturas permanentes
vcultperm=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura permanente/vppermanente.csv",
                   header=T,na.strings=c("-","NA"))

###Padronização dos períodos da base de dados
vcultperm=vcultperm[vcultperm$Ano>1994,]
climafinalanosp=climafinalanosp[climafinalanosp$Ano>1994,]
climafinalanosp=climafinalanosp[,c(1,4:18)]
climafinalestsp=climafinalestsp[climafinalestsp$Ano>1994,]
climafinalestsp=climafinalestsp[,c(1,3:59)]

#######Deflacionando série de dados
lvcultperm2=list()
lvcultperm=split(vcultperm, paste(vcultperm$Município))

for (i in 1:length(lvcultperm)){
  perm=cbind(lvcultperm[[i]]$Município,lvcultperm[[i]]$Ano,lvcultperm[[i]]$VPPermanente*ipca$V3)
  colnames(perm)=c("Municipio","Ano","VPReal")
  lvcultperm2[[i]]=perm
}

vcultperm=do.call(rbind.data.frame,lvcultperm2)
vcultperm$logreal=log(vcultperm$VPReal)
vcultperm$logreal[!is.finite(vcultperm$logreal)]=NA


#merge tables
vcultperm=merge(vcultperm,climafinalanosp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)
vcultperm=merge(vcultperm,climafinalestsp, by.x=c('Municipio','Ano'),by.y=c('Codigo','Ano'),all.x=T,all.y=F)


#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
vcultperm=merge(vcultperm,bacias, by.x=c('Municipio'), by.y=c('Municipio'),all.x=T,all.y=T)

#O painel de dados tem que necessariamente ser balanceado para que eu consiga fazer regressão espacial.
#o que eu posso fazer é deixar apenas os municípios presentes em todos os anos da série
lvcultperm=split(vcultperm,paste(vcultperm$Municipio))
lvcultperm2=list()
j=1
for (i in 1:length(lvcultperm)){
  lvcultperm[[i]]$TesteNA=0
  lvcultperm[[i]]$TesteNA[is.na(lvcultperm[[i]]$logreal)]=1
  if(sum(lvcultperm[[i]]$TesteNA)==0){
    lvcultperm2[j]=lvcultperm[i]
    j=j+1
  }
}

vcultperm=do.call(rbind.data.frame, lvcultperm2)

###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Shapefiles/municipios_2010/municipios_2010.shp")

#Selecionar apenas os municípios que pertencem ao estado de SP
spmunicipios=municipios
spmunicipios=spmunicipios[order(spmunicipios@data$uf),]
spmunicipios@data$Cod=1:length(municipios@data$id)
spmunicipios=spmunicipios[c(4781:5425),]

#Exlui Ilha Bela do litoral de São Paulo. Com nã havia conexões com o continente
#estava dando problema na hora de criar a matrix de pesos. 
spmunicipios2014=spmunicipios[c(1:12,14:645),]

#Não vai ser só ilha bela
#Tenho que excluir todos os municípios que não estão presentes no painel de dados
#Atribuindo 1 a variável bin para os elementos que estão presentes em munpanel e spmunicipios2014 simultaneamente
munpanel=vcultperm[vcultperm$Ano==2014,]
spmunicipios2014$bin=0
for (i in 1:length(spmunicipios2014$codigo_ibg)){
  for (j in 1:length(munpanel$Municipio)){
    if (spmunicipios2014$codigo_ibg[[i]]==munpanel$Municipio[[j]]){
      spmunicipios2014$bin[[i]]=1
    }
    
  }
  
}

#Pegando em spmunicipios2014 apenas os elementos com valor de bin =1  e atribuindo novamente a spmunicipios2014
spmunicipios2014=spmunicipios2014[spmunicipios2014$bin==1,]

library(spdep)
#Cria neighbors por contiguidade
spneighbors=poly2nb(spmunicipios2014)
coords=coordinates(spmunicipios2014)
#Plotanto mapa de vizinhança
plot(spneighbors,coords,col="black")

#Tentei editar a lista para ligar pontos desconexos
#edit.nb(spneighbors,coords)

#####Spatial weight matrix based on contiguity
#Como não consegui editar usei o argumento zero policy
#nem edit.nb ou zero policy funcionaram
#Peo que eu entendi esse comando cria a matriz de pesos pelas coordenadas, ao invés de usar
#a vizinhança. Usa coordenadas a uma distânai de 45 km
weigthmatrix=dnearneigh(coords,0,45,longlat = T)
weigthmatrix=nb2listw(weigthmatrix)
summary(weigthmatrix)

#Colocar binárias para os anos e para as bacias hidrográficas


library(plm)
library(splm)

#Preciso incluir as binárias para as bacias 
vcultperm$Codigobacia=as.character(vcultperm$Codigobacia)
vcultperm=cbind(vcultperm,model.matrix(~vcultperm$Codigobacia-1))

#Binárias para o ano
vcultperm$Ano2=as.character(vcultperm$Ano)
vcultperm=cbind(vcultperm,model.matrix(~vcultperm$Ano2-1))

vcultperm=plm.data(vcultperm)
vcultperm$Ano=as.numeric(vcultperm$Ano)
Y=as.matrix(vcultperm[,c(4)])
#melhor ajuste até o momento
X=as.matrix(vcultperm[,c(6,8,11,16,79:94,96:99,2)])

#Problema com a binária 78
#X=as.matrix(vculttemp[,c(11,8,10,13,78)])

fm=Y~X

######################
#Testes Lagrange multiplier

LM1=bsktest(x=fm,data=vcultperm,listw=weigthmatrix,test="LM1")
LM1

LM2=bsktest(x=fm,data=vcultperm,listw=weigthmatrix,test="LM2")
LM2

LMH=bsktest(x=fm,data=vcultperm,listw=weigthmatrix,test="LMH")
LMH

CLMlambda=bsktest(x=fm,data=vcultperm,listw=weigthmatrix,test="CLMlambda")
CLMlambda

###################
#Efeitos fixos e aleatórios não espacial + teste de hausmann
fe=plm(formula=fm,data=vcultperm,model="within")
summary(fe)

re=plm(formula=fm,data=vcultperm,model="random")
summary(re)

phtest(re, fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios sem erro espacial e
#sem lag +  teste de hausman
fe=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="within")
summary(fe)

re=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="random")
summary(re)

sphtest(x = re, x2 = fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios com erro espacial e
#sem lag +  teste de hausman

fe=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="within",spatial.error = T)
summary(fe)

re=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="random",spatial.error = T)
summary(re)

sphtest(x = re, x2 = fe)

#Modelos espaciais de efeitos fixo e efeitos aleatórios sem erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="within",lag=T,spatial.error = F)
summary(fe)

re=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="random",lag=T,spatial.error = F)
summary(re)

sphtest(x = re, x2 = fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios com erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="within",lag=T, spatial.error = T)
summary(fe)

re=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="random",lag=T,spatial.error = T)
summary(re)

sphtest(x = re, x2 = fe)

