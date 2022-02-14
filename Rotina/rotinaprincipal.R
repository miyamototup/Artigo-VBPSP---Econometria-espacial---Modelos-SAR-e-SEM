
#1) Tabela com os principais produtos agrícolas de agricultura temporária e permanente (em
#valores da produção, não quantidade), para o primeiro e última ano da série;

library(Hmisc)

####Principais produtos da agricultura temporária
#Vou deixar valores nominais. O importante é a ordem das culturas e não os valores da produção
principaistemporarias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura temporária/principais produtos da agricultura temporaria_1994 e 2014.csv",header=T,na.strings=c("-","NA"))
princptemp=aggregate(principaistemporarias[,c(4)],by=list(Produto=principaistemporarias$Lavoura.temporária,Ano=principaistemporarias$Ano),sum,na.rm=T)

#1994
princptemp1994=princptemp[c(1:31),]
princptemp1994=princptemp1994[order(-princptemp1994$x),]
colnames(princptemp1994)[3]="Valor"
setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas temporárias")
#write.csv(princptemp1994,file="Culturas temporárias mais importantes - 1994",row.names=F)
#latex(head(princptemp1994,15),rowname=NULL)

#2014
princptemp2014=princptemp[c(32:62),]
princptemp2014=princptemp2014[order(-princptemp2014$x),]
colnames(princptemp2014)[3]="Valor"
setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas temporárias")
#write.csv(princptemp2014,file="Culturas temporárias mais importantes - 2014",row.names=F)
#latex(head(princptemp2014,15),rowname=NULL)

princptemp1994=princptemp1994[,c(1,3)]
princptemp2014=princptemp2014[,c(1,3)]
princptemp=cbind(princptemp1994,princptemp2014)
colnames(princptemp)=c("Main products-1994","Value","Main products-2014","Value" )
#latex(head(princptemp,15),rowname=NULL)

#####Principais produtos da agricultura permanente
principaispermanentes=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Lavoura permanente/principais produtos da agricultura permanente_1994 e 2014.csv",header=T,na.strings=c("-","NA"))
princperm=aggregate(principaispermanentes[,c(4)],by=list(Produtor=principaispermanentes$Lavoura.permanente,Ano=principaispermanentes$Ano),sum,na.rm=T)

#1994
princperm1994=princperm[c(1:35),]
princperm1994=princperm1994[order(-princperm1994$x),]
colnames(princperm1994)[3]="Valor"
setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas permanentes")
#write.csv(princperm1994,file="Culturas permanetes mais importantes 1994",row.names=F)
#latex(head(princperm1994,15),rowname=NULL)

#2014
princperm2014=princperm[c(36:70),]
princperm2014=princperm2014[order(-princperm2014$x),]
colnames(princperm2014)[3]="Valor"
#Excluir café total
princperm2014=princperm2014[c(1,3:35),]
setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Resultados/1 - Culturas mais importantes e mapas/Culturas permanentes")
#write.csv(princperm2014,file="Culturas permanetes mais importantes 2014",row.names=F)
#latex(head(princperm2014,15),rowname=NULL)

princperm1994=princperm1994[c(1:34),c(1,3)]
princperm2014=princperm2014[,c(1,3)]
princperm=cbind(princperm1994,princperm2014)
colnames(princperm)=c("Main products-1994","Value","Main products-2014","Value" )
#latex(head(princperm,15),rowname=NULL)

#2) Mapa coroplético com distriuição relativa do VBP temporária e permanente no estado de SP,
#para o primeiro e último ano da série;
#Vão ser quatro mapas então com participação relativa de cada município

###Tratar os dados para mostrarem a participação dos municípios
#Culturas permanentes
munperm=aggregate(principaispermanentes[,c(4)],by=list(Municipio=principaispermanentes$Município,
                                                       Ano=principaispermanentes$Ano),sum,na.rm=T)

munperml=split(munperm,paste(munperm$Ano))
munperml[[1]]$Perc=(munperml[[1]]$x)/(sum(munperml[[1]]$x))
munperml[[1]]$Perc=munperml[[1]]$Perc*100
#munperml[[1]]$Perc=format(munperml[[1]]$Perc,scientific=F)
munperml[[2]]$Perc=(munperml[[2]]$x)/(sum(munperml[[2]]$x))
#munperml[[2]]$Perc=format(munperml[[2]]$Perc,scientific=F)
munperml[[2]]$Perc=munperml[[2]]$Perc*100

#Culturas temporárias
muntemp=aggregate(principaistemporarias[,c(4)],by=list(Municipio=principaistemporarias$Município,
                                                       Ano=principaistemporarias$Ano),sum,na.rm=T)
muntempl=split(muntemp,paste(muntemp$Ano))

muntempl[[1]]$Perc=(muntempl[[1]]$x)/(sum(muntempl[[1]]$x))
muntempl[[1]]$Perc=muntempl[[1]]$Perc*100

muntempl[[2]]$Perc=(muntempl[[2]]$x)/(sum(muntempl[[2]]$x))
muntempl[[2]]$Perc=muntempl[[2]]$Perc*100


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
spmunperm1994=spmunicipios
spcoordsmun=as.data.frame(coordinates(spmunperm1994))
dados=spmunperm1994@data
dados=cbind(dados,spcoordsmun)
dados$ordem=1:dim(dados)[1]
dadosp1994=merge(dados,munperml[[1]],by.x=c('codigo_ibg'),by.y=c('Municipio'),
             all.x=T,all.y=T)
#Inclusão das bacias hidrográficas
bacias=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/Bacias hidrográficas/baciamun",
                header=T)
colnames(bacias)[1]="Municipio"
dadosp1994=merge(dadosp1994,bacias, by.x=c('codigo_ibg'),by.y=c('Municipio'))


cor=c("gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54","gray57","gray60",
      "gray45","gray48","gray51","gray54")
      
cores=1:length(cor)
cores=as.data.frame(cbind(cores,cor))
colnames(cores)=c("Codigobacia","Cores")
dadosp1994=merge(dadosp1994,cores, by.x=c('Codigobacia'),by.y=c('Codigobacia'))
dadosp1994$Cores=as.character(dadosp1994$Cores)

dadosp1994=dadosp1994[order(dadosp1994$ordem),]
spmunperm1994@data=dadosp1994
#####################################################
plot(spmunperm1994, border=F,lwd=2, axes=F,las=0.01,col=dadosp1994$Cores)
plot(spestado,add=TRUE,lwd=1)
points(spcoordsmun$V1,spcoordsmun$V2,pch=21,col="darkgreen",
       bg=adjustcolor("darkgreen",0.5),cex=dadosp1994$x/7000,lwd=0.9)

#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.51,-24.55))


#Vou plotar legenda a parte
plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="y label", xlab="x lablel")
legend(c("1000", "500"),x=0.5,y=0.6, pt.bg=adjustcolor("darkgreen",0.5),col="darkgreen",
       pch=21,bty="s",pt.cex = 2:1)



#Mapa de valor da produção de cultura permanente em 1994
spmunper1994=spmunicipios
dados=spmunper1994@data
dados$ordem=1:(dim(dados)[1])

dadosp1994=merge(dados,munperml[[1]],by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=T,all.y=T)
dadosp1994$cor[dadosp1994$Perc<0.5|is.na(dadosp1994$Perc)]="gold"
dadosp1994$cor[dadosp1994$Perc>=0.5&dadosp1994$Perc<1.5]="orange"
dadosp1994$cor[dadosp1994$Perc>=1.5&dadosp1994$Perc<3]="darkorange3"
dadosp1994$cor[dadosp1994$Perc>=3]="red"
dadosp1994=dadosp1994[order(dadosp1994$ordem),]


##################Mapa de circulos proporcionais
library(ggmap)
coords=coordinates(spmunper1994)
vp1994=cbind(dadosp1994,coords)
colnames(vp1994)[14]="lat"
colnames(vp1994)[15]="lon"
colnames(vp1994)[11]="Vperm1994"

mapsp=get_map(location = c(lat=-22,lon=-48.5),zoom=6,scale=1)
ggmap(mapsp)
mapPoints <- ggmap(mapsp) +  geom_point(aes(x = lat, y = lon, size = Vperm1994), data = vp1994, alpha = 0.5,color="darkgreen")
mapPoints 
mapPointsLegend <- mapPoints  + scale_size(breaks = c(0, 5000, 10000, 25000, 50000, 75000), labels = c(0, 5, 10, 25, 50, 75), name = "Value 1994")
mapPointsLegend
  
  ######Mapa coroplético
  dadosp1994=dadosp1994[,c(2:7,1,12,13)]
  spmunper1994@data=dadosp1994
  plot(spmunper1994, border=T,lwd=.1, axes=T,las=1,col=spmunper1994@data$cor)
  plot(spestado,add=TRUE,lwd=0.8)
  
  legenda=as.character(c("<0.5%","0.5%-1.5%","1.5%-3%","3%-3.5%"))
  cores=as.character(c("gold","orange","darkorange3","red"))
  legend(x=-46.3,y=-20.5, legenda, fill=cores, bty="s", title="Percentage", cex=0.7 )
  #Adicionando escala ao mapa
  library(maps)
  map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
  source(compassRose(-46.75,-24.55))
  

############################################
#Mapa de valor da produção de cultura permanente em 2014
spmunper2014=spmunicipios
dados=spmunper2014@data
dados$ordem=1:(dim(dados)[1])

dadosp2014=merge(dados,munperml[[2]],by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=T,all.y=T)
dadosp2014$cor[dadosp2014$Perc<0.5|is.na(dadosp2014$Perc)]="gold"
dadosp2014$cor[dadosp2014$Perc>=0.5&dadosp2014$Perc<1.5]="orange"
dadosp2014$cor[dadosp2014$Perc>=1.5&dadosp2014$Perc<3]="darkorange3"
#dadosp2014$cor[dadosp2014$Perc>=3]="red"
dadosp2014=dadosp2014[order(dadosp2014$ordem),]

##Mapa de círculos proporcionais
library(ggmap)
coords=coordinates(spmunper2014)
vp2014=cbind(dadosp2014,coords)
colnames(vp2014)[14]="lat"
colnames(vp2014)[15]="lon"
colnames(vp2014)[11]="Vperm2014"

mapsp=get_map(location = c(lat=-22,lon=-48.5),zoom=6,scale=1)
ggmap(mapsp)
mapPoints <- ggmap(mapsp) +  geom_point(aes(x = lat, y = lon, size = Vperm2014), data = vp2014, alpha = 0.5,color="darkgreen")
mapPoints 
mapPointsLegend <- mapPoints  + scale_size(breaks = c(0, 25000, 50000, 100000, 150000, 255000), labels = c(0, 25, 50, 100, 150, 255), name = "Value 2014")
mapPointsLegend

################Mapa coroplético
dadosp2014=dadosp2014[,c(2:7,1,12,13)]
spmunper2014@data=dadosp2014
plot(spmunper2014, border=T,lwd=.1, axes=F,las=1,col=spmunper2014@data$cor)
plot(spestado,add=TRUE,lwd=0.8)

legenda=as.character(c("<0.5%","0.5%-1.5%","1.5%-3%"))
cores=as.character(c("gold","orange","darkorange3"))
legend(x=-46.3,y=-20.5, legenda, fill=cores, bty="s", title="Percentage", cex=0.7 )
#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.75,-24.55))

#Mapa de valor da produção de cultura temporária em 1994
spmuntemp1994=spmunicipios
dados=spmuntemp1994@data
dados$ordem=1:(dim(dados)[1])

dadost1994=merge(dados,muntempl[[1]],by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=T,all.y=T)
dadost1994$cor[dadost1994$Perc<0.5|is.na(dadost1994$Perc)]="gold"
dadost1994$cor[dadost1994$Perc>=0.5&dadost1994$Perc<1.5]="orange"
dadost1994$cor[dadost1994$Perc>=1.5&dadost1994$Perc<3]="darkorange3"
#dadosp1994$cor[dadosp1994$Perc>=3]="red"
dadost1994=dadost1994[order(dadost1994$ordem),]

##Mapa de círculos proporcionais
library(ggmap)
coords=coordinates(spmuntemp1994)
vt1994=cbind(dadost1994,coords)
colnames(vt1994)[14]="lat"
colnames(vt1994)[15]="lon"
colnames(vt1994)[11]="Vtemp1994"

mapsp=get_map(location = c(lat=-22,lon=-48.5),zoom=6,scale=1)
ggmap(mapsp)
mapPoints <- ggmap(mapsp) +  geom_point(aes(x = lat, y = lon, size = Vtemp1994), data = vt1994, alpha = 0.5,color="springgreen4")
mapPoints 
mapPointsLegend <- mapPoints  + scale_size(breaks = c(0, 5000, 10000, 25000, 50000, 70000), labels = c(0, 5, 10, 25, 50, 70), name = "Value 1994")
mapPointsLegend

##Mapa coroplético
dadost1994=dadost1994[,c(2:7,1,12,13)]
spmuntemp1994@data=dadost1994
plot(spmuntemp1994, border=T,lwd=.1, axes=F,las=1,col=spmuntemp1994@data$cor)
plot(spestado,add=TRUE,lwd=0.8)

legenda=as.character(c("<0.5%","0.5%-1.5%","1.5%-3%"))
cores=as.character(c("gold","orange","darkorange3"))
legend(x=-46.3,y=-20.5, legenda, fill=cores, bty="s", title="Percentage", cex=0.7 )
#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.75,-24.55))

#Mapa de valor da produção de cultura temporária em 2014
spmuntemp2014=spmunicipios
dados=spmuntemp2014@data
dados$ordem=1:(dim(dados)[1])

dadost2014=merge(dados,muntempl[[2]],by.x=c('codigo_ibg'),by.y=c('Municipio'),all.x=T,all.y=T)
dadost2014$cor[dadost2014$Perc<0.5|is.na(dadost2014$Perc)]="gold"
dadost2014$cor[dadost2014$Perc>=0.5&dadost2014$Perc<1.5]="orange"
dadost2014$cor[dadost2014$Perc>=1.5&dadost2014$Perc<3]="darkorange3"
#dadosp1994$cor[dadosp1994$Perc>=3]="red"
dadost2014=dadost2014[order(dadost2014$ordem),]


#Mapa de círculos proporcionais
library(ggmap)
coords=coordinates(spmuntemp2014)
vt2014=cbind(dadost2014,coords)
colnames(vt2014)[14]="lat"
colnames(vt2014)[15]="lon"
colnames(vt2014)[11]="Vtemp2014"

mapsp=get_map(location = c(lat=-22,lon=-48.5),zoom=6,scale=1)
ggmap(mapsp)
mapPoints <- ggmap(mapsp) +  geom_point(aes(x = lat, y = lon, size = Vtemp2014), data = vt2014, alpha = 0.5,color="springgreen4")
mapPoints 
mapPointsLegend <- mapPoints  + scale_size(breaks = c(0, 25000, 50000, 200000, 350000, 500000), labels = c(0, 25, 50, 200, 350, 500), name = "Value 2014")
mapPointsLegend

#Mapa coroplético
dadost2014=dadost2014[,c(2:7,1,12,13)]
spmuntemp2014@data=dadost2014
plot(spmuntemp2014, border=T,lwd=.1, axes=F,las=1,col=spmuntemp2014@data$cor)
plot(spestado,add=TRUE,lwd=0.8)

legenda=as.character(c("<0.5%","0.5%-1.5%","1.5%-3%"))
cores=as.character(c("gold","orange","darkorange3"))
legend(x=-46.3,y=-20.5, legenda, fill=cores, bty="s", title="Percentage", cex=0.7 )
#Adicionando escala ao mapa
library(maps)
map.scale(x=-47.5, y=-24.9,relwidth=0.07,metric=T,ratio=F,cex=0.7)
source(compassRose(-46.75,-24.55))

#3) LISA com clusters de concentração relativa de cada VBP no estado, no primeiro e último
#ano da série (opcional, deixa por último). 

library(spdep)


####Teste e Moran I para autorrelação espacial global

#Principais culturas permanentes 2014

#Exlui Ilha Bela do litoral de São Paulo. Com nã havia conexões com o continente
#estava dando problema na hora de criar a matrix de pesos
spmunicipios2014=spmunicipios[c(1:12,14:645),]
dados=spmunicipios2014@data
dados$ordem=1:dim(dados)[1]

cultperm2014=as.data.frame(munperml[2])
principperm2014=merge(dados,cultperm2014,by.x=c('codigo_ibg'),by.y=('X2014.Municipio'))
principperm2014=principperm2014[order(principperm2014$ordem),]
#principperm2014$logv=log(principperm2014$X2014.x)


#Preciso remover do objeto spmunicipios aqueles objetos que não existem em principperm2014
#Só uso essa tabela abaixo para identificar quais são os objetos
#principperm20142=merge(dados,cultperm2014,by.x=c('codigo_ibg'),by.y=('X2014.Municipio'),all.x=T)
#Removendo de spmunicipios2014, os municípios que estão ausentes em principperm2014
spmunicipios2014=spmunicipios2014[-c(309,456,531,561,15,193,90,98,108,148,163,164,498,575),]
#View(spmunicipios2014)

#Cria neighbors por contiguidade
spneighbors=poly2nb(spmunicipios2014)
coords=coordinates(spmunicipios2014)
#Plotanto mapa de vizinhança
plot(spneighbors,coords,col="black")


#####Spatial weight matrix based on contiguity
weigthmatrix=nb2listw(spneighbors)
summary(weigthmatrix)

#Pelo teste, há autocorrelação espacial global
moran.test(principperm2014$X2014.x,weigthmatrix)
moran.plot(principperm2014$X2014.x,weigthmatrix)

#Local Moran
lmp2014=localmoran(principperm2014$X2014.x,weigthmatrix)

#Não consegui criar os clusters LISA. Preciso falar com o Gori para entender quais são os próximos
#passos para criar esses clusters


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
ipca=ipca[c(15:36),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1994. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[22]=1
ipca$V3[21]=ipca$V1[21]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}


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
vculttemp=vculttemp[vculttemp$Ano>=1994,]
ipca=ipca[ipca$Data<2015,]
climafinalanosp=climafinalanosp[climafinalanosp$Ano>=1994,]
climafinalanosp=climafinalanosp[,c(1,4:18)]
climafinalestsp=climafinalestsp[climafinalestsp$Ano>=1994,]
climafinalestsp=climafinalestsp[,c(1,3:59)]

#######Deflacionando série de dados
lvculttemp2=list()
lvculttemp=split(vculttemp, paste(vculttemp$Municipio))

for (i in 1:length(lvculttemp)){
  temp=cbind(lvculttemp[[i]]$Municipio,lvculttemp[[i]]$Ano,lvculttemp[[i]]$VPTemporario/ipca$V2)
  colnames(temp)=c("Municipio","Ano","VTReal")
  lvculttemp2[[i]]=temp
}

vculttemp=do.call(rbind.data.frame,lvculttemp2)
vculttemp$logreal=log(vculttemp$VTReal)
vculttemp$logreal[!is.finite(vculttemp$logreal)]=NA

#Padronizando data de área colhida e aplicando log
areaculttemp=areaculttemp[areaculttemp$Ano>=1994,]
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

#Tenho que atribuir binária para os municípios da bacia do rio
#jundiaí: Salto (3545209), Itupeva (3524006), Jundiaí (3525904),
# Varzea Paulista (3556503), Campo Limpo Paulista (3509601),Mairiporã (3528502)

#não estão presentes 
#Varzea Paulista (3556503) 1994-2010
#Campo Limpo Paulista (3509601) 1995-2010 (mas há problemas com os dados entre 2001-2009. todos os anos tem valor 1)
#Mairiporã (3528502) 1994,1995,2000 (série bem incompleta)

#ACho que vou manter a seŕie entre 1994 e 2014 apenas com Salto,
#Jundiaí e Itupeva. Não compensa alterar todo o banco de dados apenas por
#causa de três municípios com problemas de dados
#Vou atribuir binárias aos três municípios logo antes de rodar o painel


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

#################################
#################################
#Antes da regressão propriamente dita preciso fazer o teste de dependência espacia
#, spatial lag e erro espacial para todos os anos
#Moran's I test
# Há dependÊncia espacial mostrada pelo Moran I 
#para todos os anos. Preciso refazer o modelo de dados em painel porque 
#nãao reordenei o painel de acordo com a ordem dos municípios no shape file
#sem essa reordenação os resultados não sao válidos
logarea=vculttemp[,c(1,2,4)]
logareal=split(logarea,paste(logarea$Ano))
ordem=spmunicipios2014@data[,c(7,9)]
ordem$bin=1:dim(ordem)[1]
panelmoran=list()

for (i in 1:length(logareal)){
morani=merge(ordem,logareal[[i]],by.x=c('codigo_ibg'),by.y=c('Municipio'))
morani=morani[order(morani$bin),]

testemoran=moran.test(morani$logreal,weigthmatrix)
#moran.plot(morani$logreal,weigthmatrix)
estimates=as.data.frame(rbind((testemoran$estimate)))
pvalue=as.data.frame(testemoran$p.value)
testemoran=cbind(estimates,pvalue,logareal[[i]]$Ano[1])
colnames(testemoran)[4]=c("Pvalue")
colnames(testemoran)[5]=c("Ano")
testemoran=testemoran[,c(5,1:4)]
panelmoran[[i]]=testemoran

}

moranI=do.call(rbind.data.frame,panelmoran)
#library(Hmisc)
#moranI=round(moranI,4)
#setwd("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP")
#latex(moranI,rowname = NULL)


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

fe=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="within",spatial.error = T)
summary(fe)

re=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="random",spatial.error = T)
summary(re)

sphtest(x = re, x2 = fe)

#Modelos espaciais de efeitos fixo e efeitos aleatórios sem erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="within",lag=T)
summary(fe)

re=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="random",lag=T)
summary(re)

sphtest(x = re, x2 = fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios com erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="within",lag=T, spatial.error = T)
summary(fe)

re=spgm(formula=fm,data=vculttemp,index=NULL,listw=weigthmatrix, model="random",lag=T,spatial.error = T)
summary(re)

sphtest(x = re, x2 = fe)





######################################################################
#####################################################################
#Modelo de dados em painel para culturas permanentes
#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Artigo VBPSP/IPCA/IPCA.csv",header=T)
ipca=ipca[c(15:36),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 1994. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2014. Multiplico o valor monetário po esse número
ipca$V3[22]=1
ipca$V3[21]=ipca$V1[21]
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
vcultperm=vcultperm[vcultperm$Ano>=1994,]
ipca=ipca[ipca$Data<2015,]
climafinalanosp=climafinalanosp[climafinalanosp$Ano>=1994,]
climafinalanosp=climafinalanosp[,c(1,4:18)]
climafinalestsp=climafinalestsp[climafinalestsp$Ano>=1994,]
climafinalestsp=climafinalestsp[,c(1,3:59)]

#######Deflacionando série de dados
lvcultperm2=list()
lvcultperm=split(vcultperm, paste(vcultperm$Município))

for (i in 1:length(lvcultperm)){
  perm=cbind(lvcultperm[[i]]$Município,lvcultperm[[i]]$Ano,lvcultperm[[i]]$VPPermanente/ipca$V2)
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
fe=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="within",lag=T)
summary(fe)

re=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="random",lag=T)
summary(re)

sphtest(x = re, x2 = fe)


#Modelos espaciais de efeitos fixo e efeitos aleatórios com erro espacial e
#com lag +  teste de hausman
fe=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="within",lag=T, spatial.error = T)
summary(fe)

re=spgm(formula=fm,data=vcultperm,index=NULL,listw=weigthmatrix, model="random",lag=T,spatial.error = T)
summary(re)

sphtest(x = re, x2 = fe)

