#Instalar pacote de dependência espacial
#install.packages('spdep')

library(spdep)

data(columbus)
mydata=columbus
attach(mydata)
Y=cbind(CRIME)
X=cbind(INC,HOVAL)
xy=cbind(mydata$X,mydata$Y)
#Pontos mostrando ondes a observações estão e qual a distância entre eles
neighbors=col.gal.nb
#mostra onde os objetos estão. Acho que é um arquivo casado com o anterior neighbors
coords=coords

#Neighbors summary
summary(neighbors)
plot(neighbors,coords)

#Descriptive statistics
summary(X)
summary(Y)

#OLS regression
olsreg=lm(Y~X)
summary(olsreg)

#Spatial analysis based on contiguity

#Spatial weight matrix based on contiguity
#VOu usar isso para calcular o local moran
listw=nb2listw(neighbors)
summary(listw)

#Moran's I test
# Há dependÊncia espacial mostrada pelo Moran I statistic=0.485770914 e p-value = 4.578e-08
moran.test(CRIME,listw)
moran.plot(CRIME,listw)

#Lagrange multiplier test for spatial lag and spatial error dependencies
#De acordo com os p-values nós temos dependência espacial e também podemos 
#usar modelo com lag e com erro espacial
lm.LMtests(olsreg,listw,test=c("LMlag","LMerr"))

#Spatial lag model
#Nesse modelo, RHO é o parametro de deppendência espacial Rho: 0.40389
# esse parâmetro é significante p-value: 0.0037154, ou seja há dependência espacial
spatial.lag=lagsarlm(CRIME~INC+HOVAL,data=mydata,listw)
summary(spatial.lag)

#Spatial error model
spatial.error=errorsarlm(CRIME~INC+HOVAL,data=mydata,listw)
summary(spatial.error)


