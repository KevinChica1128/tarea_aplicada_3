#Tarea3 Aplicada: Supuesto de independencia en los errores.
install.packages("readr")
library("readr")
cadata <- read_table2("cadata.txt")
View(cadata)
sample(1:20640,1)
## El número aleatorio generado fue 15529 ##
# Eliminamos las filas que no necesitamos, nos quedamos con las filas desde la 15529 hasta la 16029 #
cadata<- cadata[-c(1:15528,16029:20640),]

#Ajuste del modelo completo:
Regresion<- lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
summary(Regresion)
#Valors ajustados y residuales del modelo:
yajustados<-fitted(Regresion)
residuales<-residuals(Regresion)
residualesx<-c()
for (i in 1:length(residuales)) {
  residualesx[i]<-residuales[i-1]
}
residualesx[1]=0
#Prueba gráfica:
x11()
acf(residuales,ci=0.95,lag.max=200,type = c("correlation"),main="Correlograma",ylab="Autocorrelación",xlab="Residuales")


x11()
par(mfrow=c(2,1))
plot(1:500,residuales,type="o",xlim=c(0,250))
plot(1:500,residuales,type="o",xlim = c(250,500))

x11()
plot(residualesx,residuales,xlim=c(100000,200000))


#Pruebas estaidsticas:
#Prueba de rachas:
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuales)) {
  if (residuales[i]>0){
    residualesfactor[i]=1
  }
  if (residuales[i]<0){
    residualesfactor[i]=-1
  }
  }
runs.test(factor(residualesfactor))

#Prueba Durbin-Watson:
install.packages("lmtest")
library("lmtest")
dwtest(Regresion,alternative = c("two.sided"))

#Prueba de Ljung-Box:
Box.test(residuales)
