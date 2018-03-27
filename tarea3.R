#Tarea3 Aplicada: Supuesto de independencia en los errores.
install.packages("readr")
library("readr")
cadata <- read_table2("GitHub/tarea_aplicada_3/cadata.txt")
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
#Prueba gráfica:
x11()
plot(residualesx,residuales)
#Pruebas estaidsticas:
#Prueba de rachas:
library("tseries")
runs.test(residuales)
