## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: MÉTODOS DE CLUSTERING
## 
##  4. Fuentes:
##     https://www.educba.com/clustering-methods/"


## ------------------------------------------------------------------------------------
# Guardamos los OUTPUTS:
sink("OUTPUTS.txt")

# Cargamos la libreria:
library(ISLR)

# Cargamos los datos:
data.NCI60 <- NCI60
#Guardamos los datos:
write.csv(data.NCI60, file = "NCI60.csv", row.names = F)

# NOmbre de la base e datos 1
print("# NOmbre de la base e datos 1")
names(data.NCI60)

# Separamos los datos:
datos.nci <- data.NCI60$data

#Diemncion de los datos anteriores:
print("#Diemncion de los datos anteriores:")
dim(datos.nci)

# Observación de los datos:
print("# Observación de los datos:")
head(datos.nci)[, 1:6]


# Estandarización de los datos
datos.nci <- scale(datos.nci, center = TRUE, scale = TRUE)
print("# Estandarización de los datos")
head(datos.nci)[, 1:6]


# Tipos de cáncer distintos en el set de datos
print("# Tipos de cáncer distintos en el set de datos")
unique(NCI60$labs)


# Número de muestras por tipo de cáncer
print("# Número de muestras por tipo de cáncer")
table(NCI60$labs)


## ------------------------------------------------------------------------------------
# Matriz distancia euclídea entre observaciones
datos.nci.euc <- dist(datos.nci, method = "euclidean")

# Representación de dendogramas
par(mfrow = c(1,2))

# Representamos en dentogramas los datos anteriores:
pdf("Dentogramas.pdf")

plot1 <- plot(hclust(datos.nci.euc, method = "complete"), 
              labels = NCI60$labs, 
              main = "Complete linkage", 
              xlab = "",
              ylab = "",
              cex = 0.3,
              sub = "")

plot2 <- plot(hclust(datos.nci.euc, method = "average"), 
              labels = NCI60$labs, 
              main = "Average linkage", 
              xlab = "",
              ylab = "",
              cex = 0.3,
              sub = "")


plot3 <- plot(hclust(datos.nci.euc, method = "single"), 
              labes = NCI60$labs, 
              main = "Single linkage", 
              xlab = "",
              ylab = "",
              cex = 0.4,
              sub = "")

plotc <- c(plot1, plot2, plot3)

for(x in plotc){
  x
}


dev.off()

# Resultado:

"Como se observa en cada uno de los tres dendogramas, el tipo de linkage escogido afecta el resultado del agrupamiento (las hojas de un dendograma usando single linkage suelen unirse una a una). Claramente, las líneas celulares de un solo tipo de cáncer tienden a agruparse juntas."

sink()