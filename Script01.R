##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: FABRICIO SANCHEZ


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()
dir <-"C:/Users/Crystal3/Desktop"
setwd(dir)
list.files()
data <- read.table("data.txt",header=TRUE, dec=",",sep="\t")
str(data)
names(data)
is.list(data) #TRUE
is.atomic(data) #FALSE
is.numeric(data) #FALSE
is.complex(data) #FALSE
typeof(data) #"LIST"
#Es una lista.

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE
edad <- data[,"Edad"]
mean(edad) #NA
mean(edad,na.rm=TRUE) #41.73808
min(edad) #NA
min(edad,na.rm=TRUE) #14
max(edad) #NA
max(edad,na.rm=TRUE) #112

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()
genero <- data[,"Genero"]
data_f<- subset(data,subset=data[,"Genero"]=="Femenino")
table(data_f[,"Genero"]) #femenino=6183 masculino=0 sin info=0

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

data_dep<-subset(data, subset=data[,"Dependiente"]=="Si")
table(data_dep[,"Dependiente"]) #no=0 si=804 sin info=0
edad_dep<-data_dep[,"Edad"]
min_dep<-min(edad_dep,na.rm =TRUE )
min_dep #22
max_dep<-max(edad_dep, na.rm =TRUE)
max_dep #66
med_dep<-mean(edad_dep,na.rm = TRUE)
med_dep #41.28607


# 2.5 Identificar el tipo de elementos que contiene cada variable.

tipe<- numeric(ncol(data))
for (i in 1:ncol(data)){
  tipe[i]<- typeof(data[,i])
}
tipe #integer

# 2.6 Identificar la clase de cada variable (columna).

class<- numeric(ncol(data))
for (i in 1:ncol(data)){
  class[i]<- class(data[,i])
}
class #varia entre integer factor y numeric

# 2.7 Calcular la media de todas las variables numéricas (double, integer).

med<- numeric(ncol(data))
for (i in 1:ncol(data)){
  med[i] <- mean(data[,i],na.rm =TRUE)
}
med

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
na<- numeric(ncol(data))
for (i in 1:ncol(data)){
  na[i] <- sum(is.na(data[,i]))
}
na
prop.table(na)

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.

data_selecmayor <- subset(data,subset=data[,"Edad"]>40)
table(data_selecmayor[,"Edad"])

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

data_selecvivprop <- subset(data,subset=data[,"Vivienda"]=="Propia")
table(data_selecvivprop[,"Vivienda"])

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

data_seleccarg <- subset(data,subset=data[,"Cargas"]>2)
table(data_seleccarg[,"Cargas"])

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

data_selecdeuda <- subset(data,subset=data[,"Deuda"]>=500)
table(data_selecdeuda[,"Deuda"])
data_atraso<-subset(data,subset=data[,"Dias_Atraso"]>8)
table(data_atraso[,"Dias_Atraso"])
#en una sola tabla
data_selecdeudayatraso<- subset(data,(subset=data[,"Deuda"]>=500)&(subset=data[,"Dias_Atraso"]>8))
table(data_selecdeudayatraso[,"Deuda"],data_selecdeudayatraso[,"Dias_Atraso"])

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC)
#score
data_selecscore<-subset(data,subset=data[,"Score"]>=900)
table(data_selecscore[,"Score"])
#Edad
data_selecedad<-subset(data,subset=data[,"Edad"]<=35)
table(data_selecedad[,"Edad"])
#Tarjetas de Credito
data_selecTC<-subset(data,subset=data[,"Numero_TC"]>3)
table(data_selecTC[,"Numero_TC"])
#En una misma tabla
data_selecscoredadTC <- subset(data,(subset=data[,"Score"]>=900)&(subset=data[,"Edad"]<=35)&(subset=data[,"Numero_TC"]>3))
table(data_selecscoredadTC[,"Score"],data_selecscoredadTC[,"Edad"],data_selecscoredadTC[,"Numero_TC"])

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(edad,col = "red",main="Edad")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green

boxplot(edad,col = "green",main="Edad")