##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##

##Nombre: Revelo Betancourt Gabriela Elizabeth

# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()


dir <- "C:/Trabajo01_Modelos_Lineales/Modelos-Lineales-Trabajos-master-RStudio"
setwd(dir)
data<-read.table("data.txt",header=TRUE,dec=',',sep='\t')
class(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE


edad_mínima<-min(data[,"Edad"],na.rm=TRUE)
edad_media<-mean(data[,"Edad"],na.rm=TRUE)
edad_máxima<-max(data[,"Edad"],na.rm=TRUE)

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

genero_femenino<-subset(data,subset=data[,"Genero"]=="Femenino")
table(genero_femenino[,"Genero"])

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

sujetos_dependientes<-subset(data,subset=data[,"Dependiente"]=="Si")
edad_mínima_dependientes<-min(sujetos_dependientes[,"Edad"],na.rm=TRUE)
edad_media_dependientes<-mean(sujetos_dependientes[,"Edad"],na.rm=TRUE)
edad_máxima_dependientes<-max(sujetos_dependientes[,"Edad"],na.rm=TRUE)

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipo_elementos_variable<-character()
for(i in 1:ncol(data))
{
  tipo_elementos_variable[i]<-typeof(data[,i])
}
tipo_elementos_variable


# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

clase_variable<-character()
for(i in 1:ncol(data))
{
  clase_variable[i]<-class(data[,i])
}
clase_variable

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables

aux1<-logical()
aux2<-mumeric()
for(i in 1:ncol(data))
{
  aux[i]<-is.numeric(data[,i])
}
nueva_data<-data[,aux]
for(i in 1:ncol(nueva_data))
{
  p[i]<-mean(nueva_data[,i],na.rm=TRUE)
}
print(p)

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

valores_perdidos<-numeric()
for(i in 1:ncol(data))
{
  valores_perdidos[i]=sum(is.na(data[,i]))
}
valores_perdidos<-100*valores_perdidos/nrow(data)
print(valores_perdidos)

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

sujetos1<-subset(data,subset=data[,"Edad"]>40)

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

sujetos2<-subset(data,subset=data[,"Vivienda"]=="Propia")

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiares.

sujetos3<-subset(data,subset=data[,"Cargas"]>2)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

sujetos4<-subset(data,subset=data[,"Deuda"]>=500 & data[,"Dias_Atraso"]>8)

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

sujetos5<-subset(data,subset=data[,"Score"]>=900 &  data[,"Edad"]<=35 & data[,"Numero_TC"]>3)


# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(data[,"Edad"],col="red")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(data[,"Edad"],col="green")


