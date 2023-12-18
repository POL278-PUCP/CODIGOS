REPASO-CODES
============================================================

Contribuidores:

-  Co-authored-by: Luis Eduardo Valverde Ramos <https://github.com/LuisEduardoValverdeRamos>

-  Co-authored-by: Javier Flores Roque <>

------------------------------------------------------------------------------------------

Extraído del archivo `index.Rmd` Plantilla/manual para E1
 
`NOTA`: Crear una organizacion para Estadistica 1 y transferir este repositorio

````markdown
---
title: "REPASO: ESTADISTICA PARA EL ANALISIS POLITICO 1 (POL278-0590)"
output:
  rmdformats::downcute:
    downcute_theme: "chaos"
    self_contained: true
    thumbnails: false
    lightbox: true
    gallery: false
    highlight: github 
    code_folding: "hide"
---
````

PD1. Introducción al R y a base de datos, Análisis descriptivo en R
============================================================
## Importar data:
```{r}
#getwd() 
#setwd("")
#dir()

library(rio)
data1 = import("Mosteller.csv")

#names(data1)
#str(data1)
#class(data1) 
#head(data1)

#table(data1$race)
```


PD2. Análisis descriptivo (de variables) en R e introducción a la visualización de datos
============================================================
  - Librerias para el analisis descriptivo:
```{r}
library(psych)  
library(DescTools) #calcular moda y mediana. 
library(Hmisc) #Usar función describe.
library(moments) #Asimetría y curtosis.
library(ggplot2)
```

## 1. CATEGÓRICA NOMINAL: solo moda y graficas: pie y de preferencia barras
============================================================
```{r}
#table(data1$var) 
#class(data1$var)
#str(data1$var)

#data1$var[data1$var == 9]= NA #solo si es necesario borrar caso perdido

data1$Var = as.factor(data1$var)
#levels(data$var) # Vemos cuales son los levels del factor

data1$var = factor(data1$var,
                   levels = c(1:2),  #si hay datos perdidos, sino sería: levels = levels(data1$var)
                   labels = c("x", "y"),
                 ordered = F)
```

```{r}
sum(is.na(data$var)) 
data = data[complete.cases(data$var),]
```

```{r}
table(data$var)
prop.table(table(data$var))*100 
```

```{r MODA: describe}
library(Hmisc)
describe(data$var) # Moda
```
 
  + Respecto a la moda, es decir, con mayor frecuencia, hay mas docentes (`1067`) 
  del género femenino en Lima Metropolitana.
```{r}
m = 100 - 4.6072508 # % - MODA%
m
d = (1324 - 691) - 52 # n - Moda
d
``` 
  + Finalmente, el porcentaje que no es representado por la moda, a saber, 
  la variacion modal del genero de docentes es de 19.47% (`258`).
  
## **GRAFICO DE BARRAS (BARPLOT)** (NO OLVIDAR EL DATA FRAME) (PREFERIR BARRAS)
```{r message=FALSE,warning=FALSE,eval=TRUE,fig.show='hold',fig.width=9.5,fig.height=5.55, fig.align="center", fig.cap="Figure 1: Docentes según sexo (ENDO 2020)."}
grafico1 <- table(data$Type) 
grafico1 = as.data.frame(grafico1) 
colnames(grafico1) = c("Reg","Freq")

library(ggplot2) 
bp = ggplot(grafico1, aes(x = reorder(Reg,Freq), y = Freq, fill = Reg)) +
  geom_bar(stat = "identity") +
    coord_flip() + 
    labs(title = "Docentes según sexo (ENDO 2020)", 
         y = "Frecuencias", x = "Categorías", 
         subtitle = "", caption = "ENDO 2020") +
    theme(plot.title = element_text(hjust = 1)) + 
    theme(panel.background = element_rect(fill = "white", colour = "white")) + 
    geom_text(aes(label = Freq), vjust = 0.5, color ="Black", size = 5)
bp
``` 
 
## GRAFICO DE SECTORES (PIE CHART)
```{r message=FALSE,warning=FALSE,eval=TRUE,fig.show='hold',fig.width=9.5,fig.height=5.55, fig.align="center", fig.cap="Figure 1: Docentes según sexo (ENDO 2020)."}
pc = ggplot(grafico1, aes(x = "", y = Freq, fill = Reg)) +
  ggtitle("Índice de democracia") + #Agregamos el título
  geom_bar(stat = "identity",
           width = 1,
           color = "white") +
  coord_polar("y", start = 0) +
  theme_void()
pc + scale_fill_brewer(palette = "Dark2") #Color del gráfico
```

## **INTERPRETACION:**

 + Del total de docentes encuestadxs (`1325`) en la ENDO 2020, la mayoría, 
 el 80.5% (`1067`) son mujeres, mientras que menos del 20% (`258`) representan 
 a docentes hombres.  
 
Otra opcion a la interpretacion del grafico:

  + Del total de docentes encuestadxs (`1325`) en la ENDO 2020, mas del 60% 
  de docentes encuestados pertencen a una condicion laboral reconocida como 
  nombrados en la institucion educativa en la cual trabajan, seguido de un 35% 
  que son docentes contratados por concurso publico y en menos proporcion solo 
  7 docentes son contratados con otra modalidad, exactamente: 64.72% (`857`) 
  nombrados, 34.74% (`460`) por concurso público y 0.52% (`7`) por otro modalidad.



## 2. CATEGORICA ORDINAL: solo mediana y moda. cuartiles y percentiles. graficas pie, barras y boxplot.
============================================================
```{r}
#table(data$var)
#class(data$var)
#str(data$var)
#levels(data$var) # Vemos cuales son los levels del factor

#data$var[data$var == 9]= NA #solo si es necesario

data$Var= as.factor(data$var) 
data$var=factor(data$var,
                 levels = levels(data$var),
                 labels = c("Muy satisfecho","Satisfecho","Insatisfecho","Muy  insatisfecho"),
                 ordered = T)
```

```{r}
sum(is.na(data$var)) 
data = data[complete.cases(data$var), ]
```

```{r}
table(data$var)
prop.table(table(data$var))*100
```

```{r MODA}
library(DescTools)
Mode(dataDRE$P1_8, na.rm = T)
```

 + Respecto a la moda, es decir, con mayor frecuencia, 
 hay mas personas jovenes (`35`) que tienen 25 años. 
```{r}
e = 100 - 64.7280967 # % - MODA%
e
f = 1324 - 857 # n - Moda
f
``` 
 + El porcentaje que no es representado por la moda, a saber, 
 la variacion modal es de 85% (`820`).

```{r}
Median(dataDRE$P1_8, na.rm = T)
```
  + La mediana de los docentes encuestados se ubica en la segunda (`2`) escala.

```{r}
library(moments)
skewness(data$q2, na.rm = T) #asimetría
```

  + El resultado de la distribucion de los datos de la variable es asimetrica negativa (`-0.5063559`); esto es, hay una gran acumulacion de **valores altos** / **de datos** a la derecha de la moda en el último cuartil de las personas consideradas de derecha. Esto confirma lo que advertimos en el histograma. La cola de los valores de la edad de los encuestados está sesgada a la izquierda
  
  + El resultado de la distribucion de los datos de la variable es asimetrica positiva (`0.6628117`); esto es, hay una gran acumulacion de valores altos a la izquierda de la moda en el último cuartil de las encuestados, quienes son los más jovenes. Esto confirma lo que advertimos en el histograma. La cola de los valores de la edad de los encuestados está sesgada a la derecha.
  
## Medidas de posición  
```{r}
summary(data$var) #Mejor opción
#quantile(data$var, na.rm=T)
IQR(data$var, na.rm = T)
```

```{r}
library(Hmisc)
describe(data$var)
```

Respecto a los percentiles 25% (q1), 50% (q2) y 75% (q3): 

  + En relación al primer cuartil, al menos 25% de la población electoral residente en el país no se considerarían ni de izquierda ni de derecha con (`50`) puntos. Es decir, tienden hacia el centro en el espectro político.
  
  + Además, se evidencia que al menos el 50% de la población electoral residente en el país tiende a lo más a un puntaje de (`60`), es decir, están cercanos hacia a la centro-derecha que la izquierda respecto a su posición política.

  + También, el 75% de la población electoral residente en el país están muy cerca de considerarse de derecha con a lo más (`83`) puntos, es decir, con un valor alto. En ese sentido, hay más personas que se consideran de derecha que de izquierda.

  ++ El valor mínimo del total de personal es de 1 trabajador (nombrado o contratado).

  ++ El valor máximo del total de personal es de 162 trabajador (nombrado o contratado).

  ++ El 50% de los valores, es decir, el rango de trabajadores del total de personal  fluctúa entre (`50`) puntos y (`90`) puntos, por tanto, es posible decir que puntúan en puntajes altos, es decir, tienden más hacia a la derecha, pero en suma, hay una relativa dispersión de los datos.

  + La distancia entre el primer y el tercer cuartil, a saber, el rango intercuartil del personal en total es de (`3`) trabajadores (nombrado o contratado).

1. Si no hay casos atipicos:
  
  + No encontramos valores atípicos.
  
2. si hay casos atipicos:
  
  + Un suceso fortuito es que hay algunxs consideradxs de centro-izquierda e izquierda que no consideran el estatismo (12 casos en los valores más bajos, es decir, cercano a los que se consideran de izquierda). Lo que, en efecto, indica una mayor dispersión en general respecto a las puntuaciones de estatismo.


## **GRAFICO DE BARRAS (BARPLOT)**
```{r message=FALSE,warning=FALSE,eval=TRUE,fig.show='hold',fig.width=9.5,fig.height=5.55, fig.align="center", fig.cap="Figure 1: Docentes según sexo (ENDO 2020)."}
grafico1 <- table(data$Type) 
grafico1 = as.data.frame(grafico1) 
colnames(grafico1) = c("Reg","Freq")

library(ggplot2) 
bp = ggplot(grafico1, aes(x=reorder(Reg,Freq), y=Freq, fill=Reg)) +
  geom_bar(stat = "identity") +
    coord_flip() + 
    labs(y="Frecuencias", x="Categorías", 
         title="Índice de democracia", subtitle = "", caption = "ENDO 2020") +
    theme(plot.title = element_text(hjust = 1)) + 
    theme(panel.background=element_rect(fill = "white", colour = "white")) + 
    geom_text(aes(label=Freq), vjust=0.5, color="Black", size=5)
bp
``` 

## **INTERPRETACION:**

  + Del total de encuestadxs (`1325`), la mayoría, el 80.5% (`1067`), de docentes 
  encuestadxs en la ENDO 2020 son mujeres, mientras que menos del 20% (`258`) 
  representan a docentes hombres. 
  
En caso haya más escalas:
  
  + Del total de encuestadxs (`1325`), la mayoría, el 80.5% (`1067`), de docentes 
  encuestadxs en la ENDO 2020 se encuentran en la primera escala magisterial, solo 
  17 docentes (1.9%) se ubican en la escala magisterial 6.  
 
## **GRAFICO DE CAJAS (BOXPLOT)**
```{r message=FALSE,warning=FALSE,eval=TRUE,fig.show='hold',fig.width=9.5,fig.height=5.55, fig.align="center", fig.cap="Figure 4: Docentes según sexo (ENDO 2020)."}
library(ggplot2)
library(plotly)
p2 <- ggplot(datalapop, aes(x="", y=psc8, color ="")) + 
  geom_boxplot() + coord_flip() +
  theme(legend.position = "top", axis.text.y = element_blank(), 
        panel.background=element_rect(fill = "white", colour = "white")) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  labs(x="", y="Index", 
       title = "Edad de los encuestados",  subtitle = "", caption = "ENDO 2020")
ggplotly(p2)
```

## **INTERPRETACION:**

  + PARA BOXPLOT:  Los datos con respecto al quehacer 1: comunicacion y retroalimentacion se encuentra concetrados en las categorias dificil (41.2%) y facil (43.3%), mientras que a los extremos encontramos las categorias muy dificil (5.8%) y muy facil (9.5%)




## 3. VARIABLE NUMERICA: todo: tendencia central, valores percentiles, dispersion y distribucion. graficas: boxplot e histograma
============================================================
```{r}
#table(data$var)
#class(data$var) 
#str(data$var)
data$var[data$var == 9]= NA #solo si es necesario

data$var = as.numeric(data$var)
```

```{r}
sum(is.na(data$var)) 
data = data[complete.cases(data$var),]
```

## Medidas de centralidad
```{r}
summary(data$var) # Mediana # Media # Percentiles

library(DescTools)
Mode(data$var, na.rm = T)  # Moda
#table(data$var)
#prop.table(table(data$var))*100

describe(data$q2) # Percentiles y los valores mas altos y más bajos, para sacar outliers. 
```

  + El promedio de la edad de los docentes encuestadxs en la ENDO 2020, se ubica por debajo de los 50 años, exactamente en (`48.08`) años.
  
  + La mediana del personal total es de (`2`) trabajadores (nombrado o contratado).
  
  - La moda, es decir, con mayor frecuencia, hay mas personas adultas (`52`) que tienen 61 años.
```{r}
a = 100 - 4.6072508 # % - MODA%
a
b = (1324 - 691) - 52 # n - Moda
b
```
  - El porcentaje que no es representado por la moda, a saber, la variación modal de la edad de los docentes encuestados es de 95.39% (`581`).  
## Medidas de posición  
```{r}
quantile(data$var, na.rm=T) # Percentiles 
```

Respecto a los percentiles 25% (q1), 50% (q2) y 75% (q3): 

  ++ En relación al primer cuartil, al menos 25% de los encuestadxs tiene a lo más (`26`) años.
  
  ++ Además, se evidencia que al menos el 50% de los encuestadxs tiene a lo más (`36`) años.
  
  ++ También, el 75% de los encuestadxs tiende a lo más (`46`) años. 
  
## Medidas de dispersión
```{r}
sd(data$q2, na.rm = T) #desviación
var(data$q2, na.rm = T) #varianza
IQR(data$var, na.rm = T) #na.rm = T no considera los valores perdidos
```

  + La desviación estándar del total de los trabajadores respecto a la cantidad del personal de los nombrados o contratados es de (`2`) trabajadores.

  + La varianza es de (`1`).
  
  - El valor mínimo del total de personal es de 1 trabajador (nombrado o contratado).

  - El valor máximo del total de personal es de 162 trabajador (nombrado o contratado).

  - El 50% de los valores, es decir, el rango de trabajadores del total de personal oscila entre (`1`) y (`162`) trabajadores (nombrado o contratado).
  
  + La distancia entre el primer y el tercer cuartil, a saber, el rango intercuartil del personal en total es de (`3`) trabajadores (nombrado o contratado).

1. Si no hay casos atipicos:
  
  + No encontramos valores atípicos.
  
2. si hay casos atipicos:
  
  + Un suceso fortuito es que hay algunxs consideradxs de centro-izquierda e izquierda que no consideran el estatismo (12 casos en los valores más bajos, es decir, cercano a los que se consideran de izquierda). Lo que, en efecto, indica una mayor dispersión en general respecto a las puntuaciones de estatismo.


## Medidas de distribución:
```{r}
library(moments)
skewness(data$q2, na.rm = T) #asimetría
#hist(dataDRE$P1_2)

kurtosis(data$q2, na.rm = T) #curtosis
```

  ++ El resultado de la distribución de los datos de la variable es asimétrica negativa (`-0.2588208`); esto es, hay una gran acumulación de valores altos a la derecha de la moda en el último cuartil de la edad de los docentes encuestadxs. Es decir, la mayor cantidad de encuestados se encuentran dentro de las edades más altas, en otras palabras, son en su mayoría más viejos. Asimismo, existe una gran concentración de edades de docentes en torno a la edad de 57 años. Esto confirma lo que advertimos en el histograma. La cola de los valores de la edad de los encuestados está sesgada a la izquierda
  
  ++ El resultado de la distribucion de los datos de la variable es asimetrica positiva (`0.6628117`); esto es, hay una gran acumulacion de valores altos a la izquierda de la moda en el último cuartil de las encuestados, quienes son los más jovenes.  Es decir, la mayor cantidad de encuestados se encuentran dentro de las edades más altas, en otras palabras, son en su mayoría más viejos. Asimismo, existe una gran concentración de edades de docentes en torno a la edad de 57 años. Esto confirma lo que advertimos en el histograma. La cola de los valores de la edad de los encuestados está sesgada a la derecha.
  

FIJARNOS EN EL RESULTADO: 

  ++ La kurtosis es de tipo Leptocúrtica (`42.66`), es decir, hay alta concentración alrededor de la moda. Es decir, la distribucion de la edad respecto a la distribución normal indica que está apuntada.
  
  ++ La kurtosis es de tipo Platicúrtica (`-42.66`), es decir, hay menos concentración alrededor de la media. Es decir, la distribucion de la edad respecto a la distribución normal indica que está achatada.
  
  ++ La kurtosis es de tipo Mesocúrtica (`0`), es decir, no hay tanta concentración alrededor de la media.
  
## **GRAFICO DE CAJAS (BOXPLOT) E HISTOGRAMA (HIST)**
```{r fig.show='hold', message=FALSE,warning=FALSE,eval=TRUE,fig.width=9.5,fig.height=5.55, fig.align="center", fig.cap="Figure 1: Docentes según sexo (ENDO 2020)."}
library(ggplot2)
library(plotly)
p1 <- ggplot(datalapop, aes(x = "", y = psc8, color ="")) + 
  geom_boxplot() + coord_flip() +
  theme(legend.position = "top", axis.text.y = element_blank(), panel.background=element_rect(fill = "white", colour = "white")) + 
  geom_jitter(shape = 16, position = position_jitter(0.2)) + 
  labs(title = "Edad de los encuestados", x = "", y ="Index", subtitle = "", caption = "ENDO 2020")
ggplotly(p1)

# HIST:
ggplot(datalapop, aes(x = q2, color = "")) + 
  geom_histogram(fill = "white", alpha = 0.5, position = "identity") +  #agregar bins = 30 o breaks=seq()
    labs(title ="Edad de los encuestados", y = "", x="Index", subtitle = "", caption = "ENDO 2020")+
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(panel.background=element_rect(fill = "white", colour = "white"))
```

## **INTERPRETACION:**

Del total de encuestadxs (`1210`) mayores de 18 años de todos los departamentos del Perú presentan estas caracteristicas respecto a su edad: 

  
  

## Hacer un data frame para grafico:
```{r}
enfermedades = c("","")
frecuencias = c(1,2)
grafico10  = data.frame(enfermedades, frecuencias)

colnames(grafico10) = c("Reg","Freq") 
```
  --> PARA ESTE CASO SERÍA: --> ggplot(grafico10, aes(x=Reg, y=Freq, color =Reg))


## GRAFICOS BASICOS DE R:
```{r}
grafico1 <- table(data$Type) #creamos un objeto que contenga la tabla de frecuencias de la variable
```
  - PIE
```{r}
pie(grafico1,col=rainbow(7), main = "Índice de democracia", radius = 1, cex = 1.2)
```
  - BARPLOT
```{r}
barplot(grafico1, col = "skyblue",
                  main = "Índice de democracia",
                  xlab = " ",
                  ylab = "Frecuencias",
                  cex.axis = 1,
                  cex.lab = 1,
                  las = 1)
```
  - BOXPLOT
```{r fig.height=6, fig.width=9}
boxplot(data$State_Legit)
boxplot(data$State_Legit ~ data$Type) # ~ --> indica graficar la variable (State_Legit) segun los grupos de (Type)
```
  - HISTOGRAMA
```{r}
hist(data$State_Legit)
```










ESTADISTICA INFERENCIAL
============================================================

1. Intervalos de confianza para una proporción

```{r}
class(data$var)
data$var[data$var == 9] = NA #solo si es necesario
data$var=factor(data$var,
                 levels = levels(data$var),
                 labels = c("Muy en desacuerdo","Algo en desacuerdo","Algo de acuerdo","Muy de acuerdo"),
                 ordered = T) # si es ordenado o no
class(data$Var)
table(data$var)
```

```{r}
sum(is.na(data$var)) # cantidad de NA's
data = data[complete.cases(data$var),] # Elimina NA's
table(data$var)
```

```{r}
demo1 <- prop.test(209, 560) #n es el 560 y x 209
demo1
```

## **INTERPRETACIÓN**: 

  + El porcentaje de personas que están muy de acuerdo con que los extranjeros que viven en el Perú contribuyen a la economía en el Perú es, en términos generales, es medianamente alto (37.32%) En ese sentido, según los valores reportados por el intervalo de confianza, el valor del parámetro (porcentaje de personas que están muy de acuerdo con que los extranjeros que viven en el Perú contribuyen a la economía en el Perú) puede oscilar entre 33.33% y 41.49% a un nivel de confianza del 95%. 


2. Intervalos de confianza para una media
============================================================

```{r}
class()
data$var[data$var == 9] = NA #solo si es necesario
data$Var= as.numeric(data$var) # solo si es numérica
summary(data$var)
```

```{r}
sum(is.na(data$var)) 
data = data[complete.cases(data$var),]
table(data$var)
```

```{r intervalo de confianza}
library(Rmisc)
ci.indicador <- CI(data$var, ci = 0.95) 
ci.indicador 
```

## **INTERPRETACIÓN**: 

  - El promedio de horas de sueño es, en términos generales, bajo. A partir de los datos de la muestra, se ha obtenido que el promedio de horas de sueño de las y los estudiantes es de 4. En ese sentido, según los valores reportados por el intervalo de confianza, el valor del parámetro (promedio de horas de sueño de las y los estudiantes en la población –la que sea-) puede oscilar entre 3.6 y 5.5 a un nivel de confianza del 95%”




3. Construcción de indicadores
============================================================
```{r}
# REVISAR EL CODEBOOK: CONVERTIR 
data$P4_9_1_A_A[data$P4_9_1_A_A == 9]= NA #solo si es necesario
data$P4_9_1_A_A[data$P4_9_1_A_A == 8]= NA

data$P4_9_1_A_B[data$P4_9_1_A_B == 9]= NA
data$P4_9_1_A_B[data$P4_9_1_A_B == 8]= NA
data$P4_9_1_A_C[data$P4_9_1_A_C == 9]= NA
data$P4_9_1_A_C[data$P4_9_1_A_C == 8]= NA
data$P4_9_1_A_D[data$P4_9_1_A_D == 9]= NA
data$P4_9_1_A_D[data$P4_9_1_A_D == 8]= NA
data$P4_9_1_A_E[data$P4_9_1_A_E == 9]= NA
data$P4_9_1_A_E[data$P4_9_1_A_E == 8]= NA
```

```{r}
data$P4_9_1_A_B=car::recode(data$P4_9_1_A_B,"1=4; 2=3; 3=2; 4=1") #recodificas solo si es necesario, mirar el despliegue de tus variables.

data$P4_9_1_A_C=car::recode(data$P4_9_1_A_C,"1=4; 2=3; 3=2; 4=1")
data$P4_9_1_A_E=car::recode(data$P4_9_1_A_E,"1=4; 2=3; 3=2; 4=1")

```

```{r}
data$P4_9_1_A_A = as.numeric(data$P4_9_1_A_A)

data$P4_9_1_A_B = as.numeric(data$P4_9_1_A_B)
data$P4_9_1_A_C = as.numeric(data$P4_9_1_A_C)
data$P4_9_1_A_D = as.numeric(data$P4_9_1_A_D)
data$P4_9_1_A_E = as.numeric(data$P4_9_1_A_E)

data$xeno = data$P4_9_1_A_A+data$P4_9_1_A_B+data$P4_9_1_A_C+data$P4_9_1_A_D+data$P4_9_1_A_E
```

```{r}
sum(is.na(data$xeno))
#summary(data$xeno)

data1 = data[complete.cases(data$xeno),]
summary(data1$xeno)
```


  - Para una mejor comprensión, usaré una escala de 0 - 10 
```{r}
data1$xeno = data1$xeno-elvalormínimo 
data1$xeno = data1$xeno/(max(data1$xeno)-min(data1$xeno))
data1$xeno = data1$xeno*10
```

```{r}
summary(data1$xeno)
```

  - gráficos basicos de R
```{r}
boxplot(data1$xeno)
hist(data1$xeno, main = "indice de xenofobia en el perú ",)
```

## **GRAFICO DE CAJAS (BOXPLOT) E HISTOGRAMA (HIST)**
```{r fig.show='hold', message=FALSE,warning=FALSE,eval=TRUE,fig.width=9.5,fig.height=5.55, fig.align="center", fig.cap="Figure 1: Docentes según sexo (ENDO 2020)."}
library(ggplot2)
library(plotly)
p1 <- ggplot(datalapop, aes(x = "", y = psc8, color ="")) + 
  geom_boxplot() + coord_flip() +
  theme(legend.position = "top", axis.text.y = element_blank(), panel.background=element_rect(fill = "white", colour = "white")) + 
  geom_jitter(shape = 16, position = position_jitter(0.2)) + 
  labs(title = "Edad de los encuestados", x = "", y ="Index", subtitle = "", caption = "ENDO 2020")
ggplotly(p1)

# HIST:
ggplot(datalapop, aes(x = q2, color = "")) + 
  geom_histogram(fill = "white", alpha = 0.5, position = "identity") +  #agregar bins = 30 o breaks=seq()
    labs(title ="Edad de los encuestados", y = "", x="Index", subtitle = "", caption = "ENDO 2020")+
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(panel.background=element_rect(fill = "white", colour = "white"))
```

## **INTERPRETACION**: 

  + En promedio, de un indice de 0 a 10, las personas tienen un indice de xenofobia de 4.52, es decir, es una xenofobia de nivel medio. Asimismo, el 50% tiene, un indice de xenofobia de al menos 4.67. Se observa que la distribución del indice de xenofobia es simétrico, es decir, tiene una acumulación de datos en los valores medios de la distribución del indice. 




COMPARACIÓN DE GRUPOS
============================================================

1. PRUEBA T para MEDIAS (una númerica y una cualitativa)

Primero tengo que tener mi variable númerica, puede ser un indicador, con sus casos completos, con esa nueva data (data1), trabajo la variable cualitativa, tal como está a continuación
```{r}
class(data1$P4_5)

data1$P4_5[data1$P4_5 == 9]= NA #REVISAR EL CODEBOOK
data1$P4_5[data1$P4_5 == 8]= NA #REVISAR EL CODEBOOK

table(data1$P4_5)
data1$P4_5=factor(data1$P4_5,
                   levels = c(1:2),
                   labels = c("Si","No"))
sum(is.na(data1$P4_5))
class(data1$P4_5)
table(data1$P4_5)

```

```{r}
sum(is.na(data1$P4_5))
```

```{r}
data1= data1[complete.cases(data1$P4_5),]
sum(is.na(data1$P4_5))
table(data1$P4_5)
```

Aquí recodífico
```{r}
SI= data1[data1$P4_5=="Si", "xeno"] #variable xeno es el indicador (númerico) y P4_5 es la cualitativa.
NO= data1[data1$P4_5=="No", "xeno"] 
mean(SI, na.rm = TRUE) - mean(NO, na.rm = TRUE) 
```

```{r}
t.test(data1$xeno ~ data1$P4_5)
```

```{r}
library(Rmisc)
ci.indicador1 <- CI(SI, ci=0.95)
ci.indicador0 <- CI(NO, ci=0.95)
ci.indicador1
ci.indicador0
```

## **GRÁFICA**
```{r message=FALSE,warning=FALSE,eval=TRUE,fig.show='hold',fig.width=9.5,fig.height=5.5, fig.align="center", fig.cap="Figure 1: Índice de promedio de confianza entre grupos (LAPOP 2019)."}
library(gplots)
plotmeans(data1$xeno ~ data1$P4_5, 
          connect = F, mean.labels=T, digits=2, col="black", barwidth = 2, 
          xlab="Grupos", 
          ylab="",
          main = "relación entre el índice de xenofobia y si las personas tienen una relación con extranjeros que viven en el Perú ")
```

## **INTERPRETACION**:  

  + Para explorar la relación entre el índice de xenofobia y si las personas tienen una relación con extranjeros que viven en el Perú corresponde utilizar la prueba t para muestras independientes. Frente a un p-value menor a 0.05, en este caso 0.00022, concluimos que se puede rechazar la hipótesis nula que plantea que no existe diferencia de promedios entre los grupos. En consecuencia, y a partir de lo reportado en los gráficos de barras de error, se tiene evidencia estadística para afirmar que existe diferencias entre la relación entre el índice de xenofobia y  las personas tienen una relación con extranjeros que viven en el Perú, siendo los que no mantienen una relación con extranjeros, los que tienen un mayor indice de xenofobia. 



1.1. Prueba T para proporciones
============================================================

Primero, hay una proporción anterior que ya ha sido medida y tiene sus datos completos, en base, a esa nueva data (datos) podemos ejercer la comparación. 
```{r}
class(datos$SEXO)
str(datos$SEXO)
datos$SEXO=factor(datos$SEXO,
                   levels = c(1:2),
                   labels = c("Hombre","Mujer"))
sum(is.na(datos$SEXO))
class(datos$SEXO)
table(datos$SEXO)
```

Esta es mi tabla de contingencia para ambas variables, la segunda es, por lo general, las columnas, esto es, lo que se quiere comparar. 

```{r}
table(datos$VS5F,datos$SEXO)
```


```{r intervalo 1}
#Hombres que sufrieron acoso
demo3<-prop.test(88, 917) 
demo3
```

```{r intervalo 2}
#Mujeres que sufrieron acoso

demo4<-prop.test(230, 1002) 
demo4
```

```{r}
prop.test(x=c(88,230), n=c(917,1002), conf.level = 0.95)
```



## **GRAFICA**
```{r message=FALSE,warning=FALSE,eval=TRUE,fig.show='hold',fig.width=9.5,fig.height=5.55, fig.align="center", fig.cap="Figure 3: Diferencia de proporciones entre escuelas públicas urbanas y rurales (ENDO 2018)."}
library(ggplot2)
ggplot(datos = datos, 
            mapping = aes(x = factor(datos$SEXO), 
              fill = factor(datos$VS5F))) + 
  geom_bar(position = 'fill', stat = 'count') + 
  scale_fill_brewer(palette = "Set1") + 
  labs(y = "Cantidad de escuelas", 
       x = "Escuelas", 
       fill = "Tipo de escuelas", 
       title = "Diferencia de proporciones entre escuelas públicas urbanas y rurales (ENDO 2018).", 
       subtitle = "", 
       caption = "Elaboración propia con base en datos recogidos por la ENDO 2018. @luccemhu")
```

## **INTERPRETACION**: 

  + El valor de P, es menor a 0.05. Es decir, a un nivel de confianza del 95%, podemos rechazar la hipótesis nula y podemos afirmar que la proporción de mujeres acosadas es significativamente diferente que la proporción de hombre acosados. La proporción de mujeres acosadas (0.23) es mayor que la proporción de hombres acosados (0.096).

  + En particular, a un nivel de confianza del 95%, el porcentaje de mujeres acosadas varia entre un 20% y un 26%, mientras que el porcentaje de varones acosados varia entre un 7.8%  y 11.7%. Es decir, la proporción o el porcentaje de mujeres acosadas es cuantitativamente mayor en comparación a los hombres. Por tanto, las políticas contra el acoso deberían estar dirigidas o priorizadas en este sector.






2. Prueba Anova 
============================================================
Requisitos: 
```{r}
library(nortest)
by(lapop$var1,lapop$var2,lillie.test) #Mayor a 50 casos, es decir, normalidad
```

```{r}
library(car)
leveneTest(lapop$var1, lapop$var2) #Prueba de Levene, Pr es igual al Pvalue, es decir, varianzas iguales. 

```

Ahora, tenemos una variable númerica, cuya cantidad de datos completos, define la comparativa. 

```{r}
class(data1$P4_6)
data1$P4_6[data1$P4_6 == 9]= NA
data1$P4_6[data1$P4_6 == 8]= NA
table(data1$P4_6)
data1$P4_6=car::recode(data1$P4_6,"1=3; 2=2; 3=1")
data1$P4_6=factor(data1$P4_6,
                   levels = c(1:3),
                   labels = c("Son pocos","Es el número adecuado", "Son demasiados"))
sum(is.na(data1$P4_6))
class(data1$P4_6)
table(data1$P4_6)

```

```{r}
data1 = data1[complete.cases(data1$P4_6),]
sum(is.na(data1$P4_6))
table(data1$P4_6)
```

```{r}
library(psych)
describeBy(data1$xeno,data1$P4_6)
```

```{r}
anova <- aov(data1$xeno~data1$P4_6)
summary(anova)
```

```{r}
TukeyHSD(anova)
```


```{r fig.height=6, fig.width=9}
library(gplots)
plotmeans(data1$xeno ~ data1$P4_6, connect=F, barwidth=3, p = 0.95, 
          xlab="Percepción sobre el número de extranjeros en el Perú",
          ylab="Indice de xenofobía",
          main="Indice de xenofobia según percepción sobre el número de extranjeros en el Perú \n Mean Plot with 95% CI")
```


```{r}
plot(TukeyHSD(anova))
```

## **INTERPRETACION**: 

  + Para explorar la asociación relación entre el índice de xenofobia y la percepción sobre el número de extranjeros en el Perú ("Son demasiados","Es el número adecuado", "Son pocos") de más de dos categorías corresponde utilizar la prueba ANOVA. Así,tenemos evidencia estadística para afirmar que el promedio de xenofobia de las personas  es mayor en el grupo que percibe que el número de extranjeros en el Perú son muchos, en comparación a los que consideran que "son pocos"
o "es el número adecuado". Al obtener un valor menor a 0.05, rechazamos la hipótesis nula y afirmamos que existen diferencias entre, al menos, dos grupos.En particular, con la prueba Tukey, observamos que existen diferencias de xenofobia entre los grupos de las personas que perciben que el número de extranjeros son demasiados y los que perciben que el número de extranjeros es son pocos, asimismo,entre los grupos de las personas que perciben que el número de extranjeros son demasiados y los que perciben que el número de extranjeros son el adecuado.Empero, no tenemos evidencia estadística para afirmar que existan diferencias en el promedio de xenofobia entre los grupos que que perciben que los extranjeros que viven en el Perú "son muy pocos" y los que perciben que son "el número adecuado". 



# PRUEBA CHI CUADRADO
============================================================
Dos variables ordinales: 
En este caso, ya había trabajado con la variables A, pero con la D, todavía. 

```{r}
class(data$P4_9_1_A_D)
data$P4_9_1_A_D[data$P4_9_1_A_D == 9]= NA
data$P4_9_1_A_D[data$P4_9_1_A_D == 8]= NA
table(data$P4_9_1_A_D)
data$P4_9_1_A_D=car::recode(data$P4_9_1_A_D,"1=4; 2=3; 3=2; 4=1")
data$P4_9_1_A_D= as.factor(data$P4_9_1_A_D)
data$P4_9_1_A_D=factor(data$P4_9_1_A_D,
                 levels = levels(data$P4_9_1_A_D),
                 labels = c("Muy en desacuerdo","Algo en desacuerdo","Algo de acuerdo","Muy de acuerdo"),
                 ordered = T)
class(data$P4_9_1_A_D)
table(data$P4_9_1_A_D)
```

```{r}
sum(is.na(data$P4_9_1_A_D)) 
data0 = data[complete.cases(data$P4_9_1_A_D),]
table(data$P4_9_1_A_D)
```

```{r}
data = data[complete.cases(data$P4_9_1_A_A),]
data = data[complete.cases(data0$P4_9_1_A_D),]
```


```{r}
table1 = table(data$P4_9_1_A_A, data$P4_9_1_A_D)
table1
```

```{r}
prop.table(table1, 2)
```

```{r}
table1.1 = prop.table(table1, 2) * 100
table1.1
```

```{r}
barplot(table1.1, 
        main = "Percepción respecto a que los extranjeros en el Perú contribuyen a la economía peruana según traen ideas innovadoras ", legend.text = T)
```

```{r}
chisq.test(table1)
```

## **INTERPRETACION**: 

  + La prueba nos da como resultado un p-value de 2.2e-16, es decir, menor a < 0.05, por lo que se puede rechazar la hipótesis nula y afirmar la h1: Las variables son estadísticamente dependientes. Entonces, se concluye que sí existe relación entre el grado de acuerdo respecto a que los extranjeros en el Perú contribuyen a la economía peruana y traen ideas innovadoras. 

Como eran dos ordinales, hago la prueba 
simentricas

```{r}
library(DescTools)
GoodmanKruskalGamma(table1)
```
Interpretación: Existe una asociación directa media entre el grado de acuerdo respecto a que los extranjeros en el Perú contribuyen a la economía peruana y traen ideas innovadoras

Asimetricas
```{r}
SomersDelta(table1, direction = "row")
```

Interpretación: En este caso, el D de somers en 0.368 por lo que los pares son concordantes (por el signo), y la predicción es baja (por el valor 0.368). Entonces, existe una dependencia directa baja entre entre el grado de acuerdo respecto a que los extranjeros en el Perú contribuyen a la economía peruana y traen ideas. 

##Si era, una nominal. 

simétrica 

```{r}
chisq.test(tabla1)


library(vcd)
assocstats(tabla1)
```

V de Cramer tiene un coeficiente de 0.097. Este resultado está mucho más cerca de 0 que de 1, por lo que
podemos concluir que estamos ante una relación débil. Para poner unos límites arbitrarios, podemos indicar
que:

Asimétrica: 

```{r}
Lambda(tabla1.1, direction = "row") 
```

Lambda tiene un coeficiente de 0. Este resultado está mucho más cerca de 0 que de 1, por lo que podemos
concluir que la variable NO ayuda a predecir la variable dependiente. Para poner unos límites arbitrarios,
podemos indicar que





Skills: 
```{r creación de grupos}
data$GR = factor(ifelse(data$MT3 == 3, 1, # el tres se convierte en el grupo como 1
                        ifelse(data$MT3 == 5, 2,
                               ifelse(data$MT3 == 6, 3, 0))))
table(data$GR)
```






PARA EL FINAL
============================================================


CORRELACION: SOLO con/son NUMERICOS
============================================================
Para variable 1 

```{r}
data$var = as.numeric(data$var)
class(data$var)
data$var[data$var == 888888]= NA #Si es que los hay 
summary(data$var)
```

```{r}
sum(is.na(data$var)) 
data1 = data[complete.cases(data$var),]
summary(data$var)
```

Para variable 2 

```{r}
data1$var = as.numeric(data$var)
class(data1$var)
data1$var[data1$var == 888888]= NA #Si es que los hay 
summary(data1$var)
```

```{r}
sum(is.na(data1$var)) 
data2 = data1[complete.cases(data1$var),]
summary(data1$var)
```


Primero veremos el Supuesto de NORMALIDAD
   
   1. Hipótesis de la Normalidad: 
   
   H0 = hay normalidad
   H1 = no hay normalidad
   
   2. Prueba/ Kolmogorov Smirnov: prueba para n > a 50 (data grande)

```{r}
library(nortest)
lillie.test(data2$var1) #Kolmogorov smirnov
lillie.test(data2$var2)
```

 Explicación: La prueba nos da como resultado un p-value de 2.212e-09, es decir, menor a < 0.05, por lo que se puede rechazar la hipótesis nula y afirmar la h1: Los datos no proceden de una distribución normal. Por tanto, que debiéramos usar Coef de Spearman.

1.1 Tienen distribución conocida (PEARSON) Este no es el caso

```{r}
cor.test(data2$var1,data2$var2, method = c("pearson"))

```


1.2 Searman

```{r}
cor.test(data2$var1,data2$var2, method = c("spearman"))
```

Hago gráfico de correlación si lo necesito: 

```{r}
plot(DATA1$pib,DATA1$gini) 
plot(DATA1$pib,DATA1$gini, xlab="PIB del pais", ylab="Indice de gini")
```


Interpretación: Debido a que el p-value es menor a < 0.05, rechazamos la H0 y, por tanto, aceptamosla H1 de que "sí existe correlación entre las variables pib de hogar y indice de desigualdad salarial". Asimismo, como el coeficiente es de -0.36, se deduce de que, segun el criterio de Cohen, se trata de una correlación mediana. Asimismo, debido al signo, se concluye que es  negativa (a medida de que el pib aumenta, el indice de desigualdad salarial disminuye). 


1.3 [Intensidad=rho]

Desde 0.0 a 0.1 -> No es relevante 
Desde 0.1 a 0.3 -> Pequeña
Desde 0.3 a 0.5 -> Mediana 
Desde 0.5 a 1   -> Grande 


Si hay correlación, hago modelo


##Regresión lineal simple (NUMÉRICA + NUMÉRICA)

Primero trabajo las variables que quiero correlacionar, las limpios, si es hacer indice, los hago, o las convierto en dummys. Una vez hecho eso, recién aplico el modelo. Pongo 

##Modelo 1
```{r}
sum(is.na(lapop1$apoyo))
lapop1.1 = lapop1[complete.cases(lapop1$ing4), ] #Aquí hago que ambas variables tengan la misma cantidad de datos
```

```{r}
modelo1.1 <- lm(apoyo ~ ing4, data = lapop1.1) #variable dependiente, luego independiente, trabajo con data creada
summary(modelo1.1)
```

Interpretación: Debido a que el p-value del modelo es significativo (8.906e-13), se concluye que se trata de un modelo valido. Asimismo, debido al valor 8.91e-13 de la variable dummy_de acuerdo, se concluye que la variable ayuda a explicar la variable dependiente. En específico, se trata de una relación directa en el que estar de acuerdo con que "la democracia tiene problemas, pero es mejor que cualquier otra forma de gobierno" aumenta el nivel de apoyo al sistema político en 7.6 puntos. Cabe precisar que, la variable predictora introducida al modelo, tiene un R2- ajustado bajo (0.034), por lo que es capaz de explicar solo el 3,4% de la variabilidad observada en el índice de apoyo al sistema político. 


##Modelo 2: A los que gobiernan el país les interesa lo que piensa la gente como usted.

```{r}
sum(is.na(lapop1$eff1))
lapop1.2 = lapop1[complete.cases(lapop1$eff1), ]
```

```{r}
modelo1.2 <- lm(apoyo ~ eff1, data = lapop1.2)
summary(modelo1.2)
```

#Modelo 3: Para variable numérica + numérica

```{r}
modelo1.3 <- lm(apoyo ~ confianza, data = lapop1.3)
summary(modelo1.3)
```

Interpretación: Debido a que el p-value del modelo es significativo (2.2e-16), se concluye que se trata de un modelo valido. Asimismo, debido al valor 2e-16 de la variable indice de confianza, se concluye que la variable ayuda a explicar la variable dependiente. En específico, se trata de una relación directa en el que el aumento en un punto en el indice de confianza, aumenta el indice de apoyo al sistema político en 0.68 puntos. Cabe precisar que, la variable predictora introducida al modelo, tiene un R2- ajustado medio (0.43), por lo que es capaz de explicar el 43% de la variabilidad observada en el índice de apoyo al sistema político.

###Sobre aplicar la ecuación de la recta

```{r}
abline(modelo1.3)  #PROBAR
```

###Tabla N°1 - Una sola tabla 

```{r}
library(huxtable)
library(officer)
library(jtools)
library(ggstance)
library(broom.mixed)
```

```{r}
export_summs(modelo1.1,
             modelo1.2,
             modelo1.3,
             scale = FALSE,
             error_format = "[{conf.low}, {conf.high}]")
```

Se concluye que,el modelo 3 que toma en cuenta la variable "Indice de confianza" es el modelo que mejor ayuda a explicar el apoyo al sistema político puesto que, tomando en cuenta el R2 ajustado, el nivel de explicación del modelo es del 42%. Es decir, es comparativamente superior al modelo 1 y 2, quienes tienen un nivel de explicación de solo el 3% y 7%, respectivamente.


###Lineal con factor 

Convierto mi dependiente 

```{r}
data$demo_dummy <- as.factor(data$demo_dummy)
levels(data$demo_dummy) <- c("no demo", "demo")
table(data$demo_dummy)
```

Hago mi t-test

```{r}
t.test(data$gini ~ data$demo_dummy)

```
Interpretación: Hay diferencia significativa entre las variables, en donde se identifica que hay más países sin
democracia plena que países en democracia plena.
```{r}
plotmeans(data$gini ~ data$demo_dummy, data)
```

Ahora, hago modelo Uwu



##LINEAL MULTIPLE

###Modelo base Trabajo todas las variables, apoyo, 

Limpio base y hago una sola, con todas las variable 
```{r}
sum(is.na(lapop2$apoyo))
sum(is.na(lapop2$ing4))
sum(is.na(lapop2$soct2))
lapop2.1 = lapop2[complete.cases(lapop2$ing4), ]
lapop2.1 = lapop2.1[complete.cases(lapop2.1$soct2), ]
lapop2.1 = lapop2.1[complete.cases(lapop2.1$apoyo), ]
```

```{r}
sum(is.na(lapop2.1$apoyo))
sum(is.na(lapop2.1$ing4))
sum(is.na(lapop2.1$soct2))
```

Hago modelo
```{r}
modelo2.1 <- lm(tolerancia ~ apoyo + ing4 + soct2_Mejor, data = lapop2.1)
summary(modelo2.1)
```

##Ahora introducimos al modelo las variables de control Genero y EDAD

limpio base, la hago uniforme 
```{r}
sum(is.na(lapop2.1$apoyo))
sum(is.na(lapop2.1$ing4))
sum(is.na(lapop2.1$soct2))
sum(is.na(lapop2.1$q1))
sum(is.na(lapop2.1$q2))
```

```{r}
lapop2.2= lapop2.1[complete.cases(lapop2.1$q1),]
```

```{r}
sum(is.na(lapop2.2$apoyo))
sum(is.na(lapop2.2$ing4))
sum(is.na(lapop2.2$soct2))
sum(is.na(lapop2.2$q1))
sum(is.na(lapop2.2$q2))
```
###Variable género + Sexo

```{r}
lapop2.2$q1= factor(lapop2.2$q1)
levels(lapop2.2$q1)=c("Hombre","Mujer")
class(lapop2.2$q1)
table(lapop2.2$q1)
```

```{r}
library(fastDummies)
lapop2.2=dummy_cols(lapop2.2, select_columns = c("q1"))
```


En base al modelo, hago mi chiste
```{r}
modelo2.2 <- lm(tolerancia ~ apoyo + ing4 + soct2_Mejor + q1_Mujer + q2, 
                data = lapop2.2)
summary(modelo2.2)
```

```{r}
export_summs(modelo2.1,modelo2.2, scale = FALSE,
             error_format = "[{conf.low}, {conf.high}]")
```

Construido el modelo, interprete sus resultados y responda lo siguiente:

- ¿Qué variables resultaron ser estadísticamente significaticas? Comente el sentido de la relación de cada variable
- ¿Cuál es el porcentaje de varianza explicado por el modelo?
- ¿Qué variable presenta el mayor impacto sobre la variable dependiente? 

En primer lugar, las variables indice de apoyo al sistema político y el acuerdo de las personas respecto a que "la democracia tiene problemas, pero es mejor que cualquier otra forma de gobierno" tienen un p value de 3.49e-06 y 2.99e-07. Por tanto, son significativas. No osbtante, la percepción de las personas respecto a la economía tiene un p-value de 0.81, por tanto, la variable no es significativa en el modelo. 

Respecto a la relación de las variables. En primer lugar, el indice de apoyo al sistema político tiene tiene una relación directa con el nivel de tolerancia. En particular, por cada aumento de un punto en el apoyo al sistema político, el indice de tolerancia aumenta en 0.39 puntos. En segundo lugar, el estar de acuerdo con que "la democracia tiene problemas, pero es mejor que cualquier otra forma de gobierno", aumenta el indice de tolerancia en 5.3 puntos. En tercer lugar, aunque tal como se ha demostrado, la variable no es significativa; el hecho de que se considere que la economía esta mejor que hace doce meses, disminuye la tolerancia en 0.37 puntos. 

Por un lado, el porcentaje de varianza del modelo base, es decir, sin las variables de control Edad y Género, es de 0.04. Es decir, es bajo puesto que solo es capaz de explicar solo el 4% de la variabilidad observada en el índice de tolerancia. Por otro lado, el porcentaje de varianza del modelo con la inclusión de la variable género y edad, es de 0.08.Es decir, es bajo puesto que solo es capaz de explicar solo el 8% de la variabilidad observada en el índice de tolerancia. 

A pesar de no ser signifcativa, la variable x de alguna manera influye en la variable dependiente. tambien hago análisis. 

Gráfico 
```{r}
library(jtools)  # Hago mi modelo, mi rey
plot_summs(modelo1, modelo2, model.names = c("Sin controles","Con controles"))
```

###Nivel de explicación de las variables 

```{r}
library(lm.beta)
lm.beta(modelo2.2) #Variables controladas. 
```

Por último, entre las variables independendientes que son significativas en el modelo [indice de apoyo al sistema,"la democracia tiene problemas, pero es mejor que cualquier otra forma de gobierno"], el estar de acuerdo con que "la democracia tiene problemas, pero es mejor que cualquier otra forma de gobierno" (0.13) tiene el mayor impacto sobre el nivel de tolerancia porque su coeficiente es mayor en comparación al de el indice de apoyo (0.12). Cabe precisar que, si tomo en cuenta a las variables controladas Edad y Sexo, entonces, la variable que tiene un mayor impacto en el nivel de tolerancia es la edad (-0.20)

Gráfico: 

```{r}
library(leaps)
models <- regsubsets(tolerancia~apoyo+ing4+soct2_Mejor+q1_Mujer+q2, data=lapop2.2, method=("forward"))#method=c(«exhaustive», «backward», «forward», «seqrep»)
summary(models)
```

Analizar vertical, numero de estrellitas. 
Las variables que son mejor para explicar la variable dependiente. 


```{r}
plot(models)
```
Ahora, veo los colores de manera vertical.






##Trabajo de variables ordinales a dicotómicas a dummy 

###ING4

```{r}
class(lapop1$ing4)

lapop1$ing4[lapop1$ing4 == 888888]= NA 
lapop1$ing4[lapop1$ing4 == 988888]= NA

lapop1$ing4=car::recode(lapop1$ing4,"1=0; 2=0; 3=0; 4=0; 5=1; 6=1; 7=1") #"1:4=0
```

```{r}
lapop1$ing4 = factor(lapop1$ing4)
levels(lapop1$ing4) = c("En desacuerdo","De acuerdo")
class(lapop1$ing4)
table(lapop1$ing4)
```

###EFF1

```{r}
class(lapop1$eff1)
lapop1$eff1[lapop1$eff1 == 888888]= NA 
lapop1$eff1[lapop1$eff1 == 988888]= NA 
lapop1$eff1=car::recode(lapop1$eff1,"1=0; 2=0; 3=0; 4=0; 5=1; 6=1; 7=1")
```

```{r}
lapop1$eff1= factor(lapop1$eff1)
levels(lapop1$eff1)=c("En desacuerdo","De acuerdo")
class(lapop1$eff1)
table(lapop1$eff1)
```

```{r}
library(fastDummies)
lapop1=dummy_cols(lapop1, select_columns = c("ing4","eff1")) #Mis variables tienen que no tener 88 y 99 y tienen que estar en factor y codificadas con su nombre. 
```
