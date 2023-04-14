#Funciones 
set.seed(10)
df <- tibble::tibble(
  a = rnorm(10)*5,
  b = rnorm(10),
  c = rnorm(10)*2,
  d = rnorm(10)*100)

library (GGally)
ggpairs(df, lower = list(continuous = "smooth")) #Análisis exploratorio para ver valores

#Estrategia de normalización, escalar
#Xn=(Xi-Xmin)/(Xmax-Xmin)
df$a
min(df$a)
max(df$a)
(df$a[1]-min(df$a))/(max(df$a)-min(df$a))
(df$a[6]-min(df$a))/(max(df$a)-min(df$a)) #1 porque max
(df$a[9]-min(df$a))/(max(df$a)-min(df$a)) #0 porque min

#Hecho función: 
df2 = df
df2$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

df2$b <- (df$b - min(df$b, na.rm = TRUE)) /
  (max(df$b, na.rm = TRUE) - min(df$b, na.rm = TRUE))

df2$c <- (df$c - min(df$c, na.rm = TRUE)) /
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))


ggpairs(df2, lower = list(continuous = "smooth"))


#Hacerlo función
#Importante ver los candidatos a imputs (valores variables)
(df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

x <- df$a
(x-min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE)) - min(x, na.rm = TRUE)

#Usar instrucciones más reducibles
range(df$a) #máximo y mínimo
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

#Estructura de función
fescalar <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
} #Crea función

fescalar(c(0, 15, 3, 10, 12)) #Ejecturar función

#Usar función para lo anterior
df2$a <- fescalar(df$a)
df2$b <- fescalar(df$b)
df2$c <- fescalar(df$c)

#Problemas cuando función no está optimizada
set.seed(10)
x <- c(1:10, Inf)
fescalar(x) #Infinito no le cabe

fescalar <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE) #sI APLICA A inf
  (x - rng[1]) / (rng[2] - rng[1])
} 


#Recoger funciones para usar en R
source("u6_funciones/xnorm.R")

valx = fescalar(c(1:20, 81:-32))
valx

#Clase 2 - 16 feb

 #Función para dibujar
library(ggplot2)
library(ggpubr)
library(dplyr)

data(diamonds)
p1 = diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = depth, y = price, color = color)) + 
  geom_point(size = 0.6) + 
  facet_wrap(color ~., ncol = 7)
p1

p2 = diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = depth, y = price, color = color)) + 
  geom_point(size = 0.6) + 
  facet_wrap(color ~., ncol = 7)
p2

  #Hacerlo función reduce el riesgo de esquivocarse copiando y pegando
plotcut = function(corte){
  p1 = diamonds %>% filter(cut == corte) %>% 
    ggplot(aes(x = depth, y = price, color = color)) + 
    geom_point(size = 0.6) + 
    facet_wrap(color ~., ncol = 7)
  return(p1)
}

  #Evaluar la función
plotcut("Good") #Se cambia por cualquier cut

cutplots = ggarrange(plotcut("Ideal"), plotcut("Premium"), 
          plotcut("Good"), plotcut("Very Good"),
          plotcut("Fair"), labels = unique(diamonds$cut), 
          nrow = 5)
ggsave(cutplots, file = "u6_funciones/img/cutplots.jpg", units = "in", 
       width = 8, height = 12, dpi = 350)

#Automatizar más

cuts = unique(diamonds$cut) #Vector de elementos
cutplots = ggarrange(plotcut(cuts[1]), plotcut(cuts[2]),
                     plotcut(cuts[3]), plotcut(cuts[4]),
                     plotcut(cuts[5]),
                     labels = unique(diamonds$cut), 
                     nrow = 5)
ggsave(cutplots, file = "u6_funciones/img/cutplots2.pdf", units = "in", 
       width = 8, height = 12, dpi = 350)


#Automatizar aún más
figuras = list()
for (i in 1:length(cuts)){
  figura = plotcut(cuts[i])
  figuras[i] = figura} #Bucles for tiene errores
figura 

#fUNCIONES APPLY para mejorar automatización
lapply(diamonds, cuts, plotcut) #Ver como mejorar

## Separar (split) el dataset diamonds por el tipo de corte (cut)
cut_lst <- split(diamonds, f = diamonds$cut) # divide en dataframe por categorías de "cut"
names(cut_lst)

# lapply() sobre la lista de dataframes cut_lst
diamonds_lapply <- lapply(seq(cut_lst), function(i) {
  ## seleccionar solo los diamantes de color = D
  dat <- cut_lst[[i]]
  dat_d <- dat %>% filter(color == "E")
  
  ## calcular el modelo linear
  lm1 <- lm(price ~ carat, data = dat_d)
  
  ## crear gráfico de dispersión
  cut_plot <- ggplot(aes(x = carat, y = price), data = dat_d) +
    geom_point(colour = "grey20", size = 0.1) +
    stat_smooth(method = "lm", se = TRUE, 
                fill = "violet", colour = "red", size =0.5) +
    geom_text(data = NULL,
              x = min(dat_d$carat, na.rm = TRUE) + 0.2,
              y = max(dat_d$price, na.rm = TRUE),
              label = unique(dat_d$cut)) + 
    theme_minimal()
  
  ## retornar la data, el modelo lineal y la gráfica
  return(list(data = dat_d,
              modelo_lineal = lm1,
              grafica = cut_plot))
})

#Mejorar anterior
#lapply(diamonds, cuts, plotcut)

figuras_cut = lapply(cuts, FUN = plotcut) #solo define cuts porque dentro de la función ya se específica que se trabaja con diamonds

figuras_cut[[1]]

#Otro ejemplo de aplicación lapply
cuts = unique(diamonds$cut)
lm_cut = function(corte){ #Recibe argumento de corte
  tabla = diamonds %>% filter(cut == corte) #Argumento crea una tabla filtrada por el corte 
  lm1 = lm(price ~ x, data = tabla) #La tabla sirve para hacer regresión lineal 
  plotlm = ggplot(data = tabla, aes(x = price, y = x)) +
                    geom_point() + 
                    geom_smooth(method = "lm") #Gráfica de la regresión
  result = list(tabla, lm1, plotlm)
  return(result)
}

resultados =  lapply(cuts, lm_cut)
resultados[[5]]


#Clase 3 - 14 mar 20233

if(!require(devtools)) install.packages("devtools")
devtools::install_github("gualapuromoises/residualR")

  #Paquetes requeridos
library("usethis")
library("testthat")
library("roxygen2")

library(residualR)
potencia(2,3)

figscatter(iris, "Petal.Length", "Petal-Width")