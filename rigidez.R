library(readr)
setwd("/cloud/project/Rigidez")
rigidez <- read_delim("mola.csv", delim = ",")

head(rigidez)

# Gráfico linear
scatter.smooth (rigidez$x, rigidez$forca)
plot(rigidez$x, rigidez$forca)

ggplot(rigidez, aes(x, forca))+ geom_point()
+ geom_smooth(method = "lm", se = TRUE)

#Coeficientes angular e linear
coef(lm(x~ forca, data = rigidez))

#Gráfico Boxplot
boxplotgraf<-ggplot(rigidez, aes(cargas, K)) 
boxplotgraf+ geom_boxplot(fill = "red", colour = "#5464FF")

boxplotgraf<-ggplot(rigidez, aes( x,forca)) 
boxplotgraf+geom_boxplot(aes(colour = cargas))

# Outra forma de BoxPlot
par( mar=c(5,4,4,2) )
boxplot(rigidez$K)

# Histograma
grafico<-ggplot(rigidez, aes(K)) 
grafico+ geom_histogram(binwidth =0.7, fill = "blue")

#Histograma sem ser pelo GGPLOT2 e com curva de densidade
hist(rigidez$K, probability=T, col="blue")
lines(density(rigidez$K) , col="red")

#Gráfico Pirulito
ggplot(rigidez, aes(x=forca, y=K)) +
  geom_segment( aes(x=forca, xend=forca, y=0, yend=K), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Força") +
  ylab("Rigidez") +
  ggtitle("Análise da rigidez de uma mola") +
  theme(plot.title = element_text(hjust = 0.5))

#Não lembro esse comando
stem(rigidez$K)

#Gráfico pirulito interativo. DEU ERRO!
q <- ggplot(rigidez, aes(x=forca, y=K)) +
  geom_segment( aes(x=forca, xend=forca, y=0, yend=K), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Força") +
  ylab("Rigidez") +
  ggtitle("Análise da rigidez de uma mola") +
  theme(plot.title = element_text(hjust = 0.5))
# Lollipop Chart (gráfico interativo)
library(ggplot2)
ggplotly(q)

