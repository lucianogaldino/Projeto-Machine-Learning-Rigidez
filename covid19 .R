library(readr)
library(tidyverse)
setwd("/cloud/project/covid")
covidmundial <- read_delim("COVID19.csv", delim = ",")

#Importando dados somente do Brasil e da América do Sul 
covidbrasil=covidmundial[which(covidmundial$country=="Brazil"),]
covidamericasul = covidmundial %>% filter(country %in% c ("Brazil","Argentina", "Chile", "Uruguay", "Venezuela (Bolivarian Republic of)", "Peru","Ecuador","Colombia", "Paraguay","French Guiana"))

#Histograma Brasil
hist(covidbrasil$new_deaths, probability=T, col="blue")
lines(density(covidbrasil$new_deaths) , col="red")

#Boxplot novas mortes
par( mar=c(5,4,4,2) )
boxplot(covidbrasil$new_deaths)

#Boxplot novos casos 
par( mar=c(5,4,4,2) )
boxplot(covidbrasil$new_cases)

#gráfico da taxa de crescimento de mortes
scatter.smooth(covidbrasil$date, covidbrasil$cumulative_deaths)
plot(covidbrasil$date, covidbrasil$cumulative_deaths)

plot(covidamericasul$date, covidamericasul$cumulative_deaths)

ggplot(covidbrasil, aes(date, cumulative_deaths))+ geom_point()
 + geom_smooth(method = "lm", se = TRUE) 

ggplot(covidamericasul, aes(date, cumulative_deaths))+ geom_point()
+ geom_smooth(method = "lm", se = TRUE)


#Coeficientes Linear e angular
coef(lm(date~ cumulative_deaths, data = covidbrasil))

#Boxplot novas mortes da america do sul
boxplotgraf<-ggplot(covidbrasil, aes(date, new_deaths)) 
boxplotgraf+ geom_boxplot()


# Gráfico Pirulito Brasil
ggplot(covidbrasil, aes(x=date, y=new_deaths)) +
  geom_segment( aes(x=date, xend=date, y=0, yend=new_deaths), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  axis.ticks.x = element_blank()
  )
  xlab("DATA")
  ylab("MORTES")
  ggtitle("NÚMERO DE MORTES POR DIA")
  theme(plot.title = element_text(hjust = 0.5))

  
library(treemap)
# Dados
paises <- covidmundial$country
mortes <- covidmundial$cumulative_deaths
data <- data.frame(paises,mortes)
# treemap
  treemap(data,
          index="paises",
          vSize="mortes",
          type="index"
          )
  
# Treemap
  # Dados
paises <- covidamericasul$country
mortes <- covidamericasul$cumulative_deaths
data <- data.frame(paises,mortes)

# treemap
  treemap(data,
          index="paises",
          vSize="mortes",
          type="index"
          )

# Dados
paises <- covidamericasul$country
mortes <- covidamericasul$cumulative_deaths
data <- data.frame(paises,mortes)

# treemap
  treemap(data,
  index="paises",
  vSize="mortes",
  type="index"
  )
  
View(boxplotgraf)
View(data)




