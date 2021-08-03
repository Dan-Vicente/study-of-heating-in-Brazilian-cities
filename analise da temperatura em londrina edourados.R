# Analisando e comparando o aumento de temperatura ao longo do tempo entre Londrina e 

# importanto arquivo de temperatura


# Dataset:
# Berkeley Earth
# http://berkeleyearth.org/data


# Instalando e Carregando Pacotes
install.packages("readr")
install.packages("data.table")
library(readr)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(data.table)
#library(dtplyr)


# importanto os dados


system.time(df <- fread("TemperaturasGlobais.csv"))


# Criando tabela brasileira com nomenclatura

cidadesBrasil <- subset(df, Country == 'Brazil')
cidadesBrasil <- na.omit(cidadesBrasil)
head(cidadesBrasil)
nrow(df)
nrow(cidadesBrasil)
dim(cidadesBrasil)

# Organizando o código, modificando datas.

cidadesBrasil$dt <- as.POSIXct(cidadesBrasil$dt,format='%Y-%m-%d')
cidadesBrasil$Month <- month(cidadesBrasil$dt)
cidadesBrasil$Year <- year(cidadesBrasil$dt)


# importando as cidades a serem analisadas
# Londrina
Lon <- subset(cidadesBrasil, City == 'Londrina')
Lon <- subset(Lon, Year %in% c(1796,1846,1896,1946,1996,2012))

# Dourados
Dou <-subset(cidadesBrasil, City == 'Dourados')
Dou <- subset(Dou, Year %in% c(1796,1846,1896,1946,1996,2012,2019))

# Construindo os Plots

p_Lon <- ggplot(Lon, aes(x = (Month), y = AverageTemperature, color = as.factor(Year))) +
  geom_smooth(se = FALSE,fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab("Mais")+
  ylab("Temperatura") +
  scale_color_discrete("") +
  ggtitle("Temperatura MÃ©dia ao longo dos anos em Londrina") +
  theme(plot.title = element_text(size = 18))

# Buscando por Dourados

p_Dou <- ggplot(Dou, aes(x = (Month), y = AverageTemperature, color = as.factor(Year))) +
  geom_smooth(se = FALSE,fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab("Mais")+
  ylab("Temperatura") +
  scale_color_discrete("") +
  ggtitle("Temperatura MÃ©dia ao longo dos anos em Dourados")
  theme(plot.title = element_text(size = 18))



# Exportando em gráfico para possibilitar analise
p_Lon
p_Dou



# Média de Temperatura ao longo do tempo
ML = mean(Lon$AverageTemperature)


MD= mean(Dou$AverageTemperature)


# Diferença da média entre as duas cidades
ML-MD

# Criando varias demonstrações gráficas da variação de temperatura entre as cidades.
# Buscando a forma mais didatica 


# Dois plots juntos via plot

par(mar=c(4,3,3,3), col.axis="black")

plot(Lon$AverageTemperature, type="s", col="red", bty="n", xlab=" Tempo ", ylab="")
text(8, 14, "Londrina", cex=0.85, col="red") 
par(new=T) 

plot(Dou$AverageTemperature, type="s", bty="n", ann=F, axes=F, col="darkblue")
axis(side=4)
text(37, 18, "Dourados", cex=0.85, col="darkblue") #

title(main="Londrina x Dourados")




df_1996londrina <- subset(Lon, Year == 1996)
plot(AverageTemperature ~ dt, data = df_1996londrina, log = "x")

df_1996dourados <- subset(Dou, Year == 1996)
plot(AverageTemperature ~ dt, data = df_1996dourados, log = "x")

title(main="df_1996dourados x df_1996londrina")

# Através de um histograma

# Chamandoos dados

VT_Lon = Lon$AverageTemperature

# Construindo um histograma
hist(VT_Lon)
hist(VT_Lon, breaks = 5)
hist(VT_Lon, labels = T, breaks= c(0,5,10,20,30))
hist(VT_Lon, labels = T, breaks = 5)

hey = hist(VT_Lon, breaks = 5)
xaxis = seq(min(VT_Lon), max(VT_Lon), length = 10)
yaxis = dnorm(xaxis, mean = mean(VT_Lon), sd = sd(VT_Lon))
yaxis = yaxis*diff(hey$mids)*length(VT_Lon)

lines(xaxis, yaxis, col = "red")


