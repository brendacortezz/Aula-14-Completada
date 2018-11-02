                    #Aula 14 - Quebra Estrural e Bolhas

install.packages("strucchange")

library(strucchange)
library(readxl)


AION <- read_excel("C:/Econometria/AION.xlsx")

AION <- ts <- ts(AION$Fechar, start = 2017, frequency = 365)

plot(AION)

#Teste de Chow

chow <- Fstats(AION~1)    #Executa o Teste de F de Chow
sctest(chow)                 #Retorna a Estatística de Teste e o p-valor

plot(AION)
lines(breakpoints(chow))

plot(chow)
    
#Teste Bai Perron

bp_ts <- breakpoints(AION ~ 1)

bp_ts

summary(bp_ts)

#ci_ts <- confint(bp_ts)

plot(AION)               
lines(bp_ts)            #Gráfico com os breakpoints


#Gráfico com as linhas de tendências para os três períodos

fm0 <- lm(AION ~ 1)
fm1 <- lm(AION ~ breakfactor(bp_ts, breaks = 1))
fm2 <- lm(AION ~ breakfactor(bp_ts, breaks = 2))
plot(AION)
lines(ts(fitted(fm0), start = 2017, freq=365), col = 3)
lines(ts(fitted(fm1), start = 2017, frequency=365), col = 4)
lines(ts(fitted(fm2), start = 2017, frequency=365), col = 1)
lines(bp_ts)


#Estimar o Melhor Modelo ARIMA

#Modelo Integrado de Ordem 1

MIO1 <- diff(AION)
plot(MIO1)

#É estacionária?

#FAC e FACP

#Qual a ordem do modelo ARIMA(p,d,q)

#Quais combinações a serem estimadas?

#Os valores AIC e BIC dos modelos possíveis.

#O melhor modelo

#Previsão para os 6 próximos meses do valor do Bitcoin utilizando o melhor modelo



