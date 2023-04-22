#Trabalho de Séries Temporais 
#elaborado por Carina Ribeiro e Daniela Quintas 
#Analise de dados, recorrendo a metodologia Box-Jenkins

##dados
library(MASS)
mortes<-accdeaths
class(mortes)
str(mortes)
summary(mortes) 

plot(mortes)
#atraves do grafico vemos que a primeira vista a variância parece constante
#mas a media não, pelo que a serie temporal não é estacionaria
#tambem vemos que esta parece ter sazonalidade

##################################
# MODELO  SARIMA(p,d,q)x(P,D,Q)s #
##################################
# adoptar diferenciaçao (Alternativa: selecionar modelos deterministicos para  
# modelar as componentes tendencia e sazonal)

# METODOLOGIA BOX-JENKINS 
#
# PASSO 1. estabilizar variancia
# PASSO 2. ordem da diferenciaçao d=? (estabilizar tendencia)
# PASSO 3. periodo s=? (detectar sazonabilidade)
# PASSO 4. se s diferente de 0 entao é necessario identificar as 
#          ordens sazonais D=?, P=? e Q=?
#          Para P=1, D=Q=0, se coeficiente AR proximo de 1, entao D=1 
# PASSO 5. ordens da parte regular p=? e q=?


#PASSO 1: estabilizar a variancia
par(mfrow=c(2,1))
plot(mortes,main=" Dados originais")
plot(log(mortes),main=" Dados logaritmizados")
#pelo gráfico observamos que não há evidencias para não considerar
#a variancia constante, pelo que iremos continuar a usar 
#a serie original
#(consideramos a variancia constante)


#PASSO 2: ordem da diferenciaçao d=? (estabilizar tendencia)

#visualizar tendencia dos dados 
par(mfrow=c(1,1))
plot((decompose(mortes))$trend, main="Tendência dos dados")
# d=?
par(mfrow=c(2,1))
auxiliar = arima(mortes, order=c(0,1,0), seasonal=list(order=c(0,0,0), period=0))
plot(auxiliar$residuals, main="Diferenciação (d=1)") 

auxiliar2 = arima(mortes, order=c(0,2,0), seasonal=list(order=c(0,0,0), period=0))
plot(auxiliar2$residuals, main="Diferenciação (d=2)") 

#vamos utilizar as duas ordens de diferenciaçao (d=1 e d=2) e no 
#final verificar qual melhor modelo
#diferenciamos para atingir estacionariedade 

#PASSO 3: periodo s=? (detectar sazonabilidade)

periodogram=spectrum(as.vector(mortes), plot=F)
imax=which.max(periodogram$spec)
periodo=1/periodogram$freq[imax]
periodo
# vamos considerar o periodo igual a 12 meses #serie tem sazonalidade

plot(decompose(mortes)) 
# o periodo tambem pode ser confirmado a partir da decomposição do 
#grafico nomeadamente através da parte seasonal, uma vez que se 
#verifica sazonalidade com periodo 1 ano (12 meses)

#Ver em que mês ocorrem os picos
which(mortes[1:12]==max(mortes[1:12]))#ano 1973 foi no mes julho
which(mortes[12:24]==max(mortes[12:24]))#ano 1974 foi no mes agosto
which(mortes[24:36]==max(mortes[24:36]))#ano 1975 foi no mes agosto
which(mortes[36:48]==max(mortes[36:48]))#ano 1976 foi no mes agosto
which(mortes[48:60]==max(mortes[48:60]))#ano 1977 foi no mes agosto
which(mortes[60:72]==max(mortes[60:72]))#ano 1978 foi no mes agosto


#A maioria dos picos ocorre no mes de Agosto de cada ano, exceto no
#ano de 1973 em que o pico ocorreu no mes de Julho
########Primeiramente vamos considerar d=1 

# PASSO 4: se s diferente de 0 entao D=? e ordens da parte sazonal P=? e Q=? 
# Faça P=1, D=Q=0. Se coeficiente AR proximo de 1, entao D=1   #P=1, Q=0
arima(mortes,order=c(0,1,0), seasonal=list(order=c(1,0,0), period=12))
#como o coeficiente é proximo de 1 vamos considerar D=1

arima(mortes, order=c(0,1,0), seasonal=list(order=c(1,1,0), period=12))
#AIC = 868.3

arima(mortes, order=c(0,1,0), seasonal=list(order=c(0,1,1), period=12))
#AIC=  864.13

arima(mortes, order=c(0,1,0), seasonal=list(order=c(1,1,1), period=12))
#nao é estatisticamente significativo, logo aceito H0 (H0:alfa=0 e H0:beta=0) #?

arima(mortes, order=c(0,1,0), seasonal=list(order=c(2,1,0), period=12))
#AIC= 866.48

arima(mortes, order=c(0,1,0), seasonal=list(order=c(0,1,2), period=12))
#nao é estatisticamente significativo, logo aceito H0 (H0:beta2=0) 

##escolhemos o modelo com menor AIC -> SARIMA(p,1,q)x(0,1,1)_s=12

#PASSO 5: ordens da parte regular p=? e q=?
arima(mortes, order=c(0,1,0), seasonal=list(order=c(0,1,1), period=12))
#AIC= 864.13

arima(mortes, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=12))
#AIC= 859.28

arima(mortes, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
#AIC =856.88

arima(mortes, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=12))
#nao é estatisticamente significativo, logo aceito H0 (H0:phi=0)

arima(mortes, order=c(2,1,0), seasonal=list(order=c(0,1,1), period=12))
#nao é estatisticamente significativo, logo aceito H0 (H0:phi2=0)

arima(mortes, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=12))
#nao é estatisticamente significativo, logo aceito H0 (H0:teta2=0)

##escolhemos o modelo com menor AIC -> ARIMA(0,1,1)x(0,1,1)s=12

############
# CONCLUSAO: Modelo com melhor AIC SARIMA(0,1,0)x(0,1,1)s=12
############

#Analisar agora se os resíduos são ruídos brancos!

modelo1 <- arima(mortes, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
modelo1
names(modelo1)

#variância estimada dos erros
modelo1$sigma2  

#residuos do modelo
residuos1=modelo1$residuals

par(mfrow=c(2,3))
plot(mortes)
acf(mortes)
pacf(mortes)

# testar independência dos resíduos 
plot(residuos1, main="ARIMA(0,1,1)x(0,1,1)_s=12")
acf(residuos1)
pacf(residuos1)

#ver como fazer a 90%

Box.test(residuos1)
#assumimos que os resíduos são independentes  

#testar a normalidade dos residuos
library(nortest) 
lillie.test(residuos1) 
qqnorm(residuos1)
qqline(residuos1)
hist(residuos1)


#como p-value<0.05 consideramos que os residuos não tem uma distribuiçao
#normal 

#testar se a media dos residuos = 0
t.test(residuos1)
#como p-value > 0.05 não se rejeita H0:E[E_t] =0, para o nivel de 
#significancia de 5%, logo a média dos residuos e 0

#analisar se a variancia é constante #??
plot(as.vector(residuos1))
plot(residuos1)


#observamos pelos graficos que os residuos tem efetivamente uma variancia
#constante uma vez que nao se verifica nenhum padrao

library(forecast)
checkresiduals(modelo1)

#############################
# PREVISAO COM MODELOS ARIMA #
##############################
par(mfrow=c(3,1))


# Validar o modelo, ignorando ultimas 24 observaçoes e comparando valores
# estimados e observados

m1 = arima(mortes[1:48], order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
previsoes <- predict(m1, n.ahead = 24)
mortes[49:72]
previsoes$pred
previsoes$se

ts.plot(mortes, xlim=c(1973,1978), ylim=c(5000,13000), col="red", main="Previsões versus observações")
lines(ts(previsoes$pred, start=1977, deltat=1/12))
low = previsoes$pred - 1.96*previsoes$se
upper = previsoes$pred + 1.96*previsoes$se
lines(ts(low, start=1977, deltat=1/12), lty=3)
lines(ts(upper, start=1977, deltat=1/12), lty=3)

# Atraves do grafico conclui se que o modelo ajustado é um bom
#modelo uma vez que as previsoes estao muito proximas das obseraçoes reais


# Forecasting para 1979 e 1980 (dados existentes apenas ate 1978)
ts.plot(mortes, xlim=c(1973,1980), ylim=c(5000,13000), col="red", main="Previsão 1979/1980")
m1 = arima(mortes, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
previsoes79_80 <- predict(m1, n.ahead = 24)
previsoes79_80$pred
previsoes79_80$se
lines(ts(previsoes79_80$pred, start=1979, deltat=1/12), col="blue")
low = previsoes79_80$pred - 1.96*previsoes79_80$se
upper = previsoes79_80$pred + 1.96*previsoes79_80$se
lines(ts(low, start=1979, deltat=1/12), lty=3, col="blue")
lines(ts(upper, start=1979, deltat=1/12), lty=3, col="blue")

# NOTA: Todas as previsoes deverao ser convertidas para a grandeza original, 
#       invertendo a transformaçao box-cox
#ts.plot((((mort*lambda)+1)^(1/lambda)), xlim=c(1973,1980), ylim=c(5000,14000), col="red", main="Forecasting 1979/1980 - mortes ",ylab="mortess")
#lines(ts((((previsoes79_80$pred*lambda)+1)^(1/lambda)), start=1979, deltat=1/12), col="blue")
#lines(ts((((low*lambda)+1)^(1/lambda)), start=1979, deltat=1/12), lty=3, col="blue")
#lines(ts((((upper*lambda)+1)^(1/lambda)), start=1979, deltat=1/12), lty=3, col="blue")


########Agora vamos considerar d=2

# PASSO 4: se s diferente de 0 entao D=? e ordens da parte sazonal P=? e Q=? 
# Faça P=1, D=Q=0. Se coeficiente AR proximo de 1, entao D=1   #P=1, Q=0
arima(mortes, order=c(0,2,0), seasonal=list(order=c(1,0,0), period=12))
#como o coeficiente é proximo de 1 vamos considerar D=1 

arima(mortes, order=c(0,2,0), seasonal=list(order=c(1,1,0), period=12))
#AIC = 917.4

arima(mortes, order=c(0,2,0), seasonal=list(order=c(0,1,1), period=12))
#AIC= 912.82

arima(mortes, order=c(0,2,0), seasonal=list(order=c(1,1,1), period=12))
#nao é estatisticamente significativo, logo aceito H0 (H0:beta=0 e H0:alfa=0)

arima(mortes, order=c(0,2,0), seasonal=list(order=c(2,1,0), period=12))
#AIC= 914.26

arima(mortes, order=c(0,2,0), seasonal=list(order=c(0,1,2), period=12))
#nao é estatisticamente significativo, logo aceito H0 (H0:beta2=0)


##escolhemos o modelo com menor AIC -> ARIMA(p,2,q)x(0,1,1)_s=12

#PASSO 5: ordens da parte regular p=? e q=?
arima(mortes, order=c(0,2,0), seasonal=list(order=c(0,1,1), period=12))
#AIC= 912.82

arima(mortes, order=c(1,2,0), seasonal=list(order=c(0,1,1), period=12))
#AIC= 889.56

arima(mortes, order=c(0,2,1), seasonal=list(order=c(0,1,1), period=12))
#AIC = 862.56

arima(mortes, order=c(1,2,1), seasonal=list(order=c(0,1,1), period=12))
#AIC = 857.8

arima(mortes, order=c(2,2,0), seasonal=list(order=c(0,1,1), period=12))
#AIC = 874.62

arima(mortes, order=c(0,2,2), seasonal=list(order=c(0,1,1), period=12))
#AIC = 854.96

##escolhemos o modelo com menor AIC -> ARIMA(0,2,2)x(0,1,1)_s=12

############
# CONCLUSAO: Modelo com melhor AIC ARIMA(0,2,2)x(0,1,1)_s=12
############

modelo2 <- arima(mortes, order=c(0,2,2), seasonal=list(order=c(0,1,1), period=12))
modelo2
names(modelo2)

#variancia estimadas dos erros 
modelo2$sigma2

#residuos do modelo
residuos2 <- modelo2$residuals


##Analise dos resíduos 
library(nortest)
lillie.test(residuos2) 
#segue um distribuição normal se considerarmos o nivel de significancia 1%
qqnorm(residuos2)
qqline(residuos2)
hist(residuos2)

#testar se a media dos residuos = 0
t.test(residuos2)
#como p-value > 0.05 não se rejeita H0:E[E_t] =0, para o nivel de 
#significancia de 5%, logo a média dos residuos e 0

#analisar se a variancia é constante #??
plot(as.vector(residuos2))
plot(residuos2)
#observamos pelos graficos que os residuos tem efetivamente uma variancia
#constante uma vez que nao se verifica nenhum padrao

library(forecast)
checkresiduals(modelo2)

#############################
# PREVISAO COM MODELOS ARIMA #
##############################
par(mfrow=c(3,1))

# Validar o modelo, ignorando ultimas 24 observaçoes e comparando valores
# estimados e observados


m2 = arima(mortes[1:48], order=c(0,2,2), seasonal=list(order=c(0,1,1), period=12))
previsoes2 <- predict(m2, n.ahead = 24)
mortes[49:72]
previsoes2$pred
previsoes2$se

ts.plot(mortes, xlim=c(1973,1978), ylim=c(5000,13000), col="red", main="Previsões versus observações")
lines(ts(previsoes2$pred, start=1977, deltat=1/12))
low = previsoes2$pred - 1.96*previsoes2$se
upper = previsoes2$pred + 1.96*previsoes2$se
lines(ts(low, start=1977, deltat=1/12), lty=3)
lines(ts(upper, start=1977, deltat=1/12), lty=3)

# Atraves do grafico conclui se que o modelo ajustado é um bom
#modelo uma vez que as previsoes estao muito proximas das obseraçoes reais


# Forecasting para 1979 e 1980 (dados existentes apenas ate 1978)
ts.plot(mortes, xlim=c(1973,1980), ylim=c(5000,13000), col="red", main="Previsão 1979/1980")
m2 = arima(mortes, order=c(0,2,2), seasonal=list(order=c(0,1,1), period=12))
previsoes79_80 <- predict(m2, n.ahead = 24)
previsoes79_80$pred
previsoes79_80$se
lines(ts(previsoes79_80$pred, start=1979, deltat=1/12), col="blue")
low = previsoes79_80$pred - 1.96*previsoes79_80$se
upper = previsoes79_80$pred + 1.96*previsoes79_80$se
lines(ts(low, start=1979, deltat=1/12), lty=3, col="blue")
lines(ts(upper, start=1979, deltat=1/12), lty=3, col="blue")


#comparar os dois modelos

par(mfrow=c(1,2))

m1 = arima(mortes[1:48], order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
previsoes <- predict(m1, n.ahead = 24)
ts.plot(mortes, xlim=c(1973,1978), ylim=c(5000,13000), col="red", main="Previsões versus observações (d=1)")
lines(ts(previsoes$pred, start=1977, deltat=1/12))
low = previsoes$pred - 1.96*previsoes$se
upper = previsoes$pred + 1.96*previsoes$se
lines(ts(low, start=1977, deltat=1/12), lty=3)
lines(ts(upper, start=1977, deltat=1/12), lty=3)

m2 = arima(mortes[1:48], order=c(0,2,2), seasonal=list(order=c(0,1,1), period=12))
previsoes2 <- predict(m2, n.ahead = 24)
ts.plot(mortes, xlim=c(1973,1978), ylim=c(5000,13000), col="red", main="Previsões versus observações(d=2)")
lines(ts(previsoes2$pred, start=1977, deltat=1/12))
low = previsoes2$pred - 1.96*previsoes2$se
upper = previsoes2$pred + 1.96*previsoes2$se
lines(ts(low, start=1977, deltat=1/12), lty=3)
lines(ts(upper, start=1977, deltat=1/12), lty=3)
