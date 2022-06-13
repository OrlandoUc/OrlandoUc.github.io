#cargando libreriras
library(vars)
library(forecast)
library(DescTools)
library(tseries)

#cargando los datos
datos <- read.csv("datos_movilidad_gto_final.csv", header=TRUE, row.names = 1)
attach(datos)

#se convierten a un formato TS para poder graficarlos adecuadamente
datos<-ts(datos,frequency=365,start=c(2020,02))
ts.plot(datos)


library(mgcv)
#si estan relacionados
gam_mod <- gam(confirmados_suavizados~ promedio_azul_verde)
summary(gam_mod)

#si estan relacionados
gam_mod <- gam(nrd_suavizado~ promedio_azul_verde)
summary(gam_mod)

plot(promedio_azul_verde,type='l',col="brown")
lines(nrd_suavizado,typecol="black")

plot(scale(promedio_azul_verde),type='l',col='brown',main="Comparacion series",
     xlab="dia",ylab="porcentaje",ylim=c(-2,5))
lines(scale(confirmados_suavizados),col="red",type='l')
par(new = TRUE)
plot(nrd_suavizado, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "",ylim=c(0,2))
abline(h=1,col='red')
axis(side=4, at = pretty(range(nrd_suavizado,na.rm = TRUE)))
mtext("NRD", side=4, line=3)



lines(scale(sintomas_suavizado),col="red",type='l')
abline(h=1,col='red')
legend(c(150,250),c(3,5), 
       legend=c("workshop", "transit_station",
                "promedio azul y verde","NRD","sintomas"),
       col=c("blue", "green","brown","black","red"), lty=1,cex=0.55)

#guanajuto_activos1["D_lag_1"] <- shift(guanajuto_activos1$D, n=1) guanajuto_activos1 <- guanajuto_activos1 %>%                         mutate(change_point= ((D<=1)&(D_lag_1>1)) | ((D>1)&(D_lag_1<1)) )  fechas_cruce <- guanajuto_activos1 %>% filter(change_point) %>% select(FECHA_INGRESO) fechas_cruce <- c(fechas_cruce$FECHA_INGRESO-1,fechas_cruce$FECHA_INGRESO)


##MODELO DE FACTORES DINAMICOS
source("functions_adf.R")
source("functions_dynamic_factors.R")

#workplaecs y transit station
#datos_FD = datos[,2:5]
#promedio
datos_FD = datos[,-(2:3)]


X <- scale(datos_FD)

#grafico de las series
ts.plot(X, xlab="Tiempo", ylab="ITAAE", 
        main ="Gráfico de los ITAEE de los 32 estados de 1993/01 a 2019/02")

#descomposicion en valores propios
XtX <- t(X)%*%X
ed <- eigen(XtX)
# criterio tradicional
cumsum(ed$values/sum(ed$values))

rhat <- onatski2010(X, demean = 0)["ed"]
rhat

mod1_compr_princ <- pcfest(X, 1, demean = 0, constant = 1)

#graficando los residuales del modelo de factores dinamicos
et <- mod1_compr_princ$ehat
ts.plot(et, xlab="Tiempo", ylab="F_hat 1",
        main ="Residuales del modelo de FD", col=1:3)

# verificando que et ~ I(0)
pooled.test(et)

Ft <- -mod1_compr_princ$Fhat #F_hat
adf_test <- adf_test_fd <- c()
for(i in 1 : 2){
  #pruebas adf a F_hat
  adf_test[i] <- adf(Ft[,i], "trend")$p.value
  #pruebas adf a la primera diferencia de F_hat
  adf_test_fd[i] <- adf(diff(Ft[,i]), "none")$p.value
}
adf_test
adf_test_fd

#grafica componente principal 1
plot(-mod1_compr_princ$Fhat[,1], xlab="Días", ylab="F_hat 1",
        main ="Componente principal 1", col="blue",type='l',lwd=3)
abline(v=c(1,16,31,47,62,77,92,108,123,139,154,169,184,200,215,230,245,261), col='gray80', lty=2)
abline(h=c(-2,-1,0,1,2,3,4,5), col='gray80', lty=2)
axis(side= 1, at=1, labels=datos$date2[1], las=2, cex.axis=0.9)
axis(side= 1, at=16, labels=datos$date2[16], las=2, cex.axis=0.9)
axis(side= 1, at=31, labels=datos$date2[31], las=2, cex.axis=0.9)
axis(side= 1, at=47, labels=datos$date2[47], las=2, cex.axis=0.9)
axis(side= 1, at=62, labels=datos$date2[62], las=2, cex.axis=0.9)
axis(side= 1, at=77, labels=datos$date2[77], las=2, cex.axis=0.9)
axis(side= 1, at=92, labels=datos$date2[92], las=2, cex.axis=0.9)
axis(side= 1, at=108, labels=datos$date2[108], las=2, cex.axis=0.9)
axis(side= 1, at=123, labels=datos$date2[123], las=2, cex.axis=0.9)
axis(side= 1, at=139, labels=datos$date2[139], las=2, cex.axis=0.9)
axis(side= 1, at=154, labels=datos$date2[154], las=2, cex.axis=0.9)
axis(side= 1, at=169, labels=datos$date2[169], las=2, cex.axis=0.9)
axis(side= 1, at=184, labels=datos$date2[184], las=2, cex.axis=0.9)
axis(side= 1, at=200, labels=datos$date2[200], las=2, cex.axis=0.9)
axis(side= 1, at=215, labels=datos$date2[215], las=2, cex.axis=0.9)
axis(side= 1, at=230, labels=datos$date2[230], las=2, cex.axis=0.9)
axis(side= 1, at=245, labels=datos$date2[245], las=2, cex.axis=0.9)
axis(side= 1, at=261, labels=datos$date2[261], las=2, cex.axis=0.9)

#grafica residuales
plot(et[,1], xlab="", ylab="F_hat 1",xaxt="n",
     main ="Residuales modelo FD", col=1,type='l',ylim=c(-2,2))
lines(et[,2],col=2)
lines(et[,3],col=3)
grid(10,10)
abline(v=c(1,16,31,47,62,77,92,108,123,139,154,169,184,200,215,230,245,261), col='gray80', lty=2)
abline(h=c(-2,-1,0,1,2,3,4,5), col='gray80', lty=2)
axis(side= 1, at=1, labels=datos$date2[1], las=2, cex.axis=0.7)
axis(side= 1, at=16, labels=datos$date2[16], las=2, cex.axis=0.7)
axis(side= 1, at=31, labels=datos$date2[31], las=2, cex.axis=0.7)
axis(side= 1, at=47, labels=datos$date2[47], las=2, cex.axis=0.7)
axis(side= 1, at=62, labels=datos$date2[62], las=2, cex.axis=0.7)
axis(side= 1, at=77, labels=datos$date2[77], las=2, cex.axis=0.7)
axis(side= 1, at=92, labels=datos$date2[92], las=2, cex.axis=0.7)
axis(side= 1, at=108, labels=datos$date2[108], las=2, cex.axis=0.7)
axis(side= 1, at=123, labels=datos$date2[123], las=2, cex.axis=0.7)
axis(side= 1, at=139, labels=datos$date2[139], las=2, cex.axis=0.7)
axis(side= 1, at=154, labels=datos$date2[154], las=2, cex.axis=0.7)
axis(side= 1, at=169, labels=datos$date2[169], las=2, cex.axis=0.7)
axis(side= 1, at=184, labels=datos$date2[184], las=2, cex.axis=0.7)
axis(side= 1, at=200, labels=datos$date2[200], las=2, cex.axis=0.7)
axis(side= 1, at=215, labels=datos$date2[215], las=2, cex.axis=0.7)
axis(side= 1, at=230, labels=datos$date2[230], las=2, cex.axis=0.7)
axis(side= 1, at=245, labels=datos$date2[245], las=2, cex.axis=0.7)
axis(side= 1, at=261, labels=datos$date2[261], las=2, cex.axis=0.7)

