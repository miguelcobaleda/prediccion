library("quantmod") #Package to download financials historical data 
library(forecast)
library("fGarch")
library(vars)
library(depmixS4)
library(TTR)
library(ggplot2)
library(reshape2)
library(xts)
library(extrafont)

#funciones
archTest <- function(rtn,m=10){
  # Perform Lagrange Multiplier Test for ARCH effect of a time series
  # rtn: time series
  # m: selected AR order
  # TSAY(2013)
  y=(rtn-mean(rtn))^2
  T=length(rtn)
  atsq=y[(m+1):T]
  x=matrix(0,(T-m),m)
  for (i in 1:m){
    x[,i]=y[(m+1-i):(T-i)]
  }
  md=lm(atsq~x)
  summary(md)
}


###FIN FUNCIONES




#Yahoo ticker (stock or index) 
sSymbol="BMW.DE"

#get data from yahoo
mData<-getSymbols(sSymbol ,from="1990-01-01",to="2016-09-30",auto.assign=FALSE)
#Define workdata
xData=Ad(mData)

#Calculate Daily Arithmetic Return
dRentCont=dailyReturn(xData,type='log',leading=FALSE)
#Exclude NA (First data)
dRentCont=na.exclude(dRentCont)

plot.zoo(cbind(xData,dRentCont),main=paste(sSymbol," y  Rentabilidad"),xlab="años",ylab=c("Precio","rentabilidad"))
grid(lwd=2)


#Volatilidad GARCH
#Plot return squared
plot.zoo(cbind(Ad(mData),dRentCont,dRentCont^2),main=paste(sSymbol," y  Rentabilidad"),xlab="años",ylab=c("Precio","rentabilidad","Volatilidad"))

#testing mean
t.test(dRentCont)

#ACF & PACF 
# VolProxy=abs(dRentCont) # absolute value
VolProxy=dRentCont^2 #squared


#ACF y PACF
tsdisplay(VolProxy) 

#Ljung-Box Test 
Box.test(VolProxy,lag=10,  type="Lj")
Box.test(VolProxy,lag=20,  type="Lj")
Box.test(VolProxy,lag=40,  type="Lj")

#LM test
archTest(dRentCont,20)

#ARCH(1)
m1=garchFit(~1+garch(1,0),data=dRentCont,trace=F) # Fit an ARCH(1) model
summary(m1)
resi=residuals(m1,standardize=T) #residuals
resi=xts(resi,order.by=index(dRentCont)) #residuals as xts
tsdisplay(resi^2) #acf pacf residuals

#GARCH(1,1)
m2=garchFit(~1+garch(1,1),data=dRentCont,trace=F) # Fit an GARCH(1,1) model
summary(m2)

resi=residuals(m2,standardize=T) #residuals
resi=xts(resi,order.by=index(dRentCont)) #residuals as xts
tsdisplay(resi^2) #acf pacf residuals
plot(m2)


#t-student
m3=garchFit(~1+garch(1,1),data=dRentCont,trace=F,cond.dist="std")
summary(m3)
plot(m3)


v1=volatility(m3)  # Obtain volatility
v1=xts(v1,order.by=index(dRentCont)) #  volatility as XTS
plot(sqrt(252)*v1)

resi=residuals(m3,standardize=T) # Standardized residuals
resi=xts(resi,order.by=index(dRentCont)) # Standardized residuals as XTS
tsdisplay(resi^2) #acf pacf residuals
plot(resi)

predict(m3) #forecast volatility
predict(m3, n.ahead = 10, plot=TRUE, crit_val=2) #plot with 2*standard error
predict(m3,n.ahead=20,plot=TRUE,conf=.9,nx=100) # plot 100 data with 90% confidence





## VAR



## Leer datos
bmw=getSymbols("BMW.DE",env=NULL)
vw=getSymbols("VOW.DE",env=NULL)
# Generar rentabilidad mensual
rbmw=monthlyReturn(tef[,6])
rvw=monthlyReturn(ibex[,6])

#generar vector
vY=cbind(rbmw,rvw)
colnames(vY)=c("BMW.DE","VOW.DE")
vY=na.omit(vY)

#Seleccionar modelo
VARselect(vY)
#estimar
model.var=VAR(vY)
summary(model.var)
model.var1=VAR(vY,type="none")
summary(model.var1)
#causalidad de granger
causality(model.var1)
#respuesta al impulso
model.ri=irf(model.var1)
model.ri
plot(model.ri)
##prediccion
predict(model.var1, n.ahead = 8, ci = 0.95) 









## HHM

### Leer datos
bmw=getSymbols("BMW.DE",env=NULL)
bmw=na.omit(bmw)
#crear XTS 
mData=bmw$BMW.DE.Adjusted
colnames(mData)=c("Close")

#crear XTS semanal
semanal=function(mData){
  aa=seq.Date(as.Date(min(index(mData))),length.out=2+as.numeric(as.Date(max(index(mData)))-as.Date(min(index(mData)))),by="1 days")
  bb=xts(rep(NA,length(aa)),aa)
  cc=bb[time(bb[.indexwday(bb)==5])]
  dd=sapply(1:(length(cc)-1), function(x) last(mData[seq.Date(as.Date(time(cc[x])),as.Date(time(cc[x+1])),1)]))
  coredata(cc[2:(length(cc))])=dd
  return(cc)
}


mDataLR=semanal(mData)
#Añadir Rentabilidad
colnames(mDataLR)=c("Close")
#
mDataLR$Rentabilidad <- log(mDataLR$Close) - lag(log(mDataLR$Close),k=2)
#elimnar NAs
mDataLR <- na.exclude(mDataLR)

#Transformar XTS en DF
mDataLRdf <- data.frame(mDataLR)
#Poner la fecha que esta en el nombre de la fila como columna de fecha con formato
mDataLRdf$Date <-as.Date(row.names(mDataLRdf),"%Y-%m-%d")


#definir modelo HHM de markov con 2 estados. Rentabilidad en función de la constante
mod <- depmix(Rentabilidad ~ 1, family = gaussian(), nstates = 2, data = mDataLR)
set.seed(1)

# Estimar
fm2 <- fit(mod, verbose = FALSE)
#Resumen de resultados
summary(fm2)
print(fm2)

# Compute probability of being in each state
probs <- posterior(fm2)             
mDataLRdf$pBull <- probs[,2]  
mDataLRdf$pBear <- probs[,3]
mDataLRdf$pState <- probs[,1]

#Nombre a la Primera columna
#colnames(mDataLRdf$logret)=c("Rentabilidad")
nameStock <- colnames(mDataLRdf)[1]


#Crear df para ggplot2
df <- melt(mDataLRdf[,c(1,2,3,4,5,6)],id="Date",measure=c(nameStock,"Rentabilidad","pBull","pBear","pState"))


##Gráfico Probabilidad
positivoColor=subset(df,df$variable =="Rentabilidad")
pColor=ifelse(positivoColor$value >=0, "blue", "red")
f <- ggplot()+
  geom_step(data=subset(df,df$variable ==nameStock),aes(Date, value))+
  geom_linerange(data=positivoColor,aes(Date, value,ymin=0,ymax=value),color = pColor)+
  geom_linerange(data=subset(df,df$variable =="pBull"),aes(Date, value,ymin=0,ymax=value),color="cornflowerblue")+
  facet_grid(variable ~., scales = "free", as.table = TRUE) + 
  scale_x_date(date_breaks = "1 years",date_labels = "%y")+
  theme_bw() + 
  theme(panel.spacing = unit(0,"lines"), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"))+
  ggtitle("Grafico de Estados")

f 

#####ESTADOs
f <- ggplot()+
  geom_step(data=subset(df,df$variable ==nameStock),aes(Date, value))+
  geom_linerange(data=positivoColor,aes(Date, value,ymin=0,ymax=value),color = pColor)+
  geom_linerange(data=subset(df,df$variable =="pBull"),aes(Date, value,ymin=0,ymax=value),color="cornflowerblue")+
  geom_step(data=subset(df,df$variable =="pState"),aes(Date, 2-value),color="cornflowerblue",size=1)+
  facet_grid(variable ~., scales = "free", as.table = TRUE) +   
  scale_x_date(date_breaks = "1 years",date_labels = "%y")+
  theme_bw() + 
  theme(panel.spacing = unit(0,"lines"), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"))+
  ggtitle("Ciclos del BMW: Alcista vs Bajista")+labs(caption = "BMW Hidden Markov Model two states: rentabilidades quincenales")
f
