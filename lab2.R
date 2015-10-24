#LISTA ZADAÑ NR 2
#ZADANIE 2.1
x=seq(0,10,0.1)
e=rnorm(101,0,3)
plot(x,x+e)

#Oblicz wspó³czynnik korelacji próbkowej korzystaj¹c z definicji oraz funkcji cor() w pakiecie R
?cor #wspolczynnik korelacji
cor(x,x+e)

# z definicji:
cov(x,x+e)/sqrt(var(x)*var(x+e))

#Oblicz wspó³czynniki prostej MNK korzystaj¹c z definicji oraz funkcji lm() w pakiecie R
ramka=data.frame(x,x+e)
ramka.lm=lm(ramka)
abline(ramka.lm)
#czy R^2=g^2(x,y)

#ZADANIE 2.3
par(mfrow=c(2,2))
Data=read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/anscombe_quartet.txt",h=T)
head(Data)
model=lm(Data$Y1~Data$X1)
summary(model)
plot(Data$X1,Data$Y1)
abline(model)

model2=lm(Data$Y2~Data$X2)
summary(model2)
plot(Data$X2,Data$Y2)
abline(model2)

model3=lm(Data$Y3~Data$X3)
summary(model3)
plot(Data$X3,Data$Y3)
abline(model3)

model4=lm(Data$Y4~Data$X4)
summary(model4)
plot(Data$X4,Data$Y4)
abline(model4)


#2)-nie liniowa- kwadratowa?
#3)-obserwacja odstaj¹ca,która zaburza => modyfikacja danych
#4)-dziwne dane, brak zale¿noœci liniowej

#wspó³czynnik determinacji R^2-> ma sens tylko jeœli model jest liniowy!!!

#wspó³czynnik korelacji=sqrt(R^2) !!!

#------------------------------------------------------------------------------------------
#Model regresji wielokrotnej:
# y_i=B0+B1xi,1+...+B_p-1 xi,p-1+Ei

#w postaci macierzowej:
#Y=XB+E
#jedna zmienna odpowiedzi, wiele zmiennych objaœniaj¹cych

#typowe zastosowanie:
#ZADANIE 2.4
dane2=read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/realest.txt",h=T)
head(dane2)

#a) wyznaczyæ macierz eksperymentu:= macierz X
mod1=lm(dane2$Price~.,data=dane2)
summary(mod1)


?as.matrix
X<-as.matrix(dane2[-1])
#?cbind
X
Y=cbind(1)
merge(Y,X)
#b) estymatory parametrów 
C=t(X)
D=as.matrix(dane2[1])
B=solve(C%*%X)%*%C%*%D
B
?matrix
??reverse
summary(mod1)
residuals(mod1)
#SSE=915
#SST=3957
SSE=sum(residuals(mod1)^2)
SSE
Y=X%*%B
SST=sum((Y-mean(Y))^2)
SST
R=1-(SSE/SST)
R
#c) 
#niektóre wspó³czynniki s¹ ujemne
#np. przy ustalonych pozosta³ych zmiennych jeœli zwiêkszymy liczbê sypialni to cena zmaleje\
#w szczególnoœci jest ustalona zmienna powierzchnia!!

mod3=lm(dane2$Price~dane2$Bedroom)
summary(mod3)

#d)
#x=c(3,1500,8,)
predict(mod1,newdata=data.frame(Bedroom=3,Space=1500,Room=8,Lot=40,Bathroom=2,Garage=1,Tax=1000,Condition=0))

#e)
#sigmazdaszkiem^2=(1/(n-p))SSE
summary(mod1)$sigma^2

p=ncol(dane2)
sum(residuals(mod1)^2)/(nrow(dane2)-p)
