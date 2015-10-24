

#za tydzien kartkowka z wykladu - 1 z definicji i 1 zeby policzyc


#zad4.1

#a) mortality-zmienna objasniana, inne to zm. objasniaj¹ce

Dane=read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/airpollution.txt",header=TRUE)
head(Dane)
#trzeba usunac ost kolummne bo jest zdublowana
#Dane<-Dane[,-1]-to usuwa pierwsza kolumne

length(Dane)
Dane<-Dane[,-length(Dane)]
head(Dane)
names(Dane)
model<-lm(Mortality~., data=Dane)
coef(model)

#test.t Ho:Bj==0, K:Bj!=0, testujemy wplyw tej zmiennej PRZY STALYCH POZOSTALYCH ZMIENNYCH
# statystyka t=wzor

est<-summary(model)
#to nam podaje p-wartosc i stat t te¿

#jak to obliczyc samemu np. dla Nox? 
#p-wartosc to pole pod wykresem rozkladu t-studenta t(n-p) miedzy policzona 
#statystyka t i nsk
#+pole miêdzy -t i -nsk.
#to pole to dystr rozkladu t(n-p) w punkcie -t

#dystrybuanta rozkl t-studenta - funkcja pt(stat t, wymiar)?

dim(Dane)#to podaje wymiar danych
#statystyke t bierzemy z summary, a n-p z wymiaru danych
pt(-est$coefficients[14,3], dim(Dane)[1]-dim(Dane)[2])*2

#test F

#w summaty test F jest w ost linijce
############################################################################

#zad4.2
x<-runif(100)
Bo<-0.5
B1=1
E<-rnorm(100, sd=0.05)
y<-Bo+B1*x^2+E
y


model2<-lm(y~x)
model2
summary(model2)


#Czy zmienna x1 jest istotna w tym modelu na poziomie istotnoœci 0.05?
# gdy p-val jest <0.05 to moz=wimy ze zm. x1 jest istotna


plot(x,y)
abline(model2)


#wniosek z tego zadania - gdy zmienna wychodzi istotna to nie wiemy w jaki sposob y od niej zalezy
#stat. F wychodzi bardzo duza, a jej p-wart b mala - to dokladnie to samo (bo jest tylko 1 wspolczynnik)
#########################################################################

#zad.4.3
k=0
moc1=numeric(10)
moc2=numeric(10)
moc3=numeric(10)
for (i in c(20,50,100,200,300,400,500,600,700,800))
{
  k=k+1
  a1=0
  a2=0
  a3=0
for (j in 1:100)
{
B0=0.5
B1=1
B2=0.5
B3=0.05

x1=rnorm(i)
x2=rnorm(i)
x3=rnorm(i)
e=rnorm(i, sd = 5)

y=B0+B1*x1+B2*x2+B3*x3+e

if (summary(lm(y~x1+x2+x3))$coef[2,4]<0.05) a1=a1+1
if (summary(lm(y~x1+x2+x3))$coef[3,4]<0.05) a2=a2+1
if (summary(lm(y~x1+x2+x3))$coef[4,4]<0.05) a3=a3+1

}
  moc1[k]=a1/100
  moc2[k]=a2/100
  moc3[k]=a3/100
}

moc1
moc2
moc3
#oszacuj moc testu dla poszczegolnyc zmiennych (chcemy zobaczy w ilu przypadkach pwie ze zmienne sa nieistotne
#mimo ze wiemy ze sa istotne)

plot(c(20,50,100,200,300,400,500,600,700,800), moc3, xlim =c(0,800), ylim = c(0,1), type="l", col = "blue")
 lines (c(20,50,100,200,300,400,500,600,700,800), moc1, col="green")
 lines(c(20,50,100,200,300,400,500,600,700,800), moc2, col = 'red')
 

