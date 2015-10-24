# LABORATORIUM 1 (06.10.2015)
Data =read.csv("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/daneSoc.csv", sep =";")
typeof(Data)
#Lista
class(Data)
#ramka danych
#typy zmiennych- ilosciowe(liczby) i jako?ciowe(nominalne)(kategorie)
head(Data)
#wiek(ilo?ciowe), wyksztalcenie(jako?ciowe), st.cywilny(jako?ciowe), plec(j),praca(j),
#cisnienie skurczowe(il), cisnienie rozk(il)
summary(Data)
length(Data)
?length
length(Data$wiek)
#jest 204 pacjent?w
#tablica kontyngencji dla zmiennych wyksztalcenie i praca
table(Data$wyksztalcenie,Data$praca)

#podstawowe statystyki dla zmiennej opisuj?cej cis. skur. w grupie m?zczyzn ze srednim wyksztalceniem
x<-Data$cisnienie.skurczowe[Data$plec=='mezczyzna'&Data$wyksztalcenie=='srednie']
x
Data
mean(x) #136.9744
median(x) #138
sd(x) #16.82493
min(x)
max(x)
#zakres: 93-168

#wykres typu boxplot dla zmiennej opisujacej cisnienie skurczowe 
#w grupach mezczyzn zatrudnionych i bez pracy
y<-Data$cisnienie.skurczowe[Data$plec=='mezczyzna'&Data$praca=='uczen lub pracuje']
par(mfrow=c(1,2))
boxplot(y)
w<-Data$cisnienie.skurczowe[Data$plec=='mezczyzna'&Data$praca=='nie pracuje']
boxplot(w)

#Znajd? pacjent?w z wykszta?ceniem ?rednim kt?rych ci?nienie skurczowe jest pomi?dzy 140 i 150.
which(Data$wyksztalcenie=='srednie'& (Data$cisnienie.skurczowe>=140 & Data$cisnienie.skurczowe<=150))
#jest ich 11

#Znajd? pacjenta (pacjent?w) z najwi?ksz? warto?ci? ci?nienia skurczowego.
t<-max(Data$cisnienie.skurczowe)
t
which(Data$cisnienie.skurczowe==t)
#inaczej:
which(Data$cisnienie.skurczowe==max(Data$cisnienie.skurczowe))

#Znajd? pacjenta(/?w) kt?rych ci?nienie skurczowe jest wi?ksze ni? empiryczny kwantyl rz?du 0.8 tej zmiennej.
which(Data$cisnienie.skurczowe>quantile(Data$cisnienie.skurczowe, 0.8))
length(which(Data$cisnienie.skurczowe>quantile(Data$cisnienie.skurczowe, 0.8)))
#jest ich 38


#Zadanie 2.
par(mfrow=c(2,2))
for(k in c(10,50,100,500)){
x=rnorm(k,0,1)
qqnorm(x)
}

# LISTA ZADA? NUMER 2:
