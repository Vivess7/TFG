################################
###         LLIBRERIES
################################


library(readxl)
library(tidyverse)
library(zoo)
library(TSA)
library(tseries)
library(MASS)
library(stats)

####TITOL: MODELITZACIO DE VALORS EXTREMS DEL MERCAT BURSATIL ########
####                    AMB LLEIS DE POTENCIES                ########





#####################################
###         DIFERENTS FREQÜENCIES
#####################################



################################
###         EURvsJPY
################################



######### FEM L'ESTUDI PER L'INTERVAL DE TEMPS ENTRE OBSERVACIONS 10s

EURJPY=read.csv('C:/Users/Gerard/Documents/TFG/EURJPY_10s.csv')
P_open=EURJPY$Open
open=ts(P_open)
open=as.numeric(open)
sum(is.na(open))

#calculem el logaritme i els retorns.
log_open=log(open)
log_dif_open=diff(log_open)
hist(log_dif_open)
log_pos=log_dif_open[log_dif_open>0]
log_neg=abs(log_dif_open[log_dif_open<0])

#####
library(ercv)

a=hist(log(log_pos),breaks=50)
hpos=hist(log_pos,breaks=exp(a$breaks))
LogRetorns_EURJPY=hpos$mids
densitat=hpos$density
plot(LogRetorns_EURJPY,densitat,log="xy")

library(evir)
hill(log_pos, end=65000)
hill(log_pos, end=15000)
#grafiquem els estimadors de Hill


#escollim umbral
thr_1=0.0002
log_pos_retail_1=log_pos[log_pos>=thr_1]
length(log_pos_retail_1)


#calculem el MLE pel paràmetre alpha
alpha_1=length(log_pos_retail_1)*1/sum(log(log_pos_retail_1/thr_1))

h1_1=hist(log(log_pos_retail_1))
h2_1=hist(log_pos_retail_1,breaks=exp(h1_1$breaks))
h2_1$breaks
#construim el model
constant_hist_1= length(log_pos_retail_1)/length(log_pos)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_pos_retail_1)/thr_1)^(-(alpha_1)-1)


#mesures de bondat de l'elecció del threshold
cvplot(log_pos, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_pos_retail_1))

#plot de l'ajust
plot(hpos$mids,hpos$density,log="xy")
lines(sort(log_pos_retail_1),model_pw_1 , col='red')


#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#Test de bondat d'ajust (Kolmogorov-Smirnov)
set.seed(77777)
u=runif(7500,0,1)


#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_pos_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
log_test=unique(log_pos_retail_1)
ks.test(log_test, simulacio)



##############
#fem el mateix procediment per a la part negativa.

a=hist(log(log_neg),breaks=50)
hneg=hist((log_neg),breaks=exp(a$breaks))
plot(hneg$mids,hneg$density,log="xy")

hill(log_neg)
hill(log_neg, end=50000)

#escollim threshold
thr_1=0.0002
log_neg_retail_1=log_neg[log_neg>=thr_1]
length(log_neg_retail_1)

#construim el model
alpha_1=length(log_neg_retail_1)*1/sum(log(log_neg_retail_1/thr_1))

h1_1=hist(log(log_neg_retail_1))
h2_1=hist(log_neg_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_neg_retail_1)/length(log_neg)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_neg_retail_1)/thr_1)^(-(alpha_1)-1)

#mesures de bondad d'elecció del threshold
cvplot(log_neg, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_neg_retail_1))

#plot del model
plot(hneg$mids,hneg$density,log="xy")
lines(sort(log_neg_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(77777)
u=runif(7500,0,1)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_neg_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
log_test=unique(log_neg_retail_1)
ks.test(log_test, mostra)



######### FEM EL MATEIX PER L'INTERVAL DE TEMPS ENTRE OBSERVACIONS 1min

EURJPY=read.csv('C:/Users/Gerard/Documents/TFG/EURJPY_1min.csv')
P_open=EURJPY$Open
open=ts(P_open)
open=as.numeric(open)
sum(is.na(open))

#calculem el logaritme i els retorns.
log_open=log(open)
log_dif_open=diff(log_open)
hist(log_dif_open)
log_pos=log_dif_open[log_dif_open>0]
log_neg=abs(log_dif_open[log_dif_open<0])

hist(log_dif_open,breaks=100, prob=TRUE)

#####

a=hist(log(log_pos),breaks=50)
hpos=hist(log_pos,breaks=exp(a$breaks))
plot(hpos$mids,hpos$density,log="xy")

hill(log_pos, end=15000)

hill(log_pos, end=5000)


#escollim threshold
thr_1=0.0004
log_pos_retail_1=log_pos[log_pos>=thr_1]
length(log_pos_retail_1)
#construim el model
alpha_1=length(log_pos_retail_1)*1/sum(log(log_pos_retail_1/thr_1))

h1_1=hist(log(log_pos_retail_1))
h2_1=hist(log_pos_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_pos_retail_1)/length(log_pos)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_pos_retail_1)/thr_1)^(-(alpha_1)-1)


#mesures de bondat de l'elecció del threshold
cvplot(log_pos, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_pos_retail_1))

#plot de l'ajust
plot(hpos$mids,hpos$density,log="xy")
lines(sort(log_pos_retail_1),model_pw_1 , col='red')


#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(777777)
u=runif(5000,0,1)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_pos_retail_1))
lines(ecdf(simulacio), col='red')

#realiztem el test
log_test=unique(log_pos_retail_1)
ks.test(log_test, simulacio)


#fem el mateix procediment per a la part negativa.

a=hist(log(log_neg),breaks=50)
hneg=hist((log_neg),breaks=exp(a$breaks))
plot(hneg$mids,hneg$density,log="xy")

hill(log_neg, end=15000)
hill(log_neg, end=5000)


#escollim threshold
thr_1=0.0004
log_neg_retail_1=log_neg[log_neg>=thr_1]
length(log_neg_retail_1)

#construim el model
alpha_1=length(log_neg_retail_1)*1/sum(log(log_neg_retail_1/thr_1))

h1_1=hist(log(log_neg_retail_1))
h2_1=hist(log_neg_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_neg_retail_1)/length(log_neg)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_neg_retail_1)/thr_1)^(-(alpha_1)-1)

#mesures de bondad d'elecció del threshold
cvplot(log_neg, thr=thr_1)


#info de Fisher
error_1=alpha_1/sqrt(length(log_neg_retail_1))

#plot del model
plot(hneg$mids,hneg$density,log="xy")
lines(sort(log_neg_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(777777)
u=runif(5000,0,1)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_pos_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
log_test=unique(log_pos_retail_1)
ks.test(log_test, simulacio)




######### FEM EL MATEIX PER L'INTERVAL DE TEMPS ENTRE OBSERVACIONS 5min

EURJPY=read.csv('C:/Users/Gerard/Documents/TFG/EURJPY_5min.csv')
P_open=EURJPY$Open
open=ts(P_open)
open=as.numeric(open)
sum(is.na(open))

#calculem el logaritme i els retorns.
log_open=log(open)
log_dif_open=diff(log_open)
hist(log_dif_open)
log_pos=log_dif_open[log_dif_open>0]
log_neg=abs(log_dif_open[log_dif_open<0])

hist(log_dif_open,breaks=100, prob=TRUE)

#####

a=hist(log(log_pos),breaks=50)
hpos=hist(log_pos,breaks=exp(a$breaks))
plot(hpos$mids,hpos$density,log="xy")

hill(log_pos, end=65000)
hill(log_pos, end=2000)


#escollim threshold
thr_1=0.00067
log_pos_retail_1=log_pos[log_pos>=thr_1]
length(log_pos_retail_1)

#construim el model
alpha_1=length(log_pos_retail_1)*1/sum(log(log_pos_retail_1/thr_1))

h1_1=hist(log(log_pos_retail_1))
h2_1=hist(log_pos_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_pos_retail_1)/length(log_pos)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_pos_retail_1)/thr_1)^(-(alpha_1)-1)


#mesures de bondat de l'elecció del threshold

cvplot(log_pos, thr=thr_1)


#info de Fisher
error_1=alpha_1/sqrt(length(log_pos_retail_1))

#plot de l'ajust
plot(hpos$mids,hpos$density,log="xy")
lines(sort(log_pos_retail_1),model_pw_1 , col='red')


#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(777777)
u=runif(1000,0,1)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_pos_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
log_test=unique(log_pos_retail_1)
ks.test(log_test, simulacio)



#fem el mateix procediment per a la part negativa.

a=hist(log(log_neg),breaks=50)
hneg=hist((log_neg),breaks=exp(a$breaks))
plot(hneg$mids,hneg$density,log="xy")

length(log_neg[log_neg>1])
hill(log_neg, end=65000)

hill(log_neg, end=1000)

#escollim threshold
thr_1=0.00065
log_neg_retail_1=log_neg[log_neg>=thr_1]
length(log_neg_retail_1)

#construim el model
alpha_1=length(log_neg_retail_1)*1/sum(log(log_neg_retail_1/thr_1))

h1_1=hist(log(log_neg_retail_1))
h2_1=hist(log_neg_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_neg_retail_1)/length(log_neg)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_neg_retail_1)/thr_1)^(-(alpha_1)-1)

#mesures de bondad d'elecció del threshold
cvplot(log_neg, thr=thr_1)


#info de Fisher
error_1=alpha_1/sqrt(length(log_neg_retail_1))

#plot del model
plot(hneg$mids,hneg$density,log="xy")
lines(sort(log_neg_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(7777777)
u=runif(1500)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_neg_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
log_test=unique(log_neg_retail_1)
ks.test(log_test, simulacio)



######### FEM EL MATEIX PER L'INTERVAL DE TEMPS ENTRE OBSERVACIONS 10min

EURJPY=read.csv('C:/Users/Gerard/Documents/TFG/EURJPY_10min.csv')
P_open=EURJPY$Open
open=ts(P_open)
open=as.numeric(open)
sum(is.na(open))

#calculem el logaritme i els retorns.
log_open=log(open)
log_dif_open=diff(log_open)
hist(log_dif_open)
log_pos=log_dif_open[log_dif_open>0]
log_neg=abs(log_dif_open[log_dif_open<0])

hist(log_dif_open,breaks=100, prob=TRUE)

a=hist(log(log_pos),breaks=50)
hpos=hist(log_pos,breaks=exp(a$breaks))
plot(hpos$mids,hpos$density,log="xy")

hill(log_pos, end=6500)
hill(log_pos, end=1000)


#escollim threshold
thr_1=0.0009
log_pos_retail_1=log_pos[log_pos>=thr_1]
length(log_pos_retail_1)

#construim el model
alpha_1=length(log_pos_retail_1)*1/sum(log(log_pos_retail_1/thr_1))

h1_1=hist(log(log_pos_retail_1))
h2_1=hist(log_pos_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_pos_retail_1)/length(log_pos)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_pos_retail_1)/thr_1)^(-(alpha_1)-1)


#mesures de bondat de l'elecció del threshold
cvplot(log_pos, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_pos_retail_1))

#plot de l'ajust
plot(hpos$mids,hpos$density,log="xy")
lines(sort(log_pos_retail_1),model_pw_1 , col='red')


#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(777777)
u=runif(1000)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_pos_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
log_test=unique(log_pos_retail_1)
ks.test(log_test, simulacio)



##############
#fem el mateix procediment per a la part negativa.

a=hist(log(log_neg),breaks=50)
hneg=hist((log_neg),breaks=exp(a$breaks))
plot(hneg$mids,hneg$density,log="xy")

length(log_neg[log_neg>1])
hill(log_neg, end=6500)
hill(log_neg, end=1000)


#escollim threshold
thr_1=0.001
log_neg_retail_1=log_neg[log_neg>=thr_1]
length(log_neg_retail_1)

#construim el model
alpha_1=length(log_neg_retail_1)*1/sum(log(log_neg_retail_1/thr_1))

h1_1=hist(log(log_neg_retail_1))
h2_1=hist(log_neg_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_neg_retail_1)/length(log_neg)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_neg_retail_1)/thr_1)^(-(alpha_1)-1)

#mesures de bondad d'elecció del threshold
cvplot(log_neg, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_neg_retail_1))

#plot del model
plot(hneg$mids,hneg$density,log="xy")
lines(sort(log_neg_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(7777777)
u=runif(1500)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_neg_retail_1))
lines(ecdf(simulacio), col='red')

#realitzem el test
ks.test(log_neg_retail_1, simulacio)




######### FEM EL MATEIX PER L'INTERVAL DE TEMPS ENTRE OBSERVACIONS 30min

EURJPY=read.csv('C:/Users/Gerard/Documents/TFG/EURJPY_30min.csv')
P_open=EURJPY$Open
open=ts(P_open)
open=as.numeric(open)
sum(is.na(open))

#calculem el logaritme i els retorns.
log_open=log(open)
log_dif_open=diff(log_open)
hist(log_dif_open)
log_pos=log_dif_open[log_dif_open>0]
log_neg=abs(log_dif_open[log_dif_open<0])

hist(log_dif_open,breaks=100, prob=TRUE)

#####

a=hist(log(log_pos),breaks=50)
hpos=hist(log_pos,breaks=exp(a$breaks))
plot(hpos$mids,hpos$density,log="xy")

hill(log_pos, end=6500)
hill(log_pos, end=1000)


#escollim threshold
thr_1=0.0012
log_pos_retail_1=log_pos[log_pos>=thr_1]
length(log_pos_retail_1)

#construim el model
alpha_1=length(log_pos_retail_1)*1/sum(log(log_pos_retail_1/thr_1))

h1_1=hist(log(log_pos_retail_1))
h2_1=hist(log_pos_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_pos_retail_1)/length(log_pos)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_pos_retail_1)/thr_1)^(-(alpha_1)-1)


#mesures de bondat de l'elecció del threshold
cvplot(log_pos, thr=thr_1)


#info de Fisher
error_1=alpha_1/sqrt(length(log_pos_retail_1))

#plot de l'ajust
plot(hpos$mids,hpos$density,log="xy")
lines(sort(log_pos_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1

set.seed(777777)
u=runif(1500)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1
#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust

plot(ecdf(log_pos_retail_1))
lines(ecdf(simulacio), col='red')
#check visual de la diferència entre funcions de distribució acumulada

log_test=unique(log_pos_retail_1)
ks.test(log_pos_retail_1, simulacio)
#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits



##############
#fem el mateix procediment per a la part negativa.

a=hist(log(log_neg),breaks=50)
hneg=hist((log_neg),breaks=exp(a$breaks))
plot(hneg$mids,hneg$density,log="xy")

hill(log_neg, end=65000)
hill(log_neg, end=10000)


#escollim threshold
thr_1=0.00125

log_neg_retail_1=log_neg[log_neg>=thr_1]
length(log_neg_retail_1)

#construim el model
alpha_1=length(log_neg_retail_1)*1/sum(log(log_neg_retail_1/thr_1))

h1_1=hist(log(log_neg_retail_1))
h2_1=hist(log_neg_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_neg_retail_1)/length(log_neg)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_neg_retail_1)/thr_1)^(-(alpha_1)-1)

#mesures de bondad d'elecció del threshold
cvplot(log_neg, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_neg_retail_1))

#plot del model
plot(hneg$mids,hneg$density,log="xy")
lines(sort(log_neg_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(7777777)
u=runif(1500)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_neg_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
ks.test(log_neg_retail_1, simulacio)




######### FEM EL MATEIX PER L'INTERVAL DE TEMPS ENTRE OBSERVACIONS 1hora

EURJPY=read.csv('C:/Users/Gerard/Documents/TFG/EURJPY_1h.csv')
P_open=EURJPY$Open
open=ts(P_open)
open=as.numeric(open)
sum(is.na(open))

#calculem el logaritme i els retorns.
log_open=log(open)
log_dif_open=diff(log_open)
hist(log_dif_open)
log_pos=log_dif_open[log_dif_open>0]
log_neg=abs(log_dif_open[log_dif_open<0])

hist(log_dif_open,breaks=100, prob=TRUE)

#####
a=hist(log(log_pos),breaks=50)
hpos=hist(log_pos,breaks=exp(a$breaks))
plot(hpos$mids,hpos$density,log="xy")

length(log_pos[log_pos>1])
hill(log_pos, end=65000)
hill(log_pos, end=10000)


#escollim threshold
thr_1=0.00225
log_pos_retail_1=log_pos[log_pos>=thr_1]
length(log_pos_retail_1)

#construim el model
alpha_1=length(log_pos_retail_1)*1/sum(log(log_pos_retail_1/thr_1))

h1_1=hist(log(log_pos_retail_1))
h2_1=hist(log_pos_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_pos_retail_1)/length(log_pos)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_pos_retail_1)/thr_1)^(-(alpha_1)-1)


#mesures de bondat de l'elecció del threshold
cvplot(log_pos, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_pos_retail_1))

#plot de l'ajust
plot(hpos$mids,hpos$density,log="xy")
lines(sort(log_pos_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1

#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust
set.seed(777777)
u=runif(1500)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1

#check visual de la diferència entre funcions de distribució acumulada
plot(ecdf(log_pos_retail_1))
lines(ecdf(simulacio), col='red')

#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits
ks.test(log_pos_retail_1, simulacio)



##############
#fem el mateix procediment per a la part negativa.

a=hist(log(log_neg),breaks=50)
hneg=hist((log_neg),breaks=exp(a$breaks))
plot(hneg$mids,hneg$density,log="xy")

length(log_neg[log_neg>1])
hill(log_neg, end=65000)
hill(log_neg, end=10000)


#escollim threshold
thr_1=0.00175
log_neg_retail_1=log_neg[log_neg>=thr_1]
length(log_neg_retail_1)

#construim el model
alpha_1=length(log_neg_retail_1)*1/sum(log(log_neg_retail_1/thr_1))

h1_1=hist(log(log_neg_retail_1))
h2_1=hist(log_neg_retail_1,breaks=exp(h1_1$breaks))

constant_hist_1= length(log_neg_retail_1)/length(log_neg)
model_pw_1 = constant_hist_1*(alpha_1)/thr_1*(sort(log_neg_retail_1)/thr_1)^(-(alpha_1)-1)

#mesures de bondad d'elecció del threshold
cvplot(log_neg, thr=thr_1)

#info de Fisher
error_1=alpha_1/sqrt(length(log_neg_retail_1))

#plot del model
plot(hneg$mids,hneg$density,log="xy")
lines(sort(log_neg_retail_1),model_pw_1 , col='red')

#dades rellevants de l'ajust
alpha_1
error_1
thr_1


set.seed(777777)
u=runif(1500)
simulacio=(1 - u)^(-1 /alpha_1)*thr_1
#simulació manual de valors amb distribució powerlaw amb paràmetres de l'ajust

plot(ecdf(log_neg_retail_1))
lines(ecdf(simulacio), col='red')
#check visual de la diferència entre funcions de distribució acumulada

ks.test(log_neg_retail_1, simulacio)
#per a no tenir imprecisions en el càlcul del p-valor eliminem els 
#elements repetits



#######################################
###  AJUST DE DIFERENTS DISTRIBUCIONS     
#######################################


library(MASS)
lognorm=fitdistr(log_pos , 'lognormal' )
exp=fitdistr(log_pos , 'exponential' )
lognorm

a=hist(log(log_pos),breaks=50)
b=hist(log_pos,breaks=exp(a$breaks))
plot(b$mids,b$density,log="xy")
model_exp = exp$estimate*exp(-exp$estimate*b$mids)
model_logn = 1/(sqrt(2*pi)*lognorm$estimate[2]*b$mids)*exp(-(log(b$mids)-lognorm$estimate[1])^2/(2*lognorm$estimate[2]^2)) 

LogRetorns_EURJPY=b$mids
Densitat=b$density
plot(LogRetorns_EURJPY,Densitat,log="xy")
lines(sort(log_pos_retail_1),model_pw_1, col = 'green')
lines(b$mids , model_logn, col='blue')
lines(b$mids , model_exp, col='red')
legend("bottomleft", legend = c("Llei de Potencies" , 'Exponencial', 'Log-normal'), 
       col = c('green' ,'red', "blue"), lty = c(1,1,1), lwd = 2)

summary(EURJPY)

################################
###   PDF mostral vs Gaussiana
################################


cvx=read.csv('C:/Users/Gerard/Downloads/CVX.csv')
P_cierre=cvx$Adj.Close
cierre=ts(P_cierre)
#Carreguem dades


cierre=as.numeric(cierre)
sum(is.na(cierre))

log_cierre=log(cierre)
log_dif_cierre=diff(log_cierre)
temps=seq(-length(cierre)/2,length(cierre)/2,1)
temps=temps[-c(length(temps),length(temps)-1)]
length(log_dif_cierre)
plot(temps,log_dif_cierre)
#plot de les dades
#amb aquest cas es veu com la inmensa majoria dels valors oscil·len
#molt poc sobre la mitjana, i tenim varis punts aillats molt extrems.


###########Comparació PDF mostral vs Gaussiana
sigma=sd(log_dif_cierre)
sigma
mu=mean(log_dif_cierre)
eix_x=seq(-0.175,0.175,0.001)
hist(log_dif_cierre, plot=T,breaks= eix_x,freq = FALSE, probability = TRUE)
density_function=curve(dnorm(x,mu,sigma)
                       ,-0.175,0.175,length(log_dif_cierre), add=TRUE, col='red')



