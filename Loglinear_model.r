# Sinistres asseguradora: Tractament loglineal
head(bm)
attach(bm)

ls()
library(MASS)

bm.p0<-glm(formula = y ~ 1, family = poisson(link = log),offset=bm$logn)
bm.p1<-glm(formula = y ~ edat, family = poisson(link = log),offset=bm$logn)
bm.p2<-glm(formula = y ~ pot, family = poisson(link = log),offset=bm$logn)
bm.p3<-glm(formula = y ~ edat+pot, family = poisson(link = log),offset=bm$logn)
bm.p4<-glm(formula = y ~ edat*pot, family = poisson(link = log),offset=bm$logn)

anova(bm.p0,bm.p1,test="Chi")
anova(bm.p0,bm.p2,test="Chi")
anova(bm.p2,bm.p3,test="Chi")
anova(bm.p3,bm.p4,test="Chi")			# Són significatives

bm.p1<-glm(formula = y ~ offset(bm$logn) + edat + pot, family = poisson(link = log))
summary(bm.p1)
escala1 <- sum(residuals(bm.p1,type="pearson")^2)/bm.p1$df.res


bm.q1<-glm(formula = y ~ offset(bm$logn) + edat + pot, family = quasi(link = log,      variance = "mu"))
summary(bm.q1)
summary(bm.p1,dispersion= 3.103064)		# multiplica els EE per l'arrel de la dispersió

bm.nb1<-glm.nb(formula = y ~ offset(bm$logn) + edat + pot)
summary(bm.nb1)
### Ojo, no fiar-se de l'EE de l'escala 4559

### No el deixa calcular
bm.nb2<-glm.nb(formula = y ~ offset(bm$logn) + edat * pot)
summary(bm.nb2)

theta.md(y, fitted(bm.nb1), dfr = df.residual(bm.nb1))
theta.ml(y, fitted(bm.nb1), lim = 20,trace=T)				# No recomanable
theta.mm(y, fitted(bm.nb1), dfr = df.residual(bm.nb1))

?theta.md
gamma.shape(bm.nb1, it.lim = 20,verbose = TRUE)
?gamma.shape
gd<-gamma.dispersion(bm.nb1, it.lim = 20);1/gd

bm.gnb1<-glm(formula = y ~ offset(bm$logn) + edat + pot, family=neg.bin(4559.013), data = bm )
summary(bm.gnb1)
summary(bm.gnb1,dispersion=1)

# Gamma
bm.ga1<-glm(formula = y ~ offset(bm$logn) + edat + pot, family = Gamma(link = log))
summary(bm.ga1)
gs<-gamma.shape(bm.ga1, it.lim = 20,verbose = TRUE);gs;1/gs$alpha
gd<-gamma.dispersion(bm.ga1);gd
alfa<-1/0.001355877;alfa # Paràmetre de forma de la gamma
# No serà massa diferent a una normal
# Lognormal: dispersió petita bona aproximació normal

bm.lg1<-lm( log( y ) ~ offset(bm$logn) + edat + pot)
bm.lg1<-glm( log( y ) ~ offset(bm$logn) + edat + pot, family = gaussian)
summary(bm.lg1)
0.001361379
0.0369^2

bm.ga2<-glm(formula = y ~ offset(bm$logn) + edat * pot, family = Gamma(link = log))
step(bm.ga1,test="Chis")
drop1(bm.ga1,test="Chis")

# Tractament resposta binària

bm$f.pot<-factor(bm$pot,labels=c("<54","54-75","76-118","119+"))
bm$f.edat<-factor(bm$edat,labels=c("<36","36-49","50+"))


m0<-glm( I(y/n) ~ 1, family = binomial, data = bm, weights = n)
m0<-glm( I(y/n) ~ 1, family = binomial, data = bm)
m0<-glm( cbind(y,n-y) ~ 1, family = binomial, data = bm)
summary(m0)

m1<-glm(I(y/n) ~ f.pot, family = binomial, data = bm, weights = n)
m1<-glm(cbind(y,n-y)  ~ f.pot, family = binomial, data = bm)
summary(m1)
m2<-glm(cbind(y,n-y)  ~ f.edat, family = binomial, data = bm)
m3<-glm(cbind(y,n-y)  ~ f.pot+f.edat, family = binomial, data = bm)
m4<-glm(cbind(y,n-y)  ~ f.pot*f.edat, family = binomial, data = bm)
summary(m4)
AIC(m3)
AIC(m4)
ls
anova(m0,m1,test="Chisq")
anova(m0,m2,test="Chisq")
anova(m2,m3,test="Chisq")
anova(m1,m3,test="Chisq")
anova(m3,m4,test="Chisq")

bm$prm4<-predict(m4,type="response")
bm$prm3<-predict(m3,type="response")


# Dades de Seguiment d'educació universitària 
summary(wisconsin)
attach(wisconsin)
sum(Y_ijk)
wisconsin
# StatusS - A    Plans_Uni - B  Motivació - C
#
#MODELO	DEVIANZA	G.L.	
#A+B+C	2714	10	Motivación, Universidad y Estatus social independientes
#A+B*C	1092	9	Estatus social es independiente de la Motivación y Universidad
#B+A*C	1877.4	7	Asistencia a Universidad es independiente de Motivación y Estatus
#C+A*B	1920.4	7	Motivación de los padres es independiente de Estatus y Universidad
#A*B+A*C	1083.8	4	Condicionado al Estatus, Motivación y Universidad son independientes
#A*B+B*C	298.5	6	Condicionado a Universidad, Estatus y Motivación son independientes
#A*C+B*C	255.5	6	Condicionado a Motivación, Estatus y Universidad son independientes
#A*B+A*C+B*C	1.575	3	Interpretación ausencia de interacción de orden 3 ¿??
#
# Independència Total
wis1 <-glm(Y_ijk~Estimul+StatusS+Plans_Uni, family=poisson(link=log))
wis2 <-glm(Y_ijk~Estimul+StatusS+Plans_Uni+Estimul*StatusS+Estimul*Plans_Uni+StatusS*Plans_Uni, family=poisson(link=log))
summary(wis1)
anova(wis1)
1-pchisq(deviance(wis1),df.residual(wis1))
# Independencia per blocs
wis21 <-glm(Y_ijk~Plans_Uni*Estimul+StatusS, family=poisson(link=log))
wis22 <-glm(Y_ijk~Plans_Uni+Estimul*StatusS, family=poisson(link=log))
wis23 <-glm(Y_ijk~Estimul+Plans_Uni*StatusS, family=poisson(link=log))
summary(wis21)
anova(wis21)
1-pchisq(deviance(wis21),df.residual(wis21))
1-pchisq(deviance(wis22),df.residual(wis22))
1-pchisq(deviance(wis23),df.residual(wis23))
# Independencia parcial
wis31 <-glm(Y_ijk~(Plans_Uni+Estimul)*StatusS, family=poisson(link=log))
wis32 <-glm(Y_ijk~Plans_Uni*(Estimul+StatusS), family=poisson(link=log))
wis33 <-glm(Y_ijk~Estimul*(Plans_Uni+StatusS), family=poisson(link=log))
summary(wis31)
anova(wis31)
1-pchisq(deviance(wis31),df.residual(wis31))
1-pchisq(deviance(wis32),df.residual(wis32))
1-pchisq(deviance(wis33),df.residual(wis33))
# Associació Uniforme
wis41 <-glm(Y_ijk~Estimul*Plans_Uni+StatusS*Plans_Uni+Estimul*StatusS, family=poisson(link=log))
summary(wis41)
1-pchisq(deviance(wis41),df.residual(wis41))
wis51 <-glm(Y_ijk~Estimul*Plans_Uni*StatusS, family=poisson(link=log))
summary(wis51)
anova(wis51,test="Cp")
exp(predict(wis41))

# Ara vull calcular el model de resposta binària: A+C
# Per tant equivalent al log-lineal: A*C+B*A+B*C
wisconsin
wisb<-data.frame(wisconsin[1:8,],wisconsin[9:16,4])
names(wisb)[4:5] <- c("VanUni","NoVanUni")
wisb$m<-wisb$VanUni+wisb$NoVanUni
summary(wisb)
wisb

wis41b<-glm( cbind(VanUni,NoVanUni)~StatusS+Estimul,family=binomial,data=wisb)
summary(wis41b)
# Comparo les prediccions entre la proposta binària i la log-lineal corresponent: 
round(predict(wis41b,type="response")*wisb$m,dig=0)
help(predict.glm)
round(exp(predict(wis41)[1:8]))

