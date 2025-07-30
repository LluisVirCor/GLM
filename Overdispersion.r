# NOTA: LA sobredispersió no és exagerada, però provem altres models per jugar

summary(ceb)

# Nens nascuts vius
ceb$logn <-log(ceb$n)
attach(ceb)
summary(ceb)
# Poisson
ceb.ordre2 <-glm(y~offset(ceb$logn)+dur+res+educ+dur*res+dur*educ+res*educ, family=poisson(link=log))
escala2 <- (sum(residuals(ceb.ordre2,type="pearson")^2))/(ceb.ordre2$df.residual);escala2
ceb.ordre1 <-glm(y~offset(ceb$logn)+dur+res+educ, family=poisson(link=log))
escala1 <- (sum(residuals(ceb.ordre1,type="pearson")^2))/(ceb.ordre1$df.residual);escala1
summary(ceb.ordre1)
summary(ceb.ordre1,dispersion=escala1)  # Assumeix sobredispersio
summary(ceb.ordre2)
summary(ceb.ordre2,dispersion=escala2)
logLik(ceb.ordre1)
plot(log(fitted(ceb.ordre1)),log((y-fitted(ceb.ordre1))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2 ))
abline(0,1)

### Models exercici 1
ceb0 <-glm(y~offset(ceb$logn), family=poisson(link=log))
ceb1 <-glm(y~offset(ceb$logn)+dur, family=poisson(link=log))
ceb2 <-glm(y~offset(ceb$logn)+res, family=poisson(link=log))
ceb3 <-glm(y~offset(ceb$logn)+educ, family=poisson(link=log))
ceb4 <-glm(y~offset(ceb$logn)+dur+res, family=poisson(link=log))
ceb5 <-glm(y~offset(ceb$logn)+dur+educ, family=poisson(link=log))
ceb6 <-glm(y~offset(ceb$logn)+dur+educ+res, family=poisson(link=log))		# Model bo
ceb7 <-glm(y~offset(ceb$logn)+dur*educ+res, family=poisson(link=log))
ceb8 <-glm(y~offset(ceb$logn)+dur+educ*res, family=poisson(link=log))
ceb9 <-glm(y~offset(ceb$logn)+dur*res+educ, family=poisson(link=log))

### Altres models (falten per completar la taula
ceb10 <-glm(y~offset(ceb$logn)+dur*res*educ, family=poisson(link=log))

# Comparança de models (fet per mi)

anova(ceb0,ceb1,test="Chisq")
anova(ceb1,ceb4,test="Chisq")
anova(ceb1,ceb5,test="Chisq")
anova(ceb5,ceb6,test="Chisq")
anova(ceb6,ceb7,test="Chisq")
anova(ceb6,ceb8,test="Chisq")
anova(ceb6,ceb9,test="Chisq")

### No son significatives les interaccions

summary(ceb6)


summary(ceb.ordre1)
summary(ceb.ordre1,dispersion=escala1)	# multiplica els EE per l'arrel quadrada de l'escala



# Comparança de models (fet per la profe)

anova(ceb.ordre1,test="Chi") # Correcte
anova(ceb.ordre1,dispersion=escala1,test="F") # Incorrecte
anova(ceb.ordre1,ceb.ordre2,dispersion=escala1,test="Chisq") # Incorrecte
anova(ceb.ordre1,ceb.ordre2,dispersion=escala1,test="F") # Correcte assumeix sobredispersio
anova(ceb.ordre1,ceb.ordre2,dispersion=escala2,test="F") # Correcte assumeix sobredispersio
 (39.809/31)/escala2
anova(ceb.ordre1,ceb.ordre2,test="F") # Incorrecte


# NOTA: ES millor usar l'escala2 (model complert en els contrastos)



# Quasi Versemblança
cebquasi.ordre2 <-glm(y~offset(ceb$logn)+dur+res+educ+dur*res+dur*educ+res*educ, family=quasi(link=log,variance="mu"))
cebquasi.ordre1 <-glm(y~offset(ceb$logn)+dur+res+educ, family=quasi(link=log,variance="mu"))
summary(cebquasi.ordre1)
summary(cebquasi.ordre2)

escala1 <- (sum(residuals(cebquasi.ordre1,type="pearson")^2))/(cebquasi.ordre1$df.residual);escala1
escala2 <- (sum(residuals(cebquasi.ordre2,type="pearson")^2))/(cebquasi.ordre2$df.residual);escala2


#### Els contrast de deviances amb quasi MV s'utilitza el F.
anova(cebquasi.ordre1,test="F")
anova(cebquasi.ordre1,cebquasi.ordre2,dispersion=escala1,test="F") # Correcte
anova(cebquasi.ordre1,cebquasi.ordre2,dispersion=escala2,test="F") # Correcte
anova(cebquasi.ordre1,cebquasi.ordre2,test="F") # Correcte
anova(cebquasi.ordre1,cebquasi.ordre2,dispersion=escala2,test="Chisq") # Incorrecte  (Hi ha sobredispersió)

# Càlcul dels models loglineals dispersió=1

m1<-glm(y~offset(ceb$logn)+dur+res+educ, family=poisson(link=log))
m2<-glm(y~offset(ceb$logn)+dur*res+educ, family=poisson(link=log))
m3<-glm(y~offset(ceb$logn)+dur+res*educ, family=poisson(link=log))
m4<-glm(y~offset(ceb$logn)+dur*educ+res, family=poisson(link=log))

escala <- (sum(residuals(m1,type="pearson")^2))/(m1$df.residual)
escala;sqrt(escala)

summary(m1,dispersion=escala)
summary(m1)

anova(m1,m2,test="Chis")

anova(m1,m2, dispersion=escala, test="F")
anova(m1,m3, dispersion=escala, test="F")
anova(m1,m4, dispersion=escala, test="F")

anova( m0, m01, dispersion=escala, test="F")
anova( m01, m12, dispersion=escala, test="F")
anova( m12, m1, dispersion=escala, test="F")

# Binomial negativa

library(MASS)
ceb.nb1 <- glm.nb(y~offset(logn)+dur+res+educ,data = ceb)			# Usar el paràmetre que dóna de la binomial negativa per aplicar després el glm
summary(ceb.nb1)
ceb.nb2 <- glm.nb(y~offset(logn)+dur+res+educ+dur*res+dur*educ+res*educ,data = ceb)
summary(ceb.nb2)
anova(ceb.nb1,ceb.nb2,test="Chi")
anova(ceb.nb1,ceb.nb2,test="F")  # Incorrecte

### Paràmetre de la binomial
theta.md(y, fitted(ceb.nb1), dfr = df.residual(ceb.nb1))
theta.ml(y, fitted(ceb.nb1), lim = 40,trace=F)
theta.mm(y, fitted(ceb.nb1), dfr = df.residual(ceb.nb1))

ceb.nb1 <- glm.nb(y~offset(logn)+dur+res+educ, init.theta =441.1706 ,data = ceb)
summary(ceb.nb1)

ceb.gnb1 <- glm(y~offset(logn)+dur+res+educ,family=neg.bin(809403),data = ceb) # Bo
ceb.gnb1 <- glm(y~offset(logn)+dur+res+educ,family=neg.bin(1),data = ceb)	 # Dolent
summary(ceb.gnb1,dispersion=1)
summary(ceb.gnb1)
summary(ceb.nb1)
summary(cebquasi.ordre1)
summary(ceb.ordre1)


# Gamma
detach(bm)
ceb.ga1<-glm(formula = y ~ offset(logn) +dur+res+educ, family = Gamma(link = log),data=ceb)
summary(ceb.ga1)
gs<-gamma.shape(ceb.ga1, it.lim = 20,verbose = TRUE);gs;1/gs$alpha
gd<-gamma.dispersion(ceb.ga1);gd
alfa<-1/0.03200922;alfa # Paràmetre de forma de la gamma
# No serà massa semblant a una normal
# Lognormal: dispersió petita bona aproximació normal
ceb.lg1<-lm( log( y ) ~ offset(logn) +dur+res+educ,data=ceb)
ceb.lg1<-glm( log( y ) ~ offset(logn) +dur+res+educ,data=ceb, family = gaussian)
summary(ceb.lg1);AIC(ceb.lg1)
0.03200922
0.1957^2
plot(exp(fitted(ceb.lg1)),(((ceb$y)-exp(fitted(ceb.lg1)))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2 ))
abline(0,1)

