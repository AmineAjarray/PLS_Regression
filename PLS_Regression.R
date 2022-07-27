library(data.table)
library(Hmisc)
library(pls)
library(tidyverse)
library(car) #Durbin watson test
library(lmtest)# Goldfeld-Quandt test
library(tseries)
library(olsrr)# DFBETAS
library(MASS)#les r?sidus studentis?e

path2data<-file.path("/Users/macbook/Downloads/dataset.csv")
setwd("/Users/macbook/Desktop")
data<-fread(path2data)
data

describe(data)

ggplot(data = data) + 
         geom_point(mapping = aes(x = X3, y = Y))

ggplot(data = data) + 
  geom_smooth(mapping = aes(x = X3, y = Y))

ggplot(data = data, mapping = aes(x = X8, y = Y)) + 
  geom_point() + 
  geom_smooth()


ggplot(data = data) + 
  stat_summary(
    mapping = aes(x = X1, y = Y),
    fun.min = min,
    fun.max = max,
    fun = median
  )

ggplot(data = data, mapping = aes(x = X1 , y = Y)) + 
  geom_boxplot()


ggplot( data = data) + 
  geom_boxplot(mapping = aes(x = X3, y = Y))  +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")




#boxplot 
boxplot(data$X3, col = c("grey"),main = "Boxplot X3", ylab = "Quantiles")


boxplot(data[,c('X1','X2','X3')],
        col = c("grey"),                 #Pour la couleur
        main = paste("Boxplot"),     #Pour le titre
        ylab = "Quantiles")              #Pour le titre de l’axe des ordonnées


## Avec une grande différence d'unités
data$Valeur_importante<-data$Y*100

boxplot(data[,c('X1','X2','X3','Valeur_importante')],
        col = c("grey"),
        main = paste("Boxplot"),
        ylab = "Quantiles")

## HISTOGRAMME
hist(data$X1, 
     col = c("purple"),
     main = paste("Histogramme pour la variable X1"),
     ylab = "Effectifs",
     xlab = "X1")

#####################################################
#####################################################

install.packages("pls")
library(pls)

# Regression multiple Y f(X1..X7)

model2<-lm(formula =Y ~ X2 + X3  + X5 +X6 + X7 + X8 , data=updated_data)
lm<-lm(formula = Y ~ 0 + X2+ X3 + X5 +X6 + X7 + X8, data=updated_data)
#print 
print(lm) 
#summary
print(summary(lm)) 

plot(Y~X1,data=data,xlab="prix",ylab="Ventes")
#
#il semble que le nuage ne s'?tire par sur une droite, donc une regr?ssion lin?aire simple peut ne pas ?tre pertinante

plot(Y~X2,data=data,xlab="prix moyen des concurrets",ylab="Ventes")
#il semble que le nuage  s'?tire par  une droite, on peut tenter une r?gression simple

plot(Y~X3,data=data,xlab="d?pences publicitaires",ylab="Ventes")
#les nombres de ventes augmente avec le budget publicitaire
#il semble qu'une regr?ssion lin?aire simple peut ?tre pertinante

plot(Y~X4,data=data,xlab="d?fference de prix",ylab="Ventes")
#Une r?gression simple semble indiqu?e, les points ?tant dispos?s le long d'une droite.

plot(Y~X5 ,data=data,xlab="d?fference relative de prix",ylab="Ventes")
#Une r?gression simple semble indiqu?e, les points ?tant dispos?s le long d'une droite.

plot(Y~X6 ,data=data,xlab="pub carr?e",ylab="Ventes")

plot(Y~X7 ,data=data,xlab="X7",ylab="Ventes")


plot(Y~X8 ,data=data,xlab="X8",ylab="Ventes")

#boxplot

#simple linear regression
#r?gression des ventes en fonction du prix 
reg1<-lm(Y~X1,data=data)
summary(reg1)
#les coeffs sont significatives, nous rejetons l'hypoth?se H0 pour les deux param?tres estim?s au niveau ?? =5%.
# La valeur du R^2 est faible (R^2 = 0.22 ) et nous retrouvons la remarque eff?ctu?e ? propos du graphe:  une r?gression lin?aire simple
#n'est peut-?tre pas adapt?e ici
#l'estimation de ?? qui vaut ici 0.60

reg2<-lm(Y~X2,data=data)
summary(reg2)
#le coeff li?e ? la constante n'est pas significatif au niveau ?? =5%.
#La valeur du R^2 est R^2=0.53

reg3<-lm(Y~X3,data=data)
summary(reg3)
# les coeff sont signif au niv  ?? =5% et la valeur de R^2 est ?lv?e R^2=0.75 

reg4<-lm(Y~X4,data=data)
summary(reg4)
#Les tests de nullit? des deux coefficients indiquent qu'ils semblent tous deux significativement non nuls
#l'estimation de ?? qui vaut ici 0.326
#La valeur du R2 est ?lev?e (R2 = 0.775) et nous retrouvons la remarque d?j? faite : une r?gression lin?aire simple semble adapt?e.

reg5<-lm(Y~X5,data=data)
summary(reg5)
#le coeff li?e ? la constante n'est pas significatif au niveau ?? =5%
#l'estimation de ?? qui vaut ici 0.324
#La valeur du R2 est ?lev?e (R2 = 0.777)

reg6<-lm(Y~X6,data=data)
summary(reg6)
#le coeff li?e ? la constante n'est pas significatif au niveau ?? =5%
#La valeur du R2 est ?lev?e (R2 = 0.775)


reg7<-lm(Y~X7,data=data)
summary(reg7)


reg8<-lm(Y~X8,data=data)
summary(reg8)

#matrice de corr?lation

#matrice du nuage
plot(data)

DM_Matrix<-as.matrix(data)
DM_Matrix
rcorr(DM_Matrix, type=c("pearson","spearman"))

corr_matrice<-cor(data, method=c("pearson"))
corr_matrice
#r?gression multiple

lm<-lm(formula = Y ~  X1 + X2 + X3   + X5 +X6 + X7 + X8, data=data)

#summary
summary(lm)
#F_value=33.51 et p_value=2.61*10^-10 donc rejette l'hypoyhese nulle(les parametres ne sont pas tous nuls )
#R^2=91% donc ce mod?le permet d'expliquer 91% de la variablilit? des ventes.
#les p-value associ?es ? les variables X1, X2, X5, X7 et X8 sont > 0.005
#donc on acc?pte l'hypothese de nullit? pour chacun des coeff de r?gression

#les coeffs de X3, X5 et X7 sont n?gatifs alors que la corr?lation entre ces variables et la varible Y est positif?
#la corr?lation entre les variables X2, X4, X5, X7 et X8 provoque une insntabilit? des coeffs.

model<-lm(formula =Y ~ X2 + X3  + X5 +X6 + X7 + X8 , data=data)
summary(model)

plot(rstudent(model),pch=".",ylab="Résid studentis par VC")
abline(h=c(-2,2))
lines(lowess(rstudent(model)))


res<-rstudent(model)
plot(res,main="Résidus")
abline(h=0,col="red")

#ind?pendance des r?sidus



#perform Durbin-Watson test
#H0 : les r?sidus sont ind?p.
#HA : les r?sidus sont autocorr?l?es  

dwt(model)
durbinWatsonTest(model)
#d1=1.07 d2=1.83
#on a DW= 1.555 donc d1<DW<d2  alors on peut rien conclure sur l'ind?pance des r?sidus ? partir du test du Durbin Wtason
#mais d'apr?s le graphe des r?sidus on soup?onne une structuration temporelle des r?sidus donc une autocorr?lation des r?sidus





#homoscidasticit?

#The Goldfeld-Quandt test
#H0: Heteroscedasticity is not present.
#H1: Heteroscedasticity is present.
gqtest(model, order.by = ~X2 + X3  + X5 +X6 + X7 + X8, data = data, fraction = 7)

#la statistique du test est 0.61 et p_value=0.69>0.05 donc on accepte l'hypo nulle c'est-?-dire que les r?sudus sont homoscidastiques


#normalit? des r?sidus
plot(model,which=2,sub="",main="")
abline(0,1)
#la normalit? semble bien respect?e, tous les points ?tant sur la premi?re bissectrice. 

#help(plot.lm)
#test de Shapiro-wilk H0: les r?sidus suivent une loi normal
shapiro.test(residuals(model))
#p-value=0.22>0.05 donc les r?sidus sont normal
#teste de jaque bera
jarque.bera.test(residuals(model))


##la robustesse du mod?le:

#les observations influentes

ols_plot_cooksd_bar(model)

influenceIndexPlot(model)

#d'apr?s la distance de cook on remarque qu'on a 3 observations influnates( distance de cook ?lev?) 11, 24 et 25

#effet de liveir-->valeur residu petite
#valeur aberante-->valeur resid grande

stud_resids <- studres(model)

#hat matrice
hatvalues(model, infl = lm.influence(model, do.coef = FALSE))
#ols_plot_diagnostics(mdl)
#le poids de l'observation 8 sur son propre estimation est 0.657 > 2(p+1)/n donc ce pont peut repr?senter un effet de levier

#DFBETAS

ols_plot_dfbetas(model)
##l'observation  11 influe sur les coeff des variables X2
## l'observation  24 influe sur les coeff des variables X3, X5 et X6
##l'observation  25 influe sur les coeff des variables X7, et X8

#Covratio


covratio(model, infl = lm.influence(model, do.coef = FALSE),
         res = weighted.residuals(model))
#l'observation 11 a la valeur la plus petit de covration et qui est inferieur ? 1
#donc cette l'inclusion de cette observation diminue la pr?cision du mod?le


#DFFITS
dffits(model, infl = lm.influence(model, do.coef = FALSE),
       res = weighted.residuals(model) )

#la valeur absolue du dffits des observations 11, 24 et 25 est plus grand ? la valeur critqiue 2* ???(p + 1)/n =0.96 
#ces observation causent des changement sur les valeurs pr?dites




updated_data<-data[-c(24,25,11),]

#retrain the model
model2<-lm(formula =Y ~ X2 + X3  + X5 +X6 + X7 + X8 , data=updated_data)
summary(model2)
#Apr?s avoir enlever ces observations influantes on remrque que la valeur de R^2 passe de 91.05% jusqu'a 94.45%


#la colin?arit? des r?gresseurs
ols_vif_tol(model2)

#la valeur du vif est tr?s garnd pour tous les variables donc une grand multicolin?arit?.


ols_eigen_cindex(model2)
#les valeurs propres sont  faibles ce qui entrainent donc de grandes variances des coefficients.
#Les variables X2, X6 et X8 sont colin?are car ils ont des proportions de variance > 0.5 avec une index de condition ?lev? CL=435.1
#Aussi les variables X3, X5 et X6 sont colin?are car ils ont des proportions de variance > 0.5 avec une index de condition ?lev? CL=1419.9

#Pour rem?dier ? ce probl?me de multicolin?arit? on va utiliser la r?gression PLS, la r?gression Ridge et Lasso et choisir le mod?le ad?quate





#############################PLS#######################@@@@

#lm attributes  
print(attributes(lm))
#### Extraction des coefficients
coef(lm)
#### Extraction des valeurs prédites
fitted(lm)
#### Extraction des résidus
resid(lm)
DM_Matrix<-as.matrix(updated_data)
DM_Matrix

corr_matrice<-cor(updated_data, method=c("pearson"))
corr_matrice
Scale_DM<-scale(updated_data)
Scale_DM
colnames(Scale_DM)<- c("X1_cn","X2_cn","X3_cn","X4_cn","X5_cn","X6_cn","X7_cn","X8_cn","Y_cn")
DT_scale<- cbind(updated_data,Scale_DM)
DT_scale


#################Construction de T1 ###########################
DT_scale<-DT_scale[,':='(T1=(1/sqrt(0.79^2+0.88^2+0.90^2+0.89^2+0.89^2+0.9^2)) *((0.79*X2_cn)+(0.88* X3_cn)+(0.90*X5_cn)+(0.89* X6_cn)+(-0.89* X7_cn)+(-0.90* X8_cn)))]

#################Construction de T2 ###########################

# Regression y sur T1 et Xj j=1..7
#pour chercher les variables contribuant de mani?re sifnificatice
# ? la construction de T2

lm11<-lm(formula = Y_cn ~ 0 + T1 + X1_cn, data=DT_scale)
print(summary(lm11))
lm12<-lm(formula = Y_cn ~ 0 + T1 + X2_cn, data=DT_scale)
print(summary(lm12))
lm13<-lm(formula = Y_cn ~ 0 + T1 + X3_cn, data=DT_scale)
print(summary(lm13))
lm14<-lm(formula = Y_cn ~ 0 + T1 + X4_cn, data=DT_scale)
print(summary(lm14))
lm15<-lm(formula = Y_cn ~ 0 + T1 + X5_cn, data=DT_scale)
print(summary(lm15))
lm16<-lm(formula = Y_cn ~ 0 + T1 + X6_cn, data=DT_scale)
print(summary(lm16))
lm17<-lm(formula = Y_cn ~ 0 + T1 + X7_cn, data=DT_scale)
print(summary(lm17))
lm18<-lm(formula = Y_cn ~ 0 + T1 + X8_cn, data=DT_scale)
print(summary(lm18))


# Seules les variables X2 et X6 sont significatives au risque 0.05

# On calcule les r?sidus X12 X16 des regressions de X2_nc, X6_nc sur T1 
lm_R12<-lm(formula = X2_cn ~ 0 + T1  , data=DT_scale)
#### Extraction des r?sidus
X12<-resid(lm_R12)
lm_R13<-lm(formula = X3_cn ~ 0 + T1  , data=DT_scale)
#### Extraction des r?sidus
X13<-resid(lm_R13)
lm_R14<-lm(formula = X4_cn ~ 0 + T1  , data=DT_scale)
#### Extraction des r?sidus
X14<-resid(lm_R14)

lm_R15<-lm(formula = X5_cn ~ 0 + T1  , data=DT_scale)
#### Extraction des r?sidus
X15<-resid(lm_R15)
lm_R16<-lm(formula = X6_cn ~ 0 + T1  , data=DT_scale)
#### Extraction des r?sidus
X16<-resid(lm_R16)
lm_R17<-lm(formula = X7_cn ~ 0 + T1  , data=DT_scale)
#### Extraction des r?sidus
X17<-resid(lm_R17)
lm_R18<-lm(formula = X8_cn ~ 0 + T1  , data=DT_scale)
#### Extraction des r?sidus
X18<-resid(lm_R18)
DT_scale<- cbind(DT_scale,X12,X13,X14,X15,X16,X17,X18)
X12n<-X12/var(X12)
X13n<-X13/var(X13)
X14n<-X14/var(X14)
X15n<-X1/var(X15)
X16n<-X16/var(X16)
X17n<-X17/var(X17)
X18n<-X18/var(X18)

DT_scale<- cbind(DT_scale,X12n,,X13n,X14n,X15n,X16n,X17n,X18n)


###########PLS#######

pls_mdl <- plsr(Y ~. 
                ,scale = TRUE, 
                data=data,
                subset=train,validation="CV")
summary(pls_mdl)

##### Construction du mod?le 

set.seed(2)
train=sample(1:nrow(updated_data),20)
test=(-train)
data.train<-updated_data[train][,c(2,3,5,6,7,8)]
data.test<-updated_data[test]
pcr.fit=pcr(Y ~ X2 + X3  + X5 +X6 + X7 + X8,data=updated_data ,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")

pcr.fit=pcr(Y ~ X2 + X3  + X5 +X6 + X7 + X8,data=updated_data, subset=train ,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")

pcr.pred=predict(pcr.fit, data.test, ncomp = 3)


#DT_scale.train<-DT_scale[train]
modele.pls <- plsr(Y ~ X2 + X3  + X5 +X6 + X7 + X8,data=updated_data
                   ,subset=train,validation="CV")

validationplot(modele.pls,val.type = "MSEP")
summary(modele.pls)

a=as.matrix((pls.pred-data.test[,1])^2)
sum(a)/nrow(data.train)

# Choix du nombre de composantes
plot(RMSEP(modele.pls))
summary(modele.pls)

# Visualisation des erreurs d'ajustement (train) et de pr?vision (CV)
msepcv.pls<-MSEP(modele.pls,estimate=c("train","CV"))

plot(msepcv.pls,col=1,type="l",legendpos="topright",main="")

#on prend le nombre pour lequel l'erreur de pr?vision est minimum
ncomp.pls<-which.min(msepcv.pls$val["CV",,])-1
print(ncomp.pls)
######## Mod?le final : avec le nombre de composantes retenues 3

reg.pls <- plsr(Y ~ X2 + X3  + X5 +X6 + X7 + X8,data=updated_data
                ,subset=train,ncomp=3,validation="LOO",method = "oscorespls")

summary(reg.pls)


#On regarde les r?sidus 
res.pls<-residuals(reg.pls)
plot(res.pls[,,3],pch=15,cex=.5,ylab="R?sidus",main="")
abline(h=c(-2,0,2),lty=c(2,1,2), col= "darkblue")


predicted=as.data.frame(reg.pls$fitted.values)
observed=as.data.frame(updated_data[train][,1])
#Repr?sentation pour la derni?re composante qui doit ?tre celle qui converge le mieux
predit=predicted[,2]
observed=unlist(observed)
plot(observed,predit)

#Calcul des VIP
library(plsVarSel)
vip=VIP(reg.pls,3)
as.data.frame(vip)
w=which(vip>1)
vip[order(vip, decreasing=TRUE)]
bestvip=vip[w]
bestvip[order(bestvip, decreasing=TRUE)]

###########Ridge vs Lasoo###########################################

y=as.matrix(updated_data[,8])
x=model.matrix(Y ~ X2 + X3  + X5 +X6 + X7 + X8,updated_data)[,2:7]
grid=10^seq(10,-2,length=100)
#alpha=0 Ridge alpha=1 Lasoo
ridge.mod=glmnet(x ,y,alpha = 0, lambda = grid,
                 data=updated_data)
predict(ridge.mod,s=50,type = "coefficients")[2:7,]

set.seed(1)
train=sample(1:nrow(x),20)
test=(-train)


y.test=y[test]
x.test=x[test]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0,nfolds = 5)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx =x[test,] )
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha = 0)
predict(out,type="coefficients",s=bestlam)[2:7,]

#lasso 

lasso.mod=glmnet(x[train,] ,y[train],alpha = 1, 
                 lambda = grid,
                 data=updated_data)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,nfolds = 5)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx =x[test,] )
out=glmnet(x,y,alpha = 1)
predict(out,type="coefficients",s=bestlam)[2:7,]
mean((lasso.pred-y.test)^2)


a0=as.matrix((pcr.pred-data.test[,1]))
a=unlist(a0)^2
sum(a)/nrow(DT_scale.test)


