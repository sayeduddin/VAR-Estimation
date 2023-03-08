library(writexl)
library(VineCopula)
library(fGarch)
library(KScorrect)
library(stats)
library(ADGofTest)
library(tseries)

boxTest1=c()
boxTest2=c()
ksTests=c()
adTests=c()
AIC_list=c()
BIC_list=c()

boxTest1
boxTest2
ksTests
adTests
AIC_list
BIC_list

p_fchi=matrix(get.hist.quote(instrument="^fchi", start="2000-01-01",
                             end="2021-01-01", quote="AdjClose", compression='w'))

#p_fchi = na.omit(p_fchi)
p_fchi=diff(log(p_fchi))
jarqueberaTest(p_fchi)

par(mfrow=c(2,2))
acf(p_fchi, col="green", lwd=2)
pacf(p_fchi, col="green", lwd=2)
acf(p_fchi^2, col="red", lwd=2)
par(mfrow=c(1,1))






model1=garchFit(formula=~arma(1,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res1=residuals(model1, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res1, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res1^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model1@fit$ics[1])
BIC_list=append(BIC_list, model1@fit$ics[2])
u1=pnorm(res1, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u1, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u1, null="punif")$p.value)

model2=garchFit(formula=~arma(1,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res2=residuals(model2, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res2^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model2@fit$ics[1])
BIC_list=append(BIC_list, model2@fit$ics[2])
u2=pnorm(res2, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u2, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u2, null="punif")$p.value)

model3=garchFit(formula=~arma(1,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res3=residuals(model3, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res3, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res3^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model3@fit$ics[1])
BIC_list=append(BIC_list, model3@fit$ics[2])
u3=pnorm(res3, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u3, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u3, null="punif")$p.value)

model4=garchFit(formula=~arma(1,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res4=residuals(model4, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res4, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res4^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model4@fit$ics[1])
BIC_list=append(BIC_list, model4@fit$ics[2])
u4=pnorm(res4, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u4, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u4, null="punif")$p.value)

model5=garchFit(formula=~arma(1,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res5=residuals(model5, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res5, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res5^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model5@fit$ics[1])
BIC_list=append(BIC_list, model5@fit$ics[2])
u5=pnorm(res5, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u5, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u5, null="punif")$p.value)

model6=garchFit(formula=~arma(1,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res6=residuals(model6, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res6, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res6^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model6@fit$ics[1])
BIC_list=append(BIC_list, model6@fit$ics[2])
u6=pnorm(res6, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u6, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u6, null="punif")$p.value)

model7=garchFit(formula=~arma(1,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res7=residuals(model7, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res7, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res7^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model7@fit$ics[1])
BIC_list=append(BIC_list, model7@fit$ics[2])
u7=pnorm(res7, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u7, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u7, null="punif")$p.value)

model8=garchFit(formula=~arma(1,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res8=residuals(model8, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res8, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res8^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model8@fit$ics[1])
BIC_list=append(BIC_list, model8@fit$ics[2])
u8=pnorm(res8, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u8, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u8, null="punif")$p.value)

model9=garchFit(formula=~arma(1,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res9=residuals(model9, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res9, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res9^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model9@fit$ics[1])
BIC_list=append(BIC_list, model9@fit$ics[2])
u9=pnorm(res9, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u9, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u9, null="punif")$p.value)

model10=garchFit(formula=~arma(1,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res10=residuals(model10, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res10, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res10^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model10@fit$ics[1])
BIC_list=append(BIC_list, model10@fit$ics[2])
u10=pnorm(res10, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u10, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u10, null="punif")$p.value)

model11=garchFit(formula=~arma(1,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res11=residuals(model11, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res11, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res11^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model11@fit$ics[1])
BIC_list=append(BIC_list, model11@fit$ics[2])
u11=pnorm(res11, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u11, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u11, null="punif")$p.value)

model12=garchFit(formula=~arma(1,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res12=residuals(model12, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res12, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res12^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model12@fit$ics[1])
BIC_list=append(BIC_list, model12@fit$ics[2])
u12=pnorm(res12, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u12, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u12, null="punif")$p.value)

model13=garchFit(formula=~arma(1,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res13=residuals(model13, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res13, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res13^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model13@fit$ics[1])
BIC_list=append(BIC_list, model13@fit$ics[2])
u13=pnorm(res13, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u13, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u13, null="punif")$p.value)

model14=garchFit(formula=~arma(1,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res14=residuals(model14, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res14, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res14^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model14@fit$ics[1])
BIC_list=append(BIC_list, model14@fit$ics[2])
u14=pnorm(res14, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u14, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u14, null="punif")$p.value)

model15=garchFit(formula=~arma(1,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res15=residuals(model15, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res15, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res15^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model15@fit$ics[1])
BIC_list=append(BIC_list, model15@fit$ics[2])
u15=pnorm(res15, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u15, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u15, null="punif")$p.value)

model16=garchFit(formula=~arma(1,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res16=residuals(model16, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res16, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res16^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model16@fit$ics[1])
BIC_list=append(BIC_list, model16@fit$ics[2])
u16=pnorm(res16, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u16, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u16, null="punif")$p.value)

model17=garchFit(formula=~arma(1,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res17=residuals(model17, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res17, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res17^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model17@fit$ics[1])
BIC_list=append(BIC_list, model17@fit$ics[2])
u17=pnorm(res17, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u17, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u17, null="punif")$p.value)

model18=garchFit(formula=~arma(1,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res18=residuals(model18, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res18, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res18^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model18@fit$ics[1])
BIC_list=append(BIC_list, model18@fit$ics[2])
u18=pnorm(res18, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u18, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u18, null="punif")$p.value)

model19=garchFit(formula=~arma(1,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res19=residuals(model19, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res19, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res19^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model19@fit$ics[1])
BIC_list=append(BIC_list, model19@fit$ics[2])
u19=pnorm(res19, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u19, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u19, null="punif")$p.value)

model20=garchFit(formula=~arma(1,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res20=residuals(model20, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res20, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res20^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model20@fit$ics[1])
BIC_list=append(BIC_list, model20@fit$ics[2])
u20=pnorm(res20, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u20, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u20, null="punif")$p.value)

model21=garchFit(formula=~arma(1,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res21=residuals(model21, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res21, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res21^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model21@fit$ics[1])
BIC_list=append(BIC_list, model21@fit$ics[2])
u21=pnorm(res21, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u21, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u21, null="punif")$p.value)

model22=garchFit(formula=~arma(1,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res22=residuals(model22, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res22, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res22^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model22@fit$ics[1])
BIC_list=append(BIC_list, model22@fit$ics[2])
u22=pnorm(res22, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u22, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u22, null="punif")$p.value)

model23=garchFit(formula=~arma(1,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res23=residuals(model23, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res23, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res23^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model23@fit$ics[1])
BIC_list=append(BIC_list, model23@fit$ics[2])
u23=pnorm(res23, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u23, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u23, null="punif")$p.value)

model24=garchFit(formula=~arma(1,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res24=residuals(model24, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res24, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res24^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model24@fit$ics[1])
BIC_list=append(BIC_list, model24@fit$ics[2])
u24=pnorm(res24, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u24, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u24, null="punif")$p.value)

model25=garchFit(formula=~arma(1,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res25=residuals(model25, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res25, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res25^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model25@fit$ics[1])
BIC_list=append(BIC_list, model25@fit$ics[2])
u25=pnorm(res25, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u25, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u25, null="punif")$p.value)

model26=garchFit(formula=~arma(1,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res26=residuals(model26, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res26, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res26^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model26@fit$ics[1])
BIC_list=append(BIC_list, model26@fit$ics[2])
u26=pnorm(res26, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u26, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u26, null="punif")$p.value)

model27=garchFit(formula=~arma(1,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res27=residuals(model27, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res27, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res27^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model27@fit$ics[1])
BIC_list=append(BIC_list, model27@fit$ics[2])
u27=pnorm(res27, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u27, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u27, null="punif")$p.value)

model28=garchFit(formula=~arma(1,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res28=residuals(model28, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res28, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res28^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model28@fit$ics[1])
BIC_list=append(BIC_list, model28@fit$ics[2])
u28=pnorm(res28, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u28, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u28, null="punif")$p.value)

model29=garchFit(formula=~arma(1,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res29=residuals(model29, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res29, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res29^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model29@fit$ics[1])
BIC_list=append(BIC_list, model29@fit$ics[2])
u29=pnorm(res29, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u29, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u29, null="punif")$p.value)

model30=garchFit(formula=~arma(1,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res30=residuals(model30, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res30, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res30^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model30@fit$ics[1])
BIC_list=append(BIC_list, model30@fit$ics[2])
u30=pnorm(res30, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u30, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u30, null="punif")$p.value)

model31=garchFit(formula=~arma(1,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res31=residuals(model31, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res31, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res31^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model31@fit$ics[1])
BIC_list=append(BIC_list, model31@fit$ics[2])
u31=pnorm(res31, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u31, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u31, null="punif")$p.value)

model32=garchFit(formula=~arma(1,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res32=residuals(model32, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res32, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res32^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model32@fit$ics[1])
BIC_list=append(BIC_list, model32@fit$ics[2])
u32=pnorm(res32, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u32, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u32, null="punif")$p.value)

model33=garchFit(formula=~arma(1,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res33=residuals(model33, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res33, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res33^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model33@fit$ics[1])
BIC_list=append(BIC_list, model33@fit$ics[2])
u33=pnorm(res33, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u33, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u33, null="punif")$p.value)

model34=garchFit(formula=~arma(1,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res34=residuals(model34, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res34, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res34^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model34@fit$ics[1])
BIC_list=append(BIC_list, model34@fit$ics[2])
u34=pnorm(res34, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u34, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u34, null="punif")$p.value)

model35=garchFit(formula=~arma(1,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res35=residuals(model35, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res35, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res35^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model35@fit$ics[1])
BIC_list=append(BIC_list, model35@fit$ics[2])
u35=pnorm(res35, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u35, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u35, null="punif")$p.value)

model36=garchFit(formula=~arma(1,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res36=residuals(model36, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res36, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res36^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model36@fit$ics[1])
BIC_list=append(BIC_list, model36@fit$ics[2])
u36=pnorm(res36, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u36, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u36, null="punif")$p.value)

model37=garchFit(formula=~arma(7,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res37=residuals(model37, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res37, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res37^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model37@fit$ics[1])
BIC_list=append(BIC_list, model37@fit$ics[2])
u37=pnorm(res37, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u37, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u37, null="punif")$p.value)

model38=garchFit(formula=~arma(7,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res38=residuals(model38, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res38, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res38^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model38@fit$ics[1])
BIC_list=append(BIC_list, model38@fit$ics[2])
u38=pnorm(res38, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u38, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u38, null="punif")$p.value)

model39=garchFit(formula=~arma(7,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res39=residuals(model39, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res39, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res39^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model39@fit$ics[1])
BIC_list=append(BIC_list, model39@fit$ics[2])
u39=pnorm(res39, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u39, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u39, null="punif")$p.value)

model40=garchFit(formula=~arma(7,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res40=residuals(model40, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res40, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res40^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model40@fit$ics[1])
BIC_list=append(BIC_list, model40@fit$ics[2])
u40=pnorm(res40, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u40, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u40, null="punif")$p.value)

model41=garchFit(formula=~arma(7,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res41=residuals(model41, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res41, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res41^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model41@fit$ics[1])
BIC_list=append(BIC_list, model41@fit$ics[2])
u41=pnorm(res41, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u41, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u41, null="punif")$p.value)

model42=garchFit(formula=~arma(7,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res42=residuals(model42, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res42, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res42^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model42@fit$ics[1])
BIC_list=append(BIC_list, model42@fit$ics[2])
u42=pnorm(res42, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u42, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u42, null="punif")$p.value)

model43=garchFit(formula=~arma(7,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res43=residuals(model43, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res43, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res43^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model43@fit$ics[1])
BIC_list=append(BIC_list, model43@fit$ics[2])
u43=pnorm(res43, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u43, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u43, null="punif")$p.value)

model44=garchFit(formula=~arma(7,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res44=residuals(model44, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res44, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res44^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model44@fit$ics[1])
BIC_list=append(BIC_list, model44@fit$ics[2])
u44=pnorm(res44, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u44, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u44, null="punif")$p.value)

model45=garchFit(formula=~arma(7,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res45=residuals(model45, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res45, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res45^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model45@fit$ics[1])
BIC_list=append(BIC_list, model45@fit$ics[2])
u45=pnorm(res45, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u45, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u45, null="punif")$p.value)

model46=garchFit(formula=~arma(7,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res46=residuals(model46, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res46, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res46^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model46@fit$ics[1])
BIC_list=append(BIC_list, model46@fit$ics[2])
u46=pnorm(res46, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u46, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u46, null="punif")$p.value)

model47=garchFit(formula=~arma(7,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res47=residuals(model47, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res47, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res47^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model47@fit$ics[1])
BIC_list=append(BIC_list, model47@fit$ics[2])
u47=pnorm(res47, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u47, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u47, null="punif")$p.value)

model48=garchFit(formula=~arma(7,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res48=residuals(model48, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res48, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res48^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model48@fit$ics[1])
BIC_list=append(BIC_list, model48@fit$ics[2])
u48=pnorm(res48, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u48, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u48, null="punif")$p.value)

model49=garchFit(formula=~arma(7,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res49=residuals(model49, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res49, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res49^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model49@fit$ics[1])
BIC_list=append(BIC_list, model49@fit$ics[2])
u49=pnorm(res49, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u49, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u49, null="punif")$p.value)

model50=garchFit(formula=~arma(7,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res50=residuals(model50, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res50, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res50^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model50@fit$ics[1])
BIC_list=append(BIC_list, model50@fit$ics[2])
u50=pnorm(res50, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u50, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u50, null="punif")$p.value)

model51=garchFit(formula=~arma(7,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res51=residuals(model51, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res51, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res51^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model51@fit$ics[1])
BIC_list=append(BIC_list, model51@fit$ics[2])
u51=pnorm(res51, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u51, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u51, null="punif")$p.value)

model52=garchFit(formula=~arma(7,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res52=residuals(model52, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res52, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res52^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model52@fit$ics[1])
BIC_list=append(BIC_list, model52@fit$ics[2])
u52=pnorm(res52, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u52, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u52, null="punif")$p.value)

model53=garchFit(formula=~arma(7,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res53=residuals(model53, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res53, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res53^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model53@fit$ics[1])
BIC_list=append(BIC_list, model53@fit$ics[2])
u53=pnorm(res53, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u53, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u53, null="punif")$p.value)

model54=garchFit(formula=~arma(7,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res54=residuals(model54, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res54, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res54^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model54@fit$ics[1])
BIC_list=append(BIC_list, model54@fit$ics[2])
u54=pnorm(res54, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u54, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u54, null="punif")$p.value)

model55=garchFit(formula=~arma(7,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res55=residuals(model55, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res55, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res55^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model55@fit$ics[1])
BIC_list=append(BIC_list, model55@fit$ics[2])
u55=pnorm(res55, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u55, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u55, null="punif")$p.value)

model56=garchFit(formula=~arma(7,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res56=residuals(model56, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res56, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res56^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model56@fit$ics[1])
BIC_list=append(BIC_list, model56@fit$ics[2])
u56=pnorm(res56, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u56, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u56, null="punif")$p.value)

model57=garchFit(formula=~arma(7,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res57=residuals(model57, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res57, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res57^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model57@fit$ics[1])
BIC_list=append(BIC_list, model57@fit$ics[2])
u57=pnorm(res57, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u57, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u57, null="punif")$p.value)

model58=garchFit(formula=~arma(7,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res58=residuals(model58, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res58, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res58^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model58@fit$ics[1])
BIC_list=append(BIC_list, model58@fit$ics[2])
u58=pnorm(res58, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u58, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u58, null="punif")$p.value)

model59=garchFit(formula=~arma(7,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res59=residuals(model59, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res59, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res59^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model59@fit$ics[1])
BIC_list=append(BIC_list, model59@fit$ics[2])
u59=pnorm(res59, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u59, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u59, null="punif")$p.value)

model60=garchFit(formula=~arma(7,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res60=residuals(model60, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res60, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res60^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model60@fit$ics[1])
BIC_list=append(BIC_list, model60@fit$ics[2])
u60=pnorm(res60, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u60, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u60, null="punif")$p.value)

model61=garchFit(formula=~arma(7,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res61=residuals(model61, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res61, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res61^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model61@fit$ics[1])
BIC_list=append(BIC_list, model61@fit$ics[2])
u61=pnorm(res61, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u61, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u61, null="punif")$p.value)

model62=garchFit(formula=~arma(7,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res62=residuals(model62, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res62, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res62^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model62@fit$ics[1])
BIC_list=append(BIC_list, model62@fit$ics[2])
u62=pnorm(res62, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u62, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u62, null="punif")$p.value)

model63=garchFit(formula=~arma(7,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res63=residuals(model63, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res63, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res63^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model63@fit$ics[1])
BIC_list=append(BIC_list, model63@fit$ics[2])
u63=pnorm(res63, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u63, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u63, null="punif")$p.value)

model64=garchFit(formula=~arma(7,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="norm")
res64=residuals(model64, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res64, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res64^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model64@fit$ics[1])
BIC_list=append(BIC_list, model64@fit$ics[2])
u64=pnorm(res64, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u64, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u64, null="punif")$p.value)

model65=garchFit(formula=~arma(7,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="norm")
res65=residuals(model65, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res65, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res65^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model65@fit$ics[1])
BIC_list=append(BIC_list, model65@fit$ics[2])
u65=pnorm(res65, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u65, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u65, null="punif")$p.value)

model66=garchFit(formula=~arma(7,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="norm")
res66=residuals(model66, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res66, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res66^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model66@fit$ics[1])
BIC_list=append(BIC_list, model66@fit$ics[2])
u66=pnorm(res66, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u66, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u66, null="punif")$p.value)

model67=garchFit(formula=~arma(7,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="norm")
res67=residuals(model67, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res67, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res67^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model67@fit$ics[1])
BIC_list=append(BIC_list, model67@fit$ics[2])
u67=pnorm(res67, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u67, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u67, null="punif")$p.value)

model68=garchFit(formula=~arma(7,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="norm")
res68=residuals(model68, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res68, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res68^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model68@fit$ics[1])
BIC_list=append(BIC_list, model68@fit$ics[2])
u68=pnorm(res68, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u68, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u68, null="punif")$p.value)

model69=garchFit(formula=~arma(7,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="norm")
res69=residuals(model69, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res69, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res69^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model69@fit$ics[1])
BIC_list=append(BIC_list, model69@fit$ics[2])
u69=pnorm(res69, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u69, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u69, null="punif")$p.value)

model70=garchFit(formula=~arma(7,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="norm")
res70=residuals(model70, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res70, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res70^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model70@fit$ics[1])
BIC_list=append(BIC_list, model70@fit$ics[2])
u70=pnorm(res70, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u70, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u70, null="punif")$p.value)

model71=garchFit(formula=~arma(7,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="norm")
res71=residuals(model71, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res71, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res71^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model71@fit$ics[1])
BIC_list=append(BIC_list, model71@fit$ics[2])
u71=pnorm(res71, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u71, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u71, null="punif")$p.value)

model72=garchFit(formula=~arma(7,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="norm")
res72=residuals(model72, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res72, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res72^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model72@fit$ics[1])
BIC_list=append(BIC_list, model72@fit$ics[2])
u72=pnorm(res72, mean=0, sd=1)[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u72, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u72, null="punif")$p.value)

model73=garchFit(formula=~arma(1,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res73=residuals(model73, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res73, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res73^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model73@fit$ics[1])
BIC_list=append(BIC_list, model73@fit$ics[2])
u73=pged(res73, mean=0, sd=1, nu=tail(model73@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u73, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u73, null="punif")$p.value)

model74=garchFit(formula=~arma(1,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res74=residuals(model74, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res74, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res74^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model74@fit$ics[1])
BIC_list=append(BIC_list, model74@fit$ics[2])
u74=pged(res74, mean=0, sd=1, nu=tail(model74@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u74, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u74, null="punif")$p.value)

model75=garchFit(formula=~arma(1,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res75=residuals(model75, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res75, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res75^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model75@fit$ics[1])
BIC_list=append(BIC_list, model75@fit$ics[2])
u75=pged(res75, mean=0, sd=1, nu=tail(model75@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u75, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u75, null="punif")$p.value)

model76=garchFit(formula=~arma(1,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res76=residuals(model76, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res76, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res76^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model76@fit$ics[1])
BIC_list=append(BIC_list, model76@fit$ics[2])
u76=pged(res76, mean=0, sd=1, nu=tail(model76@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u76, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u76, null="punif")$p.value)

model77=garchFit(formula=~arma(1,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res77=residuals(model77, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res77, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res77^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model77@fit$ics[1])
BIC_list=append(BIC_list, model77@fit$ics[2])
u77=pged(res77, mean=0, sd=1, nu=tail(model77@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u77, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u77, null="punif")$p.value)

model78=garchFit(formula=~arma(1,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res78=residuals(model78, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res78, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res78^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model78@fit$ics[1])
BIC_list=append(BIC_list, model78@fit$ics[2])
u78=pged(res78, mean=0, sd=1, nu=tail(model78@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u78, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u78, null="punif")$p.value)

model79=garchFit(formula=~arma(1,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res79=residuals(model79, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res79, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res79^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model79@fit$ics[1])
BIC_list=append(BIC_list, model79@fit$ics[2])
u79=pged(res79, mean=0, sd=1, nu=tail(model79@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u79, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u79, null="punif")$p.value)

model80=garchFit(formula=~arma(1,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res80=residuals(model80, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res80, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res80^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model80@fit$ics[1])
BIC_list=append(BIC_list, model80@fit$ics[2])
u80=pged(res80, mean=0, sd=1, nu=tail(model80@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u80, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u80, null="punif")$p.value)

model81=garchFit(formula=~arma(1,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res81=residuals(model81, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res81, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res81^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model81@fit$ics[1])
BIC_list=append(BIC_list, model81@fit$ics[2])
u81=pged(res81, mean=0, sd=1, nu=tail(model81@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u81, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u81, null="punif")$p.value)

model82=garchFit(formula=~arma(1,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res82=residuals(model82, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res82, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res82^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model82@fit$ics[1])
BIC_list=append(BIC_list, model82@fit$ics[2])
u82=pged(res82, mean=0, sd=1, nu=tail(model82@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u82, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u82, null="punif")$p.value)

model83=garchFit(formula=~arma(1,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res83=residuals(model83, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res83, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res83^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model83@fit$ics[1])
BIC_list=append(BIC_list, model83@fit$ics[2])
u83=pged(res83, mean=0, sd=1, nu=tail(model83@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u83, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u83, null="punif")$p.value)

model84=garchFit(formula=~arma(1,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res84=residuals(model84, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res84, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res84^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model84@fit$ics[1])
BIC_list=append(BIC_list, model84@fit$ics[2])
u84=pged(res84, mean=0, sd=1, nu=tail(model84@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u84, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u84, null="punif")$p.value)

model85=garchFit(formula=~arma(1,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res85=residuals(model85, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res85, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res85^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model85@fit$ics[1])
BIC_list=append(BIC_list, model85@fit$ics[2])
u85=pged(res85, mean=0, sd=1, nu=tail(model85@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u85, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u85, null="punif")$p.value)

model86=garchFit(formula=~arma(1,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res86=residuals(model86, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res86, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res86^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model86@fit$ics[1])
BIC_list=append(BIC_list, model86@fit$ics[2])
u86=pged(res86, mean=0, sd=1, nu=tail(model86@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u86, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u86, null="punif")$p.value)

model87=garchFit(formula=~arma(1,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res87=residuals(model87, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res87, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res87^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model87@fit$ics[1])
BIC_list=append(BIC_list, model87@fit$ics[2])
u87=pged(res87, mean=0, sd=1, nu=tail(model87@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u87, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u87, null="punif")$p.value)

model88=garchFit(formula=~arma(1,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res88=residuals(model88, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res88, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res88^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model88@fit$ics[1])
BIC_list=append(BIC_list, model88@fit$ics[2])
u88=pged(res88, mean=0, sd=1, nu=tail(model88@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u88, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u88, null="punif")$p.value)

model89=garchFit(formula=~arma(1,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res89=residuals(model89, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res89, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res89^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model89@fit$ics[1])
BIC_list=append(BIC_list, model89@fit$ics[2])
u89=pged(res89, mean=0, sd=1, nu=tail(model89@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u89, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u89, null="punif")$p.value)

model90=garchFit(formula=~arma(1,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res90=residuals(model90, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res90, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res90^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model90@fit$ics[1])
BIC_list=append(BIC_list, model90@fit$ics[2])
u90=pged(res90, mean=0, sd=1, nu=tail(model90@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u90, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u90, null="punif")$p.value)

model91=garchFit(formula=~arma(1,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res91=residuals(model91, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res91, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res91^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model91@fit$ics[1])
BIC_list=append(BIC_list, model91@fit$ics[2])
u91=pged(res91, mean=0, sd=1, nu=tail(model91@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u91, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u91, null="punif")$p.value)

model92=garchFit(formula=~arma(1,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res92=residuals(model92, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res92, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res92^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model92@fit$ics[1])
BIC_list=append(BIC_list, model92@fit$ics[2])
u92=pged(res92, mean=0, sd=1, nu=tail(model92@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u92, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u92, null="punif")$p.value)

model93=garchFit(formula=~arma(1,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res93=residuals(model93, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res93, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res93^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model93@fit$ics[1])
BIC_list=append(BIC_list, model93@fit$ics[2])
u93=pged(res93, mean=0, sd=1, nu=tail(model93@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u93, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u93, null="punif")$p.value)

model94=garchFit(formula=~arma(1,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res94=residuals(model94, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res94, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res94^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model94@fit$ics[1])
BIC_list=append(BIC_list, model94@fit$ics[2])
u94=pged(res94, mean=0, sd=1, nu=tail(model94@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u94, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u94, null="punif")$p.value)

model95=garchFit(formula=~arma(1,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res95=residuals(model95, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res95, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res95^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model95@fit$ics[1])
BIC_list=append(BIC_list, model95@fit$ics[2])
u95=pged(res95, mean=0, sd=1, nu=tail(model95@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u95, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u95, null="punif")$p.value)

model96=garchFit(formula=~arma(1,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res96=residuals(model96, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res96, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res96^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model96@fit$ics[1])
BIC_list=append(BIC_list, model96@fit$ics[2])
u96=pged(res96, mean=0, sd=1, nu=tail(model96@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u96, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u96, null="punif")$p.value)

model97=garchFit(formula=~arma(1,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res97=residuals(model97, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res97, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res97^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model97@fit$ics[1])
BIC_list=append(BIC_list, model97@fit$ics[2])
u97=pged(res97, mean=0, sd=1, nu=tail(model97@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u97, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u97, null="punif")$p.value)

model98=garchFit(formula=~arma(1,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res98=residuals(model98, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res98, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res98^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model98@fit$ics[1])
BIC_list=append(BIC_list, model98@fit$ics[2])
u98=pged(res98, mean=0, sd=1, nu=tail(model98@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u98, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u98, null="punif")$p.value)

model99=garchFit(formula=~arma(1,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res99=residuals(model99, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res99, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res99^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model99@fit$ics[1])
BIC_list=append(BIC_list, model99@fit$ics[2])
u99=pged(res99, mean=0, sd=1, nu=tail(model99@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u99, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u99, null="punif")$p.value)

model100=garchFit(formula=~arma(1,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res100=residuals(model100, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res100, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res100^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model100@fit$ics[1])
BIC_list=append(BIC_list, model100@fit$ics[2])
u100=pged(res100, mean=0, sd=1, nu=tail(model100@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u100, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u100, null="punif")$p.value)

model101=garchFit(formula=~arma(1,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res101=residuals(model101, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res101, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res101^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model101@fit$ics[1])
BIC_list=append(BIC_list, model101@fit$ics[2])
u101=pged(res101, mean=0, sd=1, nu=tail(model101@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u101, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u101, null="punif")$p.value)

model102=garchFit(formula=~arma(1,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res102=residuals(model102, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res102, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res102^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model102@fit$ics[1])
BIC_list=append(BIC_list, model102@fit$ics[2])
u102=pged(res102, mean=0, sd=1, nu=tail(model102@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u102, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u102, null="punif")$p.value)

model103=garchFit(formula=~arma(1,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res103=residuals(model103, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res103, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res103^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model103@fit$ics[1])
BIC_list=append(BIC_list, model103@fit$ics[2])
u103=pged(res103, mean=0, sd=1, nu=tail(model103@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u103, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u103, null="punif")$p.value)

model104=garchFit(formula=~arma(1,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res104=residuals(model104, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res104, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res104^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model104@fit$ics[1])
BIC_list=append(BIC_list, model104@fit$ics[2])
u104=pged(res104, mean=0, sd=1, nu=tail(model104@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u104, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u104, null="punif")$p.value)

model105=garchFit(formula=~arma(1,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res105=residuals(model105, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res105, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res105^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model105@fit$ics[1])
BIC_list=append(BIC_list, model105@fit$ics[2])
u105=pged(res105, mean=0, sd=1, nu=tail(model105@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u105, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u105, null="punif")$p.value)

model106=garchFit(formula=~arma(1,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res106=residuals(model106, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res106, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res106^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model106@fit$ics[1])
BIC_list=append(BIC_list, model106@fit$ics[2])
u106=pged(res106, mean=0, sd=1, nu=tail(model106@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u106, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u106, null="punif")$p.value)

model107=garchFit(formula=~arma(1,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res107=residuals(model107, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res107, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res107^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model107@fit$ics[1])
BIC_list=append(BIC_list, model107@fit$ics[2])
u107=pged(res107, mean=0, sd=1, nu=tail(model107@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u107, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u107, null="punif")$p.value)

model108=garchFit(formula=~arma(1,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res108=residuals(model108, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res108, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res108^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model108@fit$ics[1])
BIC_list=append(BIC_list, model108@fit$ics[2])
u108=pged(res108, mean=0, sd=1, nu=tail(model108@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u108, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u108, null="punif")$p.value)

model109=garchFit(formula=~arma(7,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res109=residuals(model109, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res109, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res109^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model109@fit$ics[1])
BIC_list=append(BIC_list, model109@fit$ics[2])
u109=pged(res109, mean=0, sd=1, nu=tail(model109@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u109, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u109, null="punif")$p.value)

model110=garchFit(formula=~arma(7,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res110=residuals(model110, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res110, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res110^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model110@fit$ics[1])
BIC_list=append(BIC_list, model110@fit$ics[2])
u110=pged(res110, mean=0, sd=1, nu=tail(model110@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u110, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u110, null="punif")$p.value)

model111=garchFit(formula=~arma(7,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res111=residuals(model111, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res111, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res111^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model111@fit$ics[1])
BIC_list=append(BIC_list, model111@fit$ics[2])
u111=pged(res111, mean=0, sd=1, nu=tail(model111@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u111, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u111, null="punif")$p.value)

model112=garchFit(formula=~arma(7,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res112=residuals(model112, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res112, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res112^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model112@fit$ics[1])
BIC_list=append(BIC_list, model112@fit$ics[2])
u112=pged(res112, mean=0, sd=1, nu=tail(model112@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u112, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u112, null="punif")$p.value)

model113=garchFit(formula=~arma(7,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res113=residuals(model113, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res113, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res113^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model113@fit$ics[1])
BIC_list=append(BIC_list, model113@fit$ics[2])
u113=pged(res113, mean=0, sd=1, nu=tail(model113@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u113, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u113, null="punif")$p.value)

model114=garchFit(formula=~arma(7,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res114=residuals(model114, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res114, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res114^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model114@fit$ics[1])
BIC_list=append(BIC_list, model114@fit$ics[2])
u114=pged(res114, mean=0, sd=1, nu=tail(model114@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u114, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u114, null="punif")$p.value)

model115=garchFit(formula=~arma(7,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res115=residuals(model115, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res115, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res115^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model115@fit$ics[1])
BIC_list=append(BIC_list, model115@fit$ics[2])
u115=pged(res115, mean=0, sd=1, nu=tail(model115@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u115, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u115, null="punif")$p.value)

model116=garchFit(formula=~arma(7,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res116=residuals(model116, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res116, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res116^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model116@fit$ics[1])
BIC_list=append(BIC_list, model116@fit$ics[2])
u116=pged(res116, mean=0, sd=1, nu=tail(model116@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u116, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u116, null="punif")$p.value)

model117=garchFit(formula=~arma(7,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res117=residuals(model117, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res117, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res117^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model117@fit$ics[1])
BIC_list=append(BIC_list, model117@fit$ics[2])
u117=pged(res117, mean=0, sd=1, nu=tail(model117@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u117, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u117, null="punif")$p.value)

model118=garchFit(formula=~arma(7,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res118=residuals(model118, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res118, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res118^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model118@fit$ics[1])
BIC_list=append(BIC_list, model118@fit$ics[2])
u118=pged(res118, mean=0, sd=1, nu=tail(model118@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u118, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u118, null="punif")$p.value)

model119=garchFit(formula=~arma(7,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res119=residuals(model119, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res119, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res119^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model119@fit$ics[1])
BIC_list=append(BIC_list, model119@fit$ics[2])
u119=pged(res119, mean=0, sd=1, nu=tail(model119@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u119, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u119, null="punif")$p.value)

model120=garchFit(formula=~arma(7,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res120=residuals(model120, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res120, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res120^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model120@fit$ics[1])
BIC_list=append(BIC_list, model120@fit$ics[2])
u120=pged(res120, mean=0, sd=1, nu=tail(model120@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u120, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u120, null="punif")$p.value)

model121=garchFit(formula=~arma(7,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res121=residuals(model121, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res121, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res121^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model121@fit$ics[1])
BIC_list=append(BIC_list, model121@fit$ics[2])
u121=pged(res121, mean=0, sd=1, nu=tail(model121@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u121, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u121, null="punif")$p.value)

model122=garchFit(formula=~arma(7,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res122=residuals(model122, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res122, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res122^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model122@fit$ics[1])
BIC_list=append(BIC_list, model122@fit$ics[2])
u122=pged(res122, mean=0, sd=1, nu=tail(model122@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u122, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u122, null="punif")$p.value)

model123=garchFit(formula=~arma(7,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res123=residuals(model123, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res123, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res123^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model123@fit$ics[1])
BIC_list=append(BIC_list, model123@fit$ics[2])
u123=pged(res123, mean=0, sd=1, nu=tail(model123@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u123, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u123, null="punif")$p.value)

model124=garchFit(formula=~arma(7,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res124=residuals(model124, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res124, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res124^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model124@fit$ics[1])
BIC_list=append(BIC_list, model124@fit$ics[2])
u124=pged(res124, mean=0, sd=1, nu=tail(model124@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u124, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u124, null="punif")$p.value)

model125=garchFit(formula=~arma(7,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res125=residuals(model125, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res125, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res125^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model125@fit$ics[1])
BIC_list=append(BIC_list, model125@fit$ics[2])
u125=pged(res125, mean=0, sd=1, nu=tail(model125@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u125, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u125, null="punif")$p.value)

model126=garchFit(formula=~arma(7,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res126=residuals(model126, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res126, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res126^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model126@fit$ics[1])
BIC_list=append(BIC_list, model126@fit$ics[2])
u126=pged(res126, mean=0, sd=1, nu=tail(model126@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u126, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u126, null="punif")$p.value)

model127=garchFit(formula=~arma(7,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res127=residuals(model127, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res127, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res127^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model127@fit$ics[1])
BIC_list=append(BIC_list, model127@fit$ics[2])
u127=pged(res127, mean=0, sd=1, nu=tail(model127@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u127, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u127, null="punif")$p.value)

model128=garchFit(formula=~arma(7,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res128=residuals(model128, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res128, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res128^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model128@fit$ics[1])
BIC_list=append(BIC_list, model128@fit$ics[2])
u128=pged(res128, mean=0, sd=1, nu=tail(model128@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u128, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u128, null="punif")$p.value)

model129=garchFit(formula=~arma(7,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res129=residuals(model129, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res129, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res129^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model129@fit$ics[1])
BIC_list=append(BIC_list, model129@fit$ics[2])
u129=pged(res129, mean=0, sd=1, nu=tail(model129@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u129, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u129, null="punif")$p.value)

model130=garchFit(formula=~arma(7,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res130=residuals(model130, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res130, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res130^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model130@fit$ics[1])
BIC_list=append(BIC_list, model130@fit$ics[2])
u130=pged(res130, mean=0, sd=1, nu=tail(model130@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u130, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u130, null="punif")$p.value)

model131=garchFit(formula=~arma(7,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res131=residuals(model131, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res131, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res131^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model131@fit$ics[1])
BIC_list=append(BIC_list, model131@fit$ics[2])
u131=pged(res131, mean=0, sd=1, nu=tail(model131@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u131, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u131, null="punif")$p.value)

model132=garchFit(formula=~arma(7,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res132=residuals(model132, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res132, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res132^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model132@fit$ics[1])
BIC_list=append(BIC_list, model132@fit$ics[2])
u132=pged(res132, mean=0, sd=1, nu=tail(model132@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u132, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u132, null="punif")$p.value)

model133=garchFit(formula=~arma(7,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res133=residuals(model133, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res133, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res133^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model133@fit$ics[1])
BIC_list=append(BIC_list, model133@fit$ics[2])
u133=pged(res133, mean=0, sd=1, nu=tail(model133@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u133, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u133, null="punif")$p.value)

model134=garchFit(formula=~arma(7,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res134=residuals(model134, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res134, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res134^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model134@fit$ics[1])
BIC_list=append(BIC_list, model134@fit$ics[2])
u134=pged(res134, mean=0, sd=1, nu=tail(model134@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u134, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u134, null="punif")$p.value)

model135=garchFit(formula=~arma(7,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res135=residuals(model135, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res135, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res135^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model135@fit$ics[1])
BIC_list=append(BIC_list, model135@fit$ics[2])
u135=pged(res135, mean=0, sd=1, nu=tail(model135@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u135, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u135, null="punif")$p.value)

model136=garchFit(formula=~arma(7,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="ged")
res136=residuals(model136, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res136, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res136^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model136@fit$ics[1])
BIC_list=append(BIC_list, model136@fit$ics[2])
u136=pged(res136, mean=0, sd=1, nu=tail(model136@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u136, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u136, null="punif")$p.value)

model137=garchFit(formula=~arma(7,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="ged")
res137=residuals(model137, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res137, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res137^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model137@fit$ics[1])
BIC_list=append(BIC_list, model137@fit$ics[2])
u137=pged(res137, mean=0, sd=1, nu=tail(model137@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u137, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u137, null="punif")$p.value)

model138=garchFit(formula=~arma(7,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="ged")
res138=residuals(model138, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res138, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res138^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model138@fit$ics[1])
BIC_list=append(BIC_list, model138@fit$ics[2])
u138=pged(res138, mean=0, sd=1, nu=tail(model138@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u138, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u138, null="punif")$p.value)

model139=garchFit(formula=~arma(7,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="ged")
res139=residuals(model139, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res139, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res139^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model139@fit$ics[1])
BIC_list=append(BIC_list, model139@fit$ics[2])
u139=pged(res139, mean=0, sd=1, nu=tail(model139@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u139, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u139, null="punif")$p.value)

model140=garchFit(formula=~arma(7,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="ged")
res140=residuals(model140, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res140, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res140^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model140@fit$ics[1])
BIC_list=append(BIC_list, model140@fit$ics[2])
u140=pged(res140, mean=0, sd=1, nu=tail(model140@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u140, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u140, null="punif")$p.value)

model141=garchFit(formula=~arma(7,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="ged")
res141=residuals(model141, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res141, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res141^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model141@fit$ics[1])
BIC_list=append(BIC_list, model141@fit$ics[2])
u141=pged(res141, mean=0, sd=1, nu=tail(model141@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u141, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u141, null="punif")$p.value)

model142=garchFit(formula=~arma(7,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="ged")
res142=residuals(model142, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res142, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res142^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model142@fit$ics[1])
BIC_list=append(BIC_list, model142@fit$ics[2])
u142=pged(res142, mean=0, sd=1, nu=tail(model142@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u142, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u142, null="punif")$p.value)

model143=garchFit(formula=~arma(7,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="ged")
res143=residuals(model143, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res143, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res143^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model143@fit$ics[1])
BIC_list=append(BIC_list, model143@fit$ics[2])
u143=pged(res143, mean=0, sd=1, nu=tail(model143@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u143, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u143, null="punif")$p.value)

model144=garchFit(formula=~arma(7,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="ged")
res144=residuals(model144, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res144, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res144^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model144@fit$ics[1])
BIC_list=append(BIC_list, model144@fit$ics[2])
u144=pged(res144, mean=0, sd=1, nu=tail(model144@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u144, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u144, null="punif")$p.value)

model145=garchFit(formula=~arma(1,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res145=residuals(model145, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res145, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res145^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model145@fit$ics[1])
BIC_list=append(BIC_list, model145@fit$ics[2])
u145=pstd(res145, mean=0, sd=1, nu=tail(model145@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u145, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u145, null="punif")$p.value)

model146=garchFit(formula=~arma(1,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res146=residuals(model146, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res146, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res146^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model146@fit$ics[1])
BIC_list=append(BIC_list, model146@fit$ics[2])
u146=pstd(res146, mean=0, sd=1, nu=tail(model146@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u146, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u146, null="punif")$p.value)

model147=garchFit(formula=~arma(1,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res147=residuals(model147, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res147, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res147^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model147@fit$ics[1])
BIC_list=append(BIC_list, model147@fit$ics[2])
u147=pstd(res147, mean=0, sd=1, nu=tail(model147@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u147, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u147, null="punif")$p.value)

model148=garchFit(formula=~arma(1,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res148=residuals(model148, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res148, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res148^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model148@fit$ics[1])
BIC_list=append(BIC_list, model148@fit$ics[2])
u148=pstd(res148, mean=0, sd=1, nu=tail(model148@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u148, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u148, null="punif")$p.value)

model149=garchFit(formula=~arma(1,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res149=residuals(model149, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res149, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res149^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model149@fit$ics[1])
BIC_list=append(BIC_list, model149@fit$ics[2])
u149=pstd(res149, mean=0, sd=1, nu=tail(model149@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u149, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u149, null="punif")$p.value)

model150=garchFit(formula=~arma(1,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res150=residuals(model150, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res150, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res150^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model150@fit$ics[1])
BIC_list=append(BIC_list, model150@fit$ics[2])
u150=pstd(res150, mean=0, sd=1, nu=tail(model150@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u150, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u150, null="punif")$p.value)

model151=garchFit(formula=~arma(1,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res151=residuals(model151, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res151, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res151^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model151@fit$ics[1])
BIC_list=append(BIC_list, model151@fit$ics[2])
u151=pstd(res151, mean=0, sd=1, nu=tail(model151@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u151, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u151, null="punif")$p.value)

model152=garchFit(formula=~arma(1,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res152=residuals(model152, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res152, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res152^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model152@fit$ics[1])
BIC_list=append(BIC_list, model152@fit$ics[2])
u152=pstd(res152, mean=0, sd=1, nu=tail(model152@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u152, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u152, null="punif")$p.value)

model153=garchFit(formula=~arma(1,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res153=residuals(model153, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res153, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res153^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model153@fit$ics[1])
BIC_list=append(BIC_list, model153@fit$ics[2])
u153=pstd(res153, mean=0, sd=1, nu=tail(model153@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u153, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u153, null="punif")$p.value)

model154=garchFit(formula=~arma(1,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res154=residuals(model154, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res154, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res154^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model154@fit$ics[1])
BIC_list=append(BIC_list, model154@fit$ics[2])
u154=pstd(res154, mean=0, sd=1, nu=tail(model154@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u154, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u154, null="punif")$p.value)

model155=garchFit(formula=~arma(1,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res155=residuals(model155, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res155, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res155^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model155@fit$ics[1])
BIC_list=append(BIC_list, model155@fit$ics[2])
u155=pstd(res155, mean=0, sd=1, nu=tail(model155@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u155, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u155, null="punif")$p.value)

model156=garchFit(formula=~arma(1,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res156=residuals(model156, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res156, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res156^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model156@fit$ics[1])
BIC_list=append(BIC_list, model156@fit$ics[2])
u156=pstd(res156, mean=0, sd=1, nu=tail(model156@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u156, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u156, null="punif")$p.value)

model157=garchFit(formula=~arma(1,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res157=residuals(model157, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res157, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res157^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model157@fit$ics[1])
BIC_list=append(BIC_list, model157@fit$ics[2])
u157=pstd(res157, mean=0, sd=1, nu=tail(model157@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u157, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u157, null="punif")$p.value)

model158=garchFit(formula=~arma(1,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res158=residuals(model158, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res158, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res158^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model158@fit$ics[1])
BIC_list=append(BIC_list, model158@fit$ics[2])
u158=pstd(res158, mean=0, sd=1, nu=tail(model158@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u158, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u158, null="punif")$p.value)

model159=garchFit(formula=~arma(1,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res159=residuals(model159, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res159, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res159^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model159@fit$ics[1])
BIC_list=append(BIC_list, model159@fit$ics[2])
u159=pstd(res159, mean=0, sd=1, nu=tail(model159@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u159, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u159, null="punif")$p.value)

model160=garchFit(formula=~arma(1,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res160=residuals(model160, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res160, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res160^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model160@fit$ics[1])
BIC_list=append(BIC_list, model160@fit$ics[2])
u160=pstd(res160, mean=0, sd=1, nu=tail(model160@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u160, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u160, null="punif")$p.value)

model161=garchFit(formula=~arma(1,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res161=residuals(model161, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res161, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res161^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model161@fit$ics[1])
BIC_list=append(BIC_list, model161@fit$ics[2])
u161=pstd(res161, mean=0, sd=1, nu=tail(model161@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u161, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u161, null="punif")$p.value)

model162=garchFit(formula=~arma(1,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res162=residuals(model162, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res162, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res162^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model162@fit$ics[1])
BIC_list=append(BIC_list, model162@fit$ics[2])
u162=pstd(res162, mean=0, sd=1, nu=tail(model162@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u162, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u162, null="punif")$p.value)

model163=garchFit(formula=~arma(1,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res163=residuals(model163, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res163, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res163^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model163@fit$ics[1])
BIC_list=append(BIC_list, model163@fit$ics[2])
u163=pstd(res163, mean=0, sd=1, nu=tail(model163@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u163, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u163, null="punif")$p.value)

model164=garchFit(formula=~arma(1,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res164=residuals(model164, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res164, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res164^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model164@fit$ics[1])
BIC_list=append(BIC_list, model164@fit$ics[2])
u164=pstd(res164, mean=0, sd=1, nu=tail(model164@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u164, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u164, null="punif")$p.value)

model165=garchFit(formula=~arma(1,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res165=residuals(model165, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res165, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res165^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model165@fit$ics[1])
BIC_list=append(BIC_list, model165@fit$ics[2])
u165=pstd(res165, mean=0, sd=1, nu=tail(model165@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u165, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u165, null="punif")$p.value)

model166=garchFit(formula=~arma(1,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res166=residuals(model166, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res166, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res166^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model166@fit$ics[1])
BIC_list=append(BIC_list, model166@fit$ics[2])
u166=pstd(res166, mean=0, sd=1, nu=tail(model166@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u166, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u166, null="punif")$p.value)

model167=garchFit(formula=~arma(1,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res167=residuals(model167, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res167, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res167^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model167@fit$ics[1])
BIC_list=append(BIC_list, model167@fit$ics[2])
u167=pstd(res167, mean=0, sd=1, nu=tail(model167@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u167, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u167, null="punif")$p.value)

model168=garchFit(formula=~arma(1,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res168=residuals(model168, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res168, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res168^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model168@fit$ics[1])
BIC_list=append(BIC_list, model168@fit$ics[2])
u168=pstd(res168, mean=0, sd=1, nu=tail(model168@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u168, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u168, null="punif")$p.value)

model169=garchFit(formula=~arma(1,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res169=residuals(model169, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res169, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res169^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model169@fit$ics[1])
BIC_list=append(BIC_list, model169@fit$ics[2])
u169=pstd(res169, mean=0, sd=1, nu=tail(model169@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u169, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u169, null="punif")$p.value)

model170=garchFit(formula=~arma(1,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res170=residuals(model170, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res170, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res170^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model170@fit$ics[1])
BIC_list=append(BIC_list, model170@fit$ics[2])
u170=pstd(res170, mean=0, sd=1, nu=tail(model170@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u170, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u170, null="punif")$p.value)

model171=garchFit(formula=~arma(1,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res171=residuals(model171, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res171, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res171^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model171@fit$ics[1])
BIC_list=append(BIC_list, model171@fit$ics[2])
u171=pstd(res171, mean=0, sd=1, nu=tail(model171@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u171, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u171, null="punif")$p.value)

model172=garchFit(formula=~arma(1,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res172=residuals(model172, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res172, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res172^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model172@fit$ics[1])
BIC_list=append(BIC_list, model172@fit$ics[2])
u172=pstd(res172, mean=0, sd=1, nu=tail(model172@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u172, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u172, null="punif")$p.value)

model173=garchFit(formula=~arma(1,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res173=residuals(model173, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res173, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res173^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model173@fit$ics[1])
BIC_list=append(BIC_list, model173@fit$ics[2])
u173=pstd(res173, mean=0, sd=1, nu=tail(model173@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u173, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u173, null="punif")$p.value)

model174=garchFit(formula=~arma(1,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res174=residuals(model174, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res174, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res174^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model174@fit$ics[1])
BIC_list=append(BIC_list, model174@fit$ics[2])
u174=pstd(res174, mean=0, sd=1, nu=tail(model174@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u174, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u174, null="punif")$p.value)

model175=garchFit(formula=~arma(1,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res175=residuals(model175, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res175, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res175^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model175@fit$ics[1])
BIC_list=append(BIC_list, model175@fit$ics[2])
u175=pstd(res175, mean=0, sd=1, nu=tail(model175@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u175, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u175, null="punif")$p.value)

model176=garchFit(formula=~arma(1,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res176=residuals(model176, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res176, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res176^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model176@fit$ics[1])
BIC_list=append(BIC_list, model176@fit$ics[2])
u176=pstd(res176, mean=0, sd=1, nu=tail(model176@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u176, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u176, null="punif")$p.value)

model177=garchFit(formula=~arma(1,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res177=residuals(model177, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res177, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res177^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model177@fit$ics[1])
BIC_list=append(BIC_list, model177@fit$ics[2])
u177=pstd(res177, mean=0, sd=1, nu=tail(model177@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u177, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u177, null="punif")$p.value)

model178=garchFit(formula=~arma(1,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res178=residuals(model178, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res178, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res178^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model178@fit$ics[1])
BIC_list=append(BIC_list, model178@fit$ics[2])
u178=pstd(res178, mean=0, sd=1, nu=tail(model178@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u178, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u178, null="punif")$p.value)

model179=garchFit(formula=~arma(1,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res179=residuals(model179, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res179, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res179^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model179@fit$ics[1])
BIC_list=append(BIC_list, model179@fit$ics[2])
u179=pstd(res179, mean=0, sd=1, nu=tail(model179@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u179, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u179, null="punif")$p.value)

model180=garchFit(formula=~arma(1,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res180=residuals(model180, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res180, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res180^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model180@fit$ics[1])
BIC_list=append(BIC_list, model180@fit$ics[2])
u180=pstd(res180, mean=0, sd=1, nu=tail(model180@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u180, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u180, null="punif")$p.value)

model181=garchFit(formula=~arma(7,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res181=residuals(model181, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res181, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res181^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model181@fit$ics[1])
BIC_list=append(BIC_list, model181@fit$ics[2])
u181=pstd(res181, mean=0, sd=1, nu=tail(model181@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u181, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u181, null="punif")$p.value)

model182=garchFit(formula=~arma(7,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res182=residuals(model182, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res182, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res182^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model182@fit$ics[1])
BIC_list=append(BIC_list, model182@fit$ics[2])
u182=pstd(res182, mean=0, sd=1, nu=tail(model182@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u182, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u182, null="punif")$p.value)

model183=garchFit(formula=~arma(7,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res183=residuals(model183, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res183, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res183^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model183@fit$ics[1])
BIC_list=append(BIC_list, model183@fit$ics[2])
u183=pstd(res183, mean=0, sd=1, nu=tail(model183@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u183, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u183, null="punif")$p.value)

model184=garchFit(formula=~arma(7,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res184=residuals(model184, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res184, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res184^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model184@fit$ics[1])
BIC_list=append(BIC_list, model184@fit$ics[2])
u184=pstd(res184, mean=0, sd=1, nu=tail(model184@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u184, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u184, null="punif")$p.value)

model185=garchFit(formula=~arma(7,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res185=residuals(model185, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res185, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res185^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model185@fit$ics[1])
BIC_list=append(BIC_list, model185@fit$ics[2])
u185=pstd(res185, mean=0, sd=1, nu=tail(model185@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u185, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u185, null="punif")$p.value)

model186=garchFit(formula=~arma(7,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res186=residuals(model186, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res186, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res186^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model186@fit$ics[1])
BIC_list=append(BIC_list, model186@fit$ics[2])
u186=pstd(res186, mean=0, sd=1, nu=tail(model186@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u186, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u186, null="punif")$p.value)

model187=garchFit(formula=~arma(7,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res187=residuals(model187, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res187, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res187^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model187@fit$ics[1])
BIC_list=append(BIC_list, model187@fit$ics[2])
u187=pstd(res187, mean=0, sd=1, nu=tail(model187@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u187, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u187, null="punif")$p.value)

model188=garchFit(formula=~arma(7,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res188=residuals(model188, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res188, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res188^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model188@fit$ics[1])
BIC_list=append(BIC_list, model188@fit$ics[2])
u188=pstd(res188, mean=0, sd=1, nu=tail(model188@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u188, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u188, null="punif")$p.value)

model189=garchFit(formula=~arma(7,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res189=residuals(model189, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res189, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res189^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model189@fit$ics[1])
BIC_list=append(BIC_list, model189@fit$ics[2])
u189=pstd(res189, mean=0, sd=1, nu=tail(model189@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u189, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u189, null="punif")$p.value)

model190=garchFit(formula=~arma(7,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res190=residuals(model190, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res190, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res190^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model190@fit$ics[1])
BIC_list=append(BIC_list, model190@fit$ics[2])
u190=pstd(res190, mean=0, sd=1, nu=tail(model190@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u190, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u190, null="punif")$p.value)

model191=garchFit(formula=~arma(7,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res191=residuals(model191, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res191, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res191^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model191@fit$ics[1])
BIC_list=append(BIC_list, model191@fit$ics[2])
u191=pstd(res191, mean=0, sd=1, nu=tail(model191@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u191, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u191, null="punif")$p.value)

model192=garchFit(formula=~arma(7,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res192=residuals(model192, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res192, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res192^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model192@fit$ics[1])
BIC_list=append(BIC_list, model192@fit$ics[2])
u192=pstd(res192, mean=0, sd=1, nu=tail(model192@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u192, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u192, null="punif")$p.value)

model193=garchFit(formula=~arma(7,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res193=residuals(model193, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res193, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res193^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model193@fit$ics[1])
BIC_list=append(BIC_list, model193@fit$ics[2])
u193=pstd(res193, mean=0, sd=1, nu=tail(model193@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u193, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u193, null="punif")$p.value)

model194=garchFit(formula=~arma(7,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res194=residuals(model194, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res194, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res194^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model194@fit$ics[1])
BIC_list=append(BIC_list, model194@fit$ics[2])
u194=pstd(res194, mean=0, sd=1, nu=tail(model194@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u194, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u194, null="punif")$p.value)

model195=garchFit(formula=~arma(7,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res195=residuals(model195, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res195, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res195^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model195@fit$ics[1])
BIC_list=append(BIC_list, model195@fit$ics[2])
u195=pstd(res195, mean=0, sd=1, nu=tail(model195@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u195, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u195, null="punif")$p.value)

model196=garchFit(formula=~arma(7,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res196=residuals(model196, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res196, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res196^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model196@fit$ics[1])
BIC_list=append(BIC_list, model196@fit$ics[2])
u196=pstd(res196, mean=0, sd=1, nu=tail(model196@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u196, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u196, null="punif")$p.value)

model197=garchFit(formula=~arma(7,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res197=residuals(model197, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res197, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res197^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model197@fit$ics[1])
BIC_list=append(BIC_list, model197@fit$ics[2])
u197=pstd(res197, mean=0, sd=1, nu=tail(model197@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u197, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u197, null="punif")$p.value)

model198=garchFit(formula=~arma(7,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res198=residuals(model198, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res198, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res198^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model198@fit$ics[1])
BIC_list=append(BIC_list, model198@fit$ics[2])
u198=pstd(res198, mean=0, sd=1, nu=tail(model198@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u198, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u198, null="punif")$p.value)

model199=garchFit(formula=~arma(7,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res199=residuals(model199, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res199, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res199^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model199@fit$ics[1])
BIC_list=append(BIC_list, model199@fit$ics[2])
u199=pstd(res199, mean=0, sd=1, nu=tail(model199@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u199, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u199, null="punif")$p.value)

model200=garchFit(formula=~arma(7,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res200=residuals(model200, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res200, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res200^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model200@fit$ics[1])
BIC_list=append(BIC_list, model200@fit$ics[2])
u200=pstd(res200, mean=0, sd=1, nu=tail(model200@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u200, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u200, null="punif")$p.value)

model201=garchFit(formula=~arma(7,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res201=residuals(model201, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res201, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res201^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model201@fit$ics[1])
BIC_list=append(BIC_list, model201@fit$ics[2])
u201=pstd(res201, mean=0, sd=1, nu=tail(model201@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u201, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u201, null="punif")$p.value)

model202=garchFit(formula=~arma(7,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res202=residuals(model202, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res202, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res202^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model202@fit$ics[1])
BIC_list=append(BIC_list, model202@fit$ics[2])
u202=pstd(res202, mean=0, sd=1, nu=tail(model202@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u202, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u202, null="punif")$p.value)

model203=garchFit(formula=~arma(7,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res203=residuals(model203, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res203, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res203^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model203@fit$ics[1])
BIC_list=append(BIC_list, model203@fit$ics[2])
u203=pstd(res203, mean=0, sd=1, nu=tail(model203@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u203, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u203, null="punif")$p.value)

model204=garchFit(formula=~arma(7,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res204=residuals(model204, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res204, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res204^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model204@fit$ics[1])
BIC_list=append(BIC_list, model204@fit$ics[2])
u204=pstd(res204, mean=0, sd=1, nu=tail(model204@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u204, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u204, null="punif")$p.value)

model205=garchFit(formula=~arma(7,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res205=residuals(model205, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res205, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res205^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model205@fit$ics[1])
BIC_list=append(BIC_list, model205@fit$ics[2])
u205=pstd(res205, mean=0, sd=1, nu=tail(model205@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u205, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u205, null="punif")$p.value)

model206=garchFit(formula=~arma(7,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res206=residuals(model206, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res206, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res206^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model206@fit$ics[1])
BIC_list=append(BIC_list, model206@fit$ics[2])
u206=pstd(res206, mean=0, sd=1, nu=tail(model206@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u206, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u206, null="punif")$p.value)

model207=garchFit(formula=~arma(7,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res207=residuals(model207, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res207, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res207^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model207@fit$ics[1])
BIC_list=append(BIC_list, model207@fit$ics[2])
u207=pstd(res207, mean=0, sd=1, nu=tail(model207@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u207, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u207, null="punif")$p.value)

model208=garchFit(formula=~arma(7,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="std")
res208=residuals(model208, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res208, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res208^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model208@fit$ics[1])
BIC_list=append(BIC_list, model208@fit$ics[2])
u208=pstd(res208, mean=0, sd=1, nu=tail(model208@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u208, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u208, null="punif")$p.value)

model209=garchFit(formula=~arma(7,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="std")
res209=residuals(model209, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res209, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res209^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model209@fit$ics[1])
BIC_list=append(BIC_list, model209@fit$ics[2])
u209=pstd(res209, mean=0, sd=1, nu=tail(model209@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u209, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u209, null="punif")$p.value)

model210=garchFit(formula=~arma(7,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="std")
res210=residuals(model210, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res210, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res210^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model210@fit$ics[1])
BIC_list=append(BIC_list, model210@fit$ics[2])
u210=pstd(res210, mean=0, sd=1, nu=tail(model210@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u210, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u210, null="punif")$p.value)

model211=garchFit(formula=~arma(7,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="std")
res211=residuals(model211, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res211, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res211^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model211@fit$ics[1])
BIC_list=append(BIC_list, model211@fit$ics[2])
u211=pstd(res211, mean=0, sd=1, nu=tail(model211@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u211, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u211, null="punif")$p.value)

model212=garchFit(formula=~arma(7,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="std")
res212=residuals(model212, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res212, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res212^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model212@fit$ics[1])
BIC_list=append(BIC_list, model212@fit$ics[2])
u212=pstd(res212, mean=0, sd=1, nu=tail(model212@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u212, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u212, null="punif")$p.value)

model213=garchFit(formula=~arma(7,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="std")
res213=residuals(model213, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res213, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res213^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model213@fit$ics[1])
BIC_list=append(BIC_list, model213@fit$ics[2])
u213=pstd(res213, mean=0, sd=1, nu=tail(model213@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u213, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u213, null="punif")$p.value)

model214=garchFit(formula=~arma(7,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="std")
res214=residuals(model214, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res214, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res214^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model214@fit$ics[1])
BIC_list=append(BIC_list, model214@fit$ics[2])
u214=pstd(res214, mean=0, sd=1, nu=tail(model214@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u214, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u214, null="punif")$p.value)

model215=garchFit(formula=~arma(7,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="std")
res215=residuals(model215, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res215, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res215^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model215@fit$ics[1])
BIC_list=append(BIC_list, model215@fit$ics[2])
u215=pstd(res215, mean=0, sd=1, nu=tail(model215@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u215, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u215, null="punif")$p.value)

model216=garchFit(formula=~arma(7,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="std")
res216=residuals(model216, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res216, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res216^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model216@fit$ics[1])
BIC_list=append(BIC_list, model216@fit$ics[2])
u216=pstd(res216, mean=0, sd=1, nu=tail(model216@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u216, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u216, null="punif")$p.value)

model217=garchFit(formula=~arma(1,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res217=residuals(model217, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res217, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res217^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model217@fit$ics[1])
BIC_list=append(BIC_list, model217@fit$ics[2])
u217=psstd(res217, mean=0, sd=1, nu=tail(model217@fit$coef, n=1), xi=model217@fit$coef[length(model217@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u217, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u217, null="punif")$p.value)

model218=garchFit(formula=~arma(1,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res218=residuals(model218, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res218, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res218^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model218@fit$ics[1])
BIC_list=append(BIC_list, model218@fit$ics[2])
u218=psstd(res218, mean=0, sd=1, nu=tail(model218@fit$coef, n=1), xi=model218@fit$coef[length(model218@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u218, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u218, null="punif")$p.value)

model219=garchFit(formula=~arma(1,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res219=residuals(model219, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res219, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res219^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model219@fit$ics[1])
BIC_list=append(BIC_list, model219@fit$ics[2])
u219=psstd(res219, mean=0, sd=1, nu=tail(model219@fit$coef, n=1), xi=model219@fit$coef[length(model219@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u219, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u219, null="punif")$p.value)

model220=garchFit(formula=~arma(1,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res220=residuals(model220, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res220, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res220^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model220@fit$ics[1])
BIC_list=append(BIC_list, model220@fit$ics[2])
u220=psstd(res220, mean=0, sd=1, nu=tail(model220@fit$coef, n=1), xi=model220@fit$coef[length(model220@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u220, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u220, null="punif")$p.value)

model221=garchFit(formula=~arma(1,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res221=residuals(model221, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res221, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res221^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model221@fit$ics[1])
BIC_list=append(BIC_list, model221@fit$ics[2])
u221=psstd(res221, mean=0, sd=1, nu=tail(model221@fit$coef, n=1), xi=model221@fit$coef[length(model221@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u221, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u221, null="punif")$p.value)

model222=garchFit(formula=~arma(1,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res222=residuals(model222, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res222, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res222^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model222@fit$ics[1])
BIC_list=append(BIC_list, model222@fit$ics[2])
u222=psstd(res222, mean=0, sd=1, nu=tail(model222@fit$coef, n=1), xi=model222@fit$coef[length(model222@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u222, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u222, null="punif")$p.value)

model223=garchFit(formula=~arma(1,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res223=residuals(model223, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res223, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res223^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model223@fit$ics[1])
BIC_list=append(BIC_list, model223@fit$ics[2])
u223=psstd(res223, mean=0, sd=1, nu=tail(model223@fit$coef, n=1), xi=model223@fit$coef[length(model223@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u223, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u223, null="punif")$p.value)

model224=garchFit(formula=~arma(1,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res224=residuals(model224, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res224, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res224^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model224@fit$ics[1])
BIC_list=append(BIC_list, model224@fit$ics[2])
u224=psstd(res224, mean=0, sd=1, nu=tail(model224@fit$coef, n=1), xi=model224@fit$coef[length(model224@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u224, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u224, null="punif")$p.value)

model225=garchFit(formula=~arma(1,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res225=residuals(model225, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res225, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res225^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model225@fit$ics[1])
BIC_list=append(BIC_list, model225@fit$ics[2])
u225=psstd(res225, mean=0, sd=1, nu=tail(model225@fit$coef, n=1), xi=model225@fit$coef[length(model225@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u225, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u225, null="punif")$p.value)

model226=garchFit(formula=~arma(1,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res226=residuals(model226, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res226, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res226^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model226@fit$ics[1])
BIC_list=append(BIC_list, model226@fit$ics[2])
u226=psstd(res226, mean=0, sd=1, nu=tail(model226@fit$coef, n=1), xi=model226@fit$coef[length(model226@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u226, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u226, null="punif")$p.value)

model227=garchFit(formula=~arma(1,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res227=residuals(model227, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res227, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res227^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model227@fit$ics[1])
BIC_list=append(BIC_list, model227@fit$ics[2])
u227=psstd(res227, mean=0, sd=1, nu=tail(model227@fit$coef, n=1), xi=model227@fit$coef[length(model227@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u227, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u227, null="punif")$p.value)

model228=garchFit(formula=~arma(1,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res228=residuals(model228, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res228, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res228^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model228@fit$ics[1])
BIC_list=append(BIC_list, model228@fit$ics[2])
u228=psstd(res228, mean=0, sd=1, nu=tail(model228@fit$coef, n=1), xi=model228@fit$coef[length(model228@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u228, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u228, null="punif")$p.value)

model229=garchFit(formula=~arma(1,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res229=residuals(model229, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res229, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res229^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model229@fit$ics[1])
BIC_list=append(BIC_list, model229@fit$ics[2])
u229=psstd(res229, mean=0, sd=1, nu=tail(model229@fit$coef, n=1), xi=model229@fit$coef[length(model229@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u229, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u229, null="punif")$p.value)

model230=garchFit(formula=~arma(1,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res230=residuals(model230, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res230, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res230^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model230@fit$ics[1])
BIC_list=append(BIC_list, model230@fit$ics[2])
u230=psstd(res230, mean=0, sd=1, nu=tail(model230@fit$coef, n=1), xi=model230@fit$coef[length(model230@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u230, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u230, null="punif")$p.value)

model231=garchFit(formula=~arma(1,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res231=residuals(model231, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res231, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res231^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model231@fit$ics[1])
BIC_list=append(BIC_list, model231@fit$ics[2])
u231=psstd(res231, mean=0, sd=1, nu=tail(model231@fit$coef, n=1), xi=model231@fit$coef[length(model231@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u231, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u231, null="punif")$p.value)

model232=garchFit(formula=~arma(1,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res232=residuals(model232, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res232, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res232^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model232@fit$ics[1])
BIC_list=append(BIC_list, model232@fit$ics[2])
u232=psstd(res232, mean=0, sd=1, nu=tail(model232@fit$coef, n=1), xi=model232@fit$coef[length(model232@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u232, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u232, null="punif")$p.value)

model233=garchFit(formula=~arma(1,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res233=residuals(model233, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res233, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res233^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model233@fit$ics[1])
BIC_list=append(BIC_list, model233@fit$ics[2])
u233=psstd(res233, mean=0, sd=1, nu=tail(model233@fit$coef, n=1), xi=model233@fit$coef[length(model233@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u233, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u233, null="punif")$p.value)

model234=garchFit(formula=~arma(1,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res234=residuals(model234, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res234, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res234^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model234@fit$ics[1])
BIC_list=append(BIC_list, model234@fit$ics[2])
u234=psstd(res234, mean=0, sd=1, nu=tail(model234@fit$coef, n=1), xi=model234@fit$coef[length(model234@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u234, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u234, null="punif")$p.value)

model235=garchFit(formula=~arma(1,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res235=residuals(model235, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res235, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res235^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model235@fit$ics[1])
BIC_list=append(BIC_list, model235@fit$ics[2])
u235=psstd(res235, mean=0, sd=1, nu=tail(model235@fit$coef, n=1), xi=model235@fit$coef[length(model235@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u235, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u235, null="punif")$p.value)

model236=garchFit(formula=~arma(1,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res236=residuals(model236, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res236, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res236^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model236@fit$ics[1])
BIC_list=append(BIC_list, model236@fit$ics[2])
u236=psstd(res236, mean=0, sd=1, nu=tail(model236@fit$coef, n=1), xi=model236@fit$coef[length(model236@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u236, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u236, null="punif")$p.value)

model237=garchFit(formula=~arma(1,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res237=residuals(model237, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res237, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res237^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model237@fit$ics[1])
BIC_list=append(BIC_list, model237@fit$ics[2])
u237=psstd(res237, mean=0, sd=1, nu=tail(model237@fit$coef, n=1), xi=model237@fit$coef[length(model237@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u237, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u237, null="punif")$p.value)

model238=garchFit(formula=~arma(1,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res238=residuals(model238, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res238, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res238^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model238@fit$ics[1])
BIC_list=append(BIC_list, model238@fit$ics[2])
u238=psstd(res238, mean=0, sd=1, nu=tail(model238@fit$coef, n=1), xi=model238@fit$coef[length(model238@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u238, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u238, null="punif")$p.value)

model239=garchFit(formula=~arma(1,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res239=residuals(model239, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res239, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res239^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model239@fit$ics[1])
BIC_list=append(BIC_list, model239@fit$ics[2])
u239=psstd(res239, mean=0, sd=1, nu=tail(model239@fit$coef, n=1), xi=model239@fit$coef[length(model239@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u239, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u239, null="punif")$p.value)

model240=garchFit(formula=~arma(1,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res240=residuals(model240, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res240, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res240^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model240@fit$ics[1])
BIC_list=append(BIC_list, model240@fit$ics[2])
u240=psstd(res240, mean=0, sd=1, nu=tail(model240@fit$coef, n=1), xi=model240@fit$coef[length(model240@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u240, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u240, null="punif")$p.value)

model241=garchFit(formula=~arma(1,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res241=residuals(model241, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res241, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res241^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model241@fit$ics[1])
BIC_list=append(BIC_list, model241@fit$ics[2])
u241=psstd(res241, mean=0, sd=1, nu=tail(model241@fit$coef, n=1), xi=model241@fit$coef[length(model241@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u241, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u241, null="punif")$p.value)

model242=garchFit(formula=~arma(1,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res242=residuals(model242, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res242, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res242^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model242@fit$ics[1])
BIC_list=append(BIC_list, model242@fit$ics[2])
u242=psstd(res242, mean=0, sd=1, nu=tail(model242@fit$coef, n=1), xi=model242@fit$coef[length(model242@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u242, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u242, null="punif")$p.value)

model243=garchFit(formula=~arma(1,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res243=residuals(model243, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res243, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res243^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model243@fit$ics[1])
BIC_list=append(BIC_list, model243@fit$ics[2])
u243=psstd(res243, mean=0, sd=1, nu=tail(model243@fit$coef, n=1), xi=model243@fit$coef[length(model243@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u243, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u243, null="punif")$p.value)

model244=garchFit(formula=~arma(1,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res244=residuals(model244, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res244, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res244^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model244@fit$ics[1])
BIC_list=append(BIC_list, model244@fit$ics[2])
u244=psstd(res244, mean=0, sd=1, nu=tail(model244@fit$coef, n=1), xi=model244@fit$coef[length(model244@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u244, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u244, null="punif")$p.value)

model245=garchFit(formula=~arma(1,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res245=residuals(model245, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res245, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res245^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model245@fit$ics[1])
BIC_list=append(BIC_list, model245@fit$ics[2])
u245=psstd(res245, mean=0, sd=1, nu=tail(model245@fit$coef, n=1), xi=model245@fit$coef[length(model245@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u245, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u245, null="punif")$p.value)

model246=garchFit(formula=~arma(1,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res246=residuals(model246, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res246, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res246^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model246@fit$ics[1])
BIC_list=append(BIC_list, model246@fit$ics[2])
u246=psstd(res246, mean=0, sd=1, nu=tail(model246@fit$coef, n=1), xi=model246@fit$coef[length(model246@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u246, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u246, null="punif")$p.value)

model247=garchFit(formula=~arma(1,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res247=residuals(model247, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res247, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res247^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model247@fit$ics[1])
BIC_list=append(BIC_list, model247@fit$ics[2])
u247=psstd(res247, mean=0, sd=1, nu=tail(model247@fit$coef, n=1), xi=model247@fit$coef[length(model247@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u247, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u247, null="punif")$p.value)

model248=garchFit(formula=~arma(1,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res248=residuals(model248, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res248, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res248^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model248@fit$ics[1])
BIC_list=append(BIC_list, model248@fit$ics[2])
u248=psstd(res248, mean=0, sd=1, nu=tail(model248@fit$coef, n=1), xi=model248@fit$coef[length(model248@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u248, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u248, null="punif")$p.value)

model249=garchFit(formula=~arma(1,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res249=residuals(model249, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res249, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res249^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model249@fit$ics[1])
BIC_list=append(BIC_list, model249@fit$ics[2])
u249=psstd(res249, mean=0, sd=1, nu=tail(model249@fit$coef, n=1), xi=model249@fit$coef[length(model249@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u249, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u249, null="punif")$p.value)

model250=garchFit(formula=~arma(1,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res250=residuals(model250, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res250, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res250^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model250@fit$ics[1])
BIC_list=append(BIC_list, model250@fit$ics[2])
u250=psstd(res250, mean=0, sd=1, nu=tail(model250@fit$coef, n=1), xi=model250@fit$coef[length(model250@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u250, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u250, null="punif")$p.value)

model251=garchFit(formula=~arma(1,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res251=residuals(model251, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res251, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res251^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model251@fit$ics[1])
BIC_list=append(BIC_list, model251@fit$ics[2])
u251=psstd(res251, mean=0, sd=1, nu=tail(model251@fit$coef, n=1), xi=model251@fit$coef[length(model251@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u251, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u251, null="punif")$p.value)

model252=garchFit(formula=~arma(1,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res252=residuals(model252, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res252, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res252^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model252@fit$ics[1])
BIC_list=append(BIC_list, model252@fit$ics[2])
u252=psstd(res252, mean=0, sd=1, nu=tail(model252@fit$coef, n=1), xi=model252@fit$coef[length(model252@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u252, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u252, null="punif")$p.value)

model253=garchFit(formula=~arma(7,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res253=residuals(model253, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res253, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res253^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model253@fit$ics[1])
BIC_list=append(BIC_list, model253@fit$ics[2])
u253=psstd(res253, mean=0, sd=1, nu=tail(model253@fit$coef, n=1), xi=model253@fit$coef[length(model253@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u253, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u253, null="punif")$p.value)

model254=garchFit(formula=~arma(7,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res254=residuals(model254, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res254, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res254^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model254@fit$ics[1])
BIC_list=append(BIC_list, model254@fit$ics[2])
u254=psstd(res254, mean=0, sd=1, nu=tail(model254@fit$coef, n=1), xi=model254@fit$coef[length(model254@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u254, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u254, null="punif")$p.value)

model255=garchFit(formula=~arma(7,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res255=residuals(model255, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res255, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res255^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model255@fit$ics[1])
BIC_list=append(BIC_list, model255@fit$ics[2])
u255=psstd(res255, mean=0, sd=1, nu=tail(model255@fit$coef, n=1), xi=model255@fit$coef[length(model255@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u255, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u255, null="punif")$p.value)

model256=garchFit(formula=~arma(7,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res256=residuals(model256, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res256, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res256^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model256@fit$ics[1])
BIC_list=append(BIC_list, model256@fit$ics[2])
u256=psstd(res256, mean=0, sd=1, nu=tail(model256@fit$coef, n=1), xi=model256@fit$coef[length(model256@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u256, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u256, null="punif")$p.value)

model257=garchFit(formula=~arma(7,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res257=residuals(model257, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res257, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res257^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model257@fit$ics[1])
BIC_list=append(BIC_list, model257@fit$ics[2])
u257=psstd(res257, mean=0, sd=1, nu=tail(model257@fit$coef, n=1), xi=model257@fit$coef[length(model257@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u257, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u257, null="punif")$p.value)

model258=garchFit(formula=~arma(7,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res258=residuals(model258, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res258, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res258^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model258@fit$ics[1])
BIC_list=append(BIC_list, model258@fit$ics[2])
u258=psstd(res258, mean=0, sd=1, nu=tail(model258@fit$coef, n=1), xi=model258@fit$coef[length(model258@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u258, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u258, null="punif")$p.value)

model259=garchFit(formula=~arma(7,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res259=residuals(model259, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res259, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res259^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model259@fit$ics[1])
BIC_list=append(BIC_list, model259@fit$ics[2])
u259=psstd(res259, mean=0, sd=1, nu=tail(model259@fit$coef, n=1), xi=model259@fit$coef[length(model259@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u259, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u259, null="punif")$p.value)

model260=garchFit(formula=~arma(7,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res260=residuals(model260, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res260, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res260^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model260@fit$ics[1])
BIC_list=append(BIC_list, model260@fit$ics[2])
u260=psstd(res260, mean=0, sd=1, nu=tail(model260@fit$coef, n=1), xi=model260@fit$coef[length(model260@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u260, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u260, null="punif")$p.value)

model261=garchFit(formula=~arma(7,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res261=residuals(model261, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res261, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res261^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model261@fit$ics[1])
BIC_list=append(BIC_list, model261@fit$ics[2])
u261=psstd(res261, mean=0, sd=1, nu=tail(model261@fit$coef, n=1), xi=model261@fit$coef[length(model261@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u261, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u261, null="punif")$p.value)

model262=garchFit(formula=~arma(7,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res262=residuals(model262, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res262, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res262^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model262@fit$ics[1])
BIC_list=append(BIC_list, model262@fit$ics[2])
u262=psstd(res262, mean=0, sd=1, nu=tail(model262@fit$coef, n=1), xi=model262@fit$coef[length(model262@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u262, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u262, null="punif")$p.value)

model263=garchFit(formula=~arma(7,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res263=residuals(model263, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res263, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res263^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model263@fit$ics[1])
BIC_list=append(BIC_list, model263@fit$ics[2])
u263=psstd(res263, mean=0, sd=1, nu=tail(model263@fit$coef, n=1), xi=model263@fit$coef[length(model263@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u263, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u263, null="punif")$p.value)

model264=garchFit(formula=~arma(7,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res264=residuals(model264, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res264, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res264^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model264@fit$ics[1])
BIC_list=append(BIC_list, model264@fit$ics[2])
u264=psstd(res264, mean=0, sd=1, nu=tail(model264@fit$coef, n=1), xi=model264@fit$coef[length(model264@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u264, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u264, null="punif")$p.value)

model265=garchFit(formula=~arma(7,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res265=residuals(model265, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res265, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res265^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model265@fit$ics[1])
BIC_list=append(BIC_list, model265@fit$ics[2])
u265=psstd(res265, mean=0, sd=1, nu=tail(model265@fit$coef, n=1), xi=model265@fit$coef[length(model265@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u265, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u265, null="punif")$p.value)

model266=garchFit(formula=~arma(7,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res266=residuals(model266, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res266, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res266^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model266@fit$ics[1])
BIC_list=append(BIC_list, model266@fit$ics[2])
u266=psstd(res266, mean=0, sd=1, nu=tail(model266@fit$coef, n=1), xi=model266@fit$coef[length(model266@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u266, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u266, null="punif")$p.value)

model267=garchFit(formula=~arma(7,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res267=residuals(model267, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res267, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res267^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model267@fit$ics[1])
BIC_list=append(BIC_list, model267@fit$ics[2])
u267=psstd(res267, mean=0, sd=1, nu=tail(model267@fit$coef, n=1), xi=model267@fit$coef[length(model267@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u267, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u267, null="punif")$p.value)

model268=garchFit(formula=~arma(7,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res268=residuals(model268, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res268, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res268^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model268@fit$ics[1])
BIC_list=append(BIC_list, model268@fit$ics[2])
u268=psstd(res268, mean=0, sd=1, nu=tail(model268@fit$coef, n=1), xi=model268@fit$coef[length(model268@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u268, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u268, null="punif")$p.value)

model269=garchFit(formula=~arma(7,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res269=residuals(model269, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res269, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res269^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model269@fit$ics[1])
BIC_list=append(BIC_list, model269@fit$ics[2])
u269=psstd(res269, mean=0, sd=1, nu=tail(model269@fit$coef, n=1), xi=model269@fit$coef[length(model269@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u269, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u269, null="punif")$p.value)

model270=garchFit(formula=~arma(7,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res270=residuals(model270, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res270, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res270^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model270@fit$ics[1])
BIC_list=append(BIC_list, model270@fit$ics[2])
u270=psstd(res270, mean=0, sd=1, nu=tail(model270@fit$coef, n=1), xi=model270@fit$coef[length(model270@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u270, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u270, null="punif")$p.value)

model271=garchFit(formula=~arma(7,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res271=residuals(model271, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res271, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res271^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model271@fit$ics[1])
BIC_list=append(BIC_list, model271@fit$ics[2])
u271=psstd(res271, mean=0, sd=1, nu=tail(model271@fit$coef, n=1), xi=model271@fit$coef[length(model271@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u271, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u271, null="punif")$p.value)

model272=garchFit(formula=~arma(7,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res272=residuals(model272, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res272, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res272^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model272@fit$ics[1])
BIC_list=append(BIC_list, model272@fit$ics[2])
u272=psstd(res272, mean=0, sd=1, nu=tail(model272@fit$coef, n=1), xi=model272@fit$coef[length(model272@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u272, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u272, null="punif")$p.value)

model273=garchFit(formula=~arma(7,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res273=residuals(model273, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res273, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res273^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model273@fit$ics[1])
BIC_list=append(BIC_list, model273@fit$ics[2])
u273=psstd(res273, mean=0, sd=1, nu=tail(model273@fit$coef, n=1), xi=model273@fit$coef[length(model273@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u273, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u273, null="punif")$p.value)

model274=garchFit(formula=~arma(7,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res274=residuals(model274, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res274, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res274^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model274@fit$ics[1])
BIC_list=append(BIC_list, model274@fit$ics[2])
u274=psstd(res274, mean=0, sd=1, nu=tail(model274@fit$coef, n=1), xi=model274@fit$coef[length(model274@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u274, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u274, null="punif")$p.value)

model275=garchFit(formula=~arma(7,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res275=residuals(model275, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res275, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res275^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model275@fit$ics[1])
BIC_list=append(BIC_list, model275@fit$ics[2])
u275=psstd(res275, mean=0, sd=1, nu=tail(model275@fit$coef, n=1), xi=model275@fit$coef[length(model275@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u275, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u275, null="punif")$p.value)

model276=garchFit(formula=~arma(7,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res276=residuals(model276, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res276, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res276^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model276@fit$ics[1])
BIC_list=append(BIC_list, model276@fit$ics[2])
u276=psstd(res276, mean=0, sd=1, nu=tail(model276@fit$coef, n=1), xi=model276@fit$coef[length(model276@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u276, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u276, null="punif")$p.value)

model277=garchFit(formula=~arma(7,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res277=residuals(model277, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res277, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res277^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model277@fit$ics[1])
BIC_list=append(BIC_list, model277@fit$ics[2])
u277=psstd(res277, mean=0, sd=1, nu=tail(model277@fit$coef, n=1), xi=model277@fit$coef[length(model277@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u277, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u277, null="punif")$p.value)

model278=garchFit(formula=~arma(7,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res278=residuals(model278, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res278, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res278^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model278@fit$ics[1])
BIC_list=append(BIC_list, model278@fit$ics[2])
u278=psstd(res278, mean=0, sd=1, nu=tail(model278@fit$coef, n=1), xi=model278@fit$coef[length(model278@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u278, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u278, null="punif")$p.value)

model279=garchFit(formula=~arma(7,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res279=residuals(model279, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res279, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res279^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model279@fit$ics[1])
BIC_list=append(BIC_list, model279@fit$ics[2])
u279=psstd(res279, mean=0, sd=1, nu=tail(model279@fit$coef, n=1), xi=model279@fit$coef[length(model279@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u279, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u279, null="punif")$p.value)

model280=garchFit(formula=~arma(7,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="sstd")
res280=residuals(model280, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res280, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res280^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model280@fit$ics[1])
BIC_list=append(BIC_list, model280@fit$ics[2])
u280=psstd(res280, mean=0, sd=1, nu=tail(model280@fit$coef, n=1), xi=model280@fit$coef[length(model280@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u280, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u280, null="punif")$p.value)

model281=garchFit(formula=~arma(7,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="sstd")
res281=residuals(model281, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res281, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res281^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model281@fit$ics[1])
BIC_list=append(BIC_list, model281@fit$ics[2])
u281=psstd(res281, mean=0, sd=1, nu=tail(model281@fit$coef, n=1), xi=model281@fit$coef[length(model281@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u281, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u281, null="punif")$p.value)

model282=garchFit(formula=~arma(7,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="sstd")
res282=residuals(model282, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res282, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res282^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model282@fit$ics[1])
BIC_list=append(BIC_list, model282@fit$ics[2])
u282=psstd(res282, mean=0, sd=1, nu=tail(model282@fit$coef, n=1), xi=model282@fit$coef[length(model282@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u282, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u282, null="punif")$p.value)

model283=garchFit(formula=~arma(7,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="sstd")
res283=residuals(model283, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res283, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res283^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model283@fit$ics[1])
BIC_list=append(BIC_list, model283@fit$ics[2])
u283=psstd(res283, mean=0, sd=1, nu=tail(model283@fit$coef, n=1), xi=model283@fit$coef[length(model283@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u283, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u283, null="punif")$p.value)

model284=garchFit(formula=~arma(7,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="sstd")
res284=residuals(model284, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res284, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res284^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model284@fit$ics[1])
BIC_list=append(BIC_list, model284@fit$ics[2])
u284=psstd(res284, mean=0, sd=1, nu=tail(model284@fit$coef, n=1), xi=model284@fit$coef[length(model284@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u284, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u284, null="punif")$p.value)

model285=garchFit(formula=~arma(7,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="sstd")
res285=residuals(model285, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res285, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res285^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model285@fit$ics[1])
BIC_list=append(BIC_list, model285@fit$ics[2])
u285=psstd(res285, mean=0, sd=1, nu=tail(model285@fit$coef, n=1), xi=model285@fit$coef[length(model285@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u285, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u285, null="punif")$p.value)

model286=garchFit(formula=~arma(7,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="sstd")
res286=residuals(model286, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res286, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res286^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model286@fit$ics[1])
BIC_list=append(BIC_list, model286@fit$ics[2])
u286=psstd(res286, mean=0, sd=1, nu=tail(model286@fit$coef, n=1), xi=model286@fit$coef[length(model286@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u286, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u286, null="punif")$p.value)

model287=garchFit(formula=~arma(7,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="sstd")
res287=residuals(model287, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res287, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res287^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model287@fit$ics[1])
BIC_list=append(BIC_list, model287@fit$ics[2])
u287=psstd(res287, mean=0, sd=1, nu=tail(model287@fit$coef, n=1), xi=model287@fit$coef[length(model287@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u287, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u287, null="punif")$p.value)

model288=garchFit(formula=~arma(7,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="sstd")
res288=residuals(model288, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res288, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res288^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model288@fit$ics[1])
BIC_list=append(BIC_list, model288@fit$ics[2])
u288=psstd(res288, mean=0, sd=1, nu=tail(model288@fit$coef, n=1), xi=model288@fit$coef[length(model288@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u288, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u288, null="punif")$p.value)

model289=garchFit(formula=~arma(1,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res289=residuals(model289, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res289, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res289^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model289@fit$ics[1])
BIC_list=append(BIC_list, model289@fit$ics[2])
u289=psnorm(res289, mean=0, sd=1, xi=tail(model289@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u289, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u289, null="punif")$p.value)

model290=garchFit(formula=~arma(1,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res290=residuals(model290, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res290, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res290^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model290@fit$ics[1])
BIC_list=append(BIC_list, model290@fit$ics[2])
u290=psnorm(res290, mean=0, sd=1, xi=tail(model290@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u290, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u290, null="punif")$p.value)

model291=garchFit(formula=~arma(1,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res291=residuals(model291, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res291, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res291^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model291@fit$ics[1])
BIC_list=append(BIC_list, model291@fit$ics[2])
u291=psnorm(res291, mean=0, sd=1, xi=tail(model291@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u291, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u291, null="punif")$p.value)

model292=garchFit(formula=~arma(1,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res292=residuals(model292, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res292, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res292^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model292@fit$ics[1])
BIC_list=append(BIC_list, model292@fit$ics[2])
u292=psnorm(res292, mean=0, sd=1, xi=tail(model292@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u292, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u292, null="punif")$p.value)

model293=garchFit(formula=~arma(1,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res293=residuals(model293, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res293, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res293^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model293@fit$ics[1])
BIC_list=append(BIC_list, model293@fit$ics[2])
u293=psnorm(res293, mean=0, sd=1, xi=tail(model293@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u293, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u293, null="punif")$p.value)

model294=garchFit(formula=~arma(1,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res294=residuals(model294, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res294, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res294^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model294@fit$ics[1])
BIC_list=append(BIC_list, model294@fit$ics[2])
u294=psnorm(res294, mean=0, sd=1, xi=tail(model294@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u294, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u294, null="punif")$p.value)

model295=garchFit(formula=~arma(1,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res295=residuals(model295, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res295, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res295^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model295@fit$ics[1])
BIC_list=append(BIC_list, model295@fit$ics[2])
u295=psnorm(res295, mean=0, sd=1, xi=tail(model295@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u295, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u295, null="punif")$p.value)

model296=garchFit(formula=~arma(1,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res296=residuals(model296, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res296, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res296^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model296@fit$ics[1])
BIC_list=append(BIC_list, model296@fit$ics[2])
u296=psnorm(res296, mean=0, sd=1, xi=tail(model296@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u296, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u296, null="punif")$p.value)

model297=garchFit(formula=~arma(1,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res297=residuals(model297, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res297, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res297^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model297@fit$ics[1])
BIC_list=append(BIC_list, model297@fit$ics[2])
u297=psnorm(res297, mean=0, sd=1, xi=tail(model297@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u297, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u297, null="punif")$p.value)

model298=garchFit(formula=~arma(1,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res298=residuals(model298, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res298, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res298^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model298@fit$ics[1])
BIC_list=append(BIC_list, model298@fit$ics[2])
u298=psnorm(res298, mean=0, sd=1, xi=tail(model298@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u298, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u298, null="punif")$p.value)

model299=garchFit(formula=~arma(1,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res299=residuals(model299, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res299, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res299^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model299@fit$ics[1])
BIC_list=append(BIC_list, model299@fit$ics[2])
u299=psnorm(res299, mean=0, sd=1, xi=tail(model299@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u299, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u299, null="punif")$p.value)

model300=garchFit(formula=~arma(1,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res300=residuals(model300, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res300, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res300^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model300@fit$ics[1])
BIC_list=append(BIC_list, model300@fit$ics[2])
u300=psnorm(res300, mean=0, sd=1, xi=tail(model300@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u300, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u300, null="punif")$p.value)

model301=garchFit(formula=~arma(1,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res301=residuals(model301, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res301, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res301^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model301@fit$ics[1])
BIC_list=append(BIC_list, model301@fit$ics[2])
u301=psnorm(res301, mean=0, sd=1, xi=tail(model301@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u301, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u301, null="punif")$p.value)

model302=garchFit(formula=~arma(1,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res302=residuals(model302, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res302, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res302^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model302@fit$ics[1])
BIC_list=append(BIC_list, model302@fit$ics[2])
u302=psnorm(res302, mean=0, sd=1, xi=tail(model302@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u302, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u302, null="punif")$p.value)

model303=garchFit(formula=~arma(1,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res303=residuals(model303, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res303, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res303^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model303@fit$ics[1])
BIC_list=append(BIC_list, model303@fit$ics[2])
u303=psnorm(res303, mean=0, sd=1, xi=tail(model303@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u303, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u303, null="punif")$p.value)

model304=garchFit(formula=~arma(1,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res304=residuals(model304, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res304, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res304^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model304@fit$ics[1])
BIC_list=append(BIC_list, model304@fit$ics[2])
u304=psnorm(res304, mean=0, sd=1, xi=tail(model304@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u304, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u304, null="punif")$p.value)

model305=garchFit(formula=~arma(1,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res305=residuals(model305, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res305, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res305^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model305@fit$ics[1])
BIC_list=append(BIC_list, model305@fit$ics[2])
u305=psnorm(res305, mean=0, sd=1, xi=tail(model305@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u305, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u305, null="punif")$p.value)

model306=garchFit(formula=~arma(1,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res306=residuals(model306, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res306, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res306^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model306@fit$ics[1])
BIC_list=append(BIC_list, model306@fit$ics[2])
u306=psnorm(res306, mean=0, sd=1, xi=tail(model306@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u306, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u306, null="punif")$p.value)

model307=garchFit(formula=~arma(1,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res307=residuals(model307, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res307, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res307^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model307@fit$ics[1])
BIC_list=append(BIC_list, model307@fit$ics[2])
u307=psnorm(res307, mean=0, sd=1, xi=tail(model307@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u307, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u307, null="punif")$p.value)

model308=garchFit(formula=~arma(1,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res308=residuals(model308, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res308, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res308^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model308@fit$ics[1])
BIC_list=append(BIC_list, model308@fit$ics[2])
u308=psnorm(res308, mean=0, sd=1, xi=tail(model308@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u308, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u308, null="punif")$p.value)

model309=garchFit(formula=~arma(1,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res309=residuals(model309, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res309, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res309^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model309@fit$ics[1])
BIC_list=append(BIC_list, model309@fit$ics[2])
u309=psnorm(res309, mean=0, sd=1, xi=tail(model309@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u309, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u309, null="punif")$p.value)

model310=garchFit(formula=~arma(1,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res310=residuals(model310, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res310, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res310^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model310@fit$ics[1])
BIC_list=append(BIC_list, model310@fit$ics[2])
u310=psnorm(res310, mean=0, sd=1, xi=tail(model310@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u310, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u310, null="punif")$p.value)

model311=garchFit(formula=~arma(1,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res311=residuals(model311, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res311, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res311^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model311@fit$ics[1])
BIC_list=append(BIC_list, model311@fit$ics[2])
u311=psnorm(res311, mean=0, sd=1, xi=tail(model311@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u311, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u311, null="punif")$p.value)

model312=garchFit(formula=~arma(1,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res312=residuals(model312, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res312, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res312^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model312@fit$ics[1])
BIC_list=append(BIC_list, model312@fit$ics[2])
u312=psnorm(res312, mean=0, sd=1, xi=tail(model312@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u312, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u312, null="punif")$p.value)

model313=garchFit(formula=~arma(1,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res313=residuals(model313, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res313, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res313^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model313@fit$ics[1])
BIC_list=append(BIC_list, model313@fit$ics[2])
u313=psnorm(res313, mean=0, sd=1, xi=tail(model313@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u313, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u313, null="punif")$p.value)

model314=garchFit(formula=~arma(1,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res314=residuals(model314, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res314, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res314^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model314@fit$ics[1])
BIC_list=append(BIC_list, model314@fit$ics[2])
u314=psnorm(res314, mean=0, sd=1, xi=tail(model314@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u314, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u314, null="punif")$p.value)

model315=garchFit(formula=~arma(1,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res315=residuals(model315, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res315, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res315^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model315@fit$ics[1])
BIC_list=append(BIC_list, model315@fit$ics[2])
u315=psnorm(res315, mean=0, sd=1, xi=tail(model315@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u315, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u315, null="punif")$p.value)

model316=garchFit(formula=~arma(1,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res316=residuals(model316, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res316, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res316^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model316@fit$ics[1])
BIC_list=append(BIC_list, model316@fit$ics[2])
u316=psnorm(res316, mean=0, sd=1, xi=tail(model316@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u316, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u316, null="punif")$p.value)

model317=garchFit(formula=~arma(1,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res317=residuals(model317, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res317, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res317^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model317@fit$ics[1])
BIC_list=append(BIC_list, model317@fit$ics[2])
u317=psnorm(res317, mean=0, sd=1, xi=tail(model317@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u317, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u317, null="punif")$p.value)

model318=garchFit(formula=~arma(1,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res318=residuals(model318, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res318, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res318^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model318@fit$ics[1])
BIC_list=append(BIC_list, model318@fit$ics[2])
u318=psnorm(res318, mean=0, sd=1, xi=tail(model318@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u318, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u318, null="punif")$p.value)

model319=garchFit(formula=~arma(1,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res319=residuals(model319, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res319, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res319^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model319@fit$ics[1])
BIC_list=append(BIC_list, model319@fit$ics[2])
u319=psnorm(res319, mean=0, sd=1, xi=tail(model319@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u319, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u319, null="punif")$p.value)

model320=garchFit(formula=~arma(1,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res320=residuals(model320, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res320, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res320^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model320@fit$ics[1])
BIC_list=append(BIC_list, model320@fit$ics[2])
u320=psnorm(res320, mean=0, sd=1, xi=tail(model320@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u320, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u320, null="punif")$p.value)

model321=garchFit(formula=~arma(1,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res321=residuals(model321, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res321, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res321^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model321@fit$ics[1])
BIC_list=append(BIC_list, model321@fit$ics[2])
u321=psnorm(res321, mean=0, sd=1, xi=tail(model321@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u321, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u321, null="punif")$p.value)

model322=garchFit(formula=~arma(1,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res322=residuals(model322, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res322, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res322^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model322@fit$ics[1])
BIC_list=append(BIC_list, model322@fit$ics[2])
u322=psnorm(res322, mean=0, sd=1, xi=tail(model322@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u322, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u322, null="punif")$p.value)

model323=garchFit(formula=~arma(1,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res323=residuals(model323, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res323, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res323^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model323@fit$ics[1])
BIC_list=append(BIC_list, model323@fit$ics[2])
u323=psnorm(res323, mean=0, sd=1, xi=tail(model323@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u323, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u323, null="punif")$p.value)

model324=garchFit(formula=~arma(1,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res324=residuals(model324, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res324, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res324^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model324@fit$ics[1])
BIC_list=append(BIC_list, model324@fit$ics[2])
u324=psnorm(res324, mean=0, sd=1, xi=tail(model324@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u324, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u324, null="punif")$p.value)

model325=garchFit(formula=~arma(7,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res325=residuals(model325, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res325, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res325^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model325@fit$ics[1])
BIC_list=append(BIC_list, model325@fit$ics[2])
u325=psnorm(res325, mean=0, sd=1, xi=tail(model325@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u325, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u325, null="punif")$p.value)

model326=garchFit(formula=~arma(7,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res326=residuals(model326, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res326, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res326^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model326@fit$ics[1])
BIC_list=append(BIC_list, model326@fit$ics[2])
u326=psnorm(res326, mean=0, sd=1, xi=tail(model326@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u326, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u326, null="punif")$p.value)

model327=garchFit(formula=~arma(7,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res327=residuals(model327, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res327, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res327^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model327@fit$ics[1])
BIC_list=append(BIC_list, model327@fit$ics[2])
u327=psnorm(res327, mean=0, sd=1, xi=tail(model327@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u327, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u327, null="punif")$p.value)

model328=garchFit(formula=~arma(7,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res328=residuals(model328, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res328, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res328^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model328@fit$ics[1])
BIC_list=append(BIC_list, model328@fit$ics[2])
u328=psnorm(res328, mean=0, sd=1, xi=tail(model328@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u328, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u328, null="punif")$p.value)

model329=garchFit(formula=~arma(7,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res329=residuals(model329, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res329, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res329^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model329@fit$ics[1])
BIC_list=append(BIC_list, model329@fit$ics[2])
u329=psnorm(res329, mean=0, sd=1, xi=tail(model329@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u329, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u329, null="punif")$p.value)

model330=garchFit(formula=~arma(7,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res330=residuals(model330, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res330, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res330^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model330@fit$ics[1])
BIC_list=append(BIC_list, model330@fit$ics[2])
u330=psnorm(res330, mean=0, sd=1, xi=tail(model330@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u330, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u330, null="punif")$p.value)

model331=garchFit(formula=~arma(7,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res331=residuals(model331, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res331, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res331^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model331@fit$ics[1])
BIC_list=append(BIC_list, model331@fit$ics[2])
u331=psnorm(res331, mean=0, sd=1, xi=tail(model331@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u331, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u331, null="punif")$p.value)

model332=garchFit(formula=~arma(7,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res332=residuals(model332, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res332, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res332^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model332@fit$ics[1])
BIC_list=append(BIC_list, model332@fit$ics[2])
u332=psnorm(res332, mean=0, sd=1, xi=tail(model332@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u332, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u332, null="punif")$p.value)

model333=garchFit(formula=~arma(7,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res333=residuals(model333, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res333, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res333^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model333@fit$ics[1])
BIC_list=append(BIC_list, model333@fit$ics[2])
u333=psnorm(res333, mean=0, sd=1, xi=tail(model333@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u333, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u333, null="punif")$p.value)

model334=garchFit(formula=~arma(7,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res334=residuals(model334, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res334, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res334^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model334@fit$ics[1])
BIC_list=append(BIC_list, model334@fit$ics[2])
u334=psnorm(res334, mean=0, sd=1, xi=tail(model334@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u334, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u334, null="punif")$p.value)

model335=garchFit(formula=~arma(7,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res335=residuals(model335, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res335, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res335^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model335@fit$ics[1])
BIC_list=append(BIC_list, model335@fit$ics[2])
u335=psnorm(res335, mean=0, sd=1, xi=tail(model335@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u335, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u335, null="punif")$p.value)

model336=garchFit(formula=~arma(7,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res336=residuals(model336, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res336, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res336^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model336@fit$ics[1])
BIC_list=append(BIC_list, model336@fit$ics[2])
u336=psnorm(res336, mean=0, sd=1, xi=tail(model336@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u336, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u336, null="punif")$p.value)

model337=garchFit(formula=~arma(7,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res337=residuals(model337, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res337, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res337^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model337@fit$ics[1])
BIC_list=append(BIC_list, model337@fit$ics[2])
u337=psnorm(res337, mean=0, sd=1, xi=tail(model337@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u337, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u337, null="punif")$p.value)

model338=garchFit(formula=~arma(7,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res338=residuals(model338, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res338, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res338^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model338@fit$ics[1])
BIC_list=append(BIC_list, model338@fit$ics[2])
u338=psnorm(res338, mean=0, sd=1, xi=tail(model338@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u338, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u338, null="punif")$p.value)

model339=garchFit(formula=~arma(7,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res339=residuals(model339, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res339, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res339^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model339@fit$ics[1])
BIC_list=append(BIC_list, model339@fit$ics[2])
u339=psnorm(res339, mean=0, sd=1, xi=tail(model339@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u339, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u339, null="punif")$p.value)

model340=garchFit(formula=~arma(7,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res340=residuals(model340, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res340, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res340^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model340@fit$ics[1])
BIC_list=append(BIC_list, model340@fit$ics[2])
u340=psnorm(res340, mean=0, sd=1, xi=tail(model340@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u340, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u340, null="punif")$p.value)

model341=garchFit(formula=~arma(7,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res341=residuals(model341, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res341, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res341^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model341@fit$ics[1])
BIC_list=append(BIC_list, model341@fit$ics[2])
u341=psnorm(res341, mean=0, sd=1, xi=tail(model341@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u341, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u341, null="punif")$p.value)

model342=garchFit(formula=~arma(7,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res342=residuals(model342, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res342, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res342^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model342@fit$ics[1])
BIC_list=append(BIC_list, model342@fit$ics[2])
u342=psnorm(res342, mean=0, sd=1, xi=tail(model342@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u342, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u342, null="punif")$p.value)

model343=garchFit(formula=~arma(7,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res343=residuals(model343, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res343, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res343^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model343@fit$ics[1])
BIC_list=append(BIC_list, model343@fit$ics[2])
u343=psnorm(res343, mean=0, sd=1, xi=tail(model343@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u343, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u343, null="punif")$p.value)

model344=garchFit(formula=~arma(7,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res344=residuals(model344, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res344, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res344^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model344@fit$ics[1])
BIC_list=append(BIC_list, model344@fit$ics[2])
u344=psnorm(res344, mean=0, sd=1, xi=tail(model344@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u344, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u344, null="punif")$p.value)

model345=garchFit(formula=~arma(7,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res345=residuals(model345, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res345, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res345^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model345@fit$ics[1])
BIC_list=append(BIC_list, model345@fit$ics[2])
u345=psnorm(res345, mean=0, sd=1, xi=tail(model345@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u345, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u345, null="punif")$p.value)

model346=garchFit(formula=~arma(7,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res346=residuals(model346, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res346, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res346^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model346@fit$ics[1])
BIC_list=append(BIC_list, model346@fit$ics[2])
u346=psnorm(res346, mean=0, sd=1, xi=tail(model346@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u346, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u346, null="punif")$p.value)

model347=garchFit(formula=~arma(7,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res347=residuals(model347, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res347, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res347^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model347@fit$ics[1])
BIC_list=append(BIC_list, model347@fit$ics[2])
u347=psnorm(res347, mean=0, sd=1, xi=tail(model347@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u347, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u347, null="punif")$p.value)

model348=garchFit(formula=~arma(7,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res348=residuals(model348, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res348, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res348^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model348@fit$ics[1])
BIC_list=append(BIC_list, model348@fit$ics[2])
u348=psnorm(res348, mean=0, sd=1, xi=tail(model348@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u348, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u348, null="punif")$p.value)

model349=garchFit(formula=~arma(7,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res349=residuals(model349, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res349, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res349^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model349@fit$ics[1])
BIC_list=append(BIC_list, model349@fit$ics[2])
u349=psnorm(res349, mean=0, sd=1, xi=tail(model349@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u349, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u349, null="punif")$p.value)

model350=garchFit(formula=~arma(7,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res350=residuals(model350, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res350, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res350^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model350@fit$ics[1])
BIC_list=append(BIC_list, model350@fit$ics[2])
u350=psnorm(res350, mean=0, sd=1, xi=tail(model350@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u350, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u350, null="punif")$p.value)

model351=garchFit(formula=~arma(7,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res351=residuals(model351, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res351, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res351^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model351@fit$ics[1])
BIC_list=append(BIC_list, model351@fit$ics[2])
u351=psnorm(res351, mean=0, sd=1, xi=tail(model351@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u351, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u351, null="punif")$p.value)

model352=garchFit(formula=~arma(7,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="snorm")
res352=residuals(model352, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res352, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res352^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model352@fit$ics[1])
BIC_list=append(BIC_list, model352@fit$ics[2])
u352=psnorm(res352, mean=0, sd=1, xi=tail(model352@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u352, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u352, null="punif")$p.value)

model353=garchFit(formula=~arma(7,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="snorm")
res353=residuals(model353, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res353, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res353^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model353@fit$ics[1])
BIC_list=append(BIC_list, model353@fit$ics[2])
u353=psnorm(res353, mean=0, sd=1, xi=tail(model353@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u353, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u353, null="punif")$p.value)

model354=garchFit(formula=~arma(7,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="snorm")
res354=residuals(model354, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res354, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res354^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model354@fit$ics[1])
BIC_list=append(BIC_list, model354@fit$ics[2])
u354=psnorm(res354, mean=0, sd=1, xi=tail(model354@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u354, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u354, null="punif")$p.value)

model355=garchFit(formula=~arma(7,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="snorm")
res355=residuals(model355, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res355, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res355^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model355@fit$ics[1])
BIC_list=append(BIC_list, model355@fit$ics[2])
u355=psnorm(res355, mean=0, sd=1, xi=tail(model355@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u355, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u355, null="punif")$p.value)

model356=garchFit(formula=~arma(7,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="snorm")
res356=residuals(model356, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res356, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res356^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model356@fit$ics[1])
BIC_list=append(BIC_list, model356@fit$ics[2])
u356=psnorm(res356, mean=0, sd=1, xi=tail(model356@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u356, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u356, null="punif")$p.value)

model357=garchFit(formula=~arma(7,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="snorm")
res357=residuals(model357, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res357, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res357^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model357@fit$ics[1])
BIC_list=append(BIC_list, model357@fit$ics[2])
u357=psnorm(res357, mean=0, sd=1, xi=tail(model357@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u357, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u357, null="punif")$p.value)

model358=garchFit(formula=~arma(7,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="snorm")
res358=residuals(model358, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res358, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res358^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model358@fit$ics[1])
BIC_list=append(BIC_list, model358@fit$ics[2])
u358=psnorm(res358, mean=0, sd=1, xi=tail(model358@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u358, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u358, null="punif")$p.value)

model359=garchFit(formula=~arma(7,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="snorm")
res359=residuals(model359, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res359, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res359^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model359@fit$ics[1])
BIC_list=append(BIC_list, model359@fit$ics[2])
u359=psnorm(res359, mean=0, sd=1, xi=tail(model359@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u359, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u359, null="punif")$p.value)

model360=garchFit(formula=~arma(7,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="snorm")
res360=residuals(model360, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res360, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res360^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model360@fit$ics[1])
BIC_list=append(BIC_list, model360@fit$ics[2])
u360=psnorm(res360, mean=0, sd=1, xi=tail(model360@fit$coef, n=1))[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u360, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u360, null="punif")$p.value)

model361=garchFit(formula=~arma(1,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res361=residuals(model361, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res361, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res361^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model361@fit$ics[1])
BIC_list=append(BIC_list, model361@fit$ics[2])
u361=psged(res361, mean=0, sd=1, nu=tail(model361@fit$coef, n=1), xi=model361@fit$coef[length(model361@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u361, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u361, null="punif")$p.value)

model362=garchFit(formula=~arma(1,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res362=residuals(model362, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res362, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res362^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model362@fit$ics[1])
BIC_list=append(BIC_list, model362@fit$ics[2])
u362=psged(res362, mean=0, sd=1, nu=tail(model362@fit$coef, n=1), xi=model362@fit$coef[length(model362@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u362, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u362, null="punif")$p.value)

model363=garchFit(formula=~arma(1,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res363=residuals(model363, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res363, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res363^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model363@fit$ics[1])
BIC_list=append(BIC_list, model363@fit$ics[2])
u363=psged(res363, mean=0, sd=1, nu=tail(model363@fit$coef, n=1), xi=model363@fit$coef[length(model363@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u363, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u363, null="punif")$p.value)

model364=garchFit(formula=~arma(1,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res364=residuals(model364, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res364, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res364^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model364@fit$ics[1])
BIC_list=append(BIC_list, model364@fit$ics[2])
u364=psged(res364, mean=0, sd=1, nu=tail(model364@fit$coef, n=1), xi=model364@fit$coef[length(model364@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u364, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u364, null="punif")$p.value)

model365=garchFit(formula=~arma(1,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res365=residuals(model365, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res365, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res365^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model365@fit$ics[1])
BIC_list=append(BIC_list, model365@fit$ics[2])
u365=psged(res365, mean=0, sd=1, nu=tail(model365@fit$coef, n=1), xi=model365@fit$coef[length(model365@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u365, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u365, null="punif")$p.value)

model366=garchFit(formula=~arma(1,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res366=residuals(model366, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res366, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res366^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model366@fit$ics[1])
BIC_list=append(BIC_list, model366@fit$ics[2])
u366=psged(res366, mean=0, sd=1, nu=tail(model366@fit$coef, n=1), xi=model366@fit$coef[length(model366@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u366, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u366, null="punif")$p.value)

model367=garchFit(formula=~arma(1,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res367=residuals(model367, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res367, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res367^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model367@fit$ics[1])
BIC_list=append(BIC_list, model367@fit$ics[2])
u367=psged(res367, mean=0, sd=1, nu=tail(model367@fit$coef, n=1), xi=model367@fit$coef[length(model367@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u367, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u367, null="punif")$p.value)

model368=garchFit(formula=~arma(1,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res368=residuals(model368, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res368, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res368^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model368@fit$ics[1])
BIC_list=append(BIC_list, model368@fit$ics[2])
u368=psged(res368, mean=0, sd=1, nu=tail(model368@fit$coef, n=1), xi=model368@fit$coef[length(model368@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u368, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u368, null="punif")$p.value)

model369=garchFit(formula=~arma(1,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res369=residuals(model369, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res369, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res369^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model369@fit$ics[1])
BIC_list=append(BIC_list, model369@fit$ics[2])
u369=psged(res369, mean=0, sd=1, nu=tail(model369@fit$coef, n=1), xi=model369@fit$coef[length(model369@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u369, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u369, null="punif")$p.value)

model370=garchFit(formula=~arma(1,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res370=residuals(model370, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res370, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res370^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model370@fit$ics[1])
BIC_list=append(BIC_list, model370@fit$ics[2])
u370=psged(res370, mean=0, sd=1, nu=tail(model370@fit$coef, n=1), xi=model370@fit$coef[length(model370@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u370, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u370, null="punif")$p.value)

model371=garchFit(formula=~arma(1,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res371=residuals(model371, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res371, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res371^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model371@fit$ics[1])
BIC_list=append(BIC_list, model371@fit$ics[2])
u371=psged(res371, mean=0, sd=1, nu=tail(model371@fit$coef, n=1), xi=model371@fit$coef[length(model371@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u371, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u371, null="punif")$p.value)

model372=garchFit(formula=~arma(1,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res372=residuals(model372, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res372, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res372^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model372@fit$ics[1])
BIC_list=append(BIC_list, model372@fit$ics[2])
u372=psged(res372, mean=0, sd=1, nu=tail(model372@fit$coef, n=1), xi=model372@fit$coef[length(model372@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u372, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u372, null="punif")$p.value)

model373=garchFit(formula=~arma(1,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res373=residuals(model373, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res373, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res373^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model373@fit$ics[1])
BIC_list=append(BIC_list, model373@fit$ics[2])
u373=psged(res373, mean=0, sd=1, nu=tail(model373@fit$coef, n=1), xi=model373@fit$coef[length(model373@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u373, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u373, null="punif")$p.value)

model374=garchFit(formula=~arma(1,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res374=residuals(model374, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res374, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res374^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model374@fit$ics[1])
BIC_list=append(BIC_list, model374@fit$ics[2])
u374=psged(res374, mean=0, sd=1, nu=tail(model374@fit$coef, n=1), xi=model374@fit$coef[length(model374@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u374, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u374, null="punif")$p.value)

model375=garchFit(formula=~arma(1,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res375=residuals(model375, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res375, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res375^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model375@fit$ics[1])
BIC_list=append(BIC_list, model375@fit$ics[2])
u375=psged(res375, mean=0, sd=1, nu=tail(model375@fit$coef, n=1), xi=model375@fit$coef[length(model375@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u375, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u375, null="punif")$p.value)

model376=garchFit(formula=~arma(1,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res376=residuals(model376, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res376, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res376^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model376@fit$ics[1])
BIC_list=append(BIC_list, model376@fit$ics[2])
u376=psged(res376, mean=0, sd=1, nu=tail(model376@fit$coef, n=1), xi=model376@fit$coef[length(model376@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u376, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u376, null="punif")$p.value)

model377=garchFit(formula=~arma(1,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res377=residuals(model377, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res377, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res377^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model377@fit$ics[1])
BIC_list=append(BIC_list, model377@fit$ics[2])
u377=psged(res377, mean=0, sd=1, nu=tail(model377@fit$coef, n=1), xi=model377@fit$coef[length(model377@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u377, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u377, null="punif")$p.value)

model378=garchFit(formula=~arma(1,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res378=residuals(model378, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res378, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res378^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model378@fit$ics[1])
BIC_list=append(BIC_list, model378@fit$ics[2])
u378=psged(res378, mean=0, sd=1, nu=tail(model378@fit$coef, n=1), xi=model378@fit$coef[length(model378@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u378, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u378, null="punif")$p.value)

model379=garchFit(formula=~arma(1,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res379=residuals(model379, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res379, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res379^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model379@fit$ics[1])
BIC_list=append(BIC_list, model379@fit$ics[2])
u379=psged(res379, mean=0, sd=1, nu=tail(model379@fit$coef, n=1), xi=model379@fit$coef[length(model379@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u379, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u379, null="punif")$p.value)

model380=garchFit(formula=~arma(1,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res380=residuals(model380, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res380, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res380^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model380@fit$ics[1])
BIC_list=append(BIC_list, model380@fit$ics[2])
u380=psged(res380, mean=0, sd=1, nu=tail(model380@fit$coef, n=1), xi=model380@fit$coef[length(model380@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u380, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u380, null="punif")$p.value)

model381=garchFit(formula=~arma(1,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res381=residuals(model381, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res381, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res381^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model381@fit$ics[1])
BIC_list=append(BIC_list, model381@fit$ics[2])
u381=psged(res381, mean=0, sd=1, nu=tail(model381@fit$coef, n=1), xi=model381@fit$coef[length(model381@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u381, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u381, null="punif")$p.value)

model382=garchFit(formula=~arma(1,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res382=residuals(model382, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res382, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res382^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model382@fit$ics[1])
BIC_list=append(BIC_list, model382@fit$ics[2])
u382=psged(res382, mean=0, sd=1, nu=tail(model382@fit$coef, n=1), xi=model382@fit$coef[length(model382@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u382, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u382, null="punif")$p.value)

model383=garchFit(formula=~arma(1,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res383=residuals(model383, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res383, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res383^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model383@fit$ics[1])
BIC_list=append(BIC_list, model383@fit$ics[2])
u383=psged(res383, mean=0, sd=1, nu=tail(model383@fit$coef, n=1), xi=model383@fit$coef[length(model383@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u383, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u383, null="punif")$p.value)

model384=garchFit(formula=~arma(1,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res384=residuals(model384, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res384, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res384^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model384@fit$ics[1])
BIC_list=append(BIC_list, model384@fit$ics[2])
u384=psged(res384, mean=0, sd=1, nu=tail(model384@fit$coef, n=1), xi=model384@fit$coef[length(model384@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u384, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u384, null="punif")$p.value)

model385=garchFit(formula=~arma(1,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res385=residuals(model385, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res385, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res385^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model385@fit$ics[1])
BIC_list=append(BIC_list, model385@fit$ics[2])
u385=psged(res385, mean=0, sd=1, nu=tail(model385@fit$coef, n=1), xi=model385@fit$coef[length(model385@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u385, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u385, null="punif")$p.value)

model386=garchFit(formula=~arma(1,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res386=residuals(model386, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res386, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res386^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model386@fit$ics[1])
BIC_list=append(BIC_list, model386@fit$ics[2])
u386=psged(res386, mean=0, sd=1, nu=tail(model386@fit$coef, n=1), xi=model386@fit$coef[length(model386@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u386, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u386, null="punif")$p.value)

model387=garchFit(formula=~arma(1,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res387=residuals(model387, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res387, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res387^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model387@fit$ics[1])
BIC_list=append(BIC_list, model387@fit$ics[2])
u387=psged(res387, mean=0, sd=1, nu=tail(model387@fit$coef, n=1), xi=model387@fit$coef[length(model387@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u387, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u387, null="punif")$p.value)

model388=garchFit(formula=~arma(1,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res388=residuals(model388, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res388, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res388^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model388@fit$ics[1])
BIC_list=append(BIC_list, model388@fit$ics[2])
u388=psged(res388, mean=0, sd=1, nu=tail(model388@fit$coef, n=1), xi=model388@fit$coef[length(model388@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u388, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u388, null="punif")$p.value)

model389=garchFit(formula=~arma(1,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res389=residuals(model389, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res389, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res389^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model389@fit$ics[1])
BIC_list=append(BIC_list, model389@fit$ics[2])
u389=psged(res389, mean=0, sd=1, nu=tail(model389@fit$coef, n=1), xi=model389@fit$coef[length(model389@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u389, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u389, null="punif")$p.value)

model390=garchFit(formula=~arma(1,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res390=residuals(model390, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res390, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res390^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model390@fit$ics[1])
BIC_list=append(BIC_list, model390@fit$ics[2])
u390=psged(res390, mean=0, sd=1, nu=tail(model390@fit$coef, n=1), xi=model390@fit$coef[length(model390@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u390, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u390, null="punif")$p.value)

model391=garchFit(formula=~arma(1,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res391=residuals(model391, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res391, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res391^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model391@fit$ics[1])
BIC_list=append(BIC_list, model391@fit$ics[2])
u391=psged(res391, mean=0, sd=1, nu=tail(model391@fit$coef, n=1), xi=model391@fit$coef[length(model391@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u391, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u391, null="punif")$p.value)

model392=garchFit(formula=~arma(1,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res392=residuals(model392, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res392, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res392^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model392@fit$ics[1])
BIC_list=append(BIC_list, model392@fit$ics[2])
u392=psged(res392, mean=0, sd=1, nu=tail(model392@fit$coef, n=1), xi=model392@fit$coef[length(model392@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u392, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u392, null="punif")$p.value)

model393=garchFit(formula=~arma(1,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res393=residuals(model393, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res393, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res393^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model393@fit$ics[1])
BIC_list=append(BIC_list, model393@fit$ics[2])
u393=psged(res393, mean=0, sd=1, nu=tail(model393@fit$coef, n=1), xi=model393@fit$coef[length(model393@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u393, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u393, null="punif")$p.value)

model394=garchFit(formula=~arma(1,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res394=residuals(model394, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res394, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res394^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model394@fit$ics[1])
BIC_list=append(BIC_list, model394@fit$ics[2])
u394=psged(res394, mean=0, sd=1, nu=tail(model394@fit$coef, n=1), xi=model394@fit$coef[length(model394@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u394, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u394, null="punif")$p.value)

model395=garchFit(formula=~arma(1,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res395=residuals(model395, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res395, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res395^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model395@fit$ics[1])
BIC_list=append(BIC_list, model395@fit$ics[2])
u395=psged(res395, mean=0, sd=1, nu=tail(model395@fit$coef, n=1), xi=model395@fit$coef[length(model395@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u395, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u395, null="punif")$p.value)

model396=garchFit(formula=~arma(1,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res396=residuals(model396, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res396, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res396^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model396@fit$ics[1])
BIC_list=append(BIC_list, model396@fit$ics[2])
u396=psged(res396, mean=0, sd=1, nu=tail(model396@fit$coef, n=1), xi=model396@fit$coef[length(model396@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u396, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u396, null="punif")$p.value)

model397=garchFit(formula=~arma(7,0)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res397=residuals(model397, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res397, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res397^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model397@fit$ics[1])
BIC_list=append(BIC_list, model397@fit$ics[2])
u397=psged(res397, mean=0, sd=1, nu=tail(model397@fit$coef, n=1), xi=model397@fit$coef[length(model397@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u397, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u397, null="punif")$p.value)

model398=garchFit(formula=~arma(7,0)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res398=residuals(model398, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res398, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res398^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model398@fit$ics[1])
BIC_list=append(BIC_list, model398@fit$ics[2])
u398=psged(res398, mean=0, sd=1, nu=tail(model398@fit$coef, n=1), xi=model398@fit$coef[length(model398@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u398, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u398, null="punif")$p.value)

model399=garchFit(formula=~arma(7,0)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res399=residuals(model399, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res399, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res399^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model399@fit$ics[1])
BIC_list=append(BIC_list, model399@fit$ics[2])
u399=psged(res399, mean=0, sd=1, nu=tail(model399@fit$coef, n=1), xi=model399@fit$coef[length(model399@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u399, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u399, null="punif")$p.value)

model400=garchFit(formula=~arma(7,0)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res400=residuals(model400, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res400, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res400^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model400@fit$ics[1])
BIC_list=append(BIC_list, model400@fit$ics[2])
u400=psged(res400, mean=0, sd=1, nu=tail(model400@fit$coef, n=1), xi=model400@fit$coef[length(model400@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u400, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u400, null="punif")$p.value)

model401=garchFit(formula=~arma(7,0)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res401=residuals(model401, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res401, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res401^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model401@fit$ics[1])
BIC_list=append(BIC_list, model401@fit$ics[2])
u401=psged(res401, mean=0, sd=1, nu=tail(model401@fit$coef, n=1), xi=model401@fit$coef[length(model401@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u401, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u401, null="punif")$p.value)

model402=garchFit(formula=~arma(7,0)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res402=residuals(model402, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res402, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res402^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model402@fit$ics[1])
BIC_list=append(BIC_list, model402@fit$ics[2])
u402=psged(res402, mean=0, sd=1, nu=tail(model402@fit$coef, n=1), xi=model402@fit$coef[length(model402@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u402, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u402, null="punif")$p.value)

model403=garchFit(formula=~arma(7,0)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res403=residuals(model403, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res403, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res403^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model403@fit$ics[1])
BIC_list=append(BIC_list, model403@fit$ics[2])
u403=psged(res403, mean=0, sd=1, nu=tail(model403@fit$coef, n=1), xi=model403@fit$coef[length(model403@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u403, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u403, null="punif")$p.value)

model404=garchFit(formula=~arma(7,0)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res404=residuals(model404, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res404, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res404^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model404@fit$ics[1])
BIC_list=append(BIC_list, model404@fit$ics[2])
u404=psged(res404, mean=0, sd=1, nu=tail(model404@fit$coef, n=1), xi=model404@fit$coef[length(model404@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u404, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u404, null="punif")$p.value)

model405=garchFit(formula=~arma(7,0)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res405=residuals(model405, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res405, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res405^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model405@fit$ics[1])
BIC_list=append(BIC_list, model405@fit$ics[2])
u405=psged(res405, mean=0, sd=1, nu=tail(model405@fit$coef, n=1), xi=model405@fit$coef[length(model405@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u405, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u405, null="punif")$p.value)

model406=garchFit(formula=~arma(7,1)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res406=residuals(model406, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res406, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res406^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model406@fit$ics[1])
BIC_list=append(BIC_list, model406@fit$ics[2])
u406=psged(res406, mean=0, sd=1, nu=tail(model406@fit$coef, n=1), xi=model406@fit$coef[length(model406@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u406, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u406, null="punif")$p.value)

model407=garchFit(formula=~arma(7,1)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res407=residuals(model407, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res407, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res407^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model407@fit$ics[1])
BIC_list=append(BIC_list, model407@fit$ics[2])
u407=psged(res407, mean=0, sd=1, nu=tail(model407@fit$coef, n=1), xi=model407@fit$coef[length(model407@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u407, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u407, null="punif")$p.value)

model408=garchFit(formula=~arma(7,1)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res408=residuals(model408, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res408, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res408^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model408@fit$ics[1])
BIC_list=append(BIC_list, model408@fit$ics[2])
u408=psged(res408, mean=0, sd=1, nu=tail(model408@fit$coef, n=1), xi=model408@fit$coef[length(model408@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u408, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u408, null="punif")$p.value)

model409=garchFit(formula=~arma(7,1)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res409=residuals(model409, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res409, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res409^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model409@fit$ics[1])
BIC_list=append(BIC_list, model409@fit$ics[2])
u409=psged(res409, mean=0, sd=1, nu=tail(model409@fit$coef, n=1), xi=model409@fit$coef[length(model409@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u409, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u409, null="punif")$p.value)

model410=garchFit(formula=~arma(7,1)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res410=residuals(model410, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res410, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res410^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model410@fit$ics[1])
BIC_list=append(BIC_list, model410@fit$ics[2])
u410=psged(res410, mean=0, sd=1, nu=tail(model410@fit$coef, n=1), xi=model410@fit$coef[length(model410@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u410, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u410, null="punif")$p.value)

model411=garchFit(formula=~arma(7,1)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res411=residuals(model411, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res411, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res411^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model411@fit$ics[1])
BIC_list=append(BIC_list, model411@fit$ics[2])
u411=psged(res411, mean=0, sd=1, nu=tail(model411@fit$coef, n=1), xi=model411@fit$coef[length(model411@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u411, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u411, null="punif")$p.value)

model412=garchFit(formula=~arma(7,1)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res412=residuals(model412, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res412, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res412^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model412@fit$ics[1])
BIC_list=append(BIC_list, model412@fit$ics[2])
u412=psged(res412, mean=0, sd=1, nu=tail(model412@fit$coef, n=1), xi=model412@fit$coef[length(model412@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u412, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u412, null="punif")$p.value)

model413=garchFit(formula=~arma(7,1)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res413=residuals(model413, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res413, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res413^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model413@fit$ics[1])
BIC_list=append(BIC_list, model413@fit$ics[2])
u413=psged(res413, mean=0, sd=1, nu=tail(model413@fit$coef, n=1), xi=model413@fit$coef[length(model413@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u413, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u413, null="punif")$p.value)

model414=garchFit(formula=~arma(7,1)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res414=residuals(model414, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res414, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res414^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model414@fit$ics[1])
BIC_list=append(BIC_list, model414@fit$ics[2])
u414=psged(res414, mean=0, sd=1, nu=tail(model414@fit$coef, n=1), xi=model414@fit$coef[length(model414@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u414, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u414, null="punif")$p.value)

model415=garchFit(formula=~arma(7,2)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res415=residuals(model415, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res415, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res415^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model415@fit$ics[1])
BIC_list=append(BIC_list, model415@fit$ics[2])
u415=psged(res415, mean=0, sd=1, nu=tail(model415@fit$coef, n=1), xi=model415@fit$coef[length(model415@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u415, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u415, null="punif")$p.value)

model416=garchFit(formula=~arma(7,2)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res416=residuals(model416, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res416, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res416^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model416@fit$ics[1])
BIC_list=append(BIC_list, model416@fit$ics[2])
u416=psged(res416, mean=0, sd=1, nu=tail(model416@fit$coef, n=1), xi=model416@fit$coef[length(model416@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u416, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u416, null="punif")$p.value)

model417=garchFit(formula=~arma(7,2)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res417=residuals(model417, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res417, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res417^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model417@fit$ics[1])
BIC_list=append(BIC_list, model417@fit$ics[2])
u417=psged(res417, mean=0, sd=1, nu=tail(model417@fit$coef, n=1), xi=model417@fit$coef[length(model417@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u417, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u417, null="punif")$p.value)

model418=garchFit(formula=~arma(7,2)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res418=residuals(model418, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res418, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res418^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model418@fit$ics[1])
BIC_list=append(BIC_list, model418@fit$ics[2])
u418=psged(res418, mean=0, sd=1, nu=tail(model418@fit$coef, n=1), xi=model418@fit$coef[length(model418@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u418, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u418, null="punif")$p.value)

model419=garchFit(formula=~arma(7,2)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res419=residuals(model419, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res419, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res419^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model419@fit$ics[1])
BIC_list=append(BIC_list, model419@fit$ics[2])
u419=psged(res419, mean=0, sd=1, nu=tail(model419@fit$coef, n=1), xi=model419@fit$coef[length(model419@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u419, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u419, null="punif")$p.value)

model420=garchFit(formula=~arma(7,2)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res420=residuals(model420, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res420, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res420^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model420@fit$ics[1])
BIC_list=append(BIC_list, model420@fit$ics[2])
u420=psged(res420, mean=0, sd=1, nu=tail(model420@fit$coef, n=1), xi=model420@fit$coef[length(model420@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u420, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u420, null="punif")$p.value)

model421=garchFit(formula=~arma(7,2)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res421=residuals(model421, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res421, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res421^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model421@fit$ics[1])
BIC_list=append(BIC_list, model421@fit$ics[2])
u421=psged(res421, mean=0, sd=1, nu=tail(model421@fit$coef, n=1), xi=model421@fit$coef[length(model421@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u421, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u421, null="punif")$p.value)

model422=garchFit(formula=~arma(7,2)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res422=residuals(model422, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res422, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res422^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model422@fit$ics[1])
BIC_list=append(BIC_list, model422@fit$ics[2])
u422=psged(res422, mean=0, sd=1, nu=tail(model422@fit$coef, n=1), xi=model422@fit$coef[length(model422@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u422, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u422, null="punif")$p.value)

model423=garchFit(formula=~arma(7,2)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res423=residuals(model423, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res423, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res423^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model423@fit$ics[1])
BIC_list=append(BIC_list, model423@fit$ics[2])
u423=psged(res423, mean=0, sd=1, nu=tail(model423@fit$coef, n=1), xi=model423@fit$coef[length(model423@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u423, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u423, null="punif")$p.value)

model424=garchFit(formula=~arma(7,3)+garch(1,1),data=p_fchi,trace=F,cond.dist="sged")
res424=residuals(model424, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res424, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res424^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model424@fit$ics[1])
BIC_list=append(BIC_list, model424@fit$ics[2])
u424=psged(res424, mean=0, sd=1, nu=tail(model424@fit$coef, n=1), xi=model424@fit$coef[length(model424@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u424, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u424, null="punif")$p.value)

model425=garchFit(formula=~arma(7,3)+garch(1,2),data=p_fchi,trace=F,cond.dist="sged")
res425=residuals(model425, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res425, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res425^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model425@fit$ics[1])
BIC_list=append(BIC_list, model425@fit$ics[2])
u425=psged(res425, mean=0, sd=1, nu=tail(model425@fit$coef, n=1), xi=model425@fit$coef[length(model425@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u425, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u425, null="punif")$p.value)

model426=garchFit(formula=~arma(7,3)+garch(1,3),data=p_fchi,trace=F,cond.dist="sged")
res426=residuals(model426, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res426, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res426^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model426@fit$ics[1])
BIC_list=append(BIC_list, model426@fit$ics[2])
u426=psged(res426, mean=0, sd=1, nu=tail(model426@fit$coef, n=1), xi=model426@fit$coef[length(model426@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u426, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u426, null="punif")$p.value)

model427=garchFit(formula=~arma(7,3)+garch(2,1),data=p_fchi,trace=F,cond.dist="sged")
res427=residuals(model427, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res427, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res427^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model427@fit$ics[1])
BIC_list=append(BIC_list, model427@fit$ics[2])
u427=psged(res427, mean=0, sd=1, nu=tail(model427@fit$coef, n=1), xi=model427@fit$coef[length(model427@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u427, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u427, null="punif")$p.value)

model428=garchFit(formula=~arma(7,3)+garch(2,2),data=p_fchi,trace=F,cond.dist="sged")
res428=residuals(model428, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res428, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res428^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model428@fit$ics[1])
BIC_list=append(BIC_list, model428@fit$ics[2])
u428=psged(res428, mean=0, sd=1, nu=tail(model428@fit$coef, n=1), xi=model428@fit$coef[length(model428@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u428, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u428, null="punif")$p.value)

model429=garchFit(formula=~arma(7,3)+garch(2,3),data=p_fchi,trace=F,cond.dist="sged")
res429=residuals(model429, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res429, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res429^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model429@fit$ics[1])
BIC_list=append(BIC_list, model429@fit$ics[2])
u429=psged(res429, mean=0, sd=1, nu=tail(model429@fit$coef, n=1), xi=model429@fit$coef[length(model429@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u429, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u429, null="punif")$p.value)

model430=garchFit(formula=~arma(7,3)+garch(3,1),data=p_fchi,trace=F,cond.dist="sged")
res430=residuals(model430, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res430, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res430^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model430@fit$ics[1])
BIC_list=append(BIC_list, model430@fit$ics[2])
u430=psged(res430, mean=0, sd=1, nu=tail(model430@fit$coef, n=1), xi=model430@fit$coef[length(model430@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u430, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u430, null="punif")$p.value)

model431=garchFit(formula=~arma(7,3)+garch(3,2),data=p_fchi,trace=F,cond.dist="sged")
res431=residuals(model431, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res431, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res431^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model431@fit$ics[1])
BIC_list=append(BIC_list, model431@fit$ics[2])
u431=psged(res431, mean=0, sd=1, nu=tail(model431@fit$coef, n=1), xi=model431@fit$coef[length(model431@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u431, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u431, null="punif")$p.value)

model432=garchFit(formula=~arma(7,3)+garch(3,3),data=p_fchi,trace=F,cond.dist="sged")
res432=residuals(model432, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res432, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res432^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model432@fit$ics[1])
BIC_list=append(BIC_list, model432@fit$ics[2])
u432=psged(res432, mean=0, sd=1, nu=tail(model432@fit$coef, n=1), xi=model432@fit$coef[length(model432@fit$coef)-1])[4:length(p_fchi)]
ksTests=append(ksTests,LcKS(u432, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u432, null="punif")$p.value)






df=data.frame(model_no=c(1:length(boxTest1)), boxTest1, boxTest2, ksTests, adTests, AIC_list, BIC_list)
write_xlsx(df,"C:\\Users\\Sayed\\Desktop\\TASK 2 STAT0011\\FCHI DATA.xlsx")