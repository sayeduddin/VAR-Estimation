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

p_ftse=matrix(get.hist.quote(instrument="^ftse", start="2000-01-01",
                             end="2021-01-01", quote="AdjClose", compression='w'))

#p_ftse = na.omit(p_ftse)
p_ftse=diff(log(p_ftse))
jarqueberaTest(p_ftse)

par(mfrow=c(2,2))
acf(p_ftse, col="green", lwd=2)
pacf(p_ftse, col="green", lwd=2)
acf(p_ftse^2, col="red", lwd=2)
par(mfrow=c(1,1))






model1=garchFit(formula=~arma(1,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res1=residuals(model1, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res1, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res1^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model1@fit$ics[1])
BIC_list=append(BIC_list, model1@fit$ics[2])
u1=pnorm(res1, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u1, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u1, null="punif")$p.value)

model2=garchFit(formula=~arma(1,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res2=residuals(model2, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res2^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model2@fit$ics[1])
BIC_list=append(BIC_list, model2@fit$ics[2])
u2=pnorm(res2, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u2, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u2, null="punif")$p.value)

model3=garchFit(formula=~arma(1,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res3=residuals(model3, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res3, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res3^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model3@fit$ics[1])
BIC_list=append(BIC_list, model3@fit$ics[2])
u3=pnorm(res3, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u3, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u3, null="punif")$p.value)

model4=garchFit(formula=~arma(1,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res4=residuals(model4, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res4, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res4^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model4@fit$ics[1])
BIC_list=append(BIC_list, model4@fit$ics[2])
u4=pnorm(res4, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u4, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u4, null="punif")$p.value)

model5=garchFit(formula=~arma(1,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res5=residuals(model5, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res5, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res5^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model5@fit$ics[1])
BIC_list=append(BIC_list, model5@fit$ics[2])
u5=pnorm(res5, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u5, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u5, null="punif")$p.value)

model6=garchFit(formula=~arma(1,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res6=residuals(model6, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res6, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res6^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model6@fit$ics[1])
BIC_list=append(BIC_list, model6@fit$ics[2])
u6=pnorm(res6, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u6, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u6, null="punif")$p.value)

model7=garchFit(formula=~arma(1,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res7=residuals(model7, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res7, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res7^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model7@fit$ics[1])
BIC_list=append(BIC_list, model7@fit$ics[2])
u7=pnorm(res7, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u7, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u7, null="punif")$p.value)

model8=garchFit(formula=~arma(1,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res8=residuals(model8, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res8, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res8^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model8@fit$ics[1])
BIC_list=append(BIC_list, model8@fit$ics[2])
u8=pnorm(res8, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u8, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u8, null="punif")$p.value)

model9=garchFit(formula=~arma(1,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res9=residuals(model9, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res9, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res9^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model9@fit$ics[1])
BIC_list=append(BIC_list, model9@fit$ics[2])
u9=pnorm(res9, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u9, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u9, null="punif")$p.value)

model10=garchFit(formula=~arma(1,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res10=residuals(model10, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res10, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res10^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model10@fit$ics[1])
BIC_list=append(BIC_list, model10@fit$ics[2])
u10=pnorm(res10, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u10, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u10, null="punif")$p.value)

model11=garchFit(formula=~arma(1,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res11=residuals(model11, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res11, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res11^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model11@fit$ics[1])
BIC_list=append(BIC_list, model11@fit$ics[2])
u11=pnorm(res11, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u11, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u11, null="punif")$p.value)

model12=garchFit(formula=~arma(1,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res12=residuals(model12, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res12, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res12^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model12@fit$ics[1])
BIC_list=append(BIC_list, model12@fit$ics[2])
u12=pnorm(res12, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u12, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u12, null="punif")$p.value)

model13=garchFit(formula=~arma(1,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res13=residuals(model13, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res13, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res13^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model13@fit$ics[1])
BIC_list=append(BIC_list, model13@fit$ics[2])
u13=pnorm(res13, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u13, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u13, null="punif")$p.value)

model14=garchFit(formula=~arma(1,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res14=residuals(model14, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res14, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res14^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model14@fit$ics[1])
BIC_list=append(BIC_list, model14@fit$ics[2])
u14=pnorm(res14, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u14, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u14, null="punif")$p.value)

model15=garchFit(formula=~arma(1,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res15=residuals(model15, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res15, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res15^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model15@fit$ics[1])
BIC_list=append(BIC_list, model15@fit$ics[2])
u15=pnorm(res15, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u15, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u15, null="punif")$p.value)

model16=garchFit(formula=~arma(1,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res16=residuals(model16, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res16, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res16^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model16@fit$ics[1])
BIC_list=append(BIC_list, model16@fit$ics[2])
u16=pnorm(res16, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u16, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u16, null="punif")$p.value)

model17=garchFit(formula=~arma(1,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res17=residuals(model17, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res17, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res17^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model17@fit$ics[1])
BIC_list=append(BIC_list, model17@fit$ics[2])
u17=pnorm(res17, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u17, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u17, null="punif")$p.value)

model18=garchFit(formula=~arma(1,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res18=residuals(model18, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res18, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res18^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model18@fit$ics[1])
BIC_list=append(BIC_list, model18@fit$ics[2])
u18=pnorm(res18, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u18, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u18, null="punif")$p.value)

model19=garchFit(formula=~arma(1,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res19=residuals(model19, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res19, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res19^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model19@fit$ics[1])
BIC_list=append(BIC_list, model19@fit$ics[2])
u19=pnorm(res19, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u19, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u19, null="punif")$p.value)

model20=garchFit(formula=~arma(1,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res20=residuals(model20, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res20, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res20^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model20@fit$ics[1])
BIC_list=append(BIC_list, model20@fit$ics[2])
u20=pnorm(res20, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u20, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u20, null="punif")$p.value)

model21=garchFit(formula=~arma(1,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res21=residuals(model21, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res21, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res21^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model21@fit$ics[1])
BIC_list=append(BIC_list, model21@fit$ics[2])
u21=pnorm(res21, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u21, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u21, null="punif")$p.value)

model22=garchFit(formula=~arma(1,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res22=residuals(model22, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res22, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res22^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model22@fit$ics[1])
BIC_list=append(BIC_list, model22@fit$ics[2])
u22=pnorm(res22, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u22, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u22, null="punif")$p.value)

model23=garchFit(formula=~arma(1,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res23=residuals(model23, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res23, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res23^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model23@fit$ics[1])
BIC_list=append(BIC_list, model23@fit$ics[2])
u23=pnorm(res23, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u23, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u23, null="punif")$p.value)

model24=garchFit(formula=~arma(1,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res24=residuals(model24, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res24, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res24^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model24@fit$ics[1])
BIC_list=append(BIC_list, model24@fit$ics[2])
u24=pnorm(res24, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u24, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u24, null="punif")$p.value)

model25=garchFit(formula=~arma(1,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res25=residuals(model25, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res25, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res25^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model25@fit$ics[1])
BIC_list=append(BIC_list, model25@fit$ics[2])
u25=pnorm(res25, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u25, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u25, null="punif")$p.value)

model26=garchFit(formula=~arma(1,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res26=residuals(model26, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res26, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res26^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model26@fit$ics[1])
BIC_list=append(BIC_list, model26@fit$ics[2])
u26=pnorm(res26, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u26, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u26, null="punif")$p.value)

model27=garchFit(formula=~arma(1,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res27=residuals(model27, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res27, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res27^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model27@fit$ics[1])
BIC_list=append(BIC_list, model27@fit$ics[2])
u27=pnorm(res27, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u27, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u27, null="punif")$p.value)

model28=garchFit(formula=~arma(1,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res28=residuals(model28, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res28, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res28^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model28@fit$ics[1])
BIC_list=append(BIC_list, model28@fit$ics[2])
u28=pnorm(res28, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u28, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u28, null="punif")$p.value)

model29=garchFit(formula=~arma(1,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res29=residuals(model29, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res29, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res29^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model29@fit$ics[1])
BIC_list=append(BIC_list, model29@fit$ics[2])
u29=pnorm(res29, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u29, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u29, null="punif")$p.value)

model30=garchFit(formula=~arma(1,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res30=residuals(model30, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res30, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res30^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model30@fit$ics[1])
BIC_list=append(BIC_list, model30@fit$ics[2])
u30=pnorm(res30, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u30, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u30, null="punif")$p.value)

model31=garchFit(formula=~arma(1,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res31=residuals(model31, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res31, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res31^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model31@fit$ics[1])
BIC_list=append(BIC_list, model31@fit$ics[2])
u31=pnorm(res31, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u31, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u31, null="punif")$p.value)

model32=garchFit(formula=~arma(1,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res32=residuals(model32, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res32, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res32^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model32@fit$ics[1])
BIC_list=append(BIC_list, model32@fit$ics[2])
u32=pnorm(res32, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u32, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u32, null="punif")$p.value)

model33=garchFit(formula=~arma(1,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res33=residuals(model33, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res33, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res33^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model33@fit$ics[1])
BIC_list=append(BIC_list, model33@fit$ics[2])
u33=pnorm(res33, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u33, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u33, null="punif")$p.value)

model34=garchFit(formula=~arma(1,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res34=residuals(model34, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res34, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res34^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model34@fit$ics[1])
BIC_list=append(BIC_list, model34@fit$ics[2])
u34=pnorm(res34, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u34, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u34, null="punif")$p.value)

model35=garchFit(formula=~arma(1,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res35=residuals(model35, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res35, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res35^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model35@fit$ics[1])
BIC_list=append(BIC_list, model35@fit$ics[2])
u35=pnorm(res35, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u35, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u35, null="punif")$p.value)

model36=garchFit(formula=~arma(1,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res36=residuals(model36, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res36, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res36^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model36@fit$ics[1])
BIC_list=append(BIC_list, model36@fit$ics[2])
u36=pnorm(res36, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u36, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u36, null="punif")$p.value)

model37=garchFit(formula=~arma(3,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res37=residuals(model37, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res37, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res37^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model37@fit$ics[1])
BIC_list=append(BIC_list, model37@fit$ics[2])
u37=pnorm(res37, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u37, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u37, null="punif")$p.value)

model38=garchFit(formula=~arma(3,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res38=residuals(model38, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res38, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res38^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model38@fit$ics[1])
BIC_list=append(BIC_list, model38@fit$ics[2])
u38=pnorm(res38, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u38, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u38, null="punif")$p.value)

model39=garchFit(formula=~arma(3,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res39=residuals(model39, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res39, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res39^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model39@fit$ics[1])
BIC_list=append(BIC_list, model39@fit$ics[2])
u39=pnorm(res39, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u39, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u39, null="punif")$p.value)

model40=garchFit(formula=~arma(3,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res40=residuals(model40, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res40, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res40^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model40@fit$ics[1])
BIC_list=append(BIC_list, model40@fit$ics[2])
u40=pnorm(res40, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u40, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u40, null="punif")$p.value)

model41=garchFit(formula=~arma(3,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res41=residuals(model41, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res41, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res41^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model41@fit$ics[1])
BIC_list=append(BIC_list, model41@fit$ics[2])
u41=pnorm(res41, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u41, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u41, null="punif")$p.value)

model42=garchFit(formula=~arma(3,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res42=residuals(model42, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res42, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res42^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model42@fit$ics[1])
BIC_list=append(BIC_list, model42@fit$ics[2])
u42=pnorm(res42, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u42, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u42, null="punif")$p.value)

model43=garchFit(formula=~arma(3,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res43=residuals(model43, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res43, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res43^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model43@fit$ics[1])
BIC_list=append(BIC_list, model43@fit$ics[2])
u43=pnorm(res43, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u43, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u43, null="punif")$p.value)

model44=garchFit(formula=~arma(3,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res44=residuals(model44, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res44, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res44^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model44@fit$ics[1])
BIC_list=append(BIC_list, model44@fit$ics[2])
u44=pnorm(res44, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u44, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u44, null="punif")$p.value)

model45=garchFit(formula=~arma(3,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res45=residuals(model45, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res45, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res45^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model45@fit$ics[1])
BIC_list=append(BIC_list, model45@fit$ics[2])
u45=pnorm(res45, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u45, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u45, null="punif")$p.value)

model46=garchFit(formula=~arma(3,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res46=residuals(model46, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res46, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res46^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model46@fit$ics[1])
BIC_list=append(BIC_list, model46@fit$ics[2])
u46=pnorm(res46, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u46, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u46, null="punif")$p.value)

model47=garchFit(formula=~arma(3,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res47=residuals(model47, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res47, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res47^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model47@fit$ics[1])
BIC_list=append(BIC_list, model47@fit$ics[2])
u47=pnorm(res47, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u47, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u47, null="punif")$p.value)

model48=garchFit(formula=~arma(3,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res48=residuals(model48, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res48, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res48^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model48@fit$ics[1])
BIC_list=append(BIC_list, model48@fit$ics[2])
u48=pnorm(res48, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u48, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u48, null="punif")$p.value)

model49=garchFit(formula=~arma(3,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res49=residuals(model49, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res49, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res49^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model49@fit$ics[1])
BIC_list=append(BIC_list, model49@fit$ics[2])
u49=pnorm(res49, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u49, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u49, null="punif")$p.value)

model50=garchFit(formula=~arma(3,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res50=residuals(model50, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res50, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res50^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model50@fit$ics[1])
BIC_list=append(BIC_list, model50@fit$ics[2])
u50=pnorm(res50, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u50, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u50, null="punif")$p.value)

model51=garchFit(formula=~arma(3,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res51=residuals(model51, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res51, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res51^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model51@fit$ics[1])
BIC_list=append(BIC_list, model51@fit$ics[2])
u51=pnorm(res51, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u51, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u51, null="punif")$p.value)

model52=garchFit(formula=~arma(3,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res52=residuals(model52, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res52, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res52^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model52@fit$ics[1])
BIC_list=append(BIC_list, model52@fit$ics[2])
u52=pnorm(res52, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u52, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u52, null="punif")$p.value)

model53=garchFit(formula=~arma(3,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res53=residuals(model53, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res53, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res53^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model53@fit$ics[1])
BIC_list=append(BIC_list, model53@fit$ics[2])
u53=pnorm(res53, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u53, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u53, null="punif")$p.value)

model54=garchFit(formula=~arma(3,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res54=residuals(model54, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res54, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res54^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model54@fit$ics[1])
BIC_list=append(BIC_list, model54@fit$ics[2])
u54=pnorm(res54, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u54, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u54, null="punif")$p.value)

model55=garchFit(formula=~arma(3,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res55=residuals(model55, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res55, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res55^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model55@fit$ics[1])
BIC_list=append(BIC_list, model55@fit$ics[2])
u55=pnorm(res55, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u55, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u55, null="punif")$p.value)

model56=garchFit(formula=~arma(3,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res56=residuals(model56, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res56, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res56^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model56@fit$ics[1])
BIC_list=append(BIC_list, model56@fit$ics[2])
u56=pnorm(res56, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u56, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u56, null="punif")$p.value)

model57=garchFit(formula=~arma(3,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res57=residuals(model57, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res57, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res57^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model57@fit$ics[1])
BIC_list=append(BIC_list, model57@fit$ics[2])
u57=pnorm(res57, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u57, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u57, null="punif")$p.value)

model58=garchFit(formula=~arma(3,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res58=residuals(model58, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res58, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res58^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model58@fit$ics[1])
BIC_list=append(BIC_list, model58@fit$ics[2])
u58=pnorm(res58, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u58, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u58, null="punif")$p.value)

model59=garchFit(formula=~arma(3,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res59=residuals(model59, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res59, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res59^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model59@fit$ics[1])
BIC_list=append(BIC_list, model59@fit$ics[2])
u59=pnorm(res59, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u59, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u59, null="punif")$p.value)

model60=garchFit(formula=~arma(3,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res60=residuals(model60, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res60, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res60^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model60@fit$ics[1])
BIC_list=append(BIC_list, model60@fit$ics[2])
u60=pnorm(res60, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u60, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u60, null="punif")$p.value)

model61=garchFit(formula=~arma(3,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res61=residuals(model61, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res61, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res61^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model61@fit$ics[1])
BIC_list=append(BIC_list, model61@fit$ics[2])
u61=pnorm(res61, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u61, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u61, null="punif")$p.value)

model62=garchFit(formula=~arma(3,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res62=residuals(model62, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res62, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res62^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model62@fit$ics[1])
BIC_list=append(BIC_list, model62@fit$ics[2])
u62=pnorm(res62, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u62, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u62, null="punif")$p.value)

model63=garchFit(formula=~arma(3,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res63=residuals(model63, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res63, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res63^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model63@fit$ics[1])
BIC_list=append(BIC_list, model63@fit$ics[2])
u63=pnorm(res63, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u63, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u63, null="punif")$p.value)

model64=garchFit(formula=~arma(3,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res64=residuals(model64, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res64, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res64^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model64@fit$ics[1])
BIC_list=append(BIC_list, model64@fit$ics[2])
u64=pnorm(res64, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u64, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u64, null="punif")$p.value)

model65=garchFit(formula=~arma(3,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res65=residuals(model65, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res65, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res65^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model65@fit$ics[1])
BIC_list=append(BIC_list, model65@fit$ics[2])
u65=pnorm(res65, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u65, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u65, null="punif")$p.value)

model66=garchFit(formula=~arma(3,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res66=residuals(model66, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res66, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res66^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model66@fit$ics[1])
BIC_list=append(BIC_list, model66@fit$ics[2])
u66=pnorm(res66, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u66, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u66, null="punif")$p.value)

model67=garchFit(formula=~arma(3,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res67=residuals(model67, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res67, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res67^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model67@fit$ics[1])
BIC_list=append(BIC_list, model67@fit$ics[2])
u67=pnorm(res67, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u67, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u67, null="punif")$p.value)

model68=garchFit(formula=~arma(3,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res68=residuals(model68, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res68, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res68^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model68@fit$ics[1])
BIC_list=append(BIC_list, model68@fit$ics[2])
u68=pnorm(res68, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u68, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u68, null="punif")$p.value)

model69=garchFit(formula=~arma(3,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res69=residuals(model69, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res69, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res69^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model69@fit$ics[1])
BIC_list=append(BIC_list, model69@fit$ics[2])
u69=pnorm(res69, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u69, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u69, null="punif")$p.value)

model70=garchFit(formula=~arma(3,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res70=residuals(model70, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res70, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res70^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model70@fit$ics[1])
BIC_list=append(BIC_list, model70@fit$ics[2])
u70=pnorm(res70, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u70, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u70, null="punif")$p.value)

model71=garchFit(formula=~arma(3,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res71=residuals(model71, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res71, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res71^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model71@fit$ics[1])
BIC_list=append(BIC_list, model71@fit$ics[2])
u71=pnorm(res71, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u71, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u71, null="punif")$p.value)

model72=garchFit(formula=~arma(3,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res72=residuals(model72, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res72, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res72^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model72@fit$ics[1])
BIC_list=append(BIC_list, model72@fit$ics[2])
u72=pnorm(res72, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u72, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u72, null="punif")$p.value)

model73=garchFit(formula=~arma(4,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res73=residuals(model73, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res73, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res73^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model73@fit$ics[1])
BIC_list=append(BIC_list, model73@fit$ics[2])
u73=pnorm(res73, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u73, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u73, null="punif")$p.value)

model74=garchFit(formula=~arma(4,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res74=residuals(model74, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res74, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res74^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model74@fit$ics[1])
BIC_list=append(BIC_list, model74@fit$ics[2])
u74=pnorm(res74, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u74, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u74, null="punif")$p.value)

model75=garchFit(formula=~arma(4,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res75=residuals(model75, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res75, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res75^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model75@fit$ics[1])
BIC_list=append(BIC_list, model75@fit$ics[2])
u75=pnorm(res75, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u75, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u75, null="punif")$p.value)

model76=garchFit(formula=~arma(4,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res76=residuals(model76, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res76, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res76^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model76@fit$ics[1])
BIC_list=append(BIC_list, model76@fit$ics[2])
u76=pnorm(res76, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u76, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u76, null="punif")$p.value)

model77=garchFit(formula=~arma(4,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res77=residuals(model77, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res77, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res77^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model77@fit$ics[1])
BIC_list=append(BIC_list, model77@fit$ics[2])
u77=pnorm(res77, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u77, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u77, null="punif")$p.value)

model78=garchFit(formula=~arma(4,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res78=residuals(model78, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res78, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res78^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model78@fit$ics[1])
BIC_list=append(BIC_list, model78@fit$ics[2])
u78=pnorm(res78, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u78, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u78, null="punif")$p.value)

model79=garchFit(formula=~arma(4,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res79=residuals(model79, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res79, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res79^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model79@fit$ics[1])
BIC_list=append(BIC_list, model79@fit$ics[2])
u79=pnorm(res79, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u79, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u79, null="punif")$p.value)

model80=garchFit(formula=~arma(4,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res80=residuals(model80, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res80, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res80^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model80@fit$ics[1])
BIC_list=append(BIC_list, model80@fit$ics[2])
u80=pnorm(res80, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u80, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u80, null="punif")$p.value)

model81=garchFit(formula=~arma(4,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res81=residuals(model81, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res81, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res81^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model81@fit$ics[1])
BIC_list=append(BIC_list, model81@fit$ics[2])
u81=pnorm(res81, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u81, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u81, null="punif")$p.value)

model82=garchFit(formula=~arma(4,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res82=residuals(model82, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res82, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res82^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model82@fit$ics[1])
BIC_list=append(BIC_list, model82@fit$ics[2])
u82=pnorm(res82, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u82, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u82, null="punif")$p.value)

model83=garchFit(formula=~arma(4,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res83=residuals(model83, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res83, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res83^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model83@fit$ics[1])
BIC_list=append(BIC_list, model83@fit$ics[2])
u83=pnorm(res83, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u83, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u83, null="punif")$p.value)

model84=garchFit(formula=~arma(4,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res84=residuals(model84, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res84, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res84^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model84@fit$ics[1])
BIC_list=append(BIC_list, model84@fit$ics[2])
u84=pnorm(res84, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u84, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u84, null="punif")$p.value)

model85=garchFit(formula=~arma(4,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res85=residuals(model85, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res85, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res85^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model85@fit$ics[1])
BIC_list=append(BIC_list, model85@fit$ics[2])
u85=pnorm(res85, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u85, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u85, null="punif")$p.value)

model86=garchFit(formula=~arma(4,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res86=residuals(model86, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res86, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res86^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model86@fit$ics[1])
BIC_list=append(BIC_list, model86@fit$ics[2])
u86=pnorm(res86, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u86, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u86, null="punif")$p.value)

model87=garchFit(formula=~arma(4,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res87=residuals(model87, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res87, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res87^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model87@fit$ics[1])
BIC_list=append(BIC_list, model87@fit$ics[2])
u87=pnorm(res87, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u87, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u87, null="punif")$p.value)

model88=garchFit(formula=~arma(4,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res88=residuals(model88, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res88, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res88^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model88@fit$ics[1])
BIC_list=append(BIC_list, model88@fit$ics[2])
u88=pnorm(res88, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u88, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u88, null="punif")$p.value)

model89=garchFit(formula=~arma(4,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res89=residuals(model89, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res89, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res89^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model89@fit$ics[1])
BIC_list=append(BIC_list, model89@fit$ics[2])
u89=pnorm(res89, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u89, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u89, null="punif")$p.value)

model90=garchFit(formula=~arma(4,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res90=residuals(model90, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res90, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res90^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model90@fit$ics[1])
BIC_list=append(BIC_list, model90@fit$ics[2])
u90=pnorm(res90, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u90, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u90, null="punif")$p.value)

model91=garchFit(formula=~arma(4,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res91=residuals(model91, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res91, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res91^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model91@fit$ics[1])
BIC_list=append(BIC_list, model91@fit$ics[2])
u91=pnorm(res91, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u91, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u91, null="punif")$p.value)

model92=garchFit(formula=~arma(4,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res92=residuals(model92, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res92, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res92^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model92@fit$ics[1])
BIC_list=append(BIC_list, model92@fit$ics[2])
u92=pnorm(res92, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u92, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u92, null="punif")$p.value)

model93=garchFit(formula=~arma(4,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res93=residuals(model93, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res93, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res93^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model93@fit$ics[1])
BIC_list=append(BIC_list, model93@fit$ics[2])
u93=pnorm(res93, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u93, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u93, null="punif")$p.value)

model94=garchFit(formula=~arma(4,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res94=residuals(model94, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res94, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res94^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model94@fit$ics[1])
BIC_list=append(BIC_list, model94@fit$ics[2])
u94=pnorm(res94, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u94, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u94, null="punif")$p.value)

model95=garchFit(formula=~arma(4,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res95=residuals(model95, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res95, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res95^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model95@fit$ics[1])
BIC_list=append(BIC_list, model95@fit$ics[2])
u95=pnorm(res95, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u95, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u95, null="punif")$p.value)

model96=garchFit(formula=~arma(4,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res96=residuals(model96, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res96, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res96^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model96@fit$ics[1])
BIC_list=append(BIC_list, model96@fit$ics[2])
u96=pnorm(res96, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u96, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u96, null="punif")$p.value)

model97=garchFit(formula=~arma(4,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res97=residuals(model97, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res97, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res97^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model97@fit$ics[1])
BIC_list=append(BIC_list, model97@fit$ics[2])
u97=pnorm(res97, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u97, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u97, null="punif")$p.value)

model98=garchFit(formula=~arma(4,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res98=residuals(model98, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res98, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res98^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model98@fit$ics[1])
BIC_list=append(BIC_list, model98@fit$ics[2])
u98=pnorm(res98, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u98, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u98, null="punif")$p.value)

model99=garchFit(formula=~arma(4,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res99=residuals(model99, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res99, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res99^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model99@fit$ics[1])
BIC_list=append(BIC_list, model99@fit$ics[2])
u99=pnorm(res99, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u99, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u99, null="punif")$p.value)

model100=garchFit(formula=~arma(4,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res100=residuals(model100, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res100, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res100^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model100@fit$ics[1])
BIC_list=append(BIC_list, model100@fit$ics[2])
u100=pnorm(res100, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u100, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u100, null="punif")$p.value)

model101=garchFit(formula=~arma(4,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res101=residuals(model101, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res101, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res101^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model101@fit$ics[1])
BIC_list=append(BIC_list, model101@fit$ics[2])
u101=pnorm(res101, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u101, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u101, null="punif")$p.value)

model102=garchFit(formula=~arma(4,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res102=residuals(model102, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res102, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res102^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model102@fit$ics[1])
BIC_list=append(BIC_list, model102@fit$ics[2])
u102=pnorm(res102, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u102, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u102, null="punif")$p.value)

model103=garchFit(formula=~arma(4,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res103=residuals(model103, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res103, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res103^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model103@fit$ics[1])
BIC_list=append(BIC_list, model103@fit$ics[2])
u103=pnorm(res103, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u103, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u103, null="punif")$p.value)

model104=garchFit(formula=~arma(4,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res104=residuals(model104, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res104, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res104^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model104@fit$ics[1])
BIC_list=append(BIC_list, model104@fit$ics[2])
u104=pnorm(res104, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u104, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u104, null="punif")$p.value)

model105=garchFit(formula=~arma(4,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res105=residuals(model105, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res105, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res105^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model105@fit$ics[1])
BIC_list=append(BIC_list, model105@fit$ics[2])
u105=pnorm(res105, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u105, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u105, null="punif")$p.value)

model106=garchFit(formula=~arma(4,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res106=residuals(model106, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res106, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res106^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model106@fit$ics[1])
BIC_list=append(BIC_list, model106@fit$ics[2])
u106=pnorm(res106, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u106, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u106, null="punif")$p.value)

model107=garchFit(formula=~arma(4,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res107=residuals(model107, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res107, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res107^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model107@fit$ics[1])
BIC_list=append(BIC_list, model107@fit$ics[2])
u107=pnorm(res107, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u107, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u107, null="punif")$p.value)

model108=garchFit(formula=~arma(4,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res108=residuals(model108, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res108, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res108^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model108@fit$ics[1])
BIC_list=append(BIC_list, model108@fit$ics[2])
u108=pnorm(res108, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u108, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u108, null="punif")$p.value)

model109=garchFit(formula=~arma(7,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res109=residuals(model109, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res109, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res109^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model109@fit$ics[1])
BIC_list=append(BIC_list, model109@fit$ics[2])
u109=pnorm(res109, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u109, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u109, null="punif")$p.value)

model110=garchFit(formula=~arma(7,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res110=residuals(model110, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res110, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res110^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model110@fit$ics[1])
BIC_list=append(BIC_list, model110@fit$ics[2])
u110=pnorm(res110, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u110, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u110, null="punif")$p.value)

model111=garchFit(formula=~arma(7,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res111=residuals(model111, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res111, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res111^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model111@fit$ics[1])
BIC_list=append(BIC_list, model111@fit$ics[2])
u111=pnorm(res111, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u111, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u111, null="punif")$p.value)

model112=garchFit(formula=~arma(7,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res112=residuals(model112, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res112, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res112^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model112@fit$ics[1])
BIC_list=append(BIC_list, model112@fit$ics[2])
u112=pnorm(res112, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u112, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u112, null="punif")$p.value)

model113=garchFit(formula=~arma(7,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res113=residuals(model113, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res113, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res113^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model113@fit$ics[1])
BIC_list=append(BIC_list, model113@fit$ics[2])
u113=pnorm(res113, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u113, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u113, null="punif")$p.value)

model114=garchFit(formula=~arma(7,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res114=residuals(model114, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res114, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res114^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model114@fit$ics[1])
BIC_list=append(BIC_list, model114@fit$ics[2])
u114=pnorm(res114, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u114, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u114, null="punif")$p.value)

model115=garchFit(formula=~arma(7,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res115=residuals(model115, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res115, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res115^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model115@fit$ics[1])
BIC_list=append(BIC_list, model115@fit$ics[2])
u115=pnorm(res115, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u115, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u115, null="punif")$p.value)

model116=garchFit(formula=~arma(7,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res116=residuals(model116, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res116, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res116^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model116@fit$ics[1])
BIC_list=append(BIC_list, model116@fit$ics[2])
u116=pnorm(res116, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u116, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u116, null="punif")$p.value)

model117=garchFit(formula=~arma(7,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res117=residuals(model117, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res117, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res117^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model117@fit$ics[1])
BIC_list=append(BIC_list, model117@fit$ics[2])
u117=pnorm(res117, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u117, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u117, null="punif")$p.value)

model118=garchFit(formula=~arma(7,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res118=residuals(model118, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res118, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res118^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model118@fit$ics[1])
BIC_list=append(BIC_list, model118@fit$ics[2])
u118=pnorm(res118, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u118, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u118, null="punif")$p.value)

model119=garchFit(formula=~arma(7,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res119=residuals(model119, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res119, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res119^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model119@fit$ics[1])
BIC_list=append(BIC_list, model119@fit$ics[2])
u119=pnorm(res119, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u119, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u119, null="punif")$p.value)

model120=garchFit(formula=~arma(7,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res120=residuals(model120, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res120, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res120^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model120@fit$ics[1])
BIC_list=append(BIC_list, model120@fit$ics[2])
u120=pnorm(res120, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u120, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u120, null="punif")$p.value)

model121=garchFit(formula=~arma(7,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res121=residuals(model121, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res121, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res121^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model121@fit$ics[1])
BIC_list=append(BIC_list, model121@fit$ics[2])
u121=pnorm(res121, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u121, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u121, null="punif")$p.value)

model122=garchFit(formula=~arma(7,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res122=residuals(model122, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res122, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res122^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model122@fit$ics[1])
BIC_list=append(BIC_list, model122@fit$ics[2])
u122=pnorm(res122, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u122, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u122, null="punif")$p.value)

model123=garchFit(formula=~arma(7,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res123=residuals(model123, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res123, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res123^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model123@fit$ics[1])
BIC_list=append(BIC_list, model123@fit$ics[2])
u123=pnorm(res123, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u123, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u123, null="punif")$p.value)

model124=garchFit(formula=~arma(7,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res124=residuals(model124, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res124, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res124^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model124@fit$ics[1])
BIC_list=append(BIC_list, model124@fit$ics[2])
u124=pnorm(res124, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u124, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u124, null="punif")$p.value)

model125=garchFit(formula=~arma(7,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res125=residuals(model125, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res125, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res125^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model125@fit$ics[1])
BIC_list=append(BIC_list, model125@fit$ics[2])
u125=pnorm(res125, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u125, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u125, null="punif")$p.value)

model126=garchFit(formula=~arma(7,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res126=residuals(model126, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res126, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res126^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model126@fit$ics[1])
BIC_list=append(BIC_list, model126@fit$ics[2])
u126=pnorm(res126, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u126, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u126, null="punif")$p.value)

model127=garchFit(formula=~arma(7,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res127=residuals(model127, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res127, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res127^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model127@fit$ics[1])
BIC_list=append(BIC_list, model127@fit$ics[2])
u127=pnorm(res127, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u127, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u127, null="punif")$p.value)

model128=garchFit(formula=~arma(7,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res128=residuals(model128, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res128, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res128^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model128@fit$ics[1])
BIC_list=append(BIC_list, model128@fit$ics[2])
u128=pnorm(res128, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u128, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u128, null="punif")$p.value)

model129=garchFit(formula=~arma(7,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res129=residuals(model129, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res129, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res129^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model129@fit$ics[1])
BIC_list=append(BIC_list, model129@fit$ics[2])
u129=pnorm(res129, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u129, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u129, null="punif")$p.value)

model130=garchFit(formula=~arma(7,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res130=residuals(model130, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res130, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res130^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model130@fit$ics[1])
BIC_list=append(BIC_list, model130@fit$ics[2])
u130=pnorm(res130, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u130, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u130, null="punif")$p.value)

model131=garchFit(formula=~arma(7,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res131=residuals(model131, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res131, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res131^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model131@fit$ics[1])
BIC_list=append(BIC_list, model131@fit$ics[2])
u131=pnorm(res131, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u131, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u131, null="punif")$p.value)

model132=garchFit(formula=~arma(7,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res132=residuals(model132, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res132, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res132^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model132@fit$ics[1])
BIC_list=append(BIC_list, model132@fit$ics[2])
u132=pnorm(res132, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u132, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u132, null="punif")$p.value)

model133=garchFit(formula=~arma(7,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res133=residuals(model133, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res133, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res133^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model133@fit$ics[1])
BIC_list=append(BIC_list, model133@fit$ics[2])
u133=pnorm(res133, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u133, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u133, null="punif")$p.value)

model134=garchFit(formula=~arma(7,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res134=residuals(model134, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res134, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res134^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model134@fit$ics[1])
BIC_list=append(BIC_list, model134@fit$ics[2])
u134=pnorm(res134, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u134, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u134, null="punif")$p.value)

model135=garchFit(formula=~arma(7,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res135=residuals(model135, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res135, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res135^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model135@fit$ics[1])
BIC_list=append(BIC_list, model135@fit$ics[2])
u135=pnorm(res135, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u135, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u135, null="punif")$p.value)

model136=garchFit(formula=~arma(7,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="norm")
res136=residuals(model136, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res136, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res136^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model136@fit$ics[1])
BIC_list=append(BIC_list, model136@fit$ics[2])
u136=pnorm(res136, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u136, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u136, null="punif")$p.value)

model137=garchFit(formula=~arma(7,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="norm")
res137=residuals(model137, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res137, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res137^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model137@fit$ics[1])
BIC_list=append(BIC_list, model137@fit$ics[2])
u137=pnorm(res137, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u137, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u137, null="punif")$p.value)

model138=garchFit(formula=~arma(7,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="norm")
res138=residuals(model138, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res138, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res138^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model138@fit$ics[1])
BIC_list=append(BIC_list, model138@fit$ics[2])
u138=pnorm(res138, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u138, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u138, null="punif")$p.value)

model139=garchFit(formula=~arma(7,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="norm")
res139=residuals(model139, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res139, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res139^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model139@fit$ics[1])
BIC_list=append(BIC_list, model139@fit$ics[2])
u139=pnorm(res139, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u139, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u139, null="punif")$p.value)

model140=garchFit(formula=~arma(7,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="norm")
res140=residuals(model140, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res140, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res140^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model140@fit$ics[1])
BIC_list=append(BIC_list, model140@fit$ics[2])
u140=pnorm(res140, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u140, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u140, null="punif")$p.value)

model141=garchFit(formula=~arma(7,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="norm")
res141=residuals(model141, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res141, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res141^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model141@fit$ics[1])
BIC_list=append(BIC_list, model141@fit$ics[2])
u141=pnorm(res141, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u141, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u141, null="punif")$p.value)

model142=garchFit(formula=~arma(7,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="norm")
res142=residuals(model142, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res142, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res142^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model142@fit$ics[1])
BIC_list=append(BIC_list, model142@fit$ics[2])
u142=pnorm(res142, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u142, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u142, null="punif")$p.value)

model143=garchFit(formula=~arma(7,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="norm")
res143=residuals(model143, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res143, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res143^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model143@fit$ics[1])
BIC_list=append(BIC_list, model143@fit$ics[2])
u143=pnorm(res143, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u143, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u143, null="punif")$p.value)

model144=garchFit(formula=~arma(7,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="norm")
res144=residuals(model144, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res144, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res144^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model144@fit$ics[1])
BIC_list=append(BIC_list, model144@fit$ics[2])
u144=pnorm(res144, mean=0, sd=1)[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u144, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u144, null="punif")$p.value)

model145=garchFit(formula=~arma(1,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res145=residuals(model145, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res145, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res145^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model145@fit$ics[1])
BIC_list=append(BIC_list, model145@fit$ics[2])
u145=pged(res145, mean=0, sd=1, nu=tail(model145@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u145, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u145, null="punif")$p.value)

model146=garchFit(formula=~arma(1,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res146=residuals(model146, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res146, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res146^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model146@fit$ics[1])
BIC_list=append(BIC_list, model146@fit$ics[2])
u146=pged(res146, mean=0, sd=1, nu=tail(model146@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u146, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u146, null="punif")$p.value)

model147=garchFit(formula=~arma(1,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res147=residuals(model147, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res147, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res147^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model147@fit$ics[1])
BIC_list=append(BIC_list, model147@fit$ics[2])
u147=pged(res147, mean=0, sd=1, nu=tail(model147@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u147, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u147, null="punif")$p.value)

model148=garchFit(formula=~arma(1,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res148=residuals(model148, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res148, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res148^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model148@fit$ics[1])
BIC_list=append(BIC_list, model148@fit$ics[2])
u148=pged(res148, mean=0, sd=1, nu=tail(model148@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u148, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u148, null="punif")$p.value)

model149=garchFit(formula=~arma(1,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res149=residuals(model149, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res149, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res149^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model149@fit$ics[1])
BIC_list=append(BIC_list, model149@fit$ics[2])
u149=pged(res149, mean=0, sd=1, nu=tail(model149@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u149, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u149, null="punif")$p.value)

model150=garchFit(formula=~arma(1,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res150=residuals(model150, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res150, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res150^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model150@fit$ics[1])
BIC_list=append(BIC_list, model150@fit$ics[2])
u150=pged(res150, mean=0, sd=1, nu=tail(model150@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u150, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u150, null="punif")$p.value)

model151=garchFit(formula=~arma(1,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res151=residuals(model151, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res151, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res151^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model151@fit$ics[1])
BIC_list=append(BIC_list, model151@fit$ics[2])
u151=pged(res151, mean=0, sd=1, nu=tail(model151@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u151, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u151, null="punif")$p.value)

model152=garchFit(formula=~arma(1,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res152=residuals(model152, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res152, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res152^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model152@fit$ics[1])
BIC_list=append(BIC_list, model152@fit$ics[2])
u152=pged(res152, mean=0, sd=1, nu=tail(model152@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u152, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u152, null="punif")$p.value)

model153=garchFit(formula=~arma(1,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res153=residuals(model153, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res153, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res153^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model153@fit$ics[1])
BIC_list=append(BIC_list, model153@fit$ics[2])
u153=pged(res153, mean=0, sd=1, nu=tail(model153@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u153, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u153, null="punif")$p.value)

model154=garchFit(formula=~arma(1,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res154=residuals(model154, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res154, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res154^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model154@fit$ics[1])
BIC_list=append(BIC_list, model154@fit$ics[2])
u154=pged(res154, mean=0, sd=1, nu=tail(model154@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u154, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u154, null="punif")$p.value)

model155=garchFit(formula=~arma(1,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res155=residuals(model155, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res155, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res155^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model155@fit$ics[1])
BIC_list=append(BIC_list, model155@fit$ics[2])
u155=pged(res155, mean=0, sd=1, nu=tail(model155@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u155, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u155, null="punif")$p.value)

model156=garchFit(formula=~arma(1,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res156=residuals(model156, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res156, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res156^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model156@fit$ics[1])
BIC_list=append(BIC_list, model156@fit$ics[2])
u156=pged(res156, mean=0, sd=1, nu=tail(model156@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u156, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u156, null="punif")$p.value)

model157=garchFit(formula=~arma(1,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res157=residuals(model157, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res157, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res157^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model157@fit$ics[1])
BIC_list=append(BIC_list, model157@fit$ics[2])
u157=pged(res157, mean=0, sd=1, nu=tail(model157@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u157, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u157, null="punif")$p.value)

model158=garchFit(formula=~arma(1,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res158=residuals(model158, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res158, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res158^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model158@fit$ics[1])
BIC_list=append(BIC_list, model158@fit$ics[2])
u158=pged(res158, mean=0, sd=1, nu=tail(model158@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u158, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u158, null="punif")$p.value)

model159=garchFit(formula=~arma(1,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res159=residuals(model159, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res159, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res159^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model159@fit$ics[1])
BIC_list=append(BIC_list, model159@fit$ics[2])
u159=pged(res159, mean=0, sd=1, nu=tail(model159@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u159, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u159, null="punif")$p.value)

model160=garchFit(formula=~arma(1,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res160=residuals(model160, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res160, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res160^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model160@fit$ics[1])
BIC_list=append(BIC_list, model160@fit$ics[2])
u160=pged(res160, mean=0, sd=1, nu=tail(model160@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u160, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u160, null="punif")$p.value)

model161=garchFit(formula=~arma(1,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res161=residuals(model161, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res161, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res161^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model161@fit$ics[1])
BIC_list=append(BIC_list, model161@fit$ics[2])
u161=pged(res161, mean=0, sd=1, nu=tail(model161@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u161, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u161, null="punif")$p.value)

model162=garchFit(formula=~arma(1,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res162=residuals(model162, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res162, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res162^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model162@fit$ics[1])
BIC_list=append(BIC_list, model162@fit$ics[2])
u162=pged(res162, mean=0, sd=1, nu=tail(model162@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u162, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u162, null="punif")$p.value)

model163=garchFit(formula=~arma(1,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res163=residuals(model163, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res163, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res163^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model163@fit$ics[1])
BIC_list=append(BIC_list, model163@fit$ics[2])
u163=pged(res163, mean=0, sd=1, nu=tail(model163@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u163, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u163, null="punif")$p.value)

model164=garchFit(formula=~arma(1,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res164=residuals(model164, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res164, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res164^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model164@fit$ics[1])
BIC_list=append(BIC_list, model164@fit$ics[2])
u164=pged(res164, mean=0, sd=1, nu=tail(model164@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u164, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u164, null="punif")$p.value)

model165=garchFit(formula=~arma(1,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res165=residuals(model165, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res165, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res165^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model165@fit$ics[1])
BIC_list=append(BIC_list, model165@fit$ics[2])
u165=pged(res165, mean=0, sd=1, nu=tail(model165@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u165, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u165, null="punif")$p.value)

model166=garchFit(formula=~arma(1,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res166=residuals(model166, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res166, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res166^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model166@fit$ics[1])
BIC_list=append(BIC_list, model166@fit$ics[2])
u166=pged(res166, mean=0, sd=1, nu=tail(model166@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u166, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u166, null="punif")$p.value)

model167=garchFit(formula=~arma(1,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res167=residuals(model167, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res167, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res167^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model167@fit$ics[1])
BIC_list=append(BIC_list, model167@fit$ics[2])
u167=pged(res167, mean=0, sd=1, nu=tail(model167@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u167, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u167, null="punif")$p.value)

model168=garchFit(formula=~arma(1,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res168=residuals(model168, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res168, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res168^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model168@fit$ics[1])
BIC_list=append(BIC_list, model168@fit$ics[2])
u168=pged(res168, mean=0, sd=1, nu=tail(model168@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u168, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u168, null="punif")$p.value)

model169=garchFit(formula=~arma(1,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res169=residuals(model169, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res169, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res169^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model169@fit$ics[1])
BIC_list=append(BIC_list, model169@fit$ics[2])
u169=pged(res169, mean=0, sd=1, nu=tail(model169@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u169, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u169, null="punif")$p.value)

model170=garchFit(formula=~arma(1,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res170=residuals(model170, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res170, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res170^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model170@fit$ics[1])
BIC_list=append(BIC_list, model170@fit$ics[2])
u170=pged(res170, mean=0, sd=1, nu=tail(model170@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u170, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u170, null="punif")$p.value)

model171=garchFit(formula=~arma(1,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res171=residuals(model171, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res171, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res171^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model171@fit$ics[1])
BIC_list=append(BIC_list, model171@fit$ics[2])
u171=pged(res171, mean=0, sd=1, nu=tail(model171@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u171, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u171, null="punif")$p.value)

model172=garchFit(formula=~arma(1,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res172=residuals(model172, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res172, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res172^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model172@fit$ics[1])
BIC_list=append(BIC_list, model172@fit$ics[2])
u172=pged(res172, mean=0, sd=1, nu=tail(model172@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u172, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u172, null="punif")$p.value)

model173=garchFit(formula=~arma(1,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res173=residuals(model173, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res173, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res173^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model173@fit$ics[1])
BIC_list=append(BIC_list, model173@fit$ics[2])
u173=pged(res173, mean=0, sd=1, nu=tail(model173@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u173, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u173, null="punif")$p.value)

model174=garchFit(formula=~arma(1,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res174=residuals(model174, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res174, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res174^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model174@fit$ics[1])
BIC_list=append(BIC_list, model174@fit$ics[2])
u174=pged(res174, mean=0, sd=1, nu=tail(model174@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u174, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u174, null="punif")$p.value)

model175=garchFit(formula=~arma(1,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res175=residuals(model175, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res175, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res175^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model175@fit$ics[1])
BIC_list=append(BIC_list, model175@fit$ics[2])
u175=pged(res175, mean=0, sd=1, nu=tail(model175@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u175, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u175, null="punif")$p.value)

model176=garchFit(formula=~arma(1,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res176=residuals(model176, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res176, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res176^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model176@fit$ics[1])
BIC_list=append(BIC_list, model176@fit$ics[2])
u176=pged(res176, mean=0, sd=1, nu=tail(model176@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u176, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u176, null="punif")$p.value)

model177=garchFit(formula=~arma(1,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res177=residuals(model177, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res177, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res177^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model177@fit$ics[1])
BIC_list=append(BIC_list, model177@fit$ics[2])
u177=pged(res177, mean=0, sd=1, nu=tail(model177@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u177, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u177, null="punif")$p.value)

model178=garchFit(formula=~arma(1,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res178=residuals(model178, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res178, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res178^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model178@fit$ics[1])
BIC_list=append(BIC_list, model178@fit$ics[2])
u178=pged(res178, mean=0, sd=1, nu=tail(model178@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u178, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u178, null="punif")$p.value)

model179=garchFit(formula=~arma(1,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res179=residuals(model179, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res179, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res179^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model179@fit$ics[1])
BIC_list=append(BIC_list, model179@fit$ics[2])
u179=pged(res179, mean=0, sd=1, nu=tail(model179@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u179, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u179, null="punif")$p.value)

model180=garchFit(formula=~arma(1,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res180=residuals(model180, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res180, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res180^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model180@fit$ics[1])
BIC_list=append(BIC_list, model180@fit$ics[2])
u180=pged(res180, mean=0, sd=1, nu=tail(model180@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u180, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u180, null="punif")$p.value)

model181=garchFit(formula=~arma(3,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res181=residuals(model181, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res181, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res181^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model181@fit$ics[1])
BIC_list=append(BIC_list, model181@fit$ics[2])
u181=pged(res181, mean=0, sd=1, nu=tail(model181@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u181, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u181, null="punif")$p.value)

model182=garchFit(formula=~arma(3,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res182=residuals(model182, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res182, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res182^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model182@fit$ics[1])
BIC_list=append(BIC_list, model182@fit$ics[2])
u182=pged(res182, mean=0, sd=1, nu=tail(model182@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u182, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u182, null="punif")$p.value)

model183=garchFit(formula=~arma(3,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res183=residuals(model183, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res183, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res183^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model183@fit$ics[1])
BIC_list=append(BIC_list, model183@fit$ics[2])
u183=pged(res183, mean=0, sd=1, nu=tail(model183@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u183, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u183, null="punif")$p.value)

model184=garchFit(formula=~arma(3,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res184=residuals(model184, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res184, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res184^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model184@fit$ics[1])
BIC_list=append(BIC_list, model184@fit$ics[2])
u184=pged(res184, mean=0, sd=1, nu=tail(model184@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u184, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u184, null="punif")$p.value)

model185=garchFit(formula=~arma(3,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res185=residuals(model185, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res185, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res185^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model185@fit$ics[1])
BIC_list=append(BIC_list, model185@fit$ics[2])
u185=pged(res185, mean=0, sd=1, nu=tail(model185@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u185, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u185, null="punif")$p.value)

model186=garchFit(formula=~arma(3,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res186=residuals(model186, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res186, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res186^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model186@fit$ics[1])
BIC_list=append(BIC_list, model186@fit$ics[2])
u186=pged(res186, mean=0, sd=1, nu=tail(model186@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u186, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u186, null="punif")$p.value)

model187=garchFit(formula=~arma(3,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res187=residuals(model187, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res187, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res187^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model187@fit$ics[1])
BIC_list=append(BIC_list, model187@fit$ics[2])
u187=pged(res187, mean=0, sd=1, nu=tail(model187@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u187, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u187, null="punif")$p.value)

model188=garchFit(formula=~arma(3,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res188=residuals(model188, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res188, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res188^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model188@fit$ics[1])
BIC_list=append(BIC_list, model188@fit$ics[2])
u188=pged(res188, mean=0, sd=1, nu=tail(model188@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u188, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u188, null="punif")$p.value)

model189=garchFit(formula=~arma(3,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res189=residuals(model189, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res189, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res189^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model189@fit$ics[1])
BIC_list=append(BIC_list, model189@fit$ics[2])
u189=pged(res189, mean=0, sd=1, nu=tail(model189@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u189, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u189, null="punif")$p.value)

model190=garchFit(formula=~arma(3,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res190=residuals(model190, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res190, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res190^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model190@fit$ics[1])
BIC_list=append(BIC_list, model190@fit$ics[2])
u190=pged(res190, mean=0, sd=1, nu=tail(model190@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u190, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u190, null="punif")$p.value)

model191=garchFit(formula=~arma(3,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res191=residuals(model191, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res191, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res191^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model191@fit$ics[1])
BIC_list=append(BIC_list, model191@fit$ics[2])
u191=pged(res191, mean=0, sd=1, nu=tail(model191@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u191, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u191, null="punif")$p.value)

model192=garchFit(formula=~arma(3,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res192=residuals(model192, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res192, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res192^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model192@fit$ics[1])
BIC_list=append(BIC_list, model192@fit$ics[2])
u192=pged(res192, mean=0, sd=1, nu=tail(model192@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u192, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u192, null="punif")$p.value)

model193=garchFit(formula=~arma(3,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res193=residuals(model193, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res193, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res193^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model193@fit$ics[1])
BIC_list=append(BIC_list, model193@fit$ics[2])
u193=pged(res193, mean=0, sd=1, nu=tail(model193@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u193, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u193, null="punif")$p.value)

model194=garchFit(formula=~arma(3,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res194=residuals(model194, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res194, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res194^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model194@fit$ics[1])
BIC_list=append(BIC_list, model194@fit$ics[2])
u194=pged(res194, mean=0, sd=1, nu=tail(model194@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u194, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u194, null="punif")$p.value)

model195=garchFit(formula=~arma(3,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res195=residuals(model195, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res195, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res195^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model195@fit$ics[1])
BIC_list=append(BIC_list, model195@fit$ics[2])
u195=pged(res195, mean=0, sd=1, nu=tail(model195@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u195, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u195, null="punif")$p.value)

model196=garchFit(formula=~arma(3,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res196=residuals(model196, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res196, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res196^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model196@fit$ics[1])
BIC_list=append(BIC_list, model196@fit$ics[2])
u196=pged(res196, mean=0, sd=1, nu=tail(model196@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u196, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u196, null="punif")$p.value)

model197=garchFit(formula=~arma(3,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res197=residuals(model197, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res197, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res197^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model197@fit$ics[1])
BIC_list=append(BIC_list, model197@fit$ics[2])
u197=pged(res197, mean=0, sd=1, nu=tail(model197@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u197, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u197, null="punif")$p.value)

model198=garchFit(formula=~arma(3,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res198=residuals(model198, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res198, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res198^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model198@fit$ics[1])
BIC_list=append(BIC_list, model198@fit$ics[2])
u198=pged(res198, mean=0, sd=1, nu=tail(model198@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u198, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u198, null="punif")$p.value)

model199=garchFit(formula=~arma(3,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res199=residuals(model199, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res199, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res199^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model199@fit$ics[1])
BIC_list=append(BIC_list, model199@fit$ics[2])
u199=pged(res199, mean=0, sd=1, nu=tail(model199@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u199, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u199, null="punif")$p.value)

model200=garchFit(formula=~arma(3,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res200=residuals(model200, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res200, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res200^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model200@fit$ics[1])
BIC_list=append(BIC_list, model200@fit$ics[2])
u200=pged(res200, mean=0, sd=1, nu=tail(model200@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u200, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u200, null="punif")$p.value)

model201=garchFit(formula=~arma(3,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res201=residuals(model201, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res201, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res201^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model201@fit$ics[1])
BIC_list=append(BIC_list, model201@fit$ics[2])
u201=pged(res201, mean=0, sd=1, nu=tail(model201@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u201, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u201, null="punif")$p.value)

model202=garchFit(formula=~arma(3,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res202=residuals(model202, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res202, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res202^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model202@fit$ics[1])
BIC_list=append(BIC_list, model202@fit$ics[2])
u202=pged(res202, mean=0, sd=1, nu=tail(model202@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u202, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u202, null="punif")$p.value)

model203=garchFit(formula=~arma(3,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res203=residuals(model203, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res203, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res203^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model203@fit$ics[1])
BIC_list=append(BIC_list, model203@fit$ics[2])
u203=pged(res203, mean=0, sd=1, nu=tail(model203@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u203, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u203, null="punif")$p.value)

model204=garchFit(formula=~arma(3,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res204=residuals(model204, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res204, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res204^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model204@fit$ics[1])
BIC_list=append(BIC_list, model204@fit$ics[2])
u204=pged(res204, mean=0, sd=1, nu=tail(model204@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u204, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u204, null="punif")$p.value)

model205=garchFit(formula=~arma(3,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res205=residuals(model205, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res205, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res205^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model205@fit$ics[1])
BIC_list=append(BIC_list, model205@fit$ics[2])
u205=pged(res205, mean=0, sd=1, nu=tail(model205@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u205, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u205, null="punif")$p.value)

model206=garchFit(formula=~arma(3,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res206=residuals(model206, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res206, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res206^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model206@fit$ics[1])
BIC_list=append(BIC_list, model206@fit$ics[2])
u206=pged(res206, mean=0, sd=1, nu=tail(model206@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u206, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u206, null="punif")$p.value)

model207=garchFit(formula=~arma(3,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res207=residuals(model207, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res207, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res207^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model207@fit$ics[1])
BIC_list=append(BIC_list, model207@fit$ics[2])
u207=pged(res207, mean=0, sd=1, nu=tail(model207@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u207, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u207, null="punif")$p.value)

model208=garchFit(formula=~arma(3,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res208=residuals(model208, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res208, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res208^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model208@fit$ics[1])
BIC_list=append(BIC_list, model208@fit$ics[2])
u208=pged(res208, mean=0, sd=1, nu=tail(model208@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u208, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u208, null="punif")$p.value)

model209=garchFit(formula=~arma(3,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res209=residuals(model209, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res209, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res209^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model209@fit$ics[1])
BIC_list=append(BIC_list, model209@fit$ics[2])
u209=pged(res209, mean=0, sd=1, nu=tail(model209@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u209, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u209, null="punif")$p.value)

model210=garchFit(formula=~arma(3,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res210=residuals(model210, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res210, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res210^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model210@fit$ics[1])
BIC_list=append(BIC_list, model210@fit$ics[2])
u210=pged(res210, mean=0, sd=1, nu=tail(model210@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u210, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u210, null="punif")$p.value)

model211=garchFit(formula=~arma(3,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res211=residuals(model211, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res211, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res211^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model211@fit$ics[1])
BIC_list=append(BIC_list, model211@fit$ics[2])
u211=pged(res211, mean=0, sd=1, nu=tail(model211@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u211, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u211, null="punif")$p.value)

model212=garchFit(formula=~arma(3,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res212=residuals(model212, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res212, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res212^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model212@fit$ics[1])
BIC_list=append(BIC_list, model212@fit$ics[2])
u212=pged(res212, mean=0, sd=1, nu=tail(model212@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u212, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u212, null="punif")$p.value)

model213=garchFit(formula=~arma(3,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res213=residuals(model213, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res213, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res213^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model213@fit$ics[1])
BIC_list=append(BIC_list, model213@fit$ics[2])
u213=pged(res213, mean=0, sd=1, nu=tail(model213@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u213, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u213, null="punif")$p.value)

model214=garchFit(formula=~arma(3,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res214=residuals(model214, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res214, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res214^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model214@fit$ics[1])
BIC_list=append(BIC_list, model214@fit$ics[2])
u214=pged(res214, mean=0, sd=1, nu=tail(model214@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u214, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u214, null="punif")$p.value)

model215=garchFit(formula=~arma(3,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res215=residuals(model215, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res215, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res215^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model215@fit$ics[1])
BIC_list=append(BIC_list, model215@fit$ics[2])
u215=pged(res215, mean=0, sd=1, nu=tail(model215@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u215, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u215, null="punif")$p.value)

model216=garchFit(formula=~arma(3,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res216=residuals(model216, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res216, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res216^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model216@fit$ics[1])
BIC_list=append(BIC_list, model216@fit$ics[2])
u216=pged(res216, mean=0, sd=1, nu=tail(model216@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u216, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u216, null="punif")$p.value)

model217=garchFit(formula=~arma(4,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res217=residuals(model217, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res217, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res217^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model217@fit$ics[1])
BIC_list=append(BIC_list, model217@fit$ics[2])
u217=pged(res217, mean=0, sd=1, nu=tail(model217@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u217, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u217, null="punif")$p.value)

model218=garchFit(formula=~arma(4,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res218=residuals(model218, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res218, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res218^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model218@fit$ics[1])
BIC_list=append(BIC_list, model218@fit$ics[2])
u218=pged(res218, mean=0, sd=1, nu=tail(model218@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u218, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u218, null="punif")$p.value)

model219=garchFit(formula=~arma(4,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res219=residuals(model219, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res219, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res219^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model219@fit$ics[1])
BIC_list=append(BIC_list, model219@fit$ics[2])
u219=pged(res219, mean=0, sd=1, nu=tail(model219@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u219, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u219, null="punif")$p.value)

model220=garchFit(formula=~arma(4,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res220=residuals(model220, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res220, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res220^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model220@fit$ics[1])
BIC_list=append(BIC_list, model220@fit$ics[2])
u220=pged(res220, mean=0, sd=1, nu=tail(model220@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u220, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u220, null="punif")$p.value)

model221=garchFit(formula=~arma(4,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res221=residuals(model221, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res221, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res221^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model221@fit$ics[1])
BIC_list=append(BIC_list, model221@fit$ics[2])
u221=pged(res221, mean=0, sd=1, nu=tail(model221@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u221, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u221, null="punif")$p.value)

model222=garchFit(formula=~arma(4,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res222=residuals(model222, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res222, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res222^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model222@fit$ics[1])
BIC_list=append(BIC_list, model222@fit$ics[2])
u222=pged(res222, mean=0, sd=1, nu=tail(model222@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u222, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u222, null="punif")$p.value)

model223=garchFit(formula=~arma(4,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res223=residuals(model223, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res223, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res223^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model223@fit$ics[1])
BIC_list=append(BIC_list, model223@fit$ics[2])
u223=pged(res223, mean=0, sd=1, nu=tail(model223@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u223, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u223, null="punif")$p.value)

model224=garchFit(formula=~arma(4,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res224=residuals(model224, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res224, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res224^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model224@fit$ics[1])
BIC_list=append(BIC_list, model224@fit$ics[2])
u224=pged(res224, mean=0, sd=1, nu=tail(model224@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u224, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u224, null="punif")$p.value)

model225=garchFit(formula=~arma(4,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res225=residuals(model225, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res225, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res225^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model225@fit$ics[1])
BIC_list=append(BIC_list, model225@fit$ics[2])
u225=pged(res225, mean=0, sd=1, nu=tail(model225@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u225, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u225, null="punif")$p.value)

model226=garchFit(formula=~arma(4,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res226=residuals(model226, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res226, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res226^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model226@fit$ics[1])
BIC_list=append(BIC_list, model226@fit$ics[2])
u226=pged(res226, mean=0, sd=1, nu=tail(model226@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u226, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u226, null="punif")$p.value)

model227=garchFit(formula=~arma(4,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res227=residuals(model227, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res227, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res227^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model227@fit$ics[1])
BIC_list=append(BIC_list, model227@fit$ics[2])
u227=pged(res227, mean=0, sd=1, nu=tail(model227@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u227, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u227, null="punif")$p.value)

model228=garchFit(formula=~arma(4,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res228=residuals(model228, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res228, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res228^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model228@fit$ics[1])
BIC_list=append(BIC_list, model228@fit$ics[2])
u228=pged(res228, mean=0, sd=1, nu=tail(model228@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u228, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u228, null="punif")$p.value)

model229=garchFit(formula=~arma(4,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res229=residuals(model229, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res229, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res229^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model229@fit$ics[1])
BIC_list=append(BIC_list, model229@fit$ics[2])
u229=pged(res229, mean=0, sd=1, nu=tail(model229@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u229, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u229, null="punif")$p.value)

model230=garchFit(formula=~arma(4,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res230=residuals(model230, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res230, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res230^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model230@fit$ics[1])
BIC_list=append(BIC_list, model230@fit$ics[2])
u230=pged(res230, mean=0, sd=1, nu=tail(model230@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u230, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u230, null="punif")$p.value)

model231=garchFit(formula=~arma(4,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res231=residuals(model231, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res231, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res231^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model231@fit$ics[1])
BIC_list=append(BIC_list, model231@fit$ics[2])
u231=pged(res231, mean=0, sd=1, nu=tail(model231@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u231, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u231, null="punif")$p.value)

model232=garchFit(formula=~arma(4,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res232=residuals(model232, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res232, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res232^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model232@fit$ics[1])
BIC_list=append(BIC_list, model232@fit$ics[2])
u232=pged(res232, mean=0, sd=1, nu=tail(model232@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u232, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u232, null="punif")$p.value)

model233=garchFit(formula=~arma(4,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res233=residuals(model233, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res233, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res233^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model233@fit$ics[1])
BIC_list=append(BIC_list, model233@fit$ics[2])
u233=pged(res233, mean=0, sd=1, nu=tail(model233@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u233, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u233, null="punif")$p.value)

model234=garchFit(formula=~arma(4,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res234=residuals(model234, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res234, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res234^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model234@fit$ics[1])
BIC_list=append(BIC_list, model234@fit$ics[2])
u234=pged(res234, mean=0, sd=1, nu=tail(model234@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u234, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u234, null="punif")$p.value)

model235=garchFit(formula=~arma(4,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res235=residuals(model235, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res235, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res235^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model235@fit$ics[1])
BIC_list=append(BIC_list, model235@fit$ics[2])
u235=pged(res235, mean=0, sd=1, nu=tail(model235@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u235, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u235, null="punif")$p.value)

model236=garchFit(formula=~arma(4,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res236=residuals(model236, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res236, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res236^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model236@fit$ics[1])
BIC_list=append(BIC_list, model236@fit$ics[2])
u236=pged(res236, mean=0, sd=1, nu=tail(model236@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u236, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u236, null="punif")$p.value)

model237=garchFit(formula=~arma(4,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res237=residuals(model237, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res237, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res237^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model237@fit$ics[1])
BIC_list=append(BIC_list, model237@fit$ics[2])
u237=pged(res237, mean=0, sd=1, nu=tail(model237@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u237, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u237, null="punif")$p.value)

model238=garchFit(formula=~arma(4,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res238=residuals(model238, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res238, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res238^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model238@fit$ics[1])
BIC_list=append(BIC_list, model238@fit$ics[2])
u238=pged(res238, mean=0, sd=1, nu=tail(model238@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u238, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u238, null="punif")$p.value)

model239=garchFit(formula=~arma(4,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res239=residuals(model239, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res239, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res239^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model239@fit$ics[1])
BIC_list=append(BIC_list, model239@fit$ics[2])
u239=pged(res239, mean=0, sd=1, nu=tail(model239@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u239, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u239, null="punif")$p.value)

model240=garchFit(formula=~arma(4,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res240=residuals(model240, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res240, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res240^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model240@fit$ics[1])
BIC_list=append(BIC_list, model240@fit$ics[2])
u240=pged(res240, mean=0, sd=1, nu=tail(model240@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u240, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u240, null="punif")$p.value)

model241=garchFit(formula=~arma(4,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res241=residuals(model241, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res241, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res241^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model241@fit$ics[1])
BIC_list=append(BIC_list, model241@fit$ics[2])
u241=pged(res241, mean=0, sd=1, nu=tail(model241@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u241, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u241, null="punif")$p.value)

model242=garchFit(formula=~arma(4,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res242=residuals(model242, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res242, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res242^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model242@fit$ics[1])
BIC_list=append(BIC_list, model242@fit$ics[2])
u242=pged(res242, mean=0, sd=1, nu=tail(model242@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u242, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u242, null="punif")$p.value)

model243=garchFit(formula=~arma(4,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res243=residuals(model243, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res243, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res243^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model243@fit$ics[1])
BIC_list=append(BIC_list, model243@fit$ics[2])
u243=pged(res243, mean=0, sd=1, nu=tail(model243@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u243, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u243, null="punif")$p.value)

model244=garchFit(formula=~arma(4,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res244=residuals(model244, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res244, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res244^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model244@fit$ics[1])
BIC_list=append(BIC_list, model244@fit$ics[2])
u244=pged(res244, mean=0, sd=1, nu=tail(model244@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u244, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u244, null="punif")$p.value)

model245=garchFit(formula=~arma(4,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res245=residuals(model245, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res245, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res245^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model245@fit$ics[1])
BIC_list=append(BIC_list, model245@fit$ics[2])
u245=pged(res245, mean=0, sd=1, nu=tail(model245@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u245, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u245, null="punif")$p.value)

model246=garchFit(formula=~arma(4,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res246=residuals(model246, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res246, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res246^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model246@fit$ics[1])
BIC_list=append(BIC_list, model246@fit$ics[2])
u246=pged(res246, mean=0, sd=1, nu=tail(model246@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u246, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u246, null="punif")$p.value)

model247=garchFit(formula=~arma(4,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res247=residuals(model247, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res247, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res247^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model247@fit$ics[1])
BIC_list=append(BIC_list, model247@fit$ics[2])
u247=pged(res247, mean=0, sd=1, nu=tail(model247@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u247, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u247, null="punif")$p.value)

model248=garchFit(formula=~arma(4,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res248=residuals(model248, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res248, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res248^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model248@fit$ics[1])
BIC_list=append(BIC_list, model248@fit$ics[2])
u248=pged(res248, mean=0, sd=1, nu=tail(model248@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u248, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u248, null="punif")$p.value)

model249=garchFit(formula=~arma(4,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res249=residuals(model249, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res249, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res249^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model249@fit$ics[1])
BIC_list=append(BIC_list, model249@fit$ics[2])
u249=pged(res249, mean=0, sd=1, nu=tail(model249@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u249, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u249, null="punif")$p.value)

model250=garchFit(formula=~arma(4,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res250=residuals(model250, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res250, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res250^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model250@fit$ics[1])
BIC_list=append(BIC_list, model250@fit$ics[2])
u250=pged(res250, mean=0, sd=1, nu=tail(model250@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u250, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u250, null="punif")$p.value)

model251=garchFit(formula=~arma(4,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res251=residuals(model251, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res251, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res251^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model251@fit$ics[1])
BIC_list=append(BIC_list, model251@fit$ics[2])
u251=pged(res251, mean=0, sd=1, nu=tail(model251@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u251, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u251, null="punif")$p.value)

model252=garchFit(formula=~arma(4,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res252=residuals(model252, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res252, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res252^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model252@fit$ics[1])
BIC_list=append(BIC_list, model252@fit$ics[2])
u252=pged(res252, mean=0, sd=1, nu=tail(model252@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u252, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u252, null="punif")$p.value)

model253=garchFit(formula=~arma(7,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res253=residuals(model253, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res253, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res253^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model253@fit$ics[1])
BIC_list=append(BIC_list, model253@fit$ics[2])
u253=pged(res253, mean=0, sd=1, nu=tail(model253@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u253, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u253, null="punif")$p.value)

model254=garchFit(formula=~arma(7,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res254=residuals(model254, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res254, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res254^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model254@fit$ics[1])
BIC_list=append(BIC_list, model254@fit$ics[2])
u254=pged(res254, mean=0, sd=1, nu=tail(model254@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u254, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u254, null="punif")$p.value)

model255=garchFit(formula=~arma(7,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res255=residuals(model255, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res255, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res255^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model255@fit$ics[1])
BIC_list=append(BIC_list, model255@fit$ics[2])
u255=pged(res255, mean=0, sd=1, nu=tail(model255@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u255, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u255, null="punif")$p.value)

model256=garchFit(formula=~arma(7,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res256=residuals(model256, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res256, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res256^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model256@fit$ics[1])
BIC_list=append(BIC_list, model256@fit$ics[2])
u256=pged(res256, mean=0, sd=1, nu=tail(model256@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u256, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u256, null="punif")$p.value)

model257=garchFit(formula=~arma(7,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res257=residuals(model257, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res257, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res257^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model257@fit$ics[1])
BIC_list=append(BIC_list, model257@fit$ics[2])
u257=pged(res257, mean=0, sd=1, nu=tail(model257@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u257, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u257, null="punif")$p.value)

model258=garchFit(formula=~arma(7,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res258=residuals(model258, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res258, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res258^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model258@fit$ics[1])
BIC_list=append(BIC_list, model258@fit$ics[2])
u258=pged(res258, mean=0, sd=1, nu=tail(model258@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u258, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u258, null="punif")$p.value)

model259=garchFit(formula=~arma(7,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res259=residuals(model259, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res259, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res259^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model259@fit$ics[1])
BIC_list=append(BIC_list, model259@fit$ics[2])
u259=pged(res259, mean=0, sd=1, nu=tail(model259@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u259, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u259, null="punif")$p.value)

model260=garchFit(formula=~arma(7,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res260=residuals(model260, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res260, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res260^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model260@fit$ics[1])
BIC_list=append(BIC_list, model260@fit$ics[2])
u260=pged(res260, mean=0, sd=1, nu=tail(model260@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u260, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u260, null="punif")$p.value)

model261=garchFit(formula=~arma(7,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res261=residuals(model261, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res261, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res261^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model261@fit$ics[1])
BIC_list=append(BIC_list, model261@fit$ics[2])
u261=pged(res261, mean=0, sd=1, nu=tail(model261@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u261, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u261, null="punif")$p.value)

model262=garchFit(formula=~arma(7,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res262=residuals(model262, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res262, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res262^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model262@fit$ics[1])
BIC_list=append(BIC_list, model262@fit$ics[2])
u262=pged(res262, mean=0, sd=1, nu=tail(model262@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u262, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u262, null="punif")$p.value)

model263=garchFit(formula=~arma(7,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res263=residuals(model263, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res263, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res263^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model263@fit$ics[1])
BIC_list=append(BIC_list, model263@fit$ics[2])
u263=pged(res263, mean=0, sd=1, nu=tail(model263@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u263, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u263, null="punif")$p.value)

model264=garchFit(formula=~arma(7,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res264=residuals(model264, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res264, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res264^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model264@fit$ics[1])
BIC_list=append(BIC_list, model264@fit$ics[2])
u264=pged(res264, mean=0, sd=1, nu=tail(model264@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u264, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u264, null="punif")$p.value)

model265=garchFit(formula=~arma(7,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res265=residuals(model265, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res265, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res265^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model265@fit$ics[1])
BIC_list=append(BIC_list, model265@fit$ics[2])
u265=pged(res265, mean=0, sd=1, nu=tail(model265@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u265, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u265, null="punif")$p.value)

model266=garchFit(formula=~arma(7,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res266=residuals(model266, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res266, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res266^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model266@fit$ics[1])
BIC_list=append(BIC_list, model266@fit$ics[2])
u266=pged(res266, mean=0, sd=1, nu=tail(model266@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u266, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u266, null="punif")$p.value)

model267=garchFit(formula=~arma(7,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res267=residuals(model267, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res267, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res267^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model267@fit$ics[1])
BIC_list=append(BIC_list, model267@fit$ics[2])
u267=pged(res267, mean=0, sd=1, nu=tail(model267@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u267, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u267, null="punif")$p.value)

model268=garchFit(formula=~arma(7,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res268=residuals(model268, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res268, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res268^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model268@fit$ics[1])
BIC_list=append(BIC_list, model268@fit$ics[2])
u268=pged(res268, mean=0, sd=1, nu=tail(model268@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u268, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u268, null="punif")$p.value)

model269=garchFit(formula=~arma(7,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res269=residuals(model269, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res269, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res269^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model269@fit$ics[1])
BIC_list=append(BIC_list, model269@fit$ics[2])
u269=pged(res269, mean=0, sd=1, nu=tail(model269@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u269, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u269, null="punif")$p.value)

model270=garchFit(formula=~arma(7,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res270=residuals(model270, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res270, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res270^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model270@fit$ics[1])
BIC_list=append(BIC_list, model270@fit$ics[2])
u270=pged(res270, mean=0, sd=1, nu=tail(model270@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u270, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u270, null="punif")$p.value)

model271=garchFit(formula=~arma(7,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res271=residuals(model271, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res271, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res271^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model271@fit$ics[1])
BIC_list=append(BIC_list, model271@fit$ics[2])
u271=pged(res271, mean=0, sd=1, nu=tail(model271@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u271, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u271, null="punif")$p.value)

model272=garchFit(formula=~arma(7,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res272=residuals(model272, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res272, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res272^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model272@fit$ics[1])
BIC_list=append(BIC_list, model272@fit$ics[2])
u272=pged(res272, mean=0, sd=1, nu=tail(model272@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u272, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u272, null="punif")$p.value)

model273=garchFit(formula=~arma(7,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res273=residuals(model273, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res273, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res273^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model273@fit$ics[1])
BIC_list=append(BIC_list, model273@fit$ics[2])
u273=pged(res273, mean=0, sd=1, nu=tail(model273@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u273, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u273, null="punif")$p.value)

model274=garchFit(formula=~arma(7,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res274=residuals(model274, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res274, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res274^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model274@fit$ics[1])
BIC_list=append(BIC_list, model274@fit$ics[2])
u274=pged(res274, mean=0, sd=1, nu=tail(model274@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u274, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u274, null="punif")$p.value)

model275=garchFit(formula=~arma(7,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res275=residuals(model275, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res275, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res275^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model275@fit$ics[1])
BIC_list=append(BIC_list, model275@fit$ics[2])
u275=pged(res275, mean=0, sd=1, nu=tail(model275@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u275, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u275, null="punif")$p.value)

model276=garchFit(formula=~arma(7,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res276=residuals(model276, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res276, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res276^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model276@fit$ics[1])
BIC_list=append(BIC_list, model276@fit$ics[2])
u276=pged(res276, mean=0, sd=1, nu=tail(model276@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u276, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u276, null="punif")$p.value)

model277=garchFit(formula=~arma(7,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res277=residuals(model277, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res277, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res277^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model277@fit$ics[1])
BIC_list=append(BIC_list, model277@fit$ics[2])
u277=pged(res277, mean=0, sd=1, nu=tail(model277@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u277, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u277, null="punif")$p.value)

model278=garchFit(formula=~arma(7,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res278=residuals(model278, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res278, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res278^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model278@fit$ics[1])
BIC_list=append(BIC_list, model278@fit$ics[2])
u278=pged(res278, mean=0, sd=1, nu=tail(model278@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u278, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u278, null="punif")$p.value)

model279=garchFit(formula=~arma(7,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res279=residuals(model279, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res279, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res279^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model279@fit$ics[1])
BIC_list=append(BIC_list, model279@fit$ics[2])
u279=pged(res279, mean=0, sd=1, nu=tail(model279@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u279, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u279, null="punif")$p.value)

model280=garchFit(formula=~arma(7,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="ged")
res280=residuals(model280, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res280, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res280^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model280@fit$ics[1])
BIC_list=append(BIC_list, model280@fit$ics[2])
u280=pged(res280, mean=0, sd=1, nu=tail(model280@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u280, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u280, null="punif")$p.value)

model281=garchFit(formula=~arma(7,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="ged")
res281=residuals(model281, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res281, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res281^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model281@fit$ics[1])
BIC_list=append(BIC_list, model281@fit$ics[2])
u281=pged(res281, mean=0, sd=1, nu=tail(model281@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u281, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u281, null="punif")$p.value)

model282=garchFit(formula=~arma(7,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="ged")
res282=residuals(model282, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res282, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res282^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model282@fit$ics[1])
BIC_list=append(BIC_list, model282@fit$ics[2])
u282=pged(res282, mean=0, sd=1, nu=tail(model282@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u282, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u282, null="punif")$p.value)

model283=garchFit(formula=~arma(7,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="ged")
res283=residuals(model283, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res283, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res283^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model283@fit$ics[1])
BIC_list=append(BIC_list, model283@fit$ics[2])
u283=pged(res283, mean=0, sd=1, nu=tail(model283@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u283, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u283, null="punif")$p.value)

model284=garchFit(formula=~arma(7,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="ged")
res284=residuals(model284, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res284, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res284^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model284@fit$ics[1])
BIC_list=append(BIC_list, model284@fit$ics[2])
u284=pged(res284, mean=0, sd=1, nu=tail(model284@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u284, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u284, null="punif")$p.value)

model285=garchFit(formula=~arma(7,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="ged")
res285=residuals(model285, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res285, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res285^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model285@fit$ics[1])
BIC_list=append(BIC_list, model285@fit$ics[2])
u285=pged(res285, mean=0, sd=1, nu=tail(model285@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u285, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u285, null="punif")$p.value)

model286=garchFit(formula=~arma(7,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="ged")
res286=residuals(model286, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res286, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res286^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model286@fit$ics[1])
BIC_list=append(BIC_list, model286@fit$ics[2])
u286=pged(res286, mean=0, sd=1, nu=tail(model286@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u286, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u286, null="punif")$p.value)

model287=garchFit(formula=~arma(7,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="ged")
res287=residuals(model287, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res287, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res287^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model287@fit$ics[1])
BIC_list=append(BIC_list, model287@fit$ics[2])
u287=pged(res287, mean=0, sd=1, nu=tail(model287@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u287, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u287, null="punif")$p.value)

model288=garchFit(formula=~arma(7,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="ged")
res288=residuals(model288, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res288, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res288^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model288@fit$ics[1])
BIC_list=append(BIC_list, model288@fit$ics[2])
u288=pged(res288, mean=0, sd=1, nu=tail(model288@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u288, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u288, null="punif")$p.value)

model289=garchFit(formula=~arma(1,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res289=residuals(model289, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res289, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res289^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model289@fit$ics[1])
BIC_list=append(BIC_list, model289@fit$ics[2])
u289=pstd(res289, mean=0, sd=1, nu=tail(model289@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u289, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u289, null="punif")$p.value)

model290=garchFit(formula=~arma(1,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res290=residuals(model290, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res290, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res290^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model290@fit$ics[1])
BIC_list=append(BIC_list, model290@fit$ics[2])
u290=pstd(res290, mean=0, sd=1, nu=tail(model290@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u290, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u290, null="punif")$p.value)

model291=garchFit(formula=~arma(1,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res291=residuals(model291, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res291, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res291^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model291@fit$ics[1])
BIC_list=append(BIC_list, model291@fit$ics[2])
u291=pstd(res291, mean=0, sd=1, nu=tail(model291@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u291, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u291, null="punif")$p.value)

model292=garchFit(formula=~arma(1,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res292=residuals(model292, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res292, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res292^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model292@fit$ics[1])
BIC_list=append(BIC_list, model292@fit$ics[2])
u292=pstd(res292, mean=0, sd=1, nu=tail(model292@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u292, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u292, null="punif")$p.value)

model293=garchFit(formula=~arma(1,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res293=residuals(model293, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res293, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res293^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model293@fit$ics[1])
BIC_list=append(BIC_list, model293@fit$ics[2])
u293=pstd(res293, mean=0, sd=1, nu=tail(model293@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u293, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u293, null="punif")$p.value)

model294=garchFit(formula=~arma(1,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res294=residuals(model294, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res294, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res294^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model294@fit$ics[1])
BIC_list=append(BIC_list, model294@fit$ics[2])
u294=pstd(res294, mean=0, sd=1, nu=tail(model294@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u294, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u294, null="punif")$p.value)

model295=garchFit(formula=~arma(1,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res295=residuals(model295, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res295, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res295^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model295@fit$ics[1])
BIC_list=append(BIC_list, model295@fit$ics[2])
u295=pstd(res295, mean=0, sd=1, nu=tail(model295@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u295, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u295, null="punif")$p.value)

model296=garchFit(formula=~arma(1,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res296=residuals(model296, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res296, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res296^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model296@fit$ics[1])
BIC_list=append(BIC_list, model296@fit$ics[2])
u296=pstd(res296, mean=0, sd=1, nu=tail(model296@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u296, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u296, null="punif")$p.value)

model297=garchFit(formula=~arma(1,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res297=residuals(model297, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res297, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res297^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model297@fit$ics[1])
BIC_list=append(BIC_list, model297@fit$ics[2])
u297=pstd(res297, mean=0, sd=1, nu=tail(model297@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u297, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u297, null="punif")$p.value)

model298=garchFit(formula=~arma(1,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res298=residuals(model298, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res298, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res298^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model298@fit$ics[1])
BIC_list=append(BIC_list, model298@fit$ics[2])
u298=pstd(res298, mean=0, sd=1, nu=tail(model298@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u298, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u298, null="punif")$p.value)

model299=garchFit(formula=~arma(1,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res299=residuals(model299, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res299, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res299^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model299@fit$ics[1])
BIC_list=append(BIC_list, model299@fit$ics[2])
u299=pstd(res299, mean=0, sd=1, nu=tail(model299@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u299, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u299, null="punif")$p.value)

model300=garchFit(formula=~arma(1,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res300=residuals(model300, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res300, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res300^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model300@fit$ics[1])
BIC_list=append(BIC_list, model300@fit$ics[2])
u300=pstd(res300, mean=0, sd=1, nu=tail(model300@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u300, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u300, null="punif")$p.value)

model301=garchFit(formula=~arma(1,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res301=residuals(model301, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res301, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res301^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model301@fit$ics[1])
BIC_list=append(BIC_list, model301@fit$ics[2])
u301=pstd(res301, mean=0, sd=1, nu=tail(model301@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u301, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u301, null="punif")$p.value)

model302=garchFit(formula=~arma(1,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res302=residuals(model302, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res302, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res302^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model302@fit$ics[1])
BIC_list=append(BIC_list, model302@fit$ics[2])
u302=pstd(res302, mean=0, sd=1, nu=tail(model302@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u302, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u302, null="punif")$p.value)

model303=garchFit(formula=~arma(1,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res303=residuals(model303, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res303, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res303^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model303@fit$ics[1])
BIC_list=append(BIC_list, model303@fit$ics[2])
u303=pstd(res303, mean=0, sd=1, nu=tail(model303@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u303, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u303, null="punif")$p.value)

model304=garchFit(formula=~arma(1,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res304=residuals(model304, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res304, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res304^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model304@fit$ics[1])
BIC_list=append(BIC_list, model304@fit$ics[2])
u304=pstd(res304, mean=0, sd=1, nu=tail(model304@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u304, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u304, null="punif")$p.value)

model305=garchFit(formula=~arma(1,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res305=residuals(model305, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res305, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res305^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model305@fit$ics[1])
BIC_list=append(BIC_list, model305@fit$ics[2])
u305=pstd(res305, mean=0, sd=1, nu=tail(model305@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u305, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u305, null="punif")$p.value)

model306=garchFit(formula=~arma(1,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res306=residuals(model306, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res306, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res306^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model306@fit$ics[1])
BIC_list=append(BIC_list, model306@fit$ics[2])
u306=pstd(res306, mean=0, sd=1, nu=tail(model306@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u306, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u306, null="punif")$p.value)

model307=garchFit(formula=~arma(1,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res307=residuals(model307, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res307, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res307^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model307@fit$ics[1])
BIC_list=append(BIC_list, model307@fit$ics[2])
u307=pstd(res307, mean=0, sd=1, nu=tail(model307@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u307, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u307, null="punif")$p.value)

model308=garchFit(formula=~arma(1,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res308=residuals(model308, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res308, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res308^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model308@fit$ics[1])
BIC_list=append(BIC_list, model308@fit$ics[2])
u308=pstd(res308, mean=0, sd=1, nu=tail(model308@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u308, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u308, null="punif")$p.value)

model309=garchFit(formula=~arma(1,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res309=residuals(model309, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res309, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res309^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model309@fit$ics[1])
BIC_list=append(BIC_list, model309@fit$ics[2])
u309=pstd(res309, mean=0, sd=1, nu=tail(model309@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u309, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u309, null="punif")$p.value)

model310=garchFit(formula=~arma(1,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res310=residuals(model310, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res310, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res310^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model310@fit$ics[1])
BIC_list=append(BIC_list, model310@fit$ics[2])
u310=pstd(res310, mean=0, sd=1, nu=tail(model310@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u310, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u310, null="punif")$p.value)

model311=garchFit(formula=~arma(1,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res311=residuals(model311, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res311, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res311^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model311@fit$ics[1])
BIC_list=append(BIC_list, model311@fit$ics[2])
u311=pstd(res311, mean=0, sd=1, nu=tail(model311@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u311, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u311, null="punif")$p.value)

model312=garchFit(formula=~arma(1,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res312=residuals(model312, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res312, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res312^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model312@fit$ics[1])
BIC_list=append(BIC_list, model312@fit$ics[2])
u312=pstd(res312, mean=0, sd=1, nu=tail(model312@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u312, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u312, null="punif")$p.value)

model313=garchFit(formula=~arma(1,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res313=residuals(model313, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res313, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res313^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model313@fit$ics[1])
BIC_list=append(BIC_list, model313@fit$ics[2])
u313=pstd(res313, mean=0, sd=1, nu=tail(model313@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u313, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u313, null="punif")$p.value)

model314=garchFit(formula=~arma(1,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res314=residuals(model314, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res314, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res314^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model314@fit$ics[1])
BIC_list=append(BIC_list, model314@fit$ics[2])
u314=pstd(res314, mean=0, sd=1, nu=tail(model314@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u314, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u314, null="punif")$p.value)

model315=garchFit(formula=~arma(1,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res315=residuals(model315, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res315, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res315^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model315@fit$ics[1])
BIC_list=append(BIC_list, model315@fit$ics[2])
u315=pstd(res315, mean=0, sd=1, nu=tail(model315@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u315, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u315, null="punif")$p.value)

model316=garchFit(formula=~arma(1,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res316=residuals(model316, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res316, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res316^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model316@fit$ics[1])
BIC_list=append(BIC_list, model316@fit$ics[2])
u316=pstd(res316, mean=0, sd=1, nu=tail(model316@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u316, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u316, null="punif")$p.value)

model317=garchFit(formula=~arma(1,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res317=residuals(model317, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res317, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res317^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model317@fit$ics[1])
BIC_list=append(BIC_list, model317@fit$ics[2])
u317=pstd(res317, mean=0, sd=1, nu=tail(model317@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u317, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u317, null="punif")$p.value)

model318=garchFit(formula=~arma(1,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res318=residuals(model318, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res318, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res318^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model318@fit$ics[1])
BIC_list=append(BIC_list, model318@fit$ics[2])
u318=pstd(res318, mean=0, sd=1, nu=tail(model318@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u318, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u318, null="punif")$p.value)

model319=garchFit(formula=~arma(1,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res319=residuals(model319, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res319, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res319^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model319@fit$ics[1])
BIC_list=append(BIC_list, model319@fit$ics[2])
u319=pstd(res319, mean=0, sd=1, nu=tail(model319@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u319, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u319, null="punif")$p.value)

model320=garchFit(formula=~arma(1,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res320=residuals(model320, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res320, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res320^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model320@fit$ics[1])
BIC_list=append(BIC_list, model320@fit$ics[2])
u320=pstd(res320, mean=0, sd=1, nu=tail(model320@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u320, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u320, null="punif")$p.value)

model321=garchFit(formula=~arma(1,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res321=residuals(model321, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res321, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res321^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model321@fit$ics[1])
BIC_list=append(BIC_list, model321@fit$ics[2])
u321=pstd(res321, mean=0, sd=1, nu=tail(model321@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u321, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u321, null="punif")$p.value)

model322=garchFit(formula=~arma(1,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res322=residuals(model322, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res322, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res322^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model322@fit$ics[1])
BIC_list=append(BIC_list, model322@fit$ics[2])
u322=pstd(res322, mean=0, sd=1, nu=tail(model322@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u322, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u322, null="punif")$p.value)

model323=garchFit(formula=~arma(1,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res323=residuals(model323, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res323, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res323^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model323@fit$ics[1])
BIC_list=append(BIC_list, model323@fit$ics[2])
u323=pstd(res323, mean=0, sd=1, nu=tail(model323@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u323, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u323, null="punif")$p.value)

model324=garchFit(formula=~arma(1,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res324=residuals(model324, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res324, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res324^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model324@fit$ics[1])
BIC_list=append(BIC_list, model324@fit$ics[2])
u324=pstd(res324, mean=0, sd=1, nu=tail(model324@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u324, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u324, null="punif")$p.value)

model325=garchFit(formula=~arma(3,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res325=residuals(model325, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res325, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res325^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model325@fit$ics[1])
BIC_list=append(BIC_list, model325@fit$ics[2])
u325=pstd(res325, mean=0, sd=1, nu=tail(model325@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u325, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u325, null="punif")$p.value)

model326=garchFit(formula=~arma(3,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res326=residuals(model326, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res326, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res326^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model326@fit$ics[1])
BIC_list=append(BIC_list, model326@fit$ics[2])
u326=pstd(res326, mean=0, sd=1, nu=tail(model326@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u326, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u326, null="punif")$p.value)

model327=garchFit(formula=~arma(3,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res327=residuals(model327, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res327, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res327^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model327@fit$ics[1])
BIC_list=append(BIC_list, model327@fit$ics[2])
u327=pstd(res327, mean=0, sd=1, nu=tail(model327@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u327, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u327, null="punif")$p.value)

model328=garchFit(formula=~arma(3,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res328=residuals(model328, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res328, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res328^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model328@fit$ics[1])
BIC_list=append(BIC_list, model328@fit$ics[2])
u328=pstd(res328, mean=0, sd=1, nu=tail(model328@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u328, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u328, null="punif")$p.value)

model329=garchFit(formula=~arma(3,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res329=residuals(model329, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res329, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res329^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model329@fit$ics[1])
BIC_list=append(BIC_list, model329@fit$ics[2])
u329=pstd(res329, mean=0, sd=1, nu=tail(model329@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u329, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u329, null="punif")$p.value)

model330=garchFit(formula=~arma(3,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res330=residuals(model330, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res330, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res330^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model330@fit$ics[1])
BIC_list=append(BIC_list, model330@fit$ics[2])
u330=pstd(res330, mean=0, sd=1, nu=tail(model330@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u330, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u330, null="punif")$p.value)

model331=garchFit(formula=~arma(3,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res331=residuals(model331, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res331, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res331^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model331@fit$ics[1])
BIC_list=append(BIC_list, model331@fit$ics[2])
u331=pstd(res331, mean=0, sd=1, nu=tail(model331@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u331, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u331, null="punif")$p.value)

model332=garchFit(formula=~arma(3,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res332=residuals(model332, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res332, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res332^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model332@fit$ics[1])
BIC_list=append(BIC_list, model332@fit$ics[2])
u332=pstd(res332, mean=0, sd=1, nu=tail(model332@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u332, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u332, null="punif")$p.value)

model333=garchFit(formula=~arma(3,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res333=residuals(model333, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res333, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res333^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model333@fit$ics[1])
BIC_list=append(BIC_list, model333@fit$ics[2])
u333=pstd(res333, mean=0, sd=1, nu=tail(model333@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u333, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u333, null="punif")$p.value)

model334=garchFit(formula=~arma(3,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res334=residuals(model334, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res334, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res334^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model334@fit$ics[1])
BIC_list=append(BIC_list, model334@fit$ics[2])
u334=pstd(res334, mean=0, sd=1, nu=tail(model334@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u334, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u334, null="punif")$p.value)

model335=garchFit(formula=~arma(3,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res335=residuals(model335, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res335, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res335^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model335@fit$ics[1])
BIC_list=append(BIC_list, model335@fit$ics[2])
u335=pstd(res335, mean=0, sd=1, nu=tail(model335@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u335, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u335, null="punif")$p.value)

model336=garchFit(formula=~arma(3,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res336=residuals(model336, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res336, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res336^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model336@fit$ics[1])
BIC_list=append(BIC_list, model336@fit$ics[2])
u336=pstd(res336, mean=0, sd=1, nu=tail(model336@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u336, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u336, null="punif")$p.value)

model337=garchFit(formula=~arma(3,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res337=residuals(model337, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res337, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res337^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model337@fit$ics[1])
BIC_list=append(BIC_list, model337@fit$ics[2])
u337=pstd(res337, mean=0, sd=1, nu=tail(model337@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u337, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u337, null="punif")$p.value)

model338=garchFit(formula=~arma(3,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res338=residuals(model338, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res338, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res338^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model338@fit$ics[1])
BIC_list=append(BIC_list, model338@fit$ics[2])
u338=pstd(res338, mean=0, sd=1, nu=tail(model338@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u338, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u338, null="punif")$p.value)

model339=garchFit(formula=~arma(3,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res339=residuals(model339, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res339, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res339^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model339@fit$ics[1])
BIC_list=append(BIC_list, model339@fit$ics[2])
u339=pstd(res339, mean=0, sd=1, nu=tail(model339@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u339, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u339, null="punif")$p.value)

model340=garchFit(formula=~arma(3,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res340=residuals(model340, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res340, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res340^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model340@fit$ics[1])
BIC_list=append(BIC_list, model340@fit$ics[2])
u340=pstd(res340, mean=0, sd=1, nu=tail(model340@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u340, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u340, null="punif")$p.value)

model341=garchFit(formula=~arma(3,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res341=residuals(model341, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res341, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res341^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model341@fit$ics[1])
BIC_list=append(BIC_list, model341@fit$ics[2])
u341=pstd(res341, mean=0, sd=1, nu=tail(model341@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u341, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u341, null="punif")$p.value)

model342=garchFit(formula=~arma(3,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res342=residuals(model342, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res342, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res342^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model342@fit$ics[1])
BIC_list=append(BIC_list, model342@fit$ics[2])
u342=pstd(res342, mean=0, sd=1, nu=tail(model342@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u342, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u342, null="punif")$p.value)

model343=garchFit(formula=~arma(3,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res343=residuals(model343, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res343, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res343^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model343@fit$ics[1])
BIC_list=append(BIC_list, model343@fit$ics[2])
u343=pstd(res343, mean=0, sd=1, nu=tail(model343@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u343, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u343, null="punif")$p.value)

model344=garchFit(formula=~arma(3,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res344=residuals(model344, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res344, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res344^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model344@fit$ics[1])
BIC_list=append(BIC_list, model344@fit$ics[2])
u344=pstd(res344, mean=0, sd=1, nu=tail(model344@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u344, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u344, null="punif")$p.value)

model345=garchFit(formula=~arma(3,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res345=residuals(model345, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res345, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res345^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model345@fit$ics[1])
BIC_list=append(BIC_list, model345@fit$ics[2])
u345=pstd(res345, mean=0, sd=1, nu=tail(model345@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u345, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u345, null="punif")$p.value)

model346=garchFit(formula=~arma(3,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res346=residuals(model346, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res346, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res346^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model346@fit$ics[1])
BIC_list=append(BIC_list, model346@fit$ics[2])
u346=pstd(res346, mean=0, sd=1, nu=tail(model346@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u346, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u346, null="punif")$p.value)

model347=garchFit(formula=~arma(3,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res347=residuals(model347, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res347, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res347^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model347@fit$ics[1])
BIC_list=append(BIC_list, model347@fit$ics[2])
u347=pstd(res347, mean=0, sd=1, nu=tail(model347@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u347, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u347, null="punif")$p.value)

model348=garchFit(formula=~arma(3,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res348=residuals(model348, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res348, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res348^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model348@fit$ics[1])
BIC_list=append(BIC_list, model348@fit$ics[2])
u348=pstd(res348, mean=0, sd=1, nu=tail(model348@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u348, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u348, null="punif")$p.value)

model349=garchFit(formula=~arma(3,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res349=residuals(model349, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res349, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res349^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model349@fit$ics[1])
BIC_list=append(BIC_list, model349@fit$ics[2])
u349=pstd(res349, mean=0, sd=1, nu=tail(model349@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u349, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u349, null="punif")$p.value)

model350=garchFit(formula=~arma(3,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res350=residuals(model350, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res350, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res350^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model350@fit$ics[1])
BIC_list=append(BIC_list, model350@fit$ics[2])
u350=pstd(res350, mean=0, sd=1, nu=tail(model350@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u350, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u350, null="punif")$p.value)

model351=garchFit(formula=~arma(3,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res351=residuals(model351, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res351, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res351^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model351@fit$ics[1])
BIC_list=append(BIC_list, model351@fit$ics[2])
u351=pstd(res351, mean=0, sd=1, nu=tail(model351@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u351, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u351, null="punif")$p.value)

model352=garchFit(formula=~arma(3,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res352=residuals(model352, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res352, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res352^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model352@fit$ics[1])
BIC_list=append(BIC_list, model352@fit$ics[2])
u352=pstd(res352, mean=0, sd=1, nu=tail(model352@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u352, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u352, null="punif")$p.value)

model353=garchFit(formula=~arma(3,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res353=residuals(model353, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res353, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res353^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model353@fit$ics[1])
BIC_list=append(BIC_list, model353@fit$ics[2])
u353=pstd(res353, mean=0, sd=1, nu=tail(model353@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u353, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u353, null="punif")$p.value)

model354=garchFit(formula=~arma(3,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res354=residuals(model354, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res354, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res354^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model354@fit$ics[1])
BIC_list=append(BIC_list, model354@fit$ics[2])
u354=pstd(res354, mean=0, sd=1, nu=tail(model354@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u354, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u354, null="punif")$p.value)

model355=garchFit(formula=~arma(3,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res355=residuals(model355, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res355, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res355^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model355@fit$ics[1])
BIC_list=append(BIC_list, model355@fit$ics[2])
u355=pstd(res355, mean=0, sd=1, nu=tail(model355@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u355, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u355, null="punif")$p.value)

model356=garchFit(formula=~arma(3,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res356=residuals(model356, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res356, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res356^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model356@fit$ics[1])
BIC_list=append(BIC_list, model356@fit$ics[2])
u356=pstd(res356, mean=0, sd=1, nu=tail(model356@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u356, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u356, null="punif")$p.value)

model357=garchFit(formula=~arma(3,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res357=residuals(model357, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res357, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res357^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model357@fit$ics[1])
BIC_list=append(BIC_list, model357@fit$ics[2])
u357=pstd(res357, mean=0, sd=1, nu=tail(model357@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u357, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u357, null="punif")$p.value)

model358=garchFit(formula=~arma(3,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res358=residuals(model358, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res358, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res358^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model358@fit$ics[1])
BIC_list=append(BIC_list, model358@fit$ics[2])
u358=pstd(res358, mean=0, sd=1, nu=tail(model358@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u358, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u358, null="punif")$p.value)

model359=garchFit(formula=~arma(3,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res359=residuals(model359, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res359, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res359^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model359@fit$ics[1])
BIC_list=append(BIC_list, model359@fit$ics[2])
u359=pstd(res359, mean=0, sd=1, nu=tail(model359@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u359, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u359, null="punif")$p.value)

model360=garchFit(formula=~arma(3,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res360=residuals(model360, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res360, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res360^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model360@fit$ics[1])
BIC_list=append(BIC_list, model360@fit$ics[2])
u360=pstd(res360, mean=0, sd=1, nu=tail(model360@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u360, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u360, null="punif")$p.value)

model361=garchFit(formula=~arma(4,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res361=residuals(model361, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res361, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res361^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model361@fit$ics[1])
BIC_list=append(BIC_list, model361@fit$ics[2])
u361=pstd(res361, mean=0, sd=1, nu=tail(model361@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u361, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u361, null="punif")$p.value)

model362=garchFit(formula=~arma(4,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res362=residuals(model362, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res362, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res362^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model362@fit$ics[1])
BIC_list=append(BIC_list, model362@fit$ics[2])
u362=pstd(res362, mean=0, sd=1, nu=tail(model362@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u362, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u362, null="punif")$p.value)

model363=garchFit(formula=~arma(4,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res363=residuals(model363, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res363, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res363^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model363@fit$ics[1])
BIC_list=append(BIC_list, model363@fit$ics[2])
u363=pstd(res363, mean=0, sd=1, nu=tail(model363@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u363, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u363, null="punif")$p.value)

model364=garchFit(formula=~arma(4,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res364=residuals(model364, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res364, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res364^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model364@fit$ics[1])
BIC_list=append(BIC_list, model364@fit$ics[2])
u364=pstd(res364, mean=0, sd=1, nu=tail(model364@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u364, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u364, null="punif")$p.value)

model365=garchFit(formula=~arma(4,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res365=residuals(model365, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res365, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res365^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model365@fit$ics[1])
BIC_list=append(BIC_list, model365@fit$ics[2])
u365=pstd(res365, mean=0, sd=1, nu=tail(model365@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u365, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u365, null="punif")$p.value)

model366=garchFit(formula=~arma(4,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res366=residuals(model366, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res366, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res366^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model366@fit$ics[1])
BIC_list=append(BIC_list, model366@fit$ics[2])
u366=pstd(res366, mean=0, sd=1, nu=tail(model366@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u366, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u366, null="punif")$p.value)

model367=garchFit(formula=~arma(4,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res367=residuals(model367, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res367, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res367^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model367@fit$ics[1])
BIC_list=append(BIC_list, model367@fit$ics[2])
u367=pstd(res367, mean=0, sd=1, nu=tail(model367@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u367, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u367, null="punif")$p.value)

model368=garchFit(formula=~arma(4,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res368=residuals(model368, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res368, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res368^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model368@fit$ics[1])
BIC_list=append(BIC_list, model368@fit$ics[2])
u368=pstd(res368, mean=0, sd=1, nu=tail(model368@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u368, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u368, null="punif")$p.value)

model369=garchFit(formula=~arma(4,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res369=residuals(model369, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res369, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res369^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model369@fit$ics[1])
BIC_list=append(BIC_list, model369@fit$ics[2])
u369=pstd(res369, mean=0, sd=1, nu=tail(model369@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u369, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u369, null="punif")$p.value)

model370=garchFit(formula=~arma(4,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res370=residuals(model370, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res370, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res370^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model370@fit$ics[1])
BIC_list=append(BIC_list, model370@fit$ics[2])
u370=pstd(res370, mean=0, sd=1, nu=tail(model370@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u370, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u370, null="punif")$p.value)

model371=garchFit(formula=~arma(4,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res371=residuals(model371, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res371, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res371^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model371@fit$ics[1])
BIC_list=append(BIC_list, model371@fit$ics[2])
u371=pstd(res371, mean=0, sd=1, nu=tail(model371@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u371, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u371, null="punif")$p.value)

model372=garchFit(formula=~arma(4,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res372=residuals(model372, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res372, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res372^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model372@fit$ics[1])
BIC_list=append(BIC_list, model372@fit$ics[2])
u372=pstd(res372, mean=0, sd=1, nu=tail(model372@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u372, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u372, null="punif")$p.value)

model373=garchFit(formula=~arma(4,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res373=residuals(model373, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res373, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res373^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model373@fit$ics[1])
BIC_list=append(BIC_list, model373@fit$ics[2])
u373=pstd(res373, mean=0, sd=1, nu=tail(model373@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u373, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u373, null="punif")$p.value)

model374=garchFit(formula=~arma(4,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res374=residuals(model374, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res374, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res374^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model374@fit$ics[1])
BIC_list=append(BIC_list, model374@fit$ics[2])
u374=pstd(res374, mean=0, sd=1, nu=tail(model374@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u374, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u374, null="punif")$p.value)

model375=garchFit(formula=~arma(4,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res375=residuals(model375, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res375, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res375^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model375@fit$ics[1])
BIC_list=append(BIC_list, model375@fit$ics[2])
u375=pstd(res375, mean=0, sd=1, nu=tail(model375@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u375, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u375, null="punif")$p.value)

model376=garchFit(formula=~arma(4,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res376=residuals(model376, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res376, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res376^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model376@fit$ics[1])
BIC_list=append(BIC_list, model376@fit$ics[2])
u376=pstd(res376, mean=0, sd=1, nu=tail(model376@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u376, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u376, null="punif")$p.value)

model377=garchFit(formula=~arma(4,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res377=residuals(model377, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res377, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res377^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model377@fit$ics[1])
BIC_list=append(BIC_list, model377@fit$ics[2])
u377=pstd(res377, mean=0, sd=1, nu=tail(model377@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u377, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u377, null="punif")$p.value)

model378=garchFit(formula=~arma(4,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res378=residuals(model378, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res378, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res378^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model378@fit$ics[1])
BIC_list=append(BIC_list, model378@fit$ics[2])
u378=pstd(res378, mean=0, sd=1, nu=tail(model378@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u378, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u378, null="punif")$p.value)

model379=garchFit(formula=~arma(4,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res379=residuals(model379, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res379, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res379^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model379@fit$ics[1])
BIC_list=append(BIC_list, model379@fit$ics[2])
u379=pstd(res379, mean=0, sd=1, nu=tail(model379@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u379, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u379, null="punif")$p.value)

model380=garchFit(formula=~arma(4,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res380=residuals(model380, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res380, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res380^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model380@fit$ics[1])
BIC_list=append(BIC_list, model380@fit$ics[2])
u380=pstd(res380, mean=0, sd=1, nu=tail(model380@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u380, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u380, null="punif")$p.value)

model381=garchFit(formula=~arma(4,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res381=residuals(model381, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res381, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res381^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model381@fit$ics[1])
BIC_list=append(BIC_list, model381@fit$ics[2])
u381=pstd(res381, mean=0, sd=1, nu=tail(model381@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u381, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u381, null="punif")$p.value)

model382=garchFit(formula=~arma(4,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res382=residuals(model382, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res382, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res382^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model382@fit$ics[1])
BIC_list=append(BIC_list, model382@fit$ics[2])
u382=pstd(res382, mean=0, sd=1, nu=tail(model382@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u382, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u382, null="punif")$p.value)

model383=garchFit(formula=~arma(4,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res383=residuals(model383, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res383, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res383^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model383@fit$ics[1])
BIC_list=append(BIC_list, model383@fit$ics[2])
u383=pstd(res383, mean=0, sd=1, nu=tail(model383@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u383, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u383, null="punif")$p.value)

model384=garchFit(formula=~arma(4,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res384=residuals(model384, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res384, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res384^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model384@fit$ics[1])
BIC_list=append(BIC_list, model384@fit$ics[2])
u384=pstd(res384, mean=0, sd=1, nu=tail(model384@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u384, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u384, null="punif")$p.value)

model385=garchFit(formula=~arma(4,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res385=residuals(model385, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res385, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res385^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model385@fit$ics[1])
BIC_list=append(BIC_list, model385@fit$ics[2])
u385=pstd(res385, mean=0, sd=1, nu=tail(model385@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u385, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u385, null="punif")$p.value)

model386=garchFit(formula=~arma(4,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res386=residuals(model386, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res386, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res386^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model386@fit$ics[1])
BIC_list=append(BIC_list, model386@fit$ics[2])
u386=pstd(res386, mean=0, sd=1, nu=tail(model386@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u386, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u386, null="punif")$p.value)

model387=garchFit(formula=~arma(4,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res387=residuals(model387, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res387, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res387^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model387@fit$ics[1])
BIC_list=append(BIC_list, model387@fit$ics[2])
u387=pstd(res387, mean=0, sd=1, nu=tail(model387@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u387, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u387, null="punif")$p.value)

model388=garchFit(formula=~arma(4,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res388=residuals(model388, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res388, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res388^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model388@fit$ics[1])
BIC_list=append(BIC_list, model388@fit$ics[2])
u388=pstd(res388, mean=0, sd=1, nu=tail(model388@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u388, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u388, null="punif")$p.value)

model389=garchFit(formula=~arma(4,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res389=residuals(model389, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res389, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res389^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model389@fit$ics[1])
BIC_list=append(BIC_list, model389@fit$ics[2])
u389=pstd(res389, mean=0, sd=1, nu=tail(model389@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u389, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u389, null="punif")$p.value)

model390=garchFit(formula=~arma(4,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res390=residuals(model390, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res390, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res390^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model390@fit$ics[1])
BIC_list=append(BIC_list, model390@fit$ics[2])
u390=pstd(res390, mean=0, sd=1, nu=tail(model390@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u390, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u390, null="punif")$p.value)

model391=garchFit(formula=~arma(4,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res391=residuals(model391, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res391, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res391^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model391@fit$ics[1])
BIC_list=append(BIC_list, model391@fit$ics[2])
u391=pstd(res391, mean=0, sd=1, nu=tail(model391@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u391, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u391, null="punif")$p.value)

model392=garchFit(formula=~arma(4,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res392=residuals(model392, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res392, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res392^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model392@fit$ics[1])
BIC_list=append(BIC_list, model392@fit$ics[2])
u392=pstd(res392, mean=0, sd=1, nu=tail(model392@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u392, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u392, null="punif")$p.value)

model393=garchFit(formula=~arma(4,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res393=residuals(model393, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res393, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res393^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model393@fit$ics[1])
BIC_list=append(BIC_list, model393@fit$ics[2])
u393=pstd(res393, mean=0, sd=1, nu=tail(model393@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u393, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u393, null="punif")$p.value)

model394=garchFit(formula=~arma(4,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res394=residuals(model394, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res394, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res394^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model394@fit$ics[1])
BIC_list=append(BIC_list, model394@fit$ics[2])
u394=pstd(res394, mean=0, sd=1, nu=tail(model394@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u394, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u394, null="punif")$p.value)

model395=garchFit(formula=~arma(4,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res395=residuals(model395, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res395, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res395^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model395@fit$ics[1])
BIC_list=append(BIC_list, model395@fit$ics[2])
u395=pstd(res395, mean=0, sd=1, nu=tail(model395@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u395, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u395, null="punif")$p.value)

model396=garchFit(formula=~arma(4,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res396=residuals(model396, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res396, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res396^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model396@fit$ics[1])
BIC_list=append(BIC_list, model396@fit$ics[2])
u396=pstd(res396, mean=0, sd=1, nu=tail(model396@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u396, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u396, null="punif")$p.value)

model397=garchFit(formula=~arma(7,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res397=residuals(model397, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res397, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res397^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model397@fit$ics[1])
BIC_list=append(BIC_list, model397@fit$ics[2])
u397=pstd(res397, mean=0, sd=1, nu=tail(model397@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u397, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u397, null="punif")$p.value)

model398=garchFit(formula=~arma(7,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res398=residuals(model398, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res398, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res398^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model398@fit$ics[1])
BIC_list=append(BIC_list, model398@fit$ics[2])
u398=pstd(res398, mean=0, sd=1, nu=tail(model398@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u398, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u398, null="punif")$p.value)

model399=garchFit(formula=~arma(7,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res399=residuals(model399, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res399, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res399^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model399@fit$ics[1])
BIC_list=append(BIC_list, model399@fit$ics[2])
u399=pstd(res399, mean=0, sd=1, nu=tail(model399@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u399, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u399, null="punif")$p.value)

model400=garchFit(formula=~arma(7,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res400=residuals(model400, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res400, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res400^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model400@fit$ics[1])
BIC_list=append(BIC_list, model400@fit$ics[2])
u400=pstd(res400, mean=0, sd=1, nu=tail(model400@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u400, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u400, null="punif")$p.value)

model401=garchFit(formula=~arma(7,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res401=residuals(model401, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res401, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res401^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model401@fit$ics[1])
BIC_list=append(BIC_list, model401@fit$ics[2])
u401=pstd(res401, mean=0, sd=1, nu=tail(model401@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u401, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u401, null="punif")$p.value)

model402=garchFit(formula=~arma(7,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res402=residuals(model402, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res402, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res402^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model402@fit$ics[1])
BIC_list=append(BIC_list, model402@fit$ics[2])
u402=pstd(res402, mean=0, sd=1, nu=tail(model402@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u402, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u402, null="punif")$p.value)

model403=garchFit(formula=~arma(7,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res403=residuals(model403, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res403, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res403^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model403@fit$ics[1])
BIC_list=append(BIC_list, model403@fit$ics[2])
u403=pstd(res403, mean=0, sd=1, nu=tail(model403@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u403, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u403, null="punif")$p.value)

model404=garchFit(formula=~arma(7,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res404=residuals(model404, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res404, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res404^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model404@fit$ics[1])
BIC_list=append(BIC_list, model404@fit$ics[2])
u404=pstd(res404, mean=0, sd=1, nu=tail(model404@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u404, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u404, null="punif")$p.value)

model405=garchFit(formula=~arma(7,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res405=residuals(model405, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res405, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res405^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model405@fit$ics[1])
BIC_list=append(BIC_list, model405@fit$ics[2])
u405=pstd(res405, mean=0, sd=1, nu=tail(model405@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u405, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u405, null="punif")$p.value)

model406=garchFit(formula=~arma(7,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res406=residuals(model406, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res406, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res406^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model406@fit$ics[1])
BIC_list=append(BIC_list, model406@fit$ics[2])
u406=pstd(res406, mean=0, sd=1, nu=tail(model406@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u406, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u406, null="punif")$p.value)

model407=garchFit(formula=~arma(7,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res407=residuals(model407, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res407, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res407^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model407@fit$ics[1])
BIC_list=append(BIC_list, model407@fit$ics[2])
u407=pstd(res407, mean=0, sd=1, nu=tail(model407@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u407, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u407, null="punif")$p.value)

model408=garchFit(formula=~arma(7,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res408=residuals(model408, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res408, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res408^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model408@fit$ics[1])
BIC_list=append(BIC_list, model408@fit$ics[2])
u408=pstd(res408, mean=0, sd=1, nu=tail(model408@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u408, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u408, null="punif")$p.value)

model409=garchFit(formula=~arma(7,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res409=residuals(model409, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res409, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res409^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model409@fit$ics[1])
BIC_list=append(BIC_list, model409@fit$ics[2])
u409=pstd(res409, mean=0, sd=1, nu=tail(model409@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u409, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u409, null="punif")$p.value)

model410=garchFit(formula=~arma(7,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res410=residuals(model410, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res410, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res410^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model410@fit$ics[1])
BIC_list=append(BIC_list, model410@fit$ics[2])
u410=pstd(res410, mean=0, sd=1, nu=tail(model410@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u410, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u410, null="punif")$p.value)

model411=garchFit(formula=~arma(7,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res411=residuals(model411, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res411, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res411^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model411@fit$ics[1])
BIC_list=append(BIC_list, model411@fit$ics[2])
u411=pstd(res411, mean=0, sd=1, nu=tail(model411@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u411, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u411, null="punif")$p.value)

model412=garchFit(formula=~arma(7,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res412=residuals(model412, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res412, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res412^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model412@fit$ics[1])
BIC_list=append(BIC_list, model412@fit$ics[2])
u412=pstd(res412, mean=0, sd=1, nu=tail(model412@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u412, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u412, null="punif")$p.value)

model413=garchFit(formula=~arma(7,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res413=residuals(model413, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res413, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res413^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model413@fit$ics[1])
BIC_list=append(BIC_list, model413@fit$ics[2])
u413=pstd(res413, mean=0, sd=1, nu=tail(model413@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u413, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u413, null="punif")$p.value)

model414=garchFit(formula=~arma(7,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res414=residuals(model414, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res414, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res414^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model414@fit$ics[1])
BIC_list=append(BIC_list, model414@fit$ics[2])
u414=pstd(res414, mean=0, sd=1, nu=tail(model414@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u414, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u414, null="punif")$p.value)

model415=garchFit(formula=~arma(7,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res415=residuals(model415, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res415, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res415^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model415@fit$ics[1])
BIC_list=append(BIC_list, model415@fit$ics[2])
u415=pstd(res415, mean=0, sd=1, nu=tail(model415@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u415, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u415, null="punif")$p.value)

model416=garchFit(formula=~arma(7,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res416=residuals(model416, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res416, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res416^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model416@fit$ics[1])
BIC_list=append(BIC_list, model416@fit$ics[2])
u416=pstd(res416, mean=0, sd=1, nu=tail(model416@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u416, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u416, null="punif")$p.value)

model417=garchFit(formula=~arma(7,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res417=residuals(model417, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res417, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res417^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model417@fit$ics[1])
BIC_list=append(BIC_list, model417@fit$ics[2])
u417=pstd(res417, mean=0, sd=1, nu=tail(model417@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u417, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u417, null="punif")$p.value)

model418=garchFit(formula=~arma(7,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res418=residuals(model418, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res418, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res418^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model418@fit$ics[1])
BIC_list=append(BIC_list, model418@fit$ics[2])
u418=pstd(res418, mean=0, sd=1, nu=tail(model418@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u418, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u418, null="punif")$p.value)

model419=garchFit(formula=~arma(7,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res419=residuals(model419, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res419, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res419^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model419@fit$ics[1])
BIC_list=append(BIC_list, model419@fit$ics[2])
u419=pstd(res419, mean=0, sd=1, nu=tail(model419@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u419, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u419, null="punif")$p.value)

model420=garchFit(formula=~arma(7,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res420=residuals(model420, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res420, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res420^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model420@fit$ics[1])
BIC_list=append(BIC_list, model420@fit$ics[2])
u420=pstd(res420, mean=0, sd=1, nu=tail(model420@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u420, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u420, null="punif")$p.value)

model421=garchFit(formula=~arma(7,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res421=residuals(model421, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res421, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res421^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model421@fit$ics[1])
BIC_list=append(BIC_list, model421@fit$ics[2])
u421=pstd(res421, mean=0, sd=1, nu=tail(model421@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u421, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u421, null="punif")$p.value)

model422=garchFit(formula=~arma(7,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res422=residuals(model422, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res422, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res422^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model422@fit$ics[1])
BIC_list=append(BIC_list, model422@fit$ics[2])
u422=pstd(res422, mean=0, sd=1, nu=tail(model422@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u422, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u422, null="punif")$p.value)

model423=garchFit(formula=~arma(7,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res423=residuals(model423, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res423, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res423^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model423@fit$ics[1])
BIC_list=append(BIC_list, model423@fit$ics[2])
u423=pstd(res423, mean=0, sd=1, nu=tail(model423@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u423, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u423, null="punif")$p.value)

model424=garchFit(formula=~arma(7,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="std")
res424=residuals(model424, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res424, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res424^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model424@fit$ics[1])
BIC_list=append(BIC_list, model424@fit$ics[2])
u424=pstd(res424, mean=0, sd=1, nu=tail(model424@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u424, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u424, null="punif")$p.value)

model425=garchFit(formula=~arma(7,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="std")
res425=residuals(model425, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res425, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res425^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model425@fit$ics[1])
BIC_list=append(BIC_list, model425@fit$ics[2])
u425=pstd(res425, mean=0, sd=1, nu=tail(model425@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u425, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u425, null="punif")$p.value)

model426=garchFit(formula=~arma(7,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="std")
res426=residuals(model426, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res426, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res426^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model426@fit$ics[1])
BIC_list=append(BIC_list, model426@fit$ics[2])
u426=pstd(res426, mean=0, sd=1, nu=tail(model426@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u426, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u426, null="punif")$p.value)

model427=garchFit(formula=~arma(7,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="std")
res427=residuals(model427, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res427, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res427^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model427@fit$ics[1])
BIC_list=append(BIC_list, model427@fit$ics[2])
u427=pstd(res427, mean=0, sd=1, nu=tail(model427@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u427, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u427, null="punif")$p.value)

model428=garchFit(formula=~arma(7,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="std")
res428=residuals(model428, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res428, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res428^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model428@fit$ics[1])
BIC_list=append(BIC_list, model428@fit$ics[2])
u428=pstd(res428, mean=0, sd=1, nu=tail(model428@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u428, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u428, null="punif")$p.value)

model429=garchFit(formula=~arma(7,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="std")
res429=residuals(model429, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res429, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res429^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model429@fit$ics[1])
BIC_list=append(BIC_list, model429@fit$ics[2])
u429=pstd(res429, mean=0, sd=1, nu=tail(model429@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u429, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u429, null="punif")$p.value)

model430=garchFit(formula=~arma(7,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="std")
res430=residuals(model430, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res430, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res430^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model430@fit$ics[1])
BIC_list=append(BIC_list, model430@fit$ics[2])
u430=pstd(res430, mean=0, sd=1, nu=tail(model430@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u430, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u430, null="punif")$p.value)

model431=garchFit(formula=~arma(7,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="std")
res431=residuals(model431, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res431, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res431^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model431@fit$ics[1])
BIC_list=append(BIC_list, model431@fit$ics[2])
u431=pstd(res431, mean=0, sd=1, nu=tail(model431@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u431, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u431, null="punif")$p.value)

model432=garchFit(formula=~arma(7,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="std")
res432=residuals(model432, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res432, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res432^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model432@fit$ics[1])
BIC_list=append(BIC_list, model432@fit$ics[2])
u432=pstd(res432, mean=0, sd=1, nu=tail(model432@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u432, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u432, null="punif")$p.value)

model433=garchFit(formula=~arma(1,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res433=residuals(model433, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res433, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res433^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model433@fit$ics[1])
BIC_list=append(BIC_list, model433@fit$ics[2])
u433=psstd(res433, mean=0, sd=1, nu=tail(model433@fit$coef, n=1), xi=model433@fit$coef[length(model433@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u433, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u433, null="punif")$p.value)

model434=garchFit(formula=~arma(1,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res434=residuals(model434, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res434, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res434^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model434@fit$ics[1])
BIC_list=append(BIC_list, model434@fit$ics[2])
u434=psstd(res434, mean=0, sd=1, nu=tail(model434@fit$coef, n=1), xi=model434@fit$coef[length(model434@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u434, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u434, null="punif")$p.value)

model435=garchFit(formula=~arma(1,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res435=residuals(model435, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res435, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res435^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model435@fit$ics[1])
BIC_list=append(BIC_list, model435@fit$ics[2])
u435=psstd(res435, mean=0, sd=1, nu=tail(model435@fit$coef, n=1), xi=model435@fit$coef[length(model435@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u435, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u435, null="punif")$p.value)

model436=garchFit(formula=~arma(1,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res436=residuals(model436, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res436, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res436^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model436@fit$ics[1])
BIC_list=append(BIC_list, model436@fit$ics[2])
u436=psstd(res436, mean=0, sd=1, nu=tail(model436@fit$coef, n=1), xi=model436@fit$coef[length(model436@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u436, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u436, null="punif")$p.value)

model437=garchFit(formula=~arma(1,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res437=residuals(model437, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res437, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res437^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model437@fit$ics[1])
BIC_list=append(BIC_list, model437@fit$ics[2])
u437=psstd(res437, mean=0, sd=1, nu=tail(model437@fit$coef, n=1), xi=model437@fit$coef[length(model437@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u437, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u437, null="punif")$p.value)

model438=garchFit(formula=~arma(1,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res438=residuals(model438, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res438, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res438^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model438@fit$ics[1])
BIC_list=append(BIC_list, model438@fit$ics[2])
u438=psstd(res438, mean=0, sd=1, nu=tail(model438@fit$coef, n=1), xi=model438@fit$coef[length(model438@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u438, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u438, null="punif")$p.value)

model439=garchFit(formula=~arma(1,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res439=residuals(model439, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res439, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res439^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model439@fit$ics[1])
BIC_list=append(BIC_list, model439@fit$ics[2])
u439=psstd(res439, mean=0, sd=1, nu=tail(model439@fit$coef, n=1), xi=model439@fit$coef[length(model439@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u439, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u439, null="punif")$p.value)

model440=garchFit(formula=~arma(1,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res440=residuals(model440, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res440, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res440^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model440@fit$ics[1])
BIC_list=append(BIC_list, model440@fit$ics[2])
u440=psstd(res440, mean=0, sd=1, nu=tail(model440@fit$coef, n=1), xi=model440@fit$coef[length(model440@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u440, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u440, null="punif")$p.value)

model441=garchFit(formula=~arma(1,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res441=residuals(model441, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res441, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res441^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model441@fit$ics[1])
BIC_list=append(BIC_list, model441@fit$ics[2])
u441=psstd(res441, mean=0, sd=1, nu=tail(model441@fit$coef, n=1), xi=model441@fit$coef[length(model441@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u441, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u441, null="punif")$p.value)

model442=garchFit(formula=~arma(1,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res442=residuals(model442, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res442, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res442^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model442@fit$ics[1])
BIC_list=append(BIC_list, model442@fit$ics[2])
u442=psstd(res442, mean=0, sd=1, nu=tail(model442@fit$coef, n=1), xi=model442@fit$coef[length(model442@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u442, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u442, null="punif")$p.value)

model443=garchFit(formula=~arma(1,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res443=residuals(model443, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res443, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res443^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model443@fit$ics[1])
BIC_list=append(BIC_list, model443@fit$ics[2])
u443=psstd(res443, mean=0, sd=1, nu=tail(model443@fit$coef, n=1), xi=model443@fit$coef[length(model443@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u443, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u443, null="punif")$p.value)

model444=garchFit(formula=~arma(1,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res444=residuals(model444, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res444, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res444^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model444@fit$ics[1])
BIC_list=append(BIC_list, model444@fit$ics[2])
u444=psstd(res444, mean=0, sd=1, nu=tail(model444@fit$coef, n=1), xi=model444@fit$coef[length(model444@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u444, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u444, null="punif")$p.value)

model445=garchFit(formula=~arma(1,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res445=residuals(model445, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res445, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res445^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model445@fit$ics[1])
BIC_list=append(BIC_list, model445@fit$ics[2])
u445=psstd(res445, mean=0, sd=1, nu=tail(model445@fit$coef, n=1), xi=model445@fit$coef[length(model445@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u445, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u445, null="punif")$p.value)

model446=garchFit(formula=~arma(1,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res446=residuals(model446, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res446, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res446^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model446@fit$ics[1])
BIC_list=append(BIC_list, model446@fit$ics[2])
u446=psstd(res446, mean=0, sd=1, nu=tail(model446@fit$coef, n=1), xi=model446@fit$coef[length(model446@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u446, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u446, null="punif")$p.value)

model447=garchFit(formula=~arma(1,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res447=residuals(model447, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res447, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res447^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model447@fit$ics[1])
BIC_list=append(BIC_list, model447@fit$ics[2])
u447=psstd(res447, mean=0, sd=1, nu=tail(model447@fit$coef, n=1), xi=model447@fit$coef[length(model447@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u447, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u447, null="punif")$p.value)

model448=garchFit(formula=~arma(1,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res448=residuals(model448, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res448, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res448^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model448@fit$ics[1])
BIC_list=append(BIC_list, model448@fit$ics[2])
u448=psstd(res448, mean=0, sd=1, nu=tail(model448@fit$coef, n=1), xi=model448@fit$coef[length(model448@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u448, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u448, null="punif")$p.value)

model449=garchFit(formula=~arma(1,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res449=residuals(model449, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res449, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res449^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model449@fit$ics[1])
BIC_list=append(BIC_list, model449@fit$ics[2])
u449=psstd(res449, mean=0, sd=1, nu=tail(model449@fit$coef, n=1), xi=model449@fit$coef[length(model449@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u449, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u449, null="punif")$p.value)

model450=garchFit(formula=~arma(1,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res450=residuals(model450, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res450, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res450^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model450@fit$ics[1])
BIC_list=append(BIC_list, model450@fit$ics[2])
u450=psstd(res450, mean=0, sd=1, nu=tail(model450@fit$coef, n=1), xi=model450@fit$coef[length(model450@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u450, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u450, null="punif")$p.value)

model451=garchFit(formula=~arma(1,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res451=residuals(model451, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res451, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res451^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model451@fit$ics[1])
BIC_list=append(BIC_list, model451@fit$ics[2])
u451=psstd(res451, mean=0, sd=1, nu=tail(model451@fit$coef, n=1), xi=model451@fit$coef[length(model451@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u451, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u451, null="punif")$p.value)

model452=garchFit(formula=~arma(1,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res452=residuals(model452, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res452, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res452^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model452@fit$ics[1])
BIC_list=append(BIC_list, model452@fit$ics[2])
u452=psstd(res452, mean=0, sd=1, nu=tail(model452@fit$coef, n=1), xi=model452@fit$coef[length(model452@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u452, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u452, null="punif")$p.value)

model453=garchFit(formula=~arma(1,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res453=residuals(model453, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res453, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res453^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model453@fit$ics[1])
BIC_list=append(BIC_list, model453@fit$ics[2])
u453=psstd(res453, mean=0, sd=1, nu=tail(model453@fit$coef, n=1), xi=model453@fit$coef[length(model453@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u453, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u453, null="punif")$p.value)

model454=garchFit(formula=~arma(1,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res454=residuals(model454, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res454, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res454^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model454@fit$ics[1])
BIC_list=append(BIC_list, model454@fit$ics[2])
u454=psstd(res454, mean=0, sd=1, nu=tail(model454@fit$coef, n=1), xi=model454@fit$coef[length(model454@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u454, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u454, null="punif")$p.value)

model455=garchFit(formula=~arma(1,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res455=residuals(model455, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res455, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res455^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model455@fit$ics[1])
BIC_list=append(BIC_list, model455@fit$ics[2])
u455=psstd(res455, mean=0, sd=1, nu=tail(model455@fit$coef, n=1), xi=model455@fit$coef[length(model455@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u455, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u455, null="punif")$p.value)

model456=garchFit(formula=~arma(1,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res456=residuals(model456, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res456, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res456^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model456@fit$ics[1])
BIC_list=append(BIC_list, model456@fit$ics[2])
u456=psstd(res456, mean=0, sd=1, nu=tail(model456@fit$coef, n=1), xi=model456@fit$coef[length(model456@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u456, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u456, null="punif")$p.value)

model457=garchFit(formula=~arma(1,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res457=residuals(model457, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res457, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res457^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model457@fit$ics[1])
BIC_list=append(BIC_list, model457@fit$ics[2])
u457=psstd(res457, mean=0, sd=1, nu=tail(model457@fit$coef, n=1), xi=model457@fit$coef[length(model457@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u457, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u457, null="punif")$p.value)

model458=garchFit(formula=~arma(1,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res458=residuals(model458, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res458, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res458^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model458@fit$ics[1])
BIC_list=append(BIC_list, model458@fit$ics[2])
u458=psstd(res458, mean=0, sd=1, nu=tail(model458@fit$coef, n=1), xi=model458@fit$coef[length(model458@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u458, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u458, null="punif")$p.value)

model459=garchFit(formula=~arma(1,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res459=residuals(model459, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res459, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res459^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model459@fit$ics[1])
BIC_list=append(BIC_list, model459@fit$ics[2])
u459=psstd(res459, mean=0, sd=1, nu=tail(model459@fit$coef, n=1), xi=model459@fit$coef[length(model459@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u459, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u459, null="punif")$p.value)

model460=garchFit(formula=~arma(1,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res460=residuals(model460, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res460, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res460^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model460@fit$ics[1])
BIC_list=append(BIC_list, model460@fit$ics[2])
u460=psstd(res460, mean=0, sd=1, nu=tail(model460@fit$coef, n=1), xi=model460@fit$coef[length(model460@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u460, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u460, null="punif")$p.value)

model461=garchFit(formula=~arma(1,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res461=residuals(model461, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res461, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res461^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model461@fit$ics[1])
BIC_list=append(BIC_list, model461@fit$ics[2])
u461=psstd(res461, mean=0, sd=1, nu=tail(model461@fit$coef, n=1), xi=model461@fit$coef[length(model461@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u461, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u461, null="punif")$p.value)

model462=garchFit(formula=~arma(1,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res462=residuals(model462, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res462, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res462^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model462@fit$ics[1])
BIC_list=append(BIC_list, model462@fit$ics[2])
u462=psstd(res462, mean=0, sd=1, nu=tail(model462@fit$coef, n=1), xi=model462@fit$coef[length(model462@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u462, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u462, null="punif")$p.value)

model463=garchFit(formula=~arma(1,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res463=residuals(model463, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res463, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res463^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model463@fit$ics[1])
BIC_list=append(BIC_list, model463@fit$ics[2])
u463=psstd(res463, mean=0, sd=1, nu=tail(model463@fit$coef, n=1), xi=model463@fit$coef[length(model463@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u463, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u463, null="punif")$p.value)

model464=garchFit(formula=~arma(1,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res464=residuals(model464, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res464, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res464^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model464@fit$ics[1])
BIC_list=append(BIC_list, model464@fit$ics[2])
u464=psstd(res464, mean=0, sd=1, nu=tail(model464@fit$coef, n=1), xi=model464@fit$coef[length(model464@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u464, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u464, null="punif")$p.value)

model465=garchFit(formula=~arma(1,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res465=residuals(model465, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res465, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res465^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model465@fit$ics[1])
BIC_list=append(BIC_list, model465@fit$ics[2])
u465=psstd(res465, mean=0, sd=1, nu=tail(model465@fit$coef, n=1), xi=model465@fit$coef[length(model465@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u465, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u465, null="punif")$p.value)

model466=garchFit(formula=~arma(1,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res466=residuals(model466, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res466, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res466^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model466@fit$ics[1])
BIC_list=append(BIC_list, model466@fit$ics[2])
u466=psstd(res466, mean=0, sd=1, nu=tail(model466@fit$coef, n=1), xi=model466@fit$coef[length(model466@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u466, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u466, null="punif")$p.value)

model467=garchFit(formula=~arma(1,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res467=residuals(model467, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res467, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res467^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model467@fit$ics[1])
BIC_list=append(BIC_list, model467@fit$ics[2])
u467=psstd(res467, mean=0, sd=1, nu=tail(model467@fit$coef, n=1), xi=model467@fit$coef[length(model467@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u467, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u467, null="punif")$p.value)

model468=garchFit(formula=~arma(1,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res468=residuals(model468, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res468, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res468^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model468@fit$ics[1])
BIC_list=append(BIC_list, model468@fit$ics[2])
u468=psstd(res468, mean=0, sd=1, nu=tail(model468@fit$coef, n=1), xi=model468@fit$coef[length(model468@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u468, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u468, null="punif")$p.value)

model469=garchFit(formula=~arma(3,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res469=residuals(model469, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res469, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res469^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model469@fit$ics[1])
BIC_list=append(BIC_list, model469@fit$ics[2])
u469=psstd(res469, mean=0, sd=1, nu=tail(model469@fit$coef, n=1), xi=model469@fit$coef[length(model469@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u469, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u469, null="punif")$p.value)

model470=garchFit(formula=~arma(3,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res470=residuals(model470, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res470, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res470^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model470@fit$ics[1])
BIC_list=append(BIC_list, model470@fit$ics[2])
u470=psstd(res470, mean=0, sd=1, nu=tail(model470@fit$coef, n=1), xi=model470@fit$coef[length(model470@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u470, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u470, null="punif")$p.value)

model471=garchFit(formula=~arma(3,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res471=residuals(model471, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res471, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res471^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model471@fit$ics[1])
BIC_list=append(BIC_list, model471@fit$ics[2])
u471=psstd(res471, mean=0, sd=1, nu=tail(model471@fit$coef, n=1), xi=model471@fit$coef[length(model471@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u471, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u471, null="punif")$p.value)

model472=garchFit(formula=~arma(3,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res472=residuals(model472, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res472, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res472^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model472@fit$ics[1])
BIC_list=append(BIC_list, model472@fit$ics[2])
u472=psstd(res472, mean=0, sd=1, nu=tail(model472@fit$coef, n=1), xi=model472@fit$coef[length(model472@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u472, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u472, null="punif")$p.value)

model473=garchFit(formula=~arma(3,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res473=residuals(model473, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res473, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res473^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model473@fit$ics[1])
BIC_list=append(BIC_list, model473@fit$ics[2])
u473=psstd(res473, mean=0, sd=1, nu=tail(model473@fit$coef, n=1), xi=model473@fit$coef[length(model473@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u473, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u473, null="punif")$p.value)

model474=garchFit(formula=~arma(3,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res474=residuals(model474, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res474, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res474^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model474@fit$ics[1])
BIC_list=append(BIC_list, model474@fit$ics[2])
u474=psstd(res474, mean=0, sd=1, nu=tail(model474@fit$coef, n=1), xi=model474@fit$coef[length(model474@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u474, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u474, null="punif")$p.value)

model475=garchFit(formula=~arma(3,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res475=residuals(model475, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res475, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res475^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model475@fit$ics[1])
BIC_list=append(BIC_list, model475@fit$ics[2])
u475=psstd(res475, mean=0, sd=1, nu=tail(model475@fit$coef, n=1), xi=model475@fit$coef[length(model475@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u475, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u475, null="punif")$p.value)

model476=garchFit(formula=~arma(3,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res476=residuals(model476, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res476, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res476^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model476@fit$ics[1])
BIC_list=append(BIC_list, model476@fit$ics[2])
u476=psstd(res476, mean=0, sd=1, nu=tail(model476@fit$coef, n=1), xi=model476@fit$coef[length(model476@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u476, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u476, null="punif")$p.value)

model477=garchFit(formula=~arma(3,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res477=residuals(model477, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res477, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res477^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model477@fit$ics[1])
BIC_list=append(BIC_list, model477@fit$ics[2])
u477=psstd(res477, mean=0, sd=1, nu=tail(model477@fit$coef, n=1), xi=model477@fit$coef[length(model477@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u477, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u477, null="punif")$p.value)

model478=garchFit(formula=~arma(3,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res478=residuals(model478, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res478, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res478^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model478@fit$ics[1])
BIC_list=append(BIC_list, model478@fit$ics[2])
u478=psstd(res478, mean=0, sd=1, nu=tail(model478@fit$coef, n=1), xi=model478@fit$coef[length(model478@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u478, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u478, null="punif")$p.value)

model479=garchFit(formula=~arma(3,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res479=residuals(model479, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res479, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res479^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model479@fit$ics[1])
BIC_list=append(BIC_list, model479@fit$ics[2])
u479=psstd(res479, mean=0, sd=1, nu=tail(model479@fit$coef, n=1), xi=model479@fit$coef[length(model479@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u479, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u479, null="punif")$p.value)

model480=garchFit(formula=~arma(3,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res480=residuals(model480, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res480, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res480^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model480@fit$ics[1])
BIC_list=append(BIC_list, model480@fit$ics[2])
u480=psstd(res480, mean=0, sd=1, nu=tail(model480@fit$coef, n=1), xi=model480@fit$coef[length(model480@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u480, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u480, null="punif")$p.value)

model481=garchFit(formula=~arma(3,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res481=residuals(model481, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res481, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res481^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model481@fit$ics[1])
BIC_list=append(BIC_list, model481@fit$ics[2])
u481=psstd(res481, mean=0, sd=1, nu=tail(model481@fit$coef, n=1), xi=model481@fit$coef[length(model481@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u481, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u481, null="punif")$p.value)

model482=garchFit(formula=~arma(3,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res482=residuals(model482, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res482, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res482^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model482@fit$ics[1])
BIC_list=append(BIC_list, model482@fit$ics[2])
u482=psstd(res482, mean=0, sd=1, nu=tail(model482@fit$coef, n=1), xi=model482@fit$coef[length(model482@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u482, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u482, null="punif")$p.value)

model483=garchFit(formula=~arma(3,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res483=residuals(model483, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res483, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res483^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model483@fit$ics[1])
BIC_list=append(BIC_list, model483@fit$ics[2])
u483=psstd(res483, mean=0, sd=1, nu=tail(model483@fit$coef, n=1), xi=model483@fit$coef[length(model483@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u483, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u483, null="punif")$p.value)

model484=garchFit(formula=~arma(3,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res484=residuals(model484, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res484, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res484^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model484@fit$ics[1])
BIC_list=append(BIC_list, model484@fit$ics[2])
u484=psstd(res484, mean=0, sd=1, nu=tail(model484@fit$coef, n=1), xi=model484@fit$coef[length(model484@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u484, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u484, null="punif")$p.value)

model485=garchFit(formula=~arma(3,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res485=residuals(model485, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res485, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res485^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model485@fit$ics[1])
BIC_list=append(BIC_list, model485@fit$ics[2])
u485=psstd(res485, mean=0, sd=1, nu=tail(model485@fit$coef, n=1), xi=model485@fit$coef[length(model485@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u485, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u485, null="punif")$p.value)

model486=garchFit(formula=~arma(3,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res486=residuals(model486, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res486, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res486^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model486@fit$ics[1])
BIC_list=append(BIC_list, model486@fit$ics[2])
u486=psstd(res486, mean=0, sd=1, nu=tail(model486@fit$coef, n=1), xi=model486@fit$coef[length(model486@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u486, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u486, null="punif")$p.value)

model487=garchFit(formula=~arma(3,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res487=residuals(model487, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res487, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res487^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model487@fit$ics[1])
BIC_list=append(BIC_list, model487@fit$ics[2])
u487=psstd(res487, mean=0, sd=1, nu=tail(model487@fit$coef, n=1), xi=model487@fit$coef[length(model487@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u487, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u487, null="punif")$p.value)

model488=garchFit(formula=~arma(3,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res488=residuals(model488, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res488, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res488^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model488@fit$ics[1])
BIC_list=append(BIC_list, model488@fit$ics[2])
u488=psstd(res488, mean=0, sd=1, nu=tail(model488@fit$coef, n=1), xi=model488@fit$coef[length(model488@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u488, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u488, null="punif")$p.value)

model489=garchFit(formula=~arma(3,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res489=residuals(model489, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res489, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res489^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model489@fit$ics[1])
BIC_list=append(BIC_list, model489@fit$ics[2])
u489=psstd(res489, mean=0, sd=1, nu=tail(model489@fit$coef, n=1), xi=model489@fit$coef[length(model489@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u489, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u489, null="punif")$p.value)

model490=garchFit(formula=~arma(3,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res490=residuals(model490, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res490, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res490^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model490@fit$ics[1])
BIC_list=append(BIC_list, model490@fit$ics[2])
u490=psstd(res490, mean=0, sd=1, nu=tail(model490@fit$coef, n=1), xi=model490@fit$coef[length(model490@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u490, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u490, null="punif")$p.value)

model491=garchFit(formula=~arma(3,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res491=residuals(model491, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res491, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res491^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model491@fit$ics[1])
BIC_list=append(BIC_list, model491@fit$ics[2])
u491=psstd(res491, mean=0, sd=1, nu=tail(model491@fit$coef, n=1), xi=model491@fit$coef[length(model491@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u491, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u491, null="punif")$p.value)

model492=garchFit(formula=~arma(3,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res492=residuals(model492, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res492, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res492^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model492@fit$ics[1])
BIC_list=append(BIC_list, model492@fit$ics[2])
u492=psstd(res492, mean=0, sd=1, nu=tail(model492@fit$coef, n=1), xi=model492@fit$coef[length(model492@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u492, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u492, null="punif")$p.value)

model493=garchFit(formula=~arma(3,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res493=residuals(model493, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res493, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res493^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model493@fit$ics[1])
BIC_list=append(BIC_list, model493@fit$ics[2])
u493=psstd(res493, mean=0, sd=1, nu=tail(model493@fit$coef, n=1), xi=model493@fit$coef[length(model493@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u493, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u493, null="punif")$p.value)

model494=garchFit(formula=~arma(3,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res494=residuals(model494, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res494, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res494^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model494@fit$ics[1])
BIC_list=append(BIC_list, model494@fit$ics[2])
u494=psstd(res494, mean=0, sd=1, nu=tail(model494@fit$coef, n=1), xi=model494@fit$coef[length(model494@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u494, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u494, null="punif")$p.value)

model495=garchFit(formula=~arma(3,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res495=residuals(model495, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res495, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res495^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model495@fit$ics[1])
BIC_list=append(BIC_list, model495@fit$ics[2])
u495=psstd(res495, mean=0, sd=1, nu=tail(model495@fit$coef, n=1), xi=model495@fit$coef[length(model495@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u495, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u495, null="punif")$p.value)

model496=garchFit(formula=~arma(3,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res496=residuals(model496, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res496, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res496^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model496@fit$ics[1])
BIC_list=append(BIC_list, model496@fit$ics[2])
u496=psstd(res496, mean=0, sd=1, nu=tail(model496@fit$coef, n=1), xi=model496@fit$coef[length(model496@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u496, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u496, null="punif")$p.value)

model497=garchFit(formula=~arma(3,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res497=residuals(model497, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res497, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res497^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model497@fit$ics[1])
BIC_list=append(BIC_list, model497@fit$ics[2])
u497=psstd(res497, mean=0, sd=1, nu=tail(model497@fit$coef, n=1), xi=model497@fit$coef[length(model497@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u497, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u497, null="punif")$p.value)

model498=garchFit(formula=~arma(3,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res498=residuals(model498, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res498, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res498^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model498@fit$ics[1])
BIC_list=append(BIC_list, model498@fit$ics[2])
u498=psstd(res498, mean=0, sd=1, nu=tail(model498@fit$coef, n=1), xi=model498@fit$coef[length(model498@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u498, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u498, null="punif")$p.value)

model499=garchFit(formula=~arma(3,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res499=residuals(model499, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res499, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res499^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model499@fit$ics[1])
BIC_list=append(BIC_list, model499@fit$ics[2])
u499=psstd(res499, mean=0, sd=1, nu=tail(model499@fit$coef, n=1), xi=model499@fit$coef[length(model499@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u499, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u499, null="punif")$p.value)

model500=garchFit(formula=~arma(3,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res500=residuals(model500, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res500, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res500^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model500@fit$ics[1])
BIC_list=append(BIC_list, model500@fit$ics[2])
u500=psstd(res500, mean=0, sd=1, nu=tail(model500@fit$coef, n=1), xi=model500@fit$coef[length(model500@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u500, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u500, null="punif")$p.value)

model501=garchFit(formula=~arma(3,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res501=residuals(model501, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res501, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res501^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model501@fit$ics[1])
BIC_list=append(BIC_list, model501@fit$ics[2])
u501=psstd(res501, mean=0, sd=1, nu=tail(model501@fit$coef, n=1), xi=model501@fit$coef[length(model501@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u501, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u501, null="punif")$p.value)

model502=garchFit(formula=~arma(3,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res502=residuals(model502, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res502, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res502^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model502@fit$ics[1])
BIC_list=append(BIC_list, model502@fit$ics[2])
u502=psstd(res502, mean=0, sd=1, nu=tail(model502@fit$coef, n=1), xi=model502@fit$coef[length(model502@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u502, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u502, null="punif")$p.value)

model503=garchFit(formula=~arma(3,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res503=residuals(model503, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res503, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res503^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model503@fit$ics[1])
BIC_list=append(BIC_list, model503@fit$ics[2])
u503=psstd(res503, mean=0, sd=1, nu=tail(model503@fit$coef, n=1), xi=model503@fit$coef[length(model503@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u503, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u503, null="punif")$p.value)

model504=garchFit(formula=~arma(3,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res504=residuals(model504, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res504, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res504^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model504@fit$ics[1])
BIC_list=append(BIC_list, model504@fit$ics[2])
u504=psstd(res504, mean=0, sd=1, nu=tail(model504@fit$coef, n=1), xi=model504@fit$coef[length(model504@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u504, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u504, null="punif")$p.value)

model505=garchFit(formula=~arma(4,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res505=residuals(model505, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res505, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res505^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model505@fit$ics[1])
BIC_list=append(BIC_list, model505@fit$ics[2])
u505=psstd(res505, mean=0, sd=1, nu=tail(model505@fit$coef, n=1), xi=model505@fit$coef[length(model505@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u505, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u505, null="punif")$p.value)

model506=garchFit(formula=~arma(4,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res506=residuals(model506, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res506, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res506^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model506@fit$ics[1])
BIC_list=append(BIC_list, model506@fit$ics[2])
u506=psstd(res506, mean=0, sd=1, nu=tail(model506@fit$coef, n=1), xi=model506@fit$coef[length(model506@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u506, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u506, null="punif")$p.value)

model507=garchFit(formula=~arma(4,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res507=residuals(model507, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res507, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res507^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model507@fit$ics[1])
BIC_list=append(BIC_list, model507@fit$ics[2])
u507=psstd(res507, mean=0, sd=1, nu=tail(model507@fit$coef, n=1), xi=model507@fit$coef[length(model507@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u507, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u507, null="punif")$p.value)

model508=garchFit(formula=~arma(4,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res508=residuals(model508, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res508, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res508^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model508@fit$ics[1])
BIC_list=append(BIC_list, model508@fit$ics[2])
u508=psstd(res508, mean=0, sd=1, nu=tail(model508@fit$coef, n=1), xi=model508@fit$coef[length(model508@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u508, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u508, null="punif")$p.value)

model509=garchFit(formula=~arma(4,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res509=residuals(model509, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res509, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res509^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model509@fit$ics[1])
BIC_list=append(BIC_list, model509@fit$ics[2])
u509=psstd(res509, mean=0, sd=1, nu=tail(model509@fit$coef, n=1), xi=model509@fit$coef[length(model509@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u509, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u509, null="punif")$p.value)

model510=garchFit(formula=~arma(4,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res510=residuals(model510, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res510, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res510^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model510@fit$ics[1])
BIC_list=append(BIC_list, model510@fit$ics[2])
u510=psstd(res510, mean=0, sd=1, nu=tail(model510@fit$coef, n=1), xi=model510@fit$coef[length(model510@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u510, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u510, null="punif")$p.value)

model511=garchFit(formula=~arma(4,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res511=residuals(model511, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res511, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res511^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model511@fit$ics[1])
BIC_list=append(BIC_list, model511@fit$ics[2])
u511=psstd(res511, mean=0, sd=1, nu=tail(model511@fit$coef, n=1), xi=model511@fit$coef[length(model511@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u511, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u511, null="punif")$p.value)

model512=garchFit(formula=~arma(4,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res512=residuals(model512, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res512, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res512^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model512@fit$ics[1])
BIC_list=append(BIC_list, model512@fit$ics[2])
u512=psstd(res512, mean=0, sd=1, nu=tail(model512@fit$coef, n=1), xi=model512@fit$coef[length(model512@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u512, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u512, null="punif")$p.value)

model513=garchFit(formula=~arma(4,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res513=residuals(model513, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res513, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res513^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model513@fit$ics[1])
BIC_list=append(BIC_list, model513@fit$ics[2])
u513=psstd(res513, mean=0, sd=1, nu=tail(model513@fit$coef, n=1), xi=model513@fit$coef[length(model513@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u513, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u513, null="punif")$p.value)

model514=garchFit(formula=~arma(4,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res514=residuals(model514, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res514, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res514^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model514@fit$ics[1])
BIC_list=append(BIC_list, model514@fit$ics[2])
u514=psstd(res514, mean=0, sd=1, nu=tail(model514@fit$coef, n=1), xi=model514@fit$coef[length(model514@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u514, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u514, null="punif")$p.value)

model515=garchFit(formula=~arma(4,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res515=residuals(model515, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res515, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res515^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model515@fit$ics[1])
BIC_list=append(BIC_list, model515@fit$ics[2])
u515=psstd(res515, mean=0, sd=1, nu=tail(model515@fit$coef, n=1), xi=model515@fit$coef[length(model515@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u515, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u515, null="punif")$p.value)

model516=garchFit(formula=~arma(4,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res516=residuals(model516, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res516, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res516^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model516@fit$ics[1])
BIC_list=append(BIC_list, model516@fit$ics[2])
u516=psstd(res516, mean=0, sd=1, nu=tail(model516@fit$coef, n=1), xi=model516@fit$coef[length(model516@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u516, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u516, null="punif")$p.value)

model517=garchFit(formula=~arma(4,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res517=residuals(model517, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res517, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res517^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model517@fit$ics[1])
BIC_list=append(BIC_list, model517@fit$ics[2])
u517=psstd(res517, mean=0, sd=1, nu=tail(model517@fit$coef, n=1), xi=model517@fit$coef[length(model517@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u517, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u517, null="punif")$p.value)

model518=garchFit(formula=~arma(4,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res518=residuals(model518, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res518, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res518^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model518@fit$ics[1])
BIC_list=append(BIC_list, model518@fit$ics[2])
u518=psstd(res518, mean=0, sd=1, nu=tail(model518@fit$coef, n=1), xi=model518@fit$coef[length(model518@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u518, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u518, null="punif")$p.value)

model519=garchFit(formula=~arma(4,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res519=residuals(model519, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res519, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res519^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model519@fit$ics[1])
BIC_list=append(BIC_list, model519@fit$ics[2])
u519=psstd(res519, mean=0, sd=1, nu=tail(model519@fit$coef, n=1), xi=model519@fit$coef[length(model519@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u519, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u519, null="punif")$p.value)

model520=garchFit(formula=~arma(4,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res520=residuals(model520, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res520, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res520^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model520@fit$ics[1])
BIC_list=append(BIC_list, model520@fit$ics[2])
u520=psstd(res520, mean=0, sd=1, nu=tail(model520@fit$coef, n=1), xi=model520@fit$coef[length(model520@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u520, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u520, null="punif")$p.value)

model521=garchFit(formula=~arma(4,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res521=residuals(model521, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res521, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res521^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model521@fit$ics[1])
BIC_list=append(BIC_list, model521@fit$ics[2])
u521=psstd(res521, mean=0, sd=1, nu=tail(model521@fit$coef, n=1), xi=model521@fit$coef[length(model521@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u521, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u521, null="punif")$p.value)

model522=garchFit(formula=~arma(4,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res522=residuals(model522, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res522, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res522^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model522@fit$ics[1])
BIC_list=append(BIC_list, model522@fit$ics[2])
u522=psstd(res522, mean=0, sd=1, nu=tail(model522@fit$coef, n=1), xi=model522@fit$coef[length(model522@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u522, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u522, null="punif")$p.value)

model523=garchFit(formula=~arma(4,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res523=residuals(model523, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res523, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res523^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model523@fit$ics[1])
BIC_list=append(BIC_list, model523@fit$ics[2])
u523=psstd(res523, mean=0, sd=1, nu=tail(model523@fit$coef, n=1), xi=model523@fit$coef[length(model523@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u523, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u523, null="punif")$p.value)

model524=garchFit(formula=~arma(4,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res524=residuals(model524, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res524, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res524^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model524@fit$ics[1])
BIC_list=append(BIC_list, model524@fit$ics[2])
u524=psstd(res524, mean=0, sd=1, nu=tail(model524@fit$coef, n=1), xi=model524@fit$coef[length(model524@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u524, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u524, null="punif")$p.value)

model525=garchFit(formula=~arma(4,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res525=residuals(model525, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res525, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res525^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model525@fit$ics[1])
BIC_list=append(BIC_list, model525@fit$ics[2])
u525=psstd(res525, mean=0, sd=1, nu=tail(model525@fit$coef, n=1), xi=model525@fit$coef[length(model525@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u525, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u525, null="punif")$p.value)

model526=garchFit(formula=~arma(4,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res526=residuals(model526, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res526, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res526^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model526@fit$ics[1])
BIC_list=append(BIC_list, model526@fit$ics[2])
u526=psstd(res526, mean=0, sd=1, nu=tail(model526@fit$coef, n=1), xi=model526@fit$coef[length(model526@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u526, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u526, null="punif")$p.value)

model527=garchFit(formula=~arma(4,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res527=residuals(model527, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res527, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res527^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model527@fit$ics[1])
BIC_list=append(BIC_list, model527@fit$ics[2])
u527=psstd(res527, mean=0, sd=1, nu=tail(model527@fit$coef, n=1), xi=model527@fit$coef[length(model527@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u527, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u527, null="punif")$p.value)

model528=garchFit(formula=~arma(4,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res528=residuals(model528, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res528, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res528^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model528@fit$ics[1])
BIC_list=append(BIC_list, model528@fit$ics[2])
u528=psstd(res528, mean=0, sd=1, nu=tail(model528@fit$coef, n=1), xi=model528@fit$coef[length(model528@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u528, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u528, null="punif")$p.value)

model529=garchFit(formula=~arma(4,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res529=residuals(model529, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res529, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res529^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model529@fit$ics[1])
BIC_list=append(BIC_list, model529@fit$ics[2])
u529=psstd(res529, mean=0, sd=1, nu=tail(model529@fit$coef, n=1), xi=model529@fit$coef[length(model529@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u529, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u529, null="punif")$p.value)

model530=garchFit(formula=~arma(4,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res530=residuals(model530, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res530, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res530^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model530@fit$ics[1])
BIC_list=append(BIC_list, model530@fit$ics[2])
u530=psstd(res530, mean=0, sd=1, nu=tail(model530@fit$coef, n=1), xi=model530@fit$coef[length(model530@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u530, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u530, null="punif")$p.value)

model531=garchFit(formula=~arma(4,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res531=residuals(model531, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res531, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res531^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model531@fit$ics[1])
BIC_list=append(BIC_list, model531@fit$ics[2])
u531=psstd(res531, mean=0, sd=1, nu=tail(model531@fit$coef, n=1), xi=model531@fit$coef[length(model531@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u531, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u531, null="punif")$p.value)

model532=garchFit(formula=~arma(4,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res532=residuals(model532, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res532, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res532^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model532@fit$ics[1])
BIC_list=append(BIC_list, model532@fit$ics[2])
u532=psstd(res532, mean=0, sd=1, nu=tail(model532@fit$coef, n=1), xi=model532@fit$coef[length(model532@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u532, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u532, null="punif")$p.value)

model533=garchFit(formula=~arma(4,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res533=residuals(model533, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res533, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res533^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model533@fit$ics[1])
BIC_list=append(BIC_list, model533@fit$ics[2])
u533=psstd(res533, mean=0, sd=1, nu=tail(model533@fit$coef, n=1), xi=model533@fit$coef[length(model533@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u533, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u533, null="punif")$p.value)

model534=garchFit(formula=~arma(4,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res534=residuals(model534, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res534, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res534^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model534@fit$ics[1])
BIC_list=append(BIC_list, model534@fit$ics[2])
u534=psstd(res534, mean=0, sd=1, nu=tail(model534@fit$coef, n=1), xi=model534@fit$coef[length(model534@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u534, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u534, null="punif")$p.value)

model535=garchFit(formula=~arma(4,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res535=residuals(model535, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res535, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res535^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model535@fit$ics[1])
BIC_list=append(BIC_list, model535@fit$ics[2])
u535=psstd(res535, mean=0, sd=1, nu=tail(model535@fit$coef, n=1), xi=model535@fit$coef[length(model535@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u535, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u535, null="punif")$p.value)

model536=garchFit(formula=~arma(4,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res536=residuals(model536, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res536, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res536^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model536@fit$ics[1])
BIC_list=append(BIC_list, model536@fit$ics[2])
u536=psstd(res536, mean=0, sd=1, nu=tail(model536@fit$coef, n=1), xi=model536@fit$coef[length(model536@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u536, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u536, null="punif")$p.value)

model537=garchFit(formula=~arma(4,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res537=residuals(model537, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res537, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res537^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model537@fit$ics[1])
BIC_list=append(BIC_list, model537@fit$ics[2])
u537=psstd(res537, mean=0, sd=1, nu=tail(model537@fit$coef, n=1), xi=model537@fit$coef[length(model537@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u537, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u537, null="punif")$p.value)

model538=garchFit(formula=~arma(4,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res538=residuals(model538, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res538, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res538^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model538@fit$ics[1])
BIC_list=append(BIC_list, model538@fit$ics[2])
u538=psstd(res538, mean=0, sd=1, nu=tail(model538@fit$coef, n=1), xi=model538@fit$coef[length(model538@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u538, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u538, null="punif")$p.value)

model539=garchFit(formula=~arma(4,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res539=residuals(model539, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res539, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res539^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model539@fit$ics[1])
BIC_list=append(BIC_list, model539@fit$ics[2])
u539=psstd(res539, mean=0, sd=1, nu=tail(model539@fit$coef, n=1), xi=model539@fit$coef[length(model539@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u539, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u539, null="punif")$p.value)

model540=garchFit(formula=~arma(4,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res540=residuals(model540, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res540, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res540^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model540@fit$ics[1])
BIC_list=append(BIC_list, model540@fit$ics[2])
u540=psstd(res540, mean=0, sd=1, nu=tail(model540@fit$coef, n=1), xi=model540@fit$coef[length(model540@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u540, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u540, null="punif")$p.value)

model541=garchFit(formula=~arma(7,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res541=residuals(model541, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res541, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res541^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model541@fit$ics[1])
BIC_list=append(BIC_list, model541@fit$ics[2])
u541=psstd(res541, mean=0, sd=1, nu=tail(model541@fit$coef, n=1), xi=model541@fit$coef[length(model541@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u541, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u541, null="punif")$p.value)

model542=garchFit(formula=~arma(7,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res542=residuals(model542, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res542, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res542^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model542@fit$ics[1])
BIC_list=append(BIC_list, model542@fit$ics[2])
u542=psstd(res542, mean=0, sd=1, nu=tail(model542@fit$coef, n=1), xi=model542@fit$coef[length(model542@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u542, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u542, null="punif")$p.value)

model543=garchFit(formula=~arma(7,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res543=residuals(model543, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res543, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res543^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model543@fit$ics[1])
BIC_list=append(BIC_list, model543@fit$ics[2])
u543=psstd(res543, mean=0, sd=1, nu=tail(model543@fit$coef, n=1), xi=model543@fit$coef[length(model543@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u543, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u543, null="punif")$p.value)

model544=garchFit(formula=~arma(7,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res544=residuals(model544, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res544, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res544^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model544@fit$ics[1])
BIC_list=append(BIC_list, model544@fit$ics[2])
u544=psstd(res544, mean=0, sd=1, nu=tail(model544@fit$coef, n=1), xi=model544@fit$coef[length(model544@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u544, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u544, null="punif")$p.value)

model545=garchFit(formula=~arma(7,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res545=residuals(model545, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res545, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res545^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model545@fit$ics[1])
BIC_list=append(BIC_list, model545@fit$ics[2])
u545=psstd(res545, mean=0, sd=1, nu=tail(model545@fit$coef, n=1), xi=model545@fit$coef[length(model545@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u545, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u545, null="punif")$p.value)

model546=garchFit(formula=~arma(7,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res546=residuals(model546, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res546, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res546^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model546@fit$ics[1])
BIC_list=append(BIC_list, model546@fit$ics[2])
u546=psstd(res546, mean=0, sd=1, nu=tail(model546@fit$coef, n=1), xi=model546@fit$coef[length(model546@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u546, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u546, null="punif")$p.value)

model547=garchFit(formula=~arma(7,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res547=residuals(model547, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res547, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res547^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model547@fit$ics[1])
BIC_list=append(BIC_list, model547@fit$ics[2])
u547=psstd(res547, mean=0, sd=1, nu=tail(model547@fit$coef, n=1), xi=model547@fit$coef[length(model547@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u547, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u547, null="punif")$p.value)

model548=garchFit(formula=~arma(7,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res548=residuals(model548, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res548, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res548^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model548@fit$ics[1])
BIC_list=append(BIC_list, model548@fit$ics[2])
u548=psstd(res548, mean=0, sd=1, nu=tail(model548@fit$coef, n=1), xi=model548@fit$coef[length(model548@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u548, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u548, null="punif")$p.value)

model549=garchFit(formula=~arma(7,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res549=residuals(model549, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res549, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res549^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model549@fit$ics[1])
BIC_list=append(BIC_list, model549@fit$ics[2])
u549=psstd(res549, mean=0, sd=1, nu=tail(model549@fit$coef, n=1), xi=model549@fit$coef[length(model549@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u549, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u549, null="punif")$p.value)

model550=garchFit(formula=~arma(7,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res550=residuals(model550, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res550, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res550^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model550@fit$ics[1])
BIC_list=append(BIC_list, model550@fit$ics[2])
u550=psstd(res550, mean=0, sd=1, nu=tail(model550@fit$coef, n=1), xi=model550@fit$coef[length(model550@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u550, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u550, null="punif")$p.value)

model551=garchFit(formula=~arma(7,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res551=residuals(model551, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res551, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res551^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model551@fit$ics[1])
BIC_list=append(BIC_list, model551@fit$ics[2])
u551=psstd(res551, mean=0, sd=1, nu=tail(model551@fit$coef, n=1), xi=model551@fit$coef[length(model551@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u551, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u551, null="punif")$p.value)

model552=garchFit(formula=~arma(7,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res552=residuals(model552, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res552, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res552^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model552@fit$ics[1])
BIC_list=append(BIC_list, model552@fit$ics[2])
u552=psstd(res552, mean=0, sd=1, nu=tail(model552@fit$coef, n=1), xi=model552@fit$coef[length(model552@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u552, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u552, null="punif")$p.value)

model553=garchFit(formula=~arma(7,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res553=residuals(model553, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res553, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res553^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model553@fit$ics[1])
BIC_list=append(BIC_list, model553@fit$ics[2])
u553=psstd(res553, mean=0, sd=1, nu=tail(model553@fit$coef, n=1), xi=model553@fit$coef[length(model553@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u553, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u553, null="punif")$p.value)

model554=garchFit(formula=~arma(7,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res554=residuals(model554, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res554, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res554^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model554@fit$ics[1])
BIC_list=append(BIC_list, model554@fit$ics[2])
u554=psstd(res554, mean=0, sd=1, nu=tail(model554@fit$coef, n=1), xi=model554@fit$coef[length(model554@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u554, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u554, null="punif")$p.value)

model555=garchFit(formula=~arma(7,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res555=residuals(model555, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res555, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res555^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model555@fit$ics[1])
BIC_list=append(BIC_list, model555@fit$ics[2])
u555=psstd(res555, mean=0, sd=1, nu=tail(model555@fit$coef, n=1), xi=model555@fit$coef[length(model555@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u555, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u555, null="punif")$p.value)

model556=garchFit(formula=~arma(7,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res556=residuals(model556, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res556, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res556^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model556@fit$ics[1])
BIC_list=append(BIC_list, model556@fit$ics[2])
u556=psstd(res556, mean=0, sd=1, nu=tail(model556@fit$coef, n=1), xi=model556@fit$coef[length(model556@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u556, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u556, null="punif")$p.value)

model557=garchFit(formula=~arma(7,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res557=residuals(model557, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res557, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res557^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model557@fit$ics[1])
BIC_list=append(BIC_list, model557@fit$ics[2])
u557=psstd(res557, mean=0, sd=1, nu=tail(model557@fit$coef, n=1), xi=model557@fit$coef[length(model557@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u557, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u557, null="punif")$p.value)

model558=garchFit(formula=~arma(7,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res558=residuals(model558, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res558, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res558^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model558@fit$ics[1])
BIC_list=append(BIC_list, model558@fit$ics[2])
u558=psstd(res558, mean=0, sd=1, nu=tail(model558@fit$coef, n=1), xi=model558@fit$coef[length(model558@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u558, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u558, null="punif")$p.value)

model559=garchFit(formula=~arma(7,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res559=residuals(model559, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res559, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res559^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model559@fit$ics[1])
BIC_list=append(BIC_list, model559@fit$ics[2])
u559=psstd(res559, mean=0, sd=1, nu=tail(model559@fit$coef, n=1), xi=model559@fit$coef[length(model559@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u559, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u559, null="punif")$p.value)

model560=garchFit(formula=~arma(7,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res560=residuals(model560, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res560, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res560^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model560@fit$ics[1])
BIC_list=append(BIC_list, model560@fit$ics[2])
u560=psstd(res560, mean=0, sd=1, nu=tail(model560@fit$coef, n=1), xi=model560@fit$coef[length(model560@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u560, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u560, null="punif")$p.value)

model561=garchFit(formula=~arma(7,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res561=residuals(model561, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res561, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res561^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model561@fit$ics[1])
BIC_list=append(BIC_list, model561@fit$ics[2])
u561=psstd(res561, mean=0, sd=1, nu=tail(model561@fit$coef, n=1), xi=model561@fit$coef[length(model561@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u561, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u561, null="punif")$p.value)

model562=garchFit(formula=~arma(7,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res562=residuals(model562, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res562, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res562^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model562@fit$ics[1])
BIC_list=append(BIC_list, model562@fit$ics[2])
u562=psstd(res562, mean=0, sd=1, nu=tail(model562@fit$coef, n=1), xi=model562@fit$coef[length(model562@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u562, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u562, null="punif")$p.value)

model563=garchFit(formula=~arma(7,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res563=residuals(model563, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res563, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res563^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model563@fit$ics[1])
BIC_list=append(BIC_list, model563@fit$ics[2])
u563=psstd(res563, mean=0, sd=1, nu=tail(model563@fit$coef, n=1), xi=model563@fit$coef[length(model563@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u563, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u563, null="punif")$p.value)

model564=garchFit(formula=~arma(7,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res564=residuals(model564, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res564, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res564^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model564@fit$ics[1])
BIC_list=append(BIC_list, model564@fit$ics[2])
u564=psstd(res564, mean=0, sd=1, nu=tail(model564@fit$coef, n=1), xi=model564@fit$coef[length(model564@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u564, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u564, null="punif")$p.value)

model565=garchFit(formula=~arma(7,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res565=residuals(model565, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res565, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res565^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model565@fit$ics[1])
BIC_list=append(BIC_list, model565@fit$ics[2])
u565=psstd(res565, mean=0, sd=1, nu=tail(model565@fit$coef, n=1), xi=model565@fit$coef[length(model565@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u565, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u565, null="punif")$p.value)

model566=garchFit(formula=~arma(7,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res566=residuals(model566, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res566, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res566^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model566@fit$ics[1])
BIC_list=append(BIC_list, model566@fit$ics[2])
u566=psstd(res566, mean=0, sd=1, nu=tail(model566@fit$coef, n=1), xi=model566@fit$coef[length(model566@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u566, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u566, null="punif")$p.value)

model567=garchFit(formula=~arma(7,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res567=residuals(model567, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res567, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res567^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model567@fit$ics[1])
BIC_list=append(BIC_list, model567@fit$ics[2])
u567=psstd(res567, mean=0, sd=1, nu=tail(model567@fit$coef, n=1), xi=model567@fit$coef[length(model567@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u567, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u567, null="punif")$p.value)

model568=garchFit(formula=~arma(7,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sstd")
res568=residuals(model568, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res568, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res568^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model568@fit$ics[1])
BIC_list=append(BIC_list, model568@fit$ics[2])
u568=psstd(res568, mean=0, sd=1, nu=tail(model568@fit$coef, n=1), xi=model568@fit$coef[length(model568@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u568, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u568, null="punif")$p.value)

model569=garchFit(formula=~arma(7,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sstd")
res569=residuals(model569, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res569, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res569^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model569@fit$ics[1])
BIC_list=append(BIC_list, model569@fit$ics[2])
u569=psstd(res569, mean=0, sd=1, nu=tail(model569@fit$coef, n=1), xi=model569@fit$coef[length(model569@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u569, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u569, null="punif")$p.value)

model570=garchFit(formula=~arma(7,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sstd")
res570=residuals(model570, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res570, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res570^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model570@fit$ics[1])
BIC_list=append(BIC_list, model570@fit$ics[2])
u570=psstd(res570, mean=0, sd=1, nu=tail(model570@fit$coef, n=1), xi=model570@fit$coef[length(model570@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u570, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u570, null="punif")$p.value)

model571=garchFit(formula=~arma(7,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sstd")
res571=residuals(model571, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res571, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res571^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model571@fit$ics[1])
BIC_list=append(BIC_list, model571@fit$ics[2])
u571=psstd(res571, mean=0, sd=1, nu=tail(model571@fit$coef, n=1), xi=model571@fit$coef[length(model571@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u571, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u571, null="punif")$p.value)

model572=garchFit(formula=~arma(7,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sstd")
res572=residuals(model572, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res572, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res572^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model572@fit$ics[1])
BIC_list=append(BIC_list, model572@fit$ics[2])
u572=psstd(res572, mean=0, sd=1, nu=tail(model572@fit$coef, n=1), xi=model572@fit$coef[length(model572@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u572, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u572, null="punif")$p.value)

model573=garchFit(formula=~arma(7,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sstd")
res573=residuals(model573, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res573, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res573^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model573@fit$ics[1])
BIC_list=append(BIC_list, model573@fit$ics[2])
u573=psstd(res573, mean=0, sd=1, nu=tail(model573@fit$coef, n=1), xi=model573@fit$coef[length(model573@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u573, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u573, null="punif")$p.value)

model574=garchFit(formula=~arma(7,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sstd")
res574=residuals(model574, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res574, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res574^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model574@fit$ics[1])
BIC_list=append(BIC_list, model574@fit$ics[2])
u574=psstd(res574, mean=0, sd=1, nu=tail(model574@fit$coef, n=1), xi=model574@fit$coef[length(model574@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u574, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u574, null="punif")$p.value)

model575=garchFit(formula=~arma(7,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sstd")
res575=residuals(model575, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res575, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res575^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model575@fit$ics[1])
BIC_list=append(BIC_list, model575@fit$ics[2])
u575=psstd(res575, mean=0, sd=1, nu=tail(model575@fit$coef, n=1), xi=model575@fit$coef[length(model575@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u575, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u575, null="punif")$p.value)

model576=garchFit(formula=~arma(7,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sstd")
res576=residuals(model576, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res576, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res576^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model576@fit$ics[1])
BIC_list=append(BIC_list, model576@fit$ics[2])
u576=psstd(res576, mean=0, sd=1, nu=tail(model576@fit$coef, n=1), xi=model576@fit$coef[length(model576@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u576, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u576, null="punif")$p.value)

model577=garchFit(formula=~arma(1,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res577=residuals(model577, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res577, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res577^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model577@fit$ics[1])
BIC_list=append(BIC_list, model577@fit$ics[2])
u577=psnorm(res577, mean=0, sd=1, xi=tail(model577@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u577, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u577, null="punif")$p.value)

model578=garchFit(formula=~arma(1,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res578=residuals(model578, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res578, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res578^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model578@fit$ics[1])
BIC_list=append(BIC_list, model578@fit$ics[2])
u578=psnorm(res578, mean=0, sd=1, xi=tail(model578@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u578, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u578, null="punif")$p.value)

model579=garchFit(formula=~arma(1,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res579=residuals(model579, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res579, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res579^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model579@fit$ics[1])
BIC_list=append(BIC_list, model579@fit$ics[2])
u579=psnorm(res579, mean=0, sd=1, xi=tail(model579@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u579, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u579, null="punif")$p.value)

model580=garchFit(formula=~arma(1,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res580=residuals(model580, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res580, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res580^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model580@fit$ics[1])
BIC_list=append(BIC_list, model580@fit$ics[2])
u580=psnorm(res580, mean=0, sd=1, xi=tail(model580@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u580, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u580, null="punif")$p.value)

model581=garchFit(formula=~arma(1,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res581=residuals(model581, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res581, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res581^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model581@fit$ics[1])
BIC_list=append(BIC_list, model581@fit$ics[2])
u581=psnorm(res581, mean=0, sd=1, xi=tail(model581@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u581, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u581, null="punif")$p.value)

model582=garchFit(formula=~arma(1,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res582=residuals(model582, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res582, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res582^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model582@fit$ics[1])
BIC_list=append(BIC_list, model582@fit$ics[2])
u582=psnorm(res582, mean=0, sd=1, xi=tail(model582@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u582, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u582, null="punif")$p.value)

model583=garchFit(formula=~arma(1,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res583=residuals(model583, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res583, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res583^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model583@fit$ics[1])
BIC_list=append(BIC_list, model583@fit$ics[2])
u583=psnorm(res583, mean=0, sd=1, xi=tail(model583@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u583, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u583, null="punif")$p.value)

model584=garchFit(formula=~arma(1,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res584=residuals(model584, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res584, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res584^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model584@fit$ics[1])
BIC_list=append(BIC_list, model584@fit$ics[2])
u584=psnorm(res584, mean=0, sd=1, xi=tail(model584@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u584, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u584, null="punif")$p.value)

model585=garchFit(formula=~arma(1,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res585=residuals(model585, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res585, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res585^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model585@fit$ics[1])
BIC_list=append(BIC_list, model585@fit$ics[2])
u585=psnorm(res585, mean=0, sd=1, xi=tail(model585@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u585, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u585, null="punif")$p.value)

model586=garchFit(formula=~arma(1,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res586=residuals(model586, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res586, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res586^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model586@fit$ics[1])
BIC_list=append(BIC_list, model586@fit$ics[2])
u586=psnorm(res586, mean=0, sd=1, xi=tail(model586@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u586, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u586, null="punif")$p.value)

model587=garchFit(formula=~arma(1,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res587=residuals(model587, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res587, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res587^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model587@fit$ics[1])
BIC_list=append(BIC_list, model587@fit$ics[2])
u587=psnorm(res587, mean=0, sd=1, xi=tail(model587@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u587, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u587, null="punif")$p.value)

model588=garchFit(formula=~arma(1,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res588=residuals(model588, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res588, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res588^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model588@fit$ics[1])
BIC_list=append(BIC_list, model588@fit$ics[2])
u588=psnorm(res588, mean=0, sd=1, xi=tail(model588@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u588, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u588, null="punif")$p.value)

model589=garchFit(formula=~arma(1,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res589=residuals(model589, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res589, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res589^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model589@fit$ics[1])
BIC_list=append(BIC_list, model589@fit$ics[2])
u589=psnorm(res589, mean=0, sd=1, xi=tail(model589@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u589, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u589, null="punif")$p.value)

model590=garchFit(formula=~arma(1,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res590=residuals(model590, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res590, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res590^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model590@fit$ics[1])
BIC_list=append(BIC_list, model590@fit$ics[2])
u590=psnorm(res590, mean=0, sd=1, xi=tail(model590@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u590, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u590, null="punif")$p.value)

model591=garchFit(formula=~arma(1,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res591=residuals(model591, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res591, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res591^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model591@fit$ics[1])
BIC_list=append(BIC_list, model591@fit$ics[2])
u591=psnorm(res591, mean=0, sd=1, xi=tail(model591@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u591, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u591, null="punif")$p.value)

model592=garchFit(formula=~arma(1,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res592=residuals(model592, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res592, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res592^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model592@fit$ics[1])
BIC_list=append(BIC_list, model592@fit$ics[2])
u592=psnorm(res592, mean=0, sd=1, xi=tail(model592@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u592, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u592, null="punif")$p.value)

model593=garchFit(formula=~arma(1,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res593=residuals(model593, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res593, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res593^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model593@fit$ics[1])
BIC_list=append(BIC_list, model593@fit$ics[2])
u593=psnorm(res593, mean=0, sd=1, xi=tail(model593@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u593, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u593, null="punif")$p.value)

model594=garchFit(formula=~arma(1,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res594=residuals(model594, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res594, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res594^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model594@fit$ics[1])
BIC_list=append(BIC_list, model594@fit$ics[2])
u594=psnorm(res594, mean=0, sd=1, xi=tail(model594@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u594, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u594, null="punif")$p.value)

model595=garchFit(formula=~arma(1,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res595=residuals(model595, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res595, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res595^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model595@fit$ics[1])
BIC_list=append(BIC_list, model595@fit$ics[2])
u595=psnorm(res595, mean=0, sd=1, xi=tail(model595@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u595, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u595, null="punif")$p.value)

model596=garchFit(formula=~arma(1,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res596=residuals(model596, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res596, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res596^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model596@fit$ics[1])
BIC_list=append(BIC_list, model596@fit$ics[2])
u596=psnorm(res596, mean=0, sd=1, xi=tail(model596@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u596, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u596, null="punif")$p.value)

model597=garchFit(formula=~arma(1,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res597=residuals(model597, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res597, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res597^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model597@fit$ics[1])
BIC_list=append(BIC_list, model597@fit$ics[2])
u597=psnorm(res597, mean=0, sd=1, xi=tail(model597@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u597, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u597, null="punif")$p.value)

model598=garchFit(formula=~arma(1,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res598=residuals(model598, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res598, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res598^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model598@fit$ics[1])
BIC_list=append(BIC_list, model598@fit$ics[2])
u598=psnorm(res598, mean=0, sd=1, xi=tail(model598@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u598, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u598, null="punif")$p.value)

model599=garchFit(formula=~arma(1,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res599=residuals(model599, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res599, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res599^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model599@fit$ics[1])
BIC_list=append(BIC_list, model599@fit$ics[2])
u599=psnorm(res599, mean=0, sd=1, xi=tail(model599@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u599, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u599, null="punif")$p.value)

model600=garchFit(formula=~arma(1,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res600=residuals(model600, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res600, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res600^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model600@fit$ics[1])
BIC_list=append(BIC_list, model600@fit$ics[2])
u600=psnorm(res600, mean=0, sd=1, xi=tail(model600@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u600, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u600, null="punif")$p.value)

model601=garchFit(formula=~arma(1,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res601=residuals(model601, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res601, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res601^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model601@fit$ics[1])
BIC_list=append(BIC_list, model601@fit$ics[2])
u601=psnorm(res601, mean=0, sd=1, xi=tail(model601@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u601, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u601, null="punif")$p.value)

model602=garchFit(formula=~arma(1,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res602=residuals(model602, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res602, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res602^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model602@fit$ics[1])
BIC_list=append(BIC_list, model602@fit$ics[2])
u602=psnorm(res602, mean=0, sd=1, xi=tail(model602@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u602, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u602, null="punif")$p.value)

model603=garchFit(formula=~arma(1,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res603=residuals(model603, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res603, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res603^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model603@fit$ics[1])
BIC_list=append(BIC_list, model603@fit$ics[2])
u603=psnorm(res603, mean=0, sd=1, xi=tail(model603@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u603, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u603, null="punif")$p.value)

model604=garchFit(formula=~arma(1,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res604=residuals(model604, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res604, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res604^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model604@fit$ics[1])
BIC_list=append(BIC_list, model604@fit$ics[2])
u604=psnorm(res604, mean=0, sd=1, xi=tail(model604@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u604, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u604, null="punif")$p.value)

model605=garchFit(formula=~arma(1,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res605=residuals(model605, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res605, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res605^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model605@fit$ics[1])
BIC_list=append(BIC_list, model605@fit$ics[2])
u605=psnorm(res605, mean=0, sd=1, xi=tail(model605@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u605, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u605, null="punif")$p.value)

model606=garchFit(formula=~arma(1,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res606=residuals(model606, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res606, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res606^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model606@fit$ics[1])
BIC_list=append(BIC_list, model606@fit$ics[2])
u606=psnorm(res606, mean=0, sd=1, xi=tail(model606@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u606, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u606, null="punif")$p.value)

model607=garchFit(formula=~arma(1,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res607=residuals(model607, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res607, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res607^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model607@fit$ics[1])
BIC_list=append(BIC_list, model607@fit$ics[2])
u607=psnorm(res607, mean=0, sd=1, xi=tail(model607@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u607, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u607, null="punif")$p.value)

model608=garchFit(formula=~arma(1,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res608=residuals(model608, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res608, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res608^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model608@fit$ics[1])
BIC_list=append(BIC_list, model608@fit$ics[2])
u608=psnorm(res608, mean=0, sd=1, xi=tail(model608@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u608, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u608, null="punif")$p.value)

model609=garchFit(formula=~arma(1,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res609=residuals(model609, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res609, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res609^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model609@fit$ics[1])
BIC_list=append(BIC_list, model609@fit$ics[2])
u609=psnorm(res609, mean=0, sd=1, xi=tail(model609@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u609, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u609, null="punif")$p.value)

model610=garchFit(formula=~arma(1,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res610=residuals(model610, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res610, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res610^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model610@fit$ics[1])
BIC_list=append(BIC_list, model610@fit$ics[2])
u610=psnorm(res610, mean=0, sd=1, xi=tail(model610@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u610, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u610, null="punif")$p.value)

model611=garchFit(formula=~arma(1,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res611=residuals(model611, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res611, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res611^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model611@fit$ics[1])
BIC_list=append(BIC_list, model611@fit$ics[2])
u611=psnorm(res611, mean=0, sd=1, xi=tail(model611@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u611, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u611, null="punif")$p.value)

model612=garchFit(formula=~arma(1,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res612=residuals(model612, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res612, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res612^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model612@fit$ics[1])
BIC_list=append(BIC_list, model612@fit$ics[2])
u612=psnorm(res612, mean=0, sd=1, xi=tail(model612@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u612, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u612, null="punif")$p.value)

model613=garchFit(formula=~arma(3,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res613=residuals(model613, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res613, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res613^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model613@fit$ics[1])
BIC_list=append(BIC_list, model613@fit$ics[2])
u613=psnorm(res613, mean=0, sd=1, xi=tail(model613@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u613, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u613, null="punif")$p.value)

model614=garchFit(formula=~arma(3,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res614=residuals(model614, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res614, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res614^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model614@fit$ics[1])
BIC_list=append(BIC_list, model614@fit$ics[2])
u614=psnorm(res614, mean=0, sd=1, xi=tail(model614@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u614, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u614, null="punif")$p.value)

model615=garchFit(formula=~arma(3,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res615=residuals(model615, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res615, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res615^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model615@fit$ics[1])
BIC_list=append(BIC_list, model615@fit$ics[2])
u615=psnorm(res615, mean=0, sd=1, xi=tail(model615@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u615, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u615, null="punif")$p.value)

model616=garchFit(formula=~arma(3,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res616=residuals(model616, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res616, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res616^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model616@fit$ics[1])
BIC_list=append(BIC_list, model616@fit$ics[2])
u616=psnorm(res616, mean=0, sd=1, xi=tail(model616@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u616, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u616, null="punif")$p.value)

model617=garchFit(formula=~arma(3,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res617=residuals(model617, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res617, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res617^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model617@fit$ics[1])
BIC_list=append(BIC_list, model617@fit$ics[2])
u617=psnorm(res617, mean=0, sd=1, xi=tail(model617@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u617, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u617, null="punif")$p.value)

model618=garchFit(formula=~arma(3,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res618=residuals(model618, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res618, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res618^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model618@fit$ics[1])
BIC_list=append(BIC_list, model618@fit$ics[2])
u618=psnorm(res618, mean=0, sd=1, xi=tail(model618@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u618, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u618, null="punif")$p.value)

model619=garchFit(formula=~arma(3,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res619=residuals(model619, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res619, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res619^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model619@fit$ics[1])
BIC_list=append(BIC_list, model619@fit$ics[2])
u619=psnorm(res619, mean=0, sd=1, xi=tail(model619@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u619, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u619, null="punif")$p.value)

model620=garchFit(formula=~arma(3,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res620=residuals(model620, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res620, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res620^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model620@fit$ics[1])
BIC_list=append(BIC_list, model620@fit$ics[2])
u620=psnorm(res620, mean=0, sd=1, xi=tail(model620@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u620, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u620, null="punif")$p.value)

model621=garchFit(formula=~arma(3,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res621=residuals(model621, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res621, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res621^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model621@fit$ics[1])
BIC_list=append(BIC_list, model621@fit$ics[2])
u621=psnorm(res621, mean=0, sd=1, xi=tail(model621@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u621, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u621, null="punif")$p.value)

model622=garchFit(formula=~arma(3,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res622=residuals(model622, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res622, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res622^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model622@fit$ics[1])
BIC_list=append(BIC_list, model622@fit$ics[2])
u622=psnorm(res622, mean=0, sd=1, xi=tail(model622@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u622, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u622, null="punif")$p.value)

model623=garchFit(formula=~arma(3,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res623=residuals(model623, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res623, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res623^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model623@fit$ics[1])
BIC_list=append(BIC_list, model623@fit$ics[2])
u623=psnorm(res623, mean=0, sd=1, xi=tail(model623@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u623, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u623, null="punif")$p.value)

model624=garchFit(formula=~arma(3,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res624=residuals(model624, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res624, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res624^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model624@fit$ics[1])
BIC_list=append(BIC_list, model624@fit$ics[2])
u624=psnorm(res624, mean=0, sd=1, xi=tail(model624@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u624, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u624, null="punif")$p.value)

model625=garchFit(formula=~arma(3,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res625=residuals(model625, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res625, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res625^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model625@fit$ics[1])
BIC_list=append(BIC_list, model625@fit$ics[2])
u625=psnorm(res625, mean=0, sd=1, xi=tail(model625@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u625, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u625, null="punif")$p.value)

model626=garchFit(formula=~arma(3,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res626=residuals(model626, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res626, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res626^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model626@fit$ics[1])
BIC_list=append(BIC_list, model626@fit$ics[2])
u626=psnorm(res626, mean=0, sd=1, xi=tail(model626@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u626, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u626, null="punif")$p.value)

model627=garchFit(formula=~arma(3,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res627=residuals(model627, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res627, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res627^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model627@fit$ics[1])
BIC_list=append(BIC_list, model627@fit$ics[2])
u627=psnorm(res627, mean=0, sd=1, xi=tail(model627@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u627, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u627, null="punif")$p.value)

model628=garchFit(formula=~arma(3,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res628=residuals(model628, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res628, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res628^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model628@fit$ics[1])
BIC_list=append(BIC_list, model628@fit$ics[2])
u628=psnorm(res628, mean=0, sd=1, xi=tail(model628@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u628, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u628, null="punif")$p.value)

model629=garchFit(formula=~arma(3,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res629=residuals(model629, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res629, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res629^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model629@fit$ics[1])
BIC_list=append(BIC_list, model629@fit$ics[2])
u629=psnorm(res629, mean=0, sd=1, xi=tail(model629@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u629, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u629, null="punif")$p.value)

model630=garchFit(formula=~arma(3,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res630=residuals(model630, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res630, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res630^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model630@fit$ics[1])
BIC_list=append(BIC_list, model630@fit$ics[2])
u630=psnorm(res630, mean=0, sd=1, xi=tail(model630@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u630, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u630, null="punif")$p.value)

model631=garchFit(formula=~arma(3,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res631=residuals(model631, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res631, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res631^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model631@fit$ics[1])
BIC_list=append(BIC_list, model631@fit$ics[2])
u631=psnorm(res631, mean=0, sd=1, xi=tail(model631@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u631, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u631, null="punif")$p.value)

model632=garchFit(formula=~arma(3,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res632=residuals(model632, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res632, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res632^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model632@fit$ics[1])
BIC_list=append(BIC_list, model632@fit$ics[2])
u632=psnorm(res632, mean=0, sd=1, xi=tail(model632@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u632, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u632, null="punif")$p.value)

model633=garchFit(formula=~arma(3,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res633=residuals(model633, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res633, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res633^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model633@fit$ics[1])
BIC_list=append(BIC_list, model633@fit$ics[2])
u633=psnorm(res633, mean=0, sd=1, xi=tail(model633@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u633, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u633, null="punif")$p.value)

model634=garchFit(formula=~arma(3,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res634=residuals(model634, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res634, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res634^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model634@fit$ics[1])
BIC_list=append(BIC_list, model634@fit$ics[2])
u634=psnorm(res634, mean=0, sd=1, xi=tail(model634@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u634, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u634, null="punif")$p.value)

model635=garchFit(formula=~arma(3,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res635=residuals(model635, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res635, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res635^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model635@fit$ics[1])
BIC_list=append(BIC_list, model635@fit$ics[2])
u635=psnorm(res635, mean=0, sd=1, xi=tail(model635@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u635, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u635, null="punif")$p.value)

model636=garchFit(formula=~arma(3,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res636=residuals(model636, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res636, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res636^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model636@fit$ics[1])
BIC_list=append(BIC_list, model636@fit$ics[2])
u636=psnorm(res636, mean=0, sd=1, xi=tail(model636@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u636, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u636, null="punif")$p.value)

model637=garchFit(formula=~arma(3,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res637=residuals(model637, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res637, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res637^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model637@fit$ics[1])
BIC_list=append(BIC_list, model637@fit$ics[2])
u637=psnorm(res637, mean=0, sd=1, xi=tail(model637@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u637, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u637, null="punif")$p.value)

model638=garchFit(formula=~arma(3,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res638=residuals(model638, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res638, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res638^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model638@fit$ics[1])
BIC_list=append(BIC_list, model638@fit$ics[2])
u638=psnorm(res638, mean=0, sd=1, xi=tail(model638@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u638, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u638, null="punif")$p.value)

model639=garchFit(formula=~arma(3,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res639=residuals(model639, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res639, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res639^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model639@fit$ics[1])
BIC_list=append(BIC_list, model639@fit$ics[2])
u639=psnorm(res639, mean=0, sd=1, xi=tail(model639@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u639, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u639, null="punif")$p.value)

model640=garchFit(formula=~arma(3,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res640=residuals(model640, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res640, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res640^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model640@fit$ics[1])
BIC_list=append(BIC_list, model640@fit$ics[2])
u640=psnorm(res640, mean=0, sd=1, xi=tail(model640@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u640, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u640, null="punif")$p.value)

model641=garchFit(formula=~arma(3,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res641=residuals(model641, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res641, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res641^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model641@fit$ics[1])
BIC_list=append(BIC_list, model641@fit$ics[2])
u641=psnorm(res641, mean=0, sd=1, xi=tail(model641@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u641, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u641, null="punif")$p.value)

model642=garchFit(formula=~arma(3,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res642=residuals(model642, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res642, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res642^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model642@fit$ics[1])
BIC_list=append(BIC_list, model642@fit$ics[2])
u642=psnorm(res642, mean=0, sd=1, xi=tail(model642@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u642, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u642, null="punif")$p.value)

model643=garchFit(formula=~arma(3,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res643=residuals(model643, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res643, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res643^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model643@fit$ics[1])
BIC_list=append(BIC_list, model643@fit$ics[2])
u643=psnorm(res643, mean=0, sd=1, xi=tail(model643@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u643, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u643, null="punif")$p.value)

model644=garchFit(formula=~arma(3,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res644=residuals(model644, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res644, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res644^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model644@fit$ics[1])
BIC_list=append(BIC_list, model644@fit$ics[2])
u644=psnorm(res644, mean=0, sd=1, xi=tail(model644@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u644, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u644, null="punif")$p.value)

model645=garchFit(formula=~arma(3,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res645=residuals(model645, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res645, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res645^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model645@fit$ics[1])
BIC_list=append(BIC_list, model645@fit$ics[2])
u645=psnorm(res645, mean=0, sd=1, xi=tail(model645@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u645, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u645, null="punif")$p.value)

model646=garchFit(formula=~arma(3,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res646=residuals(model646, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res646, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res646^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model646@fit$ics[1])
BIC_list=append(BIC_list, model646@fit$ics[2])
u646=psnorm(res646, mean=0, sd=1, xi=tail(model646@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u646, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u646, null="punif")$p.value)

model647=garchFit(formula=~arma(3,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res647=residuals(model647, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res647, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res647^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model647@fit$ics[1])
BIC_list=append(BIC_list, model647@fit$ics[2])
u647=psnorm(res647, mean=0, sd=1, xi=tail(model647@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u647, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u647, null="punif")$p.value)

model648=garchFit(formula=~arma(3,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res648=residuals(model648, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res648, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res648^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model648@fit$ics[1])
BIC_list=append(BIC_list, model648@fit$ics[2])
u648=psnorm(res648, mean=0, sd=1, xi=tail(model648@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u648, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u648, null="punif")$p.value)

model649=garchFit(formula=~arma(4,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res649=residuals(model649, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res649, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res649^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model649@fit$ics[1])
BIC_list=append(BIC_list, model649@fit$ics[2])
u649=psnorm(res649, mean=0, sd=1, xi=tail(model649@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u649, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u649, null="punif")$p.value)

model650=garchFit(formula=~arma(4,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res650=residuals(model650, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res650, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res650^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model650@fit$ics[1])
BIC_list=append(BIC_list, model650@fit$ics[2])
u650=psnorm(res650, mean=0, sd=1, xi=tail(model650@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u650, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u650, null="punif")$p.value)

model651=garchFit(formula=~arma(4,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res651=residuals(model651, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res651, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res651^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model651@fit$ics[1])
BIC_list=append(BIC_list, model651@fit$ics[2])
u651=psnorm(res651, mean=0, sd=1, xi=tail(model651@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u651, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u651, null="punif")$p.value)

model652=garchFit(formula=~arma(4,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res652=residuals(model652, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res652, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res652^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model652@fit$ics[1])
BIC_list=append(BIC_list, model652@fit$ics[2])
u652=psnorm(res652, mean=0, sd=1, xi=tail(model652@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u652, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u652, null="punif")$p.value)

model653=garchFit(formula=~arma(4,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res653=residuals(model653, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res653, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res653^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model653@fit$ics[1])
BIC_list=append(BIC_list, model653@fit$ics[2])
u653=psnorm(res653, mean=0, sd=1, xi=tail(model653@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u653, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u653, null="punif")$p.value)

model654=garchFit(formula=~arma(4,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res654=residuals(model654, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res654, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res654^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model654@fit$ics[1])
BIC_list=append(BIC_list, model654@fit$ics[2])
u654=psnorm(res654, mean=0, sd=1, xi=tail(model654@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u654, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u654, null="punif")$p.value)

model655=garchFit(formula=~arma(4,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res655=residuals(model655, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res655, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res655^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model655@fit$ics[1])
BIC_list=append(BIC_list, model655@fit$ics[2])
u655=psnorm(res655, mean=0, sd=1, xi=tail(model655@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u655, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u655, null="punif")$p.value)

model656=garchFit(formula=~arma(4,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res656=residuals(model656, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res656, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res656^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model656@fit$ics[1])
BIC_list=append(BIC_list, model656@fit$ics[2])
u656=psnorm(res656, mean=0, sd=1, xi=tail(model656@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u656, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u656, null="punif")$p.value)

model657=garchFit(formula=~arma(4,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res657=residuals(model657, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res657, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res657^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model657@fit$ics[1])
BIC_list=append(BIC_list, model657@fit$ics[2])
u657=psnorm(res657, mean=0, sd=1, xi=tail(model657@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u657, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u657, null="punif")$p.value)

model658=garchFit(formula=~arma(4,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res658=residuals(model658, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res658, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res658^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model658@fit$ics[1])
BIC_list=append(BIC_list, model658@fit$ics[2])
u658=psnorm(res658, mean=0, sd=1, xi=tail(model658@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u658, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u658, null="punif")$p.value)

model659=garchFit(formula=~arma(4,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res659=residuals(model659, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res659, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res659^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model659@fit$ics[1])
BIC_list=append(BIC_list, model659@fit$ics[2])
u659=psnorm(res659, mean=0, sd=1, xi=tail(model659@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u659, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u659, null="punif")$p.value)

model660=garchFit(formula=~arma(4,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res660=residuals(model660, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res660, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res660^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model660@fit$ics[1])
BIC_list=append(BIC_list, model660@fit$ics[2])
u660=psnorm(res660, mean=0, sd=1, xi=tail(model660@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u660, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u660, null="punif")$p.value)

model661=garchFit(formula=~arma(4,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res661=residuals(model661, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res661, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res661^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model661@fit$ics[1])
BIC_list=append(BIC_list, model661@fit$ics[2])
u661=psnorm(res661, mean=0, sd=1, xi=tail(model661@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u661, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u661, null="punif")$p.value)

model662=garchFit(formula=~arma(4,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res662=residuals(model662, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res662, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res662^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model662@fit$ics[1])
BIC_list=append(BIC_list, model662@fit$ics[2])
u662=psnorm(res662, mean=0, sd=1, xi=tail(model662@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u662, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u662, null="punif")$p.value)

model663=garchFit(formula=~arma(4,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res663=residuals(model663, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res663, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res663^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model663@fit$ics[1])
BIC_list=append(BIC_list, model663@fit$ics[2])
u663=psnorm(res663, mean=0, sd=1, xi=tail(model663@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u663, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u663, null="punif")$p.value)

model664=garchFit(formula=~arma(4,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res664=residuals(model664, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res664, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res664^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model664@fit$ics[1])
BIC_list=append(BIC_list, model664@fit$ics[2])
u664=psnorm(res664, mean=0, sd=1, xi=tail(model664@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u664, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u664, null="punif")$p.value)

model665=garchFit(formula=~arma(4,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res665=residuals(model665, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res665, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res665^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model665@fit$ics[1])
BIC_list=append(BIC_list, model665@fit$ics[2])
u665=psnorm(res665, mean=0, sd=1, xi=tail(model665@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u665, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u665, null="punif")$p.value)

model666=garchFit(formula=~arma(4,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res666=residuals(model666, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res666, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res666^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model666@fit$ics[1])
BIC_list=append(BIC_list, model666@fit$ics[2])
u666=psnorm(res666, mean=0, sd=1, xi=tail(model666@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u666, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u666, null="punif")$p.value)

model667=garchFit(formula=~arma(4,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res667=residuals(model667, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res667, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res667^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model667@fit$ics[1])
BIC_list=append(BIC_list, model667@fit$ics[2])
u667=psnorm(res667, mean=0, sd=1, xi=tail(model667@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u667, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u667, null="punif")$p.value)

model668=garchFit(formula=~arma(4,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res668=residuals(model668, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res668, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res668^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model668@fit$ics[1])
BIC_list=append(BIC_list, model668@fit$ics[2])
u668=psnorm(res668, mean=0, sd=1, xi=tail(model668@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u668, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u668, null="punif")$p.value)

model669=garchFit(formula=~arma(4,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res669=residuals(model669, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res669, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res669^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model669@fit$ics[1])
BIC_list=append(BIC_list, model669@fit$ics[2])
u669=psnorm(res669, mean=0, sd=1, xi=tail(model669@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u669, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u669, null="punif")$p.value)

model670=garchFit(formula=~arma(4,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res670=residuals(model670, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res670, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res670^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model670@fit$ics[1])
BIC_list=append(BIC_list, model670@fit$ics[2])
u670=psnorm(res670, mean=0, sd=1, xi=tail(model670@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u670, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u670, null="punif")$p.value)

model671=garchFit(formula=~arma(4,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res671=residuals(model671, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res671, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res671^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model671@fit$ics[1])
BIC_list=append(BIC_list, model671@fit$ics[2])
u671=psnorm(res671, mean=0, sd=1, xi=tail(model671@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u671, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u671, null="punif")$p.value)

model672=garchFit(formula=~arma(4,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res672=residuals(model672, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res672, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res672^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model672@fit$ics[1])
BIC_list=append(BIC_list, model672@fit$ics[2])
u672=psnorm(res672, mean=0, sd=1, xi=tail(model672@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u672, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u672, null="punif")$p.value)

model673=garchFit(formula=~arma(4,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res673=residuals(model673, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res673, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res673^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model673@fit$ics[1])
BIC_list=append(BIC_list, model673@fit$ics[2])
u673=psnorm(res673, mean=0, sd=1, xi=tail(model673@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u673, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u673, null="punif")$p.value)

model674=garchFit(formula=~arma(4,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res674=residuals(model674, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res674, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res674^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model674@fit$ics[1])
BIC_list=append(BIC_list, model674@fit$ics[2])
u674=psnorm(res674, mean=0, sd=1, xi=tail(model674@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u674, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u674, null="punif")$p.value)

model675=garchFit(formula=~arma(4,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res675=residuals(model675, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res675, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res675^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model675@fit$ics[1])
BIC_list=append(BIC_list, model675@fit$ics[2])
u675=psnorm(res675, mean=0, sd=1, xi=tail(model675@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u675, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u675, null="punif")$p.value)

model676=garchFit(formula=~arma(4,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res676=residuals(model676, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res676, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res676^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model676@fit$ics[1])
BIC_list=append(BIC_list, model676@fit$ics[2])
u676=psnorm(res676, mean=0, sd=1, xi=tail(model676@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u676, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u676, null="punif")$p.value)

model677=garchFit(formula=~arma(4,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res677=residuals(model677, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res677, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res677^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model677@fit$ics[1])
BIC_list=append(BIC_list, model677@fit$ics[2])
u677=psnorm(res677, mean=0, sd=1, xi=tail(model677@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u677, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u677, null="punif")$p.value)

model678=garchFit(formula=~arma(4,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res678=residuals(model678, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res678, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res678^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model678@fit$ics[1])
BIC_list=append(BIC_list, model678@fit$ics[2])
u678=psnorm(res678, mean=0, sd=1, xi=tail(model678@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u678, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u678, null="punif")$p.value)

model679=garchFit(formula=~arma(4,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res679=residuals(model679, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res679, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res679^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model679@fit$ics[1])
BIC_list=append(BIC_list, model679@fit$ics[2])
u679=psnorm(res679, mean=0, sd=1, xi=tail(model679@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u679, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u679, null="punif")$p.value)

model680=garchFit(formula=~arma(4,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res680=residuals(model680, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res680, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res680^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model680@fit$ics[1])
BIC_list=append(BIC_list, model680@fit$ics[2])
u680=psnorm(res680, mean=0, sd=1, xi=tail(model680@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u680, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u680, null="punif")$p.value)

model681=garchFit(formula=~arma(4,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res681=residuals(model681, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res681, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res681^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model681@fit$ics[1])
BIC_list=append(BIC_list, model681@fit$ics[2])
u681=psnorm(res681, mean=0, sd=1, xi=tail(model681@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u681, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u681, null="punif")$p.value)

model682=garchFit(formula=~arma(4,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res682=residuals(model682, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res682, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res682^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model682@fit$ics[1])
BIC_list=append(BIC_list, model682@fit$ics[2])
u682=psnorm(res682, mean=0, sd=1, xi=tail(model682@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u682, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u682, null="punif")$p.value)

model683=garchFit(formula=~arma(4,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res683=residuals(model683, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res683, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res683^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model683@fit$ics[1])
BIC_list=append(BIC_list, model683@fit$ics[2])
u683=psnorm(res683, mean=0, sd=1, xi=tail(model683@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u683, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u683, null="punif")$p.value)

model684=garchFit(formula=~arma(4,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res684=residuals(model684, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res684, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res684^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model684@fit$ics[1])
BIC_list=append(BIC_list, model684@fit$ics[2])
u684=psnorm(res684, mean=0, sd=1, xi=tail(model684@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u684, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u684, null="punif")$p.value)

model685=garchFit(formula=~arma(7,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res685=residuals(model685, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res685, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res685^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model685@fit$ics[1])
BIC_list=append(BIC_list, model685@fit$ics[2])
u685=psnorm(res685, mean=0, sd=1, xi=tail(model685@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u685, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u685, null="punif")$p.value)

model686=garchFit(formula=~arma(7,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res686=residuals(model686, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res686, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res686^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model686@fit$ics[1])
BIC_list=append(BIC_list, model686@fit$ics[2])
u686=psnorm(res686, mean=0, sd=1, xi=tail(model686@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u686, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u686, null="punif")$p.value)

model687=garchFit(formula=~arma(7,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res687=residuals(model687, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res687, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res687^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model687@fit$ics[1])
BIC_list=append(BIC_list, model687@fit$ics[2])
u687=psnorm(res687, mean=0, sd=1, xi=tail(model687@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u687, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u687, null="punif")$p.value)

model688=garchFit(formula=~arma(7,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res688=residuals(model688, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res688, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res688^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model688@fit$ics[1])
BIC_list=append(BIC_list, model688@fit$ics[2])
u688=psnorm(res688, mean=0, sd=1, xi=tail(model688@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u688, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u688, null="punif")$p.value)

model689=garchFit(formula=~arma(7,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res689=residuals(model689, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res689, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res689^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model689@fit$ics[1])
BIC_list=append(BIC_list, model689@fit$ics[2])
u689=psnorm(res689, mean=0, sd=1, xi=tail(model689@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u689, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u689, null="punif")$p.value)

model690=garchFit(formula=~arma(7,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res690=residuals(model690, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res690, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res690^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model690@fit$ics[1])
BIC_list=append(BIC_list, model690@fit$ics[2])
u690=psnorm(res690, mean=0, sd=1, xi=tail(model690@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u690, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u690, null="punif")$p.value)

model691=garchFit(formula=~arma(7,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res691=residuals(model691, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res691, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res691^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model691@fit$ics[1])
BIC_list=append(BIC_list, model691@fit$ics[2])
u691=psnorm(res691, mean=0, sd=1, xi=tail(model691@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u691, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u691, null="punif")$p.value)

model692=garchFit(formula=~arma(7,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res692=residuals(model692, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res692, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res692^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model692@fit$ics[1])
BIC_list=append(BIC_list, model692@fit$ics[2])
u692=psnorm(res692, mean=0, sd=1, xi=tail(model692@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u692, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u692, null="punif")$p.value)

model693=garchFit(formula=~arma(7,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res693=residuals(model693, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res693, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res693^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model693@fit$ics[1])
BIC_list=append(BIC_list, model693@fit$ics[2])
u693=psnorm(res693, mean=0, sd=1, xi=tail(model693@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u693, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u693, null="punif")$p.value)

model694=garchFit(formula=~arma(7,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res694=residuals(model694, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res694, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res694^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model694@fit$ics[1])
BIC_list=append(BIC_list, model694@fit$ics[2])
u694=psnorm(res694, mean=0, sd=1, xi=tail(model694@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u694, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u694, null="punif")$p.value)

model695=garchFit(formula=~arma(7,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res695=residuals(model695, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res695, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res695^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model695@fit$ics[1])
BIC_list=append(BIC_list, model695@fit$ics[2])
u695=psnorm(res695, mean=0, sd=1, xi=tail(model695@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u695, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u695, null="punif")$p.value)

model696=garchFit(formula=~arma(7,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res696=residuals(model696, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res696, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res696^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model696@fit$ics[1])
BIC_list=append(BIC_list, model696@fit$ics[2])
u696=psnorm(res696, mean=0, sd=1, xi=tail(model696@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u696, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u696, null="punif")$p.value)

model697=garchFit(formula=~arma(7,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res697=residuals(model697, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res697, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res697^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model697@fit$ics[1])
BIC_list=append(BIC_list, model697@fit$ics[2])
u697=psnorm(res697, mean=0, sd=1, xi=tail(model697@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u697, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u697, null="punif")$p.value)

model698=garchFit(formula=~arma(7,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res698=residuals(model698, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res698, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res698^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model698@fit$ics[1])
BIC_list=append(BIC_list, model698@fit$ics[2])
u698=psnorm(res698, mean=0, sd=1, xi=tail(model698@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u698, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u698, null="punif")$p.value)

model699=garchFit(formula=~arma(7,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res699=residuals(model699, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res699, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res699^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model699@fit$ics[1])
BIC_list=append(BIC_list, model699@fit$ics[2])
u699=psnorm(res699, mean=0, sd=1, xi=tail(model699@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u699, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u699, null="punif")$p.value)

model700=garchFit(formula=~arma(7,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res700=residuals(model700, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res700, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res700^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model700@fit$ics[1])
BIC_list=append(BIC_list, model700@fit$ics[2])
u700=psnorm(res700, mean=0, sd=1, xi=tail(model700@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u700, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u700, null="punif")$p.value)

model701=garchFit(formula=~arma(7,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res701=residuals(model701, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res701, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res701^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model701@fit$ics[1])
BIC_list=append(BIC_list, model701@fit$ics[2])
u701=psnorm(res701, mean=0, sd=1, xi=tail(model701@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u701, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u701, null="punif")$p.value)

model702=garchFit(formula=~arma(7,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res702=residuals(model702, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res702, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res702^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model702@fit$ics[1])
BIC_list=append(BIC_list, model702@fit$ics[2])
u702=psnorm(res702, mean=0, sd=1, xi=tail(model702@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u702, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u702, null="punif")$p.value)

model703=garchFit(formula=~arma(7,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res703=residuals(model703, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res703, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res703^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model703@fit$ics[1])
BIC_list=append(BIC_list, model703@fit$ics[2])
u703=psnorm(res703, mean=0, sd=1, xi=tail(model703@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u703, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u703, null="punif")$p.value)

model704=garchFit(formula=~arma(7,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res704=residuals(model704, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res704, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res704^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model704@fit$ics[1])
BIC_list=append(BIC_list, model704@fit$ics[2])
u704=psnorm(res704, mean=0, sd=1, xi=tail(model704@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u704, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u704, null="punif")$p.value)

model705=garchFit(formula=~arma(7,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res705=residuals(model705, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res705, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res705^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model705@fit$ics[1])
BIC_list=append(BIC_list, model705@fit$ics[2])
u705=psnorm(res705, mean=0, sd=1, xi=tail(model705@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u705, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u705, null="punif")$p.value)

model706=garchFit(formula=~arma(7,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res706=residuals(model706, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res706, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res706^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model706@fit$ics[1])
BIC_list=append(BIC_list, model706@fit$ics[2])
u706=psnorm(res706, mean=0, sd=1, xi=tail(model706@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u706, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u706, null="punif")$p.value)

model707=garchFit(formula=~arma(7,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res707=residuals(model707, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res707, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res707^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model707@fit$ics[1])
BIC_list=append(BIC_list, model707@fit$ics[2])
u707=psnorm(res707, mean=0, sd=1, xi=tail(model707@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u707, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u707, null="punif")$p.value)

model708=garchFit(formula=~arma(7,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res708=residuals(model708, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res708, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res708^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model708@fit$ics[1])
BIC_list=append(BIC_list, model708@fit$ics[2])
u708=psnorm(res708, mean=0, sd=1, xi=tail(model708@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u708, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u708, null="punif")$p.value)

model709=garchFit(formula=~arma(7,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res709=residuals(model709, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res709, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res709^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model709@fit$ics[1])
BIC_list=append(BIC_list, model709@fit$ics[2])
u709=psnorm(res709, mean=0, sd=1, xi=tail(model709@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u709, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u709, null="punif")$p.value)

model710=garchFit(formula=~arma(7,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res710=residuals(model710, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res710, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res710^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model710@fit$ics[1])
BIC_list=append(BIC_list, model710@fit$ics[2])
u710=psnorm(res710, mean=0, sd=1, xi=tail(model710@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u710, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u710, null="punif")$p.value)

model711=garchFit(formula=~arma(7,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res711=residuals(model711, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res711, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res711^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model711@fit$ics[1])
BIC_list=append(BIC_list, model711@fit$ics[2])
u711=psnorm(res711, mean=0, sd=1, xi=tail(model711@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u711, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u711, null="punif")$p.value)

model712=garchFit(formula=~arma(7,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="snorm")
res712=residuals(model712, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res712, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res712^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model712@fit$ics[1])
BIC_list=append(BIC_list, model712@fit$ics[2])
u712=psnorm(res712, mean=0, sd=1, xi=tail(model712@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u712, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u712, null="punif")$p.value)

model713=garchFit(formula=~arma(7,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="snorm")
res713=residuals(model713, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res713, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res713^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model713@fit$ics[1])
BIC_list=append(BIC_list, model713@fit$ics[2])
u713=psnorm(res713, mean=0, sd=1, xi=tail(model713@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u713, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u713, null="punif")$p.value)

model714=garchFit(formula=~arma(7,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="snorm")
res714=residuals(model714, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res714, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res714^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model714@fit$ics[1])
BIC_list=append(BIC_list, model714@fit$ics[2])
u714=psnorm(res714, mean=0, sd=1, xi=tail(model714@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u714, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u714, null="punif")$p.value)

model715=garchFit(formula=~arma(7,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="snorm")
res715=residuals(model715, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res715, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res715^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model715@fit$ics[1])
BIC_list=append(BIC_list, model715@fit$ics[2])
u715=psnorm(res715, mean=0, sd=1, xi=tail(model715@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u715, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u715, null="punif")$p.value)

model716=garchFit(formula=~arma(7,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="snorm")
res716=residuals(model716, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res716, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res716^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model716@fit$ics[1])
BIC_list=append(BIC_list, model716@fit$ics[2])
u716=psnorm(res716, mean=0, sd=1, xi=tail(model716@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u716, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u716, null="punif")$p.value)

model717=garchFit(formula=~arma(7,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="snorm")
res717=residuals(model717, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res717, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res717^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model717@fit$ics[1])
BIC_list=append(BIC_list, model717@fit$ics[2])
u717=psnorm(res717, mean=0, sd=1, xi=tail(model717@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u717, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u717, null="punif")$p.value)

model718=garchFit(formula=~arma(7,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="snorm")
res718=residuals(model718, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res718, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res718^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model718@fit$ics[1])
BIC_list=append(BIC_list, model718@fit$ics[2])
u718=psnorm(res718, mean=0, sd=1, xi=tail(model718@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u718, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u718, null="punif")$p.value)

model719=garchFit(formula=~arma(7,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="snorm")
res719=residuals(model719, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res719, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res719^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model719@fit$ics[1])
BIC_list=append(BIC_list, model719@fit$ics[2])
u719=psnorm(res719, mean=0, sd=1, xi=tail(model719@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u719, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u719, null="punif")$p.value)

model720=garchFit(formula=~arma(7,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="snorm")
res720=residuals(model720, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res720, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res720^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model720@fit$ics[1])
BIC_list=append(BIC_list, model720@fit$ics[2])
u720=psnorm(res720, mean=0, sd=1, xi=tail(model720@fit$coef, n=1))[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u720, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u720, null="punif")$p.value)

model721=garchFit(formula=~arma(1,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res721=residuals(model721, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res721, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res721^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model721@fit$ics[1])
BIC_list=append(BIC_list, model721@fit$ics[2])
u721=psged(res721, mean=0, sd=1, nu=tail(model721@fit$coef, n=1), xi=model721@fit$coef[length(model721@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u721, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u721, null="punif")$p.value)

model722=garchFit(formula=~arma(1,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res722=residuals(model722, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res722, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res722^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model722@fit$ics[1])
BIC_list=append(BIC_list, model722@fit$ics[2])
u722=psged(res722, mean=0, sd=1, nu=tail(model722@fit$coef, n=1), xi=model722@fit$coef[length(model722@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u722, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u722, null="punif")$p.value)

model723=garchFit(formula=~arma(1,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res723=residuals(model723, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res723, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res723^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model723@fit$ics[1])
BIC_list=append(BIC_list, model723@fit$ics[2])
u723=psged(res723, mean=0, sd=1, nu=tail(model723@fit$coef, n=1), xi=model723@fit$coef[length(model723@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u723, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u723, null="punif")$p.value)

model724=garchFit(formula=~arma(1,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res724=residuals(model724, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res724, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res724^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model724@fit$ics[1])
BIC_list=append(BIC_list, model724@fit$ics[2])
u724=psged(res724, mean=0, sd=1, nu=tail(model724@fit$coef, n=1), xi=model724@fit$coef[length(model724@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u724, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u724, null="punif")$p.value)

model725=garchFit(formula=~arma(1,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res725=residuals(model725, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res725, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res725^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model725@fit$ics[1])
BIC_list=append(BIC_list, model725@fit$ics[2])
u725=psged(res725, mean=0, sd=1, nu=tail(model725@fit$coef, n=1), xi=model725@fit$coef[length(model725@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u725, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u725, null="punif")$p.value)

model726=garchFit(formula=~arma(1,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res726=residuals(model726, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res726, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res726^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model726@fit$ics[1])
BIC_list=append(BIC_list, model726@fit$ics[2])
u726=psged(res726, mean=0, sd=1, nu=tail(model726@fit$coef, n=1), xi=model726@fit$coef[length(model726@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u726, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u726, null="punif")$p.value)

model727=garchFit(formula=~arma(1,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res727=residuals(model727, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res727, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res727^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model727@fit$ics[1])
BIC_list=append(BIC_list, model727@fit$ics[2])
u727=psged(res727, mean=0, sd=1, nu=tail(model727@fit$coef, n=1), xi=model727@fit$coef[length(model727@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u727, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u727, null="punif")$p.value)

model728=garchFit(formula=~arma(1,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res728=residuals(model728, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res728, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res728^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model728@fit$ics[1])
BIC_list=append(BIC_list, model728@fit$ics[2])
u728=psged(res728, mean=0, sd=1, nu=tail(model728@fit$coef, n=1), xi=model728@fit$coef[length(model728@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u728, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u728, null="punif")$p.value)

model729=garchFit(formula=~arma(1,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res729=residuals(model729, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res729, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
boxTest2=append(boxTest2, Box.test(res729^2, lag=10, type=c("Ljung-Box"), fitdf=1)$p.value)
AIC_list=append(AIC_list, model729@fit$ics[1])
BIC_list=append(BIC_list, model729@fit$ics[2])
u729=psged(res729, mean=0, sd=1, nu=tail(model729@fit$coef, n=1), xi=model729@fit$coef[length(model729@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u729, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u729, null="punif")$p.value)

model730=garchFit(formula=~arma(1,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res730=residuals(model730, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res730, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res730^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model730@fit$ics[1])
BIC_list=append(BIC_list, model730@fit$ics[2])
u730=psged(res730, mean=0, sd=1, nu=tail(model730@fit$coef, n=1), xi=model730@fit$coef[length(model730@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u730, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u730, null="punif")$p.value)

model731=garchFit(formula=~arma(1,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res731=residuals(model731, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res731, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res731^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model731@fit$ics[1])
BIC_list=append(BIC_list, model731@fit$ics[2])
u731=psged(res731, mean=0, sd=1, nu=tail(model731@fit$coef, n=1), xi=model731@fit$coef[length(model731@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u731, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u731, null="punif")$p.value)

model732=garchFit(formula=~arma(1,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res732=residuals(model732, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res732, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res732^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model732@fit$ics[1])
BIC_list=append(BIC_list, model732@fit$ics[2])
u732=psged(res732, mean=0, sd=1, nu=tail(model732@fit$coef, n=1), xi=model732@fit$coef[length(model732@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u732, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u732, null="punif")$p.value)

model733=garchFit(formula=~arma(1,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res733=residuals(model733, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res733, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res733^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model733@fit$ics[1])
BIC_list=append(BIC_list, model733@fit$ics[2])
u733=psged(res733, mean=0, sd=1, nu=tail(model733@fit$coef, n=1), xi=model733@fit$coef[length(model733@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u733, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u733, null="punif")$p.value)

model734=garchFit(formula=~arma(1,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res734=residuals(model734, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res734, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res734^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model734@fit$ics[1])
BIC_list=append(BIC_list, model734@fit$ics[2])
u734=psged(res734, mean=0, sd=1, nu=tail(model734@fit$coef, n=1), xi=model734@fit$coef[length(model734@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u734, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u734, null="punif")$p.value)

model735=garchFit(formula=~arma(1,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res735=residuals(model735, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res735, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res735^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model735@fit$ics[1])
BIC_list=append(BIC_list, model735@fit$ics[2])
u735=psged(res735, mean=0, sd=1, nu=tail(model735@fit$coef, n=1), xi=model735@fit$coef[length(model735@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u735, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u735, null="punif")$p.value)

model736=garchFit(formula=~arma(1,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res736=residuals(model736, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res736, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res736^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model736@fit$ics[1])
BIC_list=append(BIC_list, model736@fit$ics[2])
u736=psged(res736, mean=0, sd=1, nu=tail(model736@fit$coef, n=1), xi=model736@fit$coef[length(model736@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u736, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u736, null="punif")$p.value)

model737=garchFit(formula=~arma(1,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res737=residuals(model737, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res737, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res737^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model737@fit$ics[1])
BIC_list=append(BIC_list, model737@fit$ics[2])
u737=psged(res737, mean=0, sd=1, nu=tail(model737@fit$coef, n=1), xi=model737@fit$coef[length(model737@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u737, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u737, null="punif")$p.value)

model738=garchFit(formula=~arma(1,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res738=residuals(model738, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res738, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
boxTest2=append(boxTest2, Box.test(res738^2, lag=10, type=c("Ljung-Box"), fitdf=2)$p.value)
AIC_list=append(AIC_list, model738@fit$ics[1])
BIC_list=append(BIC_list, model738@fit$ics[2])
u738=psged(res738, mean=0, sd=1, nu=tail(model738@fit$coef, n=1), xi=model738@fit$coef[length(model738@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u738, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u738, null="punif")$p.value)

model739=garchFit(formula=~arma(1,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res739=residuals(model739, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res739, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res739^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model739@fit$ics[1])
BIC_list=append(BIC_list, model739@fit$ics[2])
u739=psged(res739, mean=0, sd=1, nu=tail(model739@fit$coef, n=1), xi=model739@fit$coef[length(model739@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u739, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u739, null="punif")$p.value)

model740=garchFit(formula=~arma(1,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res740=residuals(model740, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res740, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res740^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model740@fit$ics[1])
BIC_list=append(BIC_list, model740@fit$ics[2])
u740=psged(res740, mean=0, sd=1, nu=tail(model740@fit$coef, n=1), xi=model740@fit$coef[length(model740@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u740, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u740, null="punif")$p.value)

model741=garchFit(formula=~arma(1,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res741=residuals(model741, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res741, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res741^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model741@fit$ics[1])
BIC_list=append(BIC_list, model741@fit$ics[2])
u741=psged(res741, mean=0, sd=1, nu=tail(model741@fit$coef, n=1), xi=model741@fit$coef[length(model741@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u741, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u741, null="punif")$p.value)

model742=garchFit(formula=~arma(1,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res742=residuals(model742, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res742, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res742^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model742@fit$ics[1])
BIC_list=append(BIC_list, model742@fit$ics[2])
u742=psged(res742, mean=0, sd=1, nu=tail(model742@fit$coef, n=1), xi=model742@fit$coef[length(model742@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u742, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u742, null="punif")$p.value)

model743=garchFit(formula=~arma(1,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res743=residuals(model743, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res743, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res743^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model743@fit$ics[1])
BIC_list=append(BIC_list, model743@fit$ics[2])
u743=psged(res743, mean=0, sd=1, nu=tail(model743@fit$coef, n=1), xi=model743@fit$coef[length(model743@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u743, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u743, null="punif")$p.value)

model744=garchFit(formula=~arma(1,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res744=residuals(model744, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res744, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res744^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model744@fit$ics[1])
BIC_list=append(BIC_list, model744@fit$ics[2])
u744=psged(res744, mean=0, sd=1, nu=tail(model744@fit$coef, n=1), xi=model744@fit$coef[length(model744@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u744, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u744, null="punif")$p.value)

model745=garchFit(formula=~arma(1,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res745=residuals(model745, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res745, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res745^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model745@fit$ics[1])
BIC_list=append(BIC_list, model745@fit$ics[2])
u745=psged(res745, mean=0, sd=1, nu=tail(model745@fit$coef, n=1), xi=model745@fit$coef[length(model745@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u745, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u745, null="punif")$p.value)

model746=garchFit(formula=~arma(1,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res746=residuals(model746, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res746, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res746^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model746@fit$ics[1])
BIC_list=append(BIC_list, model746@fit$ics[2])
u746=psged(res746, mean=0, sd=1, nu=tail(model746@fit$coef, n=1), xi=model746@fit$coef[length(model746@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u746, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u746, null="punif")$p.value)

model747=garchFit(formula=~arma(1,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res747=residuals(model747, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res747, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res747^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model747@fit$ics[1])
BIC_list=append(BIC_list, model747@fit$ics[2])
u747=psged(res747, mean=0, sd=1, nu=tail(model747@fit$coef, n=1), xi=model747@fit$coef[length(model747@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u747, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u747, null="punif")$p.value)

model748=garchFit(formula=~arma(1,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res748=residuals(model748, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res748, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res748^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model748@fit$ics[1])
BIC_list=append(BIC_list, model748@fit$ics[2])
u748=psged(res748, mean=0, sd=1, nu=tail(model748@fit$coef, n=1), xi=model748@fit$coef[length(model748@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u748, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u748, null="punif")$p.value)

model749=garchFit(formula=~arma(1,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res749=residuals(model749, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res749, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res749^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model749@fit$ics[1])
BIC_list=append(BIC_list, model749@fit$ics[2])
u749=psged(res749, mean=0, sd=1, nu=tail(model749@fit$coef, n=1), xi=model749@fit$coef[length(model749@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u749, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u749, null="punif")$p.value)

model750=garchFit(formula=~arma(1,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res750=residuals(model750, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res750, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res750^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model750@fit$ics[1])
BIC_list=append(BIC_list, model750@fit$ics[2])
u750=psged(res750, mean=0, sd=1, nu=tail(model750@fit$coef, n=1), xi=model750@fit$coef[length(model750@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u750, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u750, null="punif")$p.value)

model751=garchFit(formula=~arma(1,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res751=residuals(model751, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res751, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res751^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model751@fit$ics[1])
BIC_list=append(BIC_list, model751@fit$ics[2])
u751=psged(res751, mean=0, sd=1, nu=tail(model751@fit$coef, n=1), xi=model751@fit$coef[length(model751@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u751, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u751, null="punif")$p.value)

model752=garchFit(formula=~arma(1,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res752=residuals(model752, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res752, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res752^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model752@fit$ics[1])
BIC_list=append(BIC_list, model752@fit$ics[2])
u752=psged(res752, mean=0, sd=1, nu=tail(model752@fit$coef, n=1), xi=model752@fit$coef[length(model752@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u752, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u752, null="punif")$p.value)

model753=garchFit(formula=~arma(1,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res753=residuals(model753, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res753, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res753^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model753@fit$ics[1])
BIC_list=append(BIC_list, model753@fit$ics[2])
u753=psged(res753, mean=0, sd=1, nu=tail(model753@fit$coef, n=1), xi=model753@fit$coef[length(model753@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u753, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u753, null="punif")$p.value)

model754=garchFit(formula=~arma(1,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res754=residuals(model754, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res754, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res754^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model754@fit$ics[1])
BIC_list=append(BIC_list, model754@fit$ics[2])
u754=psged(res754, mean=0, sd=1, nu=tail(model754@fit$coef, n=1), xi=model754@fit$coef[length(model754@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u754, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u754, null="punif")$p.value)

model755=garchFit(formula=~arma(1,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res755=residuals(model755, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res755, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res755^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model755@fit$ics[1])
BIC_list=append(BIC_list, model755@fit$ics[2])
u755=psged(res755, mean=0, sd=1, nu=tail(model755@fit$coef, n=1), xi=model755@fit$coef[length(model755@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u755, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u755, null="punif")$p.value)

model756=garchFit(formula=~arma(1,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res756=residuals(model756, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res756, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res756^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model756@fit$ics[1])
BIC_list=append(BIC_list, model756@fit$ics[2])
u756=psged(res756, mean=0, sd=1, nu=tail(model756@fit$coef, n=1), xi=model756@fit$coef[length(model756@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u756, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u756, null="punif")$p.value)

model757=garchFit(formula=~arma(3,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res757=residuals(model757, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res757, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res757^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model757@fit$ics[1])
BIC_list=append(BIC_list, model757@fit$ics[2])
u757=psged(res757, mean=0, sd=1, nu=tail(model757@fit$coef, n=1), xi=model757@fit$coef[length(model757@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u757, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u757, null="punif")$p.value)

model758=garchFit(formula=~arma(3,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res758=residuals(model758, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res758, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res758^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model758@fit$ics[1])
BIC_list=append(BIC_list, model758@fit$ics[2])
u758=psged(res758, mean=0, sd=1, nu=tail(model758@fit$coef, n=1), xi=model758@fit$coef[length(model758@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u758, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u758, null="punif")$p.value)

model759=garchFit(formula=~arma(3,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res759=residuals(model759, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res759, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res759^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model759@fit$ics[1])
BIC_list=append(BIC_list, model759@fit$ics[2])
u759=psged(res759, mean=0, sd=1, nu=tail(model759@fit$coef, n=1), xi=model759@fit$coef[length(model759@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u759, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u759, null="punif")$p.value)

model760=garchFit(formula=~arma(3,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res760=residuals(model760, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res760, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res760^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model760@fit$ics[1])
BIC_list=append(BIC_list, model760@fit$ics[2])
u760=psged(res760, mean=0, sd=1, nu=tail(model760@fit$coef, n=1), xi=model760@fit$coef[length(model760@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u760, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u760, null="punif")$p.value)

model761=garchFit(formula=~arma(3,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res761=residuals(model761, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res761, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res761^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model761@fit$ics[1])
BIC_list=append(BIC_list, model761@fit$ics[2])
u761=psged(res761, mean=0, sd=1, nu=tail(model761@fit$coef, n=1), xi=model761@fit$coef[length(model761@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u761, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u761, null="punif")$p.value)

model762=garchFit(formula=~arma(3,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res762=residuals(model762, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res762, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res762^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model762@fit$ics[1])
BIC_list=append(BIC_list, model762@fit$ics[2])
u762=psged(res762, mean=0, sd=1, nu=tail(model762@fit$coef, n=1), xi=model762@fit$coef[length(model762@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u762, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u762, null="punif")$p.value)

model763=garchFit(formula=~arma(3,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res763=residuals(model763, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res763, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res763^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model763@fit$ics[1])
BIC_list=append(BIC_list, model763@fit$ics[2])
u763=psged(res763, mean=0, sd=1, nu=tail(model763@fit$coef, n=1), xi=model763@fit$coef[length(model763@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u763, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u763, null="punif")$p.value)

model764=garchFit(formula=~arma(3,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res764=residuals(model764, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res764, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res764^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model764@fit$ics[1])
BIC_list=append(BIC_list, model764@fit$ics[2])
u764=psged(res764, mean=0, sd=1, nu=tail(model764@fit$coef, n=1), xi=model764@fit$coef[length(model764@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u764, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u764, null="punif")$p.value)

model765=garchFit(formula=~arma(3,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res765=residuals(model765, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res765, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
boxTest2=append(boxTest2, Box.test(res765^2, lag=10, type=c("Ljung-Box"), fitdf=3)$p.value)
AIC_list=append(AIC_list, model765@fit$ics[1])
BIC_list=append(BIC_list, model765@fit$ics[2])
u765=psged(res765, mean=0, sd=1, nu=tail(model765@fit$coef, n=1), xi=model765@fit$coef[length(model765@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u765, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u765, null="punif")$p.value)

model766=garchFit(formula=~arma(3,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res766=residuals(model766, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res766, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res766^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model766@fit$ics[1])
BIC_list=append(BIC_list, model766@fit$ics[2])
u766=psged(res766, mean=0, sd=1, nu=tail(model766@fit$coef, n=1), xi=model766@fit$coef[length(model766@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u766, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u766, null="punif")$p.value)

model767=garchFit(formula=~arma(3,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res767=residuals(model767, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res767, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res767^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model767@fit$ics[1])
BIC_list=append(BIC_list, model767@fit$ics[2])
u767=psged(res767, mean=0, sd=1, nu=tail(model767@fit$coef, n=1), xi=model767@fit$coef[length(model767@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u767, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u767, null="punif")$p.value)

model768=garchFit(formula=~arma(3,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res768=residuals(model768, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res768, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res768^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model768@fit$ics[1])
BIC_list=append(BIC_list, model768@fit$ics[2])
u768=psged(res768, mean=0, sd=1, nu=tail(model768@fit$coef, n=1), xi=model768@fit$coef[length(model768@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u768, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u768, null="punif")$p.value)

model769=garchFit(formula=~arma(3,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res769=residuals(model769, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res769, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res769^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model769@fit$ics[1])
BIC_list=append(BIC_list, model769@fit$ics[2])
u769=psged(res769, mean=0, sd=1, nu=tail(model769@fit$coef, n=1), xi=model769@fit$coef[length(model769@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u769, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u769, null="punif")$p.value)

model770=garchFit(formula=~arma(3,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res770=residuals(model770, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res770, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res770^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model770@fit$ics[1])
BIC_list=append(BIC_list, model770@fit$ics[2])
u770=psged(res770, mean=0, sd=1, nu=tail(model770@fit$coef, n=1), xi=model770@fit$coef[length(model770@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u770, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u770, null="punif")$p.value)

model771=garchFit(formula=~arma(3,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res771=residuals(model771, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res771, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res771^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model771@fit$ics[1])
BIC_list=append(BIC_list, model771@fit$ics[2])
u771=psged(res771, mean=0, sd=1, nu=tail(model771@fit$coef, n=1), xi=model771@fit$coef[length(model771@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u771, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u771, null="punif")$p.value)

model772=garchFit(formula=~arma(3,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res772=residuals(model772, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res772, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res772^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model772@fit$ics[1])
BIC_list=append(BIC_list, model772@fit$ics[2])
u772=psged(res772, mean=0, sd=1, nu=tail(model772@fit$coef, n=1), xi=model772@fit$coef[length(model772@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u772, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u772, null="punif")$p.value)

model773=garchFit(formula=~arma(3,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res773=residuals(model773, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res773, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res773^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model773@fit$ics[1])
BIC_list=append(BIC_list, model773@fit$ics[2])
u773=psged(res773, mean=0, sd=1, nu=tail(model773@fit$coef, n=1), xi=model773@fit$coef[length(model773@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u773, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u773, null="punif")$p.value)

model774=garchFit(formula=~arma(3,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res774=residuals(model774, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res774, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res774^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model774@fit$ics[1])
BIC_list=append(BIC_list, model774@fit$ics[2])
u774=psged(res774, mean=0, sd=1, nu=tail(model774@fit$coef, n=1), xi=model774@fit$coef[length(model774@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u774, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u774, null="punif")$p.value)

model775=garchFit(formula=~arma(3,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res775=residuals(model775, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res775, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res775^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model775@fit$ics[1])
BIC_list=append(BIC_list, model775@fit$ics[2])
u775=psged(res775, mean=0, sd=1, nu=tail(model775@fit$coef, n=1), xi=model775@fit$coef[length(model775@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u775, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u775, null="punif")$p.value)

model776=garchFit(formula=~arma(3,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res776=residuals(model776, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res776, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res776^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model776@fit$ics[1])
BIC_list=append(BIC_list, model776@fit$ics[2])
u776=psged(res776, mean=0, sd=1, nu=tail(model776@fit$coef, n=1), xi=model776@fit$coef[length(model776@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u776, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u776, null="punif")$p.value)

model777=garchFit(formula=~arma(3,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res777=residuals(model777, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res777, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res777^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model777@fit$ics[1])
BIC_list=append(BIC_list, model777@fit$ics[2])
u777=psged(res777, mean=0, sd=1, nu=tail(model777@fit$coef, n=1), xi=model777@fit$coef[length(model777@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u777, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u777, null="punif")$p.value)

model778=garchFit(formula=~arma(3,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res778=residuals(model778, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res778, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res778^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model778@fit$ics[1])
BIC_list=append(BIC_list, model778@fit$ics[2])
u778=psged(res778, mean=0, sd=1, nu=tail(model778@fit$coef, n=1), xi=model778@fit$coef[length(model778@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u778, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u778, null="punif")$p.value)

model779=garchFit(formula=~arma(3,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res779=residuals(model779, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res779, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res779^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model779@fit$ics[1])
BIC_list=append(BIC_list, model779@fit$ics[2])
u779=psged(res779, mean=0, sd=1, nu=tail(model779@fit$coef, n=1), xi=model779@fit$coef[length(model779@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u779, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u779, null="punif")$p.value)

model780=garchFit(formula=~arma(3,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res780=residuals(model780, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res780, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res780^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model780@fit$ics[1])
BIC_list=append(BIC_list, model780@fit$ics[2])
u780=psged(res780, mean=0, sd=1, nu=tail(model780@fit$coef, n=1), xi=model780@fit$coef[length(model780@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u780, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u780, null="punif")$p.value)

model781=garchFit(formula=~arma(3,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res781=residuals(model781, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res781, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res781^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model781@fit$ics[1])
BIC_list=append(BIC_list, model781@fit$ics[2])
u781=psged(res781, mean=0, sd=1, nu=tail(model781@fit$coef, n=1), xi=model781@fit$coef[length(model781@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u781, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u781, null="punif")$p.value)

model782=garchFit(formula=~arma(3,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res782=residuals(model782, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res782, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res782^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model782@fit$ics[1])
BIC_list=append(BIC_list, model782@fit$ics[2])
u782=psged(res782, mean=0, sd=1, nu=tail(model782@fit$coef, n=1), xi=model782@fit$coef[length(model782@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u782, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u782, null="punif")$p.value)

model783=garchFit(formula=~arma(3,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res783=residuals(model783, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res783, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res783^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model783@fit$ics[1])
BIC_list=append(BIC_list, model783@fit$ics[2])
u783=psged(res783, mean=0, sd=1, nu=tail(model783@fit$coef, n=1), xi=model783@fit$coef[length(model783@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u783, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u783, null="punif")$p.value)

model784=garchFit(formula=~arma(3,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res784=residuals(model784, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res784, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res784^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model784@fit$ics[1])
BIC_list=append(BIC_list, model784@fit$ics[2])
u784=psged(res784, mean=0, sd=1, nu=tail(model784@fit$coef, n=1), xi=model784@fit$coef[length(model784@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u784, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u784, null="punif")$p.value)

model785=garchFit(formula=~arma(3,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res785=residuals(model785, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res785, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res785^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model785@fit$ics[1])
BIC_list=append(BIC_list, model785@fit$ics[2])
u785=psged(res785, mean=0, sd=1, nu=tail(model785@fit$coef, n=1), xi=model785@fit$coef[length(model785@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u785, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u785, null="punif")$p.value)

model786=garchFit(formula=~arma(3,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res786=residuals(model786, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res786, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res786^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model786@fit$ics[1])
BIC_list=append(BIC_list, model786@fit$ics[2])
u786=psged(res786, mean=0, sd=1, nu=tail(model786@fit$coef, n=1), xi=model786@fit$coef[length(model786@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u786, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u786, null="punif")$p.value)

model787=garchFit(formula=~arma(3,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res787=residuals(model787, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res787, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res787^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model787@fit$ics[1])
BIC_list=append(BIC_list, model787@fit$ics[2])
u787=psged(res787, mean=0, sd=1, nu=tail(model787@fit$coef, n=1), xi=model787@fit$coef[length(model787@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u787, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u787, null="punif")$p.value)

model788=garchFit(formula=~arma(3,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res788=residuals(model788, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res788, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res788^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model788@fit$ics[1])
BIC_list=append(BIC_list, model788@fit$ics[2])
u788=psged(res788, mean=0, sd=1, nu=tail(model788@fit$coef, n=1), xi=model788@fit$coef[length(model788@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u788, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u788, null="punif")$p.value)

model789=garchFit(formula=~arma(3,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res789=residuals(model789, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res789, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res789^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model789@fit$ics[1])
BIC_list=append(BIC_list, model789@fit$ics[2])
u789=psged(res789, mean=0, sd=1, nu=tail(model789@fit$coef, n=1), xi=model789@fit$coef[length(model789@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u789, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u789, null="punif")$p.value)

model790=garchFit(formula=~arma(3,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res790=residuals(model790, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res790, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res790^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model790@fit$ics[1])
BIC_list=append(BIC_list, model790@fit$ics[2])
u790=psged(res790, mean=0, sd=1, nu=tail(model790@fit$coef, n=1), xi=model790@fit$coef[length(model790@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u790, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u790, null="punif")$p.value)

model791=garchFit(formula=~arma(3,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res791=residuals(model791, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res791, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res791^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model791@fit$ics[1])
BIC_list=append(BIC_list, model791@fit$ics[2])
u791=psged(res791, mean=0, sd=1, nu=tail(model791@fit$coef, n=1), xi=model791@fit$coef[length(model791@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u791, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u791, null="punif")$p.value)

model792=garchFit(formula=~arma(3,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res792=residuals(model792, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res792, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res792^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model792@fit$ics[1])
BIC_list=append(BIC_list, model792@fit$ics[2])
u792=psged(res792, mean=0, sd=1, nu=tail(model792@fit$coef, n=1), xi=model792@fit$coef[length(model792@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u792, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u792, null="punif")$p.value)

model793=garchFit(formula=~arma(4,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res793=residuals(model793, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res793, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res793^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model793@fit$ics[1])
BIC_list=append(BIC_list, model793@fit$ics[2])
u793=psged(res793, mean=0, sd=1, nu=tail(model793@fit$coef, n=1), xi=model793@fit$coef[length(model793@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u793, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u793, null="punif")$p.value)

model794=garchFit(formula=~arma(4,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res794=residuals(model794, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res794, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res794^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model794@fit$ics[1])
BIC_list=append(BIC_list, model794@fit$ics[2])
u794=psged(res794, mean=0, sd=1, nu=tail(model794@fit$coef, n=1), xi=model794@fit$coef[length(model794@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u794, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u794, null="punif")$p.value)

model795=garchFit(formula=~arma(4,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res795=residuals(model795, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res795, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res795^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model795@fit$ics[1])
BIC_list=append(BIC_list, model795@fit$ics[2])
u795=psged(res795, mean=0, sd=1, nu=tail(model795@fit$coef, n=1), xi=model795@fit$coef[length(model795@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u795, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u795, null="punif")$p.value)

model796=garchFit(formula=~arma(4,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res796=residuals(model796, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res796, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res796^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model796@fit$ics[1])
BIC_list=append(BIC_list, model796@fit$ics[2])
u796=psged(res796, mean=0, sd=1, nu=tail(model796@fit$coef, n=1), xi=model796@fit$coef[length(model796@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u796, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u796, null="punif")$p.value)

model797=garchFit(formula=~arma(4,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res797=residuals(model797, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res797, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res797^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model797@fit$ics[1])
BIC_list=append(BIC_list, model797@fit$ics[2])
u797=psged(res797, mean=0, sd=1, nu=tail(model797@fit$coef, n=1), xi=model797@fit$coef[length(model797@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u797, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u797, null="punif")$p.value)

model798=garchFit(formula=~arma(4,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res798=residuals(model798, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res798, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res798^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model798@fit$ics[1])
BIC_list=append(BIC_list, model798@fit$ics[2])
u798=psged(res798, mean=0, sd=1, nu=tail(model798@fit$coef, n=1), xi=model798@fit$coef[length(model798@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u798, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u798, null="punif")$p.value)

model799=garchFit(formula=~arma(4,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res799=residuals(model799, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res799, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res799^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model799@fit$ics[1])
BIC_list=append(BIC_list, model799@fit$ics[2])
u799=psged(res799, mean=0, sd=1, nu=tail(model799@fit$coef, n=1), xi=model799@fit$coef[length(model799@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u799, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u799, null="punif")$p.value)

model800=garchFit(formula=~arma(4,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res800=residuals(model800, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res800, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res800^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model800@fit$ics[1])
BIC_list=append(BIC_list, model800@fit$ics[2])
u800=psged(res800, mean=0, sd=1, nu=tail(model800@fit$coef, n=1), xi=model800@fit$coef[length(model800@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u800, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u800, null="punif")$p.value)

model801=garchFit(formula=~arma(4,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res801=residuals(model801, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res801, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
boxTest2=append(boxTest2, Box.test(res801^2, lag=10, type=c("Ljung-Box"), fitdf=4)$p.value)
AIC_list=append(AIC_list, model801@fit$ics[1])
BIC_list=append(BIC_list, model801@fit$ics[2])
u801=psged(res801, mean=0, sd=1, nu=tail(model801@fit$coef, n=1), xi=model801@fit$coef[length(model801@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u801, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u801, null="punif")$p.value)

model802=garchFit(formula=~arma(4,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res802=residuals(model802, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res802, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res802^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model802@fit$ics[1])
BIC_list=append(BIC_list, model802@fit$ics[2])
u802=psged(res802, mean=0, sd=1, nu=tail(model802@fit$coef, n=1), xi=model802@fit$coef[length(model802@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u802, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u802, null="punif")$p.value)

model803=garchFit(formula=~arma(4,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res803=residuals(model803, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res803, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res803^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model803@fit$ics[1])
BIC_list=append(BIC_list, model803@fit$ics[2])
u803=psged(res803, mean=0, sd=1, nu=tail(model803@fit$coef, n=1), xi=model803@fit$coef[length(model803@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u803, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u803, null="punif")$p.value)

model804=garchFit(formula=~arma(4,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res804=residuals(model804, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res804, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res804^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model804@fit$ics[1])
BIC_list=append(BIC_list, model804@fit$ics[2])
u804=psged(res804, mean=0, sd=1, nu=tail(model804@fit$coef, n=1), xi=model804@fit$coef[length(model804@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u804, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u804, null="punif")$p.value)

model805=garchFit(formula=~arma(4,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res805=residuals(model805, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res805, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res805^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model805@fit$ics[1])
BIC_list=append(BIC_list, model805@fit$ics[2])
u805=psged(res805, mean=0, sd=1, nu=tail(model805@fit$coef, n=1), xi=model805@fit$coef[length(model805@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u805, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u805, null="punif")$p.value)

model806=garchFit(formula=~arma(4,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res806=residuals(model806, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res806, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res806^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model806@fit$ics[1])
BIC_list=append(BIC_list, model806@fit$ics[2])
u806=psged(res806, mean=0, sd=1, nu=tail(model806@fit$coef, n=1), xi=model806@fit$coef[length(model806@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u806, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u806, null="punif")$p.value)

model807=garchFit(formula=~arma(4,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res807=residuals(model807, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res807, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res807^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model807@fit$ics[1])
BIC_list=append(BIC_list, model807@fit$ics[2])
u807=psged(res807, mean=0, sd=1, nu=tail(model807@fit$coef, n=1), xi=model807@fit$coef[length(model807@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u807, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u807, null="punif")$p.value)

model808=garchFit(formula=~arma(4,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res808=residuals(model808, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res808, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res808^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model808@fit$ics[1])
BIC_list=append(BIC_list, model808@fit$ics[2])
u808=psged(res808, mean=0, sd=1, nu=tail(model808@fit$coef, n=1), xi=model808@fit$coef[length(model808@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u808, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u808, null="punif")$p.value)

model809=garchFit(formula=~arma(4,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res809=residuals(model809, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res809, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res809^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model809@fit$ics[1])
BIC_list=append(BIC_list, model809@fit$ics[2])
u809=psged(res809, mean=0, sd=1, nu=tail(model809@fit$coef, n=1), xi=model809@fit$coef[length(model809@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u809, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u809, null="punif")$p.value)

model810=garchFit(formula=~arma(4,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res810=residuals(model810, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res810, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
boxTest2=append(boxTest2, Box.test(res810^2, lag=10, type=c("Ljung-Box"), fitdf=5)$p.value)
AIC_list=append(AIC_list, model810@fit$ics[1])
BIC_list=append(BIC_list, model810@fit$ics[2])
u810=psged(res810, mean=0, sd=1, nu=tail(model810@fit$coef, n=1), xi=model810@fit$coef[length(model810@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u810, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u810, null="punif")$p.value)

model811=garchFit(formula=~arma(4,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res811=residuals(model811, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res811, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res811^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model811@fit$ics[1])
BIC_list=append(BIC_list, model811@fit$ics[2])
u811=psged(res811, mean=0, sd=1, nu=tail(model811@fit$coef, n=1), xi=model811@fit$coef[length(model811@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u811, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u811, null="punif")$p.value)

model812=garchFit(formula=~arma(4,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res812=residuals(model812, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res812, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res812^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model812@fit$ics[1])
BIC_list=append(BIC_list, model812@fit$ics[2])
u812=psged(res812, mean=0, sd=1, nu=tail(model812@fit$coef, n=1), xi=model812@fit$coef[length(model812@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u812, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u812, null="punif")$p.value)

model813=garchFit(formula=~arma(4,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res813=residuals(model813, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res813, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res813^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model813@fit$ics[1])
BIC_list=append(BIC_list, model813@fit$ics[2])
u813=psged(res813, mean=0, sd=1, nu=tail(model813@fit$coef, n=1), xi=model813@fit$coef[length(model813@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u813, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u813, null="punif")$p.value)

model814=garchFit(formula=~arma(4,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res814=residuals(model814, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res814, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res814^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model814@fit$ics[1])
BIC_list=append(BIC_list, model814@fit$ics[2])
u814=psged(res814, mean=0, sd=1, nu=tail(model814@fit$coef, n=1), xi=model814@fit$coef[length(model814@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u814, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u814, null="punif")$p.value)

model815=garchFit(formula=~arma(4,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res815=residuals(model815, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res815, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res815^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model815@fit$ics[1])
BIC_list=append(BIC_list, model815@fit$ics[2])
u815=psged(res815, mean=0, sd=1, nu=tail(model815@fit$coef, n=1), xi=model815@fit$coef[length(model815@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u815, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u815, null="punif")$p.value)

model816=garchFit(formula=~arma(4,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res816=residuals(model816, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res816, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res816^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model816@fit$ics[1])
BIC_list=append(BIC_list, model816@fit$ics[2])
u816=psged(res816, mean=0, sd=1, nu=tail(model816@fit$coef, n=1), xi=model816@fit$coef[length(model816@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u816, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u816, null="punif")$p.value)

model817=garchFit(formula=~arma(4,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res817=residuals(model817, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res817, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res817^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model817@fit$ics[1])
BIC_list=append(BIC_list, model817@fit$ics[2])
u817=psged(res817, mean=0, sd=1, nu=tail(model817@fit$coef, n=1), xi=model817@fit$coef[length(model817@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u817, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u817, null="punif")$p.value)

model818=garchFit(formula=~arma(4,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res818=residuals(model818, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res818, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res818^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model818@fit$ics[1])
BIC_list=append(BIC_list, model818@fit$ics[2])
u818=psged(res818, mean=0, sd=1, nu=tail(model818@fit$coef, n=1), xi=model818@fit$coef[length(model818@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u818, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u818, null="punif")$p.value)

model819=garchFit(formula=~arma(4,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res819=residuals(model819, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res819, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
boxTest2=append(boxTest2, Box.test(res819^2, lag=10, type=c("Ljung-Box"), fitdf=6)$p.value)
AIC_list=append(AIC_list, model819@fit$ics[1])
BIC_list=append(BIC_list, model819@fit$ics[2])
u819=psged(res819, mean=0, sd=1, nu=tail(model819@fit$coef, n=1), xi=model819@fit$coef[length(model819@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u819, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u819, null="punif")$p.value)

model820=garchFit(formula=~arma(4,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res820=residuals(model820, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res820, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res820^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model820@fit$ics[1])
BIC_list=append(BIC_list, model820@fit$ics[2])
u820=psged(res820, mean=0, sd=1, nu=tail(model820@fit$coef, n=1), xi=model820@fit$coef[length(model820@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u820, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u820, null="punif")$p.value)

model821=garchFit(formula=~arma(4,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res821=residuals(model821, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res821, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res821^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model821@fit$ics[1])
BIC_list=append(BIC_list, model821@fit$ics[2])
u821=psged(res821, mean=0, sd=1, nu=tail(model821@fit$coef, n=1), xi=model821@fit$coef[length(model821@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u821, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u821, null="punif")$p.value)

model822=garchFit(formula=~arma(4,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res822=residuals(model822, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res822, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res822^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model822@fit$ics[1])
BIC_list=append(BIC_list, model822@fit$ics[2])
u822=psged(res822, mean=0, sd=1, nu=tail(model822@fit$coef, n=1), xi=model822@fit$coef[length(model822@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u822, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u822, null="punif")$p.value)

model823=garchFit(formula=~arma(4,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res823=residuals(model823, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res823, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res823^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model823@fit$ics[1])
BIC_list=append(BIC_list, model823@fit$ics[2])
u823=psged(res823, mean=0, sd=1, nu=tail(model823@fit$coef, n=1), xi=model823@fit$coef[length(model823@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u823, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u823, null="punif")$p.value)

model824=garchFit(formula=~arma(4,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res824=residuals(model824, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res824, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res824^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model824@fit$ics[1])
BIC_list=append(BIC_list, model824@fit$ics[2])
u824=psged(res824, mean=0, sd=1, nu=tail(model824@fit$coef, n=1), xi=model824@fit$coef[length(model824@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u824, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u824, null="punif")$p.value)

model825=garchFit(formula=~arma(4,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res825=residuals(model825, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res825, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res825^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model825@fit$ics[1])
BIC_list=append(BIC_list, model825@fit$ics[2])
u825=psged(res825, mean=0, sd=1, nu=tail(model825@fit$coef, n=1), xi=model825@fit$coef[length(model825@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u825, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u825, null="punif")$p.value)

model826=garchFit(formula=~arma(4,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res826=residuals(model826, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res826, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res826^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model826@fit$ics[1])
BIC_list=append(BIC_list, model826@fit$ics[2])
u826=psged(res826, mean=0, sd=1, nu=tail(model826@fit$coef, n=1), xi=model826@fit$coef[length(model826@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u826, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u826, null="punif")$p.value)

model827=garchFit(formula=~arma(4,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res827=residuals(model827, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res827, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res827^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model827@fit$ics[1])
BIC_list=append(BIC_list, model827@fit$ics[2])
u827=psged(res827, mean=0, sd=1, nu=tail(model827@fit$coef, n=1), xi=model827@fit$coef[length(model827@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u827, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u827, null="punif")$p.value)

model828=garchFit(formula=~arma(4,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res828=residuals(model828, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res828, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res828^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model828@fit$ics[1])
BIC_list=append(BIC_list, model828@fit$ics[2])
u828=psged(res828, mean=0, sd=1, nu=tail(model828@fit$coef, n=1), xi=model828@fit$coef[length(model828@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u828, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u828, null="punif")$p.value)

model829=garchFit(formula=~arma(7,0)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res829=residuals(model829, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res829, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res829^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model829@fit$ics[1])
BIC_list=append(BIC_list, model829@fit$ics[2])
u829=psged(res829, mean=0, sd=1, nu=tail(model829@fit$coef, n=1), xi=model829@fit$coef[length(model829@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u829, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u829, null="punif")$p.value)

model830=garchFit(formula=~arma(7,0)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res830=residuals(model830, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res830, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res830^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model830@fit$ics[1])
BIC_list=append(BIC_list, model830@fit$ics[2])
u830=psged(res830, mean=0, sd=1, nu=tail(model830@fit$coef, n=1), xi=model830@fit$coef[length(model830@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u830, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u830, null="punif")$p.value)

model831=garchFit(formula=~arma(7,0)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res831=residuals(model831, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res831, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res831^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model831@fit$ics[1])
BIC_list=append(BIC_list, model831@fit$ics[2])
u831=psged(res831, mean=0, sd=1, nu=tail(model831@fit$coef, n=1), xi=model831@fit$coef[length(model831@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u831, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u831, null="punif")$p.value)

model832=garchFit(formula=~arma(7,0)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res832=residuals(model832, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res832, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res832^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model832@fit$ics[1])
BIC_list=append(BIC_list, model832@fit$ics[2])
u832=psged(res832, mean=0, sd=1, nu=tail(model832@fit$coef, n=1), xi=model832@fit$coef[length(model832@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u832, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u832, null="punif")$p.value)

model833=garchFit(formula=~arma(7,0)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res833=residuals(model833, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res833, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res833^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model833@fit$ics[1])
BIC_list=append(BIC_list, model833@fit$ics[2])
u833=psged(res833, mean=0, sd=1, nu=tail(model833@fit$coef, n=1), xi=model833@fit$coef[length(model833@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u833, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u833, null="punif")$p.value)

model834=garchFit(formula=~arma(7,0)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res834=residuals(model834, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res834, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res834^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model834@fit$ics[1])
BIC_list=append(BIC_list, model834@fit$ics[2])
u834=psged(res834, mean=0, sd=1, nu=tail(model834@fit$coef, n=1), xi=model834@fit$coef[length(model834@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u834, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u834, null="punif")$p.value)

model835=garchFit(formula=~arma(7,0)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res835=residuals(model835, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res835, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res835^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model835@fit$ics[1])
BIC_list=append(BIC_list, model835@fit$ics[2])
u835=psged(res835, mean=0, sd=1, nu=tail(model835@fit$coef, n=1), xi=model835@fit$coef[length(model835@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u835, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u835, null="punif")$p.value)

model836=garchFit(formula=~arma(7,0)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res836=residuals(model836, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res836, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res836^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model836@fit$ics[1])
BIC_list=append(BIC_list, model836@fit$ics[2])
u836=psged(res836, mean=0, sd=1, nu=tail(model836@fit$coef, n=1), xi=model836@fit$coef[length(model836@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u836, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u836, null="punif")$p.value)

model837=garchFit(formula=~arma(7,0)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res837=residuals(model837, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res837, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
boxTest2=append(boxTest2, Box.test(res837^2, lag=10, type=c("Ljung-Box"), fitdf=7)$p.value)
AIC_list=append(AIC_list, model837@fit$ics[1])
BIC_list=append(BIC_list, model837@fit$ics[2])
u837=psged(res837, mean=0, sd=1, nu=tail(model837@fit$coef, n=1), xi=model837@fit$coef[length(model837@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u837, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u837, null="punif")$p.value)

model838=garchFit(formula=~arma(7,1)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res838=residuals(model838, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res838, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res838^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model838@fit$ics[1])
BIC_list=append(BIC_list, model838@fit$ics[2])
u838=psged(res838, mean=0, sd=1, nu=tail(model838@fit$coef, n=1), xi=model838@fit$coef[length(model838@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u838, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u838, null="punif")$p.value)

model839=garchFit(formula=~arma(7,1)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res839=residuals(model839, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res839, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res839^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model839@fit$ics[1])
BIC_list=append(BIC_list, model839@fit$ics[2])
u839=psged(res839, mean=0, sd=1, nu=tail(model839@fit$coef, n=1), xi=model839@fit$coef[length(model839@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u839, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u839, null="punif")$p.value)

model840=garchFit(formula=~arma(7,1)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res840=residuals(model840, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res840, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res840^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model840@fit$ics[1])
BIC_list=append(BIC_list, model840@fit$ics[2])
u840=psged(res840, mean=0, sd=1, nu=tail(model840@fit$coef, n=1), xi=model840@fit$coef[length(model840@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u840, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u840, null="punif")$p.value)

model841=garchFit(formula=~arma(7,1)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res841=residuals(model841, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res841, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res841^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model841@fit$ics[1])
BIC_list=append(BIC_list, model841@fit$ics[2])
u841=psged(res841, mean=0, sd=1, nu=tail(model841@fit$coef, n=1), xi=model841@fit$coef[length(model841@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u841, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u841, null="punif")$p.value)

model842=garchFit(formula=~arma(7,1)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res842=residuals(model842, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res842, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res842^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model842@fit$ics[1])
BIC_list=append(BIC_list, model842@fit$ics[2])
u842=psged(res842, mean=0, sd=1, nu=tail(model842@fit$coef, n=1), xi=model842@fit$coef[length(model842@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u842, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u842, null="punif")$p.value)

model843=garchFit(formula=~arma(7,1)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res843=residuals(model843, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res843, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res843^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model843@fit$ics[1])
BIC_list=append(BIC_list, model843@fit$ics[2])
u843=psged(res843, mean=0, sd=1, nu=tail(model843@fit$coef, n=1), xi=model843@fit$coef[length(model843@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u843, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u843, null="punif")$p.value)

model844=garchFit(formula=~arma(7,1)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res844=residuals(model844, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res844, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res844^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model844@fit$ics[1])
BIC_list=append(BIC_list, model844@fit$ics[2])
u844=psged(res844, mean=0, sd=1, nu=tail(model844@fit$coef, n=1), xi=model844@fit$coef[length(model844@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u844, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u844, null="punif")$p.value)

model845=garchFit(formula=~arma(7,1)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res845=residuals(model845, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res845, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res845^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model845@fit$ics[1])
BIC_list=append(BIC_list, model845@fit$ics[2])
u845=psged(res845, mean=0, sd=1, nu=tail(model845@fit$coef, n=1), xi=model845@fit$coef[length(model845@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u845, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u845, null="punif")$p.value)

model846=garchFit(formula=~arma(7,1)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res846=residuals(model846, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res846, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
boxTest2=append(boxTest2, Box.test(res846^2, lag=10, type=c("Ljung-Box"), fitdf=8)$p.value)
AIC_list=append(AIC_list, model846@fit$ics[1])
BIC_list=append(BIC_list, model846@fit$ics[2])
u846=psged(res846, mean=0, sd=1, nu=tail(model846@fit$coef, n=1), xi=model846@fit$coef[length(model846@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u846, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u846, null="punif")$p.value)

model847=garchFit(formula=~arma(7,2)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res847=residuals(model847, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res847, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res847^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model847@fit$ics[1])
BIC_list=append(BIC_list, model847@fit$ics[2])
u847=psged(res847, mean=0, sd=1, nu=tail(model847@fit$coef, n=1), xi=model847@fit$coef[length(model847@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u847, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u847, null="punif")$p.value)

model848=garchFit(formula=~arma(7,2)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res848=residuals(model848, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res848, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res848^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model848@fit$ics[1])
BIC_list=append(BIC_list, model848@fit$ics[2])
u848=psged(res848, mean=0, sd=1, nu=tail(model848@fit$coef, n=1), xi=model848@fit$coef[length(model848@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u848, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u848, null="punif")$p.value)

model849=garchFit(formula=~arma(7,2)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res849=residuals(model849, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res849, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res849^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model849@fit$ics[1])
BIC_list=append(BIC_list, model849@fit$ics[2])
u849=psged(res849, mean=0, sd=1, nu=tail(model849@fit$coef, n=1), xi=model849@fit$coef[length(model849@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u849, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u849, null="punif")$p.value)

model850=garchFit(formula=~arma(7,2)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res850=residuals(model850, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res850, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res850^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model850@fit$ics[1])
BIC_list=append(BIC_list, model850@fit$ics[2])
u850=psged(res850, mean=0, sd=1, nu=tail(model850@fit$coef, n=1), xi=model850@fit$coef[length(model850@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u850, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u850, null="punif")$p.value)

model851=garchFit(formula=~arma(7,2)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res851=residuals(model851, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res851, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res851^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model851@fit$ics[1])
BIC_list=append(BIC_list, model851@fit$ics[2])
u851=psged(res851, mean=0, sd=1, nu=tail(model851@fit$coef, n=1), xi=model851@fit$coef[length(model851@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u851, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u851, null="punif")$p.value)

model852=garchFit(formula=~arma(7,2)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res852=residuals(model852, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res852, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res852^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model852@fit$ics[1])
BIC_list=append(BIC_list, model852@fit$ics[2])
u852=psged(res852, mean=0, sd=1, nu=tail(model852@fit$coef, n=1), xi=model852@fit$coef[length(model852@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u852, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u852, null="punif")$p.value)

model853=garchFit(formula=~arma(7,2)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res853=residuals(model853, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res853, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res853^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model853@fit$ics[1])
BIC_list=append(BIC_list, model853@fit$ics[2])
u853=psged(res853, mean=0, sd=1, nu=tail(model853@fit$coef, n=1), xi=model853@fit$coef[length(model853@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u853, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u853, null="punif")$p.value)

model854=garchFit(formula=~arma(7,2)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res854=residuals(model854, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res854, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res854^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model854@fit$ics[1])
BIC_list=append(BIC_list, model854@fit$ics[2])
u854=psged(res854, mean=0, sd=1, nu=tail(model854@fit$coef, n=1), xi=model854@fit$coef[length(model854@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u854, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u854, null="punif")$p.value)

model855=garchFit(formula=~arma(7,2)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res855=residuals(model855, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res855, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
boxTest2=append(boxTest2, Box.test(res855^2, lag=10, type=c("Ljung-Box"), fitdf=9)$p.value)
AIC_list=append(AIC_list, model855@fit$ics[1])
BIC_list=append(BIC_list, model855@fit$ics[2])
u855=psged(res855, mean=0, sd=1, nu=tail(model855@fit$coef, n=1), xi=model855@fit$coef[length(model855@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u855, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u855, null="punif")$p.value)

model856=garchFit(formula=~arma(7,3)+garch(1,1),data=p_ftse,trace=F,cond.dist="sged")
res856=residuals(model856, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res856, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res856^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model856@fit$ics[1])
BIC_list=append(BIC_list, model856@fit$ics[2])
u856=psged(res856, mean=0, sd=1, nu=tail(model856@fit$coef, n=1), xi=model856@fit$coef[length(model856@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u856, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u856, null="punif")$p.value)

model857=garchFit(formula=~arma(7,3)+garch(1,2),data=p_ftse,trace=F,cond.dist="sged")
res857=residuals(model857, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res857, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res857^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model857@fit$ics[1])
BIC_list=append(BIC_list, model857@fit$ics[2])
u857=psged(res857, mean=0, sd=1, nu=tail(model857@fit$coef, n=1), xi=model857@fit$coef[length(model857@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u857, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u857, null="punif")$p.value)

model858=garchFit(formula=~arma(7,3)+garch(1,3),data=p_ftse,trace=F,cond.dist="sged")
res858=residuals(model858, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res858, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res858^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model858@fit$ics[1])
BIC_list=append(BIC_list, model858@fit$ics[2])
u858=psged(res858, mean=0, sd=1, nu=tail(model858@fit$coef, n=1), xi=model858@fit$coef[length(model858@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u858, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u858, null="punif")$p.value)

model859=garchFit(formula=~arma(7,3)+garch(2,1),data=p_ftse,trace=F,cond.dist="sged")
res859=residuals(model859, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res859, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res859^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model859@fit$ics[1])
BIC_list=append(BIC_list, model859@fit$ics[2])
u859=psged(res859, mean=0, sd=1, nu=tail(model859@fit$coef, n=1), xi=model859@fit$coef[length(model859@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u859, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u859, null="punif")$p.value)

model860=garchFit(formula=~arma(7,3)+garch(2,2),data=p_ftse,trace=F,cond.dist="sged")
res860=residuals(model860, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res860, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res860^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model860@fit$ics[1])
BIC_list=append(BIC_list, model860@fit$ics[2])
u860=psged(res860, mean=0, sd=1, nu=tail(model860@fit$coef, n=1), xi=model860@fit$coef[length(model860@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u860, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u860, null="punif")$p.value)

model861=garchFit(formula=~arma(7,3)+garch(2,3),data=p_ftse,trace=F,cond.dist="sged")
res861=residuals(model861, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res861, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res861^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model861@fit$ics[1])
BIC_list=append(BIC_list, model861@fit$ics[2])
u861=psged(res861, mean=0, sd=1, nu=tail(model861@fit$coef, n=1), xi=model861@fit$coef[length(model861@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u861, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u861, null="punif")$p.value)

model862=garchFit(formula=~arma(7,3)+garch(3,1),data=p_ftse,trace=F,cond.dist="sged")
res862=residuals(model862, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res862, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res862^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model862@fit$ics[1])
BIC_list=append(BIC_list, model862@fit$ics[2])
u862=psged(res862, mean=0, sd=1, nu=tail(model862@fit$coef, n=1), xi=model862@fit$coef[length(model862@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u862, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u862, null="punif")$p.value)

model863=garchFit(formula=~arma(7,3)+garch(3,2),data=p_ftse,trace=F,cond.dist="sged")
res863=residuals(model863, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res863, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res863^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model863@fit$ics[1])
BIC_list=append(BIC_list, model863@fit$ics[2])
u863=psged(res863, mean=0, sd=1, nu=tail(model863@fit$coef, n=1), xi=model863@fit$coef[length(model863@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u863, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u863, null="punif")$p.value)

model864=garchFit(formula=~arma(7,3)+garch(3,3),data=p_ftse,trace=F,cond.dist="sged")
res864=residuals(model864, standardize=TRUE)
boxTest1=append(boxTest1, Box.test(res864, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
boxTest2=append(boxTest2, Box.test(res864^2, lag=10, type=c("Ljung-Box"), fitdf=10)$p.value)
AIC_list=append(AIC_list, model864@fit$ics[1])
BIC_list=append(BIC_list, model864@fit$ics[2])
u864=psged(res864, mean=0, sd=1, nu=tail(model864@fit$coef, n=1), xi=model864@fit$coef[length(model864@fit$coef)-1])[4:length(p_ftse)]
ksTests=append(ksTests,LcKS(u864, cdf = "punif")$p.value)
adTests=append(adTests, ad.test(u864, null="punif")$p.value)






df=data.frame(model_no=c(1:length(boxTest1)), boxTest1, boxTest2, ksTests, adTests, AIC_list, BIC_list)
write_xlsx(df,"C:\\Users\\Sham\\Desktop\\Uni Stuff\\Year 3\\Decision and Risk\\SCRAP ICA TASK 2\\FTSE DATA FINAL.xlsx")