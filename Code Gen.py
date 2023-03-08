#Defining Paramters
cds = ["norm", "ged", "std", "sstd", "snorm", "sged"]
pacf_val = [1,3,4,7]
index = "ftse"
tag = "ftse"
lag = "10"


#Printing the first part of the r code
print("library(writexl)\nlibrary(VineCopula)\nlibrary(fGarch)\nlibrary(KScorrect)\nlibrary(stats)\nlibrary(ADGofTest)\nlibrary(tseries)\n\nboxTest1=c()\nboxTest2=c()\nksTests=c()\nadTests=c()\nAIC_list=c()\nBIC_list=c()\n\nboxTest1\nboxTest2\nksTests\nadTests\nAIC_list\nBIC_list\n\np_"+index+"=matrix(get.hist.quote(instrument=\"^"+tag+"\", start=\"2000-01-01\",\n                             end=\"2021-01-01\", quote=\"AdjClose\", compression=\'w\'))\n\n#p_"+index+" = na.omit(p_"+index+")\np_"+index+"=diff(log(p_"+index+"))\njarqueberaTest(p_"+index+")\n\npar(mfrow=c(2,2))\nacf(p_"+index+", col=\"green\", lwd=2)\npacf(p_"+index+", col=\"green\", lwd=2)\nacf(p_"+index+"^2, col=\"red\", lwd=2)\npar(mfrow=c(1,1))\n")
print("\n\n\n\n")



#Iterating through the parameters and printing out the different possibilities for the r code
count=1
for cd in cds:
    for a in pacf_val:
        for b in range(0, 4):
            for c in range(1, 4):
                for d in range(1, 4):
                    print("model"+str(count)+"=garchFit(formula=~arma("+str(a)+","+str(b)+")+garch("+str(c)+","+str(d)+"),data=p_"+index+",trace=F,cond.dist=\""+cd+"\")")
                    print("res"+str(count)+"=residuals(model"+str(count)+", standardize=TRUE)")
                    print("boxTest1=append(boxTest1, Box.test(res"+str(count)+", lag="+lag+", type=c(\"Ljung-Box\"), fitdf="+str(a+b)+")$p.value)")
                    print("boxTest2=append(boxTest2, Box.test(res"+str(count)+"^2, lag="+lag+", type=c(\"Ljung-Box\"), fitdf="+str(a+b)+")$p.value)")
                    print("AIC_list=append(AIC_list, model"+str(count)+"@fit$ics[1])")
                    print("BIC_list=append(BIC_list, model"+str(count)+"@fit$ics[2])")
                    if cd == "norm":
                        print("u"+str(count)+"=pnorm(res"+str(count)+", mean=0, sd=1)[4:length(p_"+index+")]")
                    elif cd == "ged":
                        print("u"+str(count)+"=pged(res"+str(count)+", mean=0, sd=1, nu=tail(model"+str(count)+"@fit$coef, n=1))[4:length(p_"+index+")]")
                    elif cd == "std":
                        print("u"+str(count)+"=pstd(res"+str(count)+", mean=0, sd=1, nu=tail(model"+str(count)+"@fit$coef, n=1))[4:length(p_"+index+")]")
                    elif cd == "sstd":
                        print("u"+str(count)+"=psstd(res"+str(count)+", mean=0, sd=1, nu=tail(model"+str(count)+"@fit$coef, n=1), xi=model"+str(count)+"@fit$coef[length(model"+str(count)+"@fit$coef)-1])[4:length(p_"+index+")]")
                    elif cd == "snorm":
                        print("u"+str(count)+"=psnorm(res"+str(count)+", mean=0, sd=1, xi=tail(model"+str(count)+"@fit$coef, n=1))[4:length(p_"+index+")]")
                    elif cd == "sged":
                        print("u"+str(count)+"=psged(res"+str(count)+", mean=0, sd=1, nu=tail(model"+str(count)+"@fit$coef, n=1), xi=model"+str(count)+"@fit$coef[length(model"+str(count)+"@fit$coef)-1])[4:length(p_"+index+")]")

                    print("ksTests=append(ksTests,LcKS(u"+str(count)+", cdf = \"punif\")$p.value)")
                    print("adTests=append(adTests, ad.test(u"+str(count)+", null=\"punif\")$p.value)")
                    

                    
                    print()

                    
                    count+=1
                    if count==10:
                        break


print("\n\n\n\n")
print("df=data.frame(model_no=c(1:length(boxTest1)), boxTest1, boxTest2, ksTests, adTests, AIC_list, BIC_list)\nwrite_xlsx(df,\"C:\\\\Users\\\\Sayed\\\\Desktop\\\\TASK 2 STAT0011\\\\"+index.upper()+" DATA.xlsx\")")


