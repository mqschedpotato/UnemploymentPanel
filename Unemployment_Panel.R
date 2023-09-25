library(nortest)
library(lmtest)
library(plm)

## Import Dataset ##
library(readr) 
datapanel <- read_csv("dataPanel.csv")
View(datapanel)
summary(datapanel)

#test vif
testvif<-lm(pengangguran~pendidikan+jumlahPenduduk+HP+kerja+pdb+upah, data=datapanel)
vif(testvif)

#Uji Chow
#H0: Model common effect
#H1: Model fixed effect
common = plm(pengangguran~pendidikan+jumlahPenduduk+HP+kerja+pdb+upah, data=datapanel, model = "pooling")
fixed = plm(pengangguran~pendidikan+jumlahPenduduk+HP+kerja+pdb+upah, data=datapanel, model="within")
pooltest(common, fixed)
#Berdasarkan hasil uji Chow menunjukkan bahwa nilai p-value = 4,996e-11, 
#yang berarti hipotesis nol ditolak. Artinya metode fixed effect lebih
#baik digunakan daripada menggunakan metode common effect

#Uji Hausman
#H0: Model random effect
#H1: Model fixed effect
fixed=plm(pengangguran~pendidikan+jumlahPenduduk+HP+kerja+pdb+upah, data=datapanel, model="within")
random=plm(pengangguran~pendidikan+jumlahPenduduk+HP+kerja+pdb+upah, data=datapanel, model="random")
phtest(fixed,random)
#Karena 1.309e-05 < 0.05, maka hipotesis 0 ditolak
#Artinya metode fixed lbh baik daripada random effect

#Uji signifikan model
summary(fixed)

# model 2 : pendidikan & pdb
# Uji Chouw
common = plm(pengangguran~pendidikan+pdb, data=datapanel, model = "pooling")
fixed = plm(pengangguran~pendidikan+pdb, data=datapanel, model = "within")
pooltest(common, fixed)
#Berdasarkan hasil uji Chow menunjukkan bahwa nilai p-value = 2.2e-16, 
#yang berarti hipotesis nol ditolak. Artinya metode fixed effect lebih
#baik digunakan daripada menggunakan metode common effect

# Uji Hausman
fixed=plm(pengangguran~pendidikan+pdb, data=datapanel, model = "within")
random=plm(pengangguran~pendidikan+pdb, data=datapanel, model = "random")
phtest(fixed,random)
#Karena 4,514e-05 < 0.05, maka hipotesis 0 ditolak
#Artinya metode fixed lbh baik daripada random effect

#Uji signifikan model 2
summary(fixed)

## Asumsi Residual ##

# Distribusi Normal
library(nortest)
er<- resid(fixed)
lillie.test(er)

# Identik
bptest(fixed)

# Independent
pdwtest(fixed)


## Weighted Least Square
# Fitted value(ytopi)
fit <- fitted(fixed)
fit

## Menghitung Weighted Least
model2 <- lm(er~fit)
wt = 1 / lm(abs(model2$residuals) ~ model2$fitted.values)$fitted.values^2
model_wls<-plm(pengangguran~pendidikan+pdb,data=datapanel,weights = wt)
summary(model_wls)

#least square dummy variable
lsdv = lm(pengangguran~pendidikan+pdb+factor(Tahun)-1+factor(provinsi)-1, data=datapanel)
summary(lsdv)




