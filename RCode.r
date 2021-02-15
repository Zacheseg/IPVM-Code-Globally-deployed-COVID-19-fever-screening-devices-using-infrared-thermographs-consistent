###########################################
#Code for New infrared thermographs used for COVID-19 fever screening normalize elevated temperatures
#Author: Zachary Segal
#Date: 12/02/21
###########################################

# Initial Statistics and Plots --------------------------------------------------------------------

# Key --------------------------------------------------------------------
#BB is the Black Body we used as a target for testing
#Ex represents Extech on body mode unless otherwise specified; Black
#Surf represents Extech on Surface Mode
#FLir/flir represents Flir IRT; Brown
#Bems/bems/bem/b all represent the Bems IRT; Purple 
#TVT/tvt/t all represent TVT IRT; Blue
#Dahua/Dah/dah/d represent Dahua IRT; Red
#Cert/cert/c represetns Certify IRT; deeppink
#Zk/zk/Z/z represetns ZKTeco IRT; darkgoldenrod
#Hik/hik/h/H represents Hikvision IRT; lightseagreen
#md/Md/m/m represents Meridian IRT; orange


# Bems --------------------------------------------------------------------

#Loading Data
bems = read.csv("/Users/zacharysegal/Downloads/IRT Specs FDA - Bems.csv")

#Ensuring all data is in range, removing nas, adding column for difference between device and BB
#In some rounds low or high temperatures were recorded that should not have been
bems = bems[bems$Blackbody.Temperature >= 35 & bems$Blackbody.Temperature <= 40,]
bems = bems[!is.na(bems$Blackbody.Temperature ),]
bems$Dev.Delta = as.numeric(bems$Device.Measurement) - as.numeric(bems$Blackbody.Temperature)
bems$Device.Measurement = as.numeric(bems$Device.Measurement)


##Comparing Device Deviation from BB and BB##
#Making a linear model to compare Device Deviation from BB and BB
BemsAc = lm(bems$Dev.Delta~ bems$Blackbody.Temperature) 
summary(BemsAc)#Adjusted R-squared:  0.9978; Beta  -0.99688; P-value <2e-16 ***

##Comparing Device, Extech on Body Mode, and BB##
#Linear Models
BemsBB <- lm(bems$Device.Measurement ~ bems$Blackbody.Temperature); summary(BemsBB)
#Adjusted R-squared:  -0.04785; Beta 0.003117; P-Value 0.772  

ExBB <- lm(bems$Ex.Body ~ bems$Blackbody.Temperature); summary(ExBB)
#Adjusted R-squared:  0.981; Beta 0.76234; P-Value < 2e-16 ***

#Ploting results
plot(bems$Blackbody.Temperature, bems$Device.Measurement, 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "Bems, Flir, Extech vs Subject Temperature", lty = 1,
     ylim=c(34.5, 42.5), col = "purple" , bg = 'blue')
points(bems$Blackbody.Temperature,bems$Ex.Body,lty = 2, col = "black") 
points(bems$Blackbody.Temperature, bems$FLIR.Measurement, lty = 3, col = "brown")
legend("topleft", bty = "n", legend = c( "Extech Body Adj. R sqrd .98; Beta 76", "Bems Adj. R sqrd <.01; Beta 0", "Flir Adj R sqrd .99; Beta 1"), text.col = c(1,"purple", "brown"), bg = "transparent", lty = c(1,2,3))
abline(BemsBB, col = "purple", lty = 2)
abline(ExBB, lty = 1)
abline(lty = 3, lm(bems$FLIR.Measurement ~ bems$Blackbody.Temperature), col = "brown")


#TVT --------------------------------------------------------------------

#Loading Data
tvt = read.csv("/Users/zacharysegal/Downloads/IRT Specs FDA - TVT.csv")

#Ensuring all data is in range, adding column for difference between device and BB
tvt = tvt[tvt$Blackbody.Temperature >= 35 & tvt$Blackbody.Temperature <= 40,]
tvt$Dev.Delta = as.numeric(tvt$Device.Measurement) - as.numeric(tvt$Blackbody.Temperature)

##Comparing Device Deviation from BB and BB##
#Making a linear model to compare Device Deviation from BB and BB
tvtAc = lm(tvt$Dev.Delta~tvt$Blackbody.Temperature); summary(tvtAc)
# Adjusted R-squared:   0.9683; beta  -0.78182; p-value 6.57e-16 ***
tvtAc2 = lm(log(tvt$Dev.Delta+4)~tvt$Blackbody.Temperature); summary(tvtAc2)
#Adjusted R-squared:  0.9871 

##Comparing Device, Extech on Body Mode, and BB##
tvtBB <- lm(tvt$Device.Measurement ~ tvt$Blackbody.Temperature); summary(tvtBB)
#Adjusted R-squared:  0.6996; Beta  0.21818; p-value  1.41e-06 ***

ExBB <- lm(tvt$Ex.Body ~ tvt$Blackbody.Temperature); summary(ExBB)
#Adjusted R-squared:  0.9716; beta 0.74182; p-value < 2e-16 ***


#Plot
plot(tvt$Blackbody.Temperature, tvt$Device.Measurement, 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "TVT, Flir, Extech vs Subject Temperature", 
     ylim=c(34.5, 42.5), col = "blue", pch = 1)

points(tvt$Blackbody.Temperature,tvt$Ex.Body, col = "black", pch = 2) 
points(tvt$Blackbody.Temperature,tvt$FLIR.Measurement, col = "brown", pch = 3) 

legend("topleft", bty = "n", legend = c( "Extech Body Adj. R sqrd .97; Beta .74", "TVT Adj. R sqrd .7, Beta = .22", "Flir Adj. R sqrd .99, Beta = 1"), pch = c(2,1,3), text.col = c("black","blue", "brown"), bg = "transparent")
abline(tvtBB, col = "blue")
abline(ExBB)
abline(lm(tvt$FLIR.Measurement ~ tvt$Blackbody.Temperature), col = "brown")



#Dahua --------------------------------------------------------------------

#Loading Data
dah = read.csv("/Users/zacharysegal/Downloads/IRT Specs FDA - Dahua.csv")

#Ensuring all data is in range, removing nas, adding column for difference between device and BB
dah = dah[!is.na(dah$Blackbody.Temperature ),]
dah$Dev.Delta = as.numeric(dah$Device.Measurement) - as.numeric(dah$Blackbody.Temperature)
dah = dah[dah$Blackbody.Temperature >= 35 & dah$Blackbody.Temperature <= 40,]


##Comparing Device, Extech on Body Mode, and Flir with BB##
dahBB <- lm(dah$Device.Measurement ~ dah$Blackbody.Temperature)
summary(dahBB)#Adjusted R-squared:  0.9954;  beta   0.76883; p-value  <2e-16 ***

ExBB <- lm(dah$Ex.Body ~ dah$Blackbody.Temperature)
#Adjusted R-squared:  0.9808; beta 0.77974; p-value  < 2e-16 ***

FlirBB <- lm(dah$FLIR.Measurement ~ dah$Blackbody.Temperature)
#Adjusted R-squared:  0.9991; beta  0.998961; p-value  <2e-16 ***

#Comparing Disfference between reading and BB with BB Temperature

DahAc = lm(dah$Dev.Delta ~ dah$Blackbody.Temperature)
#Adjusted R-squared:  0.9513; Beta  -0.23117; p-value <2e-16 ***
DahAc2 = lm(log(dah$Dev.Delta+4) ~ (dah$Blackbody.Temperature));summary(DahAc2)
#not better


FlrAc = lm((dah$FLIR.Measurement-dah$Blackbody.Temperature) ~ dah$Blackbody.Temperature)
#Adjusted R-squared: -0.05132; beta  -0.001039; p-value 0.879

BBAc = lm((dah$Ex.Body-dah$Blackbody.Temperature) ~ dah$Blackbody.Temperature)
#Adjusted R-squared:  0.8013; beta -0.22026; p-value 2.62e-08 ***

#combining plots
par(mfrow=c(1,2))

#Dah vs Ex vs Temp
plot(dah$Blackbody.Temperature, dah$Device.Measurement, pch = 3, 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "Dahua, Flir, Extech vs Subject Temperature", col = "red")
points(dah$Blackbody.Temperature,dah$Ex.Body, pch = 1, col = "black") 
points(dah$Blackbody.Temperature,dah$FLIR.Measurement, pch = 2, col = "brown") 
legend(x = 34.2, y = 39.5, bty = "n", y.intersp =.5, cex = .9,
  legend = c( "Extech Body Adj. R sqrd .97; Beta .78", "Flir Adj. R sqrd .99; Beta .77", "Dahua Adj. R sqrd 1; Beta 1"),   pch = c(1,2,3),text.col = c("black","brown", "red"), bg = "transparent")
abline(dahBB, col = "red")
abline(ExBB)
abline(FlirBB, col = "brown")

plot(dah$Blackbody.Temperature, dah$Dev.Delta, ylim = c( -.7, 1.5),
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "Dahua, Flir, Extech and Subject Difference vs Subject", pch = 3, col = "red")
points(dah$Blackbody.Temperature, c(dah$Ex.Body-dah$Blackbody.Temperature), pch = 1, col = "black") 
points(dah$Blackbody.Temperature,c(dah$FLIR.Measurement-dah$Blackbody.Temperature), pch = 2, col = "brown") 
legend(x = 36.3, y = 1.6, y.intersp =.5, bty = "n",  pch = c(1,2,3),legend = c( "Extech Body Adj. R sqrd .8; Beta -.22", "Flir Adj. R sqrd 0; Beta 0", "Dahua Adj. R sqrd .95; Beta -.23"), text.col = c("black","brown", "red"), bg = "transparent")
abline(h = 0)
abline(h = 1)
#abline(h = -1)

#residuls
plot(dah$Blackbody.Temperature, DahAc$residuals, 
     xlab = "Subject Temperature °C", ylab = "Residual", 
     main = "Device ~ Subject Residuals", col = "red", type = "line", ylim = c(-.5,.5))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col= rgb(.05,.05,.05,alpha=0.1))
points(dah$Blackbody.Temperature,FlrAc$residuals, col = "brown", type = "line") 
points(dah$Blackbody.Temperature,BBAc$residuals, col = "black", type = "line") 
#legend("topleft", legend = c( "Extech Body", "Flir",  "Dahua"), text.col = c("black","seagreen","red"), bg = "transparent")
abline(h = 0)


#Hikvision Bullet --------------------------------------------------------------------

#Loading Data
hik = read.csv("/Users/zacharysegal/Downloads/Paper Rob 2.0 - Hik W BB.csv")

#Ensuring all data is in range, removing nas, adding column for difference between device and BB
hik = hik[!is.na(hik$Blackbody.Temperature ),]
hik$Dev.Delta = as.numeric(hik$Device.Measurement) - as.numeric(hik$Blackbody.Temperature)
hik = hik[hik$Blackbody.Temperature >= 35 & hik$Blackbody.Temperature <= 40,]


##Comparing Device, Extech on Body Mode, and Flir with BB##
hikBB <- lm(hik$Device.Measurement ~ hik$Blackbody.Temperature)
summary(hikBB)#Adjusted R-squared:  0.9924;  beta   0.80883; p-value  <2e-16 ***

ExBB <- lm(hik$Ex.Body ~ hik$Blackbody.Temperature); summary(ExBB)
#Adjusted R-squared:  0.9892; beta 0.73221; p-value  < 2e-16 ***

FlirBB <- lm(hik$FLIR.Measurement ~ hik$Blackbody.Temperature); summary(FlirBB)
#Adjusted R-squared:  0.9996; beta  0.996364; p-value  <2e-16 ***

#Comparing Disfference between reading and BB with BB Temperature

hikAc = lm(hik$Dev.Delta ~ hik$Blackbody.Temperature); summary(hikAc)
#Adjusted R-squared:  0.8789; Beta  -0.19117; p-value 2.28e-10 ***

FlrAc = lm((hik$FLIR.Measurement-hik$Blackbody.Temperature) ~ hik$Blackbody.Temperature); summary(FlrAc)
#Adjusted R-squared: -0.01409; beta  -0.003636; p-value 0.406

BBAc = lm((hik$Ex.Body-hik$Blackbody.Temperature) ~ hik$Blackbody.Temperature); summary(BBAc)
#Adjusted R-squared:  0.9244; beta -0.26779; p-value 2.54e-12 ***

#We tested logarithmic models as well, but chose not to include them except for with the meridian because linear regression
#is easier for readers to understand and the relations were barely strengthened
BBAc2 = lm(log(hik$Ex.Body-hik$Blackbody.Temperature +4) ~ hik$Blackbody.Temperature); summary(BBAc2)
#Adjusted R-squared: .94

#combining plots lightseagreen
par(mfrow=c(1,2))


#Hik vs Ex vs Temp
plot(hik$Blackbody.Temperature, hik$Device.Measurement, 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "Hikvision, Flir, Extech vs Subject Temperature", col = "lightseagreen", pch = 2)
points(hik$Blackbody.Temperature,hik$Ex.Body, col = "black", pch = 1) 
points(hik$Blackbody.Temperature,hik$FLIR.Measurement, col = "brown", pch = 3) 
legend(x = 34, y = 39.5, bty = "n",y.intersp =.5, cex = .9,
      legend = c( "Extech Body Adj. R sqrd .92; Beta -.27", "Hikvision Adj. R sqrd .99; Beta .8", "Flir Adj. R sqrd 1; Beta 1"), text.col = c("black","lightseagreen", " brown"), pch = c(1, 2, 3),bg = "transparent")
abline(hikBB, col = "lightseagreen")
abline(ExBB)
abline(FlirBB, col = "brown")

#accuracy
plot(hik$Blackbody.Temperature, c(hik$Device.Measurement - hik$Blackbody.Temperature), 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", ylim = c(-.6, 2),
     main = "Hikvision, Flir, Extech Difference from Subject vs Subject", col = "lightseagreen", pch = 2)
points(hik$Blackbody.Temperature,c(hik$Ex.Body- hik$Blackbody.Temperature), col = "black", pch = 1) 
points(hik$Blackbody.Temperature,c(hik$FLIR.Measurement- hik$Blackbody.Temperature), col = "brown" , pch = 3) 
legend(x = 35.2, y = 2, bty = "n", y.intersp =.5,
       legend = c( "Extech Body Adj. R sqrd .99 ; Beta .73", "Hikvision Adj. R sqrd .88; Beta -.19", "Flir Adj. R sqrd 0; Beta 0"), pch = c(1,2,3),text.col = c("black","lightseagreen", "brown"), bg = "transparent")
abline(h = 1)
abline(h = 0)

#residuls
plot(hik$Blackbody.Temperature,hikAc$residuals, 
     xlab = "Black Body Temperature °C", ylab = "Residual", 
     main = "Device ~ BB Residuals", col = "lightseagreen", type = "line", ylim = c(-.5,.5))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col= rgb(.05,.05,.05,alpha=0.1))
points(hik$Blackbody.Temperature,FlrAc$residuals, col = "brown", type = "line") 
points(hik$Blackbody.Temperature,BBAc$residuals, col = "black", type = "line") 
#legend("topleft", legend = c( "Extech Body", "Flir",  "Hikvision"), text.col = c("black","Green","lightseagreen"), bg = "transparent")
abline(h = 0)

#Certify --------------------------------------------------------------------

#Loading Data
cert = read.csv("/Users/zacharysegal/Downloads/Paper Rob 2.0 - Certify (Telpo OEM) Tablet.csv")

#Ensuring all data is in range, adding column for difference between device and BB
cert = cert[cert$Blackbody.Temperature >= 35 & cert$Blackbody.Temperature <= 40,]
cert$Dev.Delta = as.numeric(cert$Device.Measurement) - as.numeric(cert$Blackbody.Temperature)

##Comparing Device Deviation from BB and BB##
#Making a linear model to compare Device Deviation from BB and BB
certAc = lm(cert$Dev.Delta~cert$Blackbody.Temperature); summary(certAc)
# Adjusted R-squared:   0.9113; beta  -0.47065; p-value 1.17e-11 ***
certAc2 = lm(log(cert$Dev.Delta+4)~cert$Blackbody.Temperature); summary(certAc2)
#.9446

##Comparing Device, Extech on Body Mode, and BB##
certBB <- lm(cert$Device.Measurement ~ cert$Blackbody.Temperature); summary(certBB)
#Adjusted R-squared:  0.9286; Beta  0.52935; p-value  1.48e-12 ***

ExBB <- lm(cert$Ex.Body ~ cert$Blackbody.Temperature); summary(ExBB)
#Adjusted R-squared:  0.9739; beta 0.75688; p-value < 2e-16 ***


#Plot 
#side-by-side plots
par(mfrow = c(1,2))

plot(cert$Blackbody.Temperature, cert$Device.Measurement, 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "Certify, Flir, Extech vs Subject Temperature", 
     ylim=c(34.5, 42.5), col = "deeppink", pch =1)
points(cert$Blackbody.Temperature,cert$Ex.Body, col = "black" , pch = 2) 
points(cert$Blackbody.Temperature,cert$FLIR.Measurement, col = "brown", pch = 3) 
legend("topleft",bty = "n", pch = c(2,1,3), legend = c( "Extech Body Adj. R sqrd .97; Beta .76", " Certify Adj. R sqrd .93; Beta .53", "Flir Adj. R sqrd .99; Beta 1"), text.col = c("black","deeppink", "brown"), bg = "transparent")
abline(certBB, col = "deeppink")
abline(ExBB)
abline(lm(cert$FLIR.Measurement ~ cert$Blackbody.Temperature), col = "brown")

plot(cert$Blackbody.Temperature, cert$Dev.Delta, 
     xlab = "Subject Temperature °C", ylab = "Difference", 
     main = "Difference Between Device and Subject\nvs Subject Temperature", 
     ylim=c(-2, 2), col = "deeppink", pch =1)
points(cert$Blackbody.Temperature,c(cert$Ex.Body-cert$Blackbody.Temperature), col = "black", pch = 2) 
points(cert$Blackbody.Temperature,c(cert$FLIR.Measurement-cert$Blackbody.Temperature), col = "brown", pch = 3) 
abline(h = 1)
abline(h = -1)
abline(h = 0)


#ZKTeco --------------------------------------------------------------------

#Loading Data
zk = read.csv("/Users/zacharysegal/Downloads/Paper Rob 2.0 - ZKTeco 8_ Tablet.csv")

#Ensuring all data is in range, adding column for difference between device and BB
zk = zk[zk$Blackbody.Temperature >= 35 & zk$Blackbody.Temperature <= 40,]
zk$Dev.Delta = as.numeric(zk$Device.Measurement) - as.numeric(zk$Blackbody.Temperature)

##Comparing Device Deviation from BB and BB##
#Making a linear model to compare Device Deviation from BB and BB
zkAC = lm(zk$Dev.Delta~zk$Blackbody.Temperature); summary(zkAC)
# Adjusted R-squared:   0.8889; beta  -0.42182; p-value 1.00e-10 ***

##Comparing Device, Extech on Body Mode, and BB##
zkBB <- lm(zk$Device.Measurement ~ zk$Blackbody.Temperature); summary(zkBB)
#Adjusted R-squared:  0.9378; Beta  0.57818; p-value  3.96e-13 ***
zkBB2 <- lm((zk$Device.Measurement) ~ log(zk$Blackbody.Temperature)); summary(zkBB2)
#not better

ExBB <- lm(zk$Ex.Body ~ zk$Blackbody.Temperature); summary(ExBB)
#Adjusted R-squared:  0.9806; beta 0.76545; p-value < 2e-16 ***


#Plot  
#side-by-side plots
par(mfrow = c(1,2))

plot(zk$Blackbody.Temperature, zk$Device.Measurement, 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "ZKTeco, Flir, Extech vs Subject Temperature", 
     ylim=c(34.5, 42.5), col = "darkgoldenrod", pch = 1)
points(zk$Blackbody.Temperature,zk$Ex.Body, col = "black", pch = 2) 
points(zk$Blackbody.Temperature,zk$FLIR.Measurement, col = "brown", pch = 3) 
legend("topleft",bty = "n", pch = c(2,1,3),legend = c( "Extech Body Adj. R sqrd .98; Beta .77", " ZKTeco Adj. R sqrd .94; Beta .58", "Flir Adj. R sqrd .99; Beta 1"), text.col = c("black","darkgoldenrod", "brown"), bg = "transparent")
abline(zkBB, col = "darkgoldenrod")
abline(ExBB)
abline(lm(zk$FLIR.Measurement ~ zk$Blackbody.Temperature), col = "brown")


plot(zk$Blackbody.Temperature, zk$Dev.Delta, 
     xlab = "Subject Temperature °C", ylab = "Difference", 
     main = "Difference Between Device and Subject\nvs Subject Temperature", 
     ylim=c(-1.5, 1.6), col = "darkgoldenrod", pch = 1)
points(zk$Blackbody.Temperature,c(zk$Ex.Body-zk$Blackbody.Temperature), col = "black", pch = 2) 
points(zk$Blackbody.Temperature,c(zk$FLIR.Measurement-zk$Blackbody.Temperature), col = "brown", pch = 3) 
abline(h = 1)
abline(h = -1)
#points(c(36.5,40.5), c(-1,-1), type = "line")
#legend(34.5, -.8,bty = "n", legend = c( "Extech Body", "ZKTeco"), text.col = c("black","darkgoldenrod"), bg = "transparent")
abline(h = 0)

#Meridian 2.0 --------------------------------------------------------------------

#Loading Data
md = read.csv("/Users/zacharysegal/Downloads/meridian.csv")

#Ensuring all data is in range, adding column for difference between device and BB
md = md[md$Blackbody.Temperature >= 35 & md$Blackbody.Temperature <= 40,]
md$Dev.Delta = as.numeric(md$Device.Measurement) - as.numeric(md$Blackbody.Temperature)
md$logdev = log(md$Device.Measurement)
md$logbb = log(md$Blackbody.Temperature)
md$exdev = 2^(md$Device.Measurement)
md$exbb = 2^(md$Blackbody.Temperature)

##Comparing Device Deviation from BB and BB##
#Making a linear model to compare Device Deviation from BB and BB
mdAC = lm(md$Dev.Delta~md$Blackbody.Temperature); summary(mdAC)
# Adjusted R-squared:   0.8158; beta  -0.40104; p-value 1.27e-08 ***
mdlogAC = lm(md$logdev~md$Blackbody.Temperature); summary(mdlogAC)
# Adjusted R-squared:   0.9126; beta  0.015740; p-value 1.02e-11 ***


er = lm(md$Dev.Delta~md$Blackbody.Temperature); summary(er)

##Comparing Device, Extech on Body Mode, and BB##
mdBB <- lm(md$Device.Measurement ~ md$Blackbody.Temperature); summary(mdBB)
#Adjusted R-squared:  0.9086; Beta  0.59896; p-value  1.56e-11 ***



ExBB <- lm(md$Ex.Body ~ md$Blackbody.Temperature); summary(ExBB)
#Adjusted R-squared:  .9814; beta 0.73688; p-value < 2e-16 ***

#restricted to below 36.5
md37 = md[md$Blackbody.Temperature >= 35 & md$Blackbody.Temperature <= 36.25,]

md37BB <- lm(md37$Device.Measurement ~ md37$Blackbody.Temperature); summary(md37BB)
#Adjusted R-squared:  0.3902; beta <.0001; p-value 0.158 
mean(md37$Device.Measurement) #36.9

#Plot  darkgoldenrod
#side-by-side plots
par(mfrow = c(1,2))

plot(md$Blackbody.Temperature, md$Device.Measurement, 
     xlab = "Subject Temperature °C", ylab = "Device Temperature", 
     main = "Meridian, Flir, Extech vs Subject Temperature", 
     ylim=c(34.5, 42.5), col = "orange", pch = 1)
points(md$Blackbody.Temperature,md$Ex.Body, col = "black", pch =2) 
points(md$Blackbody.Temperature,md$FLIR.Measurement, col = "brown", pch = 3) 
legend("topleft",bty = "n", legend = c( "Extech Body Adj. R sqrd .98; Beta .74", " Meridian Adj. R sqrd .91; Beta .6", "Flir Adj. R sqrd .99; Beta 1"), 
      pch = c(2,1,3) ,text.col = c("black","orange", "brown"), bg = "transparent")
abline(mdBB, col = "orange")
abline(ExBB)
abline(lm(md$FLIR.Measurement ~ md$Blackbody.Temperature), col = "brown")


plot(md$Blackbody.Temperature, md$Dev.Delta, 
     xlab = "Subject Temperature °C", ylab = "Difference", 
     main = "Difference Between Device and Subject\nvs Subject Temperature", 
     ylim=c(-1, 2), col = "orange", pch = 1)
points(md$Blackbody.Temperature,c(md$Ex.Body-md$Blackbody.Temperature), col = "black", pch = 2) 
points(md$Blackbody.Temperature,c(md$FLIR.Measurement-md$Blackbody.Temperature), col = "brown", pch = 3) 
abline(h = 1)
abline(h = -1)
abline(h = 0)

# Combined Devices, Controls, Other Figures --------------------------------------------------------------------

# Key --------------------------------------------------------------------
#BB is the Black Body we used as a target for testing
#Ex represents Extech on body mode unless otherwise specified; Black
#Surf represents Extech on Surface Mode
#FLir/flir represents Flir IRT; Brown
#Bems/bems/bem/b all represent the Bems IRT; Purple 
#TVT/tvt/t all represent TVT IRT; Blue
#Dahua/Dah/dah/d represent Dahua IRT; Red
#Cert/cert/c represetns Certify IRT; deeppink
#Zk/zk/Z/z represetns ZKTeco IRT; darkgoldenrod
#Hik/hik/h/H represents Hikvision IRT; lightseagreen
#md/Md/m/m represents Meridian IRT; orange



# NCITs and FLIR ----------------------------------------------------------

#combining data of all 4 sessions including unused Meridian data; 35°C - 40°C
CMBData = data.frame("BB" = bems[bems$Blackbody.Temperature >= 35,]$Blackbody.Temperature,
                     "FlirB" = bems[bems$Blackbody.Temperature >= 35,]$FLIR.Measurement,"ExB" = bems[bems$Blackbody.Temperature >= 35,]$Ex.Body,"SurfB" = bems[bems$Blackbody.Temperature >= 35,]$Ex.Surface,
                     "FlirT" = tvt[tvt$Blackbody.Temperature >= 35,]$FLIR.Measurement,"ExT" = tvt[tvt$Blackbody.Temperature >= 35,]$Ex.Body,"Surft" = tvt[tvt$Blackbody.Temperature >= 35,]$Ex.Surface,
                     "FlirD" = dah[dah$Blackbody.Temperature >= 35 & dah$Blackbody.Temperature <=40,]$FLIR.Measurement,"ExD" = dah[dah$Blackbody.Temperature >= 35 & dah$Blackbody.Temperature <=40,]$Ex.Body,"Surfd" = dah[dah$Blackbody.Temperature >= 35,]$Ex.Surface,
                     "FlirM" = md[md$Blackbody.Temperature >= 35 & md$Blackbody.Temperature <=40,]$FLIR.Measurement,"ExM" = md[md$Blackbody.Temperature >= 35 & md$Blackbody.Temperature <=40,]$Ex.Body, "Surfm" = md[md$Blackbody.Temperature >= 35,]$Ex.Surface,
                     "FlirC" = cert[cert$Blackbody.Temperature >= 35 & cert$Blackbody.Temperature <=40,]$FLIR.Measurement,"ExC" = cert[cert$Blackbody.Temperature >= 35 & cert$Blackbody.Temperature <=40,]$Ex.Body, "Surfc" = cert[cert$Blackbody.Temperature >= 35,]$Ex.Surface,
                     "FlirZ" = zk[zk$Blackbody.Temperature >= 35 & zk$Blackbody.Temperature <=40,]$FLIR.Measurement,"ExZ" = zk[zk$Blackbody.Temperature >= 35 & zk$Blackbody.Temperature <=40,]$Ex.Body, "Surfz" = zk[zk$Blackbody.Temperature >= 35,]$Ex.Surface,
                     "FlirH" = hik[hik$Blackbody.Temperature >= 35 & hik$Blackbody.Temperature <=40,]$FLIR.Measurement,"ExH" = hik[hik$Blackbody.Temperature >= 35 & hik$Blackbody.Temperature <=40,]$Ex.Body, "Surfh" = hik[hik$Blackbody.Temperature >= 35,]$Ex.Surface
                     )

#list of relevant columns for each device
flirs = c("FlirB", "FlirT", "FlirD", "FlirM", "FlirH","FlirZ","FlirC")
Exs = c("ExB","ExT","ExD","ExM","ExH","ExZ","ExC")
Surf = c("SurfB","Surft","Surfd","Surfm", "Surfh","Surfz","Surfc")

#function to find max difference between device and BB temperature
#n = columns to go over, lengthy length of relevant df, frame = df with data, indx = column to compare too - BB in this case
maxy = function(n, lengthy, frame, indx){
  mx = 0
  
  #go over columns
  for (c in n){
    
    #go over every row
    i = 0
    while(i < lengthy){
      i= i + 1 
      dif = (frame[i, indx]-frame[i,c])^2 
      if(mx < dif ){mx = dif}}}
  mx = mx^(.5); print(mx)}


#max difference between device and BB in any round
maxy(n = flirs, lengthy = length(CMBData$BB),frame =  CMBData,indx =  "BB") #.15
maxy(Exs,length(CMBData$BB), CMBData,"BB") #1.8
maxy(Surf,length(CMBData$BB), CMBData,"BB") #.1


#isolating flir, extech body, and extech surface data into individual dfs
cmbflir = CMBData[flirs]
cmbex = CMBData[Exs]
cmsurf = CMBData[Surf]

#inner class dif
cmbflir$max = apply(cmbflir, 1, function(x) diff(range(x))); max(cmbflir$max) #.2
cmbex$max = apply(cmbex, 1, function(x) diff(range(x))); max(cmbex$max) #.6
cmsurf$max = apply(cmsurf, 1, function(x) diff(range(x))); max(cmsurf$max) #.2

#Plot

#graphing 3 plots at once
par(mfrow = c(1,1))

#Extech body
plot(CMBData$BB, CMBData$ExB, pch = 1,col = "purple", cex = 1.2, xlab = "Subject Temperature °C", ylab = "Extech Reading, Body Mode °C", main = "Extech Body")
points(CMBData$BB, CMBData$ExM, pch = 2,col ="orange ")
points(CMBData$BB, CMBData$ExD, pch = 3,col = "red")
points(CMBData$BB, CMBData$ExT, pch = 4,col = "blue")
points(CMBData$BB, CMBData$ExZ, pch = 5,col ="darkgoldenrod ")
points(CMBData$BB, CMBData$ExC, pch = 6,col = "deeppink")
points(CMBData$BB, CMBData$ExH, pch = 7,col = "lightseagreen")
legend(bty = "n", cex = 1.2, x = 34, y = 40.5, legend = c( "Each Color/Shape Corresponds to \na Different Round of Testing"), text.col = c("black","green"), bg = "transparent")
#text(39.1,38.3, "Largest Extra-Round\nDifference .45°C")
abline(coef = c(0,1))

#Extech surf
plot(CMBData$BB, pch = 1,CMBData$SurfB, col = "purple", xlab = "Subject Temperature °C", ylab = "Extech Reading, Surface Mode °C", main = "Extech Surface")
points(CMBData$BB, pch = 2,CMBData$Surfm, col ="orange ")
points(CMBData$BB, pch = 3,CMBData$Surfd, col = "red")
points(CMBData$BB, pch = 4,CMBData$Surft, col = "blue")
points(CMBData$BB, pch = 5,CMBData$Surfz, col ="darkgoldenrod ")
points(CMBData$BB, pch = 6,CMBData$Surfc, col = "deeppink")
points(CMBData$BB, pch = 7,CMBData$Surfh, col = "lightseagreen")
#legend(bty = "n", "topleft", legend = c( "Each Color Corresponds to\na Different Round of Testing"), text.col = c("black","green"), bg = "transparent")
#text(39.1,38, "Largest Extra-Round\nDifference .2°C")
abline(coef = c(0,1))

#Flir Round to Round
plot(CMBData$BB, pch = 1,CMBData$FlirB, col = "purple", xlab = "Subject Temperature °C", 
     ylab = "Flir Reading °C", main = "Flir IRT")
points(CMBData$BB, pch = 2,CMBData$FlirM, col ="orange", pch = ".")
points(CMBData$BB, pch = 3,CMBData$FlirD, col = "red", pch = "x")
points(CMBData$BB, pch = 4,CMBData$FlirT, col = "blue", pch = "f")
points(CMBData$BB, pch = 5,CMBData$FlirZ, col ="darkgoldenrod ")
points(CMBData$BB, pch = 6,CMBData$FlirC, col = "deeppink")
points(CMBData$BB, pch = 7,CMBData$FlirH, col = "lightseagreen")
#legend(bty = "n", "topleft", legend = c( "Each Color/Shape Corresponds to\na Different Round of Testing"), text.col = c("black","green"), bg = "transparent")
#text(38.9,37.6, "Largest Extra-Round\nDifference .2°C")
abline(coef = c(0,1))

# Combine --------------------------------------------------------------------
# Key --------------------------------------------------------------------
#BB is the Black Body we used as a target for testing
#Ex represents Extech on body mode unless otherwise specified; Black
#Surf represents Extech on Surface Mode
#FLir/flir represents Flir IRT; Brown
#Bems/bems/bem/b all represent the Bems IRT; Purple 
#TVT/tvt/t all represent TVT IRT; Blue
#Dahua/Dah/dah/d represent Dahua IRT; Red
#Cert/cert/c represetns Certify IRT; deeppink
#Zk/zk/Z/z represetns ZKTeco IRT; darkgoldenrod
#Hik/hik/h/H represents Hikvision IRT; lightseagreen
#md/Md/m/m represents Meridian IRT; orange

#combine device data
full = data.frame("BB" = bems$Blackbody.Temperature,"bems" = bems$Device.Measurement,
                  "tvt" = tvt$Device.Measurement, "cert" = cert$Device.Measurement, "hik" = hik$Device.Measurement
                , "dah" = dah$Device.Measurement, "zk" = zk$Device.Measurement, "md" = md$Device.Measurement)

#average all the tested temperatures
averaged  = apply(full[,2:7], 1, function(x) mean(x))

#take out the TVT and Bems
averagedwo  = apply(full[,4:7], 1, function(x) mean(x))

#average the controls
exbod = apply(cmbex[,1:7], 1, function(x) mean(x))
exsurf = apply(cmsurf[,1:7], 1, function(x) mean(x))
flrs = apply(cmbflir[,1:7], 1, function(x) mean(x))

#add to combines df
full$averaged = averaged
full$exbod = exbod
full$averagedwo = averagedwo
full$flrs = flrs

#plot data
plot(bems$Blackbody.Temperature, full$flrs, col = "brown" ,type = "l", lty =1, 
     xlab = "Subject Temperature °C", ylab = "Temperature °C", main = "Tested IRTs vs. Subject Temperature",
     xlim =c(35, 40.5))
points(bems$Blackbody.Temperature, bems$Device.Measurement, type = "l", col = "purple", lty = 2)
points(hik$Blackbody.Temperature, hik$Device.Measurement, type = "l", col = "lightseagreen", lty = 3)
points(bems$Blackbody.Temperature, dah$Device.Measurement, type = "l",col = "red", lty = 4)
points(bems$Blackbody.Temperature, zk$Device.Measurement, type = "l", col = "darkgoldenrod", lty = 5)
points(bems$Blackbody.Temperature, tvt$Device.Measurement, type = "l", col = "blue", lty = 6)
points(bems$Blackbody.Temperature, cert$Device.Measurement, type = "l", col = "deeppink", lty = 7)
points(bems$Blackbody.Temperature, md$Device.Measurement, type = "l", col = "orange", lty = 8)
text(x = c(rep(40,8)), 
     y = c(c(full[21,c(12,8,6,5,3,2)])),
     labels = c("Avg. Flir", "Meridian", "Dahua", "Hikvision",  "TVT", "Bems"), 
     col = c("Brown","orange","red", "lightseagreen",  "blue", "Purple"),
     pos = 4) 
text(x = c(rep(40,2)), 
     y = c(full[21,4] - c(-.07,.07)),
     labels = c("ZkTeco", "Certify"), 
     col = c("darkgoldenrod", "deeppink"),
     pos = 4)


plot(full$BB,full$BB,ylim = c(-3.1,2.25), col = "black" ,type = "l",  
     xlab = "Subject Temperature °C", ylab = "Device - Subject", 
     main = "Combined IRTs and Flir Difference from Subject", xlim =c(35, 40.85), lty = 2)
abline(h = -1, col = "black", lty = 1)
abline(h = 1, col = "black", lty = 1)
abline(h = 0, col = "black", lty = 1, cex = 2.1)
points(bems$Blackbody.Temperature, (bems$FLIR.Measurement - bems$Blackbody.Temperature), type = "l", col = "brown", lty = 3)
points(bems$Blackbody.Temperature, (full$exbod - bems$Blackbody.Temperature), type = "l", col = "black", lty = 4)
points(bems$Blackbody.Temperature, bems$Dev.Delta, type = "l", col = "purple", lty = 5)
points(hik$Blackbody.Temperature, hik$Dev.Delta, type = "l", col = "lightseagreen", lty = 6)
points(bems$Blackbody.Temperature, dah$Dev.Delta, type = "l",col = "red", lty = 7)
points(bems$Blackbody.Temperature, zk$Dev.Delta, type = "l", col = "darkgoldenrod", lty = 8)
points(bems$Blackbody.Temperature, tvt$Dev.Delta, type = "l", col = "blue", lty = 9)
points(bems$Blackbody.Temperature, cert$Dev.Delta, type = "l", col = "deeppink", lty = 10)
points(bems$Blackbody.Temperature, md$Dev.Delta, type = "l", col = "orange", lty = 11)
text(x = c(rep(40,8)), 
     y = c(c(full[21,c(12,8,6,5,3,2)]- c(39.94, rep(40,5)))),
     labels = c("Flir", "Meridian", "Dahua", "Hikvision",  "TVT", "Bems"), 
     col = c("Brown","orange","red", "lightseagreen",  "blue", "Purple"),
     pos = 4) 
text(x = c(rep(40,2)), 
     y = c(c(-.92,-1.08)),
     labels = c("ZkTeco", "Certify"), 
     col = c("darkgoldenrod", "deeppink"),
     pos = 4)
text(y = full[21,"exbod"] - 39.7, 
     x = 40,
     labels = c("Extech Body"), 
     col = c("black"),
     pos = 4)


#abs
plot(full$BB,full$BB,ylim = c(0,3.2), col = "black" ,type = "l",  
     xlab = "Subject Temperature °C", ylab = "abs(Device - Subject)", 
     main = "Combined IRTs and Flir Difference from Subject", xlim =c(35, 40.5), lty = 2)
abline(v = 37, col = "black", lty = 1)
points(bems$Blackbody.Temperature, abs(bems$FLIR.Measurement - bems$Blackbody.Temperature), type = "l", col = "brown", lty = 3)
points(bems$Blackbody.Temperature, abs(bems$Dev.Delta), type = "l", col = "purple", lty = 4)
points(hik$Blackbody.Temperature, abs(hik$Dev.Delta), type = "l", col = "lightseagreen", lty = 5)
points(bems$Blackbody.Temperature, abs(dah$Dev.Delta), type = "l",col = "red", lty = 6)
points(bems$Blackbody.Temperature, abs(zk$Dev.Delta), type = "l", col = "darkgoldenrod", lty = 7)
points(bems$Blackbody.Temperature, abs(tvt$Dev.Delta), type = "l", col = "blue", lty = 8)
points(bems$Blackbody.Temperature, abs(cert$Dev.Delta), type = "l", col = "deeppink", lty = 9)
points(bems$Blackbody.Temperature, abs(md$Dev.Delta), type = "l", col = "orange", lty = 10)
points(bems$Blackbody.Temperature, abs(full$exbod-full$BB), type = "l", col = "black", lty = 11)
text(x = c(rep(40,8)), 
     y = c(c(40-full[21,c(12,8,6,5,3,2)])),
     labels = c("Flir", "Meridian", "Dahua", "Hikvision",  "TVT", "Bems"), 
     col = c("Brown","orange","red", "lightseagreen",  "blue", "Purple"),
     pos = 4) 
text(x = c(rep(40,2)), 
     y = c(c(.93,1.07)),
     labels = c("ZkTeco", "Certify"), 
     col = c("darkgoldenrod", "deeppink"),
     pos = 4)
text(y = full[21,"exbod"] - 39.93, 
     x = 40,
     labels = c("Extech Body"), 
     col = c("black"),
     pos = 4)


# Averaged --------------------------------------------------------------------


plot(full$BB, c(averaged-full$BB), col = 2, type = "l", ylim = c(-1.5, 2), xlab = "Subject Temperature °C", ylab = "Device - Subject",
     main = "Difference of Averaged Devices and Extech on Body Mode\nvs. Subject Temperature")
points(full$BB, c(averagedwo-full$BB), col = 3, type = "l", lty = 2)
points(full$BB, c(exbod-full$BB), col = 1, type = "l", lty = 3)
abline(h = 0, lty = 2)
legend(x = 36.5, y = 2.2, text.width = .2,bty = "n", 
       legend = c("Average of Extech on Body Mode", "Average of Tested Devices", "Average of Tested Devices W/O Bems, TVT"),
       text.col = c(1,2,3), y.intersp =.5, cex = .9  )

#abs

plot(full$BB, abs(averaged-full$BB), col = 2, type = "l", ylim = c(0, 2), xlab = "Subject Temperature °C", ylab = "Abs(Device - Subject)",
     main = "Absolute Value of\nAveraged Device Subject Difference")
points(full$BB, abs(averagedwo-full$BB), col = 3, type = "l", lty = 2)
points(full$BB, abs(exbod-full$BB), col = 1, type = "l", lty = 3)
legend(x = 35.5, y = 2, text.width = .2,bty = "n", 
       legend = c("Average of Extech on Body Mode", "Average of Tested Devices", "Average of Tested Devices W/O Bems, TVT"),
       text.col = c(1,2,3), y.intersp =.5, cex = 1.1  )

# Bland-Altman --------------------------------------------------------------------
#A function to make plotting easier
#x is device 1, y is device 2
AltMan = function(x,y,w = "x"){
  bl = blandr.statistics(x, y)
  mn = bl$means
  dif = bl$differences
  
  #the x-axis
  if(w == "x"){ return(mn)}
  
  #the y axis
  else if(w == "y"){return(dif)}
  else{
    #draw the lines to create the plot
    abline(h = 0)
    abline(h = bl$bias, lty = 2)
    abline(h = bl$biasUpperCI, lty = 2)
    abline(h =  bl$biasLowerCI, lty = 2)
    }
}

#Bems
bl = blandr.statistics(full$bems, full$BB)
blm = lm(bl$differences ~ bl$means); summary(blm)$r.squared; blm$coefficients[2]

plot(AltMan(full$bems, full$BB, "x"),AltMan(full$bems, full$BB, "y"),
             main = "Bland-Altman plot of Bems,\nExtech on Body, and Flir vs. Subject Temp",
             col = "purple", ylab = "Differences", xlab = "Means", pch =1)
points(AltMan(full$exbod, full$BB, "x"),AltMan(full$exbod, full$BB, "y"), pch = 2 )
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), col = "brown", pch = 3)
AltMan(full$bems, full$BB, "u")
abline(blm, col = "purple")
text(pos = 3, col = "purple", x = c(bl$means[15]-1.2, bl$means[15] - .95), y  = c(bl$differences[18]+ .81,bl$differences[18] +.81),
     labels = c("R-Squared ","0.99") )
text(pos = 3, col = "purple",x = c(bl$means[15]-1.2, bl$means[15] - .95), y  = c(bl$differences[18]+ .6,bl$differences[18] +.6),
     labels = c("Beta ", "-1.979") )
legend(x = 35.5, y = -1.8, bty = "n", ncol = 1, y.intersp = .5, 
       legend = c("Bems", "Extech", "Flir"), text.col = c("Purple", "Black", "Brown"), pch = c(1,2,3))

#TVT
bl = blandr.statistics(full$tvt, full$BB)
bl$blm = lm(bl$differences ~ bl$means); summary(blm)$r.squared; blm$coefficients[2]

plot(AltMan(full$tvt, full$BB, "x"),AltMan(full$tvt, full$BB, "y"),
     main = "Bland-Altman plot of TVT,\nExtech on Body, and Flir vs. Subject Temp",
     col = "Blue", xlab = "Means", ylab = "Differences", pch = 1)
points(AltMan(full$exbod, full$BB, "x"),AltMan(full$exbod, full$BB, "y"), pch = 2)
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), col = "brown", pch = 3)
AltMan(full$tvt, full$BB, "u")
abline(blm, col = "Blue")
text(pos = 3, col = "Blue", x = c(bl$means[18], bl$means[18] + .4), y  = c(bl$differences[18]+ .8,bl$differences[18] +.8),
     labels = c("R-Squared ","0.92") )
text(pos = 3, col = "Blue",x = c(bl$means[18], bl$means[18] + .4), y  = c(bl$differences[18]+ .6,bl$differences[18] +.6),
     labels = c("Beta ", "-1.242") )
legend(#x = 35.5, y = -2, 
       "bottomleft", bty = "n", ncol = 1, y.intersp = .5, 
       legend = c("TVT", "Extech", "Flir"), text.col = c("Blue", "Black", "Brown"), pch = c(1,2,3))

#Dahua

bl = blandr.statistics(full$dah, full$BB)
blm = lm(bl$differences ~ bl$means); summary(blm)$r.squared; blm$coefficients[2]

plot(AltMan(full$dah, full$BB, "x"),AltMan(full$dah, full$BB, "y"), pch = 1,
     main = "Bland-Altman plot of Dahua,\nExtech on Body, and Flir vs. Subject Temp",
     col = "Red", xlab = "Means", ylab = "Differences", ylim = c(-.6, 1))
points(AltMan(full$exbod, full$BB, "x"),AltMan(full$exbod, full$BB, "y"), pch = 2)
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), col = "brown", pch = 3)
AltMan(full$dah, full$BB, "u")
abline(blm, col = "Red")
text(pos = 3, col = "Red", x = c(bl$means[8], bl$means[8] + .4), y  = c(bl$differences[18]+ .95,bl$differences[18] +.95),
     labels = c("R-Squared ","0.94") )
text(pos = 3, col = "Red",x = c(bl$means[8], bl$means[8] + .4), y  = c(bl$differences[18] + .89,bl$differences[18]+ .89),
     labels = c("Beta ", "-0.256") )
legend(#x = 35.5, y = -2, 
  "bottomleft", bty = "n", ncol = 1, y.intersp = .5, 
  legend = c("Dahua", "Extech", "Flir"), text.col = c("Red", "Black", "Brown"), pch = c(1,2,3))


#Cert/cert/c represetns Certify IRT; deeppink
bl = blandr.statistics(full$cert, full$BB)
blm = lm(bl$differences ~ bl$means); summary(blm)$r.squared; blm$coefficients[2]

plot(AltMan(full$cert, full$BB, "x"),AltMan(full$cert, full$BB, "y"),
     main = "Bland-Altman plot of Certify,\nExtech on Body, and Flir vs. Subject Temp",
     col = "deeppink", xlab = "Means", ylab = "Differences", pch = 1)
points(AltMan(full$exbod, full$BB, "x"),AltMan(full$exbod, full$BB, "y"), pch = 2 )
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), col = "brown", pch = 3)
AltMan(full$cert, full$BB, "u")
abline(blm, col = "deeppink")
text(pos = 3, col = "deeppink", x = c(bl$means[8]+.3 , bl$means[8] + .65), y  = c(bl$differences[18]+ .1,bl$differences[18] +.1),
     labels = c("R-Squared ","0.857") )
text(pos = 3, col = "deeppink",x = c(bl$means[8] +.3 , bl$means[8] + .65), y  = c(bl$differences[18] ,bl$differences[18]),
     labels = c("Beta ", "-0.593") )
legend(#x = 35.5, y = -2, 
  "bottomleft", bty = "n", ncol = 1, y.intersp = 1, x.intersp=.5,
  legend = c("Certify", "Extech", "Flir"), text.col = c("deeppink", "Black", "Brown"), pch = c(1,2,3))


#Zk/zk/Z/z represetns ZKTeco IRT; darkgoldenrod
bl = blandr.statistics(full$zk, full$BB)
blm = lm(bl$differences ~ bl$means); summary(blm)$r.squared; blm$coefficients[2]

plot(AltMan(full$zk, full$BB, "x"),AltMan(full$zk, full$BB, "y"), pch = 1,
     main = "Bland-Altman plot of ZKTeco,\nExtech on Body, and Flir vs. Subject Temp",
     col = "darkgoldenrod", xlab = "Means", ylab = "Differences", )
points(AltMan(full$exbod, full$BB, "x"),AltMan(full$exbod, full$BB, "y"), pch = 2)
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), col = "brown", pch = 3)
AltMan(full$zk, full$BB, "u")
abline(blm, col = "darkgoldenrod")
text(pos = 3, col = "darkgoldenrod", x = c(bl$means[8] +.3, bl$means[8] + .65), y  = c(bl$differences[18]+ .1,bl$differences[18] +.1),
     labels = c("R-Squared ","0.832") )
text(pos = 3, col = "darkgoldenrod",x = c(bl$means[8] +.3, bl$means[8] + .65), y  = c(bl$differences[18] ,bl$differences[18]),
     labels = c("Beta ", "-0.513") )
legend(#x = 35.5, y = -2, 
  "bottomleft", bty = "n", ncol = 1, y.intersp = .5, x.intersp = .5,
  legend = c("ZKTeco", "Extech", "Flir"), text.col = c("darkgoldenrod", "Black", "Brown"), pch = c(1,2,3))

#Hik/hik/h/H represents Hikvision IRT; lightseagreen
bl = blandr.statistics(full$hik, full$BB)
blm = lm(bl$differences ~ bl$means); summary(blm)$r.squared; blm$coefficients[2]

plot(AltMan(full$hik, full$BB, "x"),AltMan(full$hik, full$BB, "y"),
     main = "Bland-Altman plot of Hikvision,\nExtech on Body, and Flir vs. Subject Temp",
     col = "lightseagreen", xlab = "Means", ylab = "Differences", ylim = c(-.6, 1))
points(AltMan(full$exbod, full$BB, "x"),AltMan(full$exbod, full$BB, "y"), pch = 2)
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), col = "brown", pch = 3)
AltMan(full$hik, full$BB, "u")
abline(blm, col = "lightseagreen")
text(pos = 3, col = "lightseagreen", x = c(bl$means[8], bl$means[8] + .4), y  = c(bl$differences[18]+ .55,bl$differences[18] +.55),
     labels = c("R-Squared ","0.86") )
text(pos = 3, col = "lightseagreen",x = c(bl$means[8], bl$means[8] + .4), y  = c(bl$differences[18] + .49,bl$differences[18]+ .49),
     labels = c("Beta ", "-0.208") )
legend(#x = 35.5, y = -2, 
  "bottomleft", bty = "n", ncol = 1, y.intersp = .5, x.intersp = .5,
  legend = c("Hikvision", "Extech", "Flir"), text.col = c("lightseagreen", "Black", "Brown"), pch =c(1,2,3))



#md/Md/m/m represents Meridian IRT; orange
bl = blandr.statistics(full$md, full$BB)
blm = lm(bl$differences ~ bl$means); summary(blm)$r.squared; blm$coefficients[2]

plot(AltMan(full$md, full$BB, "x"),AltMan(full$zk, full$BB, "y"),
     main = "Bland-Altman plot of Meridian,\nExtech on Body, and Flir vs. Subject Temp",
     col = "orange", xlab = "Means", ylab = "Differences", pch = 1)
points(AltMan(full$exbod, full$BB, "x"),AltMan(full$exbod, full$BB, "y"),pch = 2 )
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), col = "brown", pch = 3)
AltMan(full$md, full$BB, "u")
abline(blm, col = "orange")
text(pos = 3, col = "orange", x = c(bl$means[8] +.4, bl$means[8] + .8), y  = c(bl$differences[18]-.6,bl$differences[18] - .6),
     labels = c("R-Squared ","0.73") )
text(pos = 3, col = "orange",x = c(bl$means[8]+.4, bl$means[8] + .8), y  = c(bl$differences[18] - .7,bl$differences[18]- .7),
     labels = c("Beta ", "-0.469") )
legend(#x = 35.5, y = -2, 
  "bottomleft", bty = "n", ncol = 1, y.intersp = .5, x.intersp = .5,
  legend = c("Meridian", "Extech", "Flir"), text.col = c("orange", "Black", "Brown"), pch = c(1,2,3))

#Combined
plot(full$BB,full$BB,ylim = c(-3.1,2), col = "black" ,type = "l",  
     xlab = "Means°C", ylab = "Differences°C", 
     main = "Combined Bland-Altman Plot", xlim =c(35.5, 40.56))
abline(h = -1, col = "black", lty = 2)
abline(h = 1, col = "black", lty = 2)
abline(h = 0, col = "black", lty = 1, cex = 2.1)
points(AltMan(full$flrs, full$BB, "x"),AltMan(full$flrs, full$BB, "y"), type = "p", col = "brown", pch = 2)
points(AltMan(full$bems, full$BB, "x"),AltMan(full$bems, full$BB, "y"), type = "p", col = "purple", pch = 3)
points(AltMan(full$hik, full$BB, "x"),AltMan(full$hik, full$BB, "y"), type = "p", col = "lightseagreen", pch = 4)
points(AltMan(full$dah, full$BB, "x"),AltMan(full$dah, full$BB, "y"), type = "p",col = "red", pch = 5)
points(AltMan(full$zk, full$BB, "x"),AltMan(full$zk, full$BB, "y"), type = "p", col = "darkgoldenrod", pch = 6)
points(AltMan(full$tvt, full$BB, "x"),AltMan(full$tvt, full$BB, "y"), type = "p", col = "blue", pch = 7)
points(AltMan(full$cert, full$BB, "x"),AltMan(full$cert, full$BB, "y"), type = "p", col = "deeppink", pch = 8)
points(AltMan(full$md, full$BB, "x"),AltMan(full$md, full$BB, "y"), type = "p", col = "orange", pch = 9)
text(x = c((full[21,c(12,8,6,5,4,3,2)] +rep(40,7))/2), 
     y = c(c(full[21,c(12,8,6,5,4,3,2)]- c(rep(40,7)))),
     labels = c("Flir", "Meridian", "Dahua", "Hikvision", "Certify", "TVT", "Bems"), 
     col = c("Brown","orange","red", "lightseagreen", "deeppink", "blue", "Purple"),
     pos = 4) 
text(x = c(full[21,7]/2 + 20), 
     y = c(full[21,7] - 39.82),
     labels = c("ZkTeco"), 
     col = c("darkgoldenrod"),
     pos = 4)


# Variances --------------------------------------------------------------------
#analyzing basic statistics of devices

var(hik$Blackbody.Temperature) #2.40625 
var(hik$FLIR.Measurement) #2.389619
var(c(hik$Blackbody.Temperature + rnorm(length(hik$Blackbody.Temperature)))) #3.312254
var(hik$Ex.Surface) #2.393869
var(hik$Ex.Body) #1.303405
var(hik$Device.Measurement) #1.585619
var(dah$Device.Measurement) #1.428571
var(zk$Device.Measurement) #0.8549048
var(cert$Device.Measurement) #0.7233333
var(md$Device.Measurement) #0.9453333
var(bems$Device.Measurement) #0.005142857
var(tvt$Device.Measurement) #0.1602857


mean(hik$Blackbody.Temperature) #37.5 
mean(hik$FLIR.Measurement) #37.51905
mean(c(hik$Blackbody.Temperature + rnorm(length(hik$Blackbody.Temperature)))) #37.46232
mean(hik$Ex.Surface) #37.50238
mean(hik$Ex.Body) #38.22619
mean(hik$Device.Measurement) #37.38095
mean(dah$Device.Measurement) #37.34286
mean(zk$Device.Measurement) #37.32381
mean(cert$Device.Measurement) #37.33333
mean(md$Device.Measurement) #37.83333
mean(bems$Device.Measurement) #36.67143
mean(tvt$Device.Measurement) #36.91429

range(hik$Blackbody.Temperature) #35 40 
range(hik$FLIR.Measurement) #35 40
range(c(hik$Blackbody.Temperature + rnorm(length(hik$Blackbody.Temperature)))) #34.43728 40.74725
range(hik$Ex.Surface) #35.05 40.05
range(hik$Ex.Body) #36.7 40.2
range(hik$Device.Measurement) #35.6 39.6
range(dah$Device.Measurement) #35.4 39.4
range(zk$Device.Measurement) #36.3 39.0
range(cert$Device.Measurement) #36.4 39.0
range(md$Device.Measurement) #36.9 39.8
range(bems$Device.Measurement) #36.5 36.8
range(tvt$Device.Measurement) #36.6 37.8

# Looking for similar adjustment processes --------------------------------------------------------------------

# ZK/Cert/Meridian --------------------------------------------------------------------

#I am first combining data with a dummy variable for device, 
#then adjusting the devices with an offset, then seeing if 
#the dummy variable is significant

#ZK and Cert
d1 =  data.frame("dev" = zk$Device.Measurement, "BB" =zk$Blackbody.Temperature, "rand" = 1)
d2 = data.frame("dev" = cert$Device.Measurement, "BB" =cert$Blackbody.Temperature, "rand" = 0)
big = rbind(d1, d2)

#adjust
d1$dev = d1$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3]; lm(big$dev~  big$BB + big$rand)$coefficients[3]
#-0.00952381 

#recombine
big = rbind(d1, d2)
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 0.302; big$BB:big$rand  beta 0.04883

#ZK and Meridian
d1 =  data.frame("dev" = zk$Device.Measurement, "BB" =zk$Blackbody.Temperature, "rand" = 1)
d2 = data.frame("dev" = md$Device.Measurement, "BB" = md$Blackbody.Temperature, "rand" = 0)
big = rbind(d1, d2)

#adjust
d1$dev = d1$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3];lm(big$dev~  big$BB + big$rand)$coefficients[3]
#-0.5095238 

#recombine
big = rbind(d1, d2)
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 0.702; big$BB:big$rand  beta -0.02078

#Cert and Meridian
d1 =  data.frame("dev" = cert$Device.Measurement, "BB" =cert$Blackbody.Temperature, "rand" = 1)
d2 = data.frame("dev" = md$Device.Measurement, "BB" = md$Blackbody.Temperature, "rand" = 0)
big = rbind(d1, d2)

#adjust
d1$dev = d1$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3]; lm(big$dev~  big$BB + big$rand)$coefficients[3]
#-0.5

#recombine
big = rbind(d1, d2)
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 0.202; big$BB:big$rand  beta -0.06961

#plot
d3= data.frame("dev" = zk$Device.Measurement, "BB" = zk$Blackbody.Temperature, "rand" = 1)
big = rbind(d3, d2)
#adjust
d3$dev = d3$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3]; lm(big$dev~  big$BB + big$rand)$coefficients[3]
#-0.5095238 


plot(d1$BB, d1$dev, col = 1, ylab = "Adjusted Device Reading",
     xlab = "Black Body Temperature °C", main = "ZKTeco, Certify, and Meridian With Offsets")
points(d1$BB, d1$dev, col = 1, type = "l")
points(d2$BB, d2$dev, col = 2)
points(d3$BB, d3$dev, col = 3)
points(d2$BB, d2$dev, col = 2, type = "l")
points(d3$BB, d3$dev, col = 3, type = "l")
legend("topleft",bty = "n", legend = "At the 10% level\nwe fail to reject\nthe null hypothesis that\nthese devices are different")


# Hik and Dah --------------------------------------------------------------------
#Hikvision and Dahua
d1 =  data.frame("dev" = hik$Device.Measurement, "BB" =hik$Blackbody.Temperature, "rand" = 1)
d2 = data.frame("dev" = dah$Device.Measurement, "BB" =dah$Blackbody.Temperature, "rand" = 0)
big = rbind(d1, d2)

#adjust
d1$dev = d1$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3]; lm(big$dev~  big$BB + big$rand)$coefficients[3]
#0.03809524 

#unadj
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 0.0489 *; big$BB:big$rand  beta 0.04000

#recombine
big = rbind(d1, d2)
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 0.0489 *; big$BB:big$rand  beta 0.04000

plot(d1$BB, d1$dev, col = 1, ylab = "Adjusted Device Reading",
     xlab = "Black Body Temperature °C", main = "Hikvision and Dahua With Offsets")
points(d1$BB, d1$dev, col = 1, type = "l")
points(d2$BB, d2$dev, col = 2)
points(d2$BB, d2$dev, col = 2, type = "l")
legend("topleft",bty = "n", legend = "P-Value of .0489\nCorrelation Coefficient beta of .04")


# TVT and Bems --------------------------------------------------------------------

d1 =  data.frame("dev" = tvt$Device.Measurement, "BB" =tvt$Blackbody.Temperature, "rand" = 1)
d2 = data.frame("dev" = bems$Device.Measurement, "BB" =bems$Blackbody.Temperature, "rand" = 0)
big = rbind(d1, d2)

#adjust
d1$dev = d1$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3]; lm(big$dev~  big$BB + big$rand)$coefficients[3]
#0.2428571 

#recombine
big = rbind(d1, d2)
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 1.39e-07 ***; big$BB:big$rand  beta 0.215065

plot(d1$BB, d1$dev, col = 1, ylab = "Adjusted Device Reading",
     xlab = "Black Body Temperature °C", main = "Hikvision and Dahua With Offsets")
points(d1$BB, d1$dev, col = 1, type = "l")
points(d2$BB, d2$dev, col = 2)
points(d2$BB, d2$dev, col = 2, type = "l")

# Extech Bullets Different --------------------------------------------------------------------

d1 =  data.frame("dev" = hik$Device.Measurement, "BB" =hik$Blackbody.Temperature, "rand" = 1)
d2 = data.frame("dev" = hik$Ex.Body, "BB" =hik$Blackbody.Temperature, "rand" = 0)
big = rbind(d1, d2)

#adjust
d1$dev = d1$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3]; lm(big$dev~  big$BB + big$rand)$coefficients[3]
#-0.8452381  

#recombine
big = rbind(d1, d2)
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 0.00216 **; big$BB:big$rand  beta 0.02328

d1 =  data.frame("dev" = dah$Device.Measurement, "BB" =dah$Blackbody.Temperature, "rand" = 1)
d2 = data.frame("dev" = dah$Ex.Body, "BB" =dah$Blackbody.Temperature, "rand" = 0)
big = rbind(d1, d2)

#adjust
d1$dev = d1$dev - lm(big$dev~  big$BB + big$rand)$coefficients[3]; lm(big$dev~  big$BB + big$rand)$coefficients[3]
#-1.028571  

#recombine
big = rbind(d1, d2)
big = big[big$BB<36.5,]
l = lm(big$dev~  big$BB + big$rand + big$BB*big$rand); summary(l)
#p-value 0.00216 **; big$BB:big$rand  beta 0.02328

plot(d1$BB, d1$dev, col = 1, ylab = "Adjusted Device Reading",
     xlab = "Black Body Temperature °C", main = "Hikvision and Dahua With Offsets")
points(d1$BB, d1$dev, col = 1, type = "l")
points(d2$BB, d2$dev, col = 2)
points(d2$BB, d2$dev, col = 2, type = "l")



