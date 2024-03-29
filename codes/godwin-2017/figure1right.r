# The main program -------------------------------------------------------------
# The following code takes the Prob(type I error) from Table A8 and produces
# Figure 1 (right) in the main paper. The file "sizeztnb.csv", derived from 
# Table A8, must be in the working directory. This code produces a graphic file 
# "sizeztnb.png" in the working directory. 

getsize <- as.matrix(read.csv("data/sizeztnb.csv"))
png(file="sizeztnb.png", width=800, height=1000)

yvals <- c(0.95,0.92,.89,.86,.79,.76,.73,.7,.63,.6,.57,.54,.47,.44,.41,.38,.31,
           .28,.25,.22,.15,.12,.09,.06)
s01 <- matrix(c(getsize[,1],yvals),24,2)
s05 <- matrix(c(getsize[,2],yvals),24,2)
s10 <- matrix(c(getsize[,3],yvals),24,2)
s01[,1] <- 15 * (s01[,1] - .01) + .2
s05[,1] <- 3 * (s05[,1] - .05) + .5
s10[,1] <- 1.5 * (s10[,1] - .1) + .8
y <- rep(c(0.5,1,2,4),6)
rangex <- c(0,1)
rangey <- c(0,1)
par("oma" = c(2,2,0,0))
par("mar" = c(7,5,0,0))

line1 <- line1l <- line1r <- line2 <- line2l <- line2r <- line3 <- 
         line3l <- line3r <- matrix(ncol = 2, nrow = 2)

line1[1,1] <- 0.2
line1[1,2] <- 0
line1[2,1] <- 0.2
line1[2,2] <- 1

line1l[1,1] <- 0.125
line1l[1,2] <- 0
line1l[2,1] <- 0.125
line1l[2,2] <- 1

line1r[1,1] <- 0.275
line1r[1,2] <- 0
line1r[2,1] <- 0.275
line1r[2,2] <- 1

line2[1,1] <- 0.5
line2[1,2] <- 0
line2[2,1] <- 0.5
line2[2,2] <- 1

line2l[1,1] <- 0.425
line2l[1,2] <- 0
line2l[2,1] <- 0.425
line2l[2,2] <- 1

line2r[1,1] <- 0.575
line2r[1,2] <- 0
line2r[2,1] <- 0.575
line2r[2,2] <- 1

line3[1,1] <- 0.8
line3[1,2] <- 0
line3[2,1] <- 0.8
line3[2,2] <- 1

line3l[1,1] <- 0.725
line3l[1,2] <- 0
line3l[2,1] <- 0.725
line3l[2,2] <- 1

line3r[1,1] <- 0.875
line3r[1,2] <- 0
line3r[2,1] <- 0.875
line3r[2,2] <- 1

psize <- 2

plot(line1, type="l", col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)
plot(line1l, type="l", lty=3, col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)
plot(line1r, type="l", lty=3, col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)

plot(line2, type="l", col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)
plot(line2l, type="l", lty=3, col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)
plot(line2r, type="l", lty=3, col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)

plot(line3, type="l", col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)
plot(line3l, type="l", lty=3, col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)
plot(line3r, type="l", lty=3, col = 1, yaxt="n",xaxt="n",xlim=rangex,ylim=rangey,ylab="", xlab="")
par(new=TRUE)

for(ii in 1:24){
  plot(t(as.matrix(s01[ii,])),type="p", col=1, yaxt="n", xaxt="n",pch=16, xlim=rangex,ylim=rangey,ylab="", xlab="", cex=psize)
  par(new=TRUE)
}

for(ii in 1:24){
  plot(t(as.matrix(s05[ii,])),type="p", col=1, yaxt="n", xaxt="n",pch=16, xlim=rangex,ylim=rangey,ylab="", xlab="", cex=psize)
  par(new=TRUE)
}

for(ii in 1:24){
  plot(t(as.matrix(s10[ii,])),type="p", col=1, yaxt="n", xaxt="n",pch=16, xlim=rangex,ylim=rangey,ylab="", xlab="", cex=psize)
  par(new=TRUE)
}

ceaxis <- 2.3

axis( 2, at=c(.95,0.92,.89,.86,.79,.76,.73,.7,.63,.6,.57,.54,.47,.44,.41,.38,.31,.28,.25,.22,.15,.12,.09,.06), labels=rep(c("1,.5","1, 2","2,.5","2, 2"),6), las=2, cex.axis= ceaxis )
axis( 2, at=1, labels=expression(paste("n  ",lambda,", ",alpha)),las=2,cex.axis=ceaxis)
axis( 1, at=c(.125,.275,.425,.575,.725,.875), labels=c(0.005,.015,.025,.075,.05,.15),las=2,cex.axis=ceaxis)
axis( 1, at=c(.2,.5,.8), labels=c(.010,.05,.1),font=2,las=2,cex.axis=ceaxis)

mtext(c("50","100","250","500","1000","2000"), side =2, outer = TRUE, at=c(0.888,.755,.625,.489,.358,.225),cex=ceaxis)
mtext("Empirical P(Type I Error)", side=1, outer=TRUE, at=0.55,cex=ceaxis)

dev.off()