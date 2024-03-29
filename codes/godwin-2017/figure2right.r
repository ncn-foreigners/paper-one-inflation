# The main program -------------------------------------------------------------
# The following code takes simulated power from Table A13 and Table A14 and
# produces Figure 2 (right) in the main paper. The files "powerztnbl1a5.csv" and
# "powerztnbl1a2.csv", derived from Table A13 and Table A14, must be in the 
# working directory. A graphic file is created in the working directory, named
# "powerztnb.png".

png(file="powerztnb.png", width=800, height=800)

rangex <- c(0,.3)
rangey <- c(0,1)

xs <- c(0,0.05,0.1,0.15,0.2,0.25,0.3)

labelsize <- 2.5
psize <- 2
lsize <- 2

par("mar"=c(4,5,0,0))

ztnb100 <- matrix(c(xs,0.067,read.csv('data/powerztnbl1a5.csv',header=FALSE)[,1]),,2)
ztnb250 <- matrix(c(xs,0.055,read.csv('data/powerztnbl1a5.csv',header=FALSE)[,2]),,2)
ztnb500 <- matrix(c(xs,0.056,read.csv('data/powerztnbl1a5.csv',header=FALSE)[,3]),,2)
ztnb1000 <-matrix(c(xs,0.053,read.csv('data/powerztnbl1a5.csv',header=FALSE)[,4]),,2)
# The main program -------------------------------------------------------------
plot(ztnb100, type="p", col=1, pch=1, ylab="power", xlab=expression(omega), 
     xlim=rangex, ylim=rangey,cex.lab=labelsize,cex.axis=labelsize,cex=psize)
lines(ztnb250, type="p", col=1, pch=2, xlim=rangex, ylim=rangey,cex=psize)
lines(ztnb500, type="p", col=1, pch=0, xlim=rangex, ylim=rangey,cex=psize)
lines(ztnb1000, type="p", col=1, pch=3, xlim=rangex, ylim=rangey,cex=psize)
lines(ztnb100, type="l", col=1, pch=1, xlim=rangex, ylim=rangey,lwd=lsize)
lines(ztnb250, type="l", col=1, pch=2, xlim=rangex, ylim=rangey,lwd=lsize)
lines(ztnb500, type="l", col=1, pch=0, xlim=rangex, ylim=rangey,lwd=lsize)
lines(ztnb1000, type="l", col=1, pch=3, xlim=rangex, ylim=rangey,lwd=lsize)

ztnb100 <- matrix(c(xs,0.040,read.csv('data/powerztnbl1a2.csv',header=FALSE)[,1]),,2)
ztnb250 <- matrix(c(xs,0.051,read.csv('data/powerztnbl1a2.csv',header=FALSE)[,2]),,2)
ztnb500 <- matrix(c(xs,0.057,read.csv('data/powerztnbl1a2.csv',header=FALSE)[,3]),,2)
ztnb1000 <-matrix(c(xs,0.058,read.csv('data/powerztnbl1a2.csv',header=FALSE)[,4]),,2)

lines(ztnb100, type="p", col=1, pch=1, xlim=rangex, ylim=rangey,cex=psize)
lines(ztnb250, type="p", col=1, pch=2, xlim=rangex, ylim=rangey,cex=psize)
lines(ztnb500, type="p", col=1, pch=0, xlim=rangex, ylim=rangey,cex=psize)
lines(ztnb1000, type="p", col=1, pch=3, xlim=rangex, ylim=rangey,cex=psize)
lines(ztnb100, type="l",col=1,pch=1,lty = 2, xlim=rangex, ylim=rangey,lwd=lsize)
lines(ztnb250, type="l",col=1,pch=2,lty = 2, xlim=rangex, ylim=rangey,lwd=lsize)
lines(ztnb500, type="l",col=1,pch=0,lty = 2, xlim=rangex, ylim=rangey,lwd=lsize)
lines(ztnb1000, type="l",col=1,pch=3,lty = 2,xlim=rangex, ylim=rangey,lwd=lsize)

legend("topleft", c("n = 100","n = 250","n = 500","n = 1000"), pch=c(1,2,0,3), 
       col=1, inset = .02, cex=2.5)
legend(x=-0.005,y=.6,c(expression(paste(alpha," = .5")),expression(paste(alpha,
       " = 2"))), lty=c(1,2), lwd= lsize, col=1, inset = .1, cex=labelsize)

dev.off()