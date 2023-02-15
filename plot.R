dat <- read.table(file="test.txt",header=T)

#head(dat)

x <- dat$Dist
y <- dat$DAC

#plot(dat$Dist,dat$Dac)

pdf("test.pdf", height=8, width=8)

# plot(x, y, xlim=range(x), ylim=range(y), xlab="Dist", ylab="DAC", main = "Distance-DAC",pch=16)
# lines(x[order(x)], y[order(x)], xlim=range(x), ylim=range(y), pch=16)

plot(x, y, xlim=range(x), ylim=c(1,400), xlab="Dist", ylab="DAC", main = "Distance-DAC",pch=1)
lines(x[order(x)], y[order(x)], xlim=range(x), ylim=c(1,400), pch=16, col="red")

dev.off()
