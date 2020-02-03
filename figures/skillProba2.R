oldpar <- par(no.readonly = TRUE)
oldwd <- getwd()
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
nombre.R <-  sys.frame(1)$ofile
require(tools)
nombre <- print(file_path_sans_ext(nombre.R))
pdf(paste0(nombre,".pdf"))

#setwd("~/gaming/trabajos/conquerClub/db/conquerSkill/Imagenes")
#par(mar=c(3.75,3.75,0.33,0.33))

a <- read.csv("datos/skillProba2.csv")
a <- a[order(a$dif),] 
a <- a[a$count>35,] 

side <- (1+sign(a[,1]*10+1))/2
a <- cbind(a,side*a[,2]*a[,3]+(1-side)*(a[,3]-a[,2]*a[,3]) )

b <- matrix(NA,ncol=4,nrow = 21)
for(i in seq(0,20)){#i=1
  b[i+1,1] <- i
  b[i+1,2] <- sum(a[abs(a[,1])==i,3])
  b[i+1,3] <- sum(a[abs(a[,1])==i,4])
  b[i+1,4] <- b[i+1,3]/b[i+1,2]
}


p <-b[seq(1,9),4]
x <- b[seq(1,9),1]
p[1] <- 0.4925
plot(x,p, ylab="", xlab="", pch=1, axes=F,ylim=c(0.48,0.81),cex=2)
points(4,p[5],pch=19,cex=2)
mtext("Skill difference", line=2.5, side=1,cex=2.5)
mtext("Probability of win", line=2.25, side=2,cex=2.5)

axis(lwd=1,lwd.ticks=1,side=2, labels=NA,cex.axis=0.6,tck=0.02)
at.y = c(0.5,0.6,0.8)
axis(lwd=0,at=at.y,side=2,cex.axis=2,line=-0.66)
axis(lwd=0,at=p[5],labels=round(p[5],2), lwd.ticks=0,side=2,cex.axis=2,line=-0.75)
axis(lwd=1,lwd.ticks=1,side=1, labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,line=-0.25,cex.axis=2) 
#axis(lwd=0,at=4,labels=4, lwd.ticks=0,side=1,cex.axis=1.5,line=-0.8,las=1)

abline(v=4,lty=3)
abline(h=p[5],lty=3)


if(F){
  for(n_desvios in seq(2,4)){
    sup <- a$avg + n_desvios* sqrt((1/a$count)*a$avg*(1-a$avg))
    inf <- a$avg - n_desvios* sqrt((1/a$count)*a$avg*(1-a$avg))
    
    polygon(c(x,rev(x)), c(sup,rev(inf)),col = rgb(0,0,0,0.3),border = F)
    
  }
}


model <- cbind(b[,1],b[,4])
colnames(model) <- c("ts","p")
model <- as.data.frame(model)
fit <-glm(model[,2]~model[,1],family=quasibinomial,weights = b[,2])

beta<-coef(fit)
lines(x,exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x)),col=1,lwd=2)




##polygon(c(x.na.omit,rev(x.na.omit)),
##        c(exp(beta.ci[2,1]+beta.ci[2,2]*x.na.omit)/(1+exp(beta.ci[2,1]+beta.ci[2,2]*x.na.omit))
##          ,rev(exp(beta.ci[1,1]+beta.ci[2,1]*x.na.omit)/(1+exp(beta.ci[1,1]+beta.ci[2,1]*x.na.omit))) )
##        , col=rgb(0,0,0,0.2) , border=F)


#x <- seq(0,10)
#y.enX <- exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x))
#plot(x,exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x)))




######333


######3



############################################3
dev.off()
system(paste("pdfcrop -m '0 0 0 0'",paste0(nombre,".pdf") ,paste0(nombre,".pdf")))
setwd(oldwd)
par(oldpar, new=F)
