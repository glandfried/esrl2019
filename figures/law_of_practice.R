######################################################################
# Header
oldpar <- par(no.readonly = TRUE)
oldwd <- getwd()
this.dir <- dirname(parent.frame(2)$ofile)
nombre.R <-  sys.frame(1)$ofile
require(tools)
nombre <- print(file_path_sans_ext(nombre.R))
setwd(this.dir)
#setwd("~/gaming/trabajos/conquerClub/SociosPorConveniencia/Imagenes")
pdf(paste0(nombre,".pdf"))
# Header
######################################################################

par(mar=c(3.75,3.75,.33,.33))



ls <- list()
ls[["ls1000"]] <- read.csv("datos/law_of_practice/learningskill_activos1024_.csv", header =T)
ls[["ls500"]] <- read.csv("datos/law_of_practice/learningskill_activos512_1024.csv", header =T)
ls[["ls200"]] <- read.csv("datos/law_of_practice/learningskill_activos256_512.csv", header =T)
ls[["ls100"]] <- read.csv("datos/law_of_practice/learningskill_activos128_256.csv", header =T)
ls[["ls70"]] <- read.csv("datos/law_of_practice/learningskill_activos64_128.csv", header =T)
ls[["ls40"]] <- read.csv("datos/law_of_practice/learningskill_activos32_64.csv", header =T)
ls[["ls20"]] <- read.csv("datos/law_of_practice/learningskill_activos16_32.csv", header =T)
ls[["ls10"]] <- read.csv("datos/law_of_practice/learningskill_activos8_16.csv", header =T)

#ls_all <- read.csv("datos/learningskill.csv", header =T)


grilla <- rev(2^seq(3,3+7))

ylim = c(min(log(ls[["ls10"]][,"avg"],10)),max(log(ls[["ls1000"]][,"avg"],10)))
xlim=c(0,3)
col = seq(0,0.8,length.out = 8)
colores <- matrix(NA,nrow = 8,ncol=1)
colores[1] = rgb(col[1],col[1],col[1])
plot(log(ls[["ls1000"]][,"rn"],10),log(ls[["ls1000"]][,"avg"],10),lwd=1,ylim=ylim,xlim=xlim,
     type="l",axes=F,ylab="",xlab="",col=colores[1])
c<-1
for (i in grilla[-1]){
  c<-c+1
  colores[c] = rgb(col[c],col[c],col[c])
  str <- paste0("ls",toString(i))
  lines(log(ls[[c]][,"rn"],10),log(ls[[c]][,"avg"],10),lwd=1,
        col=colores[c])
}

#lines(log(seq(1,1024),10),log(ls_all[,"avg"][1:1024],10),lwd=1, lty=3)

at.x <- log(outer(1:9, 10^(0:3)),10)
lab.x <- ifelse(at.x %% 1 == 0, at.x, NA)[1,]
lab.x <- lab.x[lab.x>=0& lab.x<=3]
labels.x <- sapply(lab.x,function(i)
  as.expression(bquote(10^ .(i)))
)
at.x <- c(at.x[-1,])[c(at.x[-1,])<=3 & c(at.x[-1,])>=0 ]

axis(lwd=0,lwd.ticks=1,side=1, at=at.x,labels=NA,cex.axis=0.6,tck=0.0133)
axis(lwd=0,lwd.ticks=1,side=1, at=lab.x, labels=NA,cex.axis=0.6,tck=0.03,col=1)
axis(lwd=0,side=1, at=lab.x, labels=labels.x,las=1,cex.lab=0.6,tck=0.08,line=-0.5
     ,cex.axis=1.33)


lab.y<-axis(lwd=0,lwd.ticks=0,labels=NA, side=2,cex.axis=0.6,tck=0.03) 

labels.y <- round(10^lab.y,2)

axis(lwd=0,lwd.ticks=1,labels=NA, side=2,cex.axis=0.6,tck=0.03) 
axis(lwd=0,side=2, at=lab.y, labels=labels.y,cex.lab=0.6,tck=0.08,line=-0.5,cex.axis=1.33)

axis(side=2, labels=NA,cex.axis=0.6,tck=0.0)
axis(side=1, labels=NA,cex.axis=0.6,tck=0.0)

mtext(side=1,"Games played", line=2,cex=2)
mtext(side=2,"Skill", line=2,cex=2)

legend(1.1,ylim[2] - (ylim[2]-ylim[1])*0.7,ncol=2,
       title = "Groups by maximum activity reached",
       c("1024","512","256","128"
         ,"64","32","16","8"),
       lwd=c(8),col=colores, cex=1.25, box.lty=0, bg="transparent")


####################################
# END
dev.off()
setwd(oldwd)
par(oldpar, new=F)
##############################333