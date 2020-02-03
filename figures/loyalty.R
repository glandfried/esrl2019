oldpar <- par(no.readonly = TRUE)
oldwd <- getwd()
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
nombre.R <-  sys.frame(1)$ofile
require(tools)
nombre <- print(file_path_sans_ext(nombre.R))
pdf(paste0(nombre,".pdf"),width = 8, height = 5 )
#setwd("~/gaming/trabajos/conquerClub/SociosPorConveniencia/Imagenes")

par(mar=c(3.75,3.75,.33,.33))

alpha = 0.05

##############################
#
#lst89_summary <- matrix(NA ,nrow=500,ncol=7)
#colnames(lst89_summary) <- c("mean","t_ic_i","t_ic_s","median","w_ic_i","w_ic_s","count")
#lst01_summary <- matrix(NA ,nrow=500,ncol=7)
#colnames(lst01_summary) <- c("mean","t_ic_i","t_ic_s","median","w_ic_i","w_ic_s","count")
#for (l in seq(500)){
#  f89_i <- paste0(paste0("datos/loyalty/learningskill_pteam89_ployalSup_hasta4team_",toString(l)),".csv")
#  f01_i <- paste0(paste0("datos/loyalty/learningskill_pteam89_ployalInf_hasta4team_",toString(l)),".csv")
#  ls89_i <- na.omit(read.csv(f89_i)[,1])
#  ls01_i <- na.omit(read.csv(f01_i)[,1])
#  write.csv(ls89_i,paste0(paste0("datos/loyalty/published/learningskill_pteam89_ployalSup_hasta4team_",toString(l)),".csv"), row.names=F)
#  write.csv(ls01_i,paste0(paste0("datos/loyalty/published/learningskill_pteam89_ployalInf_hasta4team_",toString(l)),".csv"), row.names=F)
#  w89 <- wilcox.test(ls89_i,conf.int = T)
#  w01 <- wilcox.test(ls01_i,conf.int = T)
#  t89 <- t.test(ls89_i)
#  t01 <- t.test(ls01_i)
#  lst89_summary[l,] <- c(t89$estimate,t89$conf.int[1],t89$conf.int[2],w89$estimate,w89$conf.int[1],w89$conf.int[2],length(ls89_i))
#  lst01_summary[l,] <- c(t01$estimate,t01$conf.int[1],t01$conf.int[2],w01$estimate,w01$conf.int[1],w01$conf.int[2],length(ls01_i))
#}
#write.csv(lst89_summary,"datos/loyalty/lstSup_summary.csv", row.names=F)
#write.csv(lst01_summary,"datos/loyalty/lstInf_summary.csv", row.names=F)

#############################


lst89_summary <- read.csv("datos/loyalty/lstSup_summary.csv")
lst89 <- lst89_summary[,"median"]
nn.89 <- lst89_summary[,"count"] 
zz.89 <- 2 #qt(1 - alpha/2, nn.89 - 1)
aa.89 <- lst89_summary[,"w_ic_i"] #lst89[,"avg"]  - zz.89 * lst89[,"stddev_pop"] /sqrt(nn.89)
bb.89 <- lst89_summary[,"w_ic_s"] #lst89[,"avg"]  + zz.89 * lst89[,"stddev_pop"] /sqrt(nn.89)

lst01_summary <- read.csv("datos/loyalty/lstInf_summary.csv")
lst01 <- lst01_summary[,"median"]
nn.01 <- lst01_summary[,"count"] 
aa.01 <- lst01_summary[,"w_ic_i"] 
bb.01 <- lst01_summary[,"w_ic_s"]

lst_summary <- read.csv("datos/loyalty/lst89_summary.csv")
lst <- lst_summary[,"median"]
nn <- lst_summary[,"count"] 

rn.max <- 500

ls <- read.csv("datos/learningskill.csv", header =T)
ls <- ls[1:rn.max,]
nn.a <- ls[,4]



y.max <- 32
y.min <- 24



plot(ls[,2][1:rn.max], type="l",ylab="",col="white", 
     ylim=c(24, 32.25), xlim=c(1,rn.max)
     ,xlab="",axes=F)
mtext("Skill", side=2, line=2,cex = 2.5)
mtext("Games played", side=1, line=2,cex = 2.5)
#mtext("Population: all players. Subpopulation: players with all game finished", side=1, line=3, cex=0.85)

#lines(ls[,2][1:rn.max],lwd=3,col="gray")
lines(ls[,2][1:rn.max],lwd=2,lty=2)
lines(lst,lwd=2,lty=3)
lines(lst01,lwd=1.33,lty=3)
lines(lst89,lwd=1.33,lty=3)

y.at =seq(ceiling(y.min), floor(y.max))
x.at = seq(0,rn.max,by=50)
axis(lwd=1,lwd.ticks=1,side=2, at=y.at,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=1,lwd.ticks=1,side=1, at=x.at,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,las=1,cex.axis=1.75,line=-0.66)
#axis(lwd=0,side=2, at=round(seq(ys.min,ys.max,length.out = 4),2),line=-0.5,cex.axis=0.6) 
axis(lwd=0,side=1, line=-0.66,cex.axis=1.75) 

segments(100,lst89[100],100,lst01[100],lty=1,lwd=2)
paso <- 4
segments(100-paso,lst89[100],100+paso,lst89[100],lty=1,lwd=2)
segments(100-paso,lst01[100],100+paso,lst01[100],lty=1,lwd=2)

mod <- rn.max%/%16
Z5.1 <- seq(rn.max)%%mod==1
lst01.points <- lst01;lst.points <- lst; lst89.points <- lst89
lst01.points[!Z5.1] <- NA;lst.points[!Z5.1] <- NA;lst89.points[!Z5.1] <- NA
lst.points<-lst
lst.points[!Z5.1] <- NA

#points(lsc.points, pch=15, cex=0.66)
#points(ls.points, pch=1,lwd=1.33,cex=1.33)
#points(lst01.points,pch=8,lwd=1.33,cex=1.33)
points(lst.points, pch=19,lwd=1.33,cex=1.33)
#points(lst89.points,pch=0,lwd=1.33,cex=1.33)


polygon(c(1:500,rev(1:500)),
        c(aa.01, rev(bb.01)), 
        col = rgb(0.5, 0, 0, 0.5),lty=2, border=F)

polygon(c(1:500,rev(1:500)),
        c(aa.89, rev(bb.89)), 
        col = rgb(0, 0.5,0, 0.5),lty=2, border=F)

par(xpd=TRUE)
legend(140,27.75,
       c("Entire population","Strong team-oriented behavior","  > Loyal players", "  > Casual players"),
       lty=c(2,3,0,0),pch=c(NA,19,15,15),col=c(1,1,rgb(0, 0.5,0, 0.5),rgb(0.5, 0,0, 0.5)),
       lwd=c(2,1.75,NA,NA),cex=1.75, box.lty=0, bg="transparent")


#######################################
dev.off()
setwd(oldwd)
par(oldpar, new=F)
