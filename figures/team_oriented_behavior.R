oldpar <- par(no.readonly = TRUE)
oldwd <- getwd()
this.dir <- dirname(parent.frame(2)$ofile)
nombre.R <-  sys.frame(1)$ofile
require(tools)
nombre <- print(file_path_sans_ext(nombre.R))
setwd(this.dir)
#setwd("~/gaming/antropocaos/cursos/statistical_learning/imagenes/")

pdf(paste0(nombre,".pdf"))
par(mar=c(3.75,3.75,.33,.33))


alpha = 0.05
rn.max<-500

lst89 <- read.csv("datos/team_oriented_behavior/learningskill_pteam89_hasta4team.csv", header =T)
lst89 <- lst89[1:rn.max,]
nn.89 <- lst89[,"count"] 
zz.89 <- qt(1 - alpha/2, nn.89 - 1)
aa.89 <- lst89[,"avg"]-2*lst89[,"stddev_pop"] /sqrt(nn.89)
bb.89 <- lst89[,"avg"]+2*lst89[,"stddev_pop"] /sqrt(nn.89)

lst01 <- read.csv("datos/team_oriented_behavior/learningskill_pteam10_hasta4team.csv", header =T)
lst01 <- lst01[1:rn.max,]
nn.01 <- lst01[,"count"] 
zz.01 <- qt(1 - alpha/2, nn.01 - 1)
#aa.01 <- lst01[,"avg"]  - zz.01 * lst01[,"stddev_pop"] /sqrt(nn.01)
#bb.01 <- lst01[,"avg"]  + zz.01 * lst01[,"stddev_pop"] /sqrt(nn.01)
aa.01 <- lst01[,"avg"]-2*lst01[,"stddev_pop"]  /sqrt(nn.01)
bb.01 <- lst01[,"avg"]+2*lst01[,"stddev_pop"]  /sqrt(nn.01)

lst45 <- read.csv("datos/team_oriented_behavior/learningskill_pteam45_hasta4team.csv", header =T)
lst45 <- lst45[1:rn.max,]
nn.45 <- lst45[,"count"] 
zz.45 <- qt(1 - alpha/2, nn.45 - 1)
aa.45 <- lst45[,"avg"]  - 2 * lst45[,"stddev_pop"]/sqrt(nn.45)
bb.45 <- lst45[,"avg"]  + 2 * lst45[,"stddev_pop"] /sqrt(nn.45)

ls <- read.csv("datos/learningskill.csv", header =T)
ls <- ls[1:rn.max,]
nn <- ls[,4]
zz <- qt(1 - alpha/2, nn - 1)
aa <- ls[,2] - zz * ls[,3]/sqrt(nn)
bb <- ls[,2] + zz * ls[,3]/sqrt(nn)

y.max <- 33
y.min <- 23

plot(1, type="l",ylab="",col="white", ylim=c(23, 33), xlim=c(1,rn.max)
     ,xlab="",axes=F)
mtext("Skill", side=2, line=2,cex = 2)
mtext("Games played", side=1, line=2,cex = 2)
#mtext("Population: all players. Subpopulation: players with all game finished", side=1, line=3, cex=0.85)

lines(lst89[,2],lty=3,lwd=3,pch=19)
lines(lst45[,2],lty=3)
lines(lst01[,2],lty=3)
lines(ls[,2][1:rn.max],lwd=2)

y.at =seq(y.min, y.max)
x.at = seq(0,500,by=100)

#box()
axis(side=2, at=y.at,labels=NA,cex.axis=0.6,tck=0.0)
axis(side=1, labels=NA,cex.axis=0.6,tck=0.0)

axis(lwd=0,lwd.ticks=1,side=2, at=y.at,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,lwd.ticks=1,side=1, at=x.at,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,las=1,cex.axis=1.25,line=-0.66)
#axis(lwd=0,side=2, at=round(seq(ys.min,ys.max,length.out = 4),2),line=-0.5,cex.axis=0.6) 
axis(lwd=0,side=1, line=-0.66,cex.axis=1.25) 



mod <- rn.max%/%16
Z5.1 <- seq(rn.max)%%mod==1
lst01.points <- lst01[,2][1:rn.max];lst45.points <- lst45[,2][1:rn.max]; lst89.points <- lst89[,2][1:rn.max]
lst01.points[!Z5.1] <- NA;lst45.points[!Z5.1] <- NA;lst89.points[!Z5.1] <- NA

mod <- rn.max%/%6
Z5.1 <- seq(rn.max)%%mod==1
ls.points<-ls[,2][1:rn.max]
ls.points[!Z5.1] <- NA

#points(lsc.points, pch=15, cex=0.66)
#points(ls.points, pch=19, cex=0.66)
points(lst01.points,pch=4,lwd=1.33,cex=1.33)
points(lst45.points, pch=2,lwd=1.33,cex=1.33)
points(lst89.points,lwd=1.33,cex=1.33,pch=19)


polygon(c(lst01[,1][!is.na(lst01[,1])][1:rn.max] ,rev(lst01[,1][!is.na(lst01[,1])][1:rn.max])),
          c(aa.01[!is.na(lst01[,1])][1:rn.max], rev(bb.01[!is.na(lst01[,1])][1:rn.max])), 
        col = rgb(0, 0, 0, 0.2),lty=2, border=F)
polygon(c(lst45[,1][!is.na(lst45[,1])][1:rn.max] ,rev(lst45[,1][!is.na(lst45[,1])][1:rn.max])),
        c(aa.45[!is.na(lst45[,1])][1:rn.max], rev(bb.45[!is.na(lst45[,1])][1:rn.max])), 
        col = rgb(0, 0, 0, 0.2),lty=2, border=F)
polygon(c(lst89[,1][!is.na(lst89[,1])][1:rn.max] ,rev(lst89[,1][!is.na(lst89[,1])][1:rn.max])),
        c(aa.89[!is.na(lst89[,1])][1:rn.max], rev(bb.89[!is.na(lst89[,1])][1:rn.max])), 
        col = rgb(0, 0, 0, 0.2),lty=2, border=F)
#polygon(c(ls[,1],rev(ls[,1])),
#        c(aa, rev(bb)), 
#        col = rgb(0, 0, 0, 0.33), border=F)
#polygon(c(lsc[,1] ,rev(lsc[,1] )),
#        c(aa.c, rev(bb.c)), 
#        col = rgb(0, 0, 0, 0.33),lty=2, border=F)





par(xpd=TRUE)
legend(-20,32.5,
       c("All players"),
       lwd=c(2),cex=1.33,box.lty=0, bg="transparent")
text(135,32.5,"Team-oriented behavior:",cex = 1.33)
legend(-10,32,
       c("Strong", "Medium", "Weak"),
       lty=c(3,3,3),pch=c(19,2,4),lwd=c(1.5,1,1),
       cex=1.33,box.lty=0, bg="transparent")

plotdim <- par("plt")
xleft    = plotdim[1] + (plotdim[2] - plotdim[1]) * 0.325
xright   = plotdim[2] - (plotdim[2] - plotdim[1]) * 0.025
ybottom  = plotdim[3] + (plotdim[4] - plotdim[3]) * 0.075  #
ytop     = plotdim[4] - (plotdim[4] - plotdim[3]) * 0.425
region <-c(xleft,xright,ybottom,ytop)
# add inset
par(fig = region, mar=c(0,0,0,0), new=TRUE)

count.max<-max(c(log(lst01[,"count"],10),
                 log(lst45[,"count"],10),
                 log(lst89[,"count"],10),
                 log(ls[,"count"],10)),
               na.rm=T)
count.min<-min(c(log(lst01[,"count"],10),
                 log(lst45[,"count"],10),
                 log(lst89[,"count"],10),
                 log(ls[,"count"],10)),
               na.rm=T)
ys.min <- count.min
ys.max<- count.max

plot(log(lst01[,"count"],10),type="l"
     , ylim=c(signif(count.min,2),signif(count.max,2) ), xlim=c(1,rn.max), 
     axes=F,ann=F,lty=3)
lines(log(lst45[,"count"],10),lty=3)
lines(log(lst89[,"count"],10),lty=3)
lines(log(ls[,"count"],10),lwd=2)



mod <- rn.max%/%16
Z.1 <- seq(rn.max)%%mod==1
lst01.count <- log(lst01[,"count"],10);lst45.count <- log(lst45[,"count"],10);
lst89.count <- log(lst89[,"count"],10);
lst01.count[!Z.1] <- NA;lst45.count[!Z.1] <- NA;lst89.count[!Z.1] <- NA

points(lst01.count,pch=4,lwd=1.33,cex=1.33)
points(lst45.count, pch=2,lwd=1.33,cex=1.33)
points(lst89.count,lwd=1.33,cex=1.33)


at.y <- log(outer(1:9, 10^(floor(ys.min):ceiling(ys.max))),10)
lab.y <- ifelse(at.y %% 1 == 0, at.y, NA)[1,]
lab.y <- lab.y[lab.y>=signif(ys.min,2) & lab.y<=signif(ys.max,2)]
labels <- sapply(lab.y,function(i)
  as.expression(bquote(10^ .(i)))
)

at.y <- c(at.y)[c(at.y)<=signif(ys.max,2) & c(at.y)>=signif(ys.min,2)]


axis(side=2, at=at.y, labels=NA,cex.axis=0.6,tck=0.0133)
axis(lwd=0,lwd.ticks=1,side=2, at=lab.y, labels=NA,cex.axis=0.6,tck=0.03)
axis(lwd=0,side=2, at=lab.y, labels=labels,las=1,cex.lab=0.6,tck=0.08,line=-0.9)
#axis(lwd=0,side=2, at=round(seq(ys.min,ys.max,length.out = 4),2),line=-0.5,cex.axis=0.6) 
axis(labels=NA, side=1,cex.axis=0.6,tck=0.03) 
axis(lwd=0,side=1, line=-0.9,cex.lab=0.6) 

mtext(side=2,"Population size", line=1.66,cex=1.33)


#######################################
# t-test for each point of experiencie
#f<-500
#count <- 0
#pvalues2 <- rep(NA,f)
#for (i in 1:f){
#  pvalue <- 2*pt(-abs((lst01[i,"avg"] - lst89[i,"avg"]) 
#                      * sqrt(lst89[i,"count"])/lst89[i,"stddev_pop"]), 
#                 lst89[i,"count"] - 1)
#  if (pvalue > 0.01 ){count <- count + 1} 
#  pvalues2[i]<-pvalue
#}
#count/f
#plot(pvalues2,type="l")
#############################################

#############################################
# Wilcoxon TEST

#lst89.500 <- read.csv("datos/learningskill_pteam89_hasta4team_skill500.csv", header =T)
#lst45.500 <- read.csv("datos/learningskill_pteam45_hasta4team_skill500.csv", header =T)
#lst01.500 <- read.csv("datos/learningskill_pteam10_hasta4team_skill500.csv", header =T)

#wilcox.test(lst45.500[[1]],lst01.500[[1]])
#wilcox.test(lst45.500[[1]],lst89.500[[1]])
#wilcox.test(lst89.500[[1]],lst01.500[[1]])[["p.value"]]

#ks.test(lst45.500[[1]], "pnorm", mean(lst45.500[[1]]), sd(lst45.500[[1]])) 
###########################

dev.off()
setwd(oldwd)
par(oldpar, new=F)

