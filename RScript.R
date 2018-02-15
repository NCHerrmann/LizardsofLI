###R script for Herrmann et al. 2018
###LIZARDS ON ISLANDS WITHIN ISLANDS: MICROHABITAT USE, MOVEMENT, AND CANNIBALISM IN ANOLIS SAGREI AND ANOLIS SMARAGDINUS

setwd("")

rm(list=ls())
library(Rcapture)
library(FSA)

#POPULATION SIZE ESTIMATES

#load raw data
lizards<-read.csv(file="lizards.csv",header=TRUE)

#####################
####CHOOSE SPECIES###
#####################
#lizards=subset(lizards,Species=="SA") #sagrei
lizards=subset(lizards,Species=="SM") #smaragdinus

#convert data to format taken by Rcpature
#format 1 (Rcapture package description pg. 3)
sublizards=subset(lizards,Rep=="N", select = c(Tag,Species))
x=matrix(0,nrow=length(sublizards$Tag),ncol=length(unique(lizards$Catch.Day)))
dat1=as.matrix(cbind(sublizards,x))

#format 3 (Rcapture package description pg. 4)
y=rep(0,length(sublizards$Tag))
dat3=as.matrix(cbind(sublizards,y))

#get both formats from a single loop
catchdays=1:as.numeric(max(lizards$Catch.Day))
for(i in 1:length(dat1[,1])){
  liz=subset(lizards,lizards$Tag==as.character(dat1[i,1]))
  tagvec=(rep(0,length(catchdays)))
  for(j in 1:length(catchdays)){
    if(length(which(liz$Catch.Day==j))>=1) {
      tagvec[j]=1
    }else{tagvec[j]=0}
  }
  dat1[i,3:(length(catchdays)+2)]=tagvec
  dat3[i,3]=length(unique(liz$Catch.Day))
}

dat1=dat1[,-1:-2]
hold=sapply(dat1, as.numeric)
dat1=matrix(hold,nrow(dat1),ncol(dat1))
dat3=as.numeric(dat3[,-1:-2])


#Fit models
SAgmodel1=closedp.t(dat1)
SAgCI1=closedpCI.t(dat1)
print(SAgmodel1)
boxplot(SAgmodel1)
plot(SAgmodel1)

#ERROR MESSAGE
#SAgmodel3=closedp.t(dat3,dfreq=FALSE, dtype="nbcap",t=12)
#SAgCI3=closedpCI.t(dat3,dfreq=FALSE, dtype="nbcap",t=12)

SMmodel1=closedp.t(dat1)
SMgCI1=closedpCI.t(dat1)
print(SMmodel1)
boxplot(SMmodel1)
plot(SMmodel1)




#STRUCTURAL MICROHABITAT USE

rm(list=ls())

#load data
lizards<-read.csv(file="lizards.csv",header=TRUE)
lizards=subset(lizards,Rep=="N" & (Sex=="M" | Sex=="F"))
greens=subset(lizards,Species=="SM")
browns=subset(lizards,Species=="SA")

#sample sizes
length(lizards$Tag[lizards$Species=="SA" & lizards$Sex=="M"]) #169
length(lizards$Tag[lizards$Species=="SA" & lizards$Sex=="F"]) #217
length(lizards$Tag[lizards$Species=="SM" & lizards$Sex=="M"]) #95
length(lizards$Tag[lizards$Species=="SM" & lizards$Sex=="F"]) #85

#statistical comparisons
colnames(lizards)
fit=lm(P..Height..cm. ~ Species*Sex, data=lizards)
summary(fit)

fit=lm(P..Height..cm. ~ Sex, data=greens) #fit model for greens alone
summary(fit)

fit=lm(P..Diam..cm. ~ Species + Sex, data=lizards)
summary(fit)

fit=lm(lizards$P..Diam..cm. ~ lizards$P..Height..cm) #perch height vs. perch diameter
summary(fit)


#Figure 4

SAMh=lizards$P..Height..cm.[lizards$Species=="SA" & lizards$Sex=="M"]
SAFh=lizards$P..Height..cm.[lizards$Species=="SA" & lizards$Sex=="F"]
SMMh=lizards$P..Height..cm.[lizards$Species=="SM" & lizards$Sex=="M"]
SMFh=lizards$P..Height..cm.[lizards$Species=="SM" & lizards$Sex=="F"]

avgSAMh=mean(SAMh,na.rm=TRUE)
sdSAMh=sd(SAMh,na.rm=TRUE)
avgSAFh=mean(SAFh,na.rm=TRUE)
sdSAFh=sd(SAFh,na.rm=TRUE)
avgSMMh=mean(SMMh,na.rm=TRUE)
sdSMMh=sd(SMMh,na.rm=TRUE)
avgSMFh=mean(SMFh,na.rm=TRUE)
sdSMFh=sd(SMFh,na.rm=TRUE)

SAMd=lizards$P..Diam..cm.[lizards$Species=="SA" & lizards$Sex=="M"]
SAFd=lizards$P..Diam..cm.[lizards$Species=="SA" & lizards$Sex=="F"]
SMMd=lizards$P..Diam..cm.[lizards$Species=="SM" & lizards$Sex=="M"]
SMFd=lizards$P..Diam..cm.[lizards$Species=="SM" & lizards$Sex=="F"]

avgSAMd=mean(SAMd,na.rm=TRUE)
sdSAMd=sd(SAMd,na.rm=TRUE)
avgSAFd=mean(SAFd,na.rm=TRUE)
sdSAFd=sd(SAFd,na.rm=TRUE)
avgSMMd=mean(SMMd,na.rm=TRUE)
sdSMMd=sd(SMMd,na.rm=TRUE)
avgSMFd=mean(SMFd,na.rm=TRUE)
sdSMFd=sd(SMFd,na.rm=TRUE)

catego=c("Males (n = 169)","Females (n = 217)","Males (n = 95)","Females (n = 85)")
cols=c("sienna3","sienna4","seagreen3","seagreen4")

par(mfcol=c(2,1))

#perch height plot
plot(x=c(0.25,0.75,1.25,1.75),y=c(avgSAMh,avgSAFh,avgSMMh,avgSMFh), xlim=c(0,2),ylim=c(0,200),
     xlab="",ylab="Perch Height (cm)", cex=1.4,col=cols,pch=19,axes=FALSE)
axis(2, cex.axis=.8)
box(which = "plot", lty = "solid")

goo=c(avgSAMh,avgSAFh,avgSMMh,avgSMFh)
foo=c(sdSAMh,sdSAFh,sdSMMh,sdSMFh)
max<-goo+foo
min<-goo-foo
arrows(c(0.25,0.75,1.25,1.75),min,c(0.25,0.75,1.25,1.75),max,code=3,angle=90,length=0.09)

#perch diameter plot
plot(x=c(0.25,0.75,1.25,1.75),y=c(avgSAMd,avgSAFd,avgSMMd,avgSMFd), xlim=c(0,2), ylim=c(-2,15),
     xlab="",ylab="Perch Diameter (cm)", cex=1.4,col=cols,pch=19,axes=FALSE)
axis(side=1, at=c(0.25,0.75,1.25,1.75), labels=catego, cex.axis=0.85, tick = FALSE)
axis(2, cex.axis=.8)
box(which = "plot", lty = "solid")

goo=c(avgSAMd,avgSAFd,avgSMMd,avgSMFd)
foo=c(sdSAMd,sdSAFd,sdSMMd,sdSMFd)
max<-goo+foo
min<-goo-foo
arrows(c(0.25,0.75,1.25,1.75),min,c(0.25,0.75,1.25,1.75),max,code=3,angle=90,length=0.09)


#Figure 5

rm(list=ls())

par(mfrow=c(4,1))

repdata<-read.csv(file="multcatch.csv",header=TRUE)
which(is.na(repdata$Diam)) #52  55  61  66  88  96 112 115 117 185 189 195 217 261 265 313 324
#these rows need to be removed for the perch diameter plot because lizards observed on
#ground were not assigned a perch diameter
repdata=repdata[-c(52,55,60,61,66,87,88,95,96,111,112,115,116,117,185,189,194,195,216,217,261,265,313,324),]

#To generate the figure, run the loop below,and then generate the associated histogram
#that follows. Must repeat for 4 subsets of "repdata", first for perch height and then
#for perch diameter. The two 4x1 panels were combined in powerpoint, along with their legends.

repdata=subset(repdata,repdata$Spec=="SA" & repdata$Sex=="M")
#repdata=subset(repdata,repdata$Spec=="SA" & repdata$Sex=="F")
#repdata=subset(repdata,repdata$Spec=="SM" & repdata$Sex=="M")
#repdata=subset(repdata,repdata$Spec=="SM" & repdata$Sex=="F")

IDs=unique(repdata$Tag)
length(IDs)
heightrange=rep(0,length(IDs))
diamrange=rep(0,length(IDs))
for(i in 1:length(IDs)){
  heightrange[i]=max(repdata$Height[repdata$Tag==IDs[i]])-min(repdata$Height[repdata$Tag==IDs[i]])
  diamrange[i]=max(repdata$Diam[repdata$Tag==IDs[i]])-min(repdata$Diam[repdata$Tag==IDs[i]])
}

#perch height histograms
cols=c("sienna3","sienna4","seagreen3","seagreen4")
div=seq(0,260,10)

hist(heightrange, xlab="",ylab="", main="",breaks=div,xlim=c(0,260),col=cols[1], axes=FALSE)
axis(2, cex.axis=.8)
#hist(heightrange, xlab="",ylab="",main="",breaks=div,xlim=c(0,260),col=cols[2], axes=FALSE)
#axis(2, cex.axis=.8)
#hist(heightrange, xlab="",ylab="",main="",breaks=div,xlim=c(0,260),col=cols[3], axes=FALSE)
#axis(2, at=c(0,1,2,3),cex.axis=.8)
#hist(heightrange, xlab="Max Height - Min Height (cm)",ylab="",main="",breaks=div, xlim=c(0,260),col=cols[4], axes=FALSE)
#axis(1,at=seq(0,260,20),cex.axis=.8)
#axis(2, at=c(0,1,2),cex.axis=.8)

#perch height legend
legend(140,2,cex=1.2,legend=c("Male A. sagrei (n=62)","Female A. sagrei (n=39)",
                              "Male A. smaragdinus (n=18)","Female A. smaragdinus (n=9)"),fill=cols)


#perch diameter histograms

cols=c("sienna3","sienna4","seagreen3","seagreen4")
div=seq(0,55,5)

#hist(diamrange,xlab="",ylab="", main="",breaks=div,xlim=c(0,55),ylim=c(0,40),col=cols[1],axes=FALSE)
#axis(2, at=c(0,10,20,30,40),cex.axis=.8)
#hist(diamrange, xlab="",ylab="",main="",breaks=div,xlim=c(0,55),ylim=c(0,30),col=cols[2], axes=FALSE)
#axis(2, at=seq(0,30,5),cex.axis=.8)
#hist(diamrange, xlab="",ylab="",main="",breaks=div,xlim=c(0,55),ylim=c(0,14),col=cols[3], axes=FALSE)
#axis(2,at=seq(0,14,2),cex.axis=.8)
#hist(diamrange, xlab="Max Diam - Min Diam (cm)",ylab="",main="",breaks=div,xlim=c(0,55),col=cols[4], axes=FALSE)
#axis(1,at=seq(0,55,5),cex.axis=.8)
#axis(2,cex.axis=.8)

#perch diameter legend
legend(21,4,cex=1.2,legend=c("Male A. sagrei (n=60)","Female A. sagrei (n=36)",
                             "Male A. smaragdinus (n=18)","Female A. smaragdinus (n=7)"),fill=cols)



#INTRA- AND INTERSEASONAL MOVEMENT

rm(list=ls())

#load data
August<-read.csv(file="AugRepsSumOnly.csv",header=TRUE)
January<-read.csv(file="JanRecapOnly.csv",header=TRUE)
foo=rbind(August,January)
sag=subset(foo,foo$Spec=="SA")

#Figure 6
sa_m_aug=foo$D[foo$Group=="sa_m_aug"]
sa_m_jan=foo$D[foo$Group=="sa_m_jan"]
sa_f_aug=foo$D[foo$Group=="sa_f_aug"]
sa_f_jan=foo$D[foo$Group=="sa_f_jan"]

catego=c("Males (n = 46)","Females (n = 26)","Males (n = 10)","Females (n = 28)")
cols=c("sienna3","sienna4")

par(mfrow=c(1,1))

boxplot(sa_m_aug,sa_f_aug,
        sa_m_jan,sa_f_jan,
        ylab="Distance (m)",ylim=c(0, 55), col=cols,axes=FALSE)
axis(side=1, at=1:4, labels=catego, cex.axis=1, tick = FALSE)
axis(2, cex.axis=.8)
posthoc=c("A","B","A","B")
text(x=1:4,y=rep(53,4),labels=posthoc,cex=1.3)
box(which = "plot", lty = "solid")



#Summary stats
length(sa_m_aug) #46
length(sa_f_aug) #26
length(sa_m_jan) #10
length(sa_f_jan) #28

mean(sa_m_aug) #8.122775
mean(sa_f_aug) #5.95738
mean(sa_m_jan) #16.1529
mean(sa_f_jan) #4.258957

sd(sa_m_aug) #7.756752
sd(sa_f_aug) #8.797341
sd(sa_m_jan) #15.51197
sd(sa_f_jan) #3.920588

#Statistical comparuson of groups
kruskal.test(D ~ Group, data=sag)
dunnTest(D ~ Group, data=sag,method="bh") #this is a K-W post-hoc


#Redoing statistical tests with fourteen A. sagrei in category 3 (see manuscript)
Aug<-read.csv(file="AugRepsSum.csv",header=TRUE)
Jan<-read.csv(file="JanRecap.csv",header=TRUE)

Aug=subset(Aug,select = c(maxd,month,group), spec=="SA")
Jan=subset(Jan,select = c(dAugJan,month,group), Spec=="SA")
colnames(Jan)=c("maxd","month","group")
foo=rbind(Aug,Jan)

dunnTest(maxd ~ group, data=foo, method="bh") #this is a K-W post-hoc
