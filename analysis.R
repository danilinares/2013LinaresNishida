nBoot<-1000
ythre<-.75

### Reading data
dat<-read.files(subject='',exp='Discrimination')
dat$subject<-factor(dat$subject,levels=c('DL','SN','MT','RH','MS','KN'))
NS<-'No surround   '; SY<-'Synchronous surround   '; SU<-'Sustained surround   '
conditions<-c(NS,SY,SU)
dat$figure<-factor(dat$figure,levels=c(3,1,2),labels=conditions)
dat$cohComparison<-2*dat$cohComparison
dat$cohComparison<-.01*dat$cohComparison
dat$x<-dat$coherence

### Number of trials
nTrials<-ddply(dat,.(subject,cohComparison,coherence),function(df) {
  number.measures<-length(df$response)
  data.frame(number.measures)
})
nTrials


################
### Figure 2 ###
################
### Figure 2A
av<-ddply(dat,.(subject,x,figure,cohComparison),function(d) averages(d))
curve<-ddply(av,.(subject,figure,cohComparison),
          function(d) curveFun(c(.5,.5),d,cumNorm,2,xmin=min(d$cohComparison)))
th<-ddply(av,.(subject,figure,cohComparison),
          function(d) threFun(c(.5,.5),d,cumNorm,cumNormQ,ythre,2,nBoot))

#plot
th$yPlot<-0; th$yPlot[th$figure==NS]<-.42
th$yPlot[th$figure==SY]<-.44; 
th$yPlot[th$figure==SU]<-.46

pPsy<-ggplot()+
 facet_grid(cohComparison~subject,labeller=mf_labeller)+
  geom_vline(data=av,aes(xintercept=cohComparison), size=.5,
             color='grey',lty=2)+
  geom_linerange(data=th,aes(x=thre,ymin=yPlot,ymax=ythre,
                              color=figure),size=.25)+
  geom_segment(data=th,aes(x=cohComparison,xend=thre,y=yPlot,yend=yPlot,
                            color=figure),size=.25)+
  geom_line(data=curve,aes(x=x,y=y,color=figure),size=.25)+
  geom_point(data=av,aes(x=x,y=y,color=figure,shape=figure),size=1)+
  scale_x_continuous(breaks=seq(0,1,by=.2),
                     limits=c(0,1),labels=c('0','.2','.4','.6','.8','1'))+
  scale_y_continuous(limits=c(.41,1.05),breaks=c(.5,.75,1))+
  xlab('Coherence variable')+ylab('Proportion correct')+
  scale_colour_manual(values=color_values)+
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.title.y=element_text(vjust=.25),
        axis.title.x=element_text(vjust=.1),
        panel.grid.minor=element_blank(),
        strip.text.y=element_text(angle=0),
        legend.text=element_text(lineheight=2.75))
pPsy
### Figure 2B
# assessing differences across conditions 
avSyncNoSurr<-subset(av,figure!=SU)
difSyncNoSurr<-ddply(avSyncNoSurr,.(subject,cohComparison),function(d) 
    difFun(c(.5,.5),d,cumNorm,cumNormQ,ythre,2,nBoot,'figure'))
difSyncNoSurr$signif<-sign(difSyncNoSurr$difMin)*sign(difSyncNoSurr$difMax)
thSignSynch<-subset(th,figure==SY)
thSignSynchAst<-thSignSynch[difSyncNoSurr$signif==1,]

avSustNoSurr<-subset(av,figure!=SY)
difSustNoSurr<-ddply(avSustNoSurr,.(subject,cohComparison),function(d) 
  difFun(c(.5,.5),d,cumNorm,cumNormQ,ythre,2,nBoot,'figure'))
difSustNoSurr$signif<-sign(difSustNoSurr$difMin)*sign(difSustNoSurr$difMax)
thSignSust<-subset(th,figure==SU)
thSignSustAst<-thSignSust[difSustNoSurr$signif==1,]

th$threS<-th$thre-th$cohComparison
th$threMinS<-th$threMin-th$cohComparison
th$threMaxS<-th$threMax-th$cohComparison
thSignSynchAst$threS<-thSignSynchAst$thre-thSignSynchAst$cohComparison
thSignSustAst$threS<-thSignSustAst$thre-thSignSustAst$cohComparison

# small shift to visualize better
th$cohComparisonPlot[th$figure==SY]<-th$cohComparison[th$figure==NS]
th$cohComparisonPlot[th$figure==NS]<-th$cohComparison[th$figure==NS]+.01
th$cohComparisonPlot[th$figure==SU]<-th$cohComparison[th$figure==NS]+.02
thSignSustAst$cohComparisonPlot<-thSignSustAst$cohComparison+0.02

#triming the very large CIs and curves
th$threMaxSTrim<-th$threMaxS
th$threMinSTrim<-th$threMinS
th$threMaxSTrim[th$threMaxSTrim>1.2]<-1.2
th$threMinSTrim[th$threMinSTrim< -0.1]<- -0.1

pThre<-ggplot()+
  facet_wrap(~subject,ncol=6,scales='free')+
  geom_linerange(data=th,size=.25,aes(x=cohComparisonPlot,ymin=threMinSTrim,
                              ymax=threMaxSTrim,color=figure,shape=figure))+
  geom_line(data=th,size=.5,
             aes(x=cohComparisonPlot,y=threS,color=figure,shape=figure))+
  geom_point(data=th,size=2,
             aes(x=cohComparisonPlot,y=threS,color=figure,shape=figure))+
   geom_point(data=thSignSynchAst,size=4,shape=1,
              aes(x=cohComparison,y=threS))+
   geom_point(data=thSignSustAst,size=4,shape=1,
              aes(x=cohComparisonPlot,y=threS))+
  scale_colour_manual(values=color_values)+
  scale_x_continuous(breaks=seq(0,.5,by=.1),
                  labels=c('0','.1','.2','.3','.4','.5'))+
  xlab('Coherence pedestal')+ylab('Threshold coherence')+
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.title.y=element_text(vjust=.4),
        axis.title.x=element_text(vjust=0),
        panel.grid.minor=element_blank())
pThre

### Generating Figure 2
pa<-arrangeGrob(pPsy,left=tG('A'))
pb<-arrangeGrob(pThre,left=tG('B'))
pdf('figures/Figure2.pdf',width=7.5,height=8.5)
grid.arrange(pa,pb,heights=c(.65,.35))
dev.off()

# rejected fits using Deviance
length(th$okDev)
length(th$okDev[th$okDev==T])
length(th$okDev[th$okDev==F])

################
### Figure 3 ###
################
fitAll<-ddply(av,.(subject),.progress ='text',fitAllfun)
curveTransdAllDirAllCoh<-ddply(fitAll,.(subject,figure),curveTransdFun)

# estimating the response up to coherence=.5+threhold for 0.5 (No surround)
curveTransdRange<-ddply(curveTransdAllDirAllCoh,.(subject),function(d){
  subTh<-subset(th,subject==unique(d$subject) & cohComparison==.5 &
                  figure==NS)
  subset(d,x<subTh$thre)
})

pTransd<-ggplot()+
  facet_wrap(~subject,as.table=F)+
  geom_line(data=subset(curveTransdRange,subject!='KN'),
            aes(x=x,y=y,color=figure))+ 
  scale_colour_manual(values=color_values,
        labels=c('No surround','Synchronous\nsurround','Sustained\nsurround'))+
  xlab('Coherence')+ylab('Arbitrary response units')+
  scale_x_continuous(breaks=seq(0,.8,by=.2))+
  theme(legend.position=c(.85,.85),
        legend.title=element_blank(),
        axis.title.y=element_text(vjust=.25),
        axis.title.x=element_text(vjust=0),
        legend.key.height=unit(1.5,'lines'))
pTransd

pdf('figures/Figure3.pdf',width=3.6,height=3.5)
pTransd
dev.off()

### icon 1 #####
pIcon<-ggplot()+
  geom_line(data=subset(curveTransdRange,subject=='DL'), size=3,
            aes(x=x,y=y,color=figure))+ 
  scale_colour_manual(values=color_values,
                      labels=c('No surround','Synchronous','Sustained'))+
  xlab('Coherence')+ylab('Response')+
  scale_x_continuous(breaks=seq(0,.8,by=.2))+
  theme(legend.position='none',
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=45),
        legend.key.height=unit(2,'line'),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=0.4,size=65),
        axis.title.x=element_text(vjust=0,size=65),
        legend.key.height=unit(1.5,'lines'))
pIcon


png('figures/icon1.png', width = 96*5, height = 96*5)
pIcon
dev.off()

################
### Figure 4 ###
################
### Figure 4A
cohComparisons<-data.frame(cohComparison=unique(dat$cohComparison))
curveAll<-ddply(cohComparisons,.(cohComparison),curveAllfun)

pPsyAll<-ggplot()+
  facet_grid(cohComparison~subject)+
  geom_vline(data=av,aes(xintercept=cohComparison), size=.5,
             color='grey',lty=2)+
  geom_line(data=curveAll,aes(x=x,y=y,color=figure),size=.25)+
  geom_point(data=av,aes(x=x,y=y,color=figure,shape=figure),size=1)+
  scale_x_continuous(breaks=seq(0,1,by=.2),
                     limits=c(0,1),labels=c('0','.2','.4','.6','.8','1'))+
  scale_y_continuous(limits=c(.41,1.05),breaks=c(.5,.75,1))+
  xlab('Coherence variable')+ylab('Proportion correct')+
  scale_colour_manual(values=color_values)+
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.title.y=element_text(vjust=.25),
        axis.title.x=element_text(vjust=.1),
        panel.grid.minor=element_blank(),
        strip.text.y=element_text(angle=0),
        legend.text=element_text(lineheight=2.75))
pPsyAll

### Figure 4B
threAll<-ddply(cohComparisons,.(cohComparison),threAllfun)

threAll$threS<-threAll$thre-threAll$cohComparison
transdRAll<-ddply(threAll,.(subject,figure),fitTransdR)
curveDerivAll<-ddply(transdRAll,.(subject,figure),curveDerivFunAll)

threAllPlot<-threAll
threAllPlot<-subset(threAllPlot,!(subject=='KN' & 
                                !(cohComparison==0 | cohComparison==.5)))
threAllPlot<-subset(threAllPlot,!(subject=='RH' & 
                                cohComparison==.5 & figure==SY))
threAllPlot<-subset(threAllPlot,!(subject=='MT' & 
                                cohComparison==.5 & figure==SY))

curveDerivAllPlot<-curveDerivAll
curveDerivAllPlot<-subset(curveDerivAllPlot,!(curveDerivAll$subject=='RH' & 
                           curveDerivAll$x>.4 & curveDerivAll$figure==SY))
curveDerivAllPlot<-subset(curveDerivAllPlot,!(curveDerivAll$subject=='MT' & 
                          curveDerivAll$x>.4 & curveDerivAll$figure==SY))
# ci
fitAllBoot<-read.table('fitAllBoot.txt',
                       stringsAsFactors=FALSE,row.names=NULL)

threAllBoot<-ddply(cohComparisons,.(cohComparison),threAllfunBoot)
ciBootAll<-ddply(threAllBoot,.(subject,figure,cohComparison),function(d){
  Q<-quantile(d$thre,c(.025,.975))
  data.frame(threMin=Q[[1]],threMax=Q[[2]])
})


# small shift to visualize better
threAllPlot$cohComparisonPlot<-threAllPlot$cohComparison
threAllPlot$cohComparisonPlot[threAllPlot$figure==SY]<-
  threAllPlot$cohComparison[threAllPlot$figure==SY]
threAllPlot$cohComparisonPlot[threAllPlot$figure==NS]<-
  threAllPlot$cohComparison[threAllPlot$figure==NS]+.01
threAllPlot$cohComparisonPlot[threAllPlot$figure==SU]<-
  threAllPlot$cohComparison[threAllPlot$figure==SU]+.02

ciBootAll$cohComparisonPlot<-ciBootAll$cohComparison
ciBootAll$cohComparisonPlot[ciBootAll$figure==SY]<-
  ciBootAll$cohComparison[ciBootAll$figure==SY]
ciBootAll$cohComparisonPlot[ciBootAll$figure==NS]<-
  ciBootAll$cohComparison[ciBootAll$figure==NS]+.01
ciBootAll$cohComparisonPlot[ciBootAll$figure==SU]<-
  ciBootAll$cohComparison[ciBootAll$figure==SU]+.02

ciBootAllPlot<-ciBootAll
ciBootAllPlot<-subset(ciBootAllPlot,!(subject=='KN' & 
                                    !(cohComparison==0 | cohComparison==.5)))
ciBootAllPlot<-subset(ciBootAllPlot,!(subject=='RH' & 
                                    cohComparison==.5 & figure==SY))
ciBootAllPlot<-subset(ciBootAllPlot,!(subject=='MT' & 
                                    cohComparison==.5 & figure==SY))

ciBootAllPlot$threMaxS<-ciBootAllPlot$threMax-ciBootAllPlot$cohComparison
ciBootAllPlot$threMinS<-ciBootAllPlot$threMin-ciBootAllPlot$cohComparison
ciBootAllPlot$threMaxS[ciBootAllPlot$subject=='KN' & ciBootAllPlot$figure==SY &
                        ciBootAllPlot$cohComparison==.5]<-.75 #to visualize

pThreAll<-ggplot(data=threAllPlot)+
  facet_wrap(~subject,scales='free',ncol=6)+
  geom_point(data=threAllPlot,size=2,
             aes(x=cohComparisonPlot,y=threS,color=figure,shape=figure))+
  geom_linerange(data=ciBootAllPlot,aes(x=cohComparisonPlot,
                    ymin=threMinS,ymax=threMaxS,color=figure))+
  geom_point(data=threAllPlot,size=2,
             aes(x=cohComparisonPlot,y=threS,color=figure,shape=figure))+
  geom_line(data=threAllPlot,
             aes(x=cohComparisonPlot,y=threS,color=figure,shape=figure))+
  scale_colour_manual(values=c(color_values))+
  scale_x_continuous(breaks=seq(0,.5,by=.1),
                     labels=c('0','.1','.2','.3','.4','.5'))+
  xlab('Coherence pedestal')+ylab('Threshold coherence')+
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.title.y=element_text(vjust=.4),
        axis.title.x=element_text(vjust=0),
        panel.grid.minor=element_blank())
pThreAll

### Generating Figure 4
paAll<-arrangeGrob(pPsyAll,left=tG('A'))
pbAll<-arrangeGrob(pThreAll,left=tG('B'))
pdf('figures/Figure4.pdf',width=7.5,height=8.5)
grid.arrange(paAll,pbAll,heights=c(.65,.35))
dev.off()


###  Icon 2 #####
threAllI<-subset(threAll,subject=='DL')
threAllI$figure<-factor(threAllI$figure,labels=c('No surround','Synchronous','Sustained'))
pIcon2<-ggplot(data=threAllI)+
  geom_point(data=threAllI,size=10,
             aes(x=cohComparison,y=threS,color=figure,shape=figure))+
  geom_line(data=threAllI, size=3,show_guide=F,
            aes(x=cohComparison,y=threS,color=figure,shape=figure))+
  scale_colour_manual(values=c(color_values))+
  scale_x_continuous(breaks=seq(0,.5,by=.1),
                     labels=c('0','.1','.2','.3','.4','.5'))+
  xlab('Pedestal')+ylab('Threshold')+
  theme(legend.title=element_blank(),
        legend.position=c(.63,.8),
        legend.text=element_text(size=50),
        legend.key.height=unit(4,'line'),
        legend.key=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.title.y=element_text(vjust=.4,size=65),
        axis.title.x=element_text(vjust=0,size=65),
        panel.grid=element_blank())
pIcon2

png('figures/icon2.png', width = 96*5, height = 96*5)
pIcon2
dev.off()

### Calculating AIC
fit<-ddply(av,.(subject,figure,cohComparison),
            function(d) fitFun(c(.5,.5),d,cumNorm,2,1))
negLogLike<-ddply(av,.(subject,figure,cohComparison),function(d){
  subFit<-subset(fit,subject==unique(d$subject) & 
                     figure==unique(d$figure) &
                     cohComparison==unique(d$cohComparison))
  negLogLike<-likeL(subFit$p,d,cumNorm,2)      
  data.frame(negLogLike)
})
akaike<-ddply(negLogLike,.(subject),function(d) {
    negLogLike=sum(d$negLogLike)
    k<-length(d$subject)*2
    aka<-2*k+2*negLogLike
    data.frame(k,aka)
})
akaike$fit<-'cumNormal'

akaikeAll<-ddply(av,.(subject),function(d){
  subFit<-subset(fitAll,subject==unique(d$subject)) 
  p<-c(subFit$p,unique(subFit$noise))
  negLogLike<-lAll(p,d)
  k<-length(subFit$subject)+1
  aka<-2*k+2*negLogLike
  data.frame(k,aka)
})
akaikeAll$fit<-'conjoint'

akaikes<-rbind(akaike,akaikeAll)
akaikeProb<-ddply(subset(akaikes,subject!='KN'),.(subject),function(d){
  dif<-max(d$aka)-min(d$aka)
   p<-exp(-.5*dif)
   data.frame(p,dif)
})
################
### Figure 5 ###
################
### Figure 5A
datApp<-read.files(subject='',exp='App')
conditions<-c(NS,SY,SU)
datApp$figure<-factor(datApp$figure,levels=c(3,1,2),labels=conditions)

datApp$x<-datApp$coherence
avApp<-ddply(datApp,.(subject,x,figure),function(d) averages(d))
curveApp<-ddply(avApp,.(subject,figure),
                function(d) curveFun(c(.5,.5),d,cumNorm,Inf,xmin=0,xmax=1))
thApp<-ddply(avApp,.(subject,figure),.progress='text',
             function(d) threFun(c(.5,.5),d,cumNorm,cumNormQ,.5,Inf,nBoot))

dat$figure<-factor(dat$figure,levels=c(3,1,2),labels=conditions)
avApp$subject<-factor(avApp$subject,
                      levels=c('DL','SN','MT','RH','MS','AI','YM','IM','HM'))

pPsyApp<-ggplot()+
  facet_wrap(~subject,ncol=5,as.table=F)+
  geom_linerange(data=avApp,aes(x=.4,ymin=0,ymax=1),lwd=.5, color='grey')+
  geom_rect(data=subset(thApp,figure==SY),
            aes(xmin=threMin,xmax=threMax,ymin=0,ymax=.5),alpha=.1)+
  geom_linerange(data=thApp,aes(x=thre,ymin=0,ymax=.5,color=figure),
                 lty=2,size=.5)+
  geom_point(data=avApp,size=2,aes(x=x,y=y,color=figure,shape=figure))+
  geom_line(data=curveApp,aes(x=x,y=y,color=figure))+
  xlab('Coherence variable (No surround)')+
  ylab('Proportion .4 coherence perceived weaker')+
  scale_x_continuous(breaks=seq(0,1,by=.2),
                     labels=c('0','.2','.4','.6','.8','1'))+
  coord_cartesian(xlim=c(0,1.1))+
  scale_y_continuous(breaks=seq(0,1,by=.2),limits=c(0,1))+
  scale_colour_manual(values=color_values,
        labels=c('No surround','Synchronous\nsurround','Sustained\nsurround'))+
  scale_shape_manual(values=c(16,17,15),
        labels=c('No surround','Synchronous\nsurround','Sustained\nsurround'))+
  theme(legend.title=element_blank(),
        panel.grid=element_blank(),
        axis.title.y=element_text(vjust=.4),
        axis.title.x=element_text(vjust=0),
        legend.position=c(.92,.8))
pPsyApp

# rejected fits using Deviance
length(thApp$okDev)
length(thApp$okDev[thApp$okDev==T])
length(thApp$okDev[thApp$okDev==F])

### Figure 5B
datApp3<-subset(datApp,subject=='DL' | subject=='MT' | subject=='SN' |
                  subject=='RH' |  subject=='MS')

resp40<-ddply(subset(fitAll,subject!='KN'),.(subject,figure),function(d){
  y40<-transd(d$p,.4)
  data.frame(y40)
})

resp40<-subset(resp40,resp40$figure!=NS)
model.transd.no.surr<-subset(fitAll,fitAll$figure==NS)
coh.app<-ddply(resp40,.(subject,figure),function(d){
  P<-subset(model.transd.no.surr,
            model.transd.no.surr$subject==unique(d$subject))
  coherence<-inv.transd(P$p,d$y40)
  data.frame(coherence,y40=d$y40)
})
coh.app<-subset(coh.app,coh.app$figure==SY)
fitApp3<-subset(thApp,(subject=='DL' | subject=='SN' |
                  subject=='MT' | subject=='RH' | subject=='MS' ) & figure==SY)
pTransdApp<-ggplot()+
  facet_wrap(~subject,ncol=5)+
  geom_line(data=subset(curveTransdAllDirAllCoh,subject!='KN'), 
            aes(x=x,y=y,color=figure))+ 
  geom_linerange(data=resp40,aes(x=.4,ymin=0,ymax=y40))+
  geom_segment(data=coh.app,aes(x=.4,xend=coherence,y=y40,yend=y40))+
  geom_rect(data=fitApp3,
            aes(xmin=threMin,xmax=threMax,ymin=0,ymax=.5),alpha=.1)+
  geom_linerange(data=fitApp3,color=color_values[2],aes(x=thre,ymin=0,ymax=.5),
      lty=2)+
  scale_colour_manual(values=color_values)+
  scale_x_continuous(breaks=seq(0,1,by=.2),limits=c(0,1))+
  theme(legend.title=element_blank(),
        axis.title.y=element_text(vjust=.4),
        axis.title.x=element_text(vjust=0),
        legend.position='top')+
  xlab('Coherence')+ylab('Arbitrary response units')
pTransdApp

### Generating Figure 5
pa5<-arrangeGrob(pPsyApp,left=tG('A'))
pb5<-arrangeGrob(pTransdApp,left=tG('B'))
pdf('figures/Figure5.pdf',width=7.5,height=6.5)
grid.arrange(pa5,pb5,heights=c(.55,.45))
dev.off()



################
### Figure 6 ###
################
curveTransdAllDirAllCoh<-ddply(fitAll,.(subject,figure),curveTransdFun)

# estimating the response up to coherence=.5+threhold for 0.5 (No surround)
curveTransdAllDir<-ddply(curveTransdAllDirAllCoh,.(subject),function(d){
  subTh<-subset(threAll,subject==unique(d$subject) & cohComparison==.5 &
                  figure==NS)
  subset(d,x<subTh$thre)
})
curveTransdAllDirAllCoh<-ddply(fitAll,.(subject,figure),curveTransdFun)

# estimating the response up to coherence=.5+threhold for 0.5 (No surround)
curveTransdRange<-ddply(curveTransdAllDirAllCoh,.(subject),function(d){
  subTh<-subset(th,subject==unique(d$subject) & cohComparison==.5 &
                  figure==NS)
  subset(d,x<subTh$thre)
})

### Response gain 
c.transd.direct.sync<-subset(curveTransdRange,curveTransdRange$figure==SY)
c.transd.direct.no.sync<-subset(curveTransdRange,curveTransdRange$figure!=SY)

sim.transd<-ddply(c.transd.direct.no.sync,.(subject,figure),function(df){
  minimization<-function(para){
    y1<-para*df$y
    y2<-c.transd.direct.sync$y[c.transd.direct.sync$subject==df$subject]
    return(sum((y1-y2)^2))
  }  
  data.frame(par=optim(c(1),minimization)$par)
})

c.transd.sim<-ddply(c.transd.direct.no.sync,.(subject,figure),function(df){
  para<-sim.transd$par[sim.transd$subject==unique(df$subject) &
                         sim.transd$figure==unique(df$figure) ]
  y<-df$y*para
  data.frame(x=df$x,y)
})
c.transd.sim$model<-'Response gain'

### Coherence gain 
ys<-ddply(curveTransdRange,.(subject),function(df){
  max.df<-ddply(df,.(figure),function(df2){
    data.frame(max=max(df2$y))
  })
  y<-min(max.df$max)
  data.frame(y)
})

c.transd.inv<-ddply(fitAll,.(subject),function(df){
  y1<-0
  y2<-ys$y[ys$subject==unique(df$subject)]
  ddply(df,.(figure),function(df2){
    y<-seq(y1,y2,len=100)
    para<-df$p[df$figure==df2$figure]
    x<-laply(y, function(z) inv.transd(para,z))
    data.frame(x,y)
  })
})

c.transd.inv.no.syn<-subset(c.transd.inv,c.transd.inv$figure!=SY)
c.transd.inv.syn<-subset(c.transd.inv,c.transd.inv$figure==SY)

sim.transd.inv<-ddply(c.transd.inv.no.syn,.(subject,figure),function(df){
  minimization<-function(para){
    x1<-para*df$x
    x2<-c.transd.inv.syn$x[c.transd.inv.syn$subject==df$subject]
    return(sum((x1-x2)^2))
  }  
  data.frame(par=optim(c(1),minimization)$par)
})

c.transd.inv.sim<-ddply(c.transd.inv.no.syn,.(subject,figure),function(df){
  para<-sim.transd.inv$par[sim.transd.inv$subject==unique(df$subject) &
                             sim.transd.inv$figure==unique(df$figure) ]
  x<-df$x*para
  data.frame(y=df$y,x) 
})
c.transd.inv.sim$model<-'Coherence gain'
c.transd<-rbind(c.transd.sim,c.transd.inv.sim)

pTransdAll<-ggplot()+
  facet_grid(subject~model,scales='free')+
  geom_line(data=subset(curveTransdRange,subject!='KN'),
            aes(x=x,y=y,color=figure))+ 
  geom_line(data=subset(c.transd,subject!='KN'),aes(x=x,y=y,color=figure),lty=2)+ 
  scale_colour_manual(values=color_values)+
  scale_x_continuous(breaks=seq(0,.8,by=.2))+
  xlab('Coherence')+ylab('Arbitrary response units')+
  theme(legend.position='top',
        legend.direction='vertical',
        legend.title=element_blank(),
        strip.text.y=element_text(angle=0),
        axis.title.y=element_text(vjust=.25),
        axis.title.x=element_text(vjust=0),
        legend.key.height=unit(1.5,'lines'))
pTransdAll

pdf('figures/Figure6.pdf',width=3.6,height=8)
pTransdAll
dev.off()


################
### Figure 7 ###
################
datSHsame<-read.table('data/datSHsame.txt')
datSHsame$direction<-1
datSHopp<-read.table('data/datSHopp.txt')
datSHopp$direction<- -1
datSHaux<-rbind(datSHsame,datSHopp)
names(datSHaux)<-c('x','y','n','sigma','direction')

#because the response of MT neurons in noisy for random-dots,
#we run several simulations and averaged them
datSH<-ddply(datSHaux,.(x,n,sigma,direction),numcolwise(mean)) 

#we substract the activity of neurons tuned to opposite directions
datSHDifAllCoh<-ddply(datSH,.(n,sigma,x),function(d){
  y<-d$y[d$direction==1]-d$y[d$direction==-1]
  data.frame(y)
})

#establishing the range of coherences to fit
datSHDif<-subset(datSHDifAllCoh,x<.6)
#7 points of the transducer function
curveTransd10<-ddply(fitAll,.(subject,figure),
                           function(d)curveTransdFun(d,10))
curveTransd6<-subset(curveTransd10,x<.6)
#scale to response to adjust the response of our response functions
fitSH<-ddply(curveTransd6,.(subject,figure),function(d){
  ddply(datSHDif,.(n,sigma),function(d2){
    fun.min<-function(p){
      y1<-d$y; y2<-p*d2$y
      return(sum((y1-y2)^2))
    }
    par<-optimize(fun.min,c(0,100))$minimum
    data.frame(x=d2$x,y=par*d2$y,k=par)
  })})

#calculate SSE
errorsSH<-ddply(fitSH,.(subject,figure,sigma,n),function(d){
  d2<-subset(curveTransd6,
             curveTransd6$subject==unique(d$subject) &
               curveTransd6$figure==unique(d$figure))
  error<-sum((d$y-d2$y)^2)
  data.frame(error)
})

#look for the parameters that better fit the response functions
minErrorSH<-ddply(errorsSH,.(subject,figure),function(d){
  n<-d$n[d$error==min(d$error)]  
  sigma<-d$sigma[d$error==min(d$error)] 
  data.frame(n,sigma)
})
minErrorSHnoSurr<-subset(minErrorSH,minErrorSH$figure==NS)


#the scale factors for the best parameters
minErrorSHnoSurrK<-ddply(minErrorSHnoSurr,.(subject),function(d){
  ds<-subset(fitSH,fitSH$subject==d$subject & fitSH$n==d$n & 
               fitSH$figure==NS & fitSH$sigma==d$sigma)
  k<-unique(ds$k)
  data.frame(k)
})

# response functions for the best parameters for the no surround
fitSHnoSurroundaux<-subset(fitSH,fitSH$figure==NS)
fitSHnoSurround<-ddply(fitSHnoSurroundaux,.(subject),function(d){
  ds<-subset(minErrorSHnoSurr,minErrorSHnoSurr$subject==unique(d$subject))
  subset(d,d$n==ds$n & d$sigma==ds$sigma)
})

datSHDifsub2<-datSHDif

#we use the same scale factor for the sustained and no surround cond
fitSHk<-ddply(minErrorSHnoSurrK,.(subject),function(d){
  ddply(datSHDif,.(n,sigma),function(d2){
    y<-d2$y*d$k
    data.frame(x=d2$x,y)
  })
})

#we calculate the errors for each n and sigma
errorsSHk<-ddply(curveTransd6,.(subject,figure),function(d){
  fitSHkSubj<-subset(fitSHk,fitSHk$subject==unique(d$subject))
  ddply(fitSHkSubj,.(n,sigma),function(d2){
    error<-sum((d$y-d2$y)^2)
    data.frame(error)
  })
})

#look for the parameters that better fit the response functions
minErrorSHk<-ddply(errorsSHk,.(subject,figure),function(d){
  n<-d$n[d$error==min(d$error)]  
  sigma<-d$sigma[d$error==min(d$error)] 
  data.frame(n,sigma)
})

#the response functions
fitSHkmin<-ddply(minErrorSHk,.(subject,figure),function(d){
  ds<-subset(fitSHk,fitSHk$n==d$n & fitSHk$sigma==d$sigma &
               fitSHk$subject==d$subject)
  data.frame(x=ds$x,y=ds$y)
})
fitSHkminSurr<-subset(fitSHkmin,fitSHkmin$figure!=NS)

#creating the table of parameters
minErrorSHSynch<-subset(minErrorSHk,minErrorSHk$figure==SY)
minErrorSHSust<-subset(minErrorSHk,minErrorSHk$figure==SU)
minErrorTable<-rbind(minErrorSHnoSurr,minErrorSHSust,minErrorSHSynch)

pTransdSH<-ggplot()+
  facet_wrap(~subject,as.table=F)+
  geom_line(data=subset(curveTransdRange,subject!='KN'),
            aes(x=x,y=y,color=figure))+ 
  geom_point(data=subset(fitSHnoSurround,subject!='KN'),
             aes(x=x,y=y,color=figure))+
  geom_point(data=subset(fitSHkminSurr,subject!='KN'),
             aes(x=x,y=y,color=figure))+
  scale_colour_manual(values=color_values,
        labels=c('No surround','Synchronous\nsurround','Sustained\nsurround'))+
  xlab('Coherence')+ylab('Arbitrary response units')+
  scale_x_continuous(breaks=seq(0,.8,by=.2))+
  theme(legend.position=c(.88,.85),
        legend.title=element_blank(),
        axis.title.y=element_text(vjust=.25),
        axis.title.x=element_text(vjust=0),
        legend.key.height=unit(1.25,'lines'))
pTransdSH

minErrorTableNew<-minErrorTable[order(minErrorTable$subject),]
tableSH<-tableGrob(subset(minErrorTableNew,subject!='KN'),
                   gpar.coretext = gpar(fontsize=8),
                   cols=expression(observer,condition,n,sigma),
                   show.rownames = FALSE)

p7a<-arrangeGrob(pTransdSH,left=tG('A'))
p7b<-arrangeGrob(tableSH,left=tG('B'))
pdf('figures/Figure7.pdf',width=3.6,height=7.5)
grid.arrange(p7a,p7b,ncol=1,heights=c(.42,.58))
dev.off()

################
### Figure 8 ###
################
transdR<-ddply(th,.(subject,figure),fitTransdR)
curveTransdTh<-ddply(transdR,.(subject,figure),curveTransdFun)

# estimating the response up to coherence=.5+threhold for 0.5 (No surround)
curveTransdRangeTh<-ddply(curveTransdTh,.(subject),function(d){
  subTh<-subset(th,subject==unique(d$subject) & cohComparison==.5 &
                  figure==NS)
  subset(d,x<subTh$thre)
})

#scaling
curveTransdRangeThSca<-ddply(curveTransdRangeTh,.(subject),function(d){
  dSub<-subset(curveTransdRange,subject==unique(d$subject))
  minimization<-function(para){  
    y1<-para*d$y
    y2<-dSub$y
    return(sum((y1-y2)^2))
  }  
  data.frame(par=optim(c(.01),minimization)$par)
})

curveTransdRangeThNorm<-ddply(curveTransdRangeTh,.(subject,figure),function(d){
  para<-subset(curveTransdRangeThSca,subject==unique(d$subject))$par
  print(para)
  y<-d$y*para
  data.frame(x=d$x,y)
})

pTransd2<-ggplot()+
  facet_wrap(~subject,as.table=F)+
  geom_line(data=subset(curveTransdRange,subject!='KN'),
            aes(x=x,y=y,color=figure),lty=2)+ 
  geom_line(data=subset(curveTransdRangeThNorm,subject!='KN'),
            aes(x=x,y=y,color=figure))+ 
  scale_colour_manual(values=color_values,
        labels=c('No surround','Synchronous\nsurround','Sustained\nsurround'))+
  xlab('Coherence')+ylab('Arbitrary response units')+
  scale_x_continuous(breaks=seq(0,.8,by=.2))+
  theme(legend.position=c(.85,.85),
        legend.title=element_blank(),
        axis.title.y=element_text(vjust=.25),
        axis.title.x=element_text(vjust=0),
        legend.key.height=unit(1.5,'lines'))
pTransd2

pdf('figures/Figure8.pdf',width=3.6,height=3.5)
pTransd2
dev.off()





