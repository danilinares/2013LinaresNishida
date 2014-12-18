library('plyr')
library('ggplot2')
library('gridExtra')
library('png')
library('RColorBrewer')
library('scales')
library('PropCIs')
library('modelfree')

### read data ##################################################################
read.files<-function(path='data',subject='.',exp='.',session='.') {
  subject<-gsub(',',replace='|',subject)
  exp<-gsub(',',replace='|',exp)
  session<-gsub(',',replace='|',session)
  r.exp<-paste('^(',subject,').*','(',exp,').*','[',session,']',sep='')
  print(path)
  names.files<-list.files(pattern=r.exp,path=path,full.names=TRUE)
  print(names.files)
  readfun<-function(x) {
    y<-gsub( paste('(',path,'/)|(\\.txt)',sep="") ,'',x)
    subj<-substr(y,0,2)
    session<-gsub('[a-zA-Z]','',y)
    exp<-gsub( paste('(',session,')|(',subj,')',sep='') ,'',y)
    df<-read.table(x, header = TRUE)
    df$subject<-toupper(subj); df$exp<-exp; df$session<-session
    return(df)
  }
  data<- do.call('rbind', lapply(names.files, readfun))
  data
}
### tuning graphs  #############################################################
color_values<-c(brewer.pal(9, "Set3")[7],brewer.pal(9, "Set1")[1],
                brewer.pal(9, "Set1")[2])
theme_set(theme_bw(10))
tG<-function(t) textGrob(t,x=0,y=1,hjust=0,vjust=1,gp=gpar(fontsize=14))
mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=='subject') { 
    value[value=='DL'] <- 'Observer DL'
    value[value=='SN'] <- 'Observer SN'
    value[value=='MT'] <- 'Observer MT'
    value[value=='MS'] <- 'Observer MS'
    value[value=='RH'] <- 'Observer RH'
    value[value=='KN'] <- 'Observer KN'
  }
  return(value)
}
### averages ###################################################################
averages<-function(df) {
  n<-length(df$response)
  nyes<-sum(df$response)
  nno<-n-nyes
  y<-nyes/n
  ci<-exactci(nyes,n,.95)$conf.int
  ymin<-ci[[1]]; ymax<-ci[[2]]
  data.frame(nyes,nno,n,y,ymin,ymax)
}
### MLE independent ############################################################
cumNorm<-function(x,p,m) (1/m)+(1-1/m)*pnorm(x,p[1],p[2]) #m choices
cumNormQ<-function(y,p,m) qnorm((y-(1/m))/(1-1/m),p[1],p[2])

likeL<-function(p,d,FUN,m){
  pr<-FUN(d$x,p,m)
  -sum(d$nyes*log(pr)+d$nno*log(1-pr))
}
curveFun<-function(pIni,d,FUN,m,xmin=min(d$x),xmax=max(d$x)){ 
  p<-optim(pIni,likeL,d=d,FUN=FUN,m=m)$par
  xseq<-seq(xmin,xmax,len=100) 
  yseq<-FUN(xseq,p,m)
  data.frame(x=xseq,y=yseq)
}
threFun<-function(pIni,d,FUN,FUN2,ythre,m,nsamples=1000){ 
  p<-optim(pIni,likeL,d=d,FUN=FUN,m=m)$par
  thre<-FUN2(ythre,p,m)
  
  yPred<-FUN(d$x,p,m) #parametric bootstrap
  dev<-deviance2(d$nyes,d$n,yPred)
  samples<-ddply(data.frame(sample=1:nsamples),.(sample),function(f){
    nyes<-rbinom(length(d$x),d$n,yPred)   
    nno<-d$n-nyes
    data.frame(x=d$x,nyes,nno,n=d$n)
  })
  threBoot<-ddply(samples,.(sample),function(d){
    p<-optim(pIni,likeL,d=d,FUN=FUN,m=m)$par
    thre<-FUN2(ythre,p,m)
    dev<-deviance2(d$nyes,d$n,yPred)
    data.frame(thre,dev)
  })
  Qdev<-quantile(threBoot$dev,c(.025,.975))  
  okDev<-dev>=Qdev[[1]] & dev<=Qdev[[2]]
  
  Q<-quantile(threBoot$thre,c(.025,.975))   
  data.frame(thre,threMin=Q[[1]],threMax=Q[[2]],
             dev,devMin=Qdev[[1]],devMax=Qdev[[2]],okDev)
}
difFun<-function(pIni,d,FUN,FUN2,ythre,m,nsamples=1000,condition){ 
  thre<-ddply(d,condition,function(d2){
    p<-optim(pIni,likeL,d=d2,FUN=FUN,m=m)$par
    thre<-FUN2(ythre,p,m)
    data.frame(thre)
  })
  dif=-diff(thre$thre)
  
  thBoot<-ddply(d,condition,function(d2){
    p<-optim(pIni,likeL,d=d2,FUN=FUN,m=m)$par
    yPred<-FUN(d2$x,p,m) #parametric bootstrap
    samples<-ddply(data.frame(sample=1:nsamples),.(sample),function(f){
      nyes<-rbinom(length(d2$x),d2$n,yPred)   
      nno<-d2$n-nyes
      data.frame(x=d2$x,nyes,nno,n=d2$n)
    })
    threBoot<-ddply(samples,.(sample),function(d2){
      p<-optim(pIni,likeL,d=d2,FUN=FUN,m=m)$par
      thre<-FUN2(ythre,p,m)
      data.frame(thre)
    })
    threBoot
  })
  condi<-unique(d[[condition]])
  difBoot<-thBoot$thre[thBoot[[condition]]==condi[1]]-
    thBoot$thre[thBoot[[condition]]==condi[2]]
  Q<-quantile(difBoot,c(.025,.975))
  data.frame(dif,difMin=Q[[1]],difMax=Q[[2]])
}
### MLE conjoint ###############################################################
transd<-function(p,x){
  f<-function(x) {  
    M<-p[1]; N<-p[2]; a<-p[3]
    num<-M*x^a; den<-x^a+N
    return(num/den)
  }
  laply(x,function(x) f(x))
}
lAll<-function (p, d) {
  P<-expand.grid(transd.par=1:3,figure=conditions)
  noise.par<-tail(p,1)
  transd.par<-head(p,-1)   
  P$transd.par<-transd.par    
  negLog<-ddply(d,.(cohComparison,figure),function(d2){
    fig<-unique(d2$figure)
    mu1<-transd(P$transd.par[P$figure==fig],d2$x)
    mu2<-transd(P$transd.par[P$figure==fig],d2$cohComparison)
    pr<-laply(mu1-mu2,function(x) pnorm(0,mean=-x,sd=sqrt(2)*noise.par))
    negLog<- -sum(d2$nyes*log(pr)+d2$nno*log(1-pr))
    data.frame(negLog)
  })
  return(sum(negLog$negLog))
}
fitAllfun<-function(d) { 
  P<-expand.grid(transd.par=1:3,figure=conditions)
  pIni<-c(rep(1,3*length(conditions)),.1)
  p<-optim(par=pIni,lAll,d=d)$par
  pNoise<-tail(p,1)
  pTransd<-head(p,-1)
  parameter<-rep(c('M','N','a'),3)
  data.frame(p=pTransd,parameter,figure=P$figure,noise=pNoise)
}
curveTransdFun<-function(d,samples=100){
  xseq<-seq(0,1,len=samples)
  yseq<-transd(d$p,xseq)
  data.frame(x=xseq,y=yseq)
}
inv.transd<-function(p,y) {
  f<-function(x) return(y-transd(p,x))
  return(uniroot(f,lower=0,upper=10)$root)
}
threAllfun<-function(d){
  ddply(fitAll,.(subject,figure),.progress='text',function(d2){
    noise.par<-unique(d2$noise)
    cohComparison<-unique(d$cohComparison)
    subject<-unique(d2$subject)
    coh<-av$x[av$subject==subject & av$cohComparison==cohComparison] 
    print(noise.par)
    mu1Mmu2<-qnorm(ythre,mean=0,sd=sqrt(2)*noise.par)
    mu2<-transd(d2$p,d$cohComparison)
    mu1<-mu1Mmu2+mu2
    thre<-inv.transd(d2$p,mu1)
    data.frame(thre)
  })
}
fitTransdR<-function(d){
  fun.min<-function(p,d) {
    y1<-transd(p,d$thre)
    y2<-transd(p,d$cohComparison)
    return( sum((y1-y2-1)^2) )
  }
  p<-optim(c(5,1,2),fun.min,d=d)$par
  data.frame(p)
}
curveDerivFunAll<-function(d){ 
  xend=.5
  ddply(data.frame(xseq= seq(0,xend,len=50)),.(xseq),function(d2){
    f<-function(inc.x) {
      y1<-transd(d$p,d2$xseq+inc.x)
      y2<-transd(d$p,d2$xseq)
      return(y1-y2-1)
    }
    yseq<-uniroot(f, c(0.,10))$root 
    data.frame(x=d2$xseq,y=yseq)
  })
}
threAllfunBoot<-function(d){
  ddply(fitAllBoot,.(subject,figure,sample),.progress='text',function(d2){
    noise.par<-unique(d2$noise)
    cohComparison<-unique(d$cohComparison)
    subject<-unique(d2$subject)
    coh<-av$x[av$subject==subject & av$cohComparison==cohComparison] 
    mu1Mmu2<-qnorm(ythre,mean=0,sd=sqrt(2)*noise.par)
    mu2<-transd(d2$p,d$cohComparison)
    mu1<-mu1Mmu2+mu2
    tryCatch({ thre<-inv.transd(d2$p,mu1) 
               data.frame(thre)},
             error=function(e) 
               message(paste('Difficulties to find threshold sample:',unique(d2$sample),'\n'))
    )
  })
}
fitFun<-function(pIni,d,FUN,m,nsamples=1000){ 
  p<-optim(pIni,likeL,d=d,FUN=FUN,m=m)$par
  
  yPred<-FUN(d$x,p,m) #parametric bootstrap
  samples<-ddply(data.frame(sample=1:nsamples),.(sample),function(f){
    nyes<-rbinom(length(d$x),d$n,yPred)   
    nno<-d$n-nyes
    data.frame(x=d$x,nyes,nno,n=d$n)
  })
  pBoot<-ddply(samples,.(sample),function(d){
    p<-optim(pIni,likeL,d=d,FUN=FUN,m=m)$par
    data.frame(p,idP=seq(1,length(p)))
  })
  dfCI<-ddply(pBoot,.(idP),function(d) {
    Q<-quantile(d$p,c(.025,.975))  
    data.frame(pmin=Q[[1]],pmax=Q[[2]])
  })
  dfP<-data.frame(p,idP=seq(1,length(p)))
  return(merge(dfP,dfCI))
}

curveAllfun<-function(d){
  ddply(fitAll,.(subject,figure),function(d2){
    noise.par<-unique(d2$noise)
    cohComparison<-unique(d$cohComparison)
    subject<-unique(d2$subject)
    if (sum(av$subject==subject & av$cohComparison==cohComparison)!=0) {
      coh<-av$x[av$subject==subject & av$cohComparison==cohComparison] 
      xseq<-seq(min(coh),max(coh),len=100)
      mu1<-transd(d2$p,xseq)
      mu2<-transd(d2$p,rep(d$cohComparison,100))
      yseq<-laply(mu1-mu2,function(x) pnorm(0,mean=-x,sd=sqrt(2)*noise.par))
      data.frame(x=xseq,y=yseq)
    }
  })
}

curveAllfunPred<-function(d){
  ddply(fitAll,.(subject,figure),function(d2){
    noise.par<-unique(d2$noise)
    cohComparison<-unique(d$cohComparison)
    subject<-unique(d2$subject)
    coh<-av$x[av$subject==subject & av$cohComparison==cohComparison
              & av$figure==unique(d2$figure)] 
    xseq<-coh
    mu1<-transd(d2$p,xseq)
    mu2<-transd(d2$p,rep(d$cohComparison,length(coh)))
    yseq<-laply(mu1-mu2,function(x) pnorm(0,mean=-x,sd=sqrt(2)*noise.par))
    data.frame(x=xseq,y=yseq)
  })
}







