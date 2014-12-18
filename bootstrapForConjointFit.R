nSamples<-1
curveAllPred<-ddply(cohComparisons,.(cohComparison),curveAllfunPred)
avBoot<-ddply(av,.(subject,figure,cohComparison),function(d){
  subCurveAllPred<-subset(curveAllPred,subject==unique(d$subject) &
        figure==unique(d$figure) & cohComparison==unique(d$cohComparison))
  yPred<-subCurveAllPred$y #parametric bootstrap
  samples<-ddply(data.frame(sample=1:nSamples),.(sample),function(f){
    nyes<-rbinom(length(d$x),d$n,yPred)   
    nno<-d$n-nyes
    data.frame(x=d$x,nyes,nno,n=d$n,y=nyes/d$n)
  })
  samples
})
fitAllBoot<-ddply(avBoot,.(subject,sample),.progress ='text',fitAllfun)

write.table(fitAllBoot,'fitAllBoot.txt')
