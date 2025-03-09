#' Generate Student Data From File
#'
#' The DataSimulatoR function creates data files (.csv) for students and grading keys (.pdf) based off of student responses to survey found on github.com/LiamOMueller/UCSanDiego_BILD5
#' @param x A data frame object from the student survey. Number of rows should be equal to number of responses, Number of columns should be equal to 33.
#' @return A series of csv files for students and pdf grading keys
#' @import cowplot
#' @import rockchalk
#' @import tidyr
#' @import ggplot2
#' @export
DataSimulatoR <- function(x){
BILD_Data<-x
names<-paste(BILD_Data[,5],".csv",sep="") #Names to give write.csv later on.
fignames<-paste(BILD_Data[,3],"_",BILD_Data[,2],".pdf",sep="")#Names for the key figures



for(i in 1:length(BILD_Data[,4])){
  set.seed(BILD_Data[i,4])

   #####Two Sample T#####

  if(BILD_Data[i,7]=="2 sample t-test"){
    if(BILD_Data[i,4] %% 2==0){ #Gonna nest another if/else in here
      if(BILD_Data[i,29]<0){
        minX <-BILD_Data[i,29] #Define from table
        muX <- BILD_Data[i,31]  #Define from table
        sdX <- abs((muX-minX)/5)
        delta<-sdX
      }else{
      minX<-log(BILD_Data[i,29]+0.001)
      muX<-log(BILD_Data[i,31]+0.001)
      sdX <- abs((muX)/12) #Making it smaller in the transformed case. Should fix values being too high.
      delta<-sdX*2 #But delta needs to be the same regardless of transformation
    }}else{
      minX <-BILD_Data[i,29] #Define from table
      muX <- BILD_Data[i,31]  #Define from table
      sdX <- abs((muX-minX)/5)
      delta<-sdX
    }
    n<-BILD_Data[i,32]

    muY <- muX + abs(rnorm(1, 0.9*delta, delta))

    nameofX <- BILD_Data[i,15] #define from table
    nameofY <- BILD_Data[i,14] #define from table
    X <- rnorm(n, muX, sdX) #creating X
    Y <- rnorm(n, muY, sdX) #creating Y
    rawvals <- cbind(X, Y) #creating data.frame

    #Modify Y to make it harder

    tempY <- (rawvals[,2])
    tempX <- (rawvals[,1])
    if(BILD_Data[i,4] %% 2==0){ #If your PID is even, you get to do a log transformation
      transformY<-exp(tempY)
      transformX<-exp(tempX)
    } else{ #otherwise, you are getting an outlier 10 times the mean.
      transformY<-tempY
      transformY[round(length(tempY)/4)]<-muY*10.2
      transformX <- tempX
    }

    ####Remove negative values if the x min is >= o
    if(BILD_Data[i,29]>=0){ #If the min of x is equal to or bigger than 0, the X data gets abs()
      finalX<-abs(transformX)
      finalY<-abs(transformY)
    }else{ # otherwise, the min of x is less than zero and we don't have to worry.
      finalX<-(transformX)
      finalY<-(transformY)
    }

    dataframe<-data.frame(finalX,finalY)
    colnames(dataframe)<-c(nameofX,nameofY)
    write.csv(x = dataframe,file = names[i])

     #Make the key

    resultX <- finalX
    resultY <- finalY
    resultdata <- cbind(resultX,resultY)
    if(BILD_Data[i,4] %% 2!=0){ #remove the outlier that was put in if the pid was odd, else transform exp data
      outlierrow<-(round(length(resultdata[,2])/4))
      resultY[outlierrow]<-NA #The outlier is always in Y
      resdat<-cbind(resultX,resultY) #
      #test for normal
    Tidydata<-pivot_longer(as.data.frame(resultdata),1:2,names_to = "IV",values_to = "DV")
    Rawresiduals<-lm(Tidydata$DV~Tidydata$IV)$residuals
    restidydat<-pivot_longer(as.data.frame(resdat),1:2,names_to = "IV",values_to = "DV")
    Resresiduals<-lm(restidydat$DV~restidydat$IV)$residuals
    ksRAW<-ks.test(Rawresiduals,pnorm,mean(Rawresiduals),sd(Rawresiduals))$p.value #KS the residuals
    ksRES<-ks.test(Resresiduals,pnorm,mean(Resresiduals),sd(Resresiduals))$p.value #KS the residuals
    }else{
      resultX<-log(resultX)
      resultY<-log(resultY)
      resdat<-cbind(resultX,resultY)
      #test for normal
      Tidydata<-pivot_longer(as.data.frame(resultdata),1:2,names_to = "IV",values_to = "DV")
      Rawresiduals<-lm(Tidydata$DV~Tidydata$IV)$residuals
      restidydat<-pivot_longer(as.data.frame(resdat),1:2,names_to = "IV",values_to = "DV")
      Resresiduals<-lm(restidydat$DV~restidydat$IV)$residuals
      ksRAW<-ks.test(Rawresiduals,pnorm,mean(Rawresiduals),sd(Rawresiduals))$p.value #KS the residuals
      ksRES<-ks.test(Resresiduals,pnorm,mean(Resresiduals),sd(Resresiduals))$p.value
    }



    model<-t.test(resdat[,1],resdat[,2])
    MeanDelta<-model$estimate[1]-model$estimate[2]
    tscore<-model$statistic
    pvalue<-model$p.value
    UCI<- model$conf.int[2]
    LCI<- model$conf.int[1]

    #make the figures

    figdata<-as.data.frame(resdat)
    RawresidualsTestFigs<-data.frame(Rawresiduals)
    ResresidualsTestFigs<-data.frame(Resresiduals)

    tidyfigdata<-tidyr::pivot_longer(data = figdata[,1:2],cols = 1:2,names_to = "group",values_to = "response")
    if(BILD_Data[i,32]<40){
      bins<-15
    }else{
      bins<-30
    }
    histX <- ggplot2::ggplot(RawresidualsTestFigs, aes(x=Rawresiduals))+
      geom_histogram(bins = bins,fill="navyblue")+
      theme_minimal()+
      xlab("Untransformed Residuals")
    histY<-ggplot2::ggplot(ResresidualsTestFigs,aes(x=Resresiduals))+
      geom_histogram(bins = bins,fill="navyblue")+
      theme_minimal()+
      xlab("Transfomred Residuals")
    fig<-ggplot2::ggplot(tidyfigdata, aes(x=group,y = response))+
      geom_boxplot()+
      theme_classic()+
      scale_x_discrete(labels=c("resultX" = nameofX,"resultY"=nameofY))+
      xlab("Group")+
      ylab("Response")
    #Now lets make some text go in this box and then we have a little mini key!
    if(BILD_Data[i,4] %% 2==0){
      problem<-"log Transform"
    }else{
      problem<-"Outlier"
    }
    xaxis<-1:10
    yaxis<-1:10
    tplotdat<-data.frame(xaxis,yaxis)
    text<-ggplot2::ggplot(tplotdat,aes(x=xaxis,y=yaxis))+
      annotate("text",x = 2,y = 10,label="Tests for Normallity")+
      annotate("text",x = 3,y = 9.5,label=round(ksRAW,4))+
      annotate("text",x = 1.2,y = 9.5,label="p val of raw =")+
      annotate("text",x = 3,y = 9,label=round(ksRES,4))+
      annotate("text",x = 1.2,y = 9,label="Transformed =")+
      annotate("text",x = 6,y = 10,label="Coefficents")+
      annotate("text",x = 5.7,y = 9.5,label="delta X = ")+
      annotate("text",x = 6,y = 9,label="t stat =")+
      annotate("text",x = 7.5,y = 9.5,label=round(MeanDelta,5))+
      annotate("text",x = 7.5,y = 9,label=round(tscore,5))+
      annotate("text",x = 2,y = 7.5,label="Model p-value:")+
      annotate("text",x = 2.5,y = 7,label=round(pvalue,8))+
      annotate("text",x = 7,y = 7.5,label="Model 95 CIs:")+
      annotate("text",x = 7,y = 7,label=UCI)+
      annotate("text",x = 7,y = 6.5,label=LCI)+
      annotate("text",x = 2,y = 5,label="Problem with Data:")+
      annotate("text",x = 2,y = 4.5,label=problem)+
      annotate("text",x = 7,y = 5,label="Test run:")+
      annotate("text",x = 7,y = 4.5,label="Two sample T")+
      theme_void()+
      theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 2))+
      xlim(c(0,10))+
      ylim(c(0,10))

    plotgrid<-cowplot::plot_grid( histX,histY,fig,text,nrow = 2,ncol = 2)
    ggplot2::ggsave(filename = fignames[i],plot = plotgrid,width = 10,height = 10,units = "in")
  }
   #####ANOVA#####
  if(BILD_Data[i,7]=="ANOVA (3 levels)"){
    if(BILD_Data[i,4] %% 2==0){
      minZ<-log(BILD_Data[i,29]+0.001) #Define your min value
      muZ<-log(BILD_Data[i,31]+0.001) #Define your mu
      sdZ <- abs((muZ)/12) #Making it smaller in the transformed case. Should fix values being too high.
      delta<-sdZ*2 #But delta needs to be the same regardless of transformation
    }else{
      minZ <-BILD_Data[i,29] #Define from table
      muZ <- BILD_Data[i,31]  #Define from table
      sdZ <- abs((muZ-minZ)/5)
      delta<-sdZ
    }
   n<-BILD_Data[i,32]
   muX <- muZ + abs(rnorm(1, .35*delta, 0.3*delta))
   muY <- muZ - abs(rnorm(1, .35*delta, 0.3*delta))
   nameofX <- BILD_Data[i,16] #define from table Should be the highest value
   nameofY <- BILD_Data[i,17] #define from table Should be the lowest value
   nameofZ <- BILD_Data[i,18] #define from table Should be the mid value
   X <- rnorm(n, muX, sdZ) #creating X
   Y <- rnorm(n, muY, sdZ) #creating Y
   Z <- rnorm(n, muZ, sdZ) #creating Y
   rawvals <- cbind(X, Y, Z) #creating data.frame

   #Modify X, Y, Z to make it harder
   if(BILD_Data[i,4] %% 2==0){ #If your PID is even, you get to do a log transformation
     transformY<-exp(Y)
     transformX<-exp(X)
     transformZ<-exp(Z)
   } else{ #otherwise, you are getting an outlier 10 times the mean.
     transformY<-Y
     transformY[round(length(Y)/4)]<-muY*10.2
     transformZ<-Z
     transformX <- X

   }

   ####Remove negative values if the x min is >= o
   if(BILD_Data[i,29]>=0){ #If the min of x is equal to or bigger than 0, the X data gets abs()
     finalX<-abs(transformX)
     finalY<-abs(transformY)
     finalZ<-abs(transformZ)
   }else{ # otherwise, the min of x is less than zero and we don't have to worry.
     finalX<-(transformX)
     finalY<-(transformY)
     finalZ<-(transformZ)
   }
   dataframe<-data.frame(finalX,finalY, finalZ)
   colnames(dataframe)<-c(nameofX,nameofY,nameofZ)

   write.csv(x = dataframe,file = names[i])

   #Make the key

   resultX <- finalX
   resultY <- finalY
   resultZ <- finalZ
   resultdata <- data.frame(resultX,resultY,resultZ)
   tidyresults<- tidyr::pivot_longer(resultdata,1:3,names_to = "IV",values_to = "DV")
   if(BILD_Data[i,4] %% 2!=0){ #remove the outlier that was put in if the pid was odd, else transform exp data, Outlers are in the Y
     outlierrow<-which.max(tidyresults$DV)
     resdat<-tidyresults[-outlierrow,]
   }else{
     resultX<-log(resultX)
     resultY<-log(resultY)
     resultZ<-log(resultZ)
     untidyresdat<-data.frame(resultX,resultY,resultZ)
     resdat<-tidyr::pivot_longer(untidyresdat,1:3,names_to = "IV",values_to = "DV")
   }
   #test for normal
   BadmodResiduals <-lm(tidyresults$DV~tidyresults$IV)$residuals
   GoodmodResidulas<-lm(resdat$DV~resdat$IV)$residuals
   ksRAW<-ks.test(BadmodResiduals,pnorm,mean(BadmodResiduals),sd(BadmodResiduals))$p.value
   ksRes<-ks.test(GoodmodResidulas,pnorm,mean(GoodmodResidulas),sd(GoodmodResidulas))$p.value




   tidydata<-resdat

   model<-lm(tidydata$DV~tidydata$IV)
   modsum<-summary(model)
   modaov<-anova(model)
   rsquare<-modsum$r.squared
   pvalue<-modaov$`Pr(>F)`[1]
   fstat<-modaov$`F value`[1]
  #Make the tukeytest
   tukeydata<-tidydata
   tukeydata$IV<-replace(tukeydata$IV,tukeydata$IV=="resultX",nameofX)
   tukeydata$IV<-replace(tukeydata$IV,tukeydata$IV=="resultY",nameofY)
   tukeydata$IV<-replace(tukeydata$IV,tukeydata$IV=="resultZ",nameofZ)
    aovmod<-aov(tukeydata$DV~tukeydata$IV)
   Tukeymod<-TukeyHSD(aovmod)
   Tukeykey<-Tukeymod$`tukeydata$IV`
   #make the figures

   #Make residuals DF
   Badresids<-data.frame(BadmodResiduals)
   Goodresids<-data.frame(GoodmodResidulas)

   if(BILD_Data[i,32]<40){
     bins<-15
   }else{
     bins<-30
   }
   histX <- ggplot2::ggplot(Badresids, aes(x=BadmodResiduals))+
     geom_histogram(bins = bins,fill="navyblue")+
     theme_minimal()+
     xlab("Untransformed residuals")
   histY<-ggplot2::ggplot(Goodresids,aes(x=GoodmodResidulas))+
     geom_histogram(bins = bins,fill="navyblue")+
     theme_minimal()+
     xlab("Transformed residuals")
   histZ<-ggplot2::ggplot()+
     theme_void()
   fig<-ggplot2::ggplot(tidydata, aes(x=IV,y = DV))+
     geom_boxplot()+
     theme_classic()+
     scale_x_discrete(labels=c("resultX" = nameofX,"resultY"=nameofY,"resultZ"=nameofZ))+
     xlab("Group")+
     ylab("Response")
   #Now lets make some text go in this box and then we have a little mini rubric!
   if(BILD_Data[i,4] %% 2==0){
     problem<-"log Transform"
   }else{
     problem<-"Outlier"
   }
   xaxis<-1:10
   yaxis<-1:10
   tplotdat<-data.frame(xaxis,yaxis)
   text<-ggplot2::ggplot(tplotdat,aes(x=xaxis,y=yaxis))+
     annotate("text",x = 2,y = 10,label="Tests for Normallity")+
     annotate("text",x = 3,y = 9.5,label=round(ksRAW,4))+
     annotate("text",x = 1.2,y = 9.5,label="p val of Raw =")+
     annotate("text",x = 3,y = 9,label=round(ksRes,4))+
     annotate("text",x = 1.2,y = 9,label="Transformed =")+
     annotate("text",x = 6,y = 10,label="Coefficents")+
     annotate("text",x = 6,y = 9.5,label="F stat =")+
     annotate("text",x = 7.5,y = 9.5,label=round(fstat,5))+
     annotate("text",x = 2,y = 7.5,label="Model p-value:")+
     annotate("text",x = 2.5,y = 7,label=round(pvalue,8))+
     annotate("text",x = 7,y = 7.5,label="r squared")+
     annotate("text",x = 7,y = 7,label=round(rsquare,8))+
     annotate("text",x = 2,y = 5,label="Problem with Data:")+
     annotate("text",x = 2,y = 4.5,label=problem)+
     annotate("text",x = 7,y = 5,label="Test run:")+
     annotate("text",x = 7,y = 4.5,label="ANOVA")+
     theme_void()+
     theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 2))+
     xlim(c(0,10))+
     ylim(c(0,10))

 #Add a tukey test
   text2<-ggplot2::ggplot(tplotdat,aes(x=xaxis,y=yaxis))+
     annotate("text",x = 5,y = 10,label="Tukey Honest Significant Difference")+
     annotate("text",x = 2,y = 9,label="Comparison")+
     annotate("text",x = 2,y = 8,label=paste(nameofZ,nameofX,sep = "-"))+
     annotate("text",x = 2,y = 7.5,label=paste(nameofY,nameofX,sep = "-"))+
     annotate("text",x = 2,y = 7,label=paste(nameofY,nameofZ,sep = "-"))+
     annotate("text",x = 5,y = 9,label="Difference")+
     annotate("text",x = 5,y = 8,label=round(Tukeykey[1,1],4))+
     annotate("text",x = 5,y = 7.5,label=round(Tukeykey[2,1],4))+
     annotate("text",x = 5,y = 7,label=round(Tukeykey[3,1],4))+

     annotate("text",x = 8,y = 9,label="p-value")+
     annotate("text",x = 8,y = 8,label=round(Tukeykey[1,4],7))+
     annotate("text",x = 8,y = 7.5,label=round(Tukeykey[2,4],7))+
     annotate("text",x = 8,y = 7,label=round(Tukeykey[3,4],7))+

     theme_void()+
     theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 2))+
     xlim(c(0,10))+
     ylim(c(4,10))

   plotgrid<-cowplot::plot_grid( histX,histY,histZ,fig,text,text2,nrow = 2,ncol = 3)
   ggplot2::ggsave(filename = fignames[i],plot = plotgrid,width = 15,height = 10,units = "in")
   }

   #####Paired t#####

  if(BILD_Data[i,7]=="Paired t-test"){
    if(BILD_Data[i,4] %% 2==0){
      minX<-log(BILD_Data[i,29]+0.001)
      muX<-log(BILD_Data[i,31]+0.001)
      sdX <- abs((muX)/12) #Making it smaller in the transformed case. Should fix values being too high.
      delta<-sdX*2 #But delta needs to be the same regardless of transformation
    }else{
      minX <-BILD_Data[i,29] #Define from table
      muX <- BILD_Data[i,31]  #Define from table
      sdX <- abs((muX-minX)/5)
      delta<-sdX
    }
    n<-BILD_Data[i,32]
    tempX<-rnorm(n=n,mean = muX,sd = sdX)
    tempdelta<-rnorm(n=n,mean = .9*delta, sd = .3*delta)
    tempY<-tempX-tempdelta
    nameofX <- BILD_Data[i,14] #define from table
    nameofY <- BILD_Data[i,15] #define from table

    #Modify Y to make it harder
    if(BILD_Data[i,4] %% 2==0){ #If your PID is even, you get to do a log transformation
      transformY<-exp(tempY)
      transformX<-exp(tempX)
    } else{ #otherwise, you are getting an outlier 10 times the mean.
      transformY<-tempY
      transformY[round(length(tempY)/4)]<-muX*10.2
      transformX <- tempX
    }

    ####Remove negative values if the x min is >= o
    if(BILD_Data[i,29]>=0){ #If the min of x is equal to or bigger than 0, the X data gets abs()
      finalX<-abs(transformX)
      finalY<-abs(transformY)
    }else{ # otherwise, the min of x is less than zero and we don't have to worry.
      finalX<-(transformX)
      finalY<-(transformY)
    }

    dataframe<-data.frame(finalX,finalY)
    colnames(dataframe)<-c(nameofX,nameofY)

    write.csv(x = dataframe,file = names[i])

    #Make the key

    resultX <- finalX
    resultY <- finalY
    resultdata <- cbind(resultX,resultY)
    predelta<-finalX-finalY
    if(BILD_Data[i,4] %% 2!=0){ #remove the outlier that was put in if the pid was odd, else transform exp data
      outlierrow<-(round(length(resultdata[,2])/4))
      resdat<-resultdata[-outlierrow,]
    }else{
      resultX<-log(resultX)
      resultY<-log(resultY)
      resdat<-cbind(resultX,resultY)
    }

    #test for normal
    deltacol<-(resdat[,1]-resdat[,2])
    resdat<-cbind(resdat,deltacol)
    ksXpval<-ks.test(resdat[,1],pnorm,mean(resdat[,1]),sd(resdat[,1]))$p.value
    ksYpval<-ks.test(resdat[,2],pnorm,mean(resdat[,2]),sd(resdat[,2]))$p.value
    ksDeltapval<-ks.test(resdat[,3],pnorm,mean(resdat[,3]),sd(resdat[,3]))$p.value
    ksPreDelta<-ks.test(predelta,pnorm,mean(predelta),sd(predelta))$p.value

    model<-t.test(resdat[,1],resdat[,2],paired = TRUE)
    MeanDelta<-model$estimate
    tscore<-model$statistic
    pvalue<-model$p.value
    UCI<- model$conf.int[2]
    LCI<- model$conf.int[1]

    #make the figures

    figdata<-as.data.frame(resdat)
    rawdat<-as.data.frame(predelta)
    tidyfigdata<-tidyr::pivot_longer(data = figdata[,1:2],cols = 1:2,names_to = "group",values_to = "response")
    if(BILD_Data[i,32]<40){
      bins<-15
    }else{
      bins<-30
    }
    histdelta <- ggplot2::ggplot(figdata, aes(x=deltacol))+
      geom_histogram(bins = bins,fill="navyblue")+
      theme_minimal()+
      xlab('Fixed deltas')
    histpredelta<-ggplot2::ggplot(rawdat,aes(x=predelta))+
      geom_histogram(bins = bins,fill="navyblue")+
      theme_minimal()+
      xlab('Raw deltas')
    fig<-ggplot2::ggplot(figdata, aes(y = deltacol))+
      geom_boxplot()+
      theme_classic()+
      geom_hline(yintercept=0, linetype="dashed",
                 color = "red", size=2)+
      xlab("Delta")+
      ylab("Response")
    #Now lets make some text go in this box and then we have a little mini rubric!
    if(BILD_Data[i,4] %% 2==0){
      problem<-"log Transform"
    }else{
      problem<-"Outlier"
    }
    xaxis<-1:10
    yaxis<-1:10
    tplotdat<-data.frame(xaxis,yaxis)
    text<-ggplot2::ggplot(tplotdat,aes(x=xaxis,y=yaxis))+
      annotate("text",x = 2,y = 10,label="Tests for Normallity")+
      annotate("text",x = 3,y = 9.5,label=round(ksPreDelta,4))+
      annotate("text",x = 1.5,y = 9.5,label="Raw Data p val =")+
      annotate("text",x = 3,y = 9,label=round(ksDeltapval,4))+
      annotate("text",x = 1,y = 9,label="Transformed =")+
      annotate("text",x = 6,y = 10,label="Coefficents")+
      annotate("text",x = 5.7,y = 9.5,label="delta X = ")+
      annotate("text",x = 6,y = 9,label="t stat =")+
      annotate("text",x = 7.5,y = 9.5,label=round(MeanDelta,5))+
      annotate("text",x = 7.5,y = 9,label=round(tscore,5))+
      annotate("text",x = 2,y = 7.5,label="Model p-value:")+
      annotate("text",x = 2.5,y = 7,label=round(pvalue,8))+
      annotate("text",x = 7,y = 7.5,label="Model 95 CIs:")+
      annotate("text",x = 7,y = 7,label=UCI)+
      annotate("text",x = 7,y = 6.5,label=LCI)+
      annotate("text",x = 2,y = 5,label="Problem with Data:")+
      annotate("text",x = 2,y = 4.5,label=problem)+
      annotate("text",x = 7,y = 5,label="Test run:")+
      annotate("text",x = 7,y = 4.5,label="Paired T")+
      theme_void()+
      theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 2))+
      xlim(c(0,10))+
      ylim(c(0,10))

    plotgrid<-cowplot::plot_grid( histpredelta,histdelta,fig,text,nrow = 2,ncol = 2)
    ggplot2::ggsave(filename = fignames[i],plot = plotgrid,width = 10,height = 10,units = "in")
  }
   #####Chi Squared#####
  if(BILD_Data[i,7]=="Chi-Squared test"){

  n <- BILD_Data[i,32] #Sample size, Define from table
  Group1True <- BILD_Data[i,12] #P for group 1, Define from table
  nameofX <- BILD_Data[i,9] #Name of group 1, define from table
  nameofY <- BILD_Data[i,10] #Name of group 2, define from table

  #Set group 2 + or - based on survey

  if(BILD_Data[i,13]=="higher"){ #If they say positive, delta P is .2
    delta <- c(Group1True+0.2)
  }

  if(BILD_Data[i,13]=="lower"){ #If they say negative, delta P is -.2
    delta <- c(Group1True-0.2)
  }

  if(delta>=1){
    delta<- 0.99
  }

  if(delta<0){
    delta<- 0.01
  }

  Group1data<- rbinom(n = n,size = 1,prob = Group1True)
  Group2data<- rbinom(n = n,size = 1,prob = delta)

  Group1TorF<-Group1data==1
  Group2TorF<-Group2data==1
  chisq.test(x =Group1TorF,y = Group2TorF)
  group <- c(rep(nameofX,length(Group1data)),rep(nameofY,length(Group2data)))
  response<-c(Group1data,Group2data)
  response<-ifelse(response==1,"yes","no")
  dat<-data.frame(group,response)
  chidat<-table(group,response)
  chitest<-chisq.test(chidat)
  sqplot<-ggplot2::ggplot(data=dat,mapping = aes(x=group,fill=response))+
    geom_bar()+
    theme_minimal()

  xaxis<-1:10
  yaxis<-1:10
  tplotdat<-data.frame(xaxis,yaxis)
  text<-ggplot2::ggplot(tplotdat,aes(x=xaxis,y=yaxis))+
    annotate("text",x = 2,y = 10,label="Probability of each group")+
    annotate("text",x = 1.8,y = 9.5,label=paste("p of",nameofX,round(sum(Group1TorF)/length(Group1TorF),4),sep=" "))+
    annotate("text",x = 1.8,y = 9,label=paste("p of",nameofY,round(sum(Group2TorF)/length(Group2TorF),4),sep=" "))+
    annotate("text",x = 6,y = 10,label="Coefficents")+
    annotate("text",x = 6,y = 9.5,label=paste("X-squared = ",round(chitest$statistic,5),sep=""))+
    annotate("text",x = 2,y = 7.5,label="Model p-value:")+
    annotate("text",x = 2.5,y = 7,label=chitest$p.value)+
    annotate("text",x = 7,y = 7.5,label="Test run:")+
    annotate("text",x = 7,y = 7,label="chi squared")+
    theme_void()+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 2))+
    xlim(c(0,10))+
    ylim(c(0,10))

  text

  plotgrid<-cowplot::plot_grid(sqplot,text,nrow = 1,ncol = 2)
  ggplot2::ggsave(filename = fignames[i],plot = plotgrid,width = 10,height = 5,units = "in")
  write.csv(x = dat,file = names[i])
  }



   #####linear regression#####
  if(BILD_Data[i,7]=="Linear regression"){

    if(BILD_Data[i,4] %% 2==0){ #If the PID is even they get skewed data
      minX<-BILD_Data[i,25] #minimum X value
      muX<-BILD_Data[i,27] #Mean X value
      minY <- log(BILD_Data[i,29]+0.01) #Minimum Y Value logged, so when we exponentiate the raw data later, it will look about right.
      muY <- log(BILD_Data[i,31]+0.01) #Mean Y value logged, so when we exponentiate the raw data later, it will look about right.
      mu<-c(muX,muY)
      sdX <- abs((muX-minX)/5) #Fake some standard deviation
      sdY <- abs((muY-minY)/10) #Fake some standard deviation, Smaller for the transformed data so we dont end up with crazy values as often
      Sds <- c(sdX,sdY) #sd of x and y
      n <- BILD_Data[i,32] #Sample size
    }else{
      minX<-BILD_Data[i,25] #minimum X value
      muX<-BILD_Data[i,27] #Mean X value
      minY <- BILD_Data[i,29] #Minimum Y Value
      muY <- BILD_Data[i,31] #Mean Y value
      mu<-c(muX,muY) #Mean of x and y
      sdX <- abs((muX-minX)/5) #Fake some standard deviation
      sdY <- abs((muY-minY)/5) #Fake some standard deviation
      Sds <- c(sdX,sdY) #sd of x and y
      n <- BILD_Data[i,32]
    }

    #Set rho + or - based on survey


    if(BILD_Data[i,23]=="positive"){ #If they say positive, rho is 0.4
      rho <- c(0.4)
    }

    if(BILD_Data[i,23]=="negative"){ #If they say negative, rho is -0.4
      rho <- c(-0.4)
    }


nameofX <- BILD_Data[i,21] #define from table
nameofY <- BILD_Data[i,22] #define from table
Sigma<-rockchalk::lazyCov(Rho = rho, Sd = Sds,d=2) #Lazy covariance matrix
rawvals<-rockchalk::mvrnorm(n=n,mu=mu,Sigma=Sigma) # Create the data
tempX<-rawvals[,1] #X data from mvnorm
tempY<-rawvals[,2] #Y data from mvnorm



#Modify Y to make it harder



if(BILD_Data[i,4] %% 2==0){ #If your PID is even, you get to do a log transformation
  transformY<-exp(tempY)
} else{ #otherwise, you are getting an outlier 10 times the mean.
  transformY<-tempY
  transformY[round(length(tempY)/4)]<-muY*10.2
}

####Remove negative values if the x and y min is >= o

if(BILD_Data[i,29]>=0){ #If the min of Y is equal to or bigger than 0, the Y data gets abs()
  FinalY<-abs(transformY)
}else{ # otherwise, the min of Y is less than zero and we dont have to worry.
  FinalY<-(transformY)
}
#Do the same for X.
if(BILD_Data[i,25]>=0){
  FinalX<-abs(tempX)
}else{
  FinalX<-(tempX)
}




dataframe<-data.frame(FinalX,FinalY)  #Make the CSVs
colnames(dataframe)<-c(nameofX,nameofY)

write.csv(x = dataframe,file = names[i])


#Make the key

resultX <- FinalX
resultY <- FinalY
resultdata <- cbind(resultX,resultY)
if(BILD_Data[i,4] %% 2!=0){ #remove the outlier that was put in if the pid was odd, else transform exp data
  outlierrow<-(round(length(resultdata[,1])/4))
  resdat<-resultdata[-outlierrow,]
}else{
  resultY<-log(resultY)
  resdat<-cbind(resultX,resultY)
}



#test for normal
ksRawX<-ks.test(resultdata[,1],pnorm,mean(resultdata[,1]),sd(resultdata[,1]))$p.value
ksRawY<-ks.test(resultdata[,2],pnorm,mean(resultdata[,2]),sd(resultdata[,2]))$p.value
ksXpval<-ks.test(resdat[,1],pnorm,mean(resdat[,1]),sd(resdat[,1]))$p.value
ksYpval<-ks.test(resdat[,2],pnorm,mean(resdat[,2]),sd(resdat[,2]))$p.value



  model<-lm(resdat[,2]~resdat[,1])
  summarymodel<-summary(model)
  intercept<-summarymodel$coefficients[1,1]
  slope<-summarymodel$coefficients[2,1]
  rsquare<-summarymodel$r.squared
  Adrsquare<-summarymodel$adj.r.squared
  pvalue<-summarymodel$coefficients[2,4]


#make the figures
figdata<-as.data.frame(resdat)
if(BILD_Data[i,32]<40){
  bins<-15
}else{
  bins<-30
}
histX <- ggplot2::ggplot(figdata, aes(x=resultX))+
  geom_histogram(bins = bins,fill="navyblue")+
  theme_minimal()+
  xlab(nameofX)
histY<- ggplot2::ggplot(figdata, aes(x=resultY))+
  geom_histogram(bins = bins,fill="navyblue")+
  theme_minimal()+
  xlab(nameofY)
fig<-ggplot2::ggplot(figdata, aes(x=resultX,y = resultY))+
  geom_point()+
  theme_classic()+
  geom_smooth(method = lm)+
  xlab(nameofX)+
  ylab(nameofY)
#Now lets make some text go in this box and then we have a little mini rubric!
if(BILD_Data[i,4] %% 2==0){
  problem<-"log Transform"
}else{
  problem<-"Outlier"
}
xaxis<-1:10
yaxis<-1:10
tplotdat<-data.frame(xaxis,yaxis)
  text<-ggplot2::ggplot(tplotdat,aes(x=xaxis,y=yaxis))+
    annotate("text",x = 2,y = 10,label="Tests for Normallity")+

    annotate("text",x = 3,y = 9.5,label=round(ksRawX,4))+
    annotate("text",x = 1.5,y = 9.5,label="Raw X =")+
    annotate("text",x = 3,y = 9,label=round(ksRawY,4))+
    annotate("text",x = 1.5,y = 9,label="Raw Y =")+

    annotate("text",x = 3,y = 8.5,label=round(ksXpval,4))+
    annotate("text",x = 1.5,y = 8.5,label="Fixed X =")+
    annotate("text",x = 3,y = 8,label=round(ksYpval,4))+
    annotate("text",x = 1.5,y = 8,label="Fixed Y =")+
    annotate("text",x = 6,y = 10,label="Coefficents")+
    annotate("text",x = 6,y = 9.5,label="Intercept = ")+
    annotate("text",x = 6,y = 9,label="Slope =")+
    annotate("text",x = 8,y = 9.5,label=round(intercept,5))+
    annotate("text",x = 7.5,y = 9,label=round(slope,5))+
    annotate("text",x = 2,y = 7.5,label="Model p-value:")+
    annotate("text",x = 2.5,y = 7,label=pvalue)+
    annotate("text",x = 7,y = 7.5,label="Model R squared:")+
    annotate("text",x = 7,y = 7,label=rsquare)+
    annotate("text",x = 2,y = 5,label="Problem with Data:")+
    annotate("text",x = 2,y = 4.5,label=problem)+
    annotate("text",x = 7,y = 5,label="Test run:")+
    annotate("text",x = 7,y = 4.5,label="regression")+
    theme_void()+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 2))+
    xlim(c(0,10))+
    ylim(c(0,10))

plotgrid<-cowplot::plot_grid(histX,histY,fig,text,nrow = 2,ncol = 2)
ggplot2::ggsave(filename = fignames[i],plot = plotgrid,width = 10,height = 10,units = "in")
  }

   #####Correlation####
  if(BILD_Data[i,7]=="Correlation"){

    if(BILD_Data[i,4] %% 2==0){ #If the PID is even they get skewed data
      minX<-BILD_Data[i,25] #minimum X value
      muX<-BILD_Data[i,27] #Mean X value
      minY <- log(BILD_Data[i,29]+0.01) #Minimum Y Value logged, so when we exponentiate the raw data later, it will look about right.
      muY <- log(BILD_Data[i,31]+0.01) #Mean Y value logged, so when we exponentiate the raw data later, it will look about right.
      mu<-c(muX,muY)
      sdX <- abs((muX-minX)/5) #Fake some standard deviation
      sdY <- abs((muY-minY)/10) #Fake some standard deviation, Smaller for the transformed data so we dont end up with crazy values as often
      Sds <- c(sdX,sdY) #sd of x and y
      n <- BILD_Data[i,32] #Sample size
    }else{
      minX<-BILD_Data[i,25] #minimum X value
      muX<-BILD_Data[i,27] #Mean X value
      minY <- BILD_Data[i,29] #Minimum Y Value
      muY <- BILD_Data[i,31] #Mean Y value
      mu<-c(muX,muY) #Mean of x and y
      sdX <- abs((muX-minX)/5) #Fake some standard deviation
      sdY <- abs((muY-minY)/5) #Fake some standard deviation
      Sds <- c(sdX,sdY) #sd of x and y
      n <- BILD_Data[i,32]
    }

    #Set rho + or - based on survey


    if(BILD_Data[i,23]=="positive"){ #If they say positive, rho is 0.4
      rho <- c(0.4)
    }

    if(BILD_Data[i,23]=="negative"){ #If they say negative, rho is -0.4
      rho <- c(-0.4)
    }


    nameofX <- BILD_Data[i,19] #define from table
    nameofY <- BILD_Data[i,20] #define from table
    Sigma<-rockchalk::lazyCov(Rho = rho, Sd = Sds,d=2) #Lazy covariance matrix
    rawvals<-rockchalk::mvrnorm(n=n,mu=mu,Sigma=Sigma) # Create the data
    tempX<-rawvals[,1] #X data from mvnorm
    tempY<-rawvals[,2] #Y data from mvnorm



    #Modify Y to make it harder



    if(BILD_Data[i,4] %% 2==0){ #If your PID is even, you get to do a log transformation
      transformY<-exp(tempY)
    } else{ #otherwise, you are getting an outlier 10 times the mean.
      transformY<-tempY
      transformY[round(length(tempY)/4)]<-muY*10.2
    }

    ####Remove negative values if the x and y min is >= o

    if(BILD_Data[i,29]>=0){ #If the min of Y is equal to or bigger than 0, the Y data gets abs()
      FinalY<-abs(transformY)
    }else{ # otherwise, the min of Y is less than zero and we dont have to worry.
      FinalY<-(transformY)
    }
    #Do the same for X.
    if(BILD_Data[i,25]>=0){
      FinalX<-abs(tempX)
    }else{
      FinalX<-(tempX)
    }




    dataframe<-data.frame(FinalX,FinalY)  #Make the CSVs
    colnames(dataframe)<-c(nameofX,nameofY)

    write.csv(x = dataframe,file = names[i])


    #Make the key

    resultX <- FinalX
    resultY <- FinalY
    resultdata <- cbind(resultX,resultY)
    if(BILD_Data[i,4] %% 2!=0){ #remove the outlier that was put in if the pid was odd, else transform exp data
      outlierrow<-(round(length(resultdata[,1])/4))
      resdat<-resultdata[-outlierrow,]
    }else{
      resultY<-log(resultY)
      resdat<-cbind(resultX,resultY)
    }



    #test for normal
    ksXpval<-ks.test(resdat[,1],pnorm,mean(resdat[,1]),sd(resdat[,1]))$p.value
    ksYpval<-ks.test(resdat[,2],pnorm,mean(resdat[,2]),sd(resdat[,2]))$p.value
    ksRawX<-ks.test(resultdata[,1],pnorm,mean(resultdata[,1]),sd(resultdata[,1]))$p.value
    ksRawY<-ks.test(resultdata[,2],pnorm,mean(resultdata[,2]),sd(resultdata[,2]))$p.value




    model<-cor.test(resdat[,1],resdat[,2])
    cor<-model$estimate
    pval<-model$p.value
    conf<-model$conf.int[1:2]


    #make the figures
    figdata<-as.data.frame(resdat)
    if(BILD_Data[i,32]<40){
      bins<-15
    }else{
      bins<-30
    }
    histX <- ggplot2::ggplot(figdata, aes(x=resultX))+
      geom_histogram(bins = bins,fill="navyblue")+
      theme_minimal()+
      xlab(nameofX)
    histY<- ggplot2::ggplot(figdata, aes(x=resultY))+
      geom_histogram(bins = bins,fill="navyblue")+
      theme_minimal()+
      xlab(nameofY)
    fig<-ggplot2::ggplot(figdata, aes(x=resultX,y = resultY))+
      geom_point()+
      theme_classic()+
      geom_smooth(method = lm)+
      xlab(nameofX)+
      ylab(nameofY)
    #Now lets make some text go in this box and then we have a little mini rubric!
    if(BILD_Data[i,4] %% 2==0){
      problem<-"log Transform"
    }else{
      problem<-"Outlier"
    }
    xaxis<-1:10
    yaxis<-1:10
    tplotdat<-data.frame(xaxis,yaxis)
    text<-ggplot2::ggplot(tplotdat,aes(x=xaxis,y=yaxis))+
      annotate("text",x = 2,y = 10,label="Tests for Normallity")+
      annotate("text",x = 3,y = 9.5,label=round(ksRawX,4))+
      annotate("text",x = 1.5,y = 9.5,label="Raw X =")+
      annotate("text",x = 3,y = 9,label=round(ksRawY,4))+
      annotate("text",x = 1.5,y = 9,label="Raw Y =")+

      annotate("text",x = 3,y = 8.5,label=round(ksXpval,4))+
      annotate("text",x = 1.5,y = 8.5,label="Fixed X =")+
      annotate("text",x = 3,y = 8,label=round(ksYpval,4))+
      annotate("text",x = 1.5,y = 8,label="Fixed Y =")+
      annotate("text",x = 6,y = 10,label="Coefficents")+
      annotate("text",x = 6,y = 9.5,label="r = ")+
      annotate("text",x = 7.5,y = 9.5,label=round(cor,5))+
      annotate("text",x = 2,y = 7.5,label="Model p-value:")+
      annotate("text",x = 2.5,y = 7,label=pval)+
      annotate("text",x = 7,y = 7.5,label="95% CI")+
      annotate("text",x = 7,y = 7,label=conf[1])+
      annotate("text",x = 7,y = 6.5,label=conf[2])+
      annotate("text",x = 2,y = 5,label="Problem with Data:")+
      annotate("text",x = 2,y = 4.5,label=problem)+
      annotate("text",x = 7,y = 5,label="Test run:")+
      annotate("text",x = 7,y = 4.5,label="correlation")+
      theme_void()+
      theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 2))+
      xlim(c(0,10))+
      ylim(c(0,10))

    plotgrid<-cowplot::plot_grid(histX,histY,fig,text,nrow = 2,ncol = 2)
    ggplot2::ggsave(filename = fignames[i],plot = plotgrid,width = 10,height = 10,units = "in")
  }
}
}


