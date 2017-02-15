library(shiny) ; library(ggplot2) ; library(shinyapps) ; library(nlme) ; library(r2excel)

# setwd("/Users/USER/Desktop/stage/stage_Paris/analyse_growth_tox_R/Tox_Mathieu/vers_de_terre/")
file="tableau_growth_Jerome.xls"
# ex=read.xlsx(file,sheetIndex = 1,header=TRUE)

my_data=ex
my_data$species.raw=as.character(my_data$species.raw)
my_data$auteur=as.character(my_data$auteur)
my_data$figure=as.character(my_data$figure)

gompertz = function(x,Min=0,Asym=0,xmid=0,scal=0){
  x$treatment=factor(x$treatment, levels=unique(x$treatment))
  ggplot(x,aes(x$time,x$bm,col=x$treatment))+
    geom_point()+theme_bw()+
  geom_smooth(method="nls",
      method.args=list(formula=y~Min+Asym*exp(-exp(-scal*(x-xmid))),
      start=list(Asym=Asym,xmid=xmid,scal=scal)),se=F,fullrange=F)+
    xlab("time (day)")+ylab("biomasse (mg)")+facet_grid(~treatment)+
    theme(legend.position="none")+
    scale_color_manual(values=c("black","red", "blue", "green","orange","purple","brown"))
}

logistique = function(x,Min=0,Asym=0,xmid=0,scal=0){
  x$treatment=factor(x$treatment, levels=unique(x$treatment))
  ggplot(x,aes(x$time,x$bm,col=x$treatment))+
    geom_point()+theme_bw()+
    geom_smooth(method="nls",
                method.args=list(formula=y~Min+(Asym-Min)/(1+exp((xmid-x)/scal)),
                                 start=list(Asym=Asym,xmid=xmid,scal=scal)),se=F,fullrange=F)+
    xlab("time (day)")+ylab("biomasse (mg)")+facet_grid(~treatment)+
    theme(legend.position="none")+
    scale_color_manual(values=c("black","red", "blue", "green","orange","purple","brown"))
}

brain_cousen = function(x,Min=0,Asym=0,xmid=0,scal=0,horm=0){
  x$time2=abs(x$time-max(x$time))
  x$treatment=factor(x$treatment, levels=unique(x$treatment))
  ggplot(x,aes(x$time2,x$bm,col=x$treatment))+
    geom_point()+theme_bw() +
    geom_smooth(method="nls",
                method.args=list(formula=y~Min+(Asym-Min+horm*x)/(1+exp(scal*log(x/xmid))),
                start=list(Asym=Asym,horm=horm,scal=scal,xmid=xmid)),se=F,fullrange=F)+
    xlab("time (day) [reverse values]")+ylab("biomasse (mg)")+
    ggtitle("la figure est inverse par rapport a la realite")+facet_grid(~treatment)+
    theme(legend.position="none")+
    scale_color_manual(values=c("black","red", "blue", "green","orange","purple","brown"))
}

brain_cousen_sans_horm = function(x,Min=0,Asym=0,xmid=0,scal=0){
  x$time2=abs(x$time-max(x$time))
  x$treatment=factor(x$treatment, levels=unique(x$treatment))
  ggplot(x,aes(x$time2,x$bm,col=x$treatment))+
    geom_point()+theme_bw()+
    geom_smooth(method="nls",
                method.args=list(formula=y~Min+(Asym-Min)/(1+exp(scal*log(x/xmid))),
                                 start=list(Asym=Asym,scal=scal,xmid=xmid)),se=F,fullrange=F)+
    xlab("time (day) [reverse values]")+ylab("biomasse (mg)") +
    ggtitle("la figure est inverse par rapport a la realite")+facet_grid(~treatment)+
    theme(legend.position="none")+
    scale_color_manual(values=c("black","red", "blue", "green","orange","purple","brown"))
}

double_brain_cousen = function(x,Min=0,Asym=0,xmid=0,scal=0,horm=0,xmid2=0){
  x$time2=abs(x$time-max(x$time))
  x$treatment=factor(x$treatment, levels=unique(x$treatment))
  ggplot(x,aes(x$time2,x$bm,col=x$treatment))+
    geom_point()+theme_bw()+
    geom_smooth(method="nls",
                method.args=list(formula=y~Min+(Asym-Min)/(1+exp(scal*log(x/xmid)))+
                                   (horm*x)/(1+exp((xmid2-x)/-scal)),
                                 start=list(Asym=Asym,horm=horm,scal=scal,
                                            xmid=xmid,xmid2=xmid2)),se=F,fullrange=F)+
    xlab("time (day) [reverse values]")+ylab("biomasse (mg)") +
    ggtitle("la figure est inverse par rapport a la realite")+facet_grid(~treatment)+
    theme(legend.position="none")+
    scale_color_manual(values=c("black","red", "blue", "green","orange","purple","brown"))
}

linear = function(x){
  x$time2=abs(x$time-max(x$time))
  x$treatment=factor(x$treatment, levels=unique(x$treatment))
  ggplot(x,aes(x$time2,x$bm,col=x$treatment))+
    geom_point()+theme_bw()+
    geom_smooth(method="lm",se=T,fullrange=F) +
    xlab("time (day) [reverse values]")+ylab("biomasse (mg)")+
    ggtitle("la figure est inverse par rapport a la realite")+facet_grid(~treatment)+
    theme(legend.position="none")+
    scale_color_manual(values=c("black","red", "blue", "green","orange","purple","brown"))
}

non_analyse = function(text="text",size=5){
  ggplot()+geom_blank(aes(1,1))+
    theme(plot.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank()
    )+annotate(geom="text",x=1,y=1,label=text,size=size,col="red")
  
}


