#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(scales)
library(doBy)
library(DT)

set.seed(1)
populationdata<-data.frame(ID=1:1000,
                           Shade=sample(c("Under Shade","Full Sun"),size = 1000,replace=TRUE,prob = c(0.95,0.18)),
                           Gender=sample(c("Male","Female"),size = 1000,replace=TRUE,prob = c(0.64,0.88)),
                           Area=round(rgamma(1000,4,1/2),1))


populationdata$SOC<-ifelse(populationdata$Shade=="Under Shade",
                           rnorm(846,75,8),
                           rgamma(154,4,1/1.5))
n1<-length(populationdata$SOC[populationdata$SOC<5])
populationdata$SOC[populationdata$SOC<5]<-populationdata$SOC[populationdata$SOC<5]+
  runif(n1,5,8)

populationdata$SOC<-round(populationdata$SOC,1)

pdat<-populationdata
pdat$Gender<-as.numeric(pdat$Gender=="Female")   
pdat$Shade<-as.numeric(pdat$Shade=="Under Shade")  
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  sidebarLayout(
    
    
    sidebarPanel(style = "position:fixed;width:inherit;",
                 
                 selectInput(inputId = "Variable",
                             "Select A Variable",
                             c("Gender of Farmer" = "Gender",
                               "Area cultivated with coffee (ha)"= "Area" ,
                               "Soil Organic Carbon Stocks (SOC)"= "SOC",
                               "Coffee Production System"="Shade")),
                 sliderInput(inputId = "Add10",
                             label="Sample Size",min = 0,max=1000,value = 1000,step=10),
                 actionButton(inputId = "refresh",
                              "New Survey"),
                 actionButton(inputId = "reset",
                              "Reset"),width = 2),
    mainPanel(
      titlePanel("Estimation"),
      tabsetPanel(
        tabPanel("Data",DT::dataTableOutput("data1")),
        tabPanel("Summary",tableOutput("stats1"),plotlyOutput("plot1")),
        tabPanel("Repeatability",actionButton(inputId = "nrep",
                                              label="Add 100 New Surveys"),
                 selectInput("axis_type","Axis range",c("Zoomed in"="zoom","Full range"="full")),
                 htmlOutput("title1"),plotlyOutput("plot3"),
                 checkboxInput("meanline","Add Mean Line",value=FALSE), checkboxInput("ciline","Add 95% CI Lines",value=FALSE), 
                 plotlyOutput("plot_hist")),
        tabPanel("Precision",tableOutput("stats2"),selectInput("type","Select Plot Variable",c("95% Confidence Interval"="CI","Standard error"="SE")),
                 plotlyOutput("plot5")),
        tabPanel("Disaggregation", selectInput(inputId = "group",
                                               "Select A Disaggregation Variable",
                                               c("Gender of Farmer" = "Gender","Coffee Production System"="Shade")),
                 tableOutput("stat_dis"),plotlyOutput("plot_d"),plotlyOutput("plot_e"))
        
        
      )
      ,width = 8)
  )
)

yaxis<-list(axis.automargin=TRUE)

server <- function(input, output) {
  
  counter <- reactiveValues(n = 0)
  sampledata<-eventReactive(input$refresh|counter$n==0,{
    if(input$Add10=="1000"|counter$n==0){
      data<-populationdata
    }
    else{
      data<-populationdata[sample(1:1000,as.numeric(as.character(input$Add10))),]
      data<-data[order(data$ID),]
    }
    data})
  
  
  observeEvent(input$refresh, {counter$n <- counter$n + 1})
  observeEvent(input$reset, {  set.seed(1)
    counter$n <- 0})
  observeEvent(input$nrep, {counter$n <- counter$n + 100})
  
  output$data1<-DT::renderDataTable({
    sampledata()
    
  }, options = list(
    pageLength = 25)  )
  
  
  
  output$plot1<-renderPlotly({
    
    if(input$Variable=="Gender"|input$Variable=="Shade"){
      p1<-ggplot(data=sampledata(),aes_string(x=input$Variable,fill=input$Variable))+
        geom_bar(mapping = aes(y = (..count..)/sum(..count..)),position="dodge",col="black")+
        xlab(input$Variable)+ylab("Number of Farmers")+ggtitle(input$Variable,subtitle = paste("N=",nrow(sampledata())))+
        scale_y_continuous(labels=percent,limits=c(0,1))
    }
    
    if(input$Variable=="SOC"|input$Variable=="Area"){
      p1<-   ggplot(data=sampledata(),aes_string(x =input$Variable))+geom_histogram(fill="red",col="black",alpha=0.6)+
        xlab(input$Variable)+ggtitle(input$Variable,subtitle = paste("N=",nrow(sampledata())))
    }   
    
    p2<- p1+theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
                  legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
  })
  
  output$stats1<-renderTable({
    
    if(input$Variable=="SOC"){
      df1<-sampledata() %>%
        summarise("Mean"=mean(SOC),"SD"=sd(SOC),"Median"=median(SOC))}
    
    if(input$Variable=="Area"){
      df1<-sampledata() %>%
        summarise("Mean"=mean(Area),"SD"=sd(Area),"Median"=median(Area))}
    
    if(input$Variable=="Gender"){
      df1<-sampledata() %>%
        summarise("Male"=paste0(sum(Gender=="Male")," (",round(100*mean(Gender=="Male")),"%)"),
                  "Female"=paste0(sum(Gender=="Female")," (",round(100*mean(Gender=="Female")),"%)"))}
    
    if(input$Variable=="Shade"){
      df1<-sampledata() %>%
        summarise("Under Shade"=paste0(sum(Shade=="Under Shade")," (",round(100*mean(Shade=="Under Shade")),"%)"),
                  "Full Sun"=paste0(sum(Shade=="Full Sun")," (",round(100*mean(Shade=="Full Sun")),"%)"))}
    
    df1
    
  })  
  
  
  output$title1<-renderText({
    addtext<-""
    addtext[input$Variable=="Gender"]<-"= Female"
    addtext[input$Variable=="Shade"]<-"= Under Shade" 
    
    HTML(paste('<font size="+2">',"Estimates of",input$Variable,addtext,"\nfrom",counter$n,"surveys each of sample size",input$Add10),'</font>')
  })
  
  output$plot3<-renderPlotly({
    
    addtext<-""
    addtext[input$Variable=="Gender"]<-"= Female"
    addtext[input$Variable=="Shade"]<-"= Under Shade" 
    
    reps<-data.frame(Survey=1:counter$n,Estimate=NA,sd=NA)
    set.seed(1)
    for(i in 1:counter$n){
      tmp<-pdat[sample(1:1000,as.numeric(as.character(input$Add10))),input$Variable]
      reps$Estimate[i]<-mean(tmp)
      reps$sd[i]<-ifelse(input$Variable=="SOC"|input$Variable=="Area",sd(tmp),
                         sqrt(mean(tmp)*(1-mean(tmp))))
    }
    reps$upper<-reps$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-as.numeric(as.character(input$Add10)))/999)*reps$sd/sqrt(as.numeric(as.character(input$Add10)))
    reps$lower<-reps$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*sqrt((1000-as.numeric(as.character(input$Add10)))/999)*reps$sd/sqrt(as.numeric(as.character(input$Add10)))
    p1<-ggplot(data=reps,aes(y=Estimate,x=Survey))
    if(input$meanline==TRUE){
      p1<-p1+geom_hline(aes(yintercept=mean(Estimate)),col="blue")
    }
    if(input$ciline==TRUE){
      
      
      p1<-p1+
        geom_errorbar(aes(ymax=upper,
                          ymin=lower),col="red",linetype=2)
    }
    
    
    if(input$axis_type=="full"){
      if(input$Variable=="Gender"|input$Variable=="Shade"){
        
        p1<-p1+scale_y_continuous(labels=scales::percent,limits=c(0,1),breaks=seq(0,1,by=0.2))
      }
      if(input$Variable=="Area"){
        
        p1<-p1+scale_y_continuous(limits=c(0,25),breaks=seq(0,25,by=5))
      }
      if(input$Variable=="SOC"){
        
        p1<-p1+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))
      }
    }
    if(input$axis_type=="zoom"){
      if(input$Variable=="Gender"|input$Variable=="Shade"){
        
        p1<-p1+scale_y_continuous(labels=scales::percent)
      }
    }
    
    p2<-p1+geom_point(size=2,col="forestgreen")+
      theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
            legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )+
      ylab(paste(input$Variable,addtext))+xlab("Survey Number")
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
    
  })
  
  output$plot_hist<-renderPlotly({
    reps<-data.frame(Survey=1:counter$n,Estimate=NA)
    set.seed(1)
    for(i in 1:counter$n){
      tmp<-pdat[sample(1:1000,as.numeric(as.character(input$Add10))),input$Variable]
      reps$Estimate[i]<-mean(tmp)
    }
    p1<-ggplot(reps,aes(x=Estimate))+
      geom_histogram()+
      ggtitle("Histogram of Sample Estimates")
    
    if(input$axis_type=="full"){
      if(input$Variable=="Gender"|input$Variable=="Shade"){
        
        p1<-p1+scale_x_continuous(labels=scales::percent,limits=c(0,1),breaks=seq(0,1,by=0.2))
      }
      if(input$Variable=="Area"){
        
        p1<-p1+scale_x_continuous(limits=c(0,25),breaks=seq(0,25,by=5))
      }
      if(input$Variable=="SOC"){
        
        p1<-p1+scale_x_continuous(limits=c(0,100),breaks=seq(0,100,by=20))
      }
    }
    if(input$axis_type=="zoom"){
      if(input$Variable=="Gender"|input$Variable=="Shade"){
        
        p1<-p1+scale_x_continuous(labels=scales::percent)
      }
    }
    
    
    
    
    ggplotly(p1)
  })
  
  
  output$plot5<-renderPlotly({
    
    if(input$type=="CI"){
      
      pdat<-populationdata
      pdat$Gender<-as.numeric(pdat$Gender=="Female")   
      pdat$Shade<-as.numeric(pdat$Shade=="Under Shade")   
      
      addtext<-""
      addtext[input$Variable=="Gender"]<-"= Female"
      addtext[input$Variable=="Shade"]<-"= Under Shade" 
      
      ns<-data.frame(N=10:1000,mean=mean(pdat[,input$Variable]),sd=sd(pdat[,input$Variable]))
      ns$SE<-sqrt(ns$sd/ns$N)
      ns$Upper<-ns$mean+qt(0.975,ns$N-1)*ns$SE
      ns$Lower<-ns$mean-qt(0.975,ns$N-1)*ns$SE
      
      print(ns)
      
      p1<-ggplot(data=ns,aes(y=mean,x=N))+
        geom_line()+geom_line(aes(y=Upper),col="red")+
        geom_line(aes(y=Lower),col="red")+
        xlab("Sample Size")+ylab("Confidence Interval")+ggtitle(paste(input$Variable,addtext,"+ Confidence Interval"))
      
      if(input$Variable=="Gender"|input$Variable=="Shade"){
        
        p1<-p1+scale_y_continuous(labels=scales::percent)
      }
      
    }
    
    else{
      
      pdat<-populationdata
      pdat$Gender<-as.numeric(pdat$Gender=="Female")   
      pdat$Shade<-as.numeric(pdat$Shade=="Under Shade")   
      
      addtext<-""
      addtext[input$Variable=="Gender"]<-"= Female"
      addtext[input$Variable=="Shade"]<-"= Under Shade" 
      
      ns<-data.frame(N=10:1000,mean=mean(pdat[,input$Variable]),sd=sd(pdat[,input$Variable]))
      ns$SE<-sqrt(ns$sd/ns$N)
      ns$Upper<-ns$mean+qt(0.975,ns$N-1)*ns$SE
      ns$Lower<-ns$mean-qt(0.975,ns$N-1)*ns$SE
      
      print(ns)
      
      p1<-ggplot(data=ns,aes(y=SE,x=N))+
        geom_line()+
        xlab("Sample Size")+ylab("Standard Error")+ggtitle(paste(input$Variable,addtext))
      
      if(input$Variable=="Gender"|input$Variable=="Shade"){
        
        p1<-p1+scale_y_continuous(labels=scales::percent)
      }
      
    }
    
    
    
    p2<-p1  +
      theme(axis.title = element_text(size=14),title = element_text(size=12,face = "bold"),
            legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p2),yaxis=yaxis1)
    new_plot
  })
  
  
  
  
  
  
  output$stats2<-renderTable({
    tmp<-sampledata()
    tmp$Gender<-100*as.numeric(tmp$Gender=="Female")   
    tmp$Shade<-100*as.numeric(tmp$Shade=="Yes") 
    
    out1<-data.frame(Variable=c("Area","Gender","SOC","Shade"),Type=c("Mean","% Female","Mean","% Under Shade"),Sample=input$Add10,
                     Estimate=c(mean(tmp$Area),mean(tmp$Gender),mean(tmp$SOC),mean(tmp$Shade)),
                     SD=c(sd(tmp$Area),
                          100*sqrt(mean(tmp$Gender/100)*(1-mean(tmp$Gender/100))),
                          sd(tmp$SOC),
                          100*sqrt(mean(tmp$Shade/100)*(1-mean(tmp$Shade/100)))))
    
    out1$SE=out1$SD/sqrt(as.numeric(as.character(input$Add10)))
    out1$LCI=out1$Estimate-qt(0.975,as.numeric(as.character(input$Add10))-1)*out1$SE
    out1$UCI=out1$Estimate+qt(0.975,as.numeric(as.character(input$Add10))-1)*out1$SE
    out1$UCI[out1$UCI>100&input$Variable=="Gender"|input$Variable=="Shade"]<-100
    out1$LCI[out1$LCI<0&input$Variable=="Gender"|input$Variable=="Shade"]<-0
    colnames(out1)[4:8]<-c("Mean (Population)","Standard Deviation (Population)","Standard Error","95% Confidence Interval (Lower)","95% Confidence Interval (Upper)")
    out1[out1$Variable==input$Variable,]
  })
  
  
  output$plot_d<-renderPlotly({
    
    addtext<-""
    addtext[input$Variable=="Gender"]<-"= Female"
    addtext[input$Variable=="Shade"]<-"= Under Shade" 
    
    if(input$Variable=="Area"|input$Variable=="SOC"){
      p1<-ggplot(data=sampledata(),aes_string(y=input$Variable,x=input$group))+
        stat_summary(fun.args=list(mult = 1.96))+
        stat_summary(geom="point")+
        xlab(input$group)+ylab(paste(input$Variable,addtext))+
        theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
              legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    }
    
    if(input$Variable=="Gender"){
      tmp<-sampledata()
      tmp$Gender<-as.numeric(tmp$Gender=="Female")   
      p1<-ggplot(data=tmp,aes_string(y=input$Variable,x=input$group))+
        stat_summary(fun.args=list(mult = 1.96))+
        stat_summary(geom="point")+
        xlab(input$group)+ylab(paste(input$Variable,addtext))+
        theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
              legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )+
        scale_y_continuous(labels=scales::percent)
    }
    if(input$Variable=="Shade"){
      tmp<-sampledata()
      tmp$Shade<-as.numeric(tmp$Shade=="Under Shade")   
      p1<-ggplot(data=tmp,aes_string(y=input$Variable,x=input$group))+
        stat_summary(fun.args=list(mult = 1.96))+
        stat_summary(geom="point")+
        xlab(input$group)+ylab(paste(input$Variable,addtext))+
        theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
              legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )+
        scale_y_continuous(labels=scales::percent)
    }  
    
    
    
    
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p1),yaxis=yaxis1)
    new_plot
  })
  
  output$plot_e<-renderPlotly({
    
    
    
    if(input$Variable=="Area"|input$Variable=="SOC"){
      p1<-ggplot(data=sampledata(),aes_string(x=input$Variable))+
        facet_wrap(input$group)+
        geom_histogram()+
        xlab(input$Variable)+
        theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
              legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    }
    
    if(input$Variable=="Gender"|input$Variable=="Shade"){
      
      p1<-ggplot(data=sampledata(),aes_string(x=input$Variable))+
        facet_wrap(input$group)+
        geom_bar()+
        xlab(input$Variable)+
        theme(axis.title = element_text(size=14),title = element_text(size=18,face = "bold"),
              legend.text = element_text(size=14),legend.title = element_text(size=16,face="bold"),axis.text =element_text(size=14) )
    }
    
    
    
    
    
    yaxis1<-list(axis.automargin=TRUE,tickprefix="       ")
    new_plot<-layout(ggplotly(p1),yaxis=yaxis1)
    new_plot
  })
  
  
  output$stat_dis<-renderTable({
    tmp<-sampledata()
    
    
    if(input$Variable=="Gender"){
      tmp$Variable<-tmp$Gender=="Female"
      tmp$Group<-tmp[,input$group]
      tmp %>%
        group_by(Group)%>%
        summarise(n=n(),"% Female"=100*mean(Variable),SE=100*sd(Variable)/n,"Lower CI"=`% Female`-1.96*SE,"Upper CI"=`% Female`+1.96*SE)->dd
    }
    
    if(input$Variable=="Shade"){
      tmp$Variable<-tmp$Shade=="Under Shade"
      tmp$Group<-tmp[,input$group]
      
      tmp %>%
        group_by(Group)%>%
        summarise(n=n(),"% Under Shade"=100*mean(Variable),SE=100*sd(Variable)/n,"Lower CI"=`% Under Shade`-1.96*SE,"Upper CI"=`% Under Shade`+1.96*SE)->dd
    }
    
    if(input$Variable=="Area"  |input$Variable=="SOC"){
      tmp$Variable<-tmp[,input$Variable]
      tmp$Group<-tmp[,input$group]
      
      tmp %>%
        group_by(Group)%>%
        summarise(n=n(),"Mean"=mean(Variable),SE=sd(Variable)/n,"Lower CI"=Mean-1.96*SE,"Upper CI"=Mean+1.96*SE)->dd
    }
    
    
    dd
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)