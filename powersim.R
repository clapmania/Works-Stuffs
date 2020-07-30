#
# This is a Shiny web application created by T.WU
# You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# how to:
# run the entire code
# then go straight to power tab :)
# and wait...............................................(minutes or days)..............................................


library(shiny)
library(ggplot2)
library(plyr)
library(lme4)
library(shinythemes)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
  
  # Application title
  titlePanel("Once upon time there was this simulation..."),
  
  # where all those inputs are
  sidebarPanel(
    
    #rvc-sc, v-sc, rvc-nsc, v-nsc, rvc-f, rvc-f
    
    sliderInput("pps", "Number of partcipants:", 10, 200, 26, 2),
    sliderInput("reps", "Number of repitation (x10):", 1, 100, 5),
    sliderInput("items", "Number of items (x10):", 1, 100, 29),
    submitButton("RunRunRunRunRun"),
    #actionButton("goButton", "Runrunrunrunrun"),
    numericInput("m1", "Mean for group rvc-sc", "-0.3467750"),
    numericInput("sd1", "SD for group rvc-sc", "0.3650179"),
    numericInput("m2", "Mean for group v-sc", "-0.3175857"),
    numericInput("sd2", "SD for group v-sc", "0.3599003"),
    numericInput("m3", "Mean for group rvc-nsc", "-0.2439723"),
    numericInput("sd3", "SD for group rvc-nsc", "0.3589646"),
    numericInput("m4", "Mean for group v-nsc", "-0.2486886"),
    numericInput("sd4", "SD for group v-nsc", "0.3766204"),
    numericInput("m5", "Mean for group rvc-f", "-0.2543065"),
    numericInput("sd5", "SD for group rvc-f", "0.3219348"),
    numericInput("m6", "Mean for group v-f", "-0.2473092"),
    numericInput("sd6", "SD for group v-f", "0.3493010"),
    
    # set the width of sidebar 
    width = 2
    
  ),
  
  # the mainPanel where outputs are
  mainPanel(
    tabsetPanel(
      tabPanel("Readme",
               br(),
               p("Suitable for 2*3 studies using lmer(), code can be customized"),
               br(),
               p("Tabs are of many functions:"), 
               p("The first one contains all the simulated data"), 
               p("The second one contains tables of summarized data and anova()"),
               p("The third one contains plots of distribution"),
               p("The last one contains the distribution of p-value substracted from model comparison"),
               br(),
               p("Using power plotting with cautious, it's a pretty demanding task for a computer, requiring large amount of calculation"),
               p("To run a prior analysis, fill in the textfield accordingly, select the number of participants and repetition, press the RUN button, and then wait for the plot to be ready"),
               p("It takes time to finish a plot when the sample size and repetition are large"),
               p("To run a post hoc analysis, fill the textfield with the effect size of your own study, and then select the number of repetition"),
               p("Please note that the real repetition is 10 * the selected reps"),
               p("For example, choosing 20 on the slide meaning the analysis will include 20 * 10 = 200 times of model comparison")),
               
               
      tabPanel("Data with many rows and columns",DT::dataTableOutput("mytable")),
      tabPanel("Aggregated and Random fit", 
               br(), 
               br(), 
               p(strong("A very brief summary of the simulated data")), 
               tableOutput('table1'), 
               br(), 
               p(strong("Where's the interaction (verb+pic vs verb*pic)?"), 
               tableOutput("table2"))),
      tabPanel("Plooots", plotOutput(outputId = "distPlot"), br(), plotOutput(outputId = "vioPlot")),
      tabPanel("Power",
               br(), 
               strong("Beware, the power analysis may freeze your computer :)"),
               br(),
               br(),
               p("Model1: lmer(rt~1+(1|participant)+(1|action)+(1|object))"),
               p("Model2: lmer(rt~verb+(1|participant)+(1|action)+(1|object))"),
               br(),
               p("As the power analysis is to find the beta - the number of time fail to reject H0 when H1 is true (an effect can be found)"),
               p("Comparing these two models will yeild a p-value, a insignificant value suggests an absence of the interaction effect."),
               p("A clustered distribution of p-value on the left end of the x-axis suggests a small beta"),
               p("And a complete list of p-value will be save in the working directory, or partial data if the analysis is interrupted"),
               plotOutput(outputId = "histPlotp"),
               br(),
               DT::dataTableOutput("mytable2"),
               br(),
               textOutput("text1")
               )
        )
      )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # this is to define the dataset
  simdata <- function(m11,m21,m31,m41,m51,m61,sd11,sd21,sd31,sd41,sd51,sd61,s,sg){
    
    #observations, sg is the number of items, which can be modified
    sg=sg
    ig=s/6
    g=sg*s
    
    group1 = rnorm(g, m11, sd11)
    group2 = rnorm(g, m21, sd21)
    group3 = rnorm(g, m31, sd31)
    group4 = rnorm(g, m41, sd41)
    group5 = rnorm(g, m51, sd51)
    group6 = rnorm(g, m61, sd61)
    
    #rs is not logged, as the simulated data is based on mean logrt
    rs = c(group1, group2, group3, group4, group5, group6)
    #condition assigned, verb(rvc-v) and pic(sc-nsc-f)
    verb = rep(c(rep("rvc",g),rep("v",g)),3)
    pic = c(rep("sc",2*g),rep("nsc",2*g),rep("f",2*g))
    
    #assign random factors, participant(ss), object, and action
    ss = paste0('s',rep(rep(seq(1:s),sg),6))
    
    obja = vector()
    objb = vector()
    action = vector()
    n=seq(1:sg)
    for (val in n){
      act = paste0("action",rep(val,s))
      action = c(action,act)
      obj1 = paste0("object1_",rep(val,s/2))
      obj2 = paste0("object2_",rep(val,s/2))
      objc1 = c(obj1,obj2)
      obja = c(obja,objc1)
      objc2 = c(obj2,obj1)
      objb = c(objb,objc2)
    }
    
    action = rep(action,6)
    object = rep(c(c(obja,objb),c(objb,obja)),3)
    
    #cbind to creat dataframe
    ds = as.data.frame(cbind(rs,verb,pic,ss,object,action))
    ds = ds[order(ss),]
    ds$rs = as.numeric(as.character(ds$rs))
    options(contrasts = c('contr.sum','contr.poly'))
    SCvNSC <-c(0.5,-0.5,0)
    SCvUNR <-c(0,-0.5,0.5)
    contrasts(ds$pic) <-cbind(SCvNSC, SCvUNR)
    
    #can be modify to extract subset of data, in this case, data of sc pic only are extracted
    
    ds=subset(ds,pic=="sc")
    write.csv(ds, file = "ds.csv")
    ds=read.csv(file="ds.csv",header=T)
    print(contrasts(ds$verb))
    ds
  }
  
  dat <- reactive({
    simdata(input$m1,input$m2,input$m3,input$m4,input$m5,input$m6,input$sd1,input$sd2,input$sd3,input$sd4,input$sd5,input$sd6,input$pps,input$items)
  })
  
  plist <- function(){
    
    ad = simdata(input$m1,input$m2,input$m3,input$m4,input$m5,input$m6,input$sd1,input$sd2,input$sd3,input$sd4,input$sd5,input$sd6,input$pps,input$items)
    
    #can be modify to accomodate for test different effect
    
    a = lmer(rs~verb+(1|ss)+(1|action)+(1|object),ad,REML = F)
    b = lmer(rs~1+(1|ss)+(1|action)+(1|object),ad,REML = F)
    
    x = anova(a,b)[2,8]
    x
  }
  
  simpower <- reactive({
    r = 0
    all_data = data.frame()
    pdata = data.frame()
    for (reps in 1:input$reps) {
      r = r + 1
      n_rep=input$reps*10
      p = replicate(10, plist())
      pserial = input$pps
      filename = paste0(as.character(r),".csv")
      filename = paste0("_",filename)
      filename = paste0(as.character(pserial),filename)
      filename = paste0("warning_",filename)
      pdata=cbind(pserial, r,n_rep, p)
      all_data=rbind(all_data,pdata)
      #w_sum=summary(warnings())
      #write.csv(pdata,file="pdata.csv")
      #print(w_sum)
      #write.csv(w_sum,file=filename)
      print(reps)
      write.csv(all_data, file = "all.csv")
    }
    all_data
    })
  
  ps <- reactive({
    anova(model1(),model2())
  })
  
  model1 <- reactive({
    lmer(rs~verb+pic+(1|ss)+(1|action)+(1|object),dat(),REML = F)
  })
  
  model2 <- reactive({
    lmer(rs~verb*pic+(1|ss)+(1|action)+(1|object),dat(),REML = F)
  })
  
  # this is to summarize the simulated data
  datsum <- reactive({
    t = ddply(dat(), c("pic","verb"), summarise, N_logrt= length(rs),sd_logrt = sd(rs), logrt = mean(rs),se_logrt = sd_logrt/sqrt(N_logrt))
  })
  
  # this is the table section
  # the first table is data summary
  output$table1 <- renderTable(datsum())
  
  # the second table is the anova table 
  output$table2 <- renderTable(ps())
  
  output$mytable = DT::renderDataTable({
    dat()
  })

  output$mytable2 = DT::renderDataTable({
    simpower()
  })
  
  # this is the plot section
  # the histogram
  output$distPlot <- renderPlot({
    
    # draw the histogram with the specified number of bins
    g1 = ggplot(data = dat(), aes(x=rs,fill=verb))+
      geom_histogram(aes(y=..density..),position = "dodge",binwidth=.03)+
      geom_density(alpha=0.6)+
      scale_fill_brewer(palette="Dark2")+
      theme_minimal() +
      scale_x_continuous(breaks=seq(-1,2,.5))+
      geom_vline(data=datsum(), aes(xintercept=logrt, color=verb),linetype="dashed")+
      facet_wrap(~pic)
    
    print(g1)
    
    })
  
  # the violin plot
  output$vioPlot <- renderPlot({
    
    g2 = ggplot(data = dat(), aes(x=verb,y=rs, fill=verb))+
      geom_violin(trim = FALSE)+
      scale_fill_brewer(palette="Dark2")+
      geom_boxplot(width=0.1,fill="white")+
      coord_flip()+
      theme_minimal() +
      facet_wrap(~pic)
    
    print(g2)
    
  })
  
  output$histPlotp <- renderPlot({
    g3 = ggplot(data=simpower(), aes(x=p))+
      geom_histogram(aes(y=..density..),binwidth=.01)+
      geom_density(alpha=0.6)+
      geom_vline(xintercept = 0.05,linetype="dashed")+
      geom_vline(xintercept = 0.01,linetype="dashed")+
      theme_minimal() +
      geom_vline(xintercept = 0.001,linetype="dashed")+
      geom_text(aes(x=0.001, y=10, label="p = 0.001",size = 12), colour="red", angle=90, vjust = 1.2)+
      geom_text(aes(x=0.01, y=10,label="p = 0.01",size=12), colour="red", angle=90, vjust = 1.2)+
      geom_text(aes(x=0.05, y=10,label="p = 0.05",size=12), colour="red", angle=90, vjust = 1.2)
    
    print(g3)
  })
  
  output$text1 <- renderText({
    power = mean(simpower()$p < 0.05)
    paste0("The power is: ",power)
  })
  
  
}


# Run the application 

shinyApp(ui = ui, server = server)

