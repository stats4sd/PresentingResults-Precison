library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(scales)
library(doBy)
library(DT)
library(plyr)

StatAreaUnderDensity <- ggproto(
    "StatAreaUnderDensity", Stat,
    required_aes = "x",
    compute_group = function(data, scales, xlim = NULL, n = 50) {
        fun <- approxfun(density(data$x))
        StatFunction$compute_group(data, scales, fun = fun, xlim = xlim, n = n)
    }
)

stat_aud <- function(mapping = NULL, data = NULL, geom = "area",
                     position = "identity", na.rm = FALSE, show.legend = NA, 
                     inherit.aes = TRUE, n = 50, xlim=NULL,  
                     ...) {
    layer(
        stat = StatAreaUnderDensity, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(xlim = xlim, n = n, ...))
}

get_hist <- function(p) {
    d <- ggplot_build(p)$data[[1]]
    data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
    return(d)
}

se <- function(x,y){
    se <- x/sqrt(y)
    return(se)
}

ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
                     sliderInput(inputId = "n",
                                 label = "Select Sample Size",
                                 min = 2, max = 100, value = 50, step = 1),
                     sliderInput(inputId = "SD",
                                 label = "Select Standard Deviation (SD)",
                                 min = 1, max = 25, value = 10, step = 1),
                     selectInput(inputId = "scale",
                                 label = "X - axis",
                                 choices = c("Fixed (50 -150)", "Dynamic")),
                     actionButton(inputId = "refresh",
                                  label = "New Sample")),
        mainPanel(
            titlePanel("Precision - Sample sizes and the Standard Deviation"),
            tabsetPanel(
                tabPanel("Graphs",
            plotOutput("density"),
            tableOutput("summary"),
            plotOutput("histogram")),
            tabPanel("Instructions",
                     htmlOutput("notes"))
            )
        )
    )
)

server <- function(input,output){

counter <- reactiveValues(n = 0)
observeEvent(input$refresh, {counter$n <- counter$n + 1})    
    
sampledata <- eventReactive(input$refresh|counter$n == 0, {
    if(counter$n == 0){
        data <- data.frame(X = rnorm(50,100,10))
    }else{
    data <- data.frame(X = rnorm(n = as.numeric(input$n), mean = 100, sd = as.numeric(input$SD)))
    }
    return(data)
})

min_den <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    min_den <- mean(data$X) - sd(data$X)
    return(min_den)
})

max_den <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    min_den <- mean(data$X) + sd(data$X)
    return(min_den)
})

min_se <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    min_se <- mean(data$X) - (sd(data$X)/sqrt(as.numeric(input$n)))
    return(min_se)
})

max_se <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    max_se <- mean(data$X) + (sd(data$X)/sqrt(as.numeric(input$n)))
    return(max_se)
})

get_hist_dims <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    
    p <- ggplot(data = data, aes(x = X))+
    geom_histogram(binwidth = 1)

    hist <- get_hist(p)
    
    return(hist)
}
)
summary_data <- eventReactive(input$refresh|counter$n == 0,{
    data <- sampledata()
    
    data2 <- data%>%
        dplyr::summarise(n = n(),
                  Mean = mean(X),
                  "Standard Deviation" = sd(X))%>%
        dplyr::mutate("Standard Error" = se(`Standard Deviation`,n))
    
    return(data2)
})

output$density <- 
    renderPlot({
        
    data <- sampledata()
    
    density <- density(data$X)
    
    p <- ggplot(data = data, aes(x = X))+
        geom_density(colour = "palevioletred2")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(density$y)*1.2,
                 x = min_den(),
                 xend = min_den(),
                 linetype = "dashed")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(density$y)*1.2,
                 x = max_den(),
                 xend = max_den(),
                 linetype = "dashed")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(density$y)*1.4,
                 x = min_se(),
                 xend = min_se(),
                 linetype = "dotdash")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(density$y)*1.4,
                 x = max_se(),
                 xend = max_se(),
                 linetype = "dotdash")+
        stat_aud(geom = "area",
                 xlim = c(min_den(), max_den()),
                 alpha = 0.5,
                 fill = "palevioletred2")+
        stat_aud(geom = "area",
                 xlim = c(min_se(), max_se()),
                 alpha = 0.5,
                 fill = "skyblue2")+
        labs(title = "Density Plot",
             x = "Sampled Values",
             y = "Desnity")+
        theme_minimal()+
        theme(plot.title = element_text(size = 20),
              axis.title = element_text(size = 15),
              axis.text = element_text(siz = 12))
    
    if(input$scale == "Dynamic"){
        p+geom_text(aes(x = min_den(),
                        y = max(density$y)*1.25,
                        label = "-1 SD"),
                    size = 6,
                    angle = 45)+
            geom_text(aes(x = max_den(),
                          y = max(density$y*1.25),
                          label = "+1 SD"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = min_se(),
                          y = max(density$y)*1.45,
                          label = "-1 SE"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = max_se(),
                          y = max(density$y*1.45),
                          label = "+1 SE"),
                      size = 6,
                      angle = 45)
    }else{
        p+geom_text(aes(x = min_den()*0.98,
                         y = max(density$y)*1.25,
                         label = "-1 SD"),
                     size = 6,
                     angle = 45)+
            geom_text(aes(x = max_den()*1.02,
                          y = max(density$y*1.25),
                          label = "+1 SD"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = min_se()*0.98,
                          y = max(density$y)*1.45,
                          label = "-1 SE"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = max_se()*1.02,
                          y = max(density$y*1.45),
                          label = "+1 SE"),
                      size = 6,
                      angle = 45)+
            scale_x_continuous(breaks = seq(20,180,10),
                               limits = c(20,180))
    }
})

output$summary <- renderTable({
    data <- summary_data()
    
    data
})

output$histogram <- renderPlot({
        
    data <- sampledata()
    
    p <- ggplot(data = data, aes(x = X))+
        geom_histogram(binwidth = 1)
    
    hist <- get_hist_dims()
    
    p <- p+annotate(geom = "segment",
               y = 0,
               yend = max(hist$y)*1.2,
               x = min_den(),
               xend = min_den(),
               linetype = "dashed")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(hist$y)*1.2,
                 x = max_den(),
                 xend = max_den(),
                 linetype = "dashed")+
        annotate(geom = "rect", xmin = min_den(),
                xmax = max_den(),
                ymin = 0,
                ymax = max(hist$y),
                fill = "palevioletred2",
            alpha = 0.5
        )+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(hist$y)*1.4,
                 x = min_se(),
                 xend = min_se(),
                 linetype = "dotdash")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(hist$y)*1.4,
                 x = max_se(),
                 xend = max_se(),
                 linetype = "dotdash")+
        annotate(geom = "rect", xmin = min_se(),
                 xmax = max_se(),
                 ymin = 0,
                 ymax = max(hist$y),
                 fill = "skyblue2",
                 alpha = 0.5
        )+
        labs(title = "Histogram",
             x = "Sampled Values",
             y = "Count")+
        theme_minimal()+
        theme(plot.title = element_text(size = 20),
              axis.title = element_text(size = 15),
              axis.text = element_text(siz = 12))
    
    if(input$scale == "Dynamic"){
        p+geom_text(aes(x = min_den(),
                         y = max(hist$y)*1.25,
                         label = "-1 SD"),
                     size = 6,
                    angle = 45)+
            geom_text(aes(x = max_den(),
                          y = max(hist$y)*1.25,
                          label = "+1 SD"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = min_se(),
                          y = max(hist$y)*1.45,
                          label = "-1 SE"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = max_se(),
                          y = max(hist$y)*1.45,
                          label = "+1 SE"),
                      size = 6,
                      angle = 45)
    }else{
        p+geom_text(aes(x = min_den()*0.98,
                         y = max(hist$y)*1.25,
                         label = "-1 SD"),
                     size = 6,
                     angle = 45)+
            geom_text(aes(x = max_den()*1.02,
                          y = max(hist$y)*1.25,
                          label = "+1 SD"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = min_se()*0.98,
                          y = max(hist$y)*1.45,
                          label = "-1 SE"),
                      size = 6,
                      angle = 45)+
            geom_text(aes(x = max_se()*1.02,
                          y = max(hist$y)*1.45,
                          label = "+1 SE"),
                      size = 6,
                      angle = 45)+
            scale_x_continuous(breaks = seq(20,180,10),
                               limits = c(20,180))
    }
})

output$notes <- renderText({
    shiny::HTML("<head>
                 <title>Instructions</title>
                 </head>
                 <body>
                 
                 <p>This app is designed to help you explore how you can visualise the precision of estimates and data from a sample </p>
                 
                 <p>To show the graph, please click onto the 'Graphs' button above.</p>
                 
                 <p>Along the left side panel you will see 3 inputs for this app.</p>
                 
                 <ul>
                 <li>Sample Size (n) - A scale of 2 - 100 determining the size of the random sample being drawn</li>
                 <li>Standard Deviation - A scale from 1 - 25 to choose what the standard deviation of the randomly drawn sample should be.</li>
                 <li>X-axis scale - A choice of how to present the x-axis scales of the graphs. Either fixed of 20 - 180 or a more dynamic option adapted to the actual range of the data</li>
                 </ul>
                 
                 <p>Using these inputs you can customise a randomly drawn sample. This sample will assume a normal distribution with mean of 100 and you selected standard deviation.</p>
                 
                 <p>After selecting your inputs, please press 'New Sample' to create a new random sample which will update the graphs and table.</p>
                 
                 <p>There are 2 graphs on display. A density plot and a histogram. These show off the distribution of the randomly drawn numbers.</br>
                 Using shaded sections they also show the areas corresponding to being within +/- 1 standard deviation and +/- 1 standard error.</p>
                 
                 <p>A table has also been included to show the estimates of the mean, standard deviation and standard error from the random sample.</p>
                 
                 <p>By playing around with this app, hopefully you should be able to notice a few things</p>
                 
                 <ul>
                 <li>The standard deviation is always larger than the standard error</li>
                 <li>THe standard deviation is unaffected by changes in the sample size but the standard error should decrease as the sample size increases</li>
                 <li>A larger sample size should give you a more consistent estimate od the mean and therefore more precision</li>
                 <li>The larger the standard deviation, the less precise your estimates are</li>
                 <li>A larger sample size also gives you a more consistent estimate of the standard deviation as this is also something being estimated from the data as well as the mean</li>
                 </ul>
                 
                 </body>")
})
    
}

shinyApp(ui = ui, server = server)
