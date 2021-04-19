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
                                 label = " Select Sample Size",
                                 min = 2, max = 100, value = 50, step = 1),
                     sliderInput(inputId = "SD",
                                 label = "Select Standard Deviation (SD)",
                                 min = 1, max = 25, value = 10, step = 1),
                     actionButton(inputId = "refresh",
                                  label = "New Sample")),
        mainPanel(
            titlePanel("Precision - Sample sizes and the Standard Deviation"),
            plotOutput("density"),
            tableOutput("summary"),
            plotOutput("histogram")
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
    min_den <- 100 - input$SD
    return(min_den)
})

max_den <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    min_den <- 100 + input$SD
    return(min_den)
})

min_se <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    min_se <- 100 - (as.numeric(input$SD)/sqrt(as.numeric(input$n)))
    return(min_se)
})

max_se <- eventReactive(input$refresh|counter$n == 0, {
    data <- sampledata()
    max_se <- 100 + (as.numeric(input$SD)/sqrt(as.numeric(input$n)))
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
    
    ggplot(data = data, aes(x = X))+
        geom_density(colour = "palevioletred2")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(density$y)*1.1,
                 x = min_den(),
                 xend = min_den(),
                 linetype = "dashed")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(density$y)*1.1,
                 x = max_den(),
                 xend = max_den(),
                 linetype = "dashed")+
        geom_text(aes(x = min_den()*0.97,
                      y = max(density$y)*1.1,
                      label = "-1 SD"),
                      size = 6)+
        geom_text(aes(x = max_den()*1.03,
                      y = max(density$y*1.1),
                      label = "+1 SD"),
                  size = 6)+
        geom_vline(aes(xintercept = min_se()),
                   linetype = "dotdash")+
        geom_vline(aes(xintercept = max_se()),
                   linetype = "dotdash")+
        geom_text(aes(x = min_se()*0.97,
                      y = max(density$y)*1.25,
                      label = "-1 SE"),
                  size = 6)+
        geom_text(aes(x = max_se()*1.03,
                      y = max(density$y*1.25),
                      label = "+1 SE"),
                  size = 6)+
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
              axis.text = element_text(siz = 12))+
        scale_x_continuous(breaks = seq(50,150,10),
                           limits = c(50,150))
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
    
    p+annotate(geom = "segment",
               y = 0,
               yend = max(hist$y)*1.1,
               x = min_den(),
               xend = min_den(),
               linetype = "dashed")+
        annotate(geom = "segment",
                 y = 0,
                 yend = max(hist$y)*1.1,
                 x = max_den(),
                 xend = max_den(),
                 linetype = "dashed")+
        geom_text(aes(x = min_den()*0.97,
                      y = max(hist$y)*1.1,
                      label = "-1 SD"),
                  size = 6)+
        geom_text(aes(x = max_den()*1.03,
                      y = max(hist$y)*1.1,
                      label = "+1 SD"),
                  size = 6)+
        annotate(geom = "rect", xmin = min_den(),
                xmax = max_den(),
                ymin = 0,
                ymax = max(hist$y),
                fill = "palevioletred2",
            alpha = 0.5
        )+
        geom_vline(aes(xintercept = min_se()),
                   linetype = "dashed")+
        geom_vline(aes(xintercept = max_se()),
                   linetype = "dashed")+
        geom_text(aes(x = min_se()*0.97,
                      y = max(hist$y)*1.25,
                      label = "-1 SE"),
                  size = 6)+
        geom_text(aes(x = max_se()*1.03,
                      y = max(hist$y)*1.25,
                      label = "+1 SE"),
                  size = 6)+
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
              axis.text = element_text(siz = 12))+
        scale_x_continuous(breaks = seq(50,150,10),
                             limits = c(50,150))
})
    
}

shinyApp(ui = ui, server = server)
