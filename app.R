library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(scales)
library(doBy)
library(DT)

data <- data.frame(X = rnorm(100,100,25))

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
                                  label = "Refresh")),
        mainPanel(
            titlePanel("Precision - Sample sizes and the Standard Deviation"),
            plotOutput("density"),
            plotOutput("histogram")
        )
    )
)

server <- function(input,output){
    
sampledata <- reactive({
    set.seed(1)
    data <- data.frame(X = rnorm(n = as.numeric(input$n), mean = 100, sd = as.numeric(input$SD)))
    return(data)
})    
    
output$density <- renderPlot({
    data <- sampledata()
    
    density <- density(data$X)
    
    min_den <- 100 - input$SD
    max_den <- 100 + input$SD
    
    ggplot(data = data, aes(x = X))+
        geom_density(colour = "palevioletred2")+
        geom_vline(aes(xintercept = min_den),
                   linetype = "dashed")+
        geom_vline(aes(xintercept = max_den),
                   linetype = "dashed")+
        geom_text(aes(x = 100 - (input$SD + (input$SD/8)),
                      y = max(density$y),
                      label = "-1 SD"),
                      size = 6)+
        geom_text(aes(x = 100 + (input$SD + (input$SD/8)),
                      y = max(density$y),
                      label = "+1 SD"),
                  size = 6)+
        stat_aud(geom = "area",
                 xlim = c(min_den, max_den),
                 alpha = 0.5,
                 fill = "palevioletred2")+
        labs(title = "Density Plot",
             x = "Sampled Values",
             y = "Desnity")+
        theme_minimal()+
        theme(plot.title = element_text(size = 20),
              axis.title = element_text(size = 15),
              axis.text = element_text(siz = 12))
})

output$histogram <- renderPlot({
    data <- sampledata()
    
    min_den <- 100 - input$SD
    max_den <- 100 + input$SD
    
    get_hist <- function(p) {
        d <- ggplot_build(p)$data[[1]]
        data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
        return(d)
    }
    
    p <- ggplot(data = data, aes(x = X))+
        geom_histogram(binwidth = 1)
    
    hist <- get_hist(p)
    
    p+geom_vline(aes(xintercept = min_den),
                                    linetype = "dashed")+
        geom_vline(aes(xintercept = max_den),
                   linetype = "dashed")+
        geom_text(aes(x = 100 - (input$SD + (input$SD/8)),
                      y = max(hist$y),
                      label = "-1 SD"),
                  size = 6)+
        geom_text(aes(x = 100 + (input$SD + (input$SD/8)),
                      y = max(hist$y),
                      label = "+1 SD"),
                  size = 6)+
        annotate(geom = "rect", xmin = 100-input$SD,
                xmax = 100+input$SD,
                ymin = 0,
                ymax = max(hist$y),
                fill = "palevioletred2",
            alpha = 0.5
        )+
        labs(title = "Histogram",
             x = "Sampled Values",
             y = "Count")+
        theme_minimal()+
        theme(plot.title = element_text(size = 20),
              axis.title = element_text(size = 15),
              axis.text = element_text(siz = 12))
})
    
}

shinyApp(ui = ui, server = server)
