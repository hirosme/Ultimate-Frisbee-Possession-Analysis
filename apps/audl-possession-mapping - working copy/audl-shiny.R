## Launches Shiny interactive webapp for throw score rate
## Contains 2 plots, 1 for unconditioned, 1 for conditioned
## that appears upon clicking coordinate on unconditioned plot

library(ggplot2)
library(tidyverse)
library(shiny)
library(shinythemes)
library(FNN)

## SET DIRECTORY OF APPLET HERE:
setwd(
  "C:/Users/hiros/Desktop/Storage/documents/Ultimate-Frisbee-Possession-Analysis/apps/audl-possession-mapping"
)


UNCONDITIONED_FILENAME = 'unconditioned_score-%.csv'

CONDITIONED_FILENAME = 'conditioned_score-%.csv'

df1 = read.csv(UNCONDITIONED_FILENAME)
df2 = read.csv(CONDITIONED_FILENAME)



ui = fluidPage(
  titlePanel('Possession Score Probability by Field Position'),
  theme = shinytheme('lumen'),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),

  fluidRow(
    column(
      width = 4,
      h4(
        'Probability that a throw attempt from a given location will lead to a goal at the end of the possession \n'
      ),
      plotOutput("plot1", height = "800px", width = "600px",
                 click = 'plot1_click'),
      verbatimTextOutput('plot1_click_info')
    ),
    column(
      width = 4,
      h4(
        'Probability that a throw attempt from the clicked location will lead to a goal at the end of the possession, mapped by throw destination'
      ),
      plotOutput("plot2", height = "800px", width = "600px",
                 click = 'plot2_click'),
      verbatimTextOutput('plot2_click_info')
    )
  ) , 
  fluidRow(
    column(
      width = 8,
      h4(
        'Click a throw start location on the left map to generate a map of score probabilities at different throw destinations'
      )
    )
  )
)

server = function(input, output) {
  output$plot1 = renderPlot({
    df1 %>% ggplot(aes(
      x = X,
      y = Y,
      z = Score_Prob,
      fill = Score_Prob
    )) +
      geom_tile() +
      geom_hline(yintercept = 80) +
      scale_fill_distiller(palette = 'Spectral',
                           direction = -1,
                           name = "Score Probability") +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         limits = c(0, 100)) +
      ylab('Distance from Endzone') +
      xlab('Target Endzone') +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
      )
    
  })
  
  output$plot1_click_info = renderPrint({
    x_0_cor = input$plot1_click$x
    y_0_cor = input$plot1_click$y

    coord = as.data.frame(matrix(c(y_0_cor,x_0_cor),nrow = 1,ncol=2))

    df1_sub = df1[,1:2]
    knn_index = get.knnx(df1_sub,coord,1)$nn.index
    
    df1[knn_index[1,1],]
  })
  
  
  output$plot2 = renderPlot({
    x_cor = input$plot1_click$x
    y_cor = input$plot1_click$y
    coord = as.data.frame(matrix(c(x_cor,y_cor),nrow = 1,ncol=2))

    
    df2_sub = df2[,1:2]
    knn_index = get.knnx(df2_sub,coord,1)$nn.index

    df2_cor = unlist(df2[knn_index[1,1],1:2])


    df2_cond_total = df2[df2$X_0 == df2_cor[1] & 
                   df2$Y_0 == df2_cor[2]
                   ,]
    df2_cond = df2_cond_total[,3:5]

    df2_cond %>%
      ggplot(aes(
        x = X,
        y = Y,
        z = Score_Prob,
        fill = Score_Prob
      )) +
      geom_tile() +
      geom_hline(yintercept = 80) +
      geom_hline(yintercept = 0) +
      scale_fill_distiller(palette = 'Spectral',
                           direction = -1,
                           name = "Score Probability") +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         limits = c(-20, 100)) +
      ylab('Distance from Endzone') +
      xlab('Target Endzone') +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
      ) +
      geom_point(aes(x = df2_cor[1], y = df2_cor[2])) +
      scale_shape_manual(values=2)
  })
  output$plot2_click_info = renderPrint({
    x_0_cor = input$plot1_click$x
    y_0_cor = input$plot1_click$y
    x_1_cor = input$plot2_click$x
    y_1_cor = input$plot2_click$y
    coord = as.data.frame(matrix(c(x_0_cor,y_0_cor,x_1_cor,y_1_cor),nrow = 1,ncol=4))
    
    df2_sub = df2[,1:4]
    knn_index = get.knnx(df2_sub,coord,1)$nn.index
    
    df2[knn_index[1,1],]
    
    
    
  })
}

shinyApp(ui, server)
