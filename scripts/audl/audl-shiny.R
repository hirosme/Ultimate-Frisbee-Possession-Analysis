## Launches Shiny interactive webapp for throw score rate
## Contains 2 plots, 1 for unconditioned, 1 for conditioned
## that appears upon clicking coordinate on unconditioned plot

library(ggplot2)
library(shiny)
library(shinythemes)

setwd(
  'C:/Users/hiros/Desktop/Storage/documents/Ultimate-Frisbee-Possession-Analysis/data/audl'
)


CONDITIONED_FILENAME = 'conditioned_score-%.csv'

df = read.csv(CONDITIONED_FILENAME)



ui = fluidPage(
  titlePanel(
    h1('UltiMaps')),
  theme = shinytheme('lumen'),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),

  h4('An Ultimate Frisbee analytics tool'),
  fluidRow(
    column(
      hr(style = "border-top: 1px solid #000000;"),
      width = 8,
      'Visualize a throw by selecting a coordinate on the first heat-map. 
      This will generate a heat-map of corresponding throw destinations.
      Select different visualization metrics using the dropdown menu.',
      hr(style = "border-top: 1px solid #ffffff;"),
    )
  ),
  sidebarPanel(
    width = 2,
    selectInput("map_type", "Output Map:",
                choices = c("EPO (Expected Point Outcome)",
                            "Completion Rate",
                            "SPA (Score Probability Added)"
                            # ,
                            # "Ending Score Probability"
                            # ,
                            # "Callahan Territory"
                            )),
    hr(style = "border-top: 1px solid #d6d6d6;"),
    helpText(
      textOutput('map_type_help')
    )
  ),
  fluidRow(
    column(
      width = 3,
      h2(
        'Throw Origin'
      ),
      plotOutput("plot1", height = "800px", width = "600px",
                 click = 'plot1_click'),
      verbatimTextOutput('plot1_click_info')
    ),
    
    column(
      width = 3,
      h2(
        'Throw Destination'
      ),
      plotOutput("plot2", height = "800px", width = "600px",
                 click = 'plot2_click'),
      verbatimTextOutput('plot2_click_info')
    )
  )
  ,
  fluidRow(
    column(
      width = 8,
      hr(style = "border-top: 1px solid #d6d6d6;"),
      'UltiMaps applies knn to over 70,000 throws from the 2021 AUDL season
      to select the 100 throws nearest each origin-destination
      coordinate.', tags$br(),
      'This app was made by Hiro Schmidt. Contact me at hirosme@gmail.com
      with any questions.'
    )
  )
)

server = function(input, output) {
  
  output$map_type_help = renderText({
    if (input$map_type == 'EPO (Expected Point Outcome)') {
      'EPO is an all-encompassing statistic that estimates the expected
      outcome of a point. EPO takes in the start and end coordinates of a
      given throw and factors in completion rate, probability of scoring on
      completion, and probability of conceding a goal on incompletion.
      EPO ranges from -1 to 1 (1 means offensive is guaranteed to score, 
      -1 means defense is guaranteed to score). EPO was first developed by
      Weiss and Childers, "Maps for Reason in Ultimate", 2013'
    } else if (input$map_type == 'Completion Rate') {
      'Completion Rate is the probability that a throw from the selected
      field location will be completed.'
    } else if (input$map_type == 'SPA (Score Probability Added)') {
      'SPA calculates the differential in the offense\'s probability of 
      scoring at the end of a possession upon attempting a given throw,
       compared to before attempting the throw.'
    } 
    else if (input$map_type == 'Ending Score Probability') {
      'Ending Score Probability is the probability that the offense scores
      at the end of its possession if it attempts a given throw.'
    }
    else if (input$map_type == 'Callahan Territory') {
      'Probability that a throw ends in a Callahan.'
    }
  })
  
  output$plot1 = renderPlot({
    df %>% ggplot(aes(
      x = X_0,
      y = Y_0,
      z = Start_Score_Rate,
      fill = Start_Score_Rate
    )) +
      geom_tile() +
      geom_hline(yintercept = 80) +
      scale_fill_distiller(palette = 'Spectral',
                           direction = -1,
                           name = "Score Probability") +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         limits = c(-5, 100)) +
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
    coord = as.data.frame(matrix(c(x_0_cor,y_0_cor),nrow = 1,ncol=2))
    
    df_sub = df[,1:2]
    knn_index = get.knnx(df_sub,coord,1)$nn.index
    
    df[knn_index[1,1],c(1:4,7)]
  })
  
  
  output$plot2 = renderPlot({
    x_cor = input$plot1_click$x
    y_cor = input$plot1_click$y
    coord = as.data.frame(matrix(c(x_cor,y_cor),nrow = 1,ncol=2))

    
    df_sub = df[,1:2]
    knn_index = get.knnx(df_sub,coord,1)$nn.index

    df_cor = unlist(df[knn_index[1,1],1:2])


    df_cond_total = df[df$X_0 == df_cor[1] & 
                   df$Y_0 == df_cor[2]
                   ,]
    #df_cond = df_cond_total[,3:5]
    
    if (input$map_type == 'EPO (Expected Point Outcome)') {
      col_num = 11
    } else if (input$map_type == 'Completion Rate') {
      col_num = 10
    } else if (input$map_type == 'SPA (Score Probability Added)') {
      col_num = 9
    } else if (input$map_type == 'Ending Score Probability') {
      col_num = 8
    } else if (input$map_type == 'Callahan Territory') {
      col_num = 5
    }
    
    df_cond_total %>%
      ggplot(aes_string(
        x = 'X_1',
        y = 'Y_1',
        z = colnames(df)[col_num],
        fill = colnames(df)[col_num]
      )) +
      geom_tile() +
      geom_hline(yintercept = 80) +
      geom_hline(yintercept = 0) +
      scale_fill_distiller(palette = 'Spectral',
                           direction = -1,
                           name = input$map_type) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         limits = c(-25, 100)) +
      ylab('Distance from Endzone') +
      xlab('Target Endzone') +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
      ) +
      geom_point(aes(x = df_cor[1], y = df_cor[2])) +
      scale_shape_manual(values=2)
  })
  output$plot2_click_info = renderPrint({
    
    x_0_cor = input$plot1_click$x
    y_0_cor = input$plot1_click$y
    x_1_cor = input$plot2_click$x
    y_1_cor = input$plot2_click$y
    coord = as.data.frame(matrix(c(x_0_cor,y_0_cor,x_1_cor,y_1_cor),nrow = 1,ncol=4))
    
    df_sub = df[,1:4]
    knn_index = get.knnx(df_sub,coord,1)$nn.index
    if (input$map_type == 'EPO (Expected Point Outcome)') {
      col_num = 11
    } else if (input$map_type == 'Completion Rate') {
      col_num = 10
    } else if (input$map_type == 'SPA (Score Probability Added)') {
      col_num = 9
    } else if (input$map_type == 'Ending Score Probability') {
      col_num = 8
    } else if (input$map_type == 'Callahan Territory') {
      col_num = 5
    }
    
    df[knn_index[1,1],c(1:4,col_num)]
    
    
    
  })
}

shinyApp(ui, server)
