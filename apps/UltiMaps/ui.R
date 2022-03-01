library(shinythemes)

ui = fluidPage(
  titlePanel(
    'UltiMaps'),
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
      This will generate a second heat-map of corresponding throw destinations.
      Select different visualization metrics using the dropdown menu. Click
      a destination coordinate on the second map for detailed info about the
      throw, including estimated calculation accuracy.',
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
      width = 4,
      h2(
        'Throw Origin'
      ),
      
      verbatimTextOutput('plot1_click_info'),
      plotOutput("plot1", height = "540px", width = "400px",
                 click = 'plot1_click')
    ),
    
    column(
      width = 4,
      h2(
        'Throw Destination'
      ),
      
      verbatimTextOutput('plot2_click_info'),
      plotOutput("plot2", height = "640px", width = "400px",
                 click = 'plot2_click')
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
