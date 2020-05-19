ui<- fluidPage(
  titlePanel("Association Rule Mining"),
  sidebarLayout(
    sidebarPanel(
      sliderInput('sup', "Support", min = 0.01, max = 1, value = 0.25, step = 0.005),
      sliderInput('conf', 'Confidence', min = 0.01, max =1, value = 0.25, step = 0.005),
      sliderInput('len', 'Minimum Rule Length', min = 1, max =10, value = 2, step = 1),
      sliderInput('mlen', 'Maximum Rule Length', min = 1, max =20, value = 10, step = 1),
      sliderInput('time', 'Maximum Time Taken', min = 1, max =15, value = 3, step = 1)
    ),
    mainPanel(
      tabsetPanel(id = 'ARM',
                  tabPanel('Plot', value = 'graph',plotOutput('plot')),
                  tabPanel('Rules',value = 'table',tableOutput('rules')))
                 
    )
  )
)