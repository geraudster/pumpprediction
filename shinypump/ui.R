library(markdown)
shinyUI(
  navbarPage(div('PumpItUp'),
             tabPanel('Explore data',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('variables')
                        ),
                        mainPanel(
                          h3('Tanzania Map'),
                          imageOutput('map', width = '800', height = '600')
                        )
                      )),
             header = NULL,
             footer = p(br(), wellPanel(includeHTML('license.html')))))