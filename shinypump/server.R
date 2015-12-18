library(ggplot2)
library(RColorBrewer)
library(reshape2)
# library(mice)
library(plyr)
library(ggmap)
trainset.values <- read.csv('../trainset_values.csv')
trainset.labels <- read.csv('../trainset_labels.csv')
pumps <- join(trainset.values, trainset.labels)
pumps <- subset(pumps, longitude != 0)
pumps$region_code <- as.factor(pumps$region_code)
pumps$district_code <- as.factor(pumps$district_code)

bbox <- make_bbox(longitude, latitude, data = pumps)
map <- qmap(bbox)

shinyServer(
  function(input, output) {
    output$variables <- renderUI(selectInput('variable', 'Choisir une variable', 
                                             choices = c('basin', 'region', 'region_code', 'district_code',
                                                         'lga', 'ward', 'status_group')))
    output$map <- renderPlot({
      variable <- input$variable
      map <- map + geom_point(aes_string(x='longitude', y='latitude', color=variable), data = pumps)
#       +
#         scale_color_manual(values=c("green", "orange", "red"), 
#                            name="Status")
      map
    })
  }
)
