# R-Projects
> This is a web_based Shiny App that visualizes the crime situation in Chicago City from 01/09/2020 to 09/09/2020.

## Table of contents
* [General info](#general-info)
* [Screenshots](#screenshots)
* [Technologies](#technologies)
* [CodeExample](#CodeExample)

## General info
This project consists of four parts:
* Tab1: Frequency of crime by Month and Crime Type. 
* Tab2: Location of Crimes by Date
* Tab3: Relationship between Crime Type and Time
* Tab4: Crimes in District

## Screenshots
![Screen Shot 2020-12-20 at 3 36 40 PM](https://user-images.githubusercontent.com/63559049/102727232-5c5f0680-42d9-11eb-9ff5-1bd625029591.png)

## Technologies
* Programming Language: R
* Libraries: shiny, shinydashboard, ggplot2, leaflet, dplyr

## Code Examples
`  output$distPlot <- renderPlot({
    month <- input$month
    my_data <- subset(crimes1, Date == month)
    ggplot(my_data, aes(x = my_data$Primary.Type,fill = my_data$Primary.Type)) + geom_bar() +
      theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) +
      theme(legend.position="top") +
      scale_fill_discrete(name = "Crime Type") +
      geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2)) +
      xlab("Crime Type") +
      ylab('Frequency')
  }, height = 450, width = 850)`
