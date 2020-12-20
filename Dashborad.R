library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Crime Info in Chicago"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Tab 1: Frequency of Crime", tabName = "tab1", icon = icon("th")),
    menuItem("Tab 2: Location of Crimes", tabName = "tab2", icon = icon("th")),
    menuItem("Tab 3: Realationship", tabName = "tab3", icon = icon("th")),
    menuItem("Tab 4: Crimes in District", tabName = "tab4", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("tab1",
              h3("Frequency of Crime by Month and Crime Type"),
              box(title = "Input", status = "primary", solidHeader = TRUE,
                radioButtons(inputId = "month", 
                              label = "Select the Month:", 
                              choices = sort(unique(crimes1$Date[which(!is.na(crimes1$Date))])), 
                              selected = c('Jan'), 
                              inline = TRUE,
                              width = '150%')),
              box(plotOutput('distPlot'), width = 11, height = "570px")
      ),
      tabItem('tab2',
              h3("Location of Crimes by Date"),
              box(dateInput(inputId = "crime_date", 
                          label = "Select a Date:", 
                          value = '2020-01-09', 
                          format = 'yyyy/mm/dd', 
                          weekstart = 1,
                          datesdisabled = NA,
                          min = '2020-01-09',
                          max = '2020-09-09')
              ),
              box(leafletOutput("map"),width = 8, height = "450px")
      ),
      tabItem('tab3',
              h3("Realationship between Crime Type and Time"),
              box(
                selectInput(inputId = "myhour", 
                            label = "Select the Hour of the day:", 
                            choices = sort(unique(crimes3$Date)),
                            selected = 00,
                            multiple = TRUE),
                selectInput(inputId = "mytype",
                            label = "Select the Type of Crime:",
                            choices = unique(crimes3$Primary.Type),
                            selected = "THEFT",
                            multiple = TRUE)
              ),
              box(plotOutput(outputId = "myheat"), width = 8, height = "450px")
      ),
      tabItem('tab4',
              h3("Crimes in different District in Chicago"),
              box(
                selectInput(inputId = "district", 
                            label = "Select the District:", 
                            choices = sort(unique(crimes4$District)), 
                            selected = '1', 
                            selectize = TRUE,
                            multiple = FALSE)
              ),
              box(
                plotOutput(outputId = "mypie"),
                plotOutput(outputId = "mybar"),
                width = 10, height = "715px"
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    month <- input$month
    my_data <- subset(crimes1, Date == month)
    ggplot(my_data, aes(x = my_data$Primary.Type,fill = my_data$Primary.Type)) + geom_bar() +
      theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) +
      theme(legend.position="top") +
      scale_fill_discrete(name = "Crime Type") +
      geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2)) +
      xlab("Crime Type") +
      ylab('Frequency')
  }, height = 550, width = 930)
  
  output$map <- renderLeaflet({
    mydata2 <- subset(crimes2, Date == input$crime_date)
    leaflet(data = mydata2) %>%
      addTiles() %>%
      addMarkers(~mydata2$Longitude, ~mydata2$Latitude, 
                 popup = ~mydata2$Location.Description)
  })
  
  mydata3 <- subset(crimes3, select = c(ID, Primary.Type, Date))
  
  heatmap <- reactive({
    a3 <- mydata3 %>% filter(crimes3$Primary.Type == input$mytype)
    b3 <- a3 %>% filter(a3$Date == as.numeric(input$myhour))
    b3 <- b3 %>% 
      group_by(Primary.Type, Date) %>% 
      summarise(counts =n())})
  
  output$myheat <- renderPlot({
    heat1 <- heatmap()
    ggplot(heat1, aes(Date, Primary.Type, fill = counts)) + 
      geom_tile(colour="black",size=0.25) +
      labs(x="Hour", y="Crime Type", title="Heatmap: Crime Type & Hour") +
      scale_fill_gradient(low = "white", high = "red") +
      geom_text(label = heat1$counts)
  })
  
  output$mypie <- renderPlot({
    mydata4a <- crimes4 %>% 
      group_by(District, Primary.Type) %>%
      summarise(counts4a = n())
    mydata4a <- subset(mydata4a, District == input$district)
    ggplot(mydata4a, aes(x = "", y = counts4a, fill = Primary.Type)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      theme_void()
  }, height = 320, width = 640)
  
  output$mybar <- renderPlot({
    mydata4b <- crimes4 %>% 
      group_by(District, Date) %>%
      summarise(counts4b = n())
    mydata4b <- subset(mydata4b, Date != "NA")
    mydata4b <- subset(mydata4b, District == input$district)
    options(repr.plot.width=8, repr.plot.height=3)
    ggplot(mydata4b, aes(x = Date, y = counts4b)) +
      geom_bar(stat = "identity", fill = '#008000') +
      coord_flip() + scale_y_continuous(name="Sum of Crimes") +
      scale_x_discrete(name="Month") +
      theme(axis.text.x = element_text(face="bold", color="#008000",
                                       size=12, angle=0),
            axis.text.y = element_text(face="bold", color="#008000",
                                       size=12, angle=0)) +
      geom_text(label = mydata4b$counts4b)
  }, height = 300, width = 600)
}

shinyApp(ui, server)