####################################################
# ER OCCUPATION
####################################################

library(shiny)
library(vroom)
library(tidyverse)

# download file
# download.file("https://github.com/jlomako/pdfscraper/raw/main/data/daily_data.csv", "daily_data.csv", quiet = TRUE)
# data <- vroom::vroom("daily_data.csv")

data <- vroom::vroom("https://github.com/jlomako/pdfscraper/raw/main/data/daily_data.csv")

# get hospital names
hospitals <- names(data[2:22])

# front end interface
ui <- fluidPage(
  
      # use bootstrap
      theme = bslib::bs_theme(bootswatch = "flatly"),
      
      # insert some HTML code
      HTML(r"(<br><br>)"),
  
      # show hospital selector
      fluidRow(column(12, selectInput(inputId = "hospital", 
                                                label = "Select a hospital", 
                                                choices = hospitals, 
                                                width = "100%"))),
      # show plot
      fluidRow(column(12, plotOutput("plot"))),
  
      # insert some HTML code
      HTML(r"(<br><br>)"),
                
      # show table
      fluidRow(column(12, tableOutput("table")))
)

# back end
server <- function(input, output, session) {
  # OBS! input objects are read-only
  selected <- reactive(data %>% select(Date, occupancy = input$hospital))
  
  # OBS! always use output object with render function
  # plot output
  output$plot <- renderPlot({
    selected() %>%
      ggplot(aes(Date, occupancy, fill = occupancy)) +
      geom_col(position = "identity", size = 0.5, show.legend = F)  +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") + # minor_breaks = "1 day"
      # geom_text(aes(label = paste0(occupancy,"%")), vjust = 1.5, colour = "white", size = 3.5) +
      theme_minimal() +
      labs(y = NULL, x = NULL) + # title = input$hospital)
      ylim(0,200) +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_gradient2(low = "light green", high = "red", mid = "yellow", midpoint = 80)
  }, res = 96)
  
  # table output
  output$table <- renderTable(
    # change weird number to actual date
    selected() %>% mutate(Date = as.character(Date))
  )

}

shinyApp(ui, server)


