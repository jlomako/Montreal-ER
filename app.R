######################################################################
#
# Occupancy rates in Montreal emergency rooms
#
######################################################################

# install.packages("vroom")
# install.packages("tidyverse")
# install.packages("shiny")

library(shiny)
library(vroom)
library(tidyverse)

# use data from my repository
data <- vroom::vroom("https://github.com/jlomako/pdfscraper/raw/main/data/daily_data.csv", show_col_types = FALSE)

# get values
hospitals <- names(data[2:22])
max_value <- max(data[,2:22], na.rm=T)
max_date <- max(data$Date)
# get hospital name with todays max occupancy
selected_hospital <- names(which.max(data[length(data$Date),2:21]))
if (is.na(selected_hospital)) { selected_hospital <- "Total Montréal" } 


ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5, bootswatch = "spacelab"),
  div(class="container-fluid py-4",
      
      # plot today
      div(class="row",
          div(class="col-12", 
              h2("Occupancy rates in Montreal emergency rooms"),
              plotOutput("plot_today"))
      ),
      
      # select hospital to show occupancy rate over time
      div(class="row pt-3",
          div(class="col-12", 
              selectInput(inputId = "hospital", 
                          label = "Select hospital to show occupancy rate over time*", 
                          choices = hospitals,
                          selected = selected_hospital,
                          width = "100%")
          )
      ),
      div(class="row",
          div(class="col-12", plotOutput("plot"))
      ),
  )
)


server <- function(input, output, session) {
  
  # output_today
  output$plot_today <- renderPlot({
    data %>%
      mutate(Date = as.character(Date)) %>%
      filter(Date == max(Date)) %>%
      pivot_longer(cols = 2:22) %>%
      filter(name != "Total Montréal") %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = reorder(name, value), y = value, fill = value)) +
      geom_col(position = "identity", size = 0.5, show.legend = F) +
      geom_text(aes(label = paste0(value,"%")), hjust = 1, colour = "white", size = 3) +
      coord_flip() +
      scale_fill_gradient2(low = "light green", high = "red", mid = "yellow", midpoint = 80) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, caption = paste("last update: ", max_date)) +
      theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
  }, res = 96)
  

  # select hospital output  
  selected <- reactive(data %>% select(Date, occupancy = input$hospital))
  output$plot <- renderPlot({
    selected() %>%
      ggplot(aes(Date, occupancy, fill = occupancy)) +
      geom_line(size = 0.5, show.legend = F, na.rm = T) +
      # geom_col(position = "identity", size = 0.5, show.legend = F, na.rm = T)  +
      scale_x_date(date_labels = "%b %d", date_breaks = "1 week", minor_breaks = "1 day") +
      scale_y_continuous(limits = c(0,max_value), labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      labs(y = "Occupancy rate\n", x = NULL, caption = "*occupancy rates are saved at 1 p.m. every day") +
      # theme(panel.grid.major = element_line()) + # horizontal lines only
      # scale_fill_gradient2(low = "light green", high = "red", mid = "yellow", midpoint = 80) +
      geom_hline(yintercept = 100, col = "red")
  }, res = 96)

}

shinyApp(ui, server)

