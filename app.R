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

# use data from my pdfscraper
data <- vroom::vroom("https://github.com/jlomako/pdfscraper/raw/main/data/daily_data.csv")

# get hospital names
hospitals <- names(data[2:22])
# get max value (for plot)
max_value <- max(data[,2:22], na.rm=T)
max_date <- max(data$Date)

# start shiny app:
ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5, bootswatch = "spacelab"),
  # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://bootswatch.com/5/spacelab/bootstrap.min.css")),
  div(class="container-fluid shadow-none p-4 mb-5 bg-light",
      div(class="row pt-3",
          div(class="col-12", 
              h2("Occupancy rates in Montreal emergency rooms", class="pb-3"),
              selectInput(inputId = "hospital", 
                          label = "Select a hospital", 
                          choices = hospitals,
                          selected = "Total Montréal",
                          width = "100%")
          )
      ),
      div(class="row",
          div(class="col-12", plotOutput("plot"))
      ),
      # plot today
      div(class="row",
          div(class="col-12 pt-3", 
              h2("Today's occupancy"),
              plotOutput("plot_today"))
      ),
  )
)

# back end
server <- function(input, output, session) {

  # plot select hospital output  
  selected <- reactive(data %>% select(Date, occupancy = input$hospital))
  output$plot <- renderPlot({
    selected() %>%
      ggplot(aes(Date, occupancy, fill = occupancy)) +
      geom_col(position = "identity", size = 0.5, show.legend = F, na.rm = T)  +
      scale_x_date(date_labels = "%b %d", date_breaks = "1 week", minor_breaks = "1 day") +
      scale_y_continuous(limits = c(0,max_value), labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      labs(y = "Occupancy rate\n", x = NULL) +
      theme(panel.grid.major.y = element_line()) + # horizontal lines only
      scale_fill_gradient2(low = "light green", high = "red", mid = "yellow", midpoint = 80) +
      geom_hline(yintercept = 100, col = "red")
  }, res = 96)
  
  
  # plot output_today
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
      scale_y_continuous(limits = c(0,max_value), labels = scales::percent_format(scale = 1)) +
      coord_flip() +
      scale_fill_gradient2(low = "light green", high = "red", mid = "yellow", midpoint = 80) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, caption = paste("\nlast update: ", max_date)) +
      theme(panel.grid.minor = element_blank())
  }, res = 96)
  
  
  
}

shinyApp(ui, server)
