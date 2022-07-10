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

# start shiny app:
ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://bootswatch.com/5/spacelab/bootstrap.min.css")),
  div(class="container-fluid shadow-none p-4 mb-5 bg-light",
      div(class="row pt-3",
          div(class="col-12", 
              HTML(r"(<h2>Occupancy rates in Montreal emergency rooms</h2><br><br>)"),
              selectInput(inputId = "hospital", 
                          label = "Select a hospital", 
                          choices = hospitals,
                          width = "100%")
          )
      ),
      div(class="row",
          div(class="col-12", plotOutput("plot"))
      )
  )
)

# back end
server <- function(input, output, session) {
  selected <- reactive(data %>% select(Date, occupancy = input$hospital))
  # plot output
  output$plot <- renderPlot({
    selected() %>%
      ggplot(aes(Date, occupancy, fill = occupancy)) +
      geom_col(position = "identity", size = 0.5, show.legend = F, na.rm = T)  +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
      theme_minimal() +
      labs(y = NULL, x = NULL) +
      ylim(0,200) +
      theme(axis.text.x = element_text(angle = 90), 
            panel.grid.major.x = element_blank(), # remove vertical grid lines
            panel.grid.major.y = element_line()) + # horizontal lines only
      scale_fill_gradient2(low = "light green", high = "red", mid = "yellow", midpoint = 80)
  }, res = 96)
}

shinyApp(ui, server)

