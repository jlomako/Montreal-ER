######################################################################
#
# Occupancy rates in Montreal emergency rooms
#
######################################################################

# install.packages("vroom")
# install.packages("tidyverse")
# install.packages("shiny")

library(shiny)
library(tidyverse)
library(vroom)

# get data from repository
data <- vroom::vroom("https://github.com/jlomako/hospital-occupancy-tracker/raw/main/data/hospitals.csv", show_col_types = FALSE)
max_date <- max(data$Date)
max_value <- max(data[,2:23], na.rm=T)
names(data)[names(data) == "Total"] <- "Total Montréal"

# get hourly data:
url <- "https://www.msss.gouv.qc.ca/professionnels/statistiques/documents/urgences/Releve_horaire_urgences_7jours.csv"
df <- read.csv(url, encoding = "latin1") # using read.csv here until vroom can handle french characters

update <- as.Date(df$Mise_a_jour[1])
update_time <- df$Heure_de_l.extraction_.image.[1]
update_txt <- paste("\nlast update:", update, "at", update_time)

# select montreal hospitals
df <- df %>% filter(str_detect(Nom_etablissement, "Montr|CHUM|CUSM|CHU Sainte-Justine")) %>%
  select(etab = Nom_etablissement, hospital_name = Nom_installation, beds_total = Nombre_de_civieres_fonctionnelles, beds_occ = Nombre_de_civieres_occupees) %>%
  mutate(beds_total = as.numeric(beds_total), beds_occ = as.numeric(beds_occ)) %>% 
  select(hospital_name, beds_occ, beds_total)
## TO DO : remove NA warning: Problem while computing `beds_total = as.numeric(beds_total)`. NAs introduced by coercion 

# calculate total and add to df
df %>% summarise(sum(beds_total, na.rm=TRUE), sum(beds_occ, na.rm=TRUE)) -> total
df <- df %>% add_row(hospital_name = "Total Montréal", beds_occ = total[1,2], beds_total = total[1,1] ) %>%
  mutate(occupancy_rate = round(100*(beds_occ/beds_total)), Date = update) %>%
  select(Date, hospital_name, occupancy_rate)

# small name change
new_name <- "Hôpital général Juif Sir Mortimer B. Davis"
names(data)[names(data) == "L'Hôpital général Juif Sir Mortimer B. Davis"] <- new_name
df$hospital_name <- str_replace(df$hospital_name, "L'Hôpital général Juif Sir Mortimer B. Davis", new_name)

# sort data
df <- df[order(-df$occupancy_rate, df$hospital_name),]

hospitals <- df$hospital_name

ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5, bootswatch = "spacelab"),
  div(class="container px-3",
      
      # plot today
      div(class="row",
          div(class="col-lg-10", 
              h2("Occupancy rates in Montreal emergency rooms"),
              plotOutput("plot_today"))
      ),
      
      # occupancy rates over time
      div(class="row",
          div(class="col-lg-10", 
              h2("Occupancy rates over time"),
              h2("Select a hospital to show occupancy rate over the last few weeks*", class="small"))
      ),
      
      # select hospital to show occupancy rate over time
      div(class="row pt-3",
          div(class="col-lg-10", 
              selectInput(inputId = "hospital", 
                          label = NULL, 
                          choices = hospitals,
                          # selected = selected_hospital,
                          width = "100%")
          )
      ),
      div(class="row",
          div(class="col-lg-10", plotOutput("plot"))
      ),
      
      # source & disclaimer
      div(class="row",
          div(class="col-lg-10 pt-6",
              h1(" ", class = "pt-6"),
              h6("Source: Ministère de la Santé et des Services sociaux du Québec", class="small text-center")),
          h1(" ", class = "pb-6"),
      ),
      
  )
)


server <- function(input, output, session) {
  
  # output_today
  output$plot_today <- renderPlot({
    df %>%
      filter(hospital_name != "Total Montréal") %>%
      mutate(occupancy_rate = ifelse(is.na(occupancy_rate), -0.01, occupancy_rate)) %>%
      ggplot(aes(x = reorder(hospital_name, occupancy_rate), y = occupancy_rate, fill = occupancy_rate)) +
      geom_col(position = "identity", size = 0.5, show.legend = F) +
      geom_text(aes(label = if_else(occupancy_rate < 0, "data not available", NULL)), colour = "grey", size = 3, hjust = "inward", na.rm=T) +
      geom_text(aes(label = if_else(occupancy_rate >= 0 & occupancy_rate <= 69, paste0(occupancy_rate,"%"), NULL)), colour = "grey", size = 3, hjust = -0.2, na.rm=T) +
      geom_text(aes(label = if_else(occupancy_rate > 69, paste0(occupancy_rate,"%"), NULL)), colour = "white", size = 3, hjust = 1, na.rm=T) +
      coord_flip() +
      scale_fill_gradient2(low = "green", high = "red", mid = "#ffff66", midpoint = 60) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, caption = paste(update_txt)) +
      theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
  }, res = 96)
  
  
  # select hospital output  
  selected <- reactive(data %>% select(Date, occupancy = input$hospital))
  output$plot <- renderPlot({
    selected() %>%
      ggplot(aes(Date, occupancy, fill = occupancy)) +
      geom_line(size = 0.5, show.legend = F, na.rm = T) +
      scale_x_date(date_labels = "%b %d", date_breaks = "1 week", minor_breaks = "1 day") +
      scale_y_continuous(limits = c(0,max_value), labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      labs(y = "Occupancy rate*\n", x = NULL, caption = "*occupancy rates at 12 a.m. every day") +
      geom_hline(yintercept = 100, col = "red")
  }, res = 96)
  
}

shinyApp(ui, server)
