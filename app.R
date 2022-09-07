######################################################################
#
# Occupancy rates in Montreal emergency rooms
#
######################################################################

# install.packages("vroom")
# install.packages("tidyverse")
# install.packages("shiny")

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)

# get data from repository
data <- vroom::vroom("https://github.com/jlomako/hospital-occupancy-tracker/raw/main/data/hospitals.csv", show_col_types = FALSE)

# get last 90 days
data <- data %>% filter(Date >= (Sys.Date()-90))

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

# get current max value (for y-axis in weekly plot)
max_today <- max(df$occupancy_rate, na.rm=T)

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
  
  tags$head(HTML("<title>Occupancy rates in Montréal emergency rooms</title>")),
  
  div(class="container-md px-0",
      
      h1("Occupancy rates in Montréal emergency rooms", class="text-center pt-3"),
      
      # card current occupancy 
      div(class="row pt-5",
          div(class="col-lg-12",
              div(class="card",
                  div(class="card-header bg-primary", h5("Current Occupancy Rates in Montréal", class="card-title")),
                  div(class="card-body px-0", plotOutput("plot_today")),
                  div(class="card-footer", h5("The occupancy rate is defined by the total number of patients on stretchers divided by the number of available stretchers.
                                               Wait times may vary depending on the number of patients and the nature of your illness or injury.",
                                              class="small"))
              ),    
          ),
      ),
      
      # card with tabs
      div(class="row pt-5",
          div(class="col-lg-12",
              div(class="card",
                  div(class="card-header bg-primary", h5("Select a hospital", class="card-title")),
                  div(class="card-body",
                      div(selectInput(inputId = "hospital", 
                                      label = NULL, # "Select a hospital", #NULL 
                                      choices = hospitals,
                                      width = "100%")
                      ),
                      #   ),
                      # div(class="card-body",
                      tabsetPanel(type = "tabs",
                                  tabPanel("current", plotOutput("plot_weekdays")),
                                  tabPanel("past 90 days", plotOutput("plot"))
                      ),
                  ),
                  div(class="card-footer", h5('This website is for informational purposes only. If you are in need of urgent medical treatment, visit your nearest ER or call 9-1-1.
                    In case of a non-urgent health issue call 8-1-1', 
                                              tags$a(href="https://www.quebec.ca/en/health/finding-a-resource/info-sante-811/", "(Info Santé)"),
                                              class="small")),
              ), # card end
          ), # col end
      ), # row end
      
      # source & disclaimer
      div(class="row",
          div(class="col-lg-12",
              h6("Data source: Ministère de la Santé et des Services sociaux du Québec", class="small text-center pt-3"),
              h6("© Copyright 2022, jlomako", class="small text-center"),
          ),
      ),
  ),
)




server <- function(input, output, session) {
  
  # output_today
  output$plot_today <- renderPlot({
    df %>%
      filter(hospital_name != "Total Montréal") %>%
      mutate(occupancy_rate = ifelse(is.na(occupancy_rate), -0.01, occupancy_rate)) %>%
      ggplot(aes(x = reorder(hospital_name, occupancy_rate), y = occupancy_rate, fill = occupancy_rate)) +
      geom_col(position = "identity", size = 3, show.legend = F) +
      scale_y_continuous(expand = c(0,0)) + # gets rid of gap between y-axis and plot
      geom_text(aes(label = if_else(occupancy_rate < 0, "no data", NULL)), colour = "grey", size = 3, hjust = "inward", na.rm=T) +
      geom_text(aes(label = if_else(occupancy_rate >= 0 & occupancy_rate <= 79, paste0(occupancy_rate,"%"), NULL)), colour = "darkgrey", size = 3, hjust = -0.1, position = position_stack(vjust = 0), na.rm=T) +
      geom_text(aes(label = if_else(occupancy_rate > 79, paste0(occupancy_rate,"%"), NULL)), colour = "white", size = 3, hjust = -0.1, position = position_stack(vjust = 0), na.rm=T) +
      coord_flip() +
      scale_fill_gradient2(low = "green", high = "red", mid = "#ffff66", midpoint = 60) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, caption = paste(update_txt)) +
      theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
  }, res = 96, height = "auto")
  
  
  # select hospital and get data for selected hospital
  selected <- reactive(data %>% select(Date, occupancy = input$hospital))
  
  # get current occupancy value for selected hospital
  occupancy_current <- reactive(filter(df, str_detect(hospital_name, input$hospital))[1,3])
  # OBS! don't forget parenthesis when calling these variables! => occupancy_current()
  
  weekday_current <- lubridate::wday(update, label = T)
  
  # plot_weekdays: means for each day
  output$plot_weekdays <- renderPlot({
    # layer for current selected occupancy, if no data available print text
    if (is.na(occupancy_current())) {
      p <- annotate("text", x=weekday_current, y=10, label = "", colour = "white", size = 2) 
      subtitle_txt <- "(No data available)"
    } else {
      p <- geom_col(aes(x=weekday_current, y=occupancy_current(), fill = occupancy_current(), alpha = 0.1), position = "identity", show.legend = F)
      subtitle_txt <- paste0("Occupancy rate: ", occupancy_current(), "%")
    }
    # get data and plot    
    selected() %>%
      # filter(Date >= (Sys.Date()-30)) %>%
      mutate(day_number = as.POSIXlt(Date)$wday+1) %>% # Sun = 1, Mon = 2 etc
      group_by(day_number) %>% 
      summarise(occupancy_mean = round(mean(occupancy, na.rm=T))) %>%
      ggplot(aes(x = lubridate::wday(day_number, label = T), y = occupancy_mean, fill = occupancy_mean)) +
      geom_col(position = "identity", show.legend=F, alpha = 0.2, na.rm=T) +
      scale_y_continuous(limits = c(0,300), expand = c(0,0)) + # OBS!!! max_today
      labs(title = input$hospital, subtitle = subtitle_txt, y = NULL, x = NULL, caption = NULL) +
      geom_hline(yintercept=100, linetype="dashed", color = "red") +
      theme_minimal() +
      scale_fill_gradient2(low = "yellow", high = "red") + 
      theme(panel.grid = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
            plot.subtitle=element_text(size=13, color="red")) + 
      p # layer for current selected occupancy
  }, res = 96)
  
  # plot: past 90 days
  output$plot <- renderPlot({
    selected() %>%
      ggplot(aes(Date, occupancy, fill = occupancy)) +
      geom_line(size = 0.5, show.legend = F, na.rm = T) +
      scale_x_date(expand = c(0,0), date_labels = "%a, %b %d", date_breaks = "1 week", minor_breaks = "1 day") +
      scale_y_continuous(expand = c(0,0), limits = c(0,max_value), labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      labs(title = input$hospital, y = NULL, x = NULL, caption = "\n*occupancy rates at 12 a.m. every day") +
      geom_hline(yintercept = 100, linetype="dashed", col = "red") +
      theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5))
  }, res = 96)
  
}

shinyApp(ui, server)


