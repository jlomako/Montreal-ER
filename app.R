######################################################################
#
# Occupancy rates in Montreal emergency rooms
#
######################################################################

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(leaflet)

# get data files
source("helper.R")


# get last 90 days from repository
data <- data %>% filter(Date >= (Sys.Date()-90))

max_date <- max(data$Date)
max_value <- max(data[,2:23], na.rm=T)
names(data)[names(data) == "Total"] <- "Total Montréal"

# df = data from mssss  
update <- as.Date(df$Mise_a_jour[1])
update_time <- df$Heure_de_l.extraction_.image.[1]
update_txt <- paste("\nlast update:", update, "at", update_time)
weekday_current <- lubridate::wday(update, label = T)


# select hospitals
df <- df %>% filter(str_detect(Nom_etablissement, "Montr|CHUM|CUSM|CHU Sainte-Justine")) %>%
  select(etab = Nom_etablissement, hospital_name = Nom_installation, beds_total = Nombre_de_civieres_fonctionnelles, beds_occ = Nombre_de_civieres_occupees) %>%
  mutate(beds_total = suppressWarnings(as.numeric(beds_total)), beds_occ = suppressWarnings(as.numeric(beds_occ))) %>% 
  select(hospital_name, beds_occ, beds_total)

# calculate total and add to df
df %>% summarise(sum(beds_total, na.rm=TRUE), sum(beds_occ, na.rm=TRUE)) -> total
df <- df %>% add_row(hospital_name = "Total Montréal", beds_occ = total[1,2], beds_total = total[1,1] ) %>%
  mutate(occupancy_rate = round(100*(beds_occ/beds_total)), Date = update) %>%
  select(Date, hospital_name, occupancy_rate)

# get current max value (for y-axis in weekly plot)
max_today <- max(df$occupancy_rate, na.rm=T)

# some name changes
new_name <- "Hôpital général Juif Sir Mortimer B. Davis"
names(data)[names(data) == "L'Hôpital général Juif Sir Mortimer B. Davis"] <- new_name
df$hospital_name <- str_replace(df$hospital_name, "L'Hôpital général Juif Sir Mortimer B. Davis", new_name)
plot_predictions$name <- str_replace(plot_predictions$name, "L'Hôpital général Juif Sir Mortimer B. Davis", new_name)
plot_predictions$name <- str_replace(plot_predictions$name, "Total", "Total Montréal")


# sort data
# df <- filter(df, hospital_name != "Total Montréal")
df <- df[order(-df$occupancy_rate, df$hospital_name),]
hospitals <- df$hospital_name

# merge occupancy_rate to df_map for map plotting
df_map <- df_map %>% 
  left_join(df, by = c("hospital_name")) %>%
  select(-Date)

# colors for circles on map
pal_red <- colorNumeric(palette = "YlOrRd", domain = df_map$occupancy_rate)


ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5, bootswatch = "spacelab"),
  
  tags$head(HTML("<title>Occupancy rates in Montréal emergency rooms</title>")),
  
  div(class="container-sm px-0",
      
      h1("Occupancy rates in Montréal emergency rooms", class="text-center pt-2"),
      
      # card current occupancy 
      div(class="row",
          div(class="col-sm-6 py-2",
              div(class="card h-100",
                  div(class="card-header bg-primary", h5("Current Occupancy Rates in Montréal", class="card-title")),
                  # div(class="card-body px-0", plotOutput("plot_today")),
                  div(class="card-body px-0", 
                      tabsetPanel(id = "tabs", type = "tabs",
                                  tabPanel(value = "tab1", "view chart", plotOutput("plot_today")),
                                  tabPanel(value = "tab2","view map", leafletOutput("map"))
                                  ) 
                      ),
                  div(class="card-footer", h5("The occupancy rate is defined by the total number of patients on stretchers divided by the number of available stretchers.
                                               Wait times may vary depending on the number of patients and the nature of your illness or injury.",
                                              class="small"))
              ),    
          ),

      
      # card with tabs

          div(class="col-sm-6 py-2",
              div(class="card h-100",
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
                                  tabPanel("today", plotOutput("plot_weekdays")),
                                  tabPanel("past 90 days", plotOutput("plot")),
                                  tabPanel("next week", plotOutput("plot_prediction"))
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
      geom_text(aes(label = if_else(occupancy_rate >= 0 & occupancy_rate <= 49, paste0(occupancy_rate,"%"), NULL)), colour = "#595959", size = 3, hjust = -0.1, position = position_stack(vjust = 0), na.rm=T) +
      geom_text(aes(label = if_else(occupancy_rate > 49, paste0(occupancy_rate,"%"), NULL)), colour = "white", size = 3, hjust = -0.1, position = position_stack(vjust = 0), na.rm=T) +
      coord_flip() +
      scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = c(0,max(df$occupancy_rate))) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, caption = paste(update_txt)) +
      theme(panel.grid = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())
  }, res = 96, height = "auto")
  
  
  # select hospital and get data for selected hospital
  selected <- reactive(data %>% select(Date, occupancy = input$hospital))
  
  # get current occupancy value for selected hospital
  occupancy_current <- reactive(filter(df, str_detect(hospital_name, input$hospital))[1,3])
  # OBS! don't forget parenthesis => occupancy_current()
  
  # plot_weekdays: means for each day
  # OBS! uses date from msss file because of timezone
  output$plot_weekdays <- renderPlot({
    # layer for current selected occupancy, if no data available print only hidden text
    if (is.na(occupancy_current())) {
      p <- annotate("text", x=weekday_current, y=10, label = "", colour = "white", size = 0) 
      subtitle_txt <- "(Currently no data available)"
    } else {
      p <- geom_col(aes(x=weekday_current, y=occupancy_current(), fill = occupancy_current()), position = "identity", show.legend = F)
      subtitle_txt <- paste0("Current occupancy rate: ", occupancy_current(), "%")
    }
    # get data and plot    
    selected() %>%
      filter(Date >= (Sys.Date()-31)) %>%
      mutate(day_number = as.POSIXlt(Date)$wday+1) %>% # Sun = 1, Mon = 2 etc
      group_by(day_number) %>% 
      summarise(occupancy_mean = round(median(occupancy, na.rm=T))) %>%
        ggplot(aes(x = lubridate::wday(day_number, label = T), y = occupancy_mean, fill = occupancy_mean)) +
        geom_col(position = "identity", show.legend=F, alpha = 0.15, na.rm=T) +
        scale_y_continuous(limits = c(0,300), expand = c(0,0)) + # OBS!!! max_today
        labs(title = input$hospital, subtitle = subtitle_txt, y = NULL, x = NULL, caption = NULL) +
        geom_hline(yintercept=100, linetype="dashed", color = "red") +
        theme_minimal() +
        scale_fill_gradient(low = "brown2", high = "brown2") + # colors for week-cols and current cols
        theme(panel.grid = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
              plot.subtitle=element_text(size=12, color="#666666")) + 
        p # layer for selected occupancy
  }, res = 96)
  
  # plot: past 90 days
  output$plot <- renderPlot({
    selected() %>%
      # filter(Date >= (Sys.Date()-90)) %>%
      ggplot(aes(Date, occupancy, fill = occupancy)) +
      geom_line(size = 0.5, show.legend = F, na.rm = T) +
      scale_x_date(expand = c(0,0), date_labels = "%a, %b %d", date_breaks = "1 week", minor_breaks = "1 day") +
      scale_y_continuous(expand = c(0,0), limits = c(0,max_value), labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      labs(title = input$hospital, y = NULL, x = NULL, caption = "") +
      geom_hline(yintercept = 100, linetype="dashed", col = "red") +
      theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5))
  }, res = 96)
  
  # plot prediction for next week
  # OBS! uses date from msss file because of timezone
  output$plot_prediction <- renderPlot({
    plot_predictions %>% filter(name == input$hospital) %>%
      filter(Date >= update+1 & Date <= update+7) %>%  
      ggplot(aes(x = as.Date(Date), y = yhat, alpha = 0.8)) + 
      geom_line(col="darkblue", size = 1) + 
      geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, fill = "band"), alpha = 0.1) + 
      scale_fill_manual(values = c("blue")) +
      scale_x_date(date_labels = "%a\n%b %d", breaks = "1 day", minor_breaks = "1 day") +
      scale_y_continuous(limits = c(1,299), expand = c(0,0), labels = scales::percent_format(scale = 1)) +
      geom_hline(yintercept = 100, linetype="dashed", col = "darkblue") +
      theme_minimal() + 
      labs(title = input$hospital, y = NULL, x = NULL, caption = "") +
      theme(legend.position="none", axis.ticks.y = element_blank())
  }, res = 96)
  
  
  # render leaflet when switching to second tab
  # inside observeEvent because map disappeared on mobile
  observeEvent(input$tabs,{
    if(input$tabs == "tab2")
      output$map <- renderLeaflet({
        leaflet(df_map) %>% addProviderTiles(providers$CartoDB.Voyager) %>% 
          addCircleMarkers(lng = ~Long, lat = ~Lat, 
                           label = ~paste0(hospital_name, ": ", occupancy_rate, " %"), 
                           stroke = T, color = "black", weight = 0.5, # borders around circles
                           fillColor = ~pal_red(occupancy_rate), 
                           fillOpacity = 0.8
          ) %>%
          setView(lng = -73.62440, lat = 45.50275, zoom = 11)
      } # close renderLeaflet
      ) # close map
  }) # close Event
  
}

shinyApp(ui, server)

