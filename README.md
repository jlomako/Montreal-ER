# Occupancy app
Shiny app that tracks occupancy rates in Montréal emergency rooms. 
### <a href = "https://jlomako.shinyapps.io/occupancy_app/">https://jlomako.shinyapps.io/occupancy_app/</a>


<br><br>
<img src="Screenshot_20220927-155300.png" alt="screenshot" width=40%>


### note to myself
* set wd to script directory to deploy app in R
* reactive expression must be within a reactive or render* function <code>selected <- reactive(data %>% select(Date, occupancy = input$hospital))</code>, don't forget parentheses when calling that variable <code>selected()</code>
* shiny uses backup data from pdfscraper, since there are no updates since Sep 14 -- note: back to usual on Sep 20
