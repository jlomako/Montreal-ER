## Track current occupancy rates in Montréal emergency rooms in Shiny app: 
### <a href="https://jlomako.shinyapps.io/occupancy_app/">Launch app</a>
See current occupancy rates for emergency rooms in Montréal. Hospitals are ranked from highest to lowest occupancy.
Click on the map to see which ERs are near you.
Select a hospital to see average occupancy rates over the week or over the past 90 days.
A new feature predicts the occupancy for the next 7 days.
App is interactive and mobile-friendly.



## preview:
<img src="Screenshot_20221003-112537.png" alt="screenshot" width=40%>
<br>
https://jlomako.shinyapps.io/occupancy_app/

### note to myself
* set wd to script directory to deploy app in R
* reactive expression must be within a reactive or render* function <code>selected <- reactive(data %>% select(Date, occupancy = input$hospital))</code>, don't forget parentheses when calling that variable <code>selected()</code>
* long-term data is collected by <a href="https://github.com/jlomako/hospital-occupancy-tracker">hospital-occupancy-tracker</a> and <a href="https://github.com/jlomako/pdfscraper">pdfscraper</a> (backup)
* predictions are done by <a href="https://github.com/jlomako/prophet-bot">prophet-bot</a>

