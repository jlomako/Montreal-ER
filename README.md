## Track current occupancy rates in Montréal emergency rooms in Shiny app: 
### <a href="https://jlomako.shinyapps.io/occupancy_app/">Launch app</a>
See current occupancy rates for emergency rooms in Montréal. Hospitals are ranked from highest to lowest occupancy.
Click on the map to see which ERs are near you.
Select a hospital to see average occupancy rates over the week or over the past 90 days.
A new feature predicts the occupancy rate for the next 7 days.
App is interactive and mobile-friendly.



## preview:
<img src="Screenshot_20220927-155300.png" alt="screenshot" width=40%>
<br>
https://jlomako.shinyapps.io/occupancy_app/

### note to myself
* set wd to script directory to deploy app in R
* reactive expression must be within a reactive or render* function <code>selected <- reactive(data %>% select(Date, occupancy = input$hospital))</code>, don't forget parentheses when calling that variable <code>selected()</code>
* shiny uses backup data from pdfscraper, since there are no updates since Sep 14 -- note: back to usual on Sep 20
* predictions are done by prophet-bot
