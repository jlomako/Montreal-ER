# helper script for occupation app

# coordinates for map
df_map <- read.csv(textConnection(
  "hospital_name,Lat,Long
Hôpital général Juif Sir Mortimer B. Davis,45.4974528,-73.6311184
Hôpital de Montréal pour enfants,45.473545,-73.600998
Hôpital Royal Victoria,45.472946, -73.601992
Hôpital général de Montréal,45.4969179,-73.588787
Hôpital Maisonneuve-Rosemont,45.573915, -73.558440
Centre hospitalier de l'Université de Montréal,45.511430, -73.557637
Hôpital général du Lakeshore,45.449043, -73.833057
CHU Sainte-Justine,45.5027504,-73.6243993
Institut universitaire en santé mentale Douglas,45.4430317,-73.5849744
Hôpital de LaSalle,45.4200411,-73.6233353
Hôpital Jean-Talon,45.5459821,-73.609411
Centre hospitalier de St. Mary,45.4947796,-73.6239577
Hôpital du Sacré-Coeur de Montréal,45.5338504,-73.7137721
Hôpital de Verdun,45.4638829,-73.563649
Campus Lachine,45.441135,-73.6767911
Hôpital Santa Cabrini,45.5804396,-73.5713617
Hôpital Notre-Dame,45.5255337,-73.5627179
Hôpital Fleury,45.5718874,-73.6499032
Institut de Cardiologie de Montréal,45.5749028,-73.578271
Pavillon Albert-Prévost,45.5285988,-73.7293315
Institut universitaire en santé mentale de Montréal,45.5886861,-73.530235"
))

# get hospital data from repository
data <- vroom::vroom("https://github.com/jlomako/hospital-occupancy-tracker/raw/main/data/hospitals.csv", show_col_types = FALSE)

# get hourly data from msss:
url <- "https://www.msss.gouv.qc.ca/professionnels/statistiques/documents/urgences/Releve_horaire_urgences_7jours.csv"
df <- read.csv(url, encoding = "latin1") # using read.csv here because vroom can't handle french characters

# get predictions from repository
plot_predictions <- vroom::vroom("https://github.com/jlomako/prophet-bot/raw/main/data/prophet.csv", show_col_types = FALSE)
plot_predictions$yhat_lower[plot_predictions$yhat_lower < 0] <- 0
