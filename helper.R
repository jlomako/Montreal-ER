#####################################
# helper script for montreal ER app
# loads data
#####################################

# get coordinates for map
df_map <- vroom::vroom("https://github.com/jlomako/quebec-emergency-rooms/raw/main/data/coordinates.csv", show_col_types = FALSE)

# get hospital data from repository
df_longterm <- vroom::vroom("https://github.com/jlomako/hospital-occupancy-tracker/raw/main/data/hospitals.csv", show_col_types = FALSE)

# get hourly data from msss / or copy from repo
url <- "https://www.msss.gouv.qc.ca/professionnels/statistiques/documents/urgences/Releve_horaire_urgences_7jours.csv"
# url <- "https://github.com/jlomako/download-file-to-repository/raw/main/data/urgence.csv"
df <- read.csv(url, encoding = "latin1") # using read.csv here because vroom can't handle french characters

# get predictions from repository
df_predictions <- vroom::vroom("https://github.com/jlomako/prophet-bot/raw/main/data/prophet.csv", show_col_types = FALSE)
df_predictions$yhat_lower[df_predictions$yhat_lower < 0] <- 0
