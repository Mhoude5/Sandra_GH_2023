# Assignment 4 Database building for Reproducible Data Science
# In this script I will create the SQL database structure
# Created 03.18.2023
# Last updated 03.23.2023

library(DBI)

# Create DB ----
Sandra_db <- dbConnect(RSQLite::SQLite(), "~/Experiments_R/Sandra_GH_2023/Sandra_db.db")

# Create Tables and enforce relationship between them ----

## Sample_Location ----
dbExecute(Sandra_db, "CREATE TABLE Sample_Location (
Species_Tmt_Pos varchar(15) NOT NULL,
Species char(4) NOT NULL,
Column integer(1) NOT NULL,
Row integer(1) NOT NULL,
Salinity varchar(5) NOT NULL,
PRIMARY KEY(Species_Tmt_Pos)
);")

dbGetQuery(Sandra_db, "SELECT * FROM Sample_Location LIMIT 10;")

## Germination
dbExecute(Sandra_db, "CREATE TABLE Germination (
auto_id integer PRIMARY KEY AUTOINCREMENT,
Species char(4) NOT NULL,
Column integer(1) NOT NULL,
Row integer(1) NOT NULL,
Salinity varchar(5) NOT NULL,
Date varchar(10) NOT NULL,
Emergence integer NOT NULL,
Observer char(2) NOT NULL,
Notes varchar(150) NOT NULL,
Checked_by char(2) NOT NULL,
Species_Tmt_Pos varchar(15) NOT NULL,
FOREIGN KEY (Species_Tmt_Pos) REFERENCES Sample_Location(Species_Tmt_Pos)
);")

dbGetQuery(Sandra_db, "SELECT * FROM Germination;")

## EC_Final
dbExecute(Sandra_db, "CREATE TABLE EC_Final(
Sample varchar(8) NOT NULL,
Start_Time varchar(12) NOT NULL,
Soil_Weight_g double(10) NOT NULL,
Water_Rate_mL double(10) NOT NULL,
Time_of_Measurement varchar(12) NOT NULL,
EC_microS_cm integer(6) NOT NULL,
PSU double(4) NOT NULL,
Temperature_C double(4) NOT NULL,
ECe_microS_cm double(15) NOT NULL,
ECe_dS_m double(4) NOT NULL,
PSU_e double(4) NOT NULL,
Target_Salinity varchar(5) NOT NULL,
remeasure varchar(75),
Species_Tmt_Pos varchar(15) NOT NULL,
FOREIGN KEY (Species_Tmt_Pos) REFERENCES Sample_Location(Species_Tmt_Pos),
PRIMARY KEY (Sample)
);")

dbGetQuery(Sandra_db, "SELECT * FROM EC_Final;")

# Upload Data into tables ----

## Upload Sample_Location ----
getwd()
Sample_Location <- read.csv("./Raw_Data/Sample_Location.csv") 

dbWriteTable(Sandra_db, "Sample_Location", Sample_Location, append = TRUE)
dbGetQuery(Sandra_db, "SELECT * FROM Sample_Location LIMIT 10;")

## Upload Germination ----
Germination <- read.csv("./Raw_Data/Germination.csv") 
Germination$auto_id <- 1:nrow(Germination)
head(Germination)

# Reorder columns so it matches our database structure
Germination <- Germination[, c(11, 1:10)]

dbWriteTable(Sandra_db, "Germination", Germination, append = TRUE)
dbGetQuery(Sandra_db, "SELECT * FROM Germination LIMIT 10;")


## Upload EC_Final ----
EC_Final <- read.csv("./Raw_Data/EC_Final.csv") 
head(EC_Final, 2)

# Reorder columns so it matches our database structure
EC_Final <- EC_Final[, c(1, 3:14, 2)]

# This .csv isn't completed, so we can remove the null rows for now
EC_Final <- na.omit(EC_Final)

dbWriteTable(Sandra_db, "EC_Final", EC_Final, append = TRUE)
dbGetQuery(Sandra_db, "SELECT * FROM EC_Final LIMIT 10;")


# Since we will create these tables in Bookdown, I am going to delete them 
# from here
dbExecute(Sandra_db, "DROP TABLE Germination;") #drop table
dbExecute(Sandra_db, "DROP TABLE EC_Final;")
dbExecute(Sandra_db, "DROP TABLE Sample_Location;")
dbGetQuery(Sandra_db, "SELECT * FROM EC_Final LIMIT 10;")

