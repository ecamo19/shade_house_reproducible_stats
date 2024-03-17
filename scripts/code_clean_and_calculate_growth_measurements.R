# Objective -------------------------------------------------------------------
# The objective of this script is to create a data frame with different
#measuares of growth.
#
# Measuments
#
# + RGR and AGR with the function agr_rgr_calculators
# + Using harvested a the beginning as initial mean value


# Load RGR and AGR FUNCTION ---------------------------------------------------
source("./R/functions_agr_rgr_calculators.R")

# Load packages ---------------------------------------------------------------
library(dplyr)
library(janitor)
# For date
library(lubridate)
# For pivot longer
library(tidyr)
# For map
library(purrr)

# Load data -------------------------------------------------------------------
raw_data_heigths <- read.csv("./raw_data/6_plant_heights_data.csv", header = TRUE) %>%
    clean_names()


# Cleaning raw data -----------------------------------------------------------

# Remove x at the beginning of the colnames
colnames(raw_data_heigths) <- sub("*x", "", colnames(raw_data_heigths))

## Dealing with dates ---------------------------------------------------------
data_heights_clean <-
    raw_data_heigths %>%

        # Transform data into longer form
        pivot_longer(
            cols = starts_with("2015"),
            names_to = "date",
            #names_prefix = "wk",
            values_to = "height_cm") %>%

        # Convert character columns to factor
        mutate(across(where(is.character), as.factor),

               # Transform column to date type
               date = ymd((.$date)))  %>%

        # Create Nfixer category
        mutate(nfixer = factor(ifelse(spcode == "ec" |
                                          spcode == "dr" |
                                          spcode == "gs","fixer",

                                      # else
                                      "nonfixer"))) %>%

        # Calculate the time intervals along the whole experiment between each
        # measurement
        group_by(id) %>%
        mutate(number_of_days = -1 *(ymd("2015-08-31") - ymd(date))) %>%

        # Remove Harvestatthebegging from the data set
        filter(!treatment %in% "Harvestatthebegging") %>%

        # Convert date to integer
        mutate(number_of_days_int = as.integer(number_of_days)) %>%

        # Order the dataframe
        dplyr::select(id,spcode,treatment,nfixer, everything(),-family)


# Calculate RGR and AGR -------------------------------------------------------

data_rgr_agr <-
    data_heights_clean %>%
        group_by(id,spcode,treatment,nfixer) %>%

        # Calculate rgr and agr using sourced funcition
        mutate(rgr = rgr(height_cm, number_of_days_int),
               agr = agr(height_cm, number_of_days_int)) %>%

        # Get rgr and agr at the final of the experiment
        filter(number_of_days == 85)


# Calculate RGR as a slope ----------------------------------------------------

# Relative growth rate (RGR) of height was calculated as the slope of the
# natural log transformed height of each individual as a function of time


## Nest data ------------------------------------------------------------------

data_height_by_id <-
    data_heights_clean %>%
        group_by(id) %>%
        dplyr::select(id,height_cm,number_of_days_int) %>%
        nest()


## Create model ---------------------------------------------------------------

# This model only returns the slope of each id plant

rgr_model_coef <- function(data) {

    # Constant added for preventing log(0)
    coef(lm(log(height_cm) ~ log(number_of_days_int + 1), data = data))[[2]]
}

## map ------------------------------------------------------------------------

rgr_slope <-
    data_height_by_id %>%

        # map the model to get the slope of each plant (rgr)
        mutate(rgr_slope = map(data, rgr_model_coef)) %>%

        # Get slope
        unnest(rgr_slope) %>%

        # Remove col data
        dplyr::select(-data)

# Join data sets and create data for models -----------------------------------
data_rgr_cleaned <-

    data_rgr_agr %>%

    # Join data by plant's id
    inner_join(., rgr_slope, by = "id") %>%

    # Remove unused colunms
    dplyr::select(-c(date, height_cm, number_of_days, number_of_days_int))

# Remove all files except clean data set --------------------------------------

items <- c("agr", "data_height_by_id", "data_heights_clean", "data_rgr_agr",
           "raw_data_heigths", "rgr", "rgr_model_coef", "rgr_slope")

remove(items, list = items)
