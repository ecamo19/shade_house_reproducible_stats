# Load packages ----------------------------------------------------------------
library(dplyr)
library(janitor)

# Load Biomass data ------------------------------------------------------------
data_soil_moisture <- 
	read.csv("raw_data/soil_moisture_data_complete.csv", header = T, 
	         na.strings = "-") %>% clean_names()

# Recode factors ---------------------------------------------------------------

# unique(data_soil_moisture$Treatment)
data_soil_moisture <-
    data_soil_moisture %>% 
    
        # Recode treatment levels 
    	mutate(treatment = recode(treatment,
    		   `ambient rain` =	                "ambientrain",			   
    		   `ambient rain + nutrients`=        "ambientrain_nutrients",
    		   `ambient rain + water` =           "ambientrain_water",
    		   `ambient rain + water + nutrients` = "ambientrain_water_nutrients",
    		   `Harvest at the begging` = "harvested_at_beginning "),
    		   
    		   # Recode wrong specie code
    		   sppcode = recode(sppcode,`dt`= "dr")) 

# Clean data -------------------------------------------------------------------

data_soil_moisture_cleaned <- 
	data_soil_moisture %>% 
            
            # remove NA's    
        	drop_na() %>%  
        	
        	# Rename columns
        	rename(spcode = sppcode,
        		   sm_before_watering = soil_moisture_meausrements_water_content_before_watering,
        		   sm_after_watering  = soil_moisture_meausrements_water_content_after_watering) %>% 
        	 
        	# Transform to factor class spcode,family and treatment
        	mutate(spcode = factor(spcode),
        	        
        	        # Order levels
        	        date_day_month = factor(date_day_month, levels = c("31_08",
        			                                                     "19_09",
        			                                                     "4_10",
        			                                                     "17_10",
        			                                                     "31_10",
        			                                                     "15_11")),
        			# Order levels
        		    treatment = factor(treatment,levels = c("ambientrain",
        		                                               "ambientrain_nutrients",
        		                                               "ambientrain_water",
        		                                               "ambientrain_water_nutrients")),
        			
        			# Create nfixer column
        			nfixer = factor(ifelse(spcode == "ec" |
        			                              spcode == "dr" |
        			                              spcode == "gs","fixer", "nonfixer"))) %>% 
        
        	
        	# Pivot soil moisture before and soil moisture after in one column
        	pivot_longer(c(sm_before_watering,sm_after_watering), 
        				 names_to = "sm_measured", values_to = "soil_moisture") %>% 
        	
        	# Recode new levels
        	mutate(sm_measured = recode(sm_measured,
        	                            `sm_before_watering` = "before_watering",
        	                            `sm_after_watering`  = "after_watering"),
        	       # Order levels
        	       sm_measured = factor(sm_measured,levels = c("before_watering",
        	                                                   "after_watering"))) %>% 
    
        	#Order the columns
        	dplyr::select(id,spcode,treatment,nfixer,date_day_month,sm_measured,everything()) %>% 
        	dplyr::select(- height_cm) %>% 
        	arrange(nfixer) 
        

# Order factors ----------------------------------------------------------------
str(data_soil_moisture_cleaned)

# Remove uncleaned data set ----------------------------------------------------
rm(data_soil_moisture)

