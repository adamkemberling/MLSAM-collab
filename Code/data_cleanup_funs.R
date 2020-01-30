#####  Data Cleanup Utility Functions for MLSAM Pipeline  ####


#' Species Selection
#'
#' @description Pull catch information for a target species by its common name.
#'
#' @param survey_data Survey dataset
#' @param common_name Common name for the target species
#'
#' @return species_out list containing two dataframes. "cod" contains all stations. "codtow" only contains stations where the target species was caught.
#' @export
#'
#' @examples
species_selection <- function(survey_data = dat, common_name = "atlantic cod") {
  
  # Catch capitalization differences
  common_name <- toupper(common_name)
  
  # Which rows have information on target species
  cod <- dat %>% filter(COMNAME == common_name)
  
  
  # remove rows if NA for lengths or abundance
  cod <- cod %>% filter(!is.na(NUMLEN))
  cod <- cod %>% filter(!is.na(ABUNDANCE))
  cod <- cod %>% filter(!is.na(LENGTH))
  
  # Keep these stations separate
  # one row for every unique ID
  codtow <- cod %>% distinct(ID,.keep_all=TRUE)
  
  # Create one row for each NUMLEN
  cod <- uncount(cod,NUMLEN)
  
  # Export the two in a list
  species_out <- list(cod = cod,
                      codtow = codtow)
  
  return(species_out)
}







#' Generate Time-Lagged Dataframe
#' 
#' Generates a dataframe for specified number of lag years. First column is the year, followed by nyears * 12 columns. 
#' Naming convention goes region_season_metric_size.
#'
#' @param GB Toggle for Georges Bank Region T/F
#' @param GOM Toggle for Gulf of Maine Region T/F
#' @param nyears Number of years to lag
#' @param p Dataframe containing annual area/season/size class mean abundance and biomass across strata
#'
#' @return survdf Survey year dataframe with column names for year lags
#' @export
#'
#' @examples
lag_years <- function(p, nlags = 3, GB = TRUE, GOM = TRUE) {
  
  
  ####  Set up dimensions  ####
  # Number of Rows equal to length of unique years
  num_years <- length(unique(p$EST_YEAR))
  
  # Number of columns = 1(year) + 12 * (n regions)
 
  #Empty Dataframe of desired dimensions
  survdf <- data.frame(matrix(NA, nrow = num_years, ncol = 25))
  
  colnames(survdf) <- c(
    "year",
    "gom_spr_abun_sml", "gom_spr_abun_med", "gom_spr_abun_lrg", #GOM Spring Abundance
    "gom_spr_bio_sml",  "gom_spr_bio_med",  "gom_spr_bio_lrg",  #GOM Spring Biomass
    "gom_fal_abun_sml", "gom_fal_abun_med", "gom_fal_abun_lrg", #GOM Fall Abundance
    "gom_fal_bio_sml",  "gom_fal_bio_med",  "gom_fal_bio_lrg",  #GOM Fall Biomass
    "gb_spr_abun_sml",  "gb_spr_abun_med",  "gb_spr_abun_lrg",  #GB Spring Abundance
    "gb_spr_bio_sml",   "gb_spr_bio_med",   "gb_spr_bio_lrg",   #GB Spring Biomass
    "gb_fal_abun_sml",  "gb_fal_abun_med",  "gb_fal_abun_lrg",  #GB Fall Abundance
    "gb_fal_bio_sml",   "gb_fal_bio_med",   "gb_fal_bio_lrg")   #GB Fall Biomass
  
  # add unique years
  survdf[,1] <- sort(unique(p$EST_YEAR))
  
  # order of rows from p are 4:1 when subset by year
  pdf <- as.data.frame(p)
  
  #Loop through
  for(i in 1:num_years){
    survdf[i, 2:25] <- as.numeric(unlist(c(subset(pdf, pdf$EST_YEAR == survdf$year[i])[4,4:9],
                                           subset(pdf, pdf$EST_YEAR == survdf$year[i])[3,4:9],
                                           subset(pdf, pdf$EST_YEAR == survdf$year[i])[2,4:9],
                                           subset(pdf, pdf$EST_YEAR == survdf$year[i])[1,4:9])))
  }
  
  
  
  ####  Setting Up Lag Years  ####
  
  #Indexing values for vectorizing
  first_lag <- num_years - 1
  final_lag <- num_years - nlags
  lag_vec   <- first_lag:final_lag
  
  #Create dataframes for n = nlags lag years
  lagged_steps <- imap(lag_vec, function(x,y){
      
      #The index of the vector indicates how many rows to skip
      n_skips <- as.numeric(y)
      
      #Then need to populate those rows with NaN values
      row_skips <- as.data.frame(matrix(data = NaN, nrow = n_skips, ncol = 24))
      colnames(row_skips) <- names(survdf)[2:25]
      
      lagged_step <- bind_rows(row_skips, survdf[1:x, -1])
      colnames(lagged_step) <- str_c(colnames(lagged_step), "_i", y)
      
      return(lagged_step)
    })

  
  # bind columns together
  survdf <- bind_cols(survdf, lagged_steps)
  
  # reorder columns all abundance together all biomass together
  survdf <- survdf[,c(str_which(colnames(survdf),pattern="bio",negate=TRUE),
                      str_which(colnames(survdf),pattern="bio",negate=FALSE))]
  
  #Return single dataframe containing rows with lagged years
  return(survdf)
}
