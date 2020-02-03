#### Mapping Model Outputs Over Spatial Stratum  ####
library(tidyverse)
library(sf)
library(gmRi)

####  Load Model Prediction Data  ####
#mod_preds  <- read_csv(here("predictions20132015.csv"), col_types = cols(), guess_max = 1e6)

####  Load Strata Shapefiles  ####
survey_strata <- read_sf(str_c(res_path, "Shapefiles/BottomTrawlStrata/BTS_Strata.shp"))


####  Reshape Data for Plotting  ####
strata_summs <- function(mod_preds) {
  mod_summs <- mod_preds %>% 
    mutate(
      #STRATUMA = str_c("0", stratum), 
      STRATA = as.integer(stratum),
      stratum = NULL) %>% 
    group_by(STRATA) %>% 
    summarise(
      obs_small = mean(observed_small, na.rm = T),
      obs_medium = mean(observed_medium, na.rm = T),
      obs_large = mean(observed_large, na.rm = T),
      pred_small = mean(predicted_small, na.rm = T),
      pred_medium = mean(predicted_medium, na.rm = T),
      pred_large = mean(predicted_large, na.rm = T),
      diff_small = obs_small - pred_small,
      diff_medium = obs_medium - pred_medium,
      diff_large = obs_large - pred_large
      ) %>% 
    ungroup() %>% 
    pivot_longer(names_to = "source", values_to = "abundance", obs_small:diff_large) %>% 
    mutate(
      size = ifelse(str_detect(source, "small"), "small", ifelse(str_detect(source, "medium"), "medium", "large")),
      type = ifelse(str_detect(source, "obs"), "observed", ifelse(str_detect(source, "diff"), "obs - pred", "predicted")),
      size = factor(size, levels = c("small", "medium", "large")),
      type = factor(type, levels = c("observed", "predicted", "obs - pred"))
      )
    
  
  ####  Join with Strata Geometries  ####
  mod_summs_sf <- survey_strata %>% inner_join(mod_summs, by = "STRATA")
  
  return(mod_summs_sf)
  
}  

# #Function in practice
# mod_summs_sf <- strata_summs(mod_preds)

####  Observed and Predicted Plots  ####
obs_pred_plot <- function(mod_summs_sf, size_facets = TRUE) {
  #Leave out the differences, plot
  data_facets <-  mod_summs_sf %>% filter(type != "obs - pred")
  
  if(size_facets == TRUE) {
    plot_out <- ggplot(data_facets) +
        geom_sf(aes(fill = abundance), color = "gray50", size = 0.05) +
        scale_fill_distiller(palette = "RdBu") +
        facet_wrap(type ~ size) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10)) +
        theme(legend.position = "bottom")
  } else {
    
    plot_list <- data_facets %>% 
      split(.$size) %>%
      map(function(x) {
        plot_out <- ggplot(x) +
             geom_sf(aes(fill = abundance), color = "gray50", size = 0.05) +
             scale_fill_distiller(palette = "RdBu") +
             facet_wrap(type ~ size) +
             guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10)) +
             theme(legend.position = "bottom")
        
      })
    
    return(plot_list)
  }
  
  
}


####  Prediction Residual Plot  ####

strata_diffs_plot <- function(mod_summs_sf, size_facets = TRUE) {
  
  mod_diffs <- mod_summs_sf %>% 
    filter(type == "obs - pred") %>% 
    mutate("prediction difference" = abundance)
  
  
  if(size_facets == TRUE) {
    # Plot with centered color scale
    limit <- ceiling(max(abs(mod_diffs$`prediction difference`), na.rm = T)) * c(-1, 1)
    
    plot_out <- mod_diffs %>% 
      ggplot() +
        geom_sf(aes(fill= `prediction difference`), color = NA) +
        scale_fill_distiller(palette = "RdBu", limit = limit) +
        facet_grid(type ~ size) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10)) +
        theme(legend.position = "bottom")
    
    return(plot_out)
    
  } else {
    
    plot_list <- mod_diffs %>% 
      split(.$size) %>% 
      map(function(x) {
        
        limit <- ceiling(max(abs(x$`prediction difference`), na.rm = T)) * c(-1, 1)
        
        plot_out <- x %>% 
          ggplot() +
          geom_sf(aes(fill= `prediction difference`), color = NA) +
          scale_fill_distiller(palette = "RdBu", limit = limit) +
          facet_grid(type ~ size) +
          guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10)) +
          theme(legend.position = "bottom")
        
        return(plot_out)
        
      })
    
    return(plot_list)
    
  }
  
}





