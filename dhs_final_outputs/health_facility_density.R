facility_files <- c("/Users/matthewnicholson/Downloads/healthmopupandbaselinenmisfacility.geojson",
                             "/Users/matthewnicholson/Downloads/Ethiopia_-_Health_data.geojson",
                            "/Users/matthewnicholson/Downloads/osm_rd_congo_health.geojson",
                            '/Users/matthewnicholson/Downloads/hotosm_gha_health_facilities_points_geojson.geojson',
                            "/Users/matthewnicholson/Downloads/healthcare_facilities.geojson"
)

calculate_facility_density <- function(dhs_panel_sf, facility_sf) {
  
  require(sf)
  require(dplyr)
  
  message("Calculating facility density...")
  
  # 1. Transform to a Metric CRS for accurate buffering
  #    Using World Mercator (3395) or Pseudo-Mercator (3857) allows for meter calculations.
  #    If you need high local precision, use the specific UTM zone for the country.
  dhs_metric <- st_transform(dhs_panel_sf, 3857)
  fac_metric <- st_transform(facility_sf, 3857)
  
  # 2. Split data into Urban and Rural to apply different buffers
  dhs_urban <- dhs_metric %>% filter(urban_rural == "Urban")
  dhs_rural <- dhs_metric %>% filter(urban_rural == "Rural")
  
  # 3. Create Buffers (5000m = 5km, 10000m = 10km)
  #    Result is now Polygons, not Points
  urban_buffers <- st_buffer(dhs_urban, dist = 5000)
  rural_buffers <- st_buffer(dhs_rural, dist = 10000)
  
  # 4. Count facilities in buffers
  #    st_intersects returns a list of indices. lengths() counts them efficiently.
  urban_buffers$facility_count <- lengths(st_intersects(urban_buffers, fac_metric))
  rural_buffers$facility_count <- lengths(st_intersects(rural_buffers, fac_metric))
  
  # 5. Recombine and clean up
  #    We drop the geometry of the buffer to return to the original point geometry 
  #    if we want to join it back to the original panel, or we keep the buffer if we want to map it.
  #    Here I return a simple dataframe of ID + Count to join back to your main panel.
  
  urban_res <- urban_buffers %>% st_drop_geometry() %>% select(cluster, year, facility_count)
  rural_res <- rural_buffers %>% st_drop_geometry() %>% select(cluster, year, facility_count)
  
  density_results <- bind_rows(urban_res, rural_res)
  
  return(density_results)
}
for(i in seq_along(countries)) {
  
  # 1. Load the specific facility file for this country
  #    (Assuming one master facility list per country)
  fac_data <- st_read(facility_files[[i]]) 
  
  # 2. Load your combined DHS panel (from previous step)
  dhs_data <- country_panels[[i]] |> 
    st_as_sf()
  
  # 3. Run the calculation
  density_data <- calculate_facility_density(dhs_data, fac_data)
  
  # 4. Join the count back to your main DHS panel
  country_panels[[i]] <- dhs_data %>%
    left_join(density_data, by = c("cluster", "year")) %>%
    mutate(facility_count = ifelse(is.na(facility_count), 0, facility_count))
    
  # Now country_panels[[i]] has a 'facility_count' column based on the 5km/10km logic
}
