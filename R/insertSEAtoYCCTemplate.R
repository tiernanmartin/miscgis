insertSEAtoYCCTemplate <- function(){
        template <- paste0("
                        # Subset the Seattle data to include only YCC tracts
                        tr1 <- INDICATOR_sea %>% subset(TRACTCE %in% tract_ycc_arb@data$TRACTCE)

                           
                           # Join the UV names
                           UVs <- tract_ycc_arb@data %>% select(TRACTCE,UV)
                           
                           tr2 <- tr1 
                           
                           tr2@data %<>%
                           left_join(UVs) 
                           
                           # Group by UV (and calculate the percentages, if applicable)
                                uv1 <- tr2@data %>% 
                                as.data.frame() %>% 
                                group_by(UV) %>% 
                                summarise(PCT_UNDER200PCT = sum(UNDER200PCT)/sum(TOTAL)) 
                           
                           # Join the summary CB/CB50 values to the grouped ACS/UV polygons
                           
                           uv9 <- myGeoJoin(spatial_data = uv_ycc_arb,data_frame = uv4,by_sp = 'UV',by_df = 'UV') 
                           
                           # Convert the decimals into 1^e2 (better for mapping)
                           
                           uv9@data %<>% 
                           mutate(CB_PCT = round_any(IND * 100,.01, round))

")
        rstudioapi::insertText(template)

}

