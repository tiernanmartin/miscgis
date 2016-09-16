insertACStoSEATemplate <- function(){
        template <- paste0("
                       tr1 <- tract_sea

                        tr_wa <- geo.make(state = 'WA',county = 'King', tract = '*')

                        abbr1 <- acs.fetch(endyear = 2014,geography = tr_wa,table.number = 'B02001',col.names = 'pretty')

                        abbr2 <- abbr1[abbr1@geography$tract %in% tr1$TRACTCE,] %>%
                        .@estimate %>%
                        cbind(abbr1[abbr1@geography$tract %in% tr1$TRACTCE,]@geography$tract) %>%
                        as.data.frame() %>%
                        select(TRACTCE = V11, TOTAL = contains('Total'),WHITE = contains('White alone')) %>%
                        mutate_each(funs(as.numeric),-contains('TRACTCE')) %>%
                        mutate(COUNT = TOTAL - XXX) %>%
                        mutate(PCT = COUNT/TOTAL) %>%
                        mutate(PCT = round_any(PCT*100,.01,round)) %>%
                        select(TRACTCE,TOTAL,COUNT,PCT)

                        tr2 <- myGeoJoin(tr1,abbr2,'TRACTCE','TRACTCE')

")
        rstudioapi::insertText(template)

}

