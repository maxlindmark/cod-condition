# Return sub_area based on coordinates
get_sub_area <- function(dat, lon, lat){

  # Get ICES rect
  dat$ices_rect <- mapplots::ices.rect2(lon = lon, lat = lat)

  # Color according to sub-division
  dat <- dat %>%
    mutate(SubDiv = NA) %>%
    mutate(SubDiv = ifelse(ices_rect %in% c("37G0", "37G1",
                                            "38G0", "38G1",
                                            "39F9", "39G0", "39G1",
                                            "40F9", "40G0", "40G1"), "22", SubDiv)) %>%
    mutate(SubDiv = ifelse(ices_rect == "40G2", "23", SubDiv)) %>%
    mutate(SubDiv = ifelse(ices_rect %in% c("37G2", "37G3", "37G4",
                                            "38G1", "38G2", "38G3", "38G4",
                                            "39G1", "39G2", "39G3", "39G4",
                                            "40G1"), "24", SubDiv)) %>%
    mutate(SubDiv = ifelse(ices_rect %in% c("40G4",
                                            "37G5", "37G6", "37G7",
                                            "38G5", "38G6", "38G7",
                                            "39G5", "39G6", "39G7",
                                            "40G5", "40G6", "40G7",
                                            "41G5", "41G6", "41G7"), "25", SubDiv)) %>%
    mutate(SubDiv = ifelse(ices_rect %in% c("37G8", "37G9", "37H0",
                                            "38G8", "38G9", "38H0",
                                            "39G8", "39G9", "39H0",
                                            "40G8", "40G9", "40H0",
                                            "41G8", "41G9", "41H0"), "26", SubDiv)) %>%
    mutate(SubDiv = ifelse(ices_rect %in% c("42G6", "42G7",
                                            "43G6", "43G7",
                                            "44G6", "44G7", "44G8"), "27", SubDiv)) %>%
    mutate(SubDiv = ifelse(ices_rect %in% c("42G8", "42G9", "42H0", "42H1", "42H2",
                                            "43G8", "43G9", "43H0", "43H1", "43H2",
                                            "44G8", "44G9", "44H0", "44H1", "44H2"), "28", SubDiv)) %>%
    mutate(SubDiv = ifelse(lat > 58.5 & lon > 19, "29", SubDiv)) %>%
    mutate(SubDiv = ifelse(lat > 57 & lat < 58.5 & lon > 19 & lon < 22, "28", SubDiv)) %>%
    mutate(SubDiv = ifelse(lat > 57 & lat < 60 & lon > 16 & lon < 18, "27", SubDiv)) %>%
    mutate(SubDiv = ifelse(lat > 55.5 & lat < 56.5 & lon > 14 & lon < 16, "25", SubDiv)) %>%
    mutate(SubDiv = factor(SubDiv))
  
}

