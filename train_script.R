#Necessary packages
packages <- c("tidyverse", "stringr", "geosphere","rgdal","rgeos","broom","ggpubr")

#Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {install.packages(packages[!installed_packages])}

#Load packages
invisible(lapply(packages, library, character.only = TRUE))

#train location data: https://groups.google.com/g/openraildata-talk/c/LOSy1oFi5CM?pli=1

#Read in the "flow_file"
flow <- read.delim("/Users/danielgray/Desktop/projects/trains/RJFAF214/RJFAF214.FFL") %>%
  
        rename(raw = 1) %>%
        mutate( update_marker = substr(raw,1,1),
                record_type = substr(raw,2,2))

routes <- flow %>% filter(record_type == "F")  %>% 
          mutate(
              origin_code = substr(raw,3,6),
              destination_code = substr(raw,7,10),
              route_code = substr(raw,11,15),
              status_code = substr(raw,16,18),
              usage_code = substr(raw,19,19),
              direction = substr(raw,20,20),
              end_date = substr(raw,21,28),
              start_date = substr(raw,29,36),
              toc = substr(raw,37,39),
              cross_london_ind = substr(raw,40,40),
              ns_disc_ind = substr(raw,41,41),
              publication_ind = substr(raw,42,42),
              flow_id = substr(raw,43,49)
              ) %>%
  
  select(-c(raw)) %>%
  
  filter(flow_id != "") #this removes the title fields

#Now read in the fare data.
fare <- flow %>% filter(record_type == "T")  %>% 
        mutate(
          flow_id = substr(raw,3,9),
          ticket_code = substr(raw,10,12),
          fare = substr(raw,13,20),
          restriction_code = substr(raw,21,22)) %>%
  
  select(-c(raw)) %>%
  
  filter(flow_id != "") 

#Now read in ticket-type data
ticket <- read.delim("/Users/danielgray/Desktop/projects/trains/RJFAF214/RJFAF214.TTY") %>%
  rename(raw = 1) %>% mutate(
    
    update_marker = substr(raw,1,1),
    ticket_code = substr(raw,2,4),
    end_date = substr(raw,5,12),
    start_date = substr(raw,13,20),
    quote_date = substr(raw,21,28),
    description = substr(raw,29,43),
    tkt_class = substr(raw,44,44),
    tkt_type = substr(raw,45,45),
    tkt_group = substr(raw,46,46),
    last_valid_day = substr(raw,47,54),
    max_passengers = substr(raw,55,57),
    min_passenders = substr(raw,58,60),
    max_adults = substr(raw,61,63),
    min_adults = substr(raw,64,66),
    max_children = substr(raw,66,69),
    min_children = substr(raw,70,72),
    restricted_by_date = substr(raw,73,73),
    restricted_by_train = substr(raw,74,74),
    restricted_by_area = substr(raw,75,75),
    validity_code = substr(raw,76,77),
    atb_description = substr(raw,78,97),
    lul_xlondon_issue = substr(raw,98,98),
    reservation_required = substr(raw,99,99),
    capri_code = substr(raw,100,102),
    lul_93 = substr(raw,103,103),
    uts_code = substr(raw,104,105),
    time_restriction = substr(raw,106,106),
    free_pass_lul = substr(raw,107,107),
    package_mkr = substr(raw,108,108),
    fare_multiplier = substr(raw,109,111),
    discount_category = substr(raw,112,113)
  ) %>%
  
  filter(update_marker == "R")

#Now thin-out the dataframes.
routes_t <- routes %>% select(origin_code,destination_code,flow_id,toc,direction)
fare_t <- fare %>% select(flow_id,fare,ticket_code)
ticket_t <- ticket %>% select(ticket_code,tkt_type,atb_description,restricted_by_date,description) 

#Free-up some memory
rm(routes,ticket,fare,flow)
gc() 

#Combine dataframes 
prices <- left_join(routes_t,fare_t, by = c("flow_id")) %>%
          left_join(.,ticket_t, by = c("ticket_code"))

advanced <- read.delim("/Users/danielgray/Desktop/projects/trains/RJFAF214/RJFAF214.TAP") %>%
  rename(raw = 1) %>%
  mutate(ticket_code = substr(raw,1,3),
         restriction_code = substr(raw,4,5),
         restriction_flat = substr(raw,6,6))

#Filter down to singles which are singles and don't require reservation (i.e. made on the day...)
singles <- prices %>% filter(tkt_type == "S") %>%
        
           filter(restricted_by_date == "N") %>%
  
           filter(!ticket_code %in% advanced$ticket_code) %>%
  
           filter(grepl("ANYTIME",description, ignore.case = T) | 
                 # grepl("OFFPEAK",description, ignore.case = T) | 
                 # grepl("OFF-PEAK",description,ignore.case = T) | 
                  grepl("PAYG",description,ignore.case = T)) %>%

           mutate(fare = as.numeric(fare)) %>%
  
           group_by(origin_code,destination_code,direction) %>%
      
           summarise(cheapest = min(fare)) #NB - this means we're taking cheapest ticket, including those with restrictions (e.g. re time you can travel, train you can take, carriage you can sit in)

#For some journeys, the ticket is the same cost each-way and so is only reported once. Let's flip these around.

reverse <- singles %>% filter(direction == "R") %>%
  
           rename(temp_col = destination_code,
                  destination_code = origin_code) %>% 
  
          rename(origin_code = temp_col)

singles <- bind_rows(singles,reverse) %>% select(-c(direction))

#This is to work out which stations are in clusters.
stations <- read.delim("/Users/danielgray/Downloads/RJFAF214/RJFAF214.FSC") %>%
  rename(raw = 1) %>%
  mutate(
    update_marker = substr(raw,1,1),
    cluster_id = substr(raw,2,5),
    cluster_nlc = substr(raw,6,9),
    end_date = substr(raw,10,17),
    start_date = substr(raw,18,25)
  ) %>%

  select(-c(raw)) %>%
  filter(update_marker == "R") %>%
  select(cluster_id,cluster_nlc)

#Lets convert the clusters into stations. First, find all the unique codes
clusters_1 <- singles %>% ungroup() %>% distinct(origin_code) %>% rename(code = origin_code)
clusters <- singles %>% ungroup() %>% distinct(destination_code) %>% dplyr::rename(code = destination_code) %>%
                        bind_rows(.,clusters_1) %>%
                        distinct()

#Now identify which of these are clusters and match in the station data.
clusters <- clusters %>% filter(code %in% stations$cluster_id) %>%
            rename(cluster_id = code) %>%
            left_join(.,stations, by = c("cluster_id"))

#Now for all these clusters - create individual records for station ids. 
#First let's whittle it down to origin_ids which are actually clusters:
singles_1 <- singles %>% filter(origin_code %in% clusters$cluster_id) %>%
             rename(cluster_id = origin_code) %>%
             left_join(.,clusters, by = c("cluster_id")) %>%
             rename(origin_code = cluster_nlc) %>%
             ungroup() %>%
             select(-c(cluster_id)) 

#Now, for these, adjust the destination_ids which are clusters
singles_1a <- singles_1 %>% filter(destination_code %in% clusters$cluster_id) %>%
             rename(cluster_id = destination_code) %>% 
             left_join(.,clusters, by = c("cluster_id")) %>%
             rename(destination_code = cluster_nlc) %>%
             ungroup() %>%
             select(-c(cluster_id)) %>%
             mutate(type = "cluster-both")
  
#These destination codes are already station codes
singles_1b <- singles_1 %>% filter(!destination_code %in% clusters$cluster_id) %>%
  mutate(type = "cluster-origin")

#Merge the two:
singles_1 <- bind_rows(singles_1a, singles_1b)

#Now to destinations
singles_2 <- singles %>% filter(destination_code %in% clusters$cluster_id) %>%
              rename(cluster_id = destination_code) %>%
              left_join(.,clusters, by = c("cluster_id")) %>%
              rename(destination_code = cluster_nlc) %>%
              ungroup() %>%
              select(-c(cluster_id))

#Now, for these, adjust the destination_ids which are clusters
singles_2a <- singles_2 %>% filter(origin_code %in% clusters$cluster_id) %>%
  rename(cluster_id = origin_code) %>% 
  left_join(.,clusters, by = c("cluster_id")) %>%
  rename(origin_code = cluster_nlc) %>%
  ungroup() %>%
  select(-c(cluster_id)) %>%
  mutate(type = "cluster-both")

#These destination codes are already station codes
singles_2b <- singles_2 %>% filter(!origin_code %in% clusters$cluster_id) %>%
  mutate(type = "cluster-destination")

#Merge the two:
singles_2 <- bind_rows(singles_2a, singles_2b)

#Now keep those that both origin and destination were already station_ids
singles_3 <- singles %>% filter(!origin_code %in% clusters$cluster_id) %>%
                         filter(!destination_code %in% clusters$cluster_id) %>%
                         mutate(type = "station-station")

singles_complete <- bind_rows(singles_1,singles_2,singles_3)


#Now read in actual station data.
locations <- read.csv("/Users/danielgray/Desktop/projects/trains/ukrail_locations.csv") %>%
             filter(crs != "") %>%
             filter(nlc != "null") %>%
             mutate(nlc_4 = substr(nlc,1,4),
                    nlc_2 = substr(nlc,5,6)) %>%
             filter(nlc_2 == 0 | nlc_2 == "00") %>% #We only have four-digit codes in the main dataset, so we want to limit just to them
             select(nlc_4,crs) %>%
             rename(origin_code = nlc_4,
                    "X3alpha" = crs) 

locations_2 <- read.csv("/Users/danielgray/Desktop/projects/trains/stations_long_lat.csv") %>%
              select(X3alpha,longitude,latitude,station_name)

locations <- left_join(locations,locations_2, by = c("X3alpha")) %>%
             filter(!is.na(longitude)) %>%
             rename(latitude_origin = latitude,
                    longitude_origin = longitude) %>%
             select(-c(X3alpha))

#map in station and location data
singles <- left_join(singles_complete,locations, by = c("origin_code"))

locations <- locations %>% rename(destination_code = origin_code,
                                  latitude_destination = latitude_origin,
                                  longitude_destination = longitude_origin)

singles <- left_join(singles, locations, by = c("destination_code")) %>%
           rename(station_origin = station_name.x,
                  station_destination = station_name.y) %>%
           filter(!is.na(latitude_origin)) %>%
           filter(!is.na(latitude_destination))

#Now we have the prices between any two stations.  It's time to identify the relevant shapefile ID for each station.
stations <- singles %>% distinct(station_origin, .keep_all =T) %>% 
                        select(station_origin, longitude_origin, latitude_origin) %>%
                        mutate(id = row_number())

singles_short <- singles %>% group_by(station_origin, station_destination) %>% summarise(min_fare = min(cheapest))

#Now read in counties and unitary authorities latitude data from https://data.gov.uk/dataset/11302ddc-65bc-4a8f-96a9-af5c456e442c/counties-and-unitary-authorities-december-2016-full-clipped-boundaries-in-england-and-wales:
centres <- read.csv("/Users/danielgray/Desktop/projects/trains/Counties_and_Unitary_Authorities_(December_2021)_UK_BFC.csv")
centres <- centres %>% mutate(LONG = as.numeric(LONG),
                              LAT = as.numeric(LAT))
#And then you work-out the average for each shapefile <--> shapefile combination.

station_authorities <- data.frame()

for (i in stations$id) {
  
  station_name <- stations %>% filter(id == i) %>% pull(station_origin)
  
  ref_long <- stations %>% slice(i) %>% pull(longitude_origin) %>% as.numeric()
  ref_lat <- stations %>% slice(i) %>% pull(latitude_origin) %>% as.numeric()
  
  temp <- centres %>% mutate(ref_long = ref_long, ref_lat = ref_lat)
  temp$distance<-distHaversine(temp[,7:8], temp[,12:13])
  
  temp <- temp %>% arrange(distance) %>% slice(1) %>% select(CTYUA21CD,CTYUA21NM) %>%
          mutate(station_origin = station_name)
  
  station_authorities <- bind_rows(station_authorities,temp)
}

final <- left_join(singles_short,station_authorities, by = c("station_origin"))

station_authorities <- station_authorities %>% rename(station_destination = station_origin,
                                                      CTYUA21CD_d = CTYUA21CD)

final <- left_join(final,station_authorities, by = c("station_destination")) %>%
         rename(code_origin = CTYUA21CD,
                code_destination = CTYUA21CD_d,
                area_origin = CTYUA21NM.x,
                area_destination = CTYUA21NM.y) %>%
  
         select(station_origin, area_origin, code_origin, station_destination, area_destination, code_destination,min_fare) %>% 
   
  filter(min_fare != 999999) %>% 
  mutate(min_fare = min_fare / 100) %>%
  group_by(area_origin,area_destination) %>% 
  mutate( mean_min_fare = mean(min_fare,na.rm = T)) %>%
  rowwise() %>%
  mutate(diff = min_fare - mean_min_fare)

chart <- final %>% distinct(area_origin,area_destination,mean_min_fare,code_origin,code_destination) %>%
                   mutate(bucket = case_when(
                     mean_min_fare < 10 ~ "<£10",
                     mean_min_fare >= 10 &  mean_min_fare < 25 ~ "£10-£25",
                     mean_min_fare >= 25 &  mean_min_fare < 50 ~ "£25-£50",
                     mean_min_fare >= 50 &  mean_min_fare < 100 ~ "£50-£100",
                     mean_min_fare >= 100 &  mean_min_fare < 150 ~ "£100-£150",
                     mean_min_fare >= 150 ~ "£150+"))
                      
#Downloaded shapefile from official website, then simplified here:  #https://mapshaper.org/. #Note - need all shapefiles to be loaded
shapefile <- readOGR(dsn="/Users/danielgray/Desktop/projects/trains/CTYUA_DEC_2021_UK_BFC-3", layer="CTYUA_DEC_2021_UK_BFC")
new_df <- tidy(shapefile)

# Recover row name 
temp_df <- data.frame(shapefile@data$CTYUA21CD)
names(temp_df) <- c("code_destination")

# Create and append "id"
temp_df$id <- as.character(seq(0,nrow(temp_df)-1))
new_df <- left_join(new_df, temp_df, by="id")

corsa <- 16440
mileage <- 125000 #https://the75andztclub.co.uk/forum/archive/index.php/t-80206.html
average_per_year <- 7400
running_per_year <- (3356 - 1104 - 1435)
fuel_cost_per_mile <- 0.1

years <- mileage / average_per_year
additional_costs <- years * running_per_year
additional_costs_per_mile <- additional_costs / mileage
depreciation_per_mile <- corsa / mileage
total_per_mile = fuel_cost_per_mile + depreciation_per_mile + additional_costs_per_mile

drive <- read.csv("/Users/danielgray/Desktop/projects/trains/Counties_and_Unitary_Authorities_(December_2021)_UK_BFC.csv") %>%
  select(LONG,LAT,CTYUA21CD)

drive_1 <- drive %>% rename(code_origin = CTYUA21CD,
                            lat_origin = LAT,
                            long_origin = LONG)

drive_2 <- drive %>% rename(code_destination = CTYUA21CD,
                            lat_destination = LAT,
                            long_destination = LONG)

drive <- tidyr::crossing(drive_1,drive_2)
drive$distance<-distHaversine(drive[,1:2], drive[,4:5])
drive$distance <- drive$distance / 1609 * total_per_mile * 1.2
drive <- drive %>% select(code_origin, code_destination, distance)

option <- chart %>% ungroup() %>% filter(code_origin == "E09000019") %>% distinct(code_origin, code_destination,bucket,mean_min_fare) 

option <- left_join(option,drive, by = c("code_origin","code_destination")) %>%
mutate(bucket_drive = case_when(
distance < 10 ~ "<£10",
distance >= 10 &  distance < 25 ~ "£10-£25",
distance >= 25 &  distance < 50 ~ "£25-£50",
distance >= 50 &  distance < 100 ~ "£50-£100",
distance >= 100 &  distance < 150 ~ "£100-£150",
distance >= 150 ~ "£150+"))

map_data <- left_join(new_df, option, by = c("code_destination")) %>%
            filter(!grepl("N0",code_destination)) %>% #NI
            filter(code_destination != "S12000023") %>% #Orkney
filter(code_destination != "S12000027") #Shetland

map1 <- ggplot() + 
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = bucket), colour = "white", size = 0.0) +

  scale_fill_manual(values = c("<£10" = "#FEF001",
                                "£10-£25" = "#FFCE03",
                                "£25-£50" = "#FD9A01",
                                "£50-£100" = "#FF2C05",
                                "£100-£150" = "#F00000",
                                "£150+" = "#8b0000")) +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + guides(fill = guide_legend(nrow = 1)) +
  labs(title = "\n ... an anytime, single train ticket.")

map2 <- ggplot() + 
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = bucket_drive), colour = "white", size = 0.0) +
  
  scale_fill_manual(values = c("<£10" = "#FEF001",
                               "£10-£25" = "#FFCE03",
                               "£25-£50" = "#FD9A01",
                               "£50-£100" = "#FF2C05",
                               "£100-£150" = "#F00000",
                               "£150+" = "#8b0000")) +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + guides(fill = guide_legend(nrow = 1)) +
  labs(title = "\n ...a typical car.")

plot <- ggarrange(map1, map2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

annotate_figure(plot, top = text_grob("Cost of travelling from London to various parts of mainland Britain using...", 
                                      color = "black", face = "bold", size = 14),
                bottom = text_grob("\n Train travel cost calculated using ATOC fare flow data.  Colour shows the average price of an anytime single ticket from stations in Islington to stations in every \n other Unitary Authority in mainland Great Britain. Advance and off-peak tickets are cheaper but cannot  be used for spontaneous travel like a car. \n Car estimate assumes fuel costs of 10.4p per mile driven, with distance equal to 1.2x the 'as the crow-flies' between the mid-point of Islington and the mid-point of the \n other Unitary Authority. The calculation also assumes (ii) depreciation costs, calculated based on the most popular UK car (a Vauxhall Corsa) with a total \n expected mileage of 125,000. It also inclues (iii) share (mileage divided by total expected mileage of 125000) of additional costs such as insurance and MOT.", 
                                   color = "black", size = 7, hjust = 0.49, just = "left"))



                    