dat_2018 <- read_excel("dat_msb_2018.xlsx")
colnames(dat_2018) <- c("code", "date", "time", "originary_fire", "consequence_fire", 
                        "län_code", "län_name","kommun_code", "kommun_name",
                        "kommun-type_code", "kommun_type", "deso_code", 
                        "type_of_environment", "environment", "number_of_cars", 
                        "number_of_buses", "number_of_trucks",
                        "number_of_campers", "number_of_other_vehicles", 
                        "number_of_agricultural_machines", "number_of_forestry_machines",
                        "number_of_other_work_machines", "number_of_trains", "number_of_boats",
                        "number_of_helicopters_and_planes", "number_of_other_appliances_or_machines",
                        "type_of_object_catching_fire_first", "object_catching_fire_first",
                        "type_of_source_of_fire_source", "fire_source", "cause") 


