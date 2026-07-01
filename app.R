#
#
# nCLIMDIV_Water_Budget_Web
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(package = "shiny")
library(package = "bslib")

library(package = "tidyverse")
library(package = "lubridate")
library(package = "ClimClass") 
library(package = "htmltools")

library(package = "DT")



#
# Input Climate Data
#



load("./nCLIMDIV.Rdata", verbose=TRUE)
load("./NCEI_nClimDiv_LUT.RData", verbose=TRUE)


#print(nClimDiv)

NCEI_nClimDiv_LUT = NCEI_nClimDiv_LUT %>%
  rename("Full_Zone_Code" = climdiv,
         "AWC"             = climdiv_mean_mass_content_of_water_in_soil) %>%
  select("Full_Zone_Code",
         "AWC")

nCLIMDIV$State_Name = gsub(pattern     = "\u00A0", 
                           replacement = "", 
                           x           = nCLIMDIV$State_Name, 
                           fixed       = TRUE)


last_year = year(max(nCLIMDIV$Date[(month(nCLIMDIV$Date) == 12)]))

#
# Create Pulldown Look-up-Tables
#  

state_zone_lut =  nCLIMDIV %>% 
  select(c(Full_Zone_Code,State_Name,Zone_Name))   %>%
  mutate(State_Code    = substring(text  = Full_Zone_Code,
                                   first = 1,
                                   last  = 2),
         SubState_Code = substring(text  = Full_Zone_Code,
                                   first = 3,
                                   last  = 4)) %>%
  mutate(Zone_Name_and_Code = str_c(SubState_Code,
                                    Zone_Name,
                                    sep = " : "),
         Zones_Per_State    = max(SubState_Code) ) %>%
  unique()

state_code_lut = state_zone_lut %>% 
  group_by(State_Name) %>% 
  mutate(Zones_Per_State = max(SubState_Code)) %>%
  select(c(State_Name,
           State_Code,
           Zones_Per_State))  %>%
  unique()

selected_zones = state_zone_lut %>% 
  filter(State_Name == "South_Dakota")


state_number_init =  as.numeric(unique(selected_zones$State_Code))


state_number = state_number_init












###############################################################################
###############################################################################
##
## User Interface Function
##


ui = page_sidebar(
  
  sidebar = sidebar(
    
    
    selectInput(inputId  = "selected_target_state_name",
                label    = "US State",
                choices  = state_code_lut$State_Name,
                selected = state_code_lut$State_Name[39]),
    
    selectInput(inputId  = "selected_target_climate_division",  # input
                label    = "State Climate Zone Division",
                choices  = selected_zones$Zone_Name_and_Code,
                selected = selected_zones$Zone_Name_and_Code[1]),
    
    sliderInput(inputId = "start_plot_year",
                label   = "Start Year for Plotting",
                min     = 1900,
                max     = last_year,
                value   = 2010,
                sep     = ""),   
    
    sliderInput(inputId = "end_plot_year",
                label   = "End Year for Plotting",
                min     = 1901,
                max     = last_year,
                value   = last_year,
                sep     = ""),  
    
    sliderInput(inputId = "soil_storage_capacity",
                label   = "Soil Storage Capacity (mm)",
                min     =  50,
                max     = 310,
                value   = 167.40298,
                sep     = ""),  
    
    p("State Climate Zone Map"),
    
    imageOutput(outputId = "state_division_map"),

    title = "User Control",
    
    fillable = FALSE,
    
  ), # sidebar()

  
  card(
    card_header("Thornthwaite Budget Graph"),
    card_body(
      plotOutput(outputId = "thornthwaitePlot")
    ),
    min_height = "400px"
  ),
  #br(),
  
  card(
    card_header("Budget Table for Full Period"),
    card_body(
      DTOutput(outputId = "budget_table"),
      p("Units in mm for water, °C for Temperatures"),
    ),
    min_height = "500px"
  ),
  #br(),  
  
  card(
    card_header("Download Full Budget Period"),
    card_body(
      downloadButton(outputId = 'downloadData', 
                     label    = "Download to CSV"),
      p("Units in mm for water, °C for Temperatures"),
      
    ),
    min_height = "170px"
  ),
  #br(),
  
  card(
    card_header("About This Dislay"),
    card_body(
      p("This web application allows you to create a 'Thornthwaite-Mather Water Budget,' a water resource accounting tool.  A 'deep dive' on how Thornthwaite-Mather Budgters work is below. "),
      p("The data driving this application is the NOAA Monthly U.S. Climate Divisional Database (NClimDiv) which provides quality-checked past climate data aggregated to regional state climate divisions."),
      p("To use, the user can select the State and then the Climate Division (a state climate divsion map will be displayed for reference. "),
      p("From there, you can provide a start and end date plot, and if you wish, change the local default maximuim soil storage."),
      p("The results can be viewed in the accompanying graph for the selected dates and the table can be downloaded as a comma-delimited file for the whole time series."),
      p("The script behind this page uses the R 'ClimClass' Package"),
    ),
    min_height = "200px"
  ),
  #br(),  
  
  card(
    card_header("About Thornthwaite-Mather Budgets"),
      card_body(
        p("The Thornthwaite-Mather Budget (Thornthwaite and Mather, 1955) is a simple water-accounting scheme that requires only minimal input data (monthly mean temperature and monthly total precipitation).  The budget can be presented as a ledger-style table or, more often, as the line/bar/area graphic shown in the topmost figure above.  "),
         
        p("Its components are as follows (all units here are in depth of water, in our case mm of water)."),
         
        HTML("<span style='color: #008000;font-weight: bold;'>Precipitation</span>"),
        p("As with all budgets, we have an income stream. Here, it is the total monthly precipitation. This data typically comes directly from observed values. In the budget graphic above, it is represented by the solid green line (a reference dashed line is available for the future climate scenarios to show the historical baseline)."),
         
        HTML("<span style='color: #880000;font-weight: bold;'>Potential Evapotranspiration</span>"),
        p("This represents the atmospheric demand for your available water. It may exceed the amount actually available in your water budget for that month. Potential Evapotranspiration can be calculated in several ways, but here it is a function of monthly and annual temperatures and latitude. Like precipitation, this is represented by a solid line in our Thornthwaite-Mather budget plots, this time in blue. And again, a historical reference line for the future climate plots is included as the dashed line."),
         
        HTML("<span style='color: #BA8E23;font-weight: bold;'>Evapotranspiration</span>"),
        p("If there is sufficient water from precipitation, the actual amount of evapotranspiration equals Potential Evaporation. But when Precipitation (supply) is outpaced by Potential Evapotranspiration (atmospheric demand), the difference is drawn from the landscape's water storage. The wetter the soil (the more storage), the easier it is to extract water to offset our budgetary imbalance. As the soil dries, resistance to extracting water from the soil increases. In our budget figure, evapotranspiration is shown as the yellow-shaded area. The actual storage amount in the model can be seen in the figure comparing the budgets across the different scenarios, but it is typically not explicitly shown in the classic Thornthwaite-Mather budget plots."),
         
        HTML("<span style='color: #FF800D;font-weight: bold;'>Deficit</span>"),
        p("Even with evaporation tapping the soil as a supplemental source to close the gap between atmospheric evaporative demand and precipitation, that still won’t be enough to fully close it. This failure to optimally close the water budget is “The Deficit.” We can express it as plant stress (often seen in the summer months), austerity measures in water resources planning, and other adverse impacts. This is represented by the orange-shaded areas in the budget plot."),
         
        HTML("<span style='color: #00EE00;font-weight: bold;'>Recharge</span>"),
        p("Once atmospheric demand (Potential Evapotranspiration) exceeds inbound water (Precipitation), we can “pay back” the water extracted from the soil reserves. This excess is “Recharge” and is shown as the green-shaded area in the budget chart."),
         
        HTML("<span style='color: #1E90FF;font-weight: bold;'>Surplus</span>"),
        p("Once the recharge is paid off and the soil water storage reaches its maximum capacity, any precipitation within a month that exceeds the Potential Evapotranspiration is presented as excess “Surplus” (the blue-shaded area). In the classical Thornthwaite-Mather budget scheme, this water is flushed from the system, much like streamflow or runoff is removed from a watershed or landscape area unit. Hypothetically, a user could send this water into a supplementary storage (e.g., groundwater or a protected reservoir) or route a fraction of it through a simple routing model."),
         
        HTML("<span style='color: #009999;font-weight: bold;'>Snowpack</span>"),
        p("Yet another reservoir that can persist for months, Snowpack (shown in our budget plot as the light blue shading), can be added to the budget scheme. Here, Precipitation when temperatures are below a given threshold (e.g., near freezing) is not subject to evaporation or loss through Surplus until temperatures rise above that threshold, at which point the “meltwater” can be partitioned latently into Recharge or Surplus."),
      ),
    min_height = "500px"
  ),
  
  card(
    card_header("Citations & References"),
    card_body(
      markdown("Thornthwaite, C.W., and J.R. Mather, 1955: The water balance. *Publications in Climatology*, **8**(1), Laboratory of Climatology. Drexel Institute of Technology, Centerton, NJ."),
      markdown("Emanuele Eccel's R ClimClass Package. [https://CRAN.R-project.org/package=ClimClass](https://CRAN.R-project.org/package=ClimClass)."),
    ),
    min_height = "150px"
  ),  
  
  
  style = "overflow-y: auto;",
  title = "NCEI Climate Zone Water Budgets",
  lang  = "en",
  
 
)   # page_sidebar page
      
    




##
###############################################################################
###############################################################################




















###############################################################################
###############################################################################
##
## Server Function
##

server = function(input, 
                  output,
                  session) {
  
  

    
    
  
  ###############################################################################
  ###############################################################################
  ##
  ## Calculate Water Budgtets
  
  
  water_budget <- reactive({
    
    
    target_climate_division = state_code_lut %>% 
      filter(State_Name == input$selected_target_state_name)
    
    target_climate_division = str_c(target_climate_division$State_Code,
                                    substring(text  = input$selected_target_climate_division,
                                              first = 1,
                                              last  = 2), 
                                    sep = "")
    
    spinup_period         = 10 # calendar years for spinup period
    
    initial_snow_cover =   0 # snow reservoir storage capacity in mm
    
    single_zone = nCLIMDIV %>% 
      filter(Full_Zone_Code == target_climate_division)
    
    # generate thonthwaite budget
    
    thorntwaite_inputs =     tibble(year   = year(single_zone$Date),
                                    month  = month(single_zone$Date),
                                    P      = single_zone$PCPN,  
                                    Tn     = single_zone$TMIN,  
                                    Tx     = single_zone$TMAX,
                                    Tm     = single_zone$TMPC)

    thorntwaite_budget_raw = thornthwaite(series          = thorntwaite_inputs, 
                                          latitude        = single_zone$Center_Lat[1], 
                                          clim_norm       = NULL, 
                                          first.yr        = min(thorntwaite_inputs$year), 
                                          last.yr         = last_year, 
                                          quant           = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), 
                                          snow.init       = initial_snow_cover, 
                                          Tsnow           = -1, 
                                          TAW             = input$soil_storage_capacity, 
                                          fr.sn.acc       = 0.95, 
                                          snow_melt_coeff = 1)
    
    
    # collect the output into a single data frame. (this probably could be made more prettier)
    
    # Precipitation
    water_budget = t( as_tibble(thorntwaite_budget_raw$W_balance$Precipitation) )
    colnames(water_budget) = str_c(1:12)
    water_budget = water_budget %>%
      as.data.frame %>% 
      rownames_to_column(.,
                         var = 'year')
    water_budget$Variable = "Precipitation"
    
    #Et0
    raw_wb2 = t( as_tibble(thorntwaite_budget_raw$W_balance$Et0) )
    colnames(raw_wb2) = str_c(1:12)
    raw_wb2 = raw_wb2 %>%
      as.data.frame %>% 
      rownames_to_column(.,
                         var = 'year')
    raw_wb2$Variable = "Potential_Evap"
    
    water_budget = rbind(water_budget,raw_wb2)
    remove(raw_wb2)
    
    #Storage
    raw_wb2           = t( as_tibble(thorntwaite_budget_raw$W_balance$Storage) )
    colnames(raw_wb2) = str_c(1:12)
    raw_wb2 = raw_wb2 %>%
      as.data.frame %>% 
      rownames_to_column(.,
                         var = 'year')
    raw_wb2$Variable = "Storage"
    
    water_budget = rbind(water_budget,raw_wb2)  
    remove(raw_wb2)
    
    #'Prec. - PotEvap.'
    raw_wb2 = t( as_tibble(thorntwaite_budget_raw$W_balance$'Prec. - Evap.') )
    colnames(raw_wb2) = str_c(1:12)
    raw_wb2 = raw_wb2 %>%
      as.data.frame %>% 
      rownames_to_column(.,
                         var = 'year')
    raw_wb2$Variable = "Prec_m_PE"
    
    water_budget = rbind(water_budget,raw_wb2)  
    remove(raw_wb2)
    
    #Deficit
    raw_wb2 = t( as_tibble(thorntwaite_budget_raw$W_balance$Deficit) )
    colnames(raw_wb2) = str_c(1:12)
    raw_wb2 = raw_wb2 %>%
      as.data.frame %>% 
      rownames_to_column(.,
                         var = 'year')
    raw_wb2$Variable = "Deficit"
    
    water_budget = rbind(water_budget,raw_wb2)
    remove(raw_wb2)
    
    #Surplus
    raw_wb2 = t( as_tibble(thorntwaite_budget_raw$W_balance$Surplus) )
    colnames(raw_wb2) = str_c(1:12)
    raw_wb2 = raw_wb2 %>%
      as.data.frame %>% 
      rownames_to_column(.,
                         var = 'year')
    raw_wb2$Variable = "Surplus"
    
    water_budget = rbind(water_budget, raw_wb2)
    remove(raw_wb2)
    
    water_budget = gather(data  = water_budget,
                          key   = month,
                          value = "value",
                          str_c(1:12))
    
    water_budget$Date = as.Date(str_c(water_budget$year,
                                      "-",
                                      water_budget$month,
                                      "-15",
                                      sep = ""))
    
    water_budget = spread(data = water_budget,
                          key  = "Variable",
                          value = "value")
    
    water_budget = water_budget %>% arrange(Date)
    
    # finish the budget by critical parameters
    
    # calculate evapotransporation
    water_budget = water_budget %>% 
      mutate(Evaporation = Potential_Evap - Deficit)
    
    # calculate precipitation - true evaporation
    water_budget = water_budget %>% 
      mutate(Prec_m_Evap = Precipitation - Evaporation)
    
    # calculate recharge by calculating the increase in soil storage from rainfall
    water_budget = water_budget %>% 
      mutate(Recharge = c(NA, diff(x = Storage, 
                                   lag = 1))) %>% 
      mutate(Recharge = ifelse(test = Recharge>0, 
                               yes  = Recharge, 
                               no   = 0))
    
    # separate recharge from surplus in teh water budget
    water_budget = water_budget %>% 
      mutate(Surplus = Surplus - Recharge)  %>% 
      mutate(Surplus = ifelse(test = Surplus>0, 
                              yes  = Surplus, 
                              no   = 0))
    
    # calculate recharge by calculating the increase in soil storage from rainfall
    water_budget = water_budget %>% 
      mutate(Snowpack = Prec_m_Evap - Recharge - Surplus)  %>% 
      mutate(Snowpack = ifelse(test = Snowpack>0, 
                               yes  = Snowpack, 
                               no   = 0))  
    
    # repair precip-PE (seems to be a typo in the original ClimClass Code)
    water_budget = water_budget %>% 
      mutate(Prec_m_PE = Precipitation - Potential_Evap)    
    
    water_budget$Surplus = round(water_budget$Surplus,1)
    water_budget$Recharge = round(water_budget$Recharge,1)
    water_budget$Precipitation = round(water_budget$Precipitation,1)
    water_budget$Deficit = round(water_budget$Deficit,1)
    water_budget$Evaporation = round(water_budget$Evaporation,1)
    water_budget$Snowpack = round(water_budget$Snowpack,1)
    water_budget$Prec_m_Evap = round(water_budget$Prec_m_Evap,1)
    water_budget$Prec_m_PE = round(water_budget$Prec_m_PE,1)
    water_budget$Temp_Avg = round(thorntwaite_inputs$Tm[1:length(water_budget$Precipitation)],1)
    
    
    water_budget = water_budget %>% select(Date,
                                           Temp_Avg,
                                           Precipitation,
                                           Potential_Evap,
                                           Evaporation,
                                           Deficit,
                                           Storage,
                                           Snowpack,
                                           Recharge,
                                           Surplus,
                                           Prec_m_PE,
                                           Prec_m_Evap)
    
    
    # clean up the mess
    remove(thorntwaite_budget_raw)
    remove(thorntwaite_inputs)
    return(water_budget)
    #
    ###############################################################################
    ###############################################################################
    
  })
  
  
  #
  ###############################################################################
  ###############################################################################
  
  
  
  
  
  
  ###############################################################################
  #
  # In-State Climate Zone Selection
  #
  
  observeEvent(input$selected_target_state_name,  {
    
    selected_zones = state_zone_lut %>% 
      filter(State_Name == input$selected_target_state_name) 
    
    state_number = as.numeric(unique(selected_zones$State_Code))
    
    updateSelectInput(session = session, 
                      inputId = "selected_target_climate_division", 
                      choices = selected_zones$Zone_Name_and_Code)
  }
  )
  
  #
  ###############################################################################
  
  
  
  
  ###############################################################################
  #
  # In-State Climate Map for Division Selection
  #
  
  output$state_division_map = renderImage({
    
    
    
    selected_zones = state_zone_lut %>% 
      filter(State_Name == input$selected_target_state_name) 
    
    state_number = as.numeric(unique(selected_zones$State_Code))
    
    
    filename <- normalizePath(file.path('./state_climate_division_images',
                                        paste('state_', 
                                              sprintf("%02d",
                                                      state_number), 
                                              '.png', 
                                              sep='')))
    
    # Return a list containing the filename
    list(src   = filename,
         width = "100%")
  }, 
  deleteFile = FALSE)
  
  #
  ###############################################################################
  
  
  
  ###############################################################################
  #
  # End Year Range Selection
  #
  
  observeEvent(input$start_plot_year,  {
    
    updateSliderInput(session = session, 
                      inputId = "end_plot_year", 
                      min = input$start_plot_year)
    
  })
  
  #
  ###############################################################################
  
  
  ###############################################################################
  #
  # Update Default Soil Water Capacity
  #
  
  observeEvent(input$selected_target_climate_division,  {
    

    selected_zones = state_zone_lut %>% 
      filter(State_Name == input$selected_target_state_name) 
    
    state_number = (unique(selected_zones$State_Code))

   climdiv_for_awc = str_c(state_number,
                           substring(text  = input$selected_target_climate_division,
                                     first = 1,
                                     last  = 2), 
                                     sep = "")

   AWC_rec = NCEI_nClimDiv_LUT %>%
     filter(Full_Zone_Code == climdiv_for_awc)
   

   slider_AWC = round(AWC_rec$AWC,1)
  
   updateSliderInput(session = session, 
                     inputId = "soil_storage_capacity", 
                     value = slider_AWC)
    
    
    
  })
  
  #
  ###############################################################################
  
  
  ###############################################################################
  #
  #  Plot Thortnwhatie Budgets
  #
  
  output$thornthwaitePlot = renderPlot(expr = {
    
    ###############################################################################
    #
    # Draw Time Series
    #
    
    # making two datasets, one for lines, the other for bars
    
    local_water_budget = water_budget()
    
    subset = local_water_budget %>% filter((year(Date) >= input$start_plot_year) &
                                             (year(Date) <= input$end_plot_year)   )
    
    subset_lines = subset %>% select(Date,
                                     Precipitation,
                                     Potential_Evap)
    
    subset_lines = gather(data  = subset_lines,
                          value = "Value",
                          key   = "Variable",
                          Precipitation,
                          Potential_Evap)
    
    subset_lines$Variable = as_factor(subset_lines$Variable)

    
    subset_bars = subset %>% select(Date,
                                    Evaporation,
                                    Surplus,
                                    Recharge,
                                    Snowpack,
                                    Deficit)
    
    subset_bars = gather(data  = subset_bars,
                         value = "Value",
                         key   = "Variable",
                         Deficit,
                         Surplus,
                         Recharge,
                         Snowpack,
                         Evaporation) 
    
    subset_bars$Variable = ordered(subset_bars$Variable,
                                     levels = c("Surplus", 
                                                "Recharge", 
                                                "Snowpack", 
                                                "Deficit", 
                                                "Evaporation"))
     
    ggplot(data = subset_lines) +
      
      theme_bw() +
      
      aes(x     = Date,
          y     = Value) +
      
      ggtitle(label    = "Thornthwaite-Mather Water Budget",
              subtitle = str_c(unique(input$selected_target_climate_division),
                               unique(input$selected_target_state_name),
                               sep = ", ")) + 
      
      labs(caption = str_c("Soil Storage Capacity = ",
                           input$soil_storage_capacity,
                           " mm",
                           sep = "")) + 
      
      xlab(label = "Time") +
      
      ylab(label = "Budget (mm)") +
      
      scale_fill_manual(values = c( "Precipitation"  = "darkgreen", 
                                    "Potential_Evap" = "darkred",
                                    "Surplus"     = "dodgerblue",
                                    "Recharge"    = "limegreen",
                                    "Snowpack"    = "lightcyan",
                                    "Deficit"     = "orange",
                                    "Evaporation" = "yellow"),
                        name = NULL) +
      
      geom_bar(data    = subset_bars,
               mapping = aes(x     = Date,
                             y    = Value,
                             fill = Variable),
               stat = 'identity',
               position = "stack") +
      
      scale_color_manual(values = c( "Precipitation"  = "darkgreen", 
                                     "Potential_Evap" = "darkred",
                                     "Surplus"     = "dodgerblue",
                                     "Recharge"    = "limegreen",
                                     "Snowpack"    = "lightcyan",
                                     "Deficit"     = "orange",
                                     "Evaporation" = "yellow"),
                         name = NULL) +
      
      geom_line(data    = subset_lines,
                mapping = aes(x     = Date,
                              y     = Value,
                              color = Variable,
                              fill  = NULL))     
    
    
    #
    ###############################################################################
    
    
  })
  
  #
  ###############################################################################
  
  
  
  
  
  ###############################################################################
  #
  # Display Thorntwaite Budgets as a Table
  #
  
  output$budget_table =  renderDataTable( expr = {
    
    
    water_budget()  
    
    
  } )
  
  #
  ###############################################################################
  
  
  ###############################################################################
  #
  # Download Full Budgtet to a CSV FIle
  #
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Water_Budget.csv", sep = "")
    },
    content = function(file) {
      write.csv(water_budget(), file, row.names = FALSE)
    }
  )
  
  #
  ###############################################################################
  
  
}

##
###############################################################################
###############################################################################







###############################################################################
###############################################################################
##
## shinyApp Function
##

# Run the application 
shinyApp(ui     = ui, 
         server = server)

##
###############################################################################
###############################################################################