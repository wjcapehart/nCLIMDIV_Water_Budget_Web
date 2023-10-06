#
#
# nCLIMDIV_Water_Budget_Web
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(package = "shiny")
library(package = "tidyverse")
library(package = "lubridate")
library(package = "ClimClass") 


#
# Input Cliamte Data
#

#load(file = url(description = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/CLASS_Examples/nCLIMDIV.Rdata"))
#load("/projects/THREDDS/local_academic_repo/CLASS_Examples/nCLIMDIV.Rdata")
load("./nCLIMDIV.Rdata")

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


ui = fluidPage(
    
    title = "NCEI Climate Zone Water Budgets",  
    
    titlePanel(title = "NCEI Climate Zone Water Budgets"),   # Application title

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            
            selectInput(inputId  = "target_state_name",
                        label    = "US State",
                        choices  = state_code_lut$State_Name,
                        selected = state_code_lut$State_Name[1]),
            
            selectInput(inputId  = "target_climate_division",
                        label    = "State Climate Zone Division",
                        choices  = selected_zones$Zone_Name_and_Code,
                        selected = selected_zones$Zone_Name_and_Code[1]),
            
            uiOutput("target_climate_division"),

            sliderInput(inputId = "start_plot_year",
                        label   = "Start Year for Plotting",
                        min     = 1900,
                        max     = last_year-1,
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
                        max     = 200,
                        value   = 150,
                        sep     = ""),  
            
            
            
            h5("State Climate Division Map"),
            
           imageOutput(outputId = "state_division_map")
        ),
            
        
        
        
        
        # Show a plot of the generated distribution
        mainPanel = mainPanel(
            
            h2("Introduction"), 
            
            p("This web application allows you to create a 'Thornthwaite-Mather Water Budget,' a water resource accounting tool that partitions rainfall between "),
            p("- ideal (or 'potential') evaporation,"),
            p("- actual evaporation,"),
            p("- extraction of water from the soil,"),
            p("- subsequent recharge of water into the soil, and"),
            p("- soil and snowpack storage of water"),
            p("This lets us review seasonal changes in the water cycle from year to year."),
            p("The script behind this page uses the R 'ClimClass' Package"),
            
            h2("Instructions"), 
            
            p("Using the map, select the state and then the numbered region in the state from the pull-down menus"),
            p("Next, you can select the period over which the budget is calculated with the sliders"),
            p("The results will be shown below"),
            h3("References"),
            p("Thornthwaite, C.W.; Mather, J.R. The Water Balance; Laboratory in Climatology, Johns Hopkins University: Baltimore, MD, USA,
1955; Volume 8, pp. 1–104."),
            p("Emanuele Eccel's R ClimClass Package. https://CRAN.R-project.org/package=ClimClass"),
          
            h2("Budget Graph"), 
            plotOutput(outputId = "thornthwaitePlot"),
            
            h2("Budget Table for Full Period"),
            dataTableOutput(outputId = "budget_table"),
            
            h2("Download Full Budget Period"),
            downloadButton('downloadData', 'Download')
          
            # h2("Deepdive on Thorthwaite Mather Budgets")

        )
    )
)


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
      filter(State_Name == input$target_state_name)
    
    target_climate_division = str_c(target_climate_division$State_Code,
                                    substring(text  = input$target_climate_division,
                                              first = 1,
                                              last  = 2), 
                                    sep = "")
    
    spinup_period         = 10 # calendar years for spinup period
    
    initial_snow_cover =   0 # snow reservoir storage capacity in mm
    
    single_zone = nCLIMDIV %>% filter(Full_Zone_Code == target_climate_division)
    
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
   water_budget$Temp_Avg = round(thorntwaite_inputs$Tm[1:length(water_budget$Precipitation)],2)
    
    
    
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
    
    observeEvent(input$target_state_name,  {
        
        selected_zones = state_zone_lut %>% 
            filter(State_Name == input$target_state_name) 
        
        state_number = as.numeric(unique(selected_zones$State_Code))
        
        updateSelectInput(session = session, 
                          inputId = "target_climate_division", 
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
            filter(State_Name == input$target_state_name) 
        
        state_number = as.numeric(unique(selected_zones$State_Code))
        

        filename <- normalizePath(file.path('./state_climate_division_images',
                                            paste('state_', 
                                                  sprintf("%02d",
                                                          state_number), 
                                                  '.gif', 
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
        
        subset_bars$Variable = as_factor(subset_bars$Variable)

        ggplot(data = subset_lines) +
            
            theme_bw() +
            
            aes(x     = Date,
                y     = Value) +
            
            ggtitle(label    = "Thornthwaite-Mather Water Budget",
                    subtitle = str_c(unique(input$target_climate_division),
                                     unique(input$target_state_name),
                                     sep = ", ")) + 
            
            labs(caption = str_c("Soil Storage Capacity = ",
                                    input$soil_storage_capacity,
                                    " mm",
                                    sep = "")) + 
            
            xlab(label = "Time") +
            
            ylab(label = "Budget (mm)") +
            
            scale_fill_manual(values = c( "Precipitation"  = "darkgreen", 
                                          "Potential_Evap" = "darkred",
                                          "Deficit"     = "orange",
                                          "Surplus"     = "green",
                                          "Recharge"    = "blue",
                                          "Snowpack"    = "lightcyan",
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
                                           "Deficit"     = "orange",
                                           "Surplus"     = "green",
                                           "Recharge"    = "blue",
                                           "Snowpack"    = "lightcyan",
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



