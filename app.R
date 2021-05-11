library(shiny)
library(shinydashboard)
library(leaflet)
library(shinythemes)
library(sf)
library(tidyverse)
library(dplyr)
library(DT)
library(rgdal)
library(sf)
library(ggmap)
library(magrittr)
library(tidyverse)
library(forcats)
library(plotly)
library(RColorBrewer)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
    skin <- "blue"

## Edith

# Load school data
school_data <- st_read("School_Boundaries.shp")

# Load census data
census_data <- st_read("2010_CensusData.shp")

zip_code <- st_read("SJCZipCodes_clip.shp")

# Function to get zip code for spatial data
get_zipcode <- function(df, crs) {
    
    google.crs <- crs
    
    zip_code_google <- zip_code %>% st_transform(crs = google.crs)
    
    data_google <- df %>% st_transform(crs = google.crs)
    
    zip_data <- st_join(x = data_google, y = zip_code_google%>%select(ZCTA5))
    
    zip_data <- zip_data %>% rename(Zip_Code = ZCTA5)
    
    return(zip_data)
}


# get zip code for school data
school_data <- get_zipcode(school_data, 4326)

# Create popup
school_data$popup <- paste("<b>", school_data$School, "</b><br>",
                           "Type: ", school_data$SchoolType, "<br>",
                           "Zip:", school_data$Zip_Code, "<br>")

# Load park data
park_data <- read_csv("Parks_Locations_and_Features.csv")

# create popup
park_data$popup <- paste("<b>", park_data$Park_Name, "</b><br>",
                         "Type: ", park_data$Park_Type, "<br>",
                         "Address: ",park_data$Address, "<br>")

types <- c(unique(park_data$Park_Type))

### Ankur Added for summary table ####

parks.spatial <- park_data %>% #projecting the table as an sf and setting the coordinate system
    st_as_sf(coords = c("Lon","Lat")) %>% 
    st_set_crs(value = 4326)

parks.subset <- park_data %>%
    select(Park_Name, Park_Type, Zip_Code, Address, Lat, Lon) %>%
    arrange(desc(Park_Type ))

parks.subset$id <- seq.int(nrow(parks.subset))

str(parks.subset)

school_data$id <- seq.int(nrow(school_data))

str(school_data)

parks_table <- parks.subset %>% select(Park_Name, Park_Type, Address) %>%
    arrange(desc(Park_Type ))



### End Ankur Add


# get population type data
population <- census_data %>% select(starts_with("SE_T054",), geometry)

# get zip code for selected data
population <- get_zipcode(population, 4326)


population <- st_drop_geometry(population)

# rename columns
names(population) <- c("Total", "White", "African American",
                       "American Indian", "Asian", "Native Hawaiian",
                       "Some Other Race", "Two or More Races", "Zip_Code")
# omit na
population <- na.omit(population)

# tidy data
population <- gather(population, key= "Race", value = "RaceTotal", -c(Total, Zip_Code))

# aggregate data by zip code and race
popAggregate <- population%>%group_by(Zip_Code, Race) %>%
    summarise(Total = sum(RaceTotal))

# find total for all groups
totals <- popAggregate %>% group_by(Zip_Code) %>%summarise(Totals = sum(Total))

# include total column on original data
popAggregate <- left_join(popAggregate, totals, by = "Zip_Code")

# calculate percent of each race

#popAgregate <- popAgregate%>%mutate(percent = round(Total/Totals*100))


popAggregate <- popAggregate%>%mutate(percent = round(Total/Totals*100))


# select data for households
households <- census_data%>%select(starts_with("SE_T058"), geometry)

households <- get_zipcode(households, 4326)

households <- st_drop_geometry(households)

# exclude totals per main household type
households <- households%>%select(-c("SE_T058_00", "SE_T058_06",
                                     "SE_T058_01", "SE_T058_03"))
# set names for new columns
names(households) <- c("Married", "Male Householder", "Female Householder",
                       "Living Alone", "Not Living Alone", "Zip_Code" )

# gather data
households <- households%>%gather(key = householdType, value = total, -Zip_Code)

households$FamNonFam <- NA

# classify family type as family or not family
households$FamNonFam[households$householdType %in%
                         c("Married", "Male Householder", "Female Householder")] <- "Family"

households$FamNonFam[!households$householdType %in%
                         c("Married", "Male Householder", "Female Householder")] <- "NonFamily"
# omit na
households <- na.omit(households)

# summarize
householdsAggregate <- households%>%group_by(Zip_Code, householdType, FamNonFam)%>%
    summarise(totals = sum(total))

# summarize household type
householdsFamily <- households%>%group_by(Zip_Code, FamNonFam)%>%summarise(Total = sum(total))

# generate donut chart code
Totals <- householdsFamily%>%group_by(Zip_Code)%>%summarise(Totals = sum(Total))

householdsFamily <- left_join(householdsFamily, Totals, by = "Zip_Code")

householdsFamily <- householdsFamily%>%mutate(percent = Total/Totals)

ymaxs <- householdsFamily%>% group_by(Zip_Code)%>%summarise(ymax = cumsum(percent))

householdsFamily <- cbind(householdsFamily, ymax=ymaxs$ymax)

ymins <- householdsFamily%>%group_by(Zip_Code)%>%summarise(ymin = c(0, head(ymax, n=-1)))

householdsFamily <- cbind(householdsFamily, ymin = ymins$ymin)

householdsFamily$labelPositions <- (householdsFamily$ymax + householdsFamily$ymin)/2

householdsFamily$label <- paste0(householdsFamily$FamNonFam, "\n", round(householdsFamily$percent*100), "%")

############################################################################

### Dana

# Load in Abandoned Properties
abandoned_spatial <- st_read("Abandoned_Property_Parcels.shp")

# Load in Business data
business_points <- read.csv("Business_Licenses_geocoded.csv", stringsAsFactors = F) %>% 
    filter(State == "IN")# Filter out businesses that are physically in South Bend IN only

# Convert bussiness to spatial data
business_spatial <- business_points %>% 
    st_as_sf(coords = c("X","Y")) %>% 
    st_set_crs(value = st_crs(4326))

# Clean up zip_code. Remove the last 4 digits of codes that have 9 digits
business_spatial$Zip_Code <- as.character(business_spatial$Zip_Code) %>%
    gsub('^([0-9]{5})([0-9]+)$', '\\1', .) %>% as.integer(.)


abandoned_spatial$Zip_Code <- as.character(abandoned_spatial$Zip_Code) %>% 
    gsub('^([0-9]{5})([0-9]+)$', '\\1', .) %>% as.integer(.)

# Create pop-up
business_spatial$popup <- paste("<b>", business_spatial$Business_N, "</b><br>",
                                "Type: ", business_spatial$Classifi_1, sep="") 

abandoned_spatial$popup <- paste('<b>', abandoned_spatial$Property_S, "</b><br>",
                                 "Structure Type: ", abandoned_spatial$Structures, sep="")



# Subset data for gender
census_fm <- census_data %>% select(SE_T003_01, SE_T003_02, geometry)

# Get zip codes for gender data
# Groupby zip and summarize total gender data by zip 
pop_fm_by_zip <- get_zipcode(census_fm, 3857) %>% 
    st_set_geometry(NULL) %>% 
    group_by(Zip_Code) %>% 
    summarise(across(.cols = starts_with("SE"), .fns = sum))

colnames(pop_fm_by_zip) <- c("zipcode", "male", "female")

# Tidy gender data
pop_fm_tidy <- gather(pop_fm_by_zip, 
                      key = "gender", 
                      value = "population",
                      -zipcode)


# Subset data for age
census_age <- census_data %>% select(starts_with("SE_T008"), -SE_T008_00, geometry)

# Get zip codes for age data
# Group_by zip and summarize total age data by zip 
pop_age_by_zip <- get_zipcode(census_age, 3857) %>% 
    st_set_geometry(NULL) %>% 
    group_by(Zip_Code) %>% 
    summarise(across(.cols = starts_with("SE"), .fns = sum))

colnames(pop_age_by_zip) <- c("zipcode", "under_5", "5-9",
                              "10-14","15-17", "18-24", "25-34", "35-44","45-54",
                              "55-64", "65-74", "75-84", "over_84")

# Tidy age data
pop_age_tidy <- gather(pop_age_by_zip,
                       key = "age_range",
                       value="population",
                       -zipcode)

age_level <- c("under_5", "5-9","10-14","15-17", "18-24", "25-34", 
               "35-44","45-54","55-64", "65-74", "75-84", "over_84")

pop_age_tidy$age_range <- factor(pop_age_tidy$age_range, levels = age_level)

# Ankur Added for summary table ###



# subset data

business_sub <- business_points %>%
    select(c(1:11))

abandoned_sub <- abandoned_spatial

abandoned_sub$address <- paste(abandoned_spatial$Address_Nu, abandoned_spatial$Street_Nam,
                                   abandoned_spatial$Suffix, sep=" ") 

abandoned_sub <- abandoned_sub %>%
    select(c("Zip_Code", "address", "Outcome_St","Structures","County_Tax","Code_Enfor"))




business_sub <- mutate(business_sub,
              classify = 
                  ifelse(
                      grepl("rest|food", business_points$Classifi_1, ignore.case=TRUE ), "RESTAURANT",
                      ifelse(
                          grepl("park", business_points$Classifi_1, ignore.case=TRUE ), "PARKING",
                          ifelse(
                              grepl("massage|tat", business_points$Classifi_1, ignore.case=TRUE ), "MASSAGE", 
                              ifelse(
                                  grepl("open|outdoor", business_points$Classifi_1, ignore.case=TRUE ), "OUTDOOR",
                                  ifelse(
                                      grepl("pet", business_points$Classifi_1, ignore.case=TRUE ), "PET",
                                      ifelse(
                                          grepl("chari|donat", business_points$Classifi_1, ignore.case=TRUE ), "CHARITY",
                                          ifelse(
                                              grepl("auto|repair", business_points$Classifi_1, ignore.case=TRUE ), "AUTOMOTIVE REPAIRS",
                                              ifelse(
                                                  grepl("taxi", business_points$Classifi_1, ignore.case=TRUE ), "TAXI/CAB",
                                                  ifelse(
                                                      grepl("hotel|motel", business_points$Classifi_1, ignore.case=TRUE ), "HOTELS",
                                                      ifelse(
                                                          grepl("pedd", business_points$Classifi_1, ignore.case=TRUE ), "PEDDLER",
                                                          ifelse(
                                                              grepl("trans", business_points$Classifi_1, ignore.case=TRUE ), "TRANSIENT MERCHANT",
                                                              ifelse(
                                                                  grepl("alarm", business_points$Classifi_1, ignore.case=TRUE ), "SECURITY COMPANY",
                                                                  ifelse(
                                                                      grepl("arbor", business_points$Classifi_1, ignore.case=TRUE ), "ARBORIST",
                                                                      ifelse(
                                                                          grepl("rubbish", business_points$Classifi_1, ignore.case=TRUE ), "GARBAGE REMOVAL",
                                                                          ifelse(
                                                                              grepl("metal", business_points$Classifi_1, ignore.case=TRUE ), "METAL DEALERS",
                                                                              ifelse(
                                                                                  grepl("vehicle", business_points$Classifi_1, ignore.case=TRUE ), "TOWING COMPANY",
                                                                                  ifelse(
                                                                                      grepl("laundry", business_points$Classifi_1, ignore.case=TRUE ), "LAUNDROMAT",
                                                                                      ifelse(
                                                                                          grepl("second", business_points$Classifi_1, ignore.case=TRUE ), "SECOND HAND DEALERS",
                                                                                          ifelse(
                                                                                              grepl("adult", business_points$Classifi_1, ignore.case=TRUE ), "ADULT BUSINESS",
                                                                                              "OTHERS") )))))))
                                                          )  )  ) ) )  ) )  ) ) ) ))

business_sub$id <- seq.int(nrow(business_sub))

abandoned_sub$id <- seq.int(nrow(abandoned_sub))

############################################################################

### Ankur

header <- dashboardHeader(
    title = "Dashboard"
) # end header

filter_s <- selectInput(inputId = "zipcode", 
                        label = "Choose Zip Code",
                        
                        #choices = business_spatial$Zip_Code,
                        choices = sort(unique(c(unique(park_data$Zip_Code), 
                                                unique(business_spatial$Zip_Code), 
                                                unique(abandoned_spatial$Zip_Code),
                                                unique(school_data$Zip_Code),
                                                unique(popAggregate$Zip_Code),
                                                unique(householdsAggregate$Zip_Code),
                                                unique(householdsFamily$Zip_Code)))), 
                        
                        selected=46617
                        
) # filter Zip Code


sidebar <- dashboardSidebar(
    filter_s,
    sidebarMenu(
        menuItem("Parks and Schools", tabName = "schools", icon = icon("map")
        ), #end menuItem Parks and School - Edith
        
        menuItem("Businesses", icon = icon("th"), tabName = "business"
        ), #end menuItem Businesses - Dana
        
        menuItem("Summary Data", icon = icon("table"),
                 menuSubItem("Parks", tabName = "park_table"),
                 menuSubItem("Businesses", tabName = "business_table")
        ), #end menuItem Summary Data - Ankur
        
        menuItem("About the Project", icon = icon("file-code-o"), tabName="about"
        ) # end menuItem About - Ankur
        
        
        
    ) # end sidebarmenu
) # end sidebar

body <- dashboardBody(
    tabItems(
        # Use the template from here for anything you want to add !!
        
        #### EDITH ##################
        
        tabItem("schools",
                fluidRow(
                    
                    # create map box  
                    box( height = 515,
                         title = "Schools and Parks",
                         status="warning", solidHeader = TRUE,
                         leafletOutput(outputId = "map"),
                    ), # end box
                    
                    # create selection box
                    box(height = 175,
                        # create school type selection  
                        selectInput(inputId = "stype",
                                    label = "Select School Type:",
                                    choices = c("Private", "Public")),
                        
                        # create park type selection
                        selectInput(inputId = "ptype",
                                    label = "Select Park Type:",
                                    choices = types)
                    ),
                    # create donut chart box
                    tabBox(height  = 300,
                           tabPanel("Family Type",
                                    plotOutput(outputId = "donut", height = 250)
                           )
                    )
                    
                ),
                
                fluidRow(
                    # create ethnicity distribution box
                    tabBox(height = 455,
                           tabPanel("Ethnicity Distribution",
                                    plotOutput(outputId = "barOne", height = 400)
                           )
                    ),
                    # create household distribution box
                    tabBox(
                        height = 455,
                        tabPanel("Household Distribution",
                                 plotOutput(outputId = "barTwo", height = 400)
                        )
                    )
                )
                
        ),# end tabItem
        
        
        # End parks and schools tab item Edith
        
        #### DANA ##################
        
        tabItem("business",
                fluidRow(
                    box(width = 2, 
                        radioButtons(inputId = "bus_type",
                                     label = "Choose Business Type",
                                     choices = c("All" = 'all', "Restaurant/Food Services" = "resto", 
                                                 "Parking" = 'parking', "Massage/Tatoo Parlors" = 'massage',
                                                 "Outdoor Venues" = 'outdoor', "Pet" = 'pet', 
                                                 "Non-profit" = 'donation', "Others" = 'other'),
                                     selected = "all")
                    ), # end box
                    
                    box(
                        title = "Map", solidHeader = T, width = 10,
                        status = "warning",
                        leafletOutput(outputId = "bus_map")
                        
                    )  # end box
                    
                ), # end fluidRow 1
                
                fluidRow(
                    box(
                        title = "Population Distribution by Age Range", status="primary",solidHeader = T, width = 6,
                        radioButtons(inputId = "age_choice", label = "",
                                     choices = c("Population Percentage" = "prop", "Population Value" = "pop" ),
                                     selected = "prop"),
                        uiOutput(outputId = "age_plot_or_warning")
                    ), # end box
                    
                    box(
                        title = "Population Distribution by Gender", status="primary", solidHeader = T, width = 6,
                        radioButtons(inputId = "fm_choice", label = "",
                                     choices = c("Population Percentage" = "prop", "Population Value" = "pop"),
                                     selected = "prop"),
                        uiOutput(outputId = 'gender_plot_or_warning')
                    )# end box
                    
                ) # end fluidRow 2
                
        ), # End business tab item Dana
        
        #### ANKUR ##################
        
        tabItem("park_table",
                fluidRow(
                    box(
                        title = "Map based on Data Selection ( Parks and Schools)", width=6,
                        status="warning", solidHeader = TRUE, collapsible = TRUE,
                        leafletOutput('x2', height=500)
                    ), # end box
                    tabBox(
                        height = 300,
                        tabPanel("Parks Data",
                                 DT::dataTableOutput("summarytable")
                        ),
                        tabPanel("School Data",
                                 DT::dataTableOutput("summarytable2")
                        )
                    )
                    
                    
                ), # end fluidRow
                fluidRow(
                    box(
                        title = "Ethnicity Data", width=6,status="primary", solidHeader = TRUE,
                        DT::dataTableOutput("ethnicitytable")
                    ),
                    box(
                        title = "Household Data", width=6,status="primary", solidHeader = TRUE,
                        DT::dataTableOutput("householdtable")
                    )
                ),# end box
                
        ), # park table - Ankur
        
        tabItem("business_table",
                fluidRow(
                    box(status="warning", solidHeader = TRUE, collapsible = TRUE,
                        title = "Map based on Data Selection ( Businesses and Abandoned Properties)", width=6,
                        leafletOutput('x3', height=400)
                    ), # end box
                    tabBox(
                        height = 300,
                        tabPanel("Age Distribution Data", 
                                 DT::dataTableOutput("agetable")
                        ),
                        tabPanel("Gender Distribution Data",
                                 DT::dataTableOutput("gendertable")
                        ),
                        tabPanel("Type of Businesses",
                                 DT::dataTableOutput("businesstype")
                        )
                    )
                    
                    
                ), # end fluidRow
                
                
                fluidRow(
                    tabBox(
                        height = 300, width=12,
                        tabPanel("Business Data",
                                 DT::dataTableOutput("business_summary")
                        ),
                        tabPanel("Abandoned Data",
                                 DT::dataTableOutput("abandoned_summary")
                        )
                    )
                ),# end box
                
        ), # business  - Ankur
        
        tabItem("about",
                fluidRow(
                    column(6, includeMarkdown("about.Rmd")
                    ), # end box
                    
                ), # end fluidRow
        )# About - Ankur
        
        
    )
)

ui <- shinyUI(
    dashboardPage(
        header,
        sidebar,
        body, skin=skin)
    
)

server <- function(input, output) {
    ## Edith
    
    
    # return school data based on inputs
    dataSchool <- eventReactive(list(input$zipcode, input$stype), {
        return(school_data%>%
                   filter(school_data$Zip_Code==input$zipcode, 
                          school_data$SchoolType==input$stype))
    })
    
    # return park data based on inputs
    dataParks <- eventReactive(c(input$zipcode, input$ptype),{
        return(park_data%>%
                   filter(park_data$Zip_Code == input$zipcode, 
                          park_data$Park_Type == input$ptype))
        
    })
    
    # return population data based on inputs
    zipOne <- eventReactive(input$zipcode, {
        return(popAggregate%>%
                   filter(Zip_Code == input$zipcode))
        
    })
    
    # return household data based on input
    zipTwo <- eventReactive(input$zipcode, {
        return(householdsAggregate%>%
                   filter(Zip_Code == input$zipcode))
        
    })
    
    # return household data based on input
    zipThree <- eventReactive(input$zipcode, {
        return(householdsFamily%>%
                   filter(Zip_Code == input$zipcode))
    })
    
    # output the school type and park type
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(data = dataSchool(), popup = ~popup) %>%
            addMarkers(data = dataParks(), popup = ~popup)
    })
    
    # plot household distribution
    output$barTwo <- renderPlot({
        zipTwo()%>%
            ggplot(aes(fct_reorder(householdType, totals), totals, fill = FamNonFam)) +
            geom_bar(stat = "identity") +
            theme(panel.background = element_blank()) +
            theme(axis.line.y = element_line(colour = "black")) +
            scale_fill_brewer(palette = "Pastel1") +
            geom_text(aes(label = totals, vjust = -.2, hjust = "center"),
                      position = position_dodge(0.90), angle = -90)+
            theme(axis.text.x = element_blank()) +
            theme(axis.ticks.x = element_blank()) +
            theme(text = element_text(size=15)) +
            xlab("") +
            ylab("") +
            theme(legend.position  = "none")+
            coord_flip()
    })
    
    # plot ethnicity distribution
    output$barOne <- renderPlot({
        zipOne()%>%
            ggplot(aes(fct_reorder(Race, percent), percent)) +
            geom_bar(stat = "identity", fill = "#CCEBC5", alpha = 0.6) +
            theme(panel.background = element_blank()) +
            theme(axis.line.y = element_line(colour = "black")) +
            geom_text(aes(label = paste0(percent,"%"), vjust = -.2, hjust = "center"),
                      position = position_dodge(0.90), angle = -90)+
            theme(axis.text.x = element_blank()) +
            theme(axis.ticks.x = element_blank()) +
            theme(text = element_text(size=15)) +
            xlab("") +
            ylab("") +
            coord_flip() 
    })
    
    # create donut chart
    output$donut <- renderPlot({
        zipThree()%>%ggplot(aes(ymax=ymax, ymin=ymin, xmax=10, xmin=2, fill=FamNonFam)) +
            geom_rect() +
            geom_label(x=5, aes(y=labelPositions,
                                label=label), size=5, color = "white") +
            scale_fill_brewer(palette = "Pastel1") +
            coord_polar(theta = "y") +
            xlim(c(-10, 10)) +
            theme_void()+
            theme(legend.position = "none")
    })
    
    #######################################################################
    
    ## Dana
    
    #### MAP for BUSINESS AND ABANDONED LOTS
    ## Subset business and abandoned data based on zip code input
    business_zip <- reactive({
        business_spatial %>% filter(Zip_Code == input$zipcode)
    })
    
    abandoned_zip <- reactive({
        abandoned_spatial %>% filter(Zip_Code == input$zipcode)
    })
    
    business_filter <- reactive({
        switch(input$bus_type,
               
               all = business_zip(),
               
               resto = business_zip() %>% filter(str_detect(str_to_lower(Classifi_1), "resta.*|food.*")),
               
               parking = business_zip() %>% filter(str_detect(str_to_lower(Classifi_1), ".*park.*")),
               
               massage = business_zip() %>% filter(str_detect(str_to_lower(Classifi_1), ".*massage.*|.*tatoo.*")),
               
               outdoor = business_zip() %>% filter(str_detect(str_to_lower(Classifi_1), ".*open.*|.*outdoor.*")),
               
               pet = business_zip() %>% filter(str_detect(str_to_lower(Classifi_1), ".*pet.*")),
               
               donation = business_zip() %>% filter(str_detect(str_to_lower(Classifi_1), ".*charitable.*|.*donation.*")),
               
               other = business_zip() %>% 
                   filter(str_detect(str_to_lower(Classifi_1), "resta.*|food.*|.*park.*|.*massage.*|
                                                              .*tatoo.*|.*open.*|.*outdoor.*|.*pet.*|
                                                              .*charitable.*|.*donation.*", 
                                     negate = T))
        ) #end switch
    })
    
    # Create map
    output$bus_map <- renderLeaflet({
        leaflet() %>% 
            
            addTiles() %>% 
            
            # User CartoDB.Position tile for easy view
            addProviderTiles(providers$CartoDB.Positron)  %>% 
            
            # Add markers for business
            addCircleMarkers(data = business_filter(),#business_zip(),
                             popup = ~popup,
                             stroke = F,
                             fillOpacity = 0.8,
                             color = "#1975d1",
                             radius= 3.1,
                             group = "Business") %>%
            
            # Add legend for business
            addLegend(data = business_filter(),#business_zip(),
                      labels = "Businesess",
                      colors = '#1975d1',
                      opacity = 1,
                      group = "Business") %>%
            
            # Add outline for abandoned properties
            addPolygons(data = abandoned_zip(),
                        popup = ~popup,
                        color = '#CC6666',
                        opacity = 0.5,
                        fillOpacity = 0.5,
                        group = "Abandoned Property") %>%
            
            # Add legend for abandoned properties
            addLegend(data = abandoned_zip(),
                      labels = "Abandoned Properties",
                      colors = '#CC6666',
                      opacity = 0.5,
                      group = "Abandoned Property") %>%
            
            # Add layer control
            addLayersControl(overlayGroups = c("Business", "Abandoned Property"),
                             options = layersControlOptions(collapsed = F)) 
        
    }) #end bus_map
    
    #### BAR GRAPH FOR AGE DISTRIBUTION
    ## Subset age data based on zip code
    age_zip <- reactive({
        
        age_filter <- pop_age_tidy %>% 
            filter(zipcode == input$zipcode) %>% 
            mutate(prop = round(population/sum(.$population) * 100, 2))
        
        return(age_filter)
    }) #end age_zip
    
    ## Switch between population and proportion 
    age_graph <- reactive({
        switch(input$age_choice,
               prop = plot_ly(age_zip(), x = ~age_range, y = ~prop, type = 'bar', color = I("#9999CC"),
                              text = ~paste("Age Range:", age_range, "<br>","Percentage:",prop, "%"),
                              hoverinfo = 'text') %>%  
                   layout(yaxis=list(title = "Population (%)")),
               
               pop = plot_ly(age_zip(), x = ~age_range, y = ~population, type = 'bar', color = I("#9999CC"),
                             text = ~paste("Age Range:", age_range, "<br>","Population:",population),
                             hoverinfo = 'text') %>%  
                   layout(yaxis=list(title = "Population")) 
               
        ) # end switch
        
    }) # end age_graph
    
    ## Display age plot or warning
    output$age_plot <- renderPlotly({
        age_graph() %>% layout(yaxis=list(titlefont = list(size = 13,
                                                           color ="navy"),
                                          tickfont = list (size = 12,
                                                           color = "navy"),
                                          gridcolor = "white",
                                          showgrid = T),
                               xaxis= list(title = "Age",
                                           titlefont = list(size = 13,
                                                            color ="navy"),
                                           tickfont = list (size = 12,
                                                            color = "#9999CC")),
                               bargap = 0.3)  
        
    }) # end age_plot
    
    output$age_warning <- renderText({
        paste("Age data not avaialable for Zip Code ", input$zipcode, ".")
    }) # end age_warning
    
    output$age_plot_or_warning <- renderUI({
        
        if(input$zipcode %in% pop_age_tidy$zipcode) {
            plotlyOutput("age_plot", height = 365)
            
        }
        else{
            textOutput("age_warning")
        }
        
    })# end age_plot_or_warning
    
    
    #### PIE GRAPH FOR GENDER DISTRIBUTION 
    ## Subset gender data based on zip code
    fm_zip <- reactive({
        
        fm_filter <- pop_fm_tidy %>%
            filter(zipcode == input$zipcode) 
        
        return(fm_filter)
    }) # end fm_zip
    
    ## Switch between population and proportion
    value_choice <- reactive({
        switch(input$fm_choice,
               
               pop = "label+value",
               prop = "label+percent"
        ) # end switch
    }) # end gender_graph
    
    ## Display gender plot or warning 
    
    output$gender_plot <- renderPlotly({
        
        plot_ly(fm_zip(), labels = ~gender, values = ~population, type = "pie",
                textposition = "inside",
                textinfo = value_choice(),
                insidetextfont = list(color = '#1975d1', size = 15),
                hoverinfo = "skip",
                marker = list(colors = c("#b7ebab","#ebc8ab"),
                              line = list(color = '#FFFFFF', width = 1.5)),
                showlegend = F) %>% 
            
            layout(yaxis=list(showgrid = F,
                              zeroline = F,
                              showticklabels = F),
                   xaxis= list(showgrid = F,
                               zeroline = F,
                               showticklabels = F))
        
    }) # end gender_plot
    
    output$gender_warning <- renderText({
        paste("Gender data no available for Zip Code ", input$zipcode, ".")
    })# end gender_warning
    
    output$gender_plot_or_warning <- renderUI({
        
        if(input$zipcode %in% pop_fm_tidy$zipcode) {
            plotlyOutput("gender_plot", height = 365)
        }
        else{
            textOutput("gender_warning")
        }
        
    })# end gender_plot_or_warning
    
    
    ######################################################################
    ## Ankur
    
    parks_sub <-  reactive({
        
        subset <- subset(parks.subset, Zip_Code == input$zipcode) 
    })
    
    school_sub <-  reactive({
        
        subset <- subset(dataSchool(), Zip_Code == input$zipcode) 
    })
    
    business_sub2 <-  reactive({
        
        subset <- subset(business_sub, Zip_Code == input$zipcode) 
    })
    
    abandoned_sub2 <-  reactive({
        
        subset <- subset(abandoned_sub, Zip_Code == input$zipcode) 
    })
    
    output$summarytable <- DT::renderDataTable({
        
        parks_sub2 <- parks_sub() %>% select(-c(Lat, Lon, id, Zip_Code))
        
        colnames(parks_sub2) <- c("Park Name","Park Type","Address")
        
        
        DT::datatable(parks_sub2, 
                      extensions=c('Responsive','Scroller','FixedHeader'),
                      selection="single", 
                      options=list(stateSave=TRUE, 
                                   fixedHeader=TRUE,
                                   scrollY=200,
                                   scroller=TRUE
                                 
                      ), filter="top") 
        
    }) # end summarytable
    
    output$summarytable2 <- DT::renderDataTable({
        
        data_sch <- dataSchool() %>% select (-c(popup, OBJECTID, Zip_Code, id)) %>%
            st_set_geometry(NULL)
        
        colnames(data_sch) <- c("School","Type of School")
        
        DT::datatable(data_sch, selection="single", filter="top", options=list(stateSave=TRUE,"pageLength"=5)) %>% formatStyle(
            'School', backgroundColor= styleInterval(3.4, c('gray','yellow'))
        )
    }) # end summarytable2
    
    output$ethnicitytable <- DT::renderDataTable({
        
        zipone_sub <- zipOne() %>% select(-c(Zip_Code, Totals))
        
        colnames(zipone_sub) <- c("Zip Code","Race","Total Population","Percentage")
        
        DT::datatable(zipone_sub, selection="none", filter="top", options=list(stateSave=TRUE,"pageLength"=5)) 
        
    }) # end ethnicity table
    
    output$householdtable <- DT::renderDataTable({
        
        ziptwo_sub <- zipTwo() %>% select(-c(Zip_Code))
        
        colnames(ziptwo_sub) <- c("Zip Code","Household Type","Family/Non Family","Total Population")
        
        DT::datatable(ziptwo_sub, selection="none", filter="top", options=list(stateSave=TRUE,"pageLength"=5)) 
        
    }) # end household table
    
    output$agetable <- DT::renderDataTable({
        
        age_zip_sub <- age_zip()
        
        colnames(age_zip_sub) <- c("Zip Code","Age Range","Population","Proportion of Population")
        
        DT::datatable(age_zip_sub, selection="none", filter="top", options=list(stateSave=TRUE,"pageLength"=5)) 
        
    }) # end age table
    
    output$gendertable <- DT::renderDataTable({
        
        fm_zip_sub <- fm_zip() 
        
        colnames(fm_zip_sub) <- c("Zip Code","Gender", "Population Number")
        
        DT::datatable(fm_zip_sub, selection="none", filter="top", options=list(stateSave=TRUE,"pageLength"=5)) 
        
    }) # end gender table
    
    output$businesstype <- DT::renderDataTable({
        
        
        
        business_type<- business_sub2() %>% 
            group_by (classify) %>%
            summarise(count = n() )
        
        colnames(business_type) <- c("Business Classification","Count of Businesses")
        
        
        
        DT::datatable(business_type, selection="none", filter="top", options=list(stateSave=TRUE,"pageLength"=5)) 
        
    }) # end gender table
    
    # to keep track of previously selected row
    
    prev_row <- reactiveVal()
    prev_row2 <- reactiveVal() # to track schools
    
    # new icon style
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
    my_icon2 = makeAwesomeIcon(icon = 'flag', markerColor = 'green', iconColor = 'blue')
    # for parks
    
    observeEvent(input$summarytable_rows_selected, {
        row_selected = parks_sub()[input$summarytable_rows_selected,]
        proxy <- leafletProxy('x2')
        print(row_selected)
        proxy %>%
            addAwesomeMarkers(popup=row_selected$Park_Name,
                              layerId = as.character(row_selected$id),
                              lng=row_selected$Lon, 
                              lat=row_selected$Lat,
                              icon = my_icon)
        
        # Reset previously selected marker
        if(!is.null(prev_row()))
        {
            proxy %>%
                addMarkers(popup=prev_row()$Park_Name, 
                           layerId = as.character(prev_row()$id),
                           lng=prev_row()$Lon, 
                           lat=prev_row()$Lat
                )
        }
        # set new value to reactiveVal 
        prev_row(row_selected)
    })
    
    observeEvent(input$summarytable2_rows_selected, {
        row_selected = school_sub()[input$summarytable2_rows_selected,]
        proxy <- leafletProxy('x2')
        print(row_selected)
        proxy %>%
            addPolygons(data=school_sub(), popup=row_selected$School,
                        layerId = as.character(row_selected$id),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 2,
                                                            bringToFront = TRUE))
        
        # Reset previously selected marker
        if(!is.null(prev_row2()))
        {
            proxy %>%
                addPolygons(data=school_sub(), popup=prev_row2()$School, 
                            layerId = as.character(prev_row()$id)
                            
                )
        }
        # set new value to reactiveVal 
        prev_row2(row_selected)
    })
    
    
    output$x2 <- renderLeaflet({
        
        pal <- colorFactor(palette = 'Set1', domain =parks_sub()$Park_Name)
        
        leaflet()  %>%
            addTiles()  %>%
            # Add markers for Parks
            addMarkers(data = parks_sub(),
                       popup = ~Park_Name,
                       layerId=as.character(parks_sub()$id),
                       #                #stroke = F,
                       #                 #fillOpacity = 0.8,
                       #                #radius=8,
                       group = "Parks") %>%
            
            
            
            
            # Add legend for Parks
            addLegend(data = parks_sub(),
                      labels = "Parks",
                      colors = 'steelblue',
                      opacity = 1,
                      group = "Parks") %>%
            
            # Add outline for schools
            addPolygons(data = dataSchool(),
                        popup = ~popup,
                        color = '#CC6666',
                        opacity = 0.5,
                        fillOpacity = 0.5,
                        group = "Schools") %>%
            
            # Add legend for Schools
            addLegend(data = dataSchool(),
                      labels = "Schools",
                      colors = '#CC6666',
                      opacity = 0.5,
                      group = "Schools") %>%
            
            # Add layer control
            addLayersControl(overlayGroups = c("Parks", "Schools"),
                             options = layersControlOptions(collapsed = F)) 
        
        
        
        
    })
    
    observeEvent(input$x2_marker_click, {
        clickId <- input$x2_marker_click$id
        dataTableProxy("summarytable") %>%
            selectRows(which(parks_sub()$id == clickId)) %>%
            selectPage(which(input$summarytable_rows_all == clickId) %/% input$summarytable_state$length + 1)
    })
    
    observeEvent(input$x2_marker_click, {
        clickId <- input$x2_marker_click$id
        dataTableProxy("summarytable2") %>%
            selectRows(which(school_sub()$id == clickId)) %>%
            selectPage(which(input$summarytable2_rows_all == clickId) %/% input$summarytable2_state$length + 1)
    })
    
    #### Events for Business Summary tabs.
    
    output$business_summary <- DT::renderDataTable({
        
        cols_to_keep <- c("Zip_Code", "Business_N", "Street_Add", "Business_P", "classify")
        
        business_table <- business_sub2() %>% select(cols_to_keep)
        
        colnames(business_table) <- c("Zip Code", "Business Name","Street Address","Business Phone", "Classification")
        
        business_table <- unique(business_table)
        
        
        DT::datatable(business_table[order(business_table$Classification),], 
                      extensions=c('Buttons','Responsive','Scroller','FixedHeader','RowGroup'),
                      selection="single", 
                      options=list(stateSave=TRUE, 
                                        fixedHeader=TRUE,
                                        deferRender=TRUE,
                                        scrollY=200,
                                         scroller=TRUE,
                                   autoWidth=TRUE,
                                   rowGroup=list(dataSrc=5),
                                  #"pageLength"=30, 
                                   dom='Bfrtip', 
                                  buttons=c('copy','csv','excel','pdf','print')
                                  ), filter="bottom") 
        
    }) # end business table
    
    output$abandoned_summary <- DT::renderDataTable({
        
        abandoned_table<- abandoned_sub2() %>% select (-c( Zip_Code, id)) %>%
            st_set_geometry(NULL)
        
        colnames(abandoned_table) <- c("Address","Outcome Status","Structure","County Tax ID","Code Enforcement")
        
        DT::datatable(abandoned_table, 
                      extensions=c('Buttons','Responsive','Scroller','FixedHeader'),
                      selection="single", 
                      options=list(stateSave=TRUE, 
                                   fixedHeader=TRUE,
                                   scrollY=200,
                                   scroller=TRUE,
                                   dom='Bfrtip', 
                                   buttons=c('copy','csv','excel','pdf','print')
                      ), filter="top") 
    }) # end abandoned table
    
    # to keep track of previously selected row
    
    prev_row3 <- reactiveVal()
    prev_row4 <- reactiveVal() # to track abandoned
    
    # new icon style
    my_icon3 = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
    my_icon4 = makeAwesomeIcon(icon = 'flag', markerColor = 'green', iconColor = 'blue')
    # for parks
    
    observeEvent(input$business_summary_rows_selected, {
        row_selected = business_sub2()[input$business_summary_rows_selected,]
        proxy <- leafletProxy('x3')
        print(row_selected)
        proxy %>%
            addAwesomeMarkers(popup=row_selected$Business_N,
                              layerId = as.character(row_selected$id),
                              lng=row_selected$X, 
                              lat=row_selected$Y,
                              icon = my_icon3)
        
        # Reset previously selected marker
        if(!is.null(prev_row3()))
        {
            proxy %>%
                addMarkers(popup=prev_row3()$Business_N, 
                           layerId = as.character(prev_row3()$id),
                           lng=prev_row3()$X, 
                           lat=prev_row3()$Y
                )
        }
        # set new value to reactiveVal 
        prev_row3(row_selected)
    })
    
    observeEvent(input$abandoned_summary_rows_selected, {
        row_selected = abandoned_sub2()[input$abandoned_summary_rows_selected,]
        proxy <- leafletProxy('x2')
        print(row_selected)
        proxy %>%
            addPolygons(data=abandoned_sub2(), popup=row_selected$Street_Nam,
                        layerId = as.character(row_selected$id),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 2,
                                                            bringToFront = TRUE))
        
        # Reset previously selected marker
        if(!is.null(prev_row4()))
        {
            proxy %>%
                addPolygons(data=abandoned_sub2(), popup=prev_row4()$Street_Nam, 
                            layerId = as.character(prev_row4()$id)
                            
                )
        }
        # set new value to reactiveVal 
        prev_row4(row_selected)
    })
    
    
    output$x3 <- renderLeaflet({
        leaflet() %>% 
            
            addTiles() %>% 
            
            # User CartoDB.Position tile for easy view
            addProviderTiles(providers$CartoDB.Positron)  %>% 
            
            # Add markers for business
            addCircleMarkers(data = business_zip(),
                             popup = ~popup,
                             stroke = F,
                             fillOpacity = 0.8,
                             radius=3,
                             group = "Business") %>%
            
            # Add legend for business
            addLegend(data = business_zip(),
                      labels = "Businesess",
                      colors = 'steelblue',
                      opacity = 1,
                      group = "Business") %>%
            
            # Add outline for abandoned properties
            addPolygons(data = abandoned_zip(),
                        popup = ~popup,
                        color = '#CC6666',
                        opacity = 0.5,
                        fillOpacity = 0.5,
                        group = "Abandoned Property") %>%
            
            # Add legend for abandoned properties
            addLegend(data = abandoned_zip(),
                      labels = "Abandoned Properties",
                      colors = '#CC6666',
                      opacity = 0.5,
                      group = "Abandoned Property") %>%
            
            # Add layer control
            addLayersControl(overlayGroups = c("Business", "Abandoned Property"),
                             options = layersControlOptions(collapsed = F)) 
        
    }) #end x3
    
    observeEvent(input$x3_marker_click, {
        clickId <- input$x3_marker_click$id
        dataTableProxy("business_summary") %>%
            selectRows(which(business_sub2()$id == clickId)) %>%
            selectPage(which(input$business_summary_rows_all == clickId) %/% input$business_summary_state$length + 1)
    })
    
    observeEvent(input$x3_marker_click, {
        clickId <- input$x3_marker_click$id
        dataTableProxy("abandoned_summary") %>%
            selectRows(which(abandoned_sub2()$id == clickId)) %>%
            selectPage(which(input$abandoned_summary_rows_all == clickId) %/% input$abandoned_summary_state$length + 1)
    })
    
    
    
    
    
} # end server

# Run the application 
shinyApp(ui = ui, server = server)

