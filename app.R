# DESCRIPTION: HENRY'S FORK FOUNDATION REAL-TIME WATER QUALITY WEBSITE
# Copyright (C) 2021  MELISSA MURADIAN AND CONTRIBUTORS (ZAC ESPINOSA AND JUSTIN APPLEBY)
# This program is free software: you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software Foundation, either version 3 of the 
# License, or any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

require(shiny)
require(shinydashboard) # Extra UI Features
require(leaflet)        # Use for Map
# install.packages('devtools')
require(xml2)
require(httr)
require(aws.s3)         # Use for upload/download
require(base)          
require(knitr)
require(xts)            # for zoo objects and functions
require(shinycssloaders)# Use for loading icons
require(scales)
# LINK TO ICONS: http://fontawesome.io/icons/ 

# Last Editted: 2-22-2021 Added disclaimer language
# Editted: Melissa Muradian
################################################################################
#### FUNCTIONS FROM SOURCE (www directory)
################################################################################
# Load function to create user-specified x-axis
source("www/createXAxis.R")
# Load function to derive necessary habitat thresholds
source("www/getHabContext_A.R")
################################################################################

################################
# A GLOBAL variable for plotting, sets transparancy
trans <- 1

###################################################################################################################
LOCATIONS <- c("AD"  = "Ashton Dam",
               "BC"  = "Buffalo Confluence",
               "BU"  = "Buffalo River",
               "CSF" = "Canyon South Fork",
               "FR"  = "Flat Rock",
               "HM"  = "Harriman",
               "IPE" = "Island Park Dam East",
               "IPW" = "Island Park Dam West",
               "LSF" = "Lower South Fork",
               "MY"  = "Marysville",
               "OR"  = "Ora Bridge",
               "PS"  = "Parker-Salem",
               "PH"  = "Pinehaven",
               "SA"  = "St. Anthony",
               "USF" = "Upper South Fork",
               "WA"  = "Warm River")

###################################################################################################################
LOCATIONCHOICES <- c("Select a new location",
                     "Ashton Dam (AD)",
                     "Buffalo Confluence (BC)",
                     "Buffalo River (BU)",
                     "Canyon South Fork (CSF)",
                     "Flat Rock (FR)",
                     "Harriman (HM)",
                     "Island Park Dam East (IPE)",
                     "Island Park Dam West (IPW)",
                     "Lower South Fork (LSF)",
                     "Marysville (MY)",
                     "Ora Bridge (OR)",
                     "Parker-Salem (PS)",
                     "Pinehaven (PH)",
                     "St. Anthony (SA)",
                     "Upper South Fork (USF)",
                     "Warm River (WA)")

####################################################################################################################
TEMPTIMES <- c("BC" = "from June 6, 2016 to August 16, 2016.",
               "HM" = "from June 29, 2018 to November 11, 2018.")

####################################################################################################################
MEANYEARS <- c("AD" = "2015:2019",
               "BC" = "not present, temporary site",
               "BU" = "2016:2019",
               "CSF" = "not present, not enough data yet",
               "FR" = "2014:2019",
               "HM" = "not present, temporary site",
               "IPE" = "2014:2019",
               "IPW" = "2016:2019",
               "LSF" = "not present, not enough data yet",
               "MY" = "2014:2019",
               "OR" = "from AD data 2015:2019", 
               "PS" = "2015:2019",
               "PH" = "2014:2019",
               "SA" = "2015:2019",
               "USF" = "not present, not enough data yet",
               "WA" = "2016:2019")

####################################################################################################################
DATATYPES <- c("Temperature" = "Temperature",
               "Dissolved Oxygen" = "DissolvedOxygen",
               "Turbidity" = "Turbidity", 
               "Chlorophyll a" = "Chloro",
               "Cyanobacteria" = "Cyano",
               "Conductivity" = "Conductivity")

####################################################################################################################
TIMEOPTIONS <- c( # "Last 24 Hours" = "lastDay",
                 "Last Month" = "lastMonth", 
                 "Last Six Months" = "lastSixMonths",
                 "Last Year" = "lastYear",
                 "Last Week" = "lastWeek",
                 # "Last Two Hours" = "lastTwoHours",   # Comment this option back in once more locations are automated
                 # "Full data span" = "full")
                 "Complete or Custom Dates" = "full")

###################################################################################################################
# MAP VARIABLES -- scope, site, and set-up
long1 <- c(-111.349024,-111.4485380,-111.424266,-111.778010,-111.455459,-111.650119,
          -111.329793)
long2 <- c(-111.395359,-111.392390,-111.395259,-111.396840)
long3 <- c(-111.510602,-111.508084)
long4 <- c(-111.656196,-111.910789,-111.243962)
lati1 <- c(44.497026,44.322109,44.098651,43.927105,44.291556,43.973157,44.113174)
lati2 <- c(44.413128,44.414551,44.416382,44.418022)
lati3 <- c(44.068075,44.071563)
lati4 <- c(43.611120,43.761910,43.373346)
popLocs1 <- c("Flat Rock", "Harriman", "Marysville", "Parker-Salem", "Pinehaven", "St. Anthony", "Warm River")
popLocs2 <- c("Buffalo Confluence", "Buffalo River", "Island Park Dam East", "Island Park Dam West")
popLocs3 <- c("AshtonDam", "OraBridge")
popLocs4 <- c("Canyon South Fork", "Lower South Fork", "Upper South Fork")
# popLocs4 <- c("Upper South Fork")
locAbbrev1 <- c("FR", "HM", "MY", "PS", "PH", "SA", "WA")
locAbbrev2 <- c("BC", "BU", "IPE", "IPW")
locAbbrev3 <- c("AD", "OR")
locAbbrev4 <- c("CSF", "LSF", "USF")
# locAbbrev4 <- c("USF")

##########################################################################
#############################  UI  #######################################
#############################  UI  #######################################
# The ui or user-interface of any webpage is the static website and all GUIs seen on that page. 
# The ui also creates the skeleton/structure of the site. In order to add tabs, creates box contents, 
# and set colors or fonts alter the UI. Additionally, each UI object has an ID parameter. The ID
# is typically the first parameter (note: if a param is not labeled, assume it to be ID). The ID
# is used to communicate with the SERVER section of the website.
ui <- dashboardPage(
  
  # Creates the Header: upper left corner is a link to HFF main website.
  # tags$a(href="https://henrysfork.org/", "Henry's Fork Foundation", style="color:white;")
  # Replace current Sidebar with one that has links: 
  #  HFF main website for upcoming events, staff, and merch;
  #  Current Water Quantity and Climate Website;
  #  Outreach and social media posts.
  dashboardHeader(title = "Henry's Fork Foundation Water Quality Data", titleWidth = 600),
  # uiOutput("hffLink"),
  
  # Creates the Header: upper left corner is a link to HFF main website.
  # tags$a(href="https://henrysfork.org/", "Henry's Fork Foundation", style="color:white;")
  
  # Each menuItem or menuSubItem corresponds to one tabItem in dashboardBody: tabName <-> tabItem
  dashboardSidebar(disable = TRUE),
  # dashboardSidebar(
  #   sidebarMenu(id="sideBar",                                                        
  #     menuItem("Water Quality", tabName = "map", icon = icon("line-chart")) # ,
    #   menuItem("Current Water Supply Report", tabName="report", icon=icon("newspaper-o")),
    #   menuItem("Water Quantity", tabName = "waterQuantity", icon = icon("area-chart"),
    #    menuSubItem("Henry's Lake", tabName="hf"),
    #    menuSubItem("Island Park Revervoir", tabName="ipr"),
    #    menuSubItem("Lower-watershed Streamflow", tabName = "rsf"),
    #    menuSubItem("Water Supply and Diversion", tabName="bc")
    #   ),
    #   menuItem("Climate", tabName = "climate", icon = icon("bar-chart"),
    #     menuSubItem("Weekly Summary", tabName="weekSum"),
    #     menuSubItem("Temperature", tabName="temp"),
    #     menuSubItem("Precipitation", tabName="prec"),
    #     menuSubItem("Snow Water Equivalent (SWE)", tabName="swe")
    #   ),
    #   menuItem("OutReach", badgeLabel="new", badgeColor = "green", tabName = "outreach", icon=icon("picture-o"))
  #    )
  # ),
  dashboardBody(
   
     tabItems(
       ################ Water Quality ################
       tabItem("map", class = "active",
        fluidRow(
          column(width = 6, offset=0,
            tags$img(src="RealtimeWQBanner 2.png", height="100%", width="100%"),  #height="100px", width="100%"), #,
            box(
              status="success", width=NULL, 
              leafletOutput(outputId = "m"),
              hr(),
              "Click a pin on the map to select a new location."
            ),
            htmlOutput("realTime"), # defined on line 866. (this chunk was on line 441 with a comma on line 440)
            fluidRow(
              # box(
              #   status="success", width=7, height="200px",
              #   div(style="display: inline-block; width: 100%;", selectInput(inputId = "timeOption", label = "Choose time", TIMEOPTIONS, selected = "Last Week")),
              #   div(style="display: inline-block; width: 100%; height: 50%;", 
              #       actionButton(inputId = "addLocations", label = "Overlay Location", inline=TRUE, width="50%", height="300px"),
              #       htmlOutput('qualityDownloadButton', inline=TRUE),
              #       plotOutput("locationsLegend", width="40%", height="80px", inline=TRUE),
              #       htmlOutput('addedLocation', inline=TRUE)
              #       ) # Creates Red/Black Legend
              # ),
              # htmlOutput("dateSpan") # Creates Date Range Input when user clicks custom dates
              ## new
              box(
                status="success", height="210px",
                div(style="display: inline-block; width: 100%;", selectInput(inputId = "timeOption", label = "Choose time period", TIMEOPTIONS, selected = "Last Month")),
                div(style="display: inline-block; width: 100%; height: 50%;", 
                    actionButton(inputId = "addLocations", label = "Add location to plot", inline=TRUE, width="50%", height="300px")) #,
                #br(), br(),
                # div(style="display: inline-block; width: 100%;", selectInput(inputId = "locationOption", label = "Choose location", LOCATIONCHOICES, selected = "IPE"))
                # Select locations doesn't do anything yet. Removed 6/15/2019
                ),
              htmlOutput("dateSpan"),
              box(
                status="success", height="150px",
                "See",strong(" Disclaimers"), " tab for data quality and usage information",
                # div(style="display: inline-block; width: 100%; height: 50%;",
                #     htmlOutput("dateSpan", width="100px", height="80px", inline=TRUE)), # Creates Date Range Input when user clicks custom dates
                div(style="display: inline-block; width: 100%; height: 50%;",
                    htmlOutput('qualityDownloadButton', inline=TRUE)),
                br(), br(),
                # div(style="display: inline-block; width: 100%; height: 50%;",
                #     plotOutput("locationsLegend", width="40%", height="80px", inline=TRUE)),
                div(style="display: inline-block; width: 100%; height: 50%;",
                    htmlOutput('qualityDownloadButton_2', inline=TRUE))
              )
              ## end new
              
            )
          ),
          column(width=6, offset=0,
              tabBox(
                width=NULL,
                tabsetPanel(id="tabs",
                  tabPanel( "Temperature", icon=icon("thermometer"),
                            fluidRow(
                              column( width=12, offset=0,
                                      box(status="success", width=NULL, #title = "Temperature", align="right",
                                          withSpinner(plotOutput("Tempg"), type = getOption("spinner.type", default=3), 
                                                      color.background = getOption("spinner.color.background", default="#ffffff"))
                                      ),
                                      htmlOutput('tabTemp')
                              ))),
                  tabPanel("Turbidity", icon=icon("tint"),
                           fluidRow(
                             column( width=12, offset=0,
                                     box(status="success", width=NULL, #title = "Turbidity",
                                         withSpinner(plotOutput('Turbidityg'), type = getOption("spinner.type", default=3), 
                                                     color.background = getOption("spinner.color.background", default="#ffffff"))
                                         ),
                                     htmlOutput('tabTurb')
                              ))),
                  tabPanel( "Dissolved Oxygen", icon=icon("cloud"),
                            fluidRow(
                              column( width=12, offset=0,
                                      box(status="success", width=NULL, #title = "Dissolved Oxygen",
                                          withSpinner(plotOutput('DissolvedOg'), type = getOption("spinner.type", default=3), 
                                                      color.background = getOption("spinner.color.background", default="#ffffff"))
                                      ),
                                      htmlOutput('tabDO')
                               ))),
                  tabPanel( "Conductivity", icon=icon("bolt"),
                            fluidRow(
                              column( width=12, offset=0,
                                      box(status="success", width=NULL, # title = "Conductivity",
                                          withSpinner(plotOutput('Conductivityg'), type = getOption("spinner.type", default=3), 
                                                      color.background = getOption("spinner.color.background", default="#ffffff"))
                                      ),
                                      box(status="success", width=NULL, 
        p("Conductivity refers to how easily electrical current passes through water and is directly related to the concentration of ions: more ions 
        in the water means higher conductivity. Salts and some inorganic compounds (nutrients) can dissolve in water, increasing the concentration of 
        ions. Thus water that is salty or has higher nutrient concentration has high conductivity and distilled water has low conductivity."),
        p("The Henry's Fork and the South Fork are low-nutrient cold water systems, so have relatively low baseline conductivity. In both rivers higher conductivity throughout
        the day or year in a particular location and higher conductivity downstream is correlated with higher water temperatures, which drive higher biological activity."))
                        )
                        )
                  ),
        tabPanel( "Phytoplankton", icon=icon("leaf"),
                  fluidRow(
                    column( width=12, offset=0,
                            box(status="success", width=NULL, 
                                withSpinner(plotOutput('Chlorog'), type = getOption("spinner.type", default=3), 
                                            color.background = getOption("spinner.color.background", default="#ffffff"))
                                ),
                            box(id="ChloroText", status="success", width=NULL, 
        p("Phytoplankton are photosynthetic micro-organisims that include green algae, diatoms, and cyanobacteria, among others. Phytoplankton are important 
        because they produce (the majority of Earth's) oxygen and form the foundation of aquatic food webs. Chlorophyll a is a specific pigment used during 
        photosynthesis, and is found inside of phytoplankton cells. Our sondes detect chlorophyll a inside of viable cells (plot above) and thus provide a 
        relative index of phytoplankton biomass in the river. A relative index is a set of data that increases and decreases in proportion to changes in the 
        target quantity, which is phytoplankton biomass in this case. Thus our continuous data provide measurements before, during, and after a phytoplankton
        bloom, which can provide indirect information about available nutrients, oxygen production, oxygen demand, and turbidity in the river."),
        p("If cyanobacteria are present in a bloom, an underestimate of total phytoplankton biomass may occur unless chlorophyll a biomass data is paired with 
        phycocyanin biomass data. Phycocyanin data and information can be found under the", strong("Cyanobacteria"), "tab."),
        p("Since initiating the HFF monitoring program on the Henry's Fork in 2014, phytoplankton data usually remain below 2 RFU. We have observed several 
        periods of elevated phytoplankton biomass. While none of these events posed a risk to fish in the river some did impact the visual clarity 
        or relative turbidity of the river. For more information about one such event, please read the following blogs:", 
        tags$a(href="https://henrysfork.org/recent-cyanobacteria-and-algae-blooms-island-park-reservoir-and-introduction-detected-shift-timing", 
        "July 2016 Cyanobacteria Bloom"), "and", tags$a(href="https://henrysfork.org/followup-july-cyanobacteria-bloom", "Follow Up")),
        p("Not all blooms are harmful! Some, but not all, cyanobacteria contain toxins that are harmful to mammals and our instruments do not distinguish 
        between toxic and non-toxic species. Thus, HFF's data are not to be used to determine whether a harmful algae bloom (HAB) is occurring. Further, 
        HFF does not accept any responsibility or liability for any decision made by reader that may lead to damage caused by a harmful algae bloom (HAB).  
        For current information on water-body health advisories connected to HABs in Idaho, please contact the", 
        tags$a(href="https://www.deq.idaho.gov/water-quality/surface-water/cyanobacteria-harmful-algal-blooms/", "Idaho Dept. of Water Quality."))
                            )))),
        tabPanel( "Cyanobacteria", icon=icon("leaf"),
                  fluidRow(
                    column( width=12, offset=0,
                            box(status="success", width=NULL, # title = "Chloro & Cyanobacteria",
                                withSpinner(plotOutput('Cyanog'), type = getOption("spinner.type", default=3), 
                                            color.background = getOption("spinner.color.background", default="#ffffff"))
                            ),
                            box(id="CyanoText", status="success", width=NULL, 
        p("Cyanobacteria are photosynthetic microscopic bacteria that are important to aquatic ecosystems for oxygen production and contribution to the food web. 
        Cyanobacteria are a member group of phytoplankton. Cyanobateria are NOT algae, but still are commonly known as \"blue-green algae\". Phycocyanin is a photosynthetic 
        pigment complex used by cyanobacteria, and is an accessory pigment to chlorophyll. Our sondes detect phycocyanin inside of viable cells (plot above) and thus 
        provide a relative index of suspended cyanobacteria biomass in the river. A relative index is a set of data that increases and decreases in proportion to changes 
        in the target quantity, which is cyanobacteria biomass in this case. Thus our continuous data provide measurements before, during, and after a phytoplankton bloom, 
        which can provide indirect information about available nutrients, oxygen production, oxygen demand, and turbidity in the river."),
        p("Since initiating the HFF monitoring program on the Henry's Fork in 2014, cyanobacteria values usually remain below 2 RFU. We have observed 
        several periods of elevated phytoplankton biomass. While none of these events posed a risk to fish in the river some did 
        impact the visual clarity or relative turbidity of the river. For more information about one such event, please read the following 
        blogs:", tags$a(href="https://henrysfork.org/recent-cyanobacteria-and-algae-blooms-island-park-reservoir-and-introduction-detected-shift-timing", 
        "July 2016 Cyanobacteria Bloom"), "and", tags$a(href="https://henrysfork.org/followup-july-cyanobacteria-bloom", "Follow Up")),
        p("Not all blooms are harmful! Some, but not all, cyanobacteria contain toxins that are harmful to mammals and our instruments do not distinguish 
        between toxic and non-toxic species. Thus, HFF's data are not to be used to determine whether a harmful algae bloom (HAB) is occurring. Further, 
        HFF does not accept any responsibility or liability for any decision made by reader that may lead to damage caused by a harmful algae bloom (HAB).  
        For current information on water-body health advisories connected to HABs in Idaho, please contact the", 
        tags$a(href="https://www.deq.idaho.gov/water-quality/surface-water/cyanobacteria-harmful-algal-blooms/", "Idaho Dept. of Water Quality."))
                                )))),
                  
                  tabPanel("Program", width=12,
                           fluidRow(
                             column(width=12, offset=0,
                                    box(status="success", width=12, title="Water Quality Monitoring Program Information",
                      p("The Henry’s Fork Foundation (HFF) initiated our comprehensive water-quality monitoring program in 2013 to monitor 
        chemical and biophysical conditions in the Henry's Fork. The goal of the program is to collect background data, assess water quality 
        factors relevant for fish, and understand ecological processes such as sediment and nutrient dynamics. HFF aims to collect data on 
        water quality parameters to gain a better understanding of rivers, including the Henry’s Fork and South Fork of the Snake River, with 
        the hope of influencing management strategies to benefit fisheries health across eastern Idaho, not to use or see the data used for 
        any regulatory purposes."), 
        p("The monitoring program includes collection of continuous sonde data and weekly or monthly water sample data. Our continuous 
        sonde data are shown on this website. 'Sondes' are high-tech, rugged computers that measure and record data on various water 
        quality attributes. We use YSI EXO2 multi-parameter sondes configured to collect measurements every 15 minutes of temperature, 
        turbidity, dissolved oxygen, conductivity, and an index of phytoplankton biomass. From each sonde site, we also collect and test 
        water samples for nutrients, suspended sediment concentration, and volatilized suspended sediment concentration, all of which 
        cannot be measured by the sondes. We hope to provide our nutrient and sediment data on this website soon."),
        p("Network coverage as well as each sonde's location was designed to capture the influence on water quality of each river reach 
        and major structure or tributary, setting us up to be able to answer a broad range of current and future research questions. 
        Below is a brief description of the significance of each sonde's position in the watershed and its collection horizon."),
        p(h4(strong("Henry's Fork Network"))),
        p(strong("Flat Rock (FR)"), " sonde captures river conditions downstream of Henry's Lake Outlet and upstream of Island Park reservoir. When 
        paired with data from IPE or IPW, these data provide some information on the influence of Island Park reservoir on downstream water 
        quality. This sonde has been operating from June 29, 2014 to the present."),
        p(strong("Island Park Dam East (IPE)"), "sonde is immediately downstream of Island Park reservoir on the eastern bank. When both outflow gates 
        are being used, this sonde captures water quality of outflow from the hydropower plant operated by Fall River Rural Electric. This sonde has 
        been operating from June 20, 2014 to the present."),
        p(strong("Island Park Dam West (IPW)"), "is immediately downstream of Island Park reservoir on the western bank. When both outflow gates are being  
        used, IPW sonde captures water quality of outflow from the original U.S. Bureau of Reclamation discharge gate. This sonde has been operating from 
        December 23, 2015 to the present."),
        p(strong("Buffalo River (BU)"), "sonde captures water quality just upstream of Fall River Rural Electric's hydroelectric diversion, which 
        is near the confluence with the Henry's Fork. This sonde has been operating from May 25, 2016 to the present."),
        p(strong("Buffalo Confluence (BC)"), "sonde was operated on short term from June 6, 2016 to August 16, 2016."),
        p(strong("Harriman Ranch (HR)"), "sonde was operated on short term from June 29, 2018 to November 7, 2018."),
        p(strong("Pinehaven (PH)"), "sonde captures the impact on water quality of the morphological changes to the river that occur near upstream of 
        the famous Harriman Ranch reach---the river changes from rapid and canyon dominated (through Box Canyon) to wide, shallow, and flat through the 
        Ranch to Pinehaven. This sonde has been operating from June 20, 2014 to the present."),
        p(strong("Warm River (WA)"), "sonde captures water quality of this main tributary just upstream of its confluence with the Henry's Fork. This 
        sonde has been operating from 2016 to the present."),
        p(strong("Marysville (MY)"), "sonde is downstream of Mesa Falls and the Warm River confluence, and upstream of Ashton reservoir---a reach that is 
        the least impacted by reservoir management or irrigation diversion/return flow on the mainstem Henry's Fork. This sonde has been operating from 
        July 7, 2014 to the present."),
        p(strong("Ora Bridge (OR)"), "sonde was installed March 29, 2019, just prior to the beginning of construction work to replace Ora Bridge. Data from this sonde provides the 
        upstream information that we pair with data from the AD sonde (downstream of Ora Bridge) to observe any impacts on water quality from the bridge construction."),
        p(strong("Ashton Dam (AD)"), "sonde is immediately downstream of Ashton reservoir to capture this reservoir's influence (and impact of the recent Ora 
        Bridge construction) on downstream water quality. Note that Ashton reservoir maintains run-of-the-river, so has minimal impact on water quality compared 
        to Island Park reservoir. This sonde has been operating from July 30, 2015 to the present."),
        p(strong("St. Anthony (SA)"), "and", strong("Parker-Salem (PS)"), "sondes are located at the upstream transition to the Snake River 
        Plain and thus capture the influence on water quality of irrigation diversion and return flow. These sondes have been operating from 
        August 3, 2015 to the present."),
        p(h4(strong("South Fork Network (South Fork Initiative)"))),
        p("Due to the connection of water and fisheries management between the Henry's Fork and South Fork Snake River, the Henry's Fork Foundation expanded its 
          approach to science and collaboration to the South Fork Snake River through the South Fork Initiative program. To learn more about this program visit 
          the SFI Webpage: ", tags$a(href="https://henrysfork.org/south-fork-initiative-0", "https://henrysfork.org/south-fork-initiative-0")),# "the SFI Webpage.")), # link defined line 555
        p(strong("Upper South Fork (USF)"), "sonde is below Palisades Dam but above the confluence of Palisade Creek (first major tributary on the SF) and captures
        the influence of Palisades Reservoir on downstream water quality."),
        p(strong("Canyon South Fork (CSF)"), "sonde is below the four major South Fork tributaries (Palisades Cr., Rainey Cr., Pine Cr., & Burns Cr.) and captures 
          their influence on the South Fork prior to major irrigation diversions."),
        p(strong("Lower South Fork (LSF)"), "sonde is below Lorenzo and captures South Fork water quality conditions below major irrigation diversions in the lower river.")
                             )))),
                  tabPanel("Disclaimers", width=12,
                           fluidRow(
                             column(width=12, offset=0,
                                    box(status="success", width=12, title="Data Usage Disclaimer",
                      p("The Henry’s Fork Foundation makes the information on this website available to the public as a service to its members and 
          other watershed stakeholders, water users, educators, and water-resource planners and managers. We regularly update products and modeling 
          methods to provide current information most relevant to these user groups. HFF makes every effort to provide accurate information. However, 
          neither HFF nor any other party affiliated with HFF represents or assures that the water quality information provided on the website or in 
          other HFF communications are in every respect complete and accurate. Thus, HFF, its members, staff or any other party affiliated with HFF 
          are not responsible for any errors or omissions in the data collected by HFF and on this website. Users of information on the HFF data website 
          are cautioned to consider carefully the provisional nature of the information before using it. Neither HFF nor its individual staff members 
          assume any legal responsibility for the accuracy or completeness of this information or for how it is used and interpreted by third parties."),
                      p("In consideration of the fact that HFF is providing and compiling data only for the public benefit, the reader acknowledges and 
          agrees that, to the fullest extent permitted by law, HFF, its members, staff and any other people affiliated with HFF:"),
                      p(HTML('&emsp;'),"1. Do not accept any responsibility or liability for the accuracy or inaccuracy of the data contained in this website."),
                      p(HTML('&emsp;'),"2. Do not accept any responsibility or liability for interpretations or decisions made by the reader after viewing the data.")),
                                    box(status="success", width=12, title="Data Quality Disclaimer",
                      p("As HFF is committed to transparent, public data and methods, this website provides our water quality data for public 
          consumption. The data shown here have been cleaned using an automated algorithm developed by HFF staff specific to the types of data 
          we collect on the Henry's and South Forks of the Snake River, but the cleaned data require additional review and analysis by staff and 
          therefore are not considered final and correct until this is done. Most data shown here are considered provisional and subject to revision. 
          If you need publishable data please contact Melissa Muradian at melissa@henrysfork.org."),
                      p("Real-time data are automatically uploaded to this website every two hours. As of winter 2020/2021, the following sites 
        automatically transmit data in real time: Flat Rock (FR), Island Park Dam East (IPE), Island Park Dam West (IPW), Pinehaven (PH), Marysville 
        (MY), Ora Bridge (OR), Ashton Dam (AD), St. Anthony (SA), Parker-Salem (PS), Upper South Fork (USF), and Lower South Fork (LSF). 
        Data from the remaining sondes must be manually downloaded before being displayed here. Regular manual downloading occurs once a month 
        during late fall to early spring and every other week otherwise."),
                      p("Each location's time series contain brief and/or extended periods of missing data when the sonde was removed from the river. 
        All of our sondes are regularly removed for maintenance and calibration. Additionally, most of our sondes are removed during the winter months 
        when conditions are outside of the operating range of the instruments or make data unreliable."),
                      p("It took our organization six years to raise enough money to support our current water-quality monitoring program. These 
        funds cover equipment as well as staff time spent on installation, maintenance, research, and development; all of which provide you the 
        opportunity to freely interact with our continuous water-quality data via this website. The cost of implementing each sonde with automatic 
        real-time data transmission is high and, as more funds become available, we will outfit more sites with real-time capability.")
                             )))),
                  tabPanel("Help", width=12,
                           fluidRow(
                             column(width=12, offset=0,
                                    box(status="success", width=12, title="Website Instructions" ,
                        p("This website provides water quality data for public consumption. These data are from Henry's Fork Foundation's network of 
         sondes. For information about our water-quality monitoring program, please click on the", strong("Program"), "tab above. For information
         about data quality and usage disclaimers, please click on the", strong("Disclaimers")," tab above."),
                       p("The default data to interact with is from Island Park Dam East (IPE). Click a tab on the interactive watershed map to select 
         data from a new location. The default timeframe of data to interact with is the last month of data. Select from 
         the", strong("Choose time"), "dropdown menu below the map to select a new time horizon of data to view."),
                       p("You can also compare data from two different locations in the same plot by using the", strong("Add location to plot"), "button 
         below the map. Be sure to check the plot title/legend for information on the data you are viewing."), p("By clicking the", strong("Download"), 
         "button below the watershed map, you can download a file of the data you are currently viewing. Most data are considered provisional; for more 
         information please read the Data Quality Disclaimer on the", strong("Disclaimers")," tab. If you need publishable data please contact Melissa 
         Muradian at melissa@henrysfork.org."))

                             )))
                )
              ) #,
        # htmlOutput("realTime")
          )
        ),
        fluidRow(
          box(
            width = NULL, background = "olive",
            "We want your feedback! \n Please email questions, concerns, or comments to melissa@henrysfork.org or jamie@henrysfork.org \n",
            uiOutput("hffLink") # defined line 547, link to hff website
          )
        ) # I use this bottom box as my "footer"
      ) # tabItem("map"...)
     ) # tabsItems
    ) # daashboardBody
) # daashboardPage

#################################################################################
################################# SERVER ########################################
################################  SERVER ########################################
#################################################################################
server <- function(input, output) {
  ######### tag url links ########################
  # HFF Webpage
  urlHFF <- tags$a(href="https://henrysfork.org/", "Henry's Fork Foundation | The Voice of the River",  
                style="color:white;")
  output$hffLink <- renderUI({
    tagList(urlHFF)
  }) # used line 535
  # SFI Webpage
  # urlSFI <- tags$a(href="https://henrysfork.org/south-fork-initiative-0", "the SFI Webpage.")
  # output$sfiLink <- renderUI({
  #   tagList(urlSFI)
  # }) # used line 482
  ## Turbidity blogs
  urlT1 <- tags$a(href="https://henrysfork.org/high-turbidity-and-sediment-event-during-memorial-day-weekend", 
                  "The Great Spring Sediment Flush of 2018 (June 8, 2018)")
  output$turb1 <- renderUI({
    tagList(urlT1)
  })  # uiOutput("turb1") line 306
  urlT2 <- tags$a(href="https://henrysfork.org/35th-anniversary-lessons-learned-part-6-water-quality", 
                  "HFF 35th Anniversary Lessons Learned: Water Quality (March 28, 2019)")
  output$turb2 <- renderUI({
    tagList(urlT2) # line 307
  })
  urlT3 <- tags$a(href="https://henrysfork.org/great-spring-2018-sediment-flush-continues-impact-water-quality-and-fishing-experience", 
                  "The Great Spring Sediment Flush Continues to Impact Fishing (August 1, 2018)")
  output$turb3 <- renderUI({
    tagList(urlT3) # line 308
  })
  urlT4 <- tags$a(href="https://henrysfork.org/turbidity-perspective", 
                  "Turbidity in Perspective (April 18, 2016)")
  output$turb4 <- renderUI({
    tagList(urlT4) 
  })
  urlT5 <- tags$a(href="https://henrysfork.org/blog-response-flow-transfer-july-15-2018", 
                  "Water Quality Response to IP Dam Flow Transfer (July 15, 2018)")
  output$turb5 <- renderUI({
    tagList(urlT5) 
  })
  urlT6 <- tags$a(href="https://henrysfork.org/hff-launches-new-investigation-turbidity", 
                  "Turbidity Persistence Test: Background and Motivation (Sept 9, 2016)")
  output$turb6 <- renderUI({
    tagList(urlT6) 
  })
  urlT7 <- tags$a(href="https://henrysfork.org/turbidity-persistence-test-conclusions-2016-data", 
                  "Turbidity Persistence Test: Conclusions (December 8, 2016)")
  output$turb7 <- renderUI({
    tagList(urlT7) 
  })
  
  ######## CUSTOM FUNCTION DEFINITIONS ###########
  
  getFullData <- function(a = input$dateRangeMain, cleanSondeDat = cleanSondeDat){
    cat(file=stderr(), "Inside getFullData, input$dateRangeMain is", a, 
        "\n length of cleanSondeDat is", length(cleanSondeDat[,1]), "\n")
    startDate <- as.POSIXct(paste0(as.character(a[1]), " 00:00:00"), format='%Y-%m-%d %H:%M:%S', tz="UTC")
    endDate <- as.POSIXct(paste0(as.character(a[2]), " 00:00:00") , format='%Y-%m-%d %H:%M:%S', tz="UTC")
    cleanSondeDat <- subset(cleanSondeDat, (as.POSIXct(cleanSondeDat$contTime) >= startDate & as.POSIXct(cleanSondeDat$contTime) <= endDate))
    cat(file=stderr(), "Returning length of cleanSondeDat is", length(cleanSondeDat[,1]),"\n")
    return(cleanSondeDat)
  }
  
  # Called by each attribute plot in case no data to display
  writeDataNA <- function(){
    cat(file=stderr(), "Inside writeDataNA() \n")
    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])/2, # the middle of any x-axis 
         par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, # the middle of any y-axis 
         "Please select an earlier time frame from the \n 'Choose time period' dropdown menu. \n \n Data are not available for this period; 
         read the 'Real Time Status' message under map. ",
         cex = 1.5)
  }
  
  # writeComingSoon() Added as place holder for SF sondes, called by each attribute plot
  writeComingSoon <- function(){
    cat(file=stderr(), "Inside writeComingSoon() \n")
    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])/2, # the middle of any x-axis 
         par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, # the middle of any y-axis 
         "Canyon South Fork sonde data coming soon! \n \n Please select a different site for now.",
         cex = 1.5)
  }
  
  writeSelectDates <- function(){
    text(par("usr")[1]+(par("usr")[2]-par("usr")[1])/2, # the middle of any x-axis 
         par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, # the middle of any y-axis 
         "To customize dates, please select beginning and ending dates from the dropdown menu")
  }
  
  # getLocationDates takes the full data file and retuns a vector length 2 of first and last 
  # date. Called by output&dateSpan
  getLocationDates <- function(location) {
    cat(file=stderr(), "Inside getLocationDates() \n")
    objectName <- paste0(location, "/", location, "_full.Rdata")
    s3load(object = objectName, bucket = "hffwq")
    # print(data_full) commented 3-26-2018 - not sure why this was here
    df <- eval(parse(text = "data_full"))
    startDate <- strsplit(as.character(df$contTime[1]), split=" ")
    startDate <- startDate[[1]][1]
    endDate <- strsplit(as.character(df$contTime[nrow(df)]), split=" ")
    endDate <- endDate[[1]][1]
    return(c(startDate,endDate))
  } 
  
  # Helper Function used to add second location to any graph in water quality section
  # myNewData loads and returns dataframe associated with new location and timeOption
  # myNewData <- function(location, timeOption) {
  #   cat(file=stderr(), "Inside myNewData() \n")
  #   # input$updateDatesButton
  #   objectName <- paste0(location, "/", location, "_", timeOption, ".Rdata")
  #   s3load(object = objectName, bucket = "hffwq")
  #   dfName <- paste0("data_", timeOption)
  #   df <- eval(parse(text = dfName))
  #   
  #   if (input$timeOption == "full") {
  #     startTime <- as.POSIXct(paste0(input$dateRangePrev[1], " 00:00:00"), format='%Y-%m-%d %H:%M:%S', tz="UTC")
  #     endTime <- as.POSIXct(paste0(input$dateRangePrev[2], " 00:00:00"), format='%Y-%m-%d %H:%M:%S', tz="UTC")
  #     df <- subset(df, (as.POSIXct(contTime) >= startTime & as.POSIXct(contTime) <= endTime))
  #   }
  #   return(df)
  # }
  
  #################### SETUP: DIRECTLY COPIED FROM MELISSA'S getHabContext.R CODE ########################
  # Helper function used to create pars and process data for Water Quality graphics:
  # Some values have been updated in order to be compatible with Shiny Apps program style
  # setUp <- function(justData, cleanSondeDat){
  setUp <- function(cleanSondeDat){
    cat(file=stderr(), "Inside setUp() \n")
    envir = new.env(parent=globalenv())
    
    # z used to calculate average daily/weekly temperature to determine DO requirements
    # z <- zoo(x = justData$Temp_C, order.by = as.POSIXlt(justData$contTime, tz="UTC"))
    # z <- zoo(x = cleanSondeDat$Temp_C, order.by = cleanSondeDat$contTime)
    z <- zoo(x = cleanSondeDat$Temp_C, order.by = as.POSIXlt(cleanSondeDat$contTime, tz="UTC"))
    avgz <- zoo(x = cleanSondeDat$Temp_avdymn, order.by = as.POSIXlt(cleanSondeDat$contTime, tz="UTC"))
    
    # DERTERMINE OPTIMAL GRIDLINES AND LABELS ON X-AXIS
    divs <- c(3,7,14,30)
    # days <- unique(format(as.POSIXlt(cleanSondeDat$contTime,format='%Y-%m-%d %H:%M:%S', tz="UTC"),format='%Y-%m-%d', tz="UTC"))
    days <- unique(as.Date(cleanSondeDat$contTime))
    ndays <- length(days)
    i <- 1
    nlabels <- ndays/divs[i]
    while(  (nlabels > 7) & (i < 4) ){
      i <- i + 1
      nlabels <- ndays/divs[i]
    }
    #
    # Need 2 cases: 1 where labels are month and day; 1 where labels are just month
    if(divs[i] == 30){ # just months
      # months <- unique(format(as.POSIXlt(cleanSondeDat$contTime,format='%Y-%m-%d %H:%M:%S', tz="UTC"),format='%Y-%m', tz="UTC"))
      months <- unique(format(cleanSondeDat$contTime, format='%Y-%m', tz="UTC"))
      newMonths <- lapply(months,function(x) paste0(x, "-01"))
      newMonths <- format(newMonths, format="%Y-%m-%d", tz="UTC")
      newMonths <- as.POSIXct(newMonths, tz="UTC", format="%Y-%m-%d")
      newMonYrs <- format(newMonths, "%Y") # Try this
      halfMonths <- lapply(months,function(x) paste0(x, "-15"))
      halfMonths <- format(halfMonths, format="%Y-%m-%d", tz="UTC")
      halfMonths <- as.POSIXct(halfMonths, tz="UTC", format="%Y-%m-%d")
      halfMonYrs <- format(newMonths, "%Y") # Try this
      
      envir$at.labels <- c(newMonths, halfMonths)
      envir$names.labels <- format(as.POSIXlt(c(newMonths,halfMonths), format='%Y-%m-%d %H:%M:%S', tz="UTC"), "%b-%d")
      envir$namesYrs.labels <- c(newMonYrs, halfMonYrs)
      # envir$v.lines <- as.POSIXct(as.POSIXlt(seq.Date(newMonths[1], days[ndays], by = 7), tz = "UTC"))
      # zAvgTemp <- apply.weekly(as.xts(z), colMeans, na.rm=T) # use average weekly temp
      
      # for(i in 3:length(months))
      #  # print(envir$at.labels)
      #  # print(envir$names.labels)
      #  envir$at.labels <- c(envir$at.labels, cleanSondeDat$contTime[which(cleanSondeDat$contTime == months[i])])
      #  envir$names.labels <- format(as.POSIXlt(envir$at.labels, format='%Y-%m-%d %H:%M:%S', tz="UTC"), "%b")
      
    }else{ # month and day
      # envir$at.labels <- as.POSIXct(as.POSIXlt(seq.Date(as.Date(days[2]), as.Date(days[ndays]), by = divs[i]), tz = "UTC")) # so dumb!
      # envir$at.labels <- as.POSIXct(as.POSIXlt(seq.Date(days[2], days[ndays], by = divs[i]), tz = "UTC"))
      envir$at.labels <- as.POSIXct(as.POSIXlt(seq.Date(days[2], days[ndays], by = 1), tz = "UTC"))
      envir$names.labels <- format(envir$at.labels, "%b-%d")
      envir$namesYrs.labels <- format(envir$at.labels, "%Y")
      
      # zAvgTemp <- apply.daily(as.xts(z), colMeans, na.rm=T) # use average daily temp
    }
    # Get habitat thresholds, derived by species, time of year, and current temperature
    zF <- z 
    coredata(zF) <- coredata(zF) * 9/5 + 32
    # head(zF); tail(zF)
    envir$zF <- zF
    avgzF <- avgz 
    coredata(avgzF) <- coredata(avgzF) * 9/5 + 32
    # head(avgzF); tail(avgzF)
    envir$avgzF <- avgzF
  envir$l <- getHabContext(z = z, zAvgDMn = avgz, data.dir = rv$location) #data.dir = data.dir) data.dir = r$location in this case 
  #  envir$l <- getHabContext(z = z, data.dir = rv$location) #data.dir = data.dir) data.dir = r$location in this case 
    envir$l$ysetC <- envir$l$yset
    envir$l$ysetF <- lapply(envir$l$yset, celsiusToFahrenheit) # Y axis in Fahrenheit
    return(envir)
  }
  
  # Convert from C -> F (temp)
  celsiusToFahrenheit <- function(c) {
    return(c * 9/5 + 32)
  }
  #################################################################################
  
  ###### Reactive Values #######
  # Note: Any function in server that uses a reactive value will be called when one of these 
  # values is updated by user action. Therefore, in addition to UI reactive values, I manually 
  # declare these to use throughout server. 
  rv <- reactiveValues(
    addnlLocation = NULL,
    location = "IPE",
    addLocations = FALSE #### Used to Add location to plot 
  )
  #################################################################################
  
  ###### Reactive Expressions #######
  # MLM wrote this 4-6-2018. loading of data used to occur each time a parameter 
  # plot was generated. Now data is only downloaded when timeframe changes (within the 
  # same location) and caching is automatically implemented.
  dat <- reactive({
    cat(file=stderr(), "Inside reactive dat() \n rv$location is",
        rv$location, "and rv$timeOption is", input$timeOption,"\n")
    objectName = paste0(rv$location, "/", rv$location, "_", input$timeOption, ".Rdata")
    s3load(object = objectName, bucket="hffwq")
    dfName = paste0("data_", input$timeOption)
    dat <- eval(parse(text = dfName))
    if (input$timeOption == "full") {
      input$updateDatesButton
      # function getFullData() definition on 470
      dat <- getFullData(a = isolate(input$dateRangeMain), dat)
    }
    return(dat)
  })
  # 
  # Code below used to be function definition for myNewData, now on line 511. Was rewritten here
  # 6/15/2019 as a reactive function to be more efficient and automatically implement caching.
  addnlDat <- reactive({
    cat(file=stderr(), "Inside reactive addnlDat() \n rv$addnlLocation is",
        rv$addnlLocation, "and rv$timeOption is", input$timeOption,"\n")
    # input$updateDatesButton
    objectName <- paste0(rv$addnlLocation, "/", rv$addnlLocation, "_", input$timeOption, ".Rdata")
    s3load(object = objectName, bucket = "hffwq")
    dfName <- paste0("data_", input$timeOption)
    addnlDat <- eval(parse(text = dfName))
    if (input$timeOption == "full") {
      addnlDat <- getFullData(a = isolate(input$dateRangeMain), addnlDat)
    }
    return(addnlDat)
  })
  #################################################################################
  
  ######### Observers ##########
  # If the reactive value input$addLocations changes, update rv$addLocations: Boolean used to add multiple locations 
  observeEvent(input$addLocations, {
    if (rv$addLocations == FALSE) { rv$addLocations = TRUE }
    else { rv$addLocations = FALSE }
  })
  
  # If reactive value input$m_marker_click changes, update locations: Used in Water Quality graphing functions
  observeEvent(input$m_marker_click, {
    rv$addnlLocation = rv$location
    rv$location = input$m_marker_click$id
  })
  #################################################################################
  
  ####### ADDING TO THE OUTPUT LIST #######
  output$dateSpan <- renderUI ({
      if (input$timeOption == "full") {
        cat(file=stderr(), "Inside output$dateSpan() \n")
        dateRange <- getLocationDates(rv$location) # function definition line 347
        cat(file=stderr(), "dateRange is", dateRange, "\n")
        box(status="success", width=5, 
          dateRangeInput("dateRangeMain", LOCATIONS[rv$location], format="yyyy-mm-dd",
            start=dateRange[1], end=dateRange[2], min=dateRange[1], max=dateRange[2]),
          if (rv$addLocations == TRUE && !is.null(rv$addnlLocation)) {
            dateRange <- getLocationDates(rv$addnlLocation)
            dateRangeInput("dateRangePrev", LOCATIONS[rv$addnlLocation], format="yyyy-mm-dd",
              start=dateRange[1], end=dateRange[2], min=dateRange[1], max=dateRange[2])
          },
          actionButton(inputId = "updateDatesButton", label = "Update"),
          cat(file=stderr(), "Returning out of output$dateSpan() \n")
          # I declare this here and not in the ui (eventhough I know this is inefficient) so that 
          # the update button only appears when the html block with the dateRangeInput calendars 
          # loads (after user has selected "custom dates").
          # This action button doesn't do anything yet... I think I need to add it to
          #  the output list below, like qualityDownloadButton
        )
        # cat(file=stderr(), "Returning out of output$dateSpan() \n") 
        # a comment here prevents tempg from being called twice (I still don't know why it IS called twice),
        # which prevents input#dateRangeMain from getting filled...
      }
  })
  
  ########### Create data download buttons ################
  output$qualityDownloadButton <- renderUI ({
    downloadButton('qualityDownload_1', paste0("Download ", rv$location, " Data"), width="50%")
  }) #NOTE: downloadButton is a Shiny function
  output$qualityDownload_1 <- downloadHandler(
    filename = function(){
      paste0("Download ", rv$location," ", Sys.time(),".csv")
    },
    content = function(con = con){
      write.csv(dat(), file = con, row.names = F)
      # data.frame(dat1 = 1:10, dat2 = 2:11)
    }
  )
  
  output$qualityDownloadButton_2 <- renderUI ({
    if(rv$addLocations && !is.null(rv$addnlLocation)) {
      downloadButton('qualityDownload_2', paste0("Download ", rv$addnlLocation, " Data"), width="50%")
    }
  }) 
  output$qualityDownload_2 <- downloadHandler(
    filename = function(){
      paste0("Download ", rv$addnlLocation," ", Sys.time(),".csv")
    },
    content = function(con = con){
      write.csv(addnlDat(), file = con, row.names = F)
    }
  )
  
  ##### Renders message box below map informing user of realtime status of the selected location
  output$realTime <- renderUI({
    if(rv$location == "BU" | rv$location == "WA" ){
      div(box(status="success", width=NULL, title="Real Time Status",
              paste("The", LOCATIONS[rv$location], "site is not yet equipped to transmit in real time. 
                    Data from this sonde is manually downloaded every other week during early spring 
                    through late fall and once a month otherwise.")))
      } else if(rv$location == "BC" | rv$location == "HM" ){
        div(box(status="success", width=NULL, title="Real Time Status",
              paste("The", LOCATIONS[rv$location], "site does not transmit in real time; 
                    it was a short-term installation", TEMPTIMES[rv$location])))  
      } else if(rv$location == "CSF" ){
                      div(box(status="success", width=NULL, title="Real Time Status",
                              paste("The", LOCATIONS[rv$location], "site is coming soon!")))
      } else {
        div(box(status="success", width=NULL, title="Real Time Status",
                paste("The", LOCATIONS[rv$location], "site DOES transmit in real time!
                      If data are not available, then this sonde is out of the river either
                      for regular maintenance or has been seasonally decommissioned for winter.")))
        
    }
  })
  
  ##### Renders alternate attribute description for HF vs SF
  #### Temperature
  output$tabTemp <- renderUI({
    if(rv$location == "CSF" | rv$location == "LSF" | rv$location == "USF"){
      box(status="success", width=NULL,
       p("Salmonids--such as Yellowstone Cutthroat Trout, Rainbow Trout, and Brown Trout--need a specific range of water 
         temperature to thrive and this optimal range shifts depending on their life-stage. Average water temperature 
         thresholds for either eggs/fry or juveniles/adults are shown above using intuitive colors from red to green. 
         Bright green denotes the optimal range of oxygen concentration for growth and activity, light green is sub-optimal
         (cool), yellowish-green is mildly stressful (warm), yellow denotes stressfully high or low temperatures that inhibit 
         growth or survival, and red denotes lethally high temperatures."),
       p("We provide habitat thresholds for Yellowstone Cutthroat Trout (Oncorhynchus Clarkii) and Rainbow Trout (Oncorhynchus 
         mykiss) at the Upper South Fork (USF) and Canyon South Fork (CSF) sites and for Brown Trout (Salmo trutta) at the Lower 
         South Fork (LSF) site. This choice reflects which species are the most common sport fish at each location. Habitat thresholds
         are shown for developing embryos during the period from spawning through fry emergence, otherwise thresholds are shown for 
         adults and juveniles. Recall that Yellowstone Cutthroat Trout usually begin spawning in May and fry emerge in July, Rainbow 
         Trout usually begin spawning in April and fry emerge by June; Brown Trout usually spawn in the fall and fry emerge through 
         late winter to early spring. Additionally, a significant proportion of  Yellowstone Cutthroat Trout in the South Fork Snake 
         River migrate to tributaries to spawn so main river conditions won't have an immediate effect on these fish during spawning 
         and fry emergence."), 
       p("Water temperatures in the South Fork Snake vary by season and by reach: spring-fed reaches and tributaries maintain more
         constant water temperatures throughout the year while the reaches below Palisades Reservoir are influenced by changes in 
         dam outflow and in-reservoir thermal stratification."), 
       p("Our sonde sites have been chosen to reflect the reach they are in. However, the South Fork is a dynamic system at large
          and fine spatial scales. Thus, there are local variations in habitat provided by big woody debris, in-stream shallow
          springs, and differences in bedrock material, just to name a few, that impact temperature and dissolved oxygen. Therefore,
          if habitat in one area is stressful, fish generally have the ability to move upstream or downstream to find better habitat."))
    } else {
      box(status="success", width=NULL, 
          p("Salmonids--such as Rainbow Trout and Brown Trout--need a specific range of water temperature to thrive and this 
            optimal range shifts depending on their life-stage. Average water temperature thresholds for either eggs/fry or 
            juveniles/adults are shown above using intuitive colors from red to green. Bright green denotes the optimal range 
            of oxygen concentration for growth and activity, light green is sub-optimal (cool), yellowish-green is mildly 
            stressful (warm), yellow denotes stressfully high or low temperatures that inhibit growth or survival, and red 
            denotes lethally high temperatures."),
          p("We provide habitat thresholds for Rainbow Trout (Oncorhynchus mykiss) at all sites above Chester Dam and for 
            Brown Trout (Salmo trutta) at St Anthony and Parker-Salem. This choice reflects which species are the most 
            common sport fish at each location. Habitat thresholds are shown for developing embryos during the period from 
            spawning through fry emergence, otherwise thresholds are shown for adults and juveniles. Recall that local 
            Rainbow Trout usually begin spawning in April and fry emerge by June; local Brown Trout usually spawn in the 
            fall and fry emerge through late winter to early spring."), 
          p("Water temperatures in the Henry's Fork vary by season and by reach: spring-fed reaches and tributaries maintain
            more constant water temperatures throughout the year; wide and shallow reaches--such as Harriman Ranch--have more 
            interaction with solar radiation or air temperatures and thus fluctuate with a change in season and daily weather; 
            the reaches below Island Park Reservoir are influenced by changes in dam outflow, in-reservoir thermal stratification, 
            and whether outflow is through the original dam gates or the power plant siphon since these outlets draw from 
            different heights in the reservoir's water column."), 
          p("Our sonde sites have been chosen to reflect the reach they are in. However, the Henry's Fork is a dynamic system at 
            large and fine spatial scales. Thus, there are local variations in habitat provided by big woody debris, in-stream 
            shallow springs, and differences in bedrock material, just to name a few, that impact temperature and dissolved oxygen. 
            Therefore, if habitat in one area is stressful, fish generally have the ability to move upstream or downstream to find 
            better habitat."))   
    }
  })
  #### Turbidity
  output$tabTurb <- renderUI({
    if(rv$location == "CSF" | rv$location == "LSF" | rv$location == "USF"){
      box(status="success", width=NULL, 
          p("Turbidity is a measure of a liquid's visual clarity. Material inside of a liquid absorbs, reflects, and scatters light making it harder 
            to see through. More material in the water results in higher turbidity, but variation in the size, shape, or color of that material impacts turbidity 
            differently. We know that the type of suspended material in the South Fork changes depending on the season or location. Thus turbidity is not a direct 
            measure of concentration of suspended material in the river. The following categories summarize the three general types of material that impact turbidity
            and include examples from the South Fork: 1) inorganic and suspended, this is mineral sediment like sand or silt that does not decay in the \"short term\"; 
            2) organic and suspended, such as bits of plants, cyanobacteria, or animal tissue that do decay in the short term; and 3) organic and dissolved, such as 
            tannins from decaying vegetation."),
          p("The South Fork is a relatively clear river with usual levels of turbidity between 0.05 and 10 Formazin Nephelometric Units (FNU). For reference, 
            well water typically ranges from 0.05 to 10 FNU, orange juice typically ranges from 300 to 900 FNU, and wastewater has a typical turbidity range of 70 to 2000 FNU."
          ))
    } else {
      box(status="success", width=NULL, 
          p("Turbidity is a measure of a liquid's visual clarity. Material inside of a liquid absorbs, reflects, and scatters light making it harder 
            to see through. More material in the water results in higher turbidity, but variation in the size, shape, or color of that material impacts turbidity 
            differently. We know that the type of suspended material in the Henry's Fork changes depending on the season or location. Thus turbidity is not a direct 
            measure of concentration of suspended material in the river. The following categories summarize the three general types of material that impact turbidity
            and include examples from the Henry's Fork: 1) inorganic and suspended, this is mineral sediment like sand or silt that does not decay in the \"short term\"; 
            2) organic and suspended, such as bits of plants, cyanobacteria, or animal tissue that do decay in the short term; and 3) organic and dissolved, such as 
            tannins from decaying vegetation."), 
          p("The Henry's Fork is a relatively clear river with usual levels of turbidity between 0.05 and 10 Formazin Nephelometric Units (FNU). For reference, 
            well water typically ranges from 0.05 to 10 FNU, orange juice typically ranges from 300 to 900 FNU, and wastewater has a typical turbidity range of 70 to 2000 FNU."),
          p("Learn more about our research on turbidity and suspended sediment here:",
            uiOutput("turb1"),
            uiOutput("turb2"),
            uiOutput("turb3"),
            uiOutput("turb4"),
            uiOutput("turb5"),
            uiOutput("turb6"),
            uiOutput("turb7")))    
    }
  })
  #### Dissolved Oxygen
  output$tabDO <- renderUI({
    if(rv$location == "CSF" | rv$location == "LSF" | rv$location == "USF"){
      box(status="success", width=NULL, 
          p("Salmonids--such as Yellowstone Cutthroat Trout, Rainbow Trout, and Brown Trout--need a specific 
            range of dissolved oxygen concentration to thrive. This optimal range shifts depending on their 
            life-stage and ambient water temperature. Average dissolved oxygen thresholds are shown above 
            using intuitive colors from red to green. Bright green denotes the optimal range of oxygen 
            concentration for growth and activity, light green is sub-optimal, yellow denotes stressfully 
            low oxygen levels, and red denotes lethally low levels of oxygen."),
          p("We provide habitat contexts for Yellowstone Cutthroat Trout (Oncorhynchus Clarkii) and 
            Rainbow trout (Oncorhynchus mykiss) at the Upper South Fork (USF) and Canyon South Fork (CSF) 
            sites and for Brown trout (Salmo trutta) at the Lower South Fork (LSF) site; this reflects which  
            species is the most common sport fish at each location. Habitat preferences are shown for developing 
            embryos during the period from spawning through fry emergence. Otherwise preferences are shown for
            adults and juveniles. Recall that Yellowstone Cutthroat Trout usually begin spawning in May and fry
            emerge in July; Rainbow trout usually begin spawning in April and fry emerge by June; local Brown 
            trout usually spawn in the fall and fry emerge through late winter/early spring. Salmonids need higher 
            oxygen concentration in warmer water, which is reflected in the figures once average weekly temperatures
            are above 60 degrees F for Cutthroats and Rainbows and 50 degrees F for Browns. Additionally, a significant proportion 
            of Yellowstone Cutthroat Trout in the South Fork Snake River migrate to tributaries to spawn so main 
            river conditions won't have an immediate effect on these fish during spawning and fry emergence."),
          p("Many factors influence the amount of dissolved oxygen in the South Fork and these factors can be 
            local (water falls and riffles) or seasonal (temperature and solar radiation). The maximum amount 
            of oxygen that water can hold depends on water temperature and atmospheric pressure; warmer water 
            temperature or lower pressure results in lower absolute oxygen levels. Nearby macrophytes--rooted 
            aquatic plants--create daily cycles in dissolved oxygen, which are evident in our 15-minute data. 
            These plants increase oxygen concentration during the day through photosynthesis, but they decrease 
            oxygen levels through the night once photosynthesis ceases and respiration continues.  [Link blogs]"),
          p("Our sonde sites have been chosen to reflect the reach they are in. However, the South Fork is a 
            dynamic system at large and fine spatial scales. Thus, there are local variations in habitat provided 
            by big woody debris, in-stream shallow springs, and differences in bedrock material, just to name a few, 
            that impact temperature and dissolved oxygen. Therefore, if habitat in one area is stressful, fish 
            generally have the ability to move upstream or downstream to find better habitat.")
      )
      
    } else {
      box(status="success", width=NULL,
          p("Salmonids--such as Rainbow Trout and Brown Trout--need a specific range of dissolved oxygen 
            concentration to thrive. This optimal range shifts depending on their life-stage and ambient 
            water temperature. Average dissolved oxygen thresholds are shown above using intuitive colors 
            from red to green. Bright green denotes the optimal range of oxygen concentration for growth 
            and activity, light green is sub-optimal, yellow denotes stressfully low oxygen levels, and 
            red denotes lethally low levels of oxygen."),
          p("We provide habitat contexts for Rainbow trout (Oncorhynchus mykiss) at all sites above Chester 
            Dam and for Brown trout (Salmo trutta) at St Anthony (SA) and Parker-Salem (PS); this reflects which 
            species is the most common sport fish at each location. Habitat preferences are shown for developing  
            embryos during the period from spawning through fry emergence. Otherwise preferences are shown for adults 
            and juveniles. Recall that local Rainbow trout usually begin spawning in April and fry emerge by June; 
            local Brown trout usually spawn in the fall and fry emerge through late winter/early spring. Salmonids 
            need higher oxygen concentration in warmer water, which is reflected in the figures once average weekly 
            temperatures are above 60 degrees F for Rainbows and 50 degrees F for Browns."),
          p("Many factors influence the amount of dissolved oxygen in the Henry's Fork and these factors can be 
            local (water falls and riffles) or seasonal (temperature and solar radiation). The maximum amount 
            of oxygen that water can hold depends on water temperature and atmospheric pressure; warmer water 
            temperature or lower pressure results in lower absolute oxygen levels. Nearby macrophytes--rooted 
            aquatic plants--create daily cycles in dissolved oxygen, which are evident in our 15-minute data. 
            These plants increase oxygen concentration during the day through photosynthesis, but they decrease 
            oxygen levels through the night once photosynthesis ceases and respiration continues.  [Link blogs]"),
          p("Our sonde sites have been chosen to reflect the reach they are in. However, the Henry's Fork is a 
            dynamic system at large and fine spatial scales. Thus, there are local variations in habitat provided 
            by big woody debris, in-stream shallow springs, and differences in bedrock material, just to name a few, 
            that impact temperature and dissolved oxygen. Therefore, if habitat in one area is stressful, fish 
            generally have the ability to move upstream or downstream to find better habitat.")
      )
    }
  })
  
  
  #### Creates Interactive Water Quality Map ######
  output$m <- renderLeaflet({
    leaflet() %>%
      setView(lng = -111.589235, lat = 43.9, zoom = 8) %>% 
      addTiles() %>%  # xAdd default OpenStreetMap map tiles
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(lng=long1, lat= lati1, popup=popLocs1, layerId = locAbbrev1, label = locAbbrev1, 
                 labelOptions = labelOptions(noHide = T)) %>%
      addMarkers(lng=long2, lat= lati2, popup=popLocs2, layerId = locAbbrev2, label = locAbbrev2, 
                 labelOptions = labelOptions(noHide = T), clusterOptions = markerClusterOptions()) %>%
      addMarkers(lng=long3, lat= lati3, popup=popLocs3, layerId = locAbbrev3, label = locAbbrev3, 
                 labelOptions = labelOptions(noHide = T), clusterOptions = markerClusterOptions()) %>%
      addMarkers(lng=long4, lat= lati4, popup=popLocs4, layerId = locAbbrev4, label = locAbbrev4, 
                 labelOptions = labelOptions(noHide = T))
  })
  
  #######################################################################
  ####################   PLOTTING CODE   ################################
  #######################################################################
  # NOTE ABOUT PLOTTING CODE 
  # Much of the graphing code in the following 5 sections repeats! This is intentional 
  # and not for lack of consideration. Individual graphic sections allows for future 
  # changes to individual plots. This creates modularity and removes annoying dependencies
  # that could arise from reusing code. Do not attempt to decompose unless you have reconciled
  # plot dependence. 
  
  ############ Chlorophyll a Graph ###################################
  output$Chlorog <- renderPlot({
    if(rv$location == "CSF") {
      writeComingSoon()
      return()
    }
    cleanSondeDat <- dat()
    lastIndex <- tail(which(!is.na(cleanSondeDat$Chloro_RFU)),1)
    lastLogged <- as.character(cleanSondeDat$contTime[lastIndex])
    extention <- subset(cleanSondeDat, select = c(2:19))
    if(!any(!is.na(extention))) {
      writeDataNA()
      return()
    }
     
    ### Create Envir inorder to pass at.labels and names.labels by reference
    envir <- setUp(cleanSondeDat)
    # cat("\nbreakDatesT is: ", unlist(envir$l$breakDatesT), "\n")
   
    par(mar=c(6, 7, 4, 3), mgp=c(4,1,0), las = 1, cex.axis = 1.5, cex.lab = 1.5)
    plot(cleanSondeDat$contTime, cleanSondeDat$Chloro_RFU, type = "l", ylab = "Chlorophyll a \nRFU", 
         xaxt = "n", xaxs = "i", xlab="", ylim=c(-1,20), yaxp=c(0,20,4), lwd=2, col=1)
    # lines(cleanSondeDat$contTime, cleanSondeDat$BGA_RFU, col=alpha("grey65", trans))
    # axis(4, labels=T, hadj=1, mgp=c(5,2.8,0))
    lines(cleanSondeDat$contTime, cleanSondeDat$Chloro_RFU, lwd = 2)
    if(rv$location != "BC" && rv$location != "HM" && rv$location != "LSF"){
      points(cleanSondeDat$contTime, cleanSondeDat$Chlor_avdymn, pch=16, col="white")
      points(cleanSondeDat$contTime, cleanSondeDat$Chlor_avdymn, pch=16, col="grey50")
      lines(na.omit(zoo(cleanSondeDat$Chlor_avdymn,cleanSondeDat$contTime)), col="white")
      lines(na.omit(zoo(cleanSondeDat$Chlor_avdymn,cleanSondeDat$contTime)), col="grey50")
    }
    
    axis(1, at = envir$at.labels, labels = envir$names.labels, mgp=c(5,1.8,0))
    box()
    abline(v = envir$at.labels, col = "grey65", lty = "dotted")
    abline(h = seq(-1,20, by = 1), col = "grey60", lty = "dotted")
    
    ### ADDING ANOTHER LOCATION TO THE SAME PLOT ###
    if (rv$addLocations == TRUE && !is.null(rv$addnlLocation)) {
      # reactive expression addnlDat() definition on line 631
      addnlSondeDat <- addnlDat()
      addnlx <- addnlSondeDat$contTime
      lastIndex2 <- tail(which(!is.na(addnlSondeDat$Chloro_RFU)),1)
      lastLogged2 <- as.character(addnlSondeDat$contTime[lastIndex2])
      addnlyCholoro <- addnlSondeDat$Chloro_RFU
      lines(addnlx, addnlyCholoro, type="l", col=alpha("royalblue1", trans))
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/3.8,  
                  bty = "n", xpd = NA, col = c("black", "grey50", "royalblue1"), lty = 1, lwd = c(2,2.5),
                  legend = c("","",""), trace=F, pch=c(NA,16,NA), y.intersp = 1.2 )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/5, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/7.5,
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/15), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(LOCATIONS[rv$location] ," Avg. Daily Mean ", MEANYEARS[rv$location]),
             paste0(LOCATIONS[rv$addnlLocation], " (" , rv$addnlLocation ,"), last logged ", lastLogged2," MST")), 
           xpd = NA
      )
    } else { #title(paste0("Data from ",LOCATIONS[rv$location], " (" , rv$location ,") ")) 
      # else no added location data
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/5.5,  
                  bty = "n", xpd = NA, col = c("black", "grey50"), lty = 1, lwd = 2, pch = c(NA, 16),
                  legend = c("",""), trace=F )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/8, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/14), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(rv$location ," Avg. Daily Mean ", MEANYEARS[rv$location])), 
           xpd = NA
      )}
  })
  
  ############ Cyanobacteria Graph ###################################
  output$Cyanog <- renderPlot({
    if(rv$location == "CSF") {
      writeComingSoon()
      return()
    }
    cleanSondeDat <- dat()
    lastIndex <- tail(which(!is.na(cleanSondeDat$BGA_RFU)),1)
    lastLogged <- as.character(cleanSondeDat$contTime[lastIndex])
    extention <- subset(cleanSondeDat, select = c(2:19))
    if(!any(!is.na(extention))) {
      writeDataNA()
      return()
    }
    
    ### Create Envir inorder to pass at.labels and names.labels by reference
    envir <- setUp(cleanSondeDat)
    # cat("\nbreakDatesT is: ", unlist(envir$l$breakDatesT), "\n")
    
    par(mar=c(6, 7, 4, 3), mgp=c(4,1,0), las = 1, cex.axis = 1.5, cex.lab = 1.5)
    plot(cleanSondeDat$contTime, cleanSondeDat$BGA_RFU, type = "l", ylab = "Cyanobacteria \nRFU", 
         xaxt = "n", xaxs = "i", xlab="", ylim=c(-1,20), yaxp=c(0,20,4), lwd=2, col=1)
    # lines(cleanSondeDat$contTime, cleanSondeDat$BGA_RFU, col=alpha("grey65", trans))
    # axis(4, labels=T, hadj=1, mgp=c(5,2.8,0))
    lines(cleanSondeDat$contTime, cleanSondeDat$BGA_RFU, lwd = 2)
    
    if(rv$location != "BC" && rv$location != "HM" && rv$location != "USF" && rv$location != "LSF"){
      points(cleanSondeDat$contTime, cleanSondeDat$Cyano_avdymn, pch=16, col="white")
      points(cleanSondeDat$contTime, cleanSondeDat$Cyano_avdymn, pch=16, col="grey50")
      lines(na.omit(zoo(cleanSondeDat$Cyano_avdymn,cleanSondeDat$contTime)), col="white")
      lines(na.omit(zoo(cleanSondeDat$Cyano_avdymn,cleanSondeDat$contTime)), col="grey50")
    }
    axis(1, at = envir$at.labels, labels = envir$names.labels, mgp=c(5,1.8,0))
    box()
    abline(v = envir$at.labels, col = "grey65", lty = "dotted")
    abline(h = seq(-1,20, by = 1), col = "grey60", lty = "dotted")
    
    ### ADDING ANOTHER LOCATION TO THE SAME PLOT ###
    if (rv$addLocations == TRUE && !is.null(rv$addnlLocation)) {
      # reactive expression addnlDat() definition on line 631
      addnlSondeDat <- addnlDat()
      addnlx <- addnlSondeDat$contTime
      lastIndex2 <- tail(which(!is.na(addnlSondeDat$BGA_RFU)),1)
      lastLogged2 <- as.character(addnlSondeDat$contTime[lastIndex2])
      addnlyCyano <- addnlSondeDat$BGA_RFU
      lines(addnlx, addnlyCyano, type = "l", col="royalblue1")
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/3.8,  
                  bty = "n", xpd = NA, col = c("black", "grey50", "royalblue1"), lty = 1, lwd = c(2,2.5),
                  legend = c("","",""), trace=F, pch=c(NA,16,NA), y.intersp = 1.2 )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/5, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/7.5,
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/15), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(LOCATIONS[rv$location] ," Avg. Daily Mean ", MEANYEARS[rv$location]),
             paste0(LOCATIONS[rv$addnlLocation], " (" , rv$addnlLocation ,"), last logged ", lastLogged2," MST")), 
           xpd = NA
      )
    } else { #title(paste0("Data from ",LOCATIONS[rv$location], " (" , rv$location ,") ")) 
      # else no added location data
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/5.5,  
                  bty = "n", xpd = NA, col = c("black", "grey50"), lty = 1, lwd = 2, pch = c(NA, 16),
                  legend = c("",""), trace=F )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/8, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/14), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(rv$location ," Avg. Daily Mean ", MEANYEARS[rv$location])), 
           xpd = NA
      )}
     })
  
  ################ Conductivity Graph ##################################
  output$Conductivityg <- renderPlot({
    if(rv$location == "CSF") {
      writeComingSoon()
      return()
    }
    cleanSondeDat <- dat()
    lastIndex <- tail(which(!is.na(cleanSondeDat$Cond_muSCm)),1)
    lastLogged <- as.character(cleanSondeDat$contTime[lastIndex])
    extention <- subset(cleanSondeDat, select = c(2:19))
    if(!any(!is.na(extention))) {
      writeDataNA()
      return()
    }
    
    ### Create Envir inorder to pass at.labels and names.labels by reference
    envir <- setUp(cleanSondeDat)
    
    par(mar=c(6, 7, 4, 3), mgp=c(4,1,0), las = 1, cex.axis = 1.5, cex.lab = 1.5)
    # title <- as.list(expression("Conductivity ", paste( mu, "S/cm")))
    if(rv$location == "LSF" | rv$location == "USF"){
      plot(cleanSondeDat$contTime, cleanSondeDat$Cond_muSCm, type = "l", ylab = "Conductivity \nMicrosiemens", 
           xlab="", xaxt="n", xaxs = "i", ylim=c(75,350), yaxp=c(75,350,11), lwd=2)
    }else{
      plot(cleanSondeDat$contTime, cleanSondeDat$Cond_muSCm, type = "l", ylab = "Conductivity \nMicrosiemens", 
           xlab="", xaxt="n", xaxs = "i", ylim=c(75,300), yaxp=c(75,300,9), lwd=2)
    }
    
    title <- as.list(expression("Conductivity ", paste( mu, "S/cm")))
    # axis(4, labels=T, ylim=c(75,170), yaxp=c(75,150,3), hadj=1, mgp=c(5,2.8,0))
    
    lines(cleanSondeDat$contTime, cleanSondeDat$Cond_muSCm, lwd = 2)
    if(rv$location != "BC" && rv$location != "HM" && rv$location != "USF" && rv$location != "LSF"){
      points(cleanSondeDat$contTime, cleanSondeDat$Cond_avdymn, pch=16, col="white")
      points(cleanSondeDat$contTime, cleanSondeDat$Cond_avdymn, pch=16, col="grey50")
      lines(na.omit(zoo(cleanSondeDat$Cond_avdymn,cleanSondeDat$contTime)), col="white")
      lines(na.omit(zoo(cleanSondeDat$Cond_avdymn,cleanSondeDat$contTime)), col="grey50")
    }
    axis(1, at = envir$at.labels, labels = envir$names.labels, mgp=c(5,1.8,0))
    box()
    abline(v = envir$at.labels, col = "grey65", lty = "dotted")
    abline(h = c(seq(75,300, 25)), col = "grey60", lty = "dotted")

    ### ADDING ANOTHER LOCATION TO THE SAME PLOT ###
    if (rv$addLocations == TRUE && !is.null(rv$addnlLocation)) {
      addnlSondeDat <- addnlDat()
      addnlx <- addnlSondeDat$contTime
      lastIndex2 <- tail(which(!is.na(addnlSondeDat$Cond_muSCm)),1)
      lastLogged2 <- as.character(addnlSondeDat$contTime[lastIndex2])
      addnly <- addnlSondeDat$Cond_muSCm
      lines(addnlx, addnly, type = "l", col="royalblue1", lwd = 2)
      # title(paste0(LOCATIONS[rv$location], " and ", LOCATIONS[rv$addnlLocation]))
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/3.8,  
                  bty = "n", xpd = NA, col = c("black", "grey50", "royalblue1"), lty = 1, lwd = c(2,2.5),
                  legend = c("","",""), trace=F, pch=c(NA,16,NA), y.intersp = 1.2 )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/5, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/7.5,
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/15), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(LOCATIONS[rv$location] ," Avg. Daily Mean ", MEANYEARS[rv$location]),
             paste0(LOCATIONS[rv$addnlLocation], " (" , rv$addnlLocation ,"), last logged ", lastLogged2," MST")), 
           xpd = NA
      )
    } else { #title(paste0("Data from ",LOCATIONS[rv$location], " (" , rv$location ,") ")) 
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/5.5,  
                  bty = "n", xpd = NA, col = c("black", "grey50"), lty = 1, lwd = 2, pch = c(NA, 16),
                  legend = c("",""), trace=F )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/8, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/14), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(rv$location ," Avg. Daily Mean ", MEANYEARS[rv$location])), 
           xpd = NA
      )
    }
  }) # end renderPlot
  
  #################### Dissolved Oxygen Graph ##########################
  output$DissolvedOg <- renderPlot ({
    if(rv$location == "CSF") {
      writeComingSoon()
      return()
    }
    cleanSondeDat <- dat()
    lastIndex <- tail(which(!is.na(cleanSondeDat$ODO_mgL)),1)
    lastLogged <- as.character(cleanSondeDat$contTime[lastIndex])
    extention <- subset(cleanSondeDat, select = c(2:19))
    if(!any(!is.na(extention))) {
      writeDataNA()
      return()
    }
     
    ### Create Envir in order to pass at.labels and names.labels by reference
    envir <- setUp(cleanSondeDat)
    
    # DISSOLVED OXYGEN
    par(mar=c(6, 7, 4, 3), mgp=c(4,1,0), las = 1, cex.axis = 1.5, cex.lab = 1.5)
    plot(cleanSondeDat$contTime, cleanSondeDat$ODO_mgL, type = "l", ylab = expression('Dissolved O'[2]~' mg/L'), 
         xlab="", xaxt="n", xaxs = "i", yaxs = "i", ylim=c(0,16), yaxp=c(0,16,8), lwd=2)
    breakDatesT <- envir$l$breakDatesT
    breakDatesDO <- envir$l$breakDatesDO
    ysetC <- envir$l$ysetC
    shades <- envir$l$shades
    
    # Use what's in DO.Fac to either grab 3rd (adults, warm), 4th (adults, cool),
    #  or 5th (embryos) list component from yset:
    for(j in 1:(envir$l$nEndPtsDO - 1)){
      m <- envir$l$DO.Fac[envir$l$endPtsDO[j]]
      polygon(x=c(breakDatesDO[j],breakDatesDO[j], breakDatesDO[j+1],breakDatesDO[j+1]),
              y= ysetC[[m]][1:4], col=rgb(1,0,0, alpha = .45), border=NA)
      polygon(x=c(breakDatesDO[j],breakDatesDO[j], breakDatesDO[j+1],breakDatesDO[j+1]),
              y= ysetC[[m]][5:8], col=rgb(1,1,0, alpha = .9), border=NA)
      polygon(x=c(breakDatesDO[j],breakDatesDO[j], breakDatesDO[j+1],breakDatesDO[j+1]),
              y= ysetC[[m]][9:12], col=rgb(.5,.9,.25, alpha = .3), border=NA)
      polygon(x=c(breakDatesDO[j],breakDatesDO[j], breakDatesDO[j+1],breakDatesDO[j+1]),
              y= ysetC[[m]][13:16], col=rgb(.5,.9,.25, alpha = .65), border=NA)
    }
    
    lines(cleanSondeDat$contTime, cleanSondeDat$ODO_mgL, xpd=NA, col="white", lwd = 2) # plots over the dashed line
    lines(cleanSondeDat$contTime, cleanSondeDat$ODO_mgL, xpd=NA, lwd = 2) # plots over the dashed line
    if(rv$location != "BC" && rv$location != "HM" && rv$location != "USF" && rv$location != "LSF"){
      points(cleanSondeDat$contTime, cleanSondeDat$ODO_avdymn, pch=16, col="white")
      points(cleanSondeDat$contTime, cleanSondeDat$ODO_avdymn, pch=16, col="grey50")
      lines(na.omit(zoo(cleanSondeDat$ODO_avdymn,cleanSondeDat$contTime)), col="white")
      lines(na.omit(zoo(cleanSondeDat$ODO_avdymn,cleanSondeDat$contTime)), col="grey50")
    }

    title <- as.list(expression('Dissolved O'[2], 'mg/L'))
    # axis(4, labels=T, ylim=c(0,16), yaxp=c(0,15,3), hadj=1, mgp=c(5,2.2,0))
    axis(1, at = envir$at.labels, labels = envir$names.labels, mgp=c(5,1.8,0))
    box()
    abline(v = envir$at.labels, col = "grey60", lty = "dotted")
    abline(h = seq(0,16, by = 2), col = "grey60", lty = "dotted")
    
    # Legend
    if (rv$location == "PS" || rv$location == "SA"  || rv$location == "LSF") {
      habitatText <- paste0("Colors represent habitat contexts for Brown trout, the most common sport fish near ", LOCATIONS[rv$location])
    } else {
      habitatText <- paste0("Colors represent habitat contexts for Rainbow trout, the most common sport fish near ", LOCATIONS[rv$location])  
    } 
    mtext(habitatText, side = 1, at = c(cleanSondeDat$contTime[1]), line = 5, adj = 0, cex=.90) 
    
    legend("bottomleft", # text.width=c(0,0.15,0.31,0.45),
           bty = "n", x.intersp=.5, inset = c(0, -.30), # Distance from margin as fraction of plotting region
           legend=c("Lethal", "Stressful", "Sub-optimal", "Optimal"),
           fill=c(rgb(1,0,0, alpha = .7), rgb(1,1,0, alpha = 1), rgb(.5,.9,.25, alpha = .3), rgb(.5,.9,.25, alpha = .9)),
           cex = 0.75, xpd = NA, horiz=TRUE)
    
    ### ADDING ANOTHER LOCATION TO THE SAME PLOT ###
    if (rv$addLocations == TRUE && !is.null(rv$addnlLocation)) {
      addnlSondeDat <- addnlDat()
      addnlx <- addnlSondeDat$contTime
      addnly <- addnlSondeDat$ODO_mgL
      lastIndex2 <- tail(which(!is.na(addnly)),1)
      lastLogged2 <- as.character(addnlx[lastIndex2])
      lines(addnlx, addnly, type = "l", col="white", lwd = 2.5)
      lines(addnlx, addnly, type = "l", col="royalblue1", lwd = 2.5)
            # title(paste0(LOCATIONS[rv$location], " and ", LOCATIONS[rv$addnlLocation]))
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/3.8,  
                  bty = "n", xpd = NA, col = c("black", "grey50", "royalblue1"), lty = 1, lwd = c(2,2.5),
                  legend = c("","",""), trace=F, pch=c(NA,16,NA), y.intersp = 1.2 )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/5, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/7.5,
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/15), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(LOCATIONS[rv$location] ," Avg. Daily Mean ", MEANYEARS[rv$location]),
             paste0(LOCATIONS[rv$addnlLocation], " (" , rv$addnlLocation ,"), last logged ", lastLogged2," MST")), 
           xpd = NA
      )
    } else { #title(paste0("Data from ",LOCATIONS[rv$location], " (" , rv$location ,") ")) 
      # else no added location data
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/5.5,  
                  bty = "n", xpd = NA, col = c("black", "grey50"), lty = 1, lwd = 2, pch = c(NA, 16),
                  legend = c("",""), trace=F )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/8, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/14), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(rv$location ," Avg. Daily Mean ", MEANYEARS[rv$location])), 
           xpd = NA
      )
    }
  }) 
  
  ###################### Turbidity Graph #################################
  output$Turbidityg <- renderPlot ({
    if(rv$location == "CSF") {
      writeComingSoon()
      return()
    }
    cleanSondeDat <- dat()
    lastIndex <- tail(which(!is.na(cleanSondeDat$Turb_FNU)),1)
    lastLogged <- as.character(cleanSondeDat$contTime[lastIndex])
    extention <- subset(cleanSondeDat, select = c(2:19))
    if(!any(!is.na(extention))) {
      writeDataNA()
      return()
    }
    
    ### Create Envir inorder to pass at.labels and names.labels by reference
    envir <- setUp(cleanSondeDat)
    
    par(mar=c(6, 7, 4, 3), mgp=c(4,1,0), las = 1, cex.axis = 1.5, cex.lab = 1.5)
    plot(cleanSondeDat$contTime, cleanSondeDat$Turb_FNU, type = "l", ylab = "Turbidity \nFNU", 
         xlab="", xaxt="n", xaxs = "i", yaxs = "i", ylim=c(0,25), yaxp=c(0,20,4), lwd=2)
    # axis(4, labels=T, ylim=c(0,23), yaxp=c(0,20,4), hadj=1, mgp=c(5,2.2,0))
   
    lines(cleanSondeDat$contTime, cleanSondeDat$Turb_FNU, lwd = 2)
    if(rv$location != "BC" && rv$location != "HM" && rv$location != "USF" && rv$location != "LSF"){
      points(cleanSondeDat$contTime, cleanSondeDat$Turb_avdymn, pch=16, col="white")
      points(cleanSondeDat$contTime, cleanSondeDat$Turb_avdymn, pch=16, col="grey50")
      lines(na.omit(zoo(cleanSondeDat$Turb_avdymn,cleanSondeDat$contTime)), col="white")
      lines(na.omit(zoo(cleanSondeDat$Turb_avdymn,cleanSondeDat$contTime)), col="grey50")
    }
     
    axis(1, at = envir$at.labels, labels = envir$names.labels, mgp=c(5,1.8,0))
    box()
    abline(v = envir$at.labels, col = "grey65", lty = "dotted")
    abline(h = seq(0,20, by = 5), col = "grey60", lty = "dotted")
    
    ### ADDING ANOTHER LOCATION TO THE SAME PLOT ###
    if (rv$addLocations == TRUE && !is.null(rv$addnlLocation)) {
      addnlSondeDat <- addnlDat()
      addnlx <- addnlSondeDat$contTime
      lastIndex2 <- tail(which(!is.na(addnlSondeDat$Turb_FNU)),1)
      lastLogged2 <- as.character(addnlSondeDat$contTime[lastIndex2])
      addnly <- addnlSondeDat$Turb_FNU
      lines(addnlx, addnly, type = "l", col="royalblue1", lwd = 2.5)
      # title(paste0(LOCATIONS[rv$location], " and ", LOCATIONS[rv$addnlLocation]))
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/3.8,  
                  bty = "n", xpd = NA, col = c("black", "grey50", "royalblue1"), lty = 1, lwd = c(2,2.5),
                  legend = c("","",""), trace=F, pch=c(NA,16,NA), y.intersp = 1.2 )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/5, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/7.5,
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/15), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(LOCATIONS[rv$location] ," Avg. Daily Mean ", MEANYEARS[rv$location]),
             paste0(LOCATIONS[rv$addnlLocation], " (" , rv$addnlLocation ,"), last logged ", lastLogged2," MST")), 
           xpd = NA
      )
    } else { #title(paste0("Data from ",LOCATIONS[rv$location], " (" , rv$location ,") ")) 
      # else no added location data
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/5.5,  
                  bty = "n", xpd = NA, col = c("black", "grey50"), lty = 1, lwd = 2, pch = c(NA, 16),
                  legend = c("",""), trace=F )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/8, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/14), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(rv$location ," Avg. Daily Mean ", MEANYEARS[rv$location])), 
           xpd = NA
      )
    }
  }) 
  
  ################# Temperature Graph ######################################
  output$Tempg <- renderPlot({
    # Since this is the initial plot, I need to pull the data down here, but only during
    # app startup - ?? Do: I NEED TO FIGURE OUT HOW TO INITIATE CLEANSONDEDAT ONCE, NOT HERE ??
    if(rv$location == "CSF") {
    writeComingSoon()
    return()
  }
    objectName = paste0(rv$location, "/", rv$location, "_", input$timeOption, ".Rdata")
    s3load(object = objectName, bucket="hffwq")
    dfName = paste0("data_", input$timeOption)
    cleanSondeDat <- eval(parse(text = dfName))
    
    if (input$timeOption == "full") {
      cat(file=stderr(), "Tempg was called \n")
      input$updateDatesButton
      cleanSondeDat <- getFullData(a = isolate(input$dateRangeMain), cleanSondeDat)
    }
    lastIndex <- tail(which(!is.na(cleanSondeDat$Temp_C)),1)
    lastLogged <- as.character(cleanSondeDat$contTime[lastIndex])
    
    # cleanSondeDat <- data()
    extention <- subset(cleanSondeDat, select = c(2:19))
    if(!any(!is.na(extention))) {
      writeDataNA()
      return() 
    }
    
    ### Create Envir inorder to pass at.labels and names.labels by reference
    envir <- setUp(cleanSondeDat)
    breakDatesT <- envir$l$breakDatesT
    breakDatesDO <- envir$l$breakDatesDO
    ysetF <- envir$l$ysetF
    shades <- envir$l$shades
    
    # par(mar=c(6, 7, 4, 3))
    par(mar=c(6, 7, 4, 3), las = 1, mgp=c(4,1,0), cex.axis = 1.5, cex.lab = 1.5)
    plot(cleanSondeDat$contTime, coredata(envir$zF), type = "l", ylab = expression('Temperature'~degree~F), 
         xlab="", xaxt="n", xaxs = "i", yaxs = "i", ylim=c(32,80), yaxt = "n", lwd=2) 
         # ylim based on F. Celsius Axis: ylim=c(0,27), yaxp=c(0,25,5), lwd=2)
    
    # Use what's in tempFac to either grab 1st (adults, juv, fry), 2nd (embryos), list component from yset
    for(j in 1:(envir$l$nEndPtsT - 1)){
      # j <- 1
      m <- envir$l$tempFac[envir$l$endPtsT[j]]
      polygon(x=c(breakDatesT[j],breakDatesT[j], breakDatesT[j+1],breakDatesT[j+1]),
              y= ysetF[[m]][1:4], col=shades[[m]][1], border=NA)
      polygon(x=c(breakDatesT[j],breakDatesT[j], breakDatesT[j+1],breakDatesT[j+1]),
              y= ysetF[[m]][5:8], col=shades[[m]][2], border=NA)
      polygon(x=c(breakDatesT[j],breakDatesT[j], breakDatesT[j+1],breakDatesT[j+1]),
              y= ysetF[[m]][9:12], col=shades[[m]][3], border=NA)
      polygon(x=c(breakDatesT[j],breakDatesT[j], breakDatesT[j+1],breakDatesT[j+1]),
              y= ysetF[[m]][13:16], col=shades[[m]][4], border=NA)
      polygon(x=c(breakDatesT[j],breakDatesT[j], breakDatesT[j+1],breakDatesT[j+1]),
              y= ysetF[[m]][17:20], col=shades[[m]][5], border=NA)
      polygon(x=c(breakDatesT[j],breakDatesT[j], breakDatesT[j+1],breakDatesT[j+1]),
              y= ysetF[[m]][21:24], col=shades[[m]][6], border=NA)
    }
    
    lines(cleanSondeDat$contTime, coredata(envir$zF), col="white", xpd=NA, lwd = 2) # plots over the dashed line
    lines(cleanSondeDat$contTime, coredata(envir$zF), col=1, xpd=NA, lwd = 2)
    
    if(rv$location != "BC" && rv$location != "HM" && rv$location != "USF" && rv$location != "LSF"){
      points(cleanSondeDat$contTime, coredata(envir$avgzF), pch=16, col="white")
      points(cleanSondeDat$contTime, coredata(envir$avgzF), pch=16, col="grey50")
      lines(na.omit(zoo(coredata(envir$avgzF),cleanSondeDat$contTime)), col="white")
      lines(na.omit(zoo(coredata(envir$avgzF),cleanSondeDat$contTime)), col="grey50")
    }
   
    axis(2, at = c(32, seq(40,70, by = 10)))
    # axis(4, at = c(32, 40, 54, 68, 77), labels=T, ylim=c(32,80), yaxp=c(0,25,5), hadj=1, mgp=c(5,2.2,0), las=1)
    axis(1, at = envir$at.labels, labels = envir$names.labels, mgp=c(5,1,0), cex.axis = .8)
    # axis(1, at = envir$at.labels, labels = envir$namesYrs.labels, mgp=c(5,2,0), cex.axis = .8)
    
    box()
    abline(v = envir$at.labels, col = "grey65", lty = "dotted")
    abline(h = seq(35,80, by = 5), col = "grey60", lty = "dotted")
    
    # Legend
    if (rv$location == "PS" || rv$location == "SA"  || rv$location == "LSF") {
      habitatText <- paste0("Colors represent habitat contexts for Brown trout, the most common sport fish near ", LOCATIONS[rv$location])
    } else {
      habitatText <- paste0("Colors represent habitat contexts for Rainbow trout, the most common sport fish near ", LOCATIONS[rv$location])  
    } 
    mtext(habitatText, side = 1, at = c(cleanSondeDat$contTime[1]), line = 5, adj = 0, cex=.90) #, family="ariel")
    
    legend("bottomleft", # text.width=c(0,0.15,0.31,0.45),
           bty = "n", x.intersp=.5, inset = c(0, -.30), # Distance from margin as fraction of plotting region
           legend=c("Lethal", "Stressful", "Semi-stressful", "Sub-optimal", "Optimal"),
           fill=c(rgb(1,0,0, alpha = .7), rgb(1,1,0, alpha = 1), rgb(.9, 1, .5, alpha = .7), 
                  rgb(.5,.9,.25, alpha = .3), rgb(.5,.9,.25, alpha = .9)),
           cex = 0.75, xpd = NA, horiz=TRUE)
    
    ### ADDING ANOTHER LOCATION TO THE SAME PLOT ###
    if (rv$addLocations == TRUE && !is.null(rv$addnlLocation)) {
      # If added location data
      addnlSondeDat <- addnlDat()
      addnlx <- addnlSondeDat$contTime
      lastIndex2 <- tail(which(!is.na(addnlSondeDat$Temp_C)),1)
      lastLogged2 <- as.character(addnlSondeDat$contTime[lastIndex2])
      addnly <- celsiusToFahrenheit(addnlSondeDat$Temp_C)
      lines(addnlx, addnly, type = "l", col="white", lwd = 2.5)
      lines(addnlx, addnly, type = "l", col="royalblue1", lwd = 2.5)
     # title(paste0(LOCATIONS[rv$location], " and ", LOCATIONS[rv$addnlLocation]))
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/3.8,  
                  bty = "n", xpd = NA, col = c("black", "grey50", "royalblue1"), lty = 1, lwd = c(2,2.5),
                  legend = c("","",""), trace=F, pch=c(NA,16,NA), y.intersp = 1.2 )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/5, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/7.5,
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/15), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(LOCATIONS[rv$location] ," Avg. Daily Mean ", MEANYEARS[rv$location]),
             paste0(LOCATIONS[rv$addnlLocation], " (" , rv$addnlLocation ,"), last logged ", lastLogged2," MST")), 
           xpd = NA
      )
    } else { #title(paste0("Data from ",LOCATIONS[rv$location], " (" , rv$location ,") ")) 
      # else no added location data
      b <- legend(cleanSondeDat$contTime[1], 
                  par("usr")[4]+(par("usr")[4]-par("usr")[3])/5.5,  
                  bty = "n", xpd = NA, col = c("black", "grey50"), lty = 1, lwd = 2, pch = c(NA, 16),
                  legend = c("",""), trace=F )
      text(cleanSondeDat$contTime[1], 
           c(par("usr")[4]+(par("usr")[4]-par("usr")[3])/8, 
             par("usr")[4]+(par("usr")[4]-par("usr")[3])/14), pos = 4, offset = 3,
           c(paste0(LOCATIONS[rv$location], " (" , rv$location ,"), last logged ", lastLogged," MST"),
             paste0(rv$location ," Avg. Daily Mean ", MEANYEARS[rv$location])), 
           xpd = NA
           )
              }
  }) 
 }

shinyApp(ui = ui, server = server)


# # Run profvis
# require(profvis)
# setwd("/Volumes/Z Drive/Sonde Automation Project/R Code/Scientific Website")
# profvis({
#   runApp(getwd(), display.mode = "normal" )},
#   prof_output = getwd()
#   )
# To reload a saved profvis:
# profvis(prof_input = '/path_to_save_output/file108f93bff877b.Rprof')  

