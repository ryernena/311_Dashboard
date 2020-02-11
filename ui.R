library(DT)
library(shiny)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(maps)

shinyUI(dashboardPage(skin = "black",
    dashboardHeader(title = "Dashboard of Miami's 311 Service Requests for 2019",titleWidth = 700),
    dashboardSidebar(
        sidebarUserPanel(name="",subtitle="",image = "./sr_admin.jpg"),
        sidebarMenu(id='sideBarMenu',
            menuItem("Dashboard Information", tabName = "summary", icon = icon('book')),
            menuItem("Map and Charts", tabName = "map", icon = icon("map")),
            menuItem("Data", tabName = "data", icon = icon("database"))),
        
        conditionalPanel("input.sideBarMenu == 'map'",
            selectizeInput("month",
                           "Select Month",
                           multiple=TRUE,
                           choices=unique(x$month),
                           selected=sort(unique(x$month))[1]),
            selectizeInput("selected",
                           "Select Request Type for Map",
                           multiple=TRUE,
                           choices=unique(top10$issue_type),
                           selected=unique(top10$issue_type)[1]))
        
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
        .skin-black .main-header .logo {
                           background: black; 
                            color:white;
                            font-size: 16pt;
                            }
        .skin-black .main-header .logo:hover {
                            background: rgb(85,0,0);
                            }
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper{
                             background: -webkit-linear-gradient( black , white); 
                             }
        .skin-black .main-header .navbar{
                             background: -webkit-linear-gradient(left, black, white); 
                            }
        .skin-black .user-panel>.info{
                      display:none;
                             }
            .user-panel>.image>img{
                        max-width: 125px;
                      height: 125px;
                      border-radius:20%;
                       margin-left: 40px;

                              }
                            '))),
        tabItems(
            tabItem(tabName = "summary",
                    fluidRow(
                        box(
                            h2(tags$b("Dashboard Information:")),
                            tags$hr(),
                            h4('The 311 Service Requests Dashboard include service requests logged in the year of 2019 by the City of Miami-Dade County. This information is publicly available in the Miami Dade County’s Open Data portal', tags$a(href="https://www.gis-mdc.opendata.arcgis.com/"), '. The main goals of this project were to analyze a large dataset and to create an interactive dashboard that enables a user to uncover potential insights on both a temporal and geographic level.',
                               tags$br(),tags$br(),'The data sets include services completed reactively and proactively by Miami-Dade County departments and requests submitted by citizens via phone (311), online (miamidade.gov), and other self service channels such as the 311Direct mobile application.'),
                            
                            tags$hr(),
                            p(h3(tags$b('Who is it for and why?')),
                              tags$br(),
                              'For the ease of use and to get most useful insights out this dashboard, top ten service request types have been filtered from the data and used to build this dashboard. 
                              This dashboard is useful mostly for internal use within Miami-Dade County by executives and head of Departments to:',
                                tags$li("Measure KPIs"),
                                tags$li("Identify problem areas/Improve process efficiency"),
                                tags$li("Resource Allocation-scheduled maintenance and front desk"),
                                tags$li("Improve/Monitor effectiveness of IT tools/channels"),
                              ),
                            
                            tags$hr(),
                            p(h3(tags$b('What is in the dashboard?')),
                                tags$br(),'The Dashboard consist of two main menus named 1. Map and Charts and 2. Data.',
                                tags$li("Map and Charts: This menu has various types of charts and a map shows clusters of selected request types. There are two conditional filters to select Month and Type of Request based on which charts will display visuals."),
                                tags$li("Data: This menu shows the data table that has been used to build charts and the map. Data table can be searched and filtered by using any search word relevant to service requests. Data table has been cutdown to limited number of useful columns."),
                                ),
                            tags$br(),
                            tags$hr(),
                            p(h3(tags$b('How to use?')),
                              tags$br(),
                              'Which Department is performing well? Select the month from the dropdown list of months and the Gauge Chart will show the percentage of the requests handled on-time by top 5 departments. If you select multiple months, the Gauge Chart will sum up the requests for selected months and shows the results.',
                              
                              tags$br(),
                              tags$b('How many requests logged each day during 2019?'), 'Click on the cell of desired day on the time series map and it will display the count. We can notice busiest days during the year by looking at the color density.',
                              tags$br(),
                              tags$b('How many requests for each month and what types are they?'), 'Select a month or multiple months from dropdown list of months, then the bar chart will show the count of each request type for selected months. You can turn on/off the type of requests by clicking on the legend items.',
                              tags$br(),
                              tags$b('Counts on the method of request received?'), 'This bar chart shows different types and counts of methods/channels through which requested were received from citizens or internal departments. Select a month or multiple months from dropdown list of months, then the bar chart will show the count of each method/channel and request type for selected months.',
                              tags$br(),
                              tags$b('Want to see locations of these events on the map?'), 'Select months and type of requests from dropdown lists and the map will render clusters of the requests accordingly. As you zoom in, clusters will break down until the map shows individual event/request. You can identify the address by clicking on the markers.',
                              tags$br(),
                              tags$b('Want to know the count of each request type per City?'), 'Select months and type of requests from dropdown lists and the bar chart will show the count and type of requests for each city.',
                              tags$br(),
                              tags$b('Want to explore the underlying data?'), 'Click on the “Data” menu which shows the data table on a different page. Enter a search word in the Search textbox and the table will be filter if it finds the word in any of the columns. Multiple words can be entered by typing them with a space in between.'
                                ),
                            tags$p(h3(tags$b('Insights and what’s next?')),
                                   tags$br(),
                                    tags$li("Insights are endless and depending on the questions asked by each individual to this dashboard, because this dashboard caters for different groups of audiences and for different purposes."), 
                                    tags$li("Can be improved by including more filtering controls to drilldown on the information."),
                                    tags$li("Can be targeted for citizens by including insensitive data and different kind of measures."),
                                    tags$li("More interaction can be built between the map and charts.")),width=12, status='primary')
                    )),
            tabItem(tabName = "map",
                    #fluidRow(infoBoxOutput("maxBox"),
                     #        infoBoxOutput("minBox"),
                       #      infoBoxOutput("avgBox")),
                    fluidRow(box(htmlOutput("gauge"),height = 200,solidHeader = TRUE,title ="Monthly % of requests handeled on-time by each Deparment"),
                             box(htmlOutput("time"), height = 200,solidHeader = TRUE,footer = "Daily Number of Service Requests for 2019")),
                    fluidRow(box(plotlyOutput("plotlybar2"), height =300),
                            (box(plotlyOutput("sourcebar"), height =300))),
                    fluidRow(box(leafletOutput("mymap"), height =450),
                             box(plotlyOutput("plotlybar"), height =450))
                    
                    ),
            tabItem(tabName = "data",
                fluidRow(box(DT::dataTableOutput("table"),width = 12)))
        )
    )
)
)