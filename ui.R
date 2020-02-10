library(DT)
library(shiny)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(maps)

shinyUI(dashboardPage(skin = "black",
    dashboardHeader(title = "Dashboard of Top#10 Miami's 311 Service Requests",titleWidth = 400),
    dashboardSidebar(
        sidebarUserPanel(name="",subtitle="",image = "./sr_admin.jpg"),
        sidebarMenu(id='sideBarMenu',
            menuItem("Dashboard Information", tabName = "summary", icon = icon('book')),
            menuItem("Map and Charts", tabName = "map", icon = icon("map")),
            menuItem("Data", tabName = "data", icon = icon("database"))),
        
        conditionalPanel("input.sideBarMenu == 'map'",
            selectizeInput("month",
                           "Select Month for Gauge Chat",
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
                    fluidRow(box(
                        h2(tags$b("Dashboard Information:")),
                        tags$hr(),
                        h4('Welcome to my Data Visualization project using packages such as RShiny, Leaflet, and Plotly. The main goals of this project were to analyze a large dataset and to create an interactive dashboard that enables a user to uncover potential insights on both a temporal and geographic level.'),
                        tags$hr(),
                        p('To achieve this, I developed two visualizations that can be accessed through the left-hand navigation pane.\
                            The Interactive Chart provides a user a way to identify unique trends over time by filtering \
                          by complaint type, borough, or time. And to further investigate a discovered trend, the Interactive Map can be used to similarly filter and then view geographic representations of incident activity.\
                          Feel free to toggle a heatmap, cluster, and top 50 display to explore the most reported addresses both on the map and within the data table beneath it.'),
                        tags$hr(),
                        p('My main inspiration for this project came from a ', tags$a(href="https://www.youtube.com/watch?v=6xsvGYIxJok", "video"),' on data storytelling. I also wanted to understand how NYC Open Data could provide a deeper understanding of the dynamic city we live in. \
                            For reference, part of my approach in summarizing this dataset of over 2 million rows was to consolidate as many of the similar complaint types as I could into uniform categories.\
                          Through this process, I narrowed down the over 250 complaint types into 10 categories such as \"noise\", \"vermin\", \"unsanitary\", \"construction\", etc. For example, the category \"tree\" is actually an aggregation of numerous complaint_types found such as:',tags$li('Damaged Tree'),
                          tags$li("Overgrown Tree/Branches"), tags$li("Dead Tree"), tags$li("New Tree Request")),
                        tags$br(),
                        tags$p('Lastly, this app is a work in progress and some possible directions I would like to extend it would be:', tags$li("Add external data such as rent price or crime data to explore any potential correlation"),
                               tags$li("Improve performance at higher data volumes"), tags$li('Develop real-time integration with Open Data API')),width=12, status='primary')
                         )
                    ),
            tabItem(tabName = "map",
                    #fluidRow(infoBoxOutput("maxBox"),
                     #        infoBoxOutput("minBox"),
                       #      infoBoxOutput("avgBox")),
                    fluidRow(box(htmlOutput("gauge"),height = 200,solidHeader = TRUE,title ="Monthly % of requests handeled on-time by each Daparment"),
                             box(htmlOutput("time"), height = 200,solidHeader = TRUE,footer = "Total Number of Service Requests for 2019")),
                    fluidRow(box(htmlOutput("bar"), height =300),
                            (box(htmlOutput("sourcepie"), height =300,title = "Source of the request"))),
                    fluidRow(box(leafletOutput("mymap"), height =400),
                             box(plotlyOutput("plotlybar"), height =400))
                    
                    ),
            tabItem(tabName = "data",
                fluidRow(box(DT::dataTableOutput("table"),width = 12)))
        )
    )
)
)