#

#### please set your word directory first!

library(shiny)

packages.used=c("shiny", "plotly", "shinydashboard", "leaflet")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
    install.packages(packages.needed, dependencies = TRUE)
}

library(DT)
library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(purrr)
library(scales)
library(lattice)
library(htmltools)
library(maps)
library(plotly)


### build dashboard

header <- dashboardHeader(
    dropdownMenu(
        type = "notifications",
        notificationItem(
            text = "10 people are looking at the same website",
            icon("users")
        )
    )                                                     
)


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("University Search", tabName = "search", icon = icon("search-location")),
        menuItem("Overview", tabName = "descriptive", icon = icon("chart-line")),
        menuItem("References", tabName = "references", icon = icon("th"))
    )
)

body <- dashboardBody(
    ## set the color of header
    tags$head(tags$style(HTML('/* logo */
                                .skin-blue .main-header .logo {
                                background-color: #9bddff;
                                }
                            /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #9bddff;
                                }
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #9bddff;
                            }
                            /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #9bddff;
                            }
                            /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #f8f9f9;
                            }
                            /* body */
                                .content-wrapper, .right-side {
                                background-color: #f8f9f9;
                            }
                            /*    Move everything below the header */
                            .content-wrapper {
                            margin-top: 100px;
                            }
                            .content {
                            padding-top: 50px;
                            }
                            /*    Format the title/subtitle text */
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 50%;
                            left: 50%;
                            transform:translate(-50%, -50%);
                            }
                            @media (max-width: 590px) {
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 10%;
                            left: 10%;
                            transform:translate(-5%, -5%);
                            }
                            }
                            @media (max-width: 767px) {
                            .primary-title {
                            font-size: 1.1em;
                            }
                            .primary-subtitle {
                            font-size: 1em;
                            }
                            }
                            /*    Make the image taller */
                            .main-header .logo {
                             height: 190px;
                              }
                            /*    Override the default media-specific settings */
                            @media (max-width: 5000px) {
                            .main-header {
                            padding: 0 0;
                            position: relative;
                            }
                            .main-header .logo,
                            .main-header .navbar {
                            width: 100%;
                            float: none;
                            }
                           .main-header .navbar {
                           margin: 0;
                           }
                           .main-header .navbar-custom-menu {
                          float: right;
                          }
                          }
                          /*    Move the sidebar down */
                          .main-sidebar {
                          position: absolute;
                          }
                          .left-side, .main-sidebar {
                          padding-top: 250px;
                          }'
    ))),
    tabItems(
        tabItem(
            tabName = "home",
            fluidPage(
                tags$style(
                    HTML('
          .box.box-solid.box-primary>.box-header {
            color:#fff;
              background:#9bddff
          }
          
          .box.box-solid.box-primary{
            border-bottom-color:#9bddff;
              border-left-color:#9bddff;
              border-right-color:#9bddff;
              border-top-color:#9bddff;
          }'
                    )
                ),
                fluidRow(
                    box(width = 15, title = "Introduction", status = "primary",
                        solidHeader = TRUE, 
                        h3("Help you find the ideal university"),
                        h4("Our shiny app is based on Universities in the United States. "),
                        h4("This application aims to help users to discover and compare schools in a more efficient manner,
                           and is created by Olivia Wang in April 2020, who is a Columbia student taking a tiny step here by designing this application to help simplify the college decision making process for fellow users."),
                        h4("Let's explore this app!"))),
                fluidRow(
                    box(width = 15, title = "User Guide", status = "primary",
                        solidHeader = TRUE,
                        h3("How to use this app?"),
                        tags$div(tags$ul(
                            tags$li("University Search: This part is our search map and it contains seven filters."),
                            tags$li("Overview: The tab has several graphs to give users a board overview of univerisity situation they have chosen in University Search tab.")
                        ))
                    )
                ),
                fluidRow(
                    tags$img(
                        src = "columbia.jpg",
                        width = "100%"
                    )
                )
            )
        ),
        tabItem(
            tabName = "search",
            
            fluidPage(
                fluidRow(
                    column(2,
                           div(selectInput("major", label = "Major", 
                                           choices = c("All" ,"Agriculture"="agriculture", "Architecture",
                                                       "Biology"="Bio","Business", "Computer Science"="CS", 
                                                       "Education"="Edu", "Engineering", "History", "Math and Statistics"="MathStat",
                                                       "Nature Resources"="NatureResource", "Psychology", "Social Science"="SocialScience"), selected = "All"),
                               
                               selectInput("Citytype", label = "Type of City",
                                           choices = c("All", "City", "Rural", "Suburb","Town"),selected = "All")
                           ),
                           hr(),
                           sliderInput("ForbesRank","Academic performance",0,100,60),
                           sliderInput("AvgCost","Average Cost of Attendance",0, 300,200),
                           sliderInput("Earn","Earnings & Jobs",0, 200, 100),
                           sliderInput("CrimeRate","CrimeRate",0, 150, 80),
                           sliderInput("HappyRank","Happiness/Life quality",0, 100, 60),
                           sliderInput("confirmed_rate","Covid-19 condition",0,50,30),
                           submitButton("Find my dream school!",width='100%')),
                    column(10, leafletOutput("map",height="800px"),div())
                )
            )
        ),
        tabItem(
            tabName = "descriptive",
            tabsetPanel(type = "tabs",
                        tabPanel("Univerisy's Conditions",
                                 
                                 fluidRow(
                                     column(10, plotlyOutput('crime'), div())),
                                 
                                 fluidRow(),
                                 
                                 fluidRow(
                                     column(10, plotlyOutput('cost'), div())),
                                 
                                 fluidRow(),
                                 
                                 fluidRow(
                                     column(10, plotlyOutput('covid'), div()))),
                    
                
                               tabPanel('Ranking',
                                        dataTableOutput("tablerank"),
                                        tags$style(type="text/css", '#myTable tfoot {display:none;}')))),
        
        tabItem(
            tabName = "references",
            fluidPage(
                fluidRow(
                    box(width = 15, title = "Data Source", status = "primary",
                        solidHeader = TRUE,
                        "The data source of this shiny app is from",
                        tags$a(href = "https://collegescorecard.ed.gov/data/", "College Score"), 
                        ".")
                ),
                fluidRow(
                    box(width = 15, title = "Project Code", status = "primary",
                        solidHeader = TRUE, 
                        "The code of this project can be found at",
                        actionButton(inputId='code', label="GitHub", 
                                     icon = icon("github"), 
                                     onclick ="window.open('https://github.com/olivia3395')"),
                        ".")
                ),
                fluidRow(
                    box(width = 15, title = "Contact Us", status = "primary",
                        solidHeader = TRUE, 
                        h4("Feel free to contact me if you're interested in this app!"),
                        h5("Wang, Yuyao: yw3395@columbia.edu")
                    )
                ),
                fluidRow(
                    tags$img(
                        src = "columbia2.jpg",
                        width = "100%"
                    )
                )
            )
        )
    )
)



# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)


server<-function(input, output){
    #read data
    
    load(file="schdata.Rdata")
    #subset data depending on user input in Shiny app
    # Define reactive ({})
    
    major<-reactive({
        major<-input$major
    })
    
    Citytype<-reactive({
        Citytype<-input$Citytype
    })
    
    ForbesRank<-reactive({
        ForbesRank<-input$ForbesRank
    })
    
    
    AvgCost<-reactive({
        AvgCost<-input$AvgCost
    })
    
    
    Earn<-reactive({
        Earn<-input$Earn
    })
    
    
    CrimeRate<-reactive({
        CrimeRate<-input$CrimeRate
    })
    
    HappyRank<-reactive({
        HappyRank<-input$HappyRank
    })
    
    confirmed_rate<-reactive({
        confirmed_rate<-input$confirmed_rate
    })
    
    
    v1<-reactive({
        if (major() == "All") {
            v1<-schdata
        } 
        else {
            selectmaj<-paste("Major", "_", major(), sep="")
            v1<- schdata %>% filter_(paste(selectmaj, "==", 1))}}) 
    
    v2<- reactive({
        v2<- v1()[v1()$ForbesRank < ForbesRank(), ]
    })   
    
    
    v4<- reactive({
        
        v4<- v2()[as.numeric(v2()$AvgCost) < AvgCost(), ]
    })   
    
    v5<- reactive({
        v5<- v4()[as.numeric(v4()$Earn)< Earn(), ]  
    })  
    
    
    v6<- reactive({
        v6<- v2()[v2()$CrimeRate < CrimeRate(), ]  
        
    })  
    
    v7<- reactive({
        
        v7<- v6()[v6()$HappyRank< HappyRank(), ]     
    })  
    
    
    v8<- reactive({
        v8<- v7()[v7()$confirmed_rate < confirmed_rate(), ]       
    })
    
    v3<- reactive({
        if (Citytype() == "All") {
            v3<- v8()} 
        else {
            v3<- v8()%>%
                filter(Citytype==Citytype()) 
        }})
    

    output$map <- renderLeaflet({
        
        urls <- paste0(as.character("<b><a href='http://"), as.character(v3()$URL), "'>", as.character(v3()$Name),as.character("</a></b>"))
        content <- paste(sep = "<br/>",
                         urls, 
                         paste("Rank:", as.character(v3()$ForbesRank))
        )
        
        mapStates = map("state", fill = TRUE, plot = FALSE)
        leaflet(data = mapStates) %>% addTiles() %>%
            addMarkers(v3()$Longitude, v3()$Latitude, 
                       popup = content, 
                       icon = list(iconUrl = 'https://cdn0.iconfinder.com/data/icons/education-flat-7/128/18_School_Building-512.png'
                                   ,iconSize = c(30,30)))
    })
    
    
    # table rank
    output$tablerank = DT:: renderDataTable({
        v3()[,c(3,4,5,14,27,28,30,31)]
    },options = list(orderClasses = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(width = '175px', targets = c(1)),
                                       list(width = '25px', targets = c(0,2)))))
    
    
    # cost plot
    output$cost <- renderPlotly({
        
        edu <- v3()
        fee <- edu %>% select(Name, AvgCost,rank_level)
        
        cost_plot=ggplot(fee, aes(x=AvgCost, color = rank_level))+
            geom_histogram(aes(y = ..density..),alpha = 0.7, fill = "#ff6c5f")+
            ggtitle("Density of Average Cost with Histogram overlay")+
            theme_set(theme_bw())+
            theme(panel.grid.major=element_line(colour=NA))+
            theme_classic() + 
            theme(axis.line = element_blank()) + 
            theme(legend.position="bottom")
        
        ggplotly(cost_plot) %>%
            layout(legend = list(bgcolor = "transparent",
                                 bordercolor = "transparent")) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')
    }
    )
    
    # crime plot
    
    
    output$crime <- renderPlotly({
        
        edu <- v3()
        crime <- edu %>% select(Name, CrimeRate,rank_level)
        crime$CrimeRate <- as.numeric(as.character(crime$CrimeRate))
        
        crime_plot=ggplot(crime, aes(x=CrimeRate,color = rank_level))+
            geom_histogram(aes(y = ..density..),alpha = 0.7, fill = "#56B4E9")+
            ggtitle("Density of CrimeRate with Histogram overlay")+
            theme_set(theme_bw())+
            theme(panel.grid.major=element_line(colour=NA))+
            theme_classic() + 
            theme(axis.line = element_blank()) + 
            theme(legend.position="bottom")
        
        ggplotly(crime_plot) %>%
            layout(legend = list(bgcolor = "transparent",
                                 bordercolor = "transparent")) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')
    }
    )
    
    # covid-19 plot
    
    
    output$covid<- renderPlotly({
        
        edu <- v3()
        covid <- edu %>% select(Name, confirmed_rate)
        covid$confirmed_rate<- as.numeric(as.character(covid$confirmed_rate))
        
        covid_plot=ggplot(covid, aes(x=confirmed_rate))+
            geom_histogram(aes(y = ..density..),alpha = 0.7, fill = "#ff4f81")+
            ggtitle("Density of confirmed_rate with Histogram overlay")+
            theme_set(theme_bw())+
            theme(panel.grid.major=element_line(colour=NA))+
            theme_classic() + 
            theme(axis.line = element_blank()) + 
            theme(legend.position="bottom")
        
        ggplotly(covid_plot) %>%
            layout(legend = list(bgcolor = "transparent",
                                 bordercolor = "transparent")) %>%
            layout(plot_bgcolor='transparent') %>%
            layout(paper_bgcolor='transparent')
    }
    )
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
