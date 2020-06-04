##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "GraduateEmployment",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "GraduatEmploymentSG",
      img(src = "sg0.jpg", width = "100%", class = "bg"),
    )),
    
    tags$br(),
    
    
    ##########################################
    ####  Panel: Main>Summary             ####
    ##########################################
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        ################################################
        #### Panel: Main>Summary>Tables & Pie Chart ####
        ################################################
        
        # ------------------
        # ranking $ pie chart section
        # ------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Data by Year"),
            tags$br(),
            selectInput(
              "checkYear",
              "Select Year",
              choices = list("2018", "2017", "2016",
                             "2015", "2014", "2013"),
              selected = "2018"
            )
          ),
          
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel("Ranking", tableOutput("datahead")),
              tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))
            ),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
        
        
        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------
            
            h3("Data Overview"),
            tags$br(),
            setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
            sliderInput(
              "incomeRange",
              label = "Salary Range",
              min = 1600,
              max = 5000,
              value = c(1600, 5000)
            ),
            # setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
            sliderInput(
              "employRange",
              label = "Employment Rate Range",
              min = 0,
              max = 100,
              value = c(0, 100)
            ),
            selectInput(
              "checkYearGroup",
              "Select Year",
              choices = data$year,
              selected = "2018",
              multiple = TRUE
            ),
            
            #checkboxGroupInput("checkYear", label = "Select Year",
            #                  choices = list("2013", "2014", "2015", "2016", "2017", "2018"),
            #                 selected = list("2013", "2014", "2015", "2016", "2017", "2018"), inline = TRUE),
            
            actionButton("actionDT", "Filter", class = "btn btn-warning"),
          ),
          mainPanel(
            h3("Browse All"),
            tags$br(),
            dataTableOutput("myTable"),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
      ),
      
      
      ################################################
      #### Panel: Main>Plots                      ####
      ################################################
      
      tabPanel(
        "Visual Comparison",
        
        # --------------------
        # density plot section
        # --------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Density Plot Panel"),
            tags$br(),
            selectInput(
              "selectvar",
              label = "Choose a variable to display",
              choices = c(
                "Basic Montly Salary (Median)" = "basic_monthly_median",
                "Fulltime Employment Rate" = "employment_rate_ft_perm"
              ),
              selected = "basic monthly mean"
            ),
            
            checkboxGroupInput(
              "checkGroup",
              label = "Select University",
              choices = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              ),
              selected = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              )
            ),
          ),
          mainPanel(
            h3("Distribution"),
            plotlyOutput(outputId = "densityPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # bar plot section
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Bar Plot Panel"),
            tags$br(),
            radioButtons(
              "radio",
              label = "Select University",
              choices = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              ),
              selected = "Nanyang Technological University"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Median Income by School (aggregate)"),
            plotlyOutput(outputId = "uniPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),
        
        # --------------------
        # box plot section
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Box Plot Panel"),
            tags$br(),
            checkboxGroupInput(
              "checkGroupbox",
              label = "Select University",
              choices = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              ),
              selected = list(
                "Nanyang Technological University" = "Nanyang Technological University",
                "National University of Singapore" = "National University of Singapore",
                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                "Singapore Management University" = "Singapore Management University",
                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                "Singapore University of Technology and Design" = "Singapore University of Technology and Design"
              )
            ),
            
            tags$hr()
          ),
          mainPanel(
            h3("Median Income Comparison (aggregate)"),
            plotlyOutput(outputId = "boxPlot"),
            tags$br(),
            tags$br(),
            tags$br(),
          )
        ),
        
        tags$hr(),
        
        # --------------------
        # Scatter plot section
        # --------------------
        
        
        fluidPage(fluidRow(
          h3("Fulltime Employment Rate vs. Median Income by University in 2018"),
          align = "center",
          plotlyOutput(outputId = "scatPlot", width = "100%"),
          div(style = "height:400px")
        )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        
      ),
      
      
      ################################################
      #### Panel: Main>Details                    ####
      ################################################
      
      tabPanel(
        "Details By University",
        h3("Graduates' Income and Employment Rate by Year", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(
                4,
                selectInput(
                  "detailUniversity",
                  label = "Select University",
                  choices = unique(data$university),
                  selected = "National University of Singapore",
                  width = 400
                ),
              ),
              column(
                4,
                selectInput(
                  "detailSchool",
                  "Select School",
                  choices = "",
                  selected = "",
                  width = 400
                )
              ),
              column(4,
                     column(
                       8,
                       selectInput(
                         "detailMajor",
                         "Select Program",
                         choices = "",
                         selected = "",
                         width = 400
                       )
                     ),
                     column(
                       4,
                       tags$br(),
                       actionButton("detailFilter", "Filter", class = "btn btn-warning btn-sm")
                     ))
            )),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        
        fluidRow(
          column(4, tableOutput("detailTable")),
          column(4, h5("Montly Median Income", align="center"), plotOutput(outputId = "detailPlot", height = "300px")),
          column(4, h5("Fulltime Employment rate", align="center"), plotOutput(outputId = "detailPlotem", height = "300px"))
        ),
        
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br()
      )
    )
  ),
  
  
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  tabPanel("Documentation",
           fluidPage(htmlOutput("doc"))),
  
  ################################################
  #### Panel: About                           ####
  ################################################
  tabPanel("About",
           fluidPage(htmlOutput("abo")))
)
