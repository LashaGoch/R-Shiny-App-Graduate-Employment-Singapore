##########################################
####   Main Libraries                 ####
##########################################
library(shiny)
library(DT)
library(rsconnect)
library(shinythemes)

##########################################
####   Shiny ui                       ####
##########################################

# ------------------
# Main title section
# ------------------

ui <- navbarPage("GraduateEmployment", theme = shinytheme("flatly"),
  tabPanel("Main",
  # App title ----
  titlePanel(div(column(width = 6, h1("Graduate Employment in Singapore", align="left")), 
                  column(width = 6, tags$img(src = "sg0.jpg", width = "100%")),
              windowTitle="GraduatEmploymentSG")),
  tags$br(),
  tags$br(),
  tags$br(),
  

##########################################
####  Panel: Main>Summary             ####
##########################################  

  tabsetPanel(type = "tabs",
      tabPanel("Summary",
        sidebarLayout(
               
               
         sidebarPanel(
           selectInput("checkYear", "Select Year", 
                       choices = list("2018", "2017", "2016", 
                                      "2015", "2014", "2013"), 
                       selected = "2018")),
      
    mainPanel(
      
################################################
#### Panel: Main>Summary>Tables & Pie Chart ####
################################################
      tabsetPanel(type = "tabs",
                  tabPanel("Ranking", tableOutput("datahead")),
                  tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))),
      tags$br(),
      tags$br(),
      tags$hr(),

# ------------------
# data table section
# ------------------

      )
),

  sidebarLayout(
    sidebarPanel(
      # ------------------
      # Data overview filters
      # ------------------
      
      h3("Data Overview"),
      tags$br(),
      sliderInput("incomeRange", label = "Salary Range", min=1600, max=5000, value=c(1600,5000)),
      sliderInput("employRange", label = "Employment Rate Range", min=0, max=100, value=c(0, 100)),
      selectInput("checkYearGroup", "Select Year", choices = data$year, selected = "2018",
                  multiple = TRUE),
      
      #checkboxGroupInput("checkYear", label = "Select Year", 
      #                  choices = list("2013", "2014", "2015", "2016", "2017", "2018"),
      #                 selected = list("2013", "2014", "2015", "2016", "2017", "2018"), inline = TRUE),
      
      actionButton("actionDT", "Filter"),
      tags$hr()
      
    ),
    mainPanel(
      h3("Data Table"),
      dataTableOutput("myTable"),
      tags$br(),
      tags$br(),
      tags$hr()
      
    )
    
  )


),
  

################################################
#### Panel: Main>Plots                      ####
################################################

tabPanel("Visual Comparison",
         
# --------------------
# density plot section
# --------------------
         sidebarLayout(
           sidebarPanel(
             h3("Density plot"),
             tags$br(),
             selectInput("selectvar",
               label = "Choose a variable to display",
               choices = c("Basic Montly Salary (Median)" = "basic_monthly_median",
                           "Fulltime Employment Rate" = "employment_rate_ft_perm"),
               selected = "basic monthly mean"),
             
             checkboxGroupInput("checkGroup", label = "Select University",
                                choices = list("Nanyang Technological University" = "Nanyang Technological University", 
                                               "National University of Singapore" = "National University of Singapore", 
                                               "Singapore Institute of Technology" = "Singapore Institute of Technology",
                                               "Singapore Management University" = "Singapore Management University",
                                               "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                                               "Singapore University of Technology and Design" = "Singapore University of Technology and Design"),
                                selected = list("Nanyang Technological University" = "Nanyang Technological University", 
                                                "National University of Singapore" = "National University of Singapore", 
                                                "Singapore Institute of Technology" = "Singapore Institute of Technology",
                                                "Singapore Management University" = "Singapore Management University",
                                                "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                                                "Singapore University of Technology and Design" = "Singapore University of Technology and Design")),),
           mainPanel(
             h3("Density Plot"),
             plotOutput(outputId = "densityPlot"),
             tags$br(),
             tags$br(),
             tags$hr())),
         
# --------------------
# bar plot section
# --------------------
         sidebarLayout(
           sidebarPanel(
             
             h3("Bar Plot"),
             tags$br(),
             radioButtons("radio", label = "Select University",
                          choices = list("Nanyang Technological University" = "Nanyang Technological University", 
                                         "National University of Singapore" = "National University of Singapore", 
                                         "Singapore Institute of Technology" = "Singapore Institute of Technology",
                                         "Singapore Management University" = "Singapore Management University"),
                          selected = "Nanyang Technological University"),
             tags$hr()),
           mainPanel(
             h3("Bar Plot"),
             plotOutput(outputId = "uniPlot"),
             tags$br(),
             tags$br(),
             tags$hr()
           )),
         
# --------------------
# box plot section
# --------------------
         sidebarLayout(
           sidebarPanel(
             h3("Box Plot"),
             tags$br(),
           checkboxInput("checkOutlier", label = "Show Outliers", value = TRUE),
           checkboxGroupInput("checkGroupbox", label = "Select University",
                              choices = list("Nanyang Technological University" = "Nanyang Technological University", 
                                             "National University of Singapore" = "National University of Singapore", 
                                             "Singapore Institute of Technology" = "Singapore Institute of Technology",
                                             "Singapore Management University" = "Singapore Management University",
                                             "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                                             "Singapore University of Technology and Design" = "Singapore University of Technology and Design"),
                              selected = list("Nanyang Technological University" = "Nanyang Technological University", 
                                              "National University of Singapore" = "National University of Singapore", 
                                              "Singapore Institute of Technology" = "Singapore Institute of Technology",
                                              "Singapore Management University" = "Singapore Management University",
                                              "Singapore University of Social Sciences" = "Singapore University of Social Sciences",
                                              "Singapore University of Technology and Design" = "Singapore University of Technology and Design")),
         
           tags$hr()),
          mainPanel(
           h3("Box Plot"),
           plotOutput(outputId = "boxPlot"),
           br()
         )),
# --------------------
# Scatter plot section
# --------------------

           fluidPage(h3("Scatter Plot"), align="center",
             plotOutput(outputId = "scatPlot", width = "80%"))),

################################################
#### Panel: Main>Details                    ####
################################################

tabPanel("Details By University", 
         h3("Graduates' Income and Employment Rate by Year"),
         br(),
         fluidRow(
           column(4,
                  selectInput("detailUniversity",
                    label = "Select University",
                    choices = unique(data$university),
                    selected = "National University of Singapore"),),
           column(4, selectInput("detailSchool", "Select School",
                              choices ="", selected = "")),
           column(4, selectInput("detailMajor", "Select Program",
                              choices ="", selected = ""))),
         fluidRow(
           column(4, actionButton("detailFilter", "Filter"),)),
         
         fluidRow(
           column(4, tableOutput("detailTable")),
           column(8, plotOutput(outputId="detailPlot", height = "300px"))),
         tags$br(),
         tags$br(),
         tags$hr()))),

################################################
#### Panel: Documentation                   ####
################################################

 tabPanel("Documentation",
         fluidPage(
           htmlOutput("doc"))),

################################################
#### Panel: About                           ####
################################################
  tabPanel("About",
          fluidPage(htmlOutput("abo")))
)
