##########################################
####   Main Libraries                 ####
##########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(kableExtra)

##########################################
####   Attaching datasets             ####
##########################################

# data <- readRDS("data/employment_data.rds")

data <- read.csv("data/employment_data1.csv")
data_g <- readRDS("data/graduates_by_institutions.rds")


## Setting datatables view

opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(targets = c(1, 6), searchable = FALSE))
)


##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {

################################################
#### Panel: Main>Summary>Tables & Pie Chart ####
################################################

# ----------------
# Summary section
# ----------------
  
  output$datahead <- renderPrint({
    data %>%
      filter(year%in%input$checkYear) %>%
      group_by(university) %>%
      arrange(desc(gross_monthly_median)) %>% 
      summarise_at(.vars = names(.)[8], list(min, max)) %>%
      kable("html", col.names = c("University", "Min Median Monthly Income", 
                                  "Max Median Monthly Income")) %>%
      kable_styling(c("striped","hover"), full_width = T)
    
  })

# ----------------
# pie plot section
# ----------------    

  output$piePlot <- renderPlot({
    pieDt <- data_g %>% 
    filter(year==max(input$checkYear)) %>% 
    group_by(university) %>% 
    tally(graduates)
  
  colnames(pieDt) <- c("university", "count")
  
  cols <- c("#00718E","greenyellow","#00FF00", "#00C639","#00AA55", 
              "midnightblue", "#0055AA", "#001CE3","blue4","blue",
              "#00FF66", "#00C222","#00AA88", 
              "yellow", "red", "purple","brown")
    
    
    ggplot(pieDt, aes(x="", y=count, fill=university)) +
      geom_bar(stat="identity", width=1, color="white") +
      theme_void() +
      theme(legend.position="right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start=0) +
      scale_fill_manual(values=cols) +
      labs(title = "Universities by number of graduates")
    
  })
  
  # ------------------
  # data table section
  # ------------------
  
  # filter the checkgroup input:
  
  yearGroup <- reactive({
    input$actionDT
    isolate(
      return(data[data$year%in%input$checkYear, ])
    )
  })
  
  
  filtered_DT <- reactive({
    input$actionDT
    isolate({
      minIncome <- input$incomeRange[1]
      maxIncome <- input$incomeRange[2]
      minEmploy <- input$employRange[1]
      maxEmploy <- input$employRange[2]
    })
    
    yearGroup() %>% 
      filter(basic_monthly_median > minIncome, basic_monthly_median < maxIncome) %>% 
      filter(employment_rate_ft_perm > minEmploy, employment_rate_ft_perm < maxEmploy) %>%
      select(1,2,3,4,6, 8) 
  })
  
  # render DT:
  
  output$myTable <- renderDataTable({
    
    filtered_DT() %>% 
      datatable(., rownames = FALSE, class = "table", options = list(pageLength = 5, scrollX=T), 
                colnames = c("Year", "University", "School", "Degree", "Fulltime Employment Rate", "Basic Monthly Salary(Median)"))
    
  })
  
  
################################################
#### Panel: Main>Plots                      ####
################################################
  
# --------------------
# density plot section
# --------------------
  
  # filter the checkgroup input:
  
  dent <-  reactive({
    
    return(data[data$university%in%input$checkGroup, ])
    
  })
  
  # render density plot
  
  output$densityPlot <- renderPlot({
    
    ggplot(data=dent(), aes_string(x=input$selectvar)) + 
      geom_density(aes(colour = university, fill=university), alpha=0.5) +
      theme(legend.position="bottom") + labs(x = input$selectvar)
    
    
  })
  
  
# ----------------
# bar plot section
# ----------------
  
  # filter the input and group data:
  
  output$uniPlot <- renderPlot({
    
    monthly <- data %>% 
      filter(university==input$radio) %>% 
      group_by(school) %>% 
      summarise_at(.vars = names(.)[7:8],.funs = c(mean="mean"))
    
    # render bar plot
    
    p <- ggplot(data.frame(monthly), aes(x=reorder(school,basic_monthly_median_mean), 
                                         y=basic_monthly_median_mean)) + 
      geom_bar(stat="identity", fill="steelblue", alpha=0.7) +
      xlab("Schools") + ylab(input$radio)
    p + coord_flip() 
    
  })

# ----------------
# box plot section
# ----------------
  
  # filter the checkgroup input:
  
  uniMedian <-  reactive({
    return(data[data$university%in%input$checkGroupbox, ])
  })
  
  # render box plot
  
  output$boxPlot <- renderPlot({
    
    if (input$checkOutlier != TRUE){
      p <- ggplot(data=uniMedian(), 
                  aes(x=university, y=basic_monthly_median)) +
        geom_boxplot(fill="steelblue", alpha=0.5, outlier.shape = NA) +
        xlab("University") + ylab("Basic Monthly Median of Graduates")
      
      p + coord_flip()
    } else {
      p <- ggplot(data=uniMedian(), 
                  aes(x=university, y=basic_monthly_median)) +
        geom_boxplot(fill="steelblue", alpha=0.5) +
        xlab("University") + ylab("Basic Monthly Median of Graduates")
      
      p + coord_flip()
    }
})
  
  
# ----------------
# scatter plot section
# ----------------
  
  output$scatPlot <- renderPlot({
    
    ggplot(data, aes(x=basic_monthly_median, y=employment_rate_ft_perm, 
                     color=university, shape=as.factor(year))) +
      theme(legend.position = "bottom") + 
      geom_smooth(mapping = aes(linetype = "r2"),
                  method = "lm",
                  formula = y ~ x + log(x), se = FALSE,
                  color = "grey") +
      geom_point(size=2)
  })
  
  
################################################
#### Panel: Main>Details                    ####
################################################
  
  
  observeEvent(
    input$detailUniversity,
    updateSelectInput(session, "detailSchool", "Choose a School",
                      choices = unique(data$school[data$university==input$detailUniversity]))
  )
  observeEvent(
    input$detailSchool,
    updateSelectInput(session, "detailMajor", "Choose a Program",
                      choices = unique(data$degree[data$school==input$detailSchool & 
                                                     data$university==input$detailUniversity]))
  )
  
  detailTB <- eventReactive(
    input$detailMajor,
    {
      data %>% 
        filter(school==input$detailSchool & 
                 university==input$detailUniversity & 
                 degree==input$detailMajor) %>% 
        select(c("year", "basic_monthly_median", "employment_rate_ft_perm")) 
      
    })
  
  output$detailTable <- renderPrint({
    input$detailFilter
    
    isolate({
      detailTB() %>% 
        data.frame() %>% 
        kable("html", col.names = c("Year", "Median Montly Income", 
                                    "Fulltime Employment Rate")) %>%
        kable_styling(c("striped","hover"), full_width = F) 
    })
  })
  
  
  output$detailPlot <- renderPlot({
    input$detailFilter
    
    isolate({
      ggplot(detailTB(), aes(x=year)) +
        geom_line(aes(y=basic_monthly_median), size=1, color="#69b3a2") +
        geom_point(aes(y=basic_monthly_median), size=3, shape=17)
    })
    
  })
  
################################################
#### Panel: Documentation                   ####
################################################

  getPageDoc<-function() {
    return(includeHTML("gesrmarkdown.html"))
  }
  output$doc<-renderUI({getPageDoc()})
  
  
################################################
#### Panel: About                           ####
################################################

  getPageAbo<-function() {
    return(includeHTML("about.html"))
  }
  output$abo<-renderUI({getPageAbo()})
  
  
}



