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
library(ggthemes)
library(plotly)
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
  
  # cols <- c("#00718E","greenyellow","#00FF00", "#00C639","#00AA55", 
  #             "midnightblue", "#0055AA", "#001CE3","blue4","blue",
  #             "#00FF66", "#00C222","#00AA88", 
  #             "yellow", "red", "purple","brown")
  
  # colmap <- c("#2c3e50", "#e67e22", "#f1c40f", "#e74c3c", "#F97F51", 
  #             "#27ae60", "#2980b9", "#8e44ad", "#8e44ad", "#95a5a6",
  #             "#f39c12", "#d35400", "#c0392b", "#bdc3c7", "#D6A2E8",
  #             "#25CCF7", "#16a085")
  
  colmap <- c("#f94144", "#f3722c", "#f8961e", "#f9c74f", "#90be6d", "#43aa8b", "#577590", 
              "#ffadad", "#ffd6a5", "#fdffb6", "#caffbf", "#9bf6ff", "#a0c4ff", "#bdb2ff",
              "#e5e5e5", "#ffffff", "#f07167")
    
    
    ggplot(pieDt, aes(x="", y=count, fill=university)) +
      geom_bar(stat="identity", width=10, color="white") +
      theme_void() +
      theme(legend.position="right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start=0) +
      scale_color_manual(values=colmap) +
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
    
    colmap <- c("#2c3e50", "#e67e22", "#f1c40f", "#e74c3c", "#F97F51",
                "#27ae60")
    
    ggplot(data=dent(), aes_string(x=input$selectvar)) + 
      geom_density(aes(fill=university), size=1) +
      theme(legend.position="bottom") + labs(x = input$selectvar) +
      scale_fill_manual(values=colmap) +
      theme_hc()
    
    
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
    
    colmap <- c("#2c3e50", "#e67e22", "#f1c40f", "#e74c3c", "#F97F51",
                "#27ae60", "#2980b9", "#8e44ad", "#8e44ad", "#95a5a6",
                "#f39c12", "#d35400", "#c0392b", "#bdc3c7", "#D6A2E8",
                "#25CCF7", "#16a085")
    
    p <- monthly %>% 
      data.frame() %>% 
      ggplot(., aes(x=reorder(school,basic_monthly_median_mean), 
                    y=basic_monthly_median_mean, fill=school)) + 
      geom_bar(stat="identity", width = 0.5) +
      scale_fill_manual(values=colmap) + theme_hc() 
      # xlab("Schools") + ylab(input$radio)
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
    
    colmap <- c("#2c3e50", "#e67e22", "#f1c40f", "#e74c3c", "#F97F51",
                "#27ae60")
    
    if (input$checkOutlier != TRUE){
      ggplot(data=uniMedian(), aes(x=university, y=basic_monthly_median, fill=university)) +

        geom_boxplot(color="black",size=1, width=0.3, outlier.shape = NA) +
        scale_fill_manual(values=colmap) +
        
        # xlab("University") + ylab("Basic Monthly Median of Graduates") +
        theme_hc() 

      
    } else {
      ggplot(data=uniMedian(), aes(x=university, y=basic_monthly_median, fill=university)) +
        geom_boxplot(color="black",size=1, width=0.3) +
        scale_fill_manual(values=colmap) +
        
        # xlab("University") + ylab("Basic Monthly Median of Graduates") +
        theme_hc() 
      

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
      ggplot(detailTB(), aes(x=year, y=basic_monthly_median)) +
        geom_smooth(mapping = aes(linetype = "r2"),
                    method = "lm",
                    formula = y ~ x + log(x), se = FALSE,
                    color = "#bdd5ea",
                    linetype = "dashed",
                    size = 2,
                    alpha = 0.5) +
        geom_line(aes(y=basic_monthly_median), size=2, color="#2c3e50") +
        geom_point(aes(x=year, y=basic_monthly_median), size=7, shape=21, 
                   colour="white", fill="#fca311", stroke=5) +
        
        theme_hc(base_size = 16, base_family = "sans", bgcolor = "default")
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



