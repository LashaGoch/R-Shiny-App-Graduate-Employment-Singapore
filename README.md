
## Advanced Programming in R 2020 Project 
![sg0.jpg](attachments/27cb9fe3.jpg)

Authors: Zimin Luo `417124` and Lashari Gochiashvili `425198`

### 

- This project has been deployed on [Shinyapps.io](https://gesurvey.shinyapps.io/Graduate-Employment-Survey/)
- The detailed documentation can be found [here](https://gesurvey.shinyapps.io/Graduate-Employment-Survey/).

-------

#### Description

This project is aimed at providing visualizations of graduates employment survey in Singapore. We selected two parameters: gross monthly income and full time employment rate, which we believe are to be of interest for most students. Users are able to check or compare gross monthly income and full time employment across different universities/programs/year.

The data is provided by Ministry of Education in Singapore based on graduates employment survey across different universities. We believe that this project can be served as a reference for senior students who are currently looking for jobs or a guideline for prospect students.

The data is available on [data.gov.sg](https://data.gov.sg/).
- [Graduate Employment Survey - NTU, NUS, SIT, SMU, SUSS & SUTD](https://data.gov.sg/dataset/graduate-employment-survey-ntu-nus-sit-smu-suss-sutd)
- [Universities - Intake, Enrolment and Graduates by Course](https://data.gov.sg/dataset/universities-intake-enrolment-and-graduates-by-course)

---

#### Workflow and Structure

##### Data Wrangling

The first step is devoted to data wrangling. In the raw dataset, the main problems are missing information, misplaced texts, duplicate contents, and redundant information. The main technique used is regex.

##### Shinyapps

We have prepared an interactive documentation of the shinyapp with GIFs, please visit [here](https://gesurvey.shinyapps.io/Graduate-Employment-Survey/).

```flowchart
st=>start: Start
e=>end: End
op1=>operation: a. Import & data wrangling: DataWrangling.R 
op2=>operation: b. Shinyapps: server.R, ui.R
op3=>operation: c. Documentation: about.Rmd, gesrmarkdown.Rmd
st->op1->op2->op3->e
```
---
#### Packages used

![technology.jpg](attachments/5e7963a2.jpg)

    [✘] 1. Writing own functions in R (including defensive programming)
    [✘] 2. Object-oriented programming - creating own classes, methods and generic functions of the S3, S4 and R6 systems 
    [✔︎] 3. Advanced data processing witsh dplyr, dtplyr, tidyr 
    [✔︎] 4. Automation of scripts and reports (RMarkdown) 
    [✔︎] 5. Shiny basics 
    [✔︎] 6. Creating analytical dashboards 
    [✘] 7. Use of C++ in R (Rcpp) 
    [✘] 8. Vectorization of the code 
    [✘] 9. Creating own R packages
    
