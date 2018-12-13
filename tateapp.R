library(DT)
library(shiny)
library(plotly)
library(kableExtra)
library(shinythemes)

ui <- fluidPage(theme=shinytheme("cyborg"),
                
  titlePanel("Tate Musuem Data Analysis Controls"),
  sidebarLayout(
    sidebarPanel(
      #the logo of tate 
     img(src="1.png", height = 250, width = 300,aligh="middle"), 
    
      #sidebar breaks for better compostion on the page
      br(),
      
      # Sidebar with a slider input for number of years
     sliderInput("bins", label = h3("Year"), min = 1700, 
                 max = 2013, value = c(1700, 1960), sep = ""),
     radioButtons("result",label = h3("Hypothesis Summary:"),
                  list("LinearRegression"="lm","AnovaTest"="anova")),
      #checkbox to select different variables for machine learning
      checkboxGroupInput("features", label = h3("Variables in Regression Model"), 
                         choices = list("Year" = "year",
                                        "Country" = "country",
                                        "Surface" = "surface",
                                        "Area" = "area",
                                        "Media" = "media"),
                         selected = "year"),
      #radio button to select result 
      fluidRow(column(12, verbatimTextOutput("value", placeholder = F)))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  #introduction of tate
                  tabPanel("Introduction of Tate",uiOutput("pic")), 
                  #create a dataset for further studies
                  tabPanel("Gender Composition Dataset",
                           list(tags$head(tags$style("body {color: #ADD8E6; }"))), 
                           DT::dataTableOutput("Gendergap")
                           
                           
                           ),
                  #draw plot to show difference of artwork acquisitions between two genders
                  tabPanel("The Gap of Acquisition Between Genders",plotOutput("comparasion")),
                  #create new variables called "difference" and "difference in percentage"
                  tabPanel("Table of Gender Difference", DT::dataTableOutput("Gendergap_new")),
                  #plot the result based on difference in numbers
                  tabPanel("Number Gender Difference Plot",plotlyOutput("numPlot")),
                  #plot the result based on difference in percentage
                  tabPanel("Gender Percentage Difference Plot",plotlyOutput("pecPlot")),
                  #hypothesis that want to be tested
                  tabPanel("Hypothesis1",
                           tags$style(type='text/css', '#description {background-color: rgba(0,0,0,1);color:rgba(0, 255, 0, 1); font-size: 30px
                          }'),
                          htmlOutput("description")),
                  #the linear regresion result and anova test
                  tabPanel("Hypothesis1: Summary", 
                           tags$style(type='text/css', '#summary {background-color: rgba(0,0,0,1);color:rgba(0, 255, 0, 1); font-size: 16px
                          }'),
                           verbatimTextOutput("summary")),
                  
                  # Medium Analysis
                  tabPanel("Medium Analysis", tableOutput("mediumTable"), verbatimTextOutput("medium_analysis")), 
                  #Material Analysis
                  tabPanel("Material Analysis", plotlyOutput("materialPlot"), verbatimTextOutput("material_analysis")),
                  #Surface Analysis
                  tabPanel("Surface Analysis", plotlyOutput("surfacePlot"), verbatimTextOutput("surface_analysis")),
                  #Year Analysis
                  tabPanel("Year Analysis", plotlyOutput("yearPlot"), verbatimTextOutput("year_analysis")),
                  #Hypothesis 2: testing the model
                  tabPanel("Hypothesis 2",
                           tags$style(type='text/css', '#categorycode {background-color: rgba(0,0,0,1);color:rgba(0, 255, 0, 1); font-size: 30px
                          }'),
                           htmlOutput("categorycode")),
                  #reduce the category for the model(show code)
                  tabPanel("Category Reduction", uiOutput("reduction")),
                  # Data as datatable for the model
                  tabPanel("ML Dataset", DT::dataTableOutput('tbl')), 
                  # select variables for the model
                  tabPanel("Hypothesis2: Model Summary",
                  tags$style(type='text/css', '#model_summary {background-color: rgba(0,0,0,1);color:rgba(0, 255, 0, 1); font-size: 16px
                                      }'),
                  # Regression output
                         verbatimTextOutput("model_summary"))
      )
    )
  ))



# SERVER
server <- function(input, output) {
  load("analysis_data.rdata")
  load("tate_clean.rdata")
  load("Gendergap.rdata")
  load("Gendergap_new.rdata")  
  
  myDataLoadFunction1 <- reactive({
    return(Gendergap)
  })
  
  myDataLoadFunction2 <- reactive({
    return(Gendergap_new)
  })
  
  myDataLoadFunction3 <- reactive({
    return(analysis_data)
  })
  
  #introduction of tate
  output$pic <- renderUI({
    pic<-("tate-modern-young-persons-membership-740x431@2x.jpg")
    tags$img(src = pic)
    
  }) 
  
  #Gender Composition Dataset
  output$Gendergap <- DT::renderDataTable({
    DataToDisplay <- myDataLoadFunction1()
  
    datatable(DataToDisplay) %>% formatStyle(
      'acquisitionYear',
      target = 'row',
      backgroundColor = 'black')
    
    
  }, escape = FALSE)
  
  #the Gap of Acquisition between Genders
  output$comparasion <- renderPlot({
    load("Gendergap.rdata")
    
    ggplot(Gendergap[Gendergap$acquisitionYear<=input$bins[2],],aes(x = acquisitionYear, y = log(count),colour=gender)) +
      geom_point() +
      stat_smooth() +
      geom_smooth() +
      xlab("Aquisition Year") +
      ylab("Number of artworks acquired each year") +
      ggtitle("The Gap of Aquisition Between Genders")
  })
  
  #Table of Gender Difference
  output$Gendergap_new <- DT::renderDataTable({
    DataToDisplay <- myDataLoadFunction2()
    datatable(DataToDisplay) %>% formatStyle( 'acquisitionYear', target = 'row', backgroundColor = 'black')
    
  }, escape = FALSE)
  
  
  #Number Gender Difference Plot
  output$numPlot <- renderPlotly({
    load("Gendergap_new.rdata")
    
    fit <- lm(Difference ~ acquisitionYear, data = Gendergap_new[Gendergap_new$acquisitionYear<=input$bins[2],])
    plot_ly(Gendergap_new[Gendergap_new$acquisitionYear<=input$bins[2],], x = ~acquisitionYear, y = ~Difference,type = "scatter",color= ~acquisitionYear,name = "Acquisition Year",showlegend=FALSE) %>%
      add_markers(y = ~Difference) %>%  
      add_lines(x=~acquisitionYear,y=fitted(fit),alpha = 0.2, name = "Regression Line", hoverinfo = "none")
    
  })
  
  
  #Gender Percengate Difference Plot
  output$pecPlot <- renderPlotly({
    load("Gendergap_new.rdata")
    
    fit <- lm(DifferenceOfPercentage ~ acquisitionYear, data = Gendergap_new[Gendergap_new$acquisitionYear<=input$bins[2],])
    plot_ly(Gendergap_new[Gendergap_new$acquisitionYear<=input$bins[2],], x = ~acquisitionYear, y = ~DifferenceOfPercentage,color= ~acquisitionYear,type = "scatter",showlegend=FALSE) %>%
      add_markers(y = ~DifferenceOfPercentage) %>%  
      add_lines(x=~acquisitionYear,y=fitted(fit))
    
  })
  
  
  #Hypothesis 1
  output$description <- renderUI({
    str1 <- paste("From year 1900 to year 2013,")
    str2 <- paste("The difference between the percentage of artwork acquired between different genders has decreased"
                
                  )
    HTML(paste(div(str1, str2), sep = '<br/>'))
    
   
  })
  
  #Hypothesis 1 Summary
  output$summary <- renderPrint({
    switch(input$result,lm=print(lm(Gendergap_new$DifferenceOfPercentage~Gendergap_new$acquisitionYear)),
           anova=anova( lm.differenceOfPercentage.years))
    
  }) 
  
  #Select variables
  output$value <- renderText({
    fmla <- paste("Formula \n gender ~", paste(input$features, collapse = " + "))
    fmla
  })
  
  
  #Medium Analysis
  output$mediumTable <- function(){
    medium <- tate %>% group_by(medium) %>% dplyr::summarize(count = n()) %>% arrange(desc(count)) %>% filter(medium != "")
    top_medium <- unname(head(medium, 10))
    colnames(top_medium) <- c("medium", "count")
    
    top_medium %>%       
      knitr::kable("html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                    font_size = 12,
                    full_width = F, 
                    position = "left")
    
  }
  
    # Material Analysis
  output$materialPlot <- renderPlotly({
    
    material_media <- data.frame("medium" = substring(tate$medium, 1, regexpr("on ", tate$medium) -1)) %>% 
      group_by(medium) %>% 
      dplyr::summarize(count = n()) %>% 
      arrange(desc(count)) %>% 
      filter(medium != "")
    
    top_material <- head(material_media, 10)
    
    save("top_material", file = "top_material.rdata")
    
        # table these media results
    x <- as.character(top_material$medium)
    y <- as.numeric(top_material$count)
    data <- data.frame(x, y)
    colnames(data) <- c("material", "count")
    
       # plot the material
    plot_ly(data, x = ~x, y = ~y, type = 'bar',
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)',
                                           width = 1.5))) %>%
      layout(title = "Top materials of artwork in Tate",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  
  # Surface Analysis
  output$surfacePlot <- renderPlotly({
    
    # divide media with surface
    surface_media <- data.frame("medium" = substring(tate$medium, regexpr("on ", tate$medium) +0)) %>% 
      group_by(medium) %>% dplyr::summarize(count = n()) %>% 
      arrange(desc(count)) %>% filter(medium != "")
    
    top_surface <- head(surface_media, 10)
    
    # table these surface results
    x <- as.character(top_surface$medium)
    y <- as.numeric(top_surface$count)
    data <- data.frame(x, y)
    colnames(data) <- c("surface", "count")
    
    # plot the surface
    plot_ly(data, x = ~x, y = ~y, type = 'bar',
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)',
                                           width = 1.5))) %>%
      layout(title = "Top surfaces of artwork in Tate",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
  
  })
 
  #Year Analysis
  output$yearPlot <- renderPlotly({
    ggplotly(ggplot(tate[tate$year >= input$bins[1],], aes(year)) + 
               geom_histogram(aes(y = ..density..), alpha = 0.9, fill = "#333333", binwidth = 1) + 
               geom_density(fill = "#ff4d4d", alpha = 0.5) + 
               theme(panel.background = element_rect(fill = '#ffffff')) + 
               ggtitle("Histogram of artwork era (year of artwork)"))
    
  })
  
  #Hypothesis 2 
  output$categorycode <- renderUI({
    str1 <- paste("From year 1900 to year 2013,")
    str2 <- paste("There exists a correlation between the features we explored and gender of the artist"
                  
    )
    HTML(paste(div(str1, str2), sep = '<br/>'))
    
    
  })
  
  #Category Reduction
  output$reduction <- renderUI({
    pic<-("reduction.jpg")
    tags$img(src = pic)
    
  })
  
  
  # ML Dataset 
  output$tbl = DT::renderDataTable({
    
    DT::datatable(analysis_data, options = list(lengthChange = FALSE))
    DataToDisplay <- myDataLoadFunction3()
    
    datatable(DataToDisplay) %>% formatStyle(
      'country',
      target = 'row',
      backgroundColor = 'black')
    
  })
  
  # Hypothesis 2: Model Summary
  output$model_summary <- renderPrint({
    fmla <- as.formula(paste("gender ~", paste(input$features, collapse = "+")))
    fit <- glm(fmla, data = analysis_data, family = binomial())
    summary(fit)
  })  
  
  
}

#run the app
shinyApp(ui = ui, server = server)