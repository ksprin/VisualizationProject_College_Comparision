#load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)
library(readxl)
library(shinythemes)
library(ggwordcloud)
library(tidyr)
library(plotly)

#cleaning data code
my_data <- read_excel("uvarawdata.xlsx")
uva = subset(my_data, select = -c(3,5,7,8,9))
uva = uva %>% rename("GPA" = "Course GPA", "Term" = "Term Desc")
vt_data <- read.csv("virginiatechdata.csv")
vt = vt_data %>% mutate(Term= gsub("Summer I", "Summer",Term)) %>% mutate(Term= gsub("SummerI", "Summer",Term))
#Dataset%>% filter(Sport == "Basketball" & Age<19)
vt = vt %>% filter(Academic.Year == c("2018-19","2019-20","2020-21","2021-22","2022-23"))
#mutating fall columns
vtf2018 = vt %>% filter(Academic.Year == "2018-19") %>% filter(Term == "Fall") %>% mutate(Year= "2018 Fall" )
vtf2019 = vt %>% filter(Academic.Year == "2019-20") %>% filter(Term == "Fall") %>% mutate(Year= "2019 Fall" )
vtf2020 = vt %>% filter(Academic.Year == "2020-21") %>% filter(Term == "Fall") %>% mutate(Year= "2020 Fall" )
vtf2021 = vt %>% filter(Academic.Year == "2021-22") %>% filter(Term == "Fall") %>% mutate(Year= "2021 Fall" )
vtf2022 = vt %>% filter(Academic.Year == "2022-23") %>% filter(Term == "Fall") %>% mutate(Year= "2022 Fall" )
#mutating spring columns
vtsp2019 = vt %>% filter(Academic.Year == "2018-19") %>% filter(Term == "Spring") %>% mutate(Year= "2019 Spring" )
vtsp2020 = vt %>% filter(Academic.Year == "2019-20") %>% filter(Term == "Spring") %>% mutate(Year= "2020 Spring" )
vtsp2021 = vt %>% filter(Academic.Year == "2020-21") %>% filter(Term == "Spring") %>% mutate(Year= "2021 Spring" )
vtsp2022 = vt %>% filter(Academic.Year == "2021-22") %>% filter(Term == "Spring") %>% mutate(Year= "2022 Spring" )
vtsp2023 = vt %>% filter(Academic.Year == "2022-23") %>% filter(Term == "Spring") %>% mutate(Year= "2023 Spring" )
#mutating summer columns
vts2019 = vt %>% filter(Academic.Year == "2018-19") %>% filter(Term == "Summer") %>% mutate(Year= "2019 Summer" )
vts2020 = vt %>% filter(Academic.Year == "2019-20") %>% filter(Term == "Summer") %>% mutate(Year= "2020 Summer" )
vts2021 = vt %>% filter(Academic.Year == "2020-21") %>% filter(Term == "Summer") %>% mutate(Year= "2021 Summer" )
vts2022 = vt %>% filter(Academic.Year == "2021-22") %>% filter(Term == "Summer") %>% mutate(Year= "2022 Summer" )
vts2023 = vt %>% filter(Academic.Year == "2022-23") %>% filter(Term == "Summer") %>% mutate(Year= "2023 Summer" )

#standardizing the vt data to match uva's data format is
virginiatech = rbind(vtf2018, vtf2019, vtf2020, vtf2021, vtf2022, vtsp2019, vtsp2020, vtsp2021, vtsp2022, vtsp2023, vts2019, vts2020, vts2021, vts2022, vts2023)
virginiatech = subset(virginiatech, select = -c(Academic.Year,Term,Course.No.,CRN, Graded.Enrollment,Credits) )
virginiatech = rename(virginiatech, "Class Title" = 2, "A1" = 5, "A2" = 6, "B1" = 7, "B2" = 8, "B3" = 9, "C1" = 10, "C2" = 11, "C3" = 12, "D1"= 13,"D2"= 14,"D3"= 15, "F1"= 16, "W"= 17) %>% mutate(DFW= D1+D2+D3+F1)
virginiatech = subset(virginiatech, select = -c(D1,D2,D3,F1,W) )
virginiatech <- virginiatech[, c(13,1,2,3,4,5,6,7,8,9,10,11,12,14)]
virginiatech = mutate(virginiatech, "A" = as.integer(A1), "A-" = as.integer(A2), "B+" = as.integer(B1), "B"= as.integer(B2), "B-"=as.integer(B3), "C+"=as.integer(C1), "C"=as.integer(C2), "C-"=as.integer(C3))
virginiatech = subset(virginiatech, select = -c(A1,A2,B1,B2,B3,C1,C2,C3))


#standardizing the uva data to match tech's data format
calculations = rename(uva, "A1"=7, "num"=6)
calculations = mutate(calculations, A = A1+A)
calculations = subset(calculations, select = -c(A1))
calculations = rename(calculations, "A1"=7, "A2"=8, "B1"=9, "B2"=10, "B3"=11, "C1"=12, "C2"=13, "C3"=14)
calculations = mutate(calculations, "A" = as.integer(100*A1/num), "A-" = as.integer(100*A2/num), "B+" = as.integer(100*B1/num), "B"= as.integer(100*B2/num), "B-"=as.integer(100*B3/num), "C+"=as.integer(100*C1/num), "C"=as.integer(100*C2/num), "C-"=as.integer(100*C3/num))
calculations = subset(calculations, select = -c(A1,A2,B1,B2,B3,C1,C2,C3))
calculations <- calculations[, c(1,2,3,4,5,6,8,9,10,11,12,13,14,15,7)]
#Data$Years = substr(Data$Details, 1, 4)
calculations$year = substr(calculations$Term, 1, 4) 
calculations <- calculations[, c(16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
calculations = mutate(calculations, "class.size.range"="")
calculations <- calculations %>%
  rename(class_size = 7)
calculations$class_size_cat <- with(calculations, ifelse(class_size < 15, 'Less than 15', ifelse(class_size < 31, '15-30', ifelse(class_size < 61, '31-60', ifelse(class_size < 101, '61-100', ifelse(class_size < 201, '101-200', 'Over 200'))))))
wahoowa = calculations

uva <- wahoowa
vt <- virginiatech

write.csv(wahoowa, "uva2.csv")
write.csv(virginiatech, "vt2.csv")

#read in cleaned data
uva <- read.csv("uva2.csv")
vt <- read.csv("vt2.csv")

#changes to table for tab one
data<-uva
data$class_size_cat <- as.factor(data$class_size_cat)
data$GPA <- as.numeric(data$GPA)
data$GPA <- round(data$GPA,digits=1)

#changes to table for tab three
vt_df<-vt
uva_df<-uva
uva_df<-uva_df%>%mutate(avg_A= A/(A+A.+B.+B+B..1+C.+C+C..1+DFW))
vt_df<-vt_df%>%mutate(avg_A= A/(A+A.+B.+B+B..1+C.+C+C..1+DFW))
vt_df$Year <- gsub("\\s.*", "", vt_df$Year)
vt_df$Year <- as.numeric(vt_df$Year)


#shiny app

ui<-fluidPage(theme=shinytheme("flatly"),
              navbarPage("Comparison of UVA Classes",
                         tabPanel("UVA Class Size vs. Class Avg. GPA",
                                  sidebarPanel(
                                    selectInput("grade1","Select GPA or DFW:",
                                                choices = c("GPA","DFW"),
                                                selected="GPA")
                                    
                                    
                                  ),
                                  sidebarPanel(
                                    selectInput("class_size","Select class size:",
                                                choices = unique(data$class_size_cat)
                                    )
                                    
                                  ),
                                  sidebarPanel(
                                    sliderInput(inputId = "date1", label = "Select date:", min = min(data$year),max = max(data$year),value = 2021, step = 1, animate = F, sep="")
                                  ),
                                  mainPanel(plotlyOutput("graph_tab1"),
                                            p(strong("Figure 1: UVA class size and GPA/DFW rate." ), "This interactive chart allows you to pick between GPA and DFW (Drop, Fail, and Withdraw rate) and explore how class size and year affects GPA distributions or the DFW rate.")      
                                  )
                         ), #end of tab 1
                         
                         tabPanel("Classes with Lowest/Highest GPA: UVA vs. VT",
                                  fluidRow(
                                    column(6, 
                                           wellPanel(
                                             selectInput("input_def", "Upload .csv file or default", choices = c("Upload", "Default"), selected = "Upload"),
                                             fileInput(
                                               inputId = "csvFile",
                                               label = "Choose a csv file for UVA:", 
                                               accept = ".csv"
                                             ),
                                             selectInput("grade", "Choose Grade Metric", choices = c("GPA", "DFW"), selected = "GPA"),
                                             numericInput("size", "Max Size:", min = 10, max = 100, 
                                                          value = 7, step = 2),
                                             plotOutput("graph"),
                                             plotOutput("graph2")
                                           )
                                    ),
                                    column(6, 
                                           wellPanel(
                                             selectInput("input_def2", "Upload .csv file or default", choices = c("Upload", "Default"), selected = "Upload"),
                                             fileInput(
                                               inputId = "csvFile2",
                                               label = "Choose a csv file for VT:", 
                                               accept = ".csv"
                                             ),
                                             selectInput("grade2", "Choose Grade Metric", choices = c("GPA", "DFW"), selected = "GPA"),
                                             numericInput("size2", "Max Size:", min = 10, max = 100, 
                                                          value = 7, step = 2),
                                             plotOutput("graphVT"),
                                             plotOutput("graph2VT")
                                           )
                                    ),
                                    p(strong("Figure 2: UVA vs VT easiest/hardest classes."), " This interactive chart allows you to upload updated datasets when the original datasets from UVA and VT institutional research sites are updated with future semesters data. The dashboard creates a wordcloud with the easiest and hardest classes at VT and UVA based on DFW rate or GPA, based on user selection. The max size widget increases the wordcloud’s text size to improve readability and accessibility.")
                                  )
                                  
                         ), #end of tab 2
                         tabPanel("UVA Department vs. Avg. GPA",
                                  fluidRow(
                                    column(6,
                                           wellPanel(
                                             selectInput(
                                               inputId = "x",
                                               label = "X-Axis:",
                                               choices= c("GPA","avg_A","DFW")
                                             ),
                                             selectInput(
                                               inputId = "dept_selector",
                                               label = "Select dept:",
                                               choices = c("All Dept",unique(uva_df$Subject))
                                             ),
                                             sliderInput(
                                               inputId = "year_selector",
                                               label = "Select Year Range:",
                                               min = min(uva_df$year),
                                               max = max(uva_df$year),
                                               value = c(min(uva_df$year), max(uva_df$year)),
                                               step = 1,
                                               sep = ""
                                             ),
                                             plotOutput("gpa_plot_uva",click = "plot_click_uva"),
                                             verbatimTextOutput("information_uva")
                                           )),
                                    column(6,
                                           wellPanel(
                                             selectInput(
                                               inputId = "x_vt",
                                               label = "X-Axis:",
                                               choices = c("GPA","avg_A","DFW")
                                             ),
                                             selectInput(
                                               inputId = "dept_selector_vt",
                                               label = "select VT Dept:",
                                               choices = c("All Dept VT",unique(vt_df$Subject))
                                             ),
                                             sliderInput(
                                               inputId = "year_selector_vt",
                                               label = "Select Year Range:",
                                               min = min(vt_df$Year),
                                               max = max(vt_df$Year),
                                               value = c(min(vt_df$Year), max(vt_df$Year)),
                                               step = 1,
                                               sep = ""
                                             ),
                                             plotOutput("gpa_plot_vt",click = "plot_click_vt"),
                                             verbatimTextOutput("information_vt")
                                           )),
                                    p(strong("Figure 3: UVA vs VT GPA/DFW rate by department."), "This interactive chart allows you to pick the metric of focus, GPA or DFW rate, and compare departments between VT and UVA, or conduct an overall comparison. Click on either graph to show the value of the X-axis at that point.")
                                  )
                                  
                         ) #end of tab 3
                         
              )
)

server<-function(input,output){
  #for tab 1
  filtered_data1 <- reactive({
    req(input$date1)
    data %>%
      filter(class_size_cat %in% input$class_size)%>%
      filter(year %in% input$date1)
    
  })
  
  chart_color <- reactive({
    if (input$grade1 == "GPA"){
      "orange"
    }
    else{
      "navyblue"
    }
  })
  
  output$graph_tab1<-renderPlotly({
    ggplot(filtered_data1(),aes_string(x=input$grade1,fill="class_size_cat")) + geom_bar(fill = chart_color()) + labs(x=input$grade1,fill = "Class Size")
    ggplotly(ggplot(filtered_data1(), aes_string(x=input$grade1,fill="class_size_cat"))+geom_bar(fill = chart_color())+ labs(x=input$grade1, fill = "Class Size" ))
  }
  )
  
  #for tab 2
  Dataset <- reactive({
    if (input$input_def == "Upload"){
      if(is.null(input$csvFile)){
        uva
      }
      else{
        req(input$csvFile)
        read.csv(input$csvFile$datapath)
      }
    }
    else{
      uva
    }
  })
  
  Dataset2 <- reactive({
    if (input$input_def2 == "Upload"){
      if(is.null(input$csvFile2)){
        vt
      }
      else{
        req(input$csvFile2)
        read.csv(input$csvFile2$datapath)
      }
    }
    else{
      vt
    }
  })
  
  output$graph <- renderPlot({
    sorted_data <- Dataset() %>%
      arrange(desc(get(input$grade))) %>%
      head(10)
    ggplot(sorted_data, aes(label = sorted_data$Class.Title, size = sorted_data$grade, color = sorted_data$Class.Title)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = input$size) +
      scale_color_viridis_d(option = "inferno") +
      ggtitle("Easiest Classes Based on GPA/ Toughest Classes based on DFW")
  })
  
  output$graph2 <- renderPlot({
    sorted_data1 <- Dataset() %>%
      arrange(get(input$grade)) %>%
      head(10)
    ggplot(sorted_data1, aes(label = sorted_data1$Class.Title, size = sorted_data1$grade, color = sorted_data1$Class.Title)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = input$size) +
      scale_color_viridis_d(option = "turbo") +
      ggtitle("Toughest Classes Based on GPA/ Easiest Classes based on DFW")
  })
  
  output$graphVT <- renderPlot({
    sorted_data2 <- Dataset2() %>%
      arrange(desc(get(input$grade2))) %>%
      head(10)
    ggplot(sorted_data2, aes(label = sorted_data2$Class.Title, size = sorted_data2$grade, color = sorted_data2$Class.Title)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = input$size2) +
      scale_color_viridis_d(option = "inferno") +
      ggtitle("Easiest Classes Based on GPA/ Toughest Classes based on DFW")
  })
  
  output$graph2VT <- renderPlot({
    sorted_data3 <- Dataset2() %>%
      arrange(get(input$grade2)) %>%
      head(10)
    ggplot(sorted_data3, aes(label = sorted_data3$Class.Title, size = sorted_data3$grade, color = sorted_data3$Class.Title)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = input$size2) +
      scale_color_viridis_d(option = "turbo") +
      ggtitle("Toughest Classes Based on GPA/ Easiest Classes based on DFW")
  })
  
  #for tab3
  observe({
    if (input$year_selector[1] == input$year_selector[2]) {
      updateSliderInput(session, "year_selector", value = c(input$year_selector[1] - 1, input$year_selector[2] + 1))
    }
  })
  
  observe({
    if (input$year_selector_vt[1] == input$year_selector_vt[2]) {
      updateSliderInput(session, "year_selector_vt", value = c(input$year_selector_vt[1] - 1, input$year_selector_vt[2] + 1))
    }
  })
  
  filtered_data <- reactive({
    if (input$dept_selector == "All Dept") {
      filtered <- uva_df
    } else {
      filtered <- subset(uva_df, Subject == input$dept_selector)
    }
    filtered[filtered$year >= input$year_selector[1] &
               filtered$year <= input$year_selector[2], ]
  })
  
  filtered_data_vt <- reactive({
    if (input$dept_selector_vt == "All Dept VT") {
      filtered_vt <- vt_df
    } else {
      filtered_vt <- subset(vt_df, Subject == input$dept_selector_vt)
    }
    filtered_vt[filtered_vt$Year >= input$year_selector_vt[1] &
                  filtered_vt$Year <= input$year_selector_vt[2], ]
  })
  
  output$gpa_plot_uva <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$x)) + geom_boxplot(color = "navyblue", fill = "orange")
  })
  
  output$gpa_plot_vt <- renderPlot({
    ggplot(filtered_data_vt(), aes_string(x = input$x_vt)) + geom_boxplot(color = "firebrick4", fill = "darkorange2")
  })
  
  output$information_uva<-renderPrint({
    paste0("X value = ", input$plot_click_uva$x)
  })
  
  output$information_vt<-renderPrint({
    paste0("X value = ", input$plot_click_vt$x)
  })
}

shinyApp(ui,server)
