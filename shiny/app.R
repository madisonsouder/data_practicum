#Madison Souder
#Data Practicum - Shiny App

#Packages
library(shiny)
library(tidyverse)
library(magrittr)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(fable)
library(forecast)

#Data-sets
state <- read.csv("Vaccination_State_Data.csv", check.names = FALSE)
national <- read.csv("Vaccination_National_Data.csv", check.names = FALSE)
all <- read.csv("All_Vaccination_Data.csv", check.names = FALSE)

WHO_vax <- read.csv("WHO_vaccination_rates.csv", check.names = FALSE)
cases <- read.csv("preventable_disease_cases.csv", check.names = FALSE)

regression <- read.csv("regression_data.csv", check.names = FALSE)

dip_data <- read.csv("diptheria_model_data.csv", check.names = FALSE)
tetanus_data <- read.csv("tetanus_model_data.csv", check.names = FALSE)
pertussis_data <- read.csv("pertussis_model_data.csv", check.names = FALSE)
rubella_data <- read.csv("rubella_model_data.csv", check.names = FALSE)
measles_data <- read.csv("measles_model_data.csv", check.names = FALSE)


#Creating abbreviations for the States
state_abbr <- data.frame(state.name = state.name, state.abb = state.abb)

state <- state %>%
  left_join(state_abbr, by = c("Geography" = "state.name")) 

all <- all %>%
  left_join(state_abbr, by = c("Geography" = "state.name"))

#Variables for Filters
all_vaccinations <- all %>%
  filter(`Vaccine/Exemption` != "Exemption") %>%
  pull(`Vaccine/Exemption`) %>%
  unique()

all_exemptions <- all %>%
  filter(`Vaccine/Exemption`== "Exemption") %>%
  pull(Dose) %>%
  unique()

all_states <- unique(state$state.abb)
all_years <- unique(all$`School Year`)

#Splits the dose_type into two columns
WHO_vax %<>% 
  separate(dose_type, into = c("vaccine", "dose"), sep = ", ")

WHO_vax$dose[WHO_vax$dose == "birth dose"] <- "1st dose"

vaccinations_WHO <- unique(WHO_vax$vaccine)
dose_WHO <- unique(WHO_vax$dose)

years_WHO <- unique(WHO_vax$year)

all_diseases <- unique(cases$disease)
years_cases <- unique(cases$year)

#Define UI
ui <- navbarPage("United States Vaccination Compliance",
            
  tabPanel("Compliance Rates",
    sidebarLayout(
      sidebarPanel(
        #Checkbox for Vaccinations
        checkboxGroupButtons(
          inputId = "checkbox_WHO_vax", 
          label = h4("Vaccinations"), 
          choices = vaccinations_WHO, 
          selected = vaccinations_WHO, 
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"), 
            no = icon("remove", lib = "glyphicon"))),

        #Drop-down for year
        pickerInput(
          inputId = "picker_WHO_year", 
          label = h4("Year"), 
          choices = years_WHO,
          selected = years_WHO,
          multiple = TRUE, 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"))
        
      ),
      mainPanel( 
        fluidRow(
          plotlyOutput("plotly_cr_1"),
          plotlyOutput("plotly_cr_2"),
          plotlyOutput("plotly_cr_3"),
          plotlyOutput("plotly_cr_4"),
          plotlyOutput("plotly_cr_5"))
        
      )
    )
  ),
  
  tabPanel("School Mandated Vaccination Compliance",
           sidebarLayout(
             sidebarPanel(
               #Checkbox for Vaccinations
               checkboxGroupButtons(
                 inputId = "checkbox_vax", 
                 label = h4("Vaccinations"), 
                 choices = all_vaccinations, 
                 selected = all_vaccinations, 
                 checkIcon = list(
                   yes = icon("ok", lib = "glyphicon"), 
                   no = icon("remove", lib = "glyphicon"))),
               
               #Checkbox for Exemptions
               checkboxGroupButtons(
                 inputId = "checkbox_exemp", 
                 label = h4("Exemptions"), 
                 choices = all_exemptions, 
                 selected = all_exemptions, 
                 checkIcon = list(
                   yes = icon("ok", lib = "glyphicon"), 
                   no = icon("remove", lib = "glyphicon"))),
               
               #Drop-down for States
               pickerInput(
                 inputId = "picker_state", 
                 label = h4("States"), 
                 choices = all_states,
                 selected = all_states,
                 multiple = TRUE, 
                 options = list(
                   `actions-box` = TRUE, 
                   size = 10,
                   `selected-text-format` = "count > 3")),
               
               #Drop-down for School Year
               pickerInput(
                 inputId = "picker_year", 
                 label = h4("School Year"), 
                 choices = all_years,
                 selected = all_years,
                 multiple = TRUE, 
                 options = list(
                   `actions-box` = TRUE, 
                   size = 10,
                   `selected-text-format` = "count > 3")),
               
               br(),
               br(),
               
             ),
             mainPanel(
               fluidRow(
                plotlyOutput("plotly_k_1"),
                plotlyOutput("plotly_k_2"),
                plotlyOutput("plotly_k_3"))
               )
           )
  ),
  tabPanel("Reported Cases",
           sidebarLayout(
             sidebarPanel(
               #Checkbox for Diseases
               checkboxGroupButtons(
                 inputId = "checkbox_diseases", 
                 label = h4("Diseases"), 
                 choices = all_diseases, 
                 selected = all_diseases, 
                 checkIcon = list(
                   yes = icon("ok", lib = "glyphicon"), 
                   no = icon("remove", lib = "glyphicon"))),
               
               #Drop-down for year
               pickerInput(
                 inputId = "picker_case_year", 
                 label = h4("Year"), 
                 choices = years_cases,
                 selected = years_cases,
                 multiple = TRUE, 
                 options = list(
                   `actions-box` = TRUE, 
                   size = 10,
                   `selected-text-format` = "count > 3"))
               
             ),
             mainPanel(
               fluidRow(
                plotlyOutput("plotly_c_1"),
                plotlyOutput("plotly_c_2"))
               
             )
           )
  ),
  
  tabPanel("Future",
           sidebarLayout(
             sidebarPanel(
               h2("Change the compliance:"),
               p("Prediction models were built bases on the amount of reported 
                 cases and the compliance rates (%). Changing the coverage values
                 will predict how many cases of each disease are likely to occur."),
               
               br(),
               
               sliderInput("coverage_1st", "1st Dose Coverage (%):", 
                            value = 90, min = 0, max = 100),
               sliderInput("coverage_2nd", "2nd Dose Coverage (%):", 
                            value = 85, min = 0, max = 100),
               sliderInput("coverage_3rd", "3rd Dose Coverage (%):", 
                            value = 80, min = 0, max = 100)
               
             ),
             mainPanel(
               fluidRow(
                 h3("Diphtheria, Total Tetanus, and Pertussis"),
                 p("Herd Immunity for Diphteria = 75% - 80%"),
                 p("Herd Immunity for Pertussis = 90% - 94%"),
                 plotOutput("plot_f_1"),
                 verbatimTextOutput("dip_predict"),
                 verbatimTextOutput("tetanus_predict"),
                 verbatimTextOutput("pertussis_predict"),
                 
                 h3("Measles"),
                 p("Herd Immunity = 91% - 94%"),
                 plotOutput("plot_f_2"),
                 verbatimTextOutput("measles_predict"),
                 
                 h3("Rubella"),
                 p("Herd Immunity = 83% - 94%"),
                 plotOutput("plot_f_3"),
                 verbatimTextOutput("rubella_predict"),
                 
                 h3("Hepatitis B"),
                 plotOutput("plot_f_7"),
                 
                 h3("Polio"),
                 plotOutput ("plot_f_8"),
                 
                 h3("Kindergarten Vaccination Rates"),
                 plotOutput("plot_f_4"),
                 plotOutput("plot_f_5"),
                 
                 h3("Kindergarten Exemption Rates"),
                 plotOutput("plot_f_6")
                 )
               )
             )
  )
)

#Define Server
server <- function(input, output, session){
  
  #Filters for Compliance 1st Line Chart
  line_WHO_comp_1 <- reactive ({
    WHO_vax %>%
      filter(
        vaccine %in% input$checkbox_WHO_vax,
        year %in% input$picker_WHO_year,
        dose == "1st dose") %>%
      group_by(year, vaccine) %>%
      summarize(Avg_Estimate = mean(coverage, na.rm = TRUE), 
                .groups = "drop")
    })
  
  #Compliance 1st Dose Line Chart
  output$plotly_cr_1 <- renderPlotly({
    ggplot(line_WHO_comp_1(), aes(x = year,
                                y = Avg_Estimate,
                                color = vaccine)) +
      geom_line() +
      geom_point() +
      labs(title = "Vaccination Compliance (1st Dose)",
           x = "Year",
           y = "Compliance (%)",
           color = "Vaccine") +
      scale_color_manual(values = c(
        "DTP-containing vaccine" = "red",
        "HepB" = "orange",
        "Inactivated polio-containing vaccine" = "dodgerblue",
        "Measles-containing vaccine" = "darkgreen",
        "Rubella-containing vaccine" = "lightgreen")) +
      theme_bw()
    
    ggplotly()
    })
  
  #Filters for Compliance 1st Line Chart
  line_WHO_comp_2 <- reactive ({
    WHO_vax %>%
      filter(
        vaccine %in% input$checkbox_WHO_vax,
        year %in% input$picker_WHO_year,
        dose == "2nd dose") %>%
      group_by(year, vaccine) %>%
      summarize(Avg_Estimate = mean(coverage, na.rm = TRUE), 
                .groups = "drop")
  })
  
  #Compliance 1st Dose Line Chart
  output$plotly_cr_2 <- renderPlotly({
    ggplot(line_WHO_comp_2(), aes(x = year,
                                  y = Avg_Estimate,
                                  color = vaccine)) +
      geom_line() +
      geom_point() +
      labs(title = "Vaccination Compliance (2nd Dose)",
           x = "Year",
           y = "Compliance (%)",
           color = "Vaccine") +
      scale_color_manual(values = c(
        "Inactivated polio-containing vaccine" = "dodgerblue",
        "Measles-containing vaccine" = "darkgreen")) +
      theme_bw()
    
    ggplotly()
  })
  
  #Filters for Compliance 3rd Line Chart
  line_WHO_comp_3 <- reactive ({
    WHO_vax %>%
      filter(
        vaccine %in% input$checkbox_WHO_vax,
        year %in% input$picker_WHO_year,
        dose == "3rd dose") %>%
      group_by(year, vaccine) %>%
      summarize(Avg_Estimate = mean(coverage, na.rm = TRUE), 
                .groups = "drop")
  })
  
  #Compliance 3rd Dose Line Chart
  output$plotly_cr_3 <- renderPlotly({
    ggplot(line_WHO_comp_3(), aes(x = year,
                                  y = Avg_Estimate,
                                  color = vaccine)) +
      geom_line() +
      geom_point() +
      labs(title = "Vaccination Compliance (3rd Dose)",
           x = "Year",
           y = "Compliance (%)",
           color = "Vaccine") +
      scale_color_manual(values = c(
        "DTP-containing vaccine" = "red",
        "HepB" = "orange",
        "Polio" = "dodgerblue")) +
      theme_bw()
    
    ggplotly()
    })
  
  #Total for Kindergarten Vaccinations
  average <- all %>%
    filter(
      `Vaccine/Exemption` != "Exemption",
      Geography != "U.S. Median") %>%
    group_by(`School Year`) %>%
    summarize(Avg_Estimate = mean(`Estimate (%)`, na.rm = TRUE), 
              .groups = "drop") %>%
    mutate(`Vaccine/Exemption` = "Total Average")
  
  #Filters for Kindergarten Vaccination Line Chart
  line_vax <- reactive({
    all %>%
      filter(
        `Vaccine/Exemption` %in% input$checkbox_vax,
        state.abb %in% input$picker_state,
        `School Year` %in% input$picker_year,
        `Vaccine/Exemption` != "Exemption",
        Geography != "U.S. Median") %>%
      group_by(`School Year`, `Vaccine/Exemption`) %>%
      summarize(Avg_Estimate = mean(`Estimate (%)`, na.rm = TRUE), 
                .groups = "drop")
  })
  
  #Kindergarten Vaccination Line Chart
  output$plotly_k_1 <- renderPlotly({
    combined_data <- bind_rows(line_vax(), average) %>%
      mutate(`School Year` = factor(`School Year`, 
                                    levels = unique(`School Year`)))
    
    ggplot(combined_data, aes(x = `School Year`, 
                              y = Avg_Estimate,
                              color = `Vaccine/Exemption`,
                              group = `Vaccine/Exemption`)) +
      geom_line() +
      geom_point() +
      labs(title = "Kindergarten Vaccination Compliance",
           x = "School Year",
           y = "Compliance (%)",
           color = "Vaccine") +
      scale_color_manual(values = c(
        "DTP, DTaP, or DT" = "red",
        "Hepatitis B" = "orange",
        "MMR" = "forestgreen",
        "Polio" = "dodgerblue",
        "Varicella" = "purple")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16))
    
    ggplotly()
  })
  
  #Filters for Exemption Line Chart
  line_exemp <- reactive({
    all %>%
      filter(Dose %in% input$checkbox_exemp) %>%
      filter(state.abb %in% input$picker_state) %>%
      filter(`School Year` %in% input$picker_year) %>%
      
      filter(`Vaccine/Exemption` == "Exemption") %>%
      filter(Geography != "U.S. Median") %>%
      group_by(`School Year`, Dose) %>%
      summarise(avg_exemption_rate = mean(`Estimate (%)`, na.rm = TRUE))%>%
      mutate(`School Year` = factor(`School Year`, 
                                    levels = unique(`School Year`))) 
    
  })
  
  #Line plot for Exemption rate
  output$plotly_k_2 <- renderPlotly({
    ggplot(line_exemp(), aes(x = `School Year`, 
                             y = avg_exemption_rate,
                             color = Dose)) +
      geom_line(group = 1) +
      geom_point() +
      labs(title = "Change in Kindergarten Exemption Rates",
           x = "School Year",
           y = "Average Exemption Rate (%)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16)) 
    ggplotly()
  })
  
  #Filters for Kindergarten Map plot
  rate_map <- reactive({
    state %>% 
      filter(`Vaccine/Exemption` %in% input$checkbox_vax,
             state.abb %in% input$picker_state,
             `School Year` %in% input$picker_year) %>%
      
      #Data Manipulation
      filter(`Vaccine/Exemption` != "Exemption") %>%
      filter(!is.na(`Estimate (%)`)) %>%
      filter(`Estimate (%)` > 0) %>%
      group_by(state.abb) %>%
      summarise(avg_vaccination_rate = mean(`Estimate (%)`, na.rm = TRUE)) %>%
      ungroup()
  })
    
  #Map plot
  output$plotly_k_3 <- renderPlotly({
    rate_map() %>%                     
      plot_geo(locationmode = "USA-states") %>%
      add_trace(z = ~avg_vaccination_rate,
                locations = ~state.abb,
                color = ~avg_vaccination_rate,
                colors = "RdYlGn") %>%
      layout(
        geo = list(scope = "usa",
                   projection = list(type = "albers usa")),
        title = "Average Vaccination Rate by State") %>%
      colorbar(title = "Vaccination Rate (%)") %>%
      layout()
  })
  
  #Filters for Reported Cases
  reported <- reactive({
    cases %>%
      filter(disease %in% input$checkbox_diseases,
             year %in% input$picker_case_year) %>%
      filter(!is.na(year), !is.na(cases), !is.na(disease))
  })
  
  #Reported Cases Plot
  output$plotly_c_1 <- renderPlotly({
    ggplot(reported()) +
      geom_point(mapping = aes(x = year,
                               y = cases,
                               color = disease)) +
      geom_line(mapping = aes(x = year,
                              y = cases,
                              color = disease,
                              group = disease)) +
      labs(title = "Reported Cases of Vaccine Preventable Disease",
           x = "Year",
           y = "Reported Cases",
           color = "Disease") +
      scale_color_manual(values = c("Diphtheria" = "maroon4",
                                    "Measles" = "green",
                                    "Mumps" = "darkgreen",
                                    "Rubella" = "aquamarine4",
                                    "Pertussis" = "red",
                                    "Total tetanus" = "red4")) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly()
  })
  
  #Filters for Regression
  regress <- reactive({
    regression %>%
      filter(disease %in% input$checkbox_diseases)
  })
  
  #Regression Plot
  output$plotly_c_2 <- renderPlotly({
    regress() %>%
      ggplot(aes(x = Avg_Estimate, y = cases, color = disease)) +
      geom_point() + 
      geom_smooth(method = "lm", color = "blue") +
      labs(title = "Vaccine Preventable Disease Cases vs Vaccination Compliance",
           x = "Vaccine Compliance (%)",
           y = "Reported Cases",
           color = "Disease") +
      scale_color_manual(values = c("Diphtheria" = "maroon4",
                                    "Measles" = "darkgreen",
                                    "Rubella" = "green",
                                    "Pertussis" = "red",
                                    "Total tetanus" = "red4")) +
      theme_light()
  })
  
  #Diphtheria Model
  diphtheria_model <- lm(cases ~ year + coverage_1st_dose + coverage_3rd_dose, 
                         data = dip_data)
  
  output$dip_predict <- renderText({
    pred <- predict(diphtheria_model, newdata = data.frame(
      year = 2023,
      coverage_1st_dose = input$coverage_1st,
      coverage_3rd_dose = input$coverage_3rd
    ))
    paste("Predicted Diphtheria Cases:", round(pred, 2))
  })
  
  #Tetanus Model
  tetanus_model <- lm(cases ~ year + coverage_1st_dose + coverage_3rd_dose, 
                      data = tetanus_data)
  
  output$tetanus_predict <- renderText({
    pred <- predict(tetanus_model, newdata = data.frame(
      year = 2023,
      coverage_1st_dose = input$coverage_1st,
      coverage_3rd_dose = input$coverage_3rd
    ))
    paste("Predicted Total Tetanus Cases:", round(pred, 2))
  })
  
  #Pertussis Model
  pertussis_model <- lm(cases ~ year + coverage_3rd_dose + under_herd_3rd, 
                        data = pertussis_data)
  
  output$pertussis_predict <- renderText({
    pred <- predict(pertussis_model, newdata = data.frame(
      year = 2023,
      coverage_3rd_dose = input$coverage_3rd,
      under_herd_3rd = pmax(92 - input$coverage_3rd, 0)
    ))
    paste("Predicted Pertussis Cases:", round(pred, 2))
  })
  
  #DTP Forecasting
  dtp_forecast <- WHO_vax %>% 
    filter(vaccine == "DTP-containing vaccine") %>%
    group_by(year, dose) %>%
    summarize(Avg_Estimate = mean(coverage, na.rm = TRUE), 
              .groups = "drop") %>%
    as_tsibble(index = year, key = dose)

  dtp_forecast_ets <- dtp_forecast %>%
    model(ETS())

  fc_dtp_forecast_ets <- dtp_forecast_ets %>%
    forecast(h = 6)
  
  #DTP Forecast Plot
  output$plot_f_1 <- renderPlot({
    autoplot(fc_dtp_forecast_ets, dtp_forecast) +
      labs(title = "DTP Vaccine Compliance",
           subtitle = "6-year Forecasting using an ETS Model",
           y = "Compliance (%)",
           x = "Year") +
      theme_light()
  })

  #Measles Model
  measles_model <- lm(cases ~ coverage_1st_dose, 
                      data = measles_data)
  
  output$measles_predict <- renderText({
    pred <- predict(measles_model, newdata = data.frame(
      coverage_1st_dose = input$coverage_1st))
    paste("Predicted Measles Cases:", round(pred, 2))
  })
  
  #Measles forecast
  measles_forecast <- WHO_vax %>% 
    filter(vaccine == "Measles-containing vaccine") %>%
    group_by(year, dose) %>%
    summarize(Avg_Estimate = mean(coverage, na.rm = TRUE),
              .groups = "drop") %>%
    as_tsibble(index = year, key = dose)
  
  measles_forecast_ets <- measles_forecast %>%
    model(ETS())
  
  fc_measles_forecast_ets <- measles_forecast_ets %>%
    forecast(h = 6)
  
  #Measles Forecasting Plot
  output$plot_f_2 <- renderPlot({
  autoplot(fc_measles_forecast_ets, measles_forecast) +
    labs(title = "Measles-containg Vaccine Compliance",
         subtitle = "6-year Forecasting using an ETS Model",
         y = "Compliance (%)",
         x = "Year") +
    theme_light()
  })
  
  #Rubella Model
  rubella_model <- lm(cases ~ year + coverage_1st_dose,
                      data = rubella_data)
  
  output$rubella_predict <- renderText({
    pred <- predict(rubella_model, newdata = data.frame(
      year = 2023,
      coverage_1st_dose = input$coverage_1st))
    paste("Predicted Rubella Cases:", round(pred, 2))
  })
  
  #Rubella Forecasting
  rubella_forecast <- WHO_vax %>% 
    filter(vaccine == "Rubella-containing vaccine") %>%
    group_by(year, dose) %>%
    summarize(Avg_Estimate = mean(coverage, na.rm = TRUE),
              .groups = "drop") %>%
    as_tsibble(index = year, key = dose)
  
  rubella_forecast_ets <- rubella_forecast %>%
    model(ETS())
  
  fc_rubella_forecast_ets <- rubella_forecast_ets %>%
    forecast(h = 6)
  
  #Rubella Forecasting Plot
  output$plot_f_3 <- renderPlot({
    autoplot(fc_rubella_forecast_ets, rubella_forecast) +
      labs(title = "Rubella-containg Vaccine Compliance",
           subtitle = "6-year Forecasting using an ETS Model",
           y = "Compliance (%)",
           x = "Year") +
      theme_light()
  })
  
  #Hep B Forecasting
  hep_forecast <- WHO_vax %>% 
    filter(vaccine == "HepB") %>%
    group_by(year, dose) %>%
    summarize(Avg_Estimate = mean(coverage, na.rm = TRUE),
              .groups = "drop") %>%
    as_tsibble(index = year, key = dose)
  
  hep_forecast_ets <- hep_forecast %>%
    model(ETS())
  
  fc_hep_forecast_ets <- hep_forecast_ets %>%
    forecast(h = 6)
  
  #Hep B Forecasting Plot
  output$plot_f_7 <- renderPlot({
    autoplot(fc_hep_forecast_ets, hep_forecast) +
      labs(title = "Hepatitis B Vaccine Compliance",
           subtitle = "6-year Forecasting using an ETS Model",
           y = "Compliance (%)",
           x = "Year") +
      theme_light()
  })
  
  #Polio Forecasting
  pol_forecast <- WHO_vax %>% 
    filter(vaccine %in% c("Inactivated polio-containing vaccine", "Polio")) %>%
    filter(dose != "2nd dose") %>%
    group_by(year, dose) %>%
    summarize(Avg_Estimate = mean(coverage, na.rm = TRUE),
              .groups = "drop") %>%
    as_tsibble(index = year, key = dose)
  
  pol_forecast_ets <- pol_forecast %>%
    model(ETS())
  
  fc_pol_forecast_ets <- pol_forecast_ets %>%
    forecast(h = 6)
  
  #Polio Forecasting Plot
  output$plot_f_8 <- renderPlot({
    autoplot(fc_pol_forecast_ets, pol_forecast) +
      labs(title = "Polio Vaccine Compliance",
           subtitle = "6-year Forecasting using an ETS Model",
           y = "Compliance (%)",
           x = "Year") +
      facet_wrap(~ dose, scales = "free") +
      theme_light()
  })
  
  #Kindergarten Vaccination Forecasting
  kid_vax_forecast <- all %>%
    filter(`Vaccine/Exemption` != "Exemption") %>%
    filter(Geography != "U.S. Median") %>%
    group_by(`School Year`) %>% 
    summarize(Avg_Estimate = mean(`Estimate (%)`, na.rm = TRUE)) %>%
    filter(`School Year` != "2009-10") %>%
    mutate(start_year = as.integer(str_sub(`School Year`, 1, 4))) %>%
    as_tsibble(index = start_year)

  kid_vax_ets <- kid_vax_forecast %>%
    model(AAN = ETS(Avg_Estimate ~ error("A") + trend("A") + season("N")))
 
  fc_kid_vax_ets <- kid_vax_ets %>%
    forecast(h = 6)
  
  #Kindergarten Vaccination Forecasting Plot
  output$plot_f_4 <- renderPlot({
    autoplot(fc_kid_vax_ets, kid_vax_forecast) +
      labs(title = "Kindergarten Vaccination Compliance",
           subtitle = "6-year forecast using an ETS (A, A, N) Model",
           x = "Start of School Year",
           y = "Compliance (%)") +
      scale_x_continuous (breaks = seq(2011, 2029, by = 2)) +
      scale_y_continuous(limits = c(88, 95),
                         breaks = seq(88, 95, by = 2)) +
      theme_light()
  })
  
  #Kindergarten Vaccination Rates by Vaccine Forecasting
  type_comp_forecast <- all %>% 
    filter(`Vaccine/Exemption` != "Exemption") %>% 
    filter(`Geography Type` != "U.S. Median") %>% 
    group_by(`School Year`, `Vaccine/Exemption`) %>%  
    summarize(Avg_Estimate = mean(`Estimate (%)`, na.rm = TRUE), 
      .groups = "drop") %>%
    filter(`School Year` != "2009-10") %>%
    mutate(start_year = as.integer(str_sub(`School Year`, 1, 4))) %>%
    as_tsibble(index = start_year, 
               key = `Vaccine/Exemption`)
  
  kid_type_comp_ets <- type_comp_forecast %>%
    model(ETS(Avg_Estimate))
  
  fc_kid_type_comp_ets <- kid_type_comp_ets %>%
    forecast(h = 6)
  
  #Kindergarten Vaccination Rates by Vaccine Forecasting Plot
  output$plot_f_5 <-renderPlot ({
    autoplot(fc_kid_type_comp_ets, type_comp_forecast) +
      labs(title = "Kindergarten Vaccination Compliance by Vaccine",
           subtitle = "6-year Forecasting Using an ETS Model",
           y = "Compliance (%)",
           x = "Start of School Year") +
      facet_wrap(~`Vaccine/Exemption`, scales = "free_y") +
      theme_light()
  })
  
  #Kindergarten Exemption Rates Forecasting
  exemp_forecast <- all %>%
    filter(`Vaccine/Exemption` == "Exemption") %>%
    filter(Geography != "U.S. Median") %>%
    group_by(`School Year`) %>%
    summarise(total_exemptions = sum(`Number of Exemptions`, na.rm = TRUE),
              avg_exemption_rate = mean(`Estimate (%)`, na.rm = TRUE)) %>%
    filter(`School Year` != "2009-10") %>%
    mutate(start_year = as.integer(str_sub(`School Year`, 1, 4))) %>%
    as_tsibble(index = start_year)

  kid_exemp_ets <- exemp_forecast %>%
    model(AAN = ETS(avg_exemption_rate ~ error("A") + trend("A") + season("N")))
  
  fc_kid_exemp_ets <- kid_exemp_ets %>%
    forecast(h = 6) 
  
  #Kindergarten Exemption Rates Forecasting Plot
  output$plot_f_6 <- renderPlot ({
    autoplot(fc_kid_exemp_ets, exemp_forecast) +
      labs(title = "Kindergarten Exemptions Rates",
           subtitle = "6-year forecast using an ETS (A, A, N) Model",
           x = "Start of School Year",
           y = "Exemption Rate (%)") +
      theme_light()
  })
}

#Create Shiny app
shinyApp(ui, server)
