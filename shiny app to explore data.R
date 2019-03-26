library(tidyverse)
library(readxl)
library(broom)
library(nlme)
library(car)
library(shiny)


# Setup data and functions -----------------------------------------------------------------------------------


testAutocorr <- function(model, data=NULL, max.lag = 10, time.points = 25) {
  data <- eval(model$call$data)  # Only works if 'lm' call has dataframe named in bracket
  print(dwt(model, max.lag = max.lag, alternative = "two.sided"))
  par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
  par(fig = c(0.03, 1, 0.8, 1))
  plot(
    data$Time[1:time.points],
    residuals(model)[1:time.points],
    type = 'o',
    pch = 16,
    col = "red"
  )
  
  par(fig = c(0.03, 0.5, 0.05, 0.75), new = TRUE)
  acf(residuals(model))
  
  par(fig = c(0.55, 1, 0.05, 0.75), new = TRUE)
  acf(residuals(model), type = 'partial')
}

constructCIRibbon <- function(newdata, model) {
  newdata <- newdata %>%
    mutate(Predict = predict(model, newdata = newdata))
  mm <- model.matrix(as.formula(paste0("~ ", model$call$model[3])),
                     data = newdata)
  vars <- mm %*% vcov(model) %*% t(mm)
  sds <- sqrt(diag(vars))
  newdata <- newdata %>% mutate(lowCI = Predict - 1.96 * sds,
                                HiCI = Predict + 1.96 * sds)
}

printCoefficients <- function(model){
  as_tibble(trimws(format(round(summary(model)$tTable, 3), nsmall=3))) %>%
    mutate(Coefficient = rownames(summary(model)$tTable)) %>% 
    select(Coefficient, Value, Std.Error, 'p-value') %>% 
    print()
}

# Shiny app --------------------------------------------------------------------------------------------------

#** UI -------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "ages",
                  label = "Age group:",
                  choices = c("Under 16", "Under 18", "Under 20"),
                  selected = "Under 18"),
      selectInput(inputId = "main",
                  label = "Country to observe:",
                  choices = c("Scotland", "England", "Wales", "England and Wales"),
                  selected = "England"),
      selectInput(inputId = "control",
                  label = "Country to compare:",
                  choices = c("Scotland", "England", "Wales", "England and Wales"),
                  selected = "Scotland"),
      checkboxInput(inputId = "int1",
                    label = "Intervention 1",
                    value = TRUE),
      sliderInput(inputId = "int1yr",
                  label = "Beginning of intervention 1:",
                  min = 1995,
                  max = 2014,
                  step = 1,
                  value = 1999),
      checkboxInput(inputId = "int2",
                    label = "Intervention 2",
                    value = TRUE),
      sliderInput(inputId = "int2yr",
                  label = "Beginning of intervention 2:",
                  min = 1995,
                  max = 2014,
                  step = 1,
                  value = 2008),
      checkboxInput(inputId = "pi1",
                    label = "Phase-in",
                    value = FALSE),
      sliderInput(inputId = "pi1yr",
                  label = "Intervention 1 phase-in period:",
                  min = 1995,
                  max = 2014,
                  step = 1,
                  value = c(1999:2000))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Full Plot", plotOutput(outputId = "modelplot"),
                           dataTableOutput(outputId = "modelsummary")),
    #              tabPanel("Final model", dataTableOutput(outputId = "modelsummary")),
                  tabPanel("Dataframe for model", dataTableOutput(outputId = "dataframesumm"))
                  )
    )
  )
)

#** Server ----

server <- function(input, output) {
    
  
    all.UK.rates <-
      reactive({read_xlsx("Conception rates by age and country.xlsx", sheet = paste(input$ages))})
    
  dfa <- reactive({all.UK.rates() %>% filter(Country == input$main |
                            Country == input$control) %>%
    gather("Year", "Value",-1) %>%
    filter(!is.na(Value)) %>%  # removed filter for year>1991
    arrange(Country) %>%
    mutate(
      England = ifelse(Country == input$main, 1, 0),
      Year = as.numeric(Year),
      Time = Year - min(Year) + 1) %>% 
    mutate(Time_Eng = Time*England)
    })
  
    dfb <- reactive({
  if (input$int1) {
      dfa() %>% 
      mutate(Cat1 = ifelse(Year < input$int1yr, 0, 1),
      Trend1 = ifelse(Cat1 == 0, 0, Year - input$int1yr + 1)
    ) %>%
    mutate_at(., colnames(.)[7:8], list(Eng = ~ .*England)) 
  }
  })
    
    dfc <- reactive({
  if (input$int2) {
      dfb() %>% 
      mutate(Cat2 = ifelse(Year < input$int2yr, 0, 1),
      Trend2 = ifelse(Cat2 == 0, 0, Year - input$int2yr + 1)
    ) %>%
    mutate_at(., colnames(.)[11:12], list(Eng = ~ .*England)) 
  }
  })

output$modelplotsimple <- renderPlot({
  ggplot(data=dfc(), aes(
    Year,
    Value,
    group = interaction(Country, Cat1, Cat2),
    col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE)
})

output$dataframesumm <- renderDataTable({
  arrange(dfc(), by=Year)
})
  
  modelGls_null <- reactive({
if (input$int1 & input$int2) {
    gls(
    Value ~ Time +
      England +
      Time_Eng +
      Cat1 +
      Trend1 +
      Cat1_Eng +
      Trend1_Eng +
      Cat2 +
      Trend2 +
      Cat2_Eng +
      Trend2_Eng,
    data = dfc(),
    correlation = NULL,
    method = "ML"
  )}
  })
  
  output$modelsummary <- renderDataTable({
    printCoefficients(modelGls_null())
  })

  modcfac_base <- tibble(  # needs to be updated!!!!
    Time = c(8:25),
    England = 1,
    Time_Eng = c(8:25),
    Cat1 = 1,
    Trend1 = c(1:18),
    Cat2 = c(rep(0,9), rep(1,9)),
    Trend2 = c(rep(0,9), 1:9),
    Cat1_Eng = c(rep(0,9), rep(1,9)),
    Trend1_Eng = c(rep(0,9), 10:18),
    Cat2_Eng = 0,
    Trend2_Eng = 0
  )  # Remove _Eng interactions (retaining 1st intervention interactions for 2nd int)


  ###Tricky bits here
  modcfac <- reactive({
  left_join(modcfac_base, constructCIRibbon(modcfac_base, modelGls_null()))
    })
  ###

dfd <- reactive({
   left_join(dfc(), constructCIRibbon((dfc() %>% filter(England==1, Year >= input$int1yr)), modelGls_null())) 
})

output$dfd <- renderDataTable(dfd())

  output$modelplot <- renderPlot({
    dfd() %>% 
    mutate(Predict = predict(modelGls_null())) %>%  # Add Predicts for non-England
    ggplot(aes(
      Time,
      Value,
      col = Country,
      fill = Country,
      group = interaction(Country, Cat1, Cat2)
    )) +
    # Show all data points
    geom_point(data=dfc(), show.legend = FALSE) +
    # Counterfactual trend lines
    geom_line(
      data = modcfac(),
      aes(
        x = Time,
        y = Predict,
        group = Cat2,
        col = "Control",
        fill = NULL
      ),
      linetype = "longdash",
      size = 1,
      inherit.aes = FALSE
    ) +
    # Counterfactual confidence intervals (not shown in legend)
    geom_ribbon(
      data=modcfac(),
      aes(
        x=Time,
        ymin = lowCI,
        ymax=HiCI,
        group = Cat2,
        col=NULL,
        fill="Control"
      ),
      alpha=0.5,
      size = 1,
      show.legend = FALSE,
      inherit.aes = FALSE) +
    # Model trend lines
    geom_line(aes(y=Predict), size = 1) +
    # Confidence intervals (not shown in legend)
    geom_ribbon(
      aes(
        x=Time,
        ymin = lowCI,
        ymax=HiCI,
        col=NULL,
        fill=Country
      ),
      alpha=0.5,
      size = 1,
      show.legend = FALSE) +
    # Intervention time points
    geom_vline(xintercept = 7.5,
               linetype = "dotted",
               col = "#000000CC") +
    geom_vline(xintercept = 16.5,
               linetype = "dotted",
               col = "#000000CC") +
    # Display parameters
    scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                       labels = seq(1995, 2015, by = 5)) +
    theme(panel.background = element_blank(),
          legend.key  = element_blank()) +
    ylab("Rate of pregnancies to under-18s, per 1,000") +
    xlab("Year") +
    coord_cartesian(ylim = c(0, 60)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_colour_manual(
      breaks = c("England", "Wales", "Scotland", "England and Wales", "Control"),
      values = c("Wales" = "#00AB39",
                 "Scotland" = "#0072C6",
                 "England" = "#CF142B",
                 "England and Wales" = "#CF142B",
                 "Control" = "#F7D917"),
      aesthetics = c("colour", "fill"))
  })
}


shinyApp(ui, server)