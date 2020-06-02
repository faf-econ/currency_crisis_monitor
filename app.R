# 1 Run Helper -------------------------------------------------------------------------

source("./helper.R")

# 2 User Interface ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Currency Crisis Monitor"),
  withMathJax(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create currency crisis charts with 
               updated information from the IMF."),
      
      selectInput("country", 
                  label = "Choose a Country",
                  choices = c("World Map", country_list),
                  selected = "World Map"),
      
      sliderInput("sd_w", 
                  label = "Backward Window (# months) for \\(\\sigma_{e_{i}}\\) and \\(\\sigma_{fx_{i}}\\)",
                  min = 2, max = 120,
                  value = 12),
      
      sliderInput("erpi_w", 
                  label = "Backward Window (# months) for Trigger",
                  min = 2, max = 120,
                  value = 60),
      
      sliderInput("eta", 
                  label = "\\(\\eta\\): Number of sd above mean to consider Currency Crisis",
                  min = 1, max = 4,
                  value = 3, step = .5),
      
      sliderInput("acc_stress", 
                  label = "Backward Window (# months) with no crisis",
                  min = 1, max = 24,
                  value = 6),
      
      sliderInput("delta", 
                  label = "\\(\\delta\\): Pressure Tolerance",
                  min = 0, max = 2,
                  value = .1, step = .25),
      
      sliderInput("start",
                  label = "Year to start computation:",
                  min = 1970, max = 2018, 
                  value = 1970, sep = "")
      
    ),
    
    mainPanel(
      
#* 2.1 Output: Tabset w/ plot, summary, and table ----

      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotlyOutput("map")),
                  tabPanel("Plot",
                           conditionalPanel(
                             condition = "input.country != 'World Map'",
                             radioButtons("add_line", 
                                          label = "Select:", 
                                          choices = list("ERPI and Currency Crisis" = 1,
                                                         "Exchange Rate" = 2,
                                                         "International Reserves" = 3),
                                          selected = 1)
                           ),
                           plotlyOutput("plot"),
                           sliderInput("range",
                                       label = "Period of time to show:",
                                       min = 1970, max = 2018, 
                                       value = c(1970, 2018), sep = "")
                           ),
                  tabPanel("Summary", tableOutput("summary")),
                  tabPanel("Table", DT::dataTableOutput("table"),
                           downloadButton("downloadData", "Download"))
      )
      
    )
  )
)


# 3 Server ---------------------------------------------------------------------

server <- function(input, output) {
  
  dataInput <- reactive({
    
    ierp %>%
      filter(year > input$start) %>%
      group_by(code) %>%
      mutate(e_mean = rollapply(data = e, width = input$sd_w, 
                              FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "right"),
             fx_mean = rollapply(data = fx, width = input$sd_w, 
                               FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "right"),
             e_sd = rollapply(data = e, width = input$sd_w, 
                              FUN = function(x) sd(x, na.rm = TRUE), partial = TRUE, align = "right"),
             fx_sd = rollapply(data = fx, width = input$sd_w, 
                               FUN = function(x) sd(x, na.rm = TRUE), partial = TRUE, align = "right"),
             e_chg = e/lag(e)-1,
             fx_chg = fx/lag(fx)-1) %>%
      drop_na(e_chg, fx_chg) %>%
      mutate(erpi = e_chg - e_sd/fx_sd * fx_chg,
             # ERPI and triggers
             erpi_mean = rollapply(data = erpi, width = input$erpi_w, 
                                   FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "right"),
             erpi_sd = rollapply(data = erpi, width = input$erpi_w, 
                                 FUN = function(x) sd(x, na.rm = TRUE), partial = TRUE, align = "right"),
             trigger = erpi_mean + input$eta*erpi_sd,
             # Currency Crisis
             above_trigger = ifelse(erpi > trigger, 1, 0),
             cc = ifelse(erpi > input$delta & above_trigger == 1, 1, 0),
             cc = ifelse(is.na(cc), 0, cc),
             acc_above_cc = rollapply(data = cc, width = input$acc_stress,
                                           FUN = function(x) sum(x, na.rm = TRUE), partial = TRUE, align = "right"),
             cc = ifelse(erpi > input$delta & above_trigger == 1 & lag(acc_above_cc == 0), 
                         1, 0),
             cc = ifelse(is.na(cc), 0, cc),
             acc_cc = cumsum(cc),
             # For plots
             cc_plot = ifelse(erpi > input$delta & above_trigger == 1 & lag(acc_above_cc == 0),
                              (erpi + .01), NA),
             acc_cc_plot = ifelse(cc == 1, acc_cc, NA),
             
             # Standarized
             std_e = (e - e_mean) / e_sd,
             std_fx = (fx - fx_mean) / fx_sd
      ) %>%
      ungroup() %>%
      filter(code != "AN") %>%
      mutate(date = as.Date(as.yearmon(year) + (month-1)/12, frac = 0),
             code = countrycode(code, origin = "iso2c", destination = "iso3c"),
             country = countrycode(code, origin = "iso3c", destination = "country.name")) %>%
      inner_join(select(countrycode::codelist, iso3c, continent, region), 
                 by = c("code" = "iso3c")) %>%
      select(code, country, region, continent, date, everything()) %>%
      drop_na(code) %>%
      filter(year %in% input$range[1]:input$range[2])
    
  })
  
  #* 3.1 Generate plot of the data ----
  output$plot <- renderPlotly({
    
    req(input$country)
    req(input$add_line)
  
    if(input$country == "World Map"){
      
      ggplot(as.data.frame(1:2), aes(1, 2))+
        theme_void()+
        geom_text(aes(label = "Only available selecting World Map\nChange to Map Tab"), size = 10)
      
      
    }else{
      
      if(input$add_line == 1){
        
        (dataInput() %>%
           filter(country == input$country) %>%
           # ggPlot
           ggplot(aes(x = date, y = erpi)) +
           # Geoms:
           geom_hline(yintercept = 0, color = "gray") +
           geom_line(aes(group = code), color = "#014d64", size = .3) +
           geom_line(aes(y = trigger, group = code), linetype = "dashed", color = "#01a2d9", size = .2) +
           
           geom_point(aes(y = cc_plot), shape = 4, size = 2, color = "red") +
           geom_text(aes(y = 1.1*cc_plot, label = acc_cc_plot), size = 2.5, color = "red") +
           # Axis
           xlab("Time") +
           scale_y_continuous(name = "ERPI and Trigger") +
           # Theme:
           theme_minimal() +
           scale_color_economist() +
           theme(legend.position = "none") +
           # Labs
           labs(title = paste0("Currency Crisis in ", input$country))) %>%
          ggplotly(tooltip = c("date", "erpi", "trigger")) %>%
          print()
        
      }else if(input$add_line == 2){
        
        (dataInput() %>%
           filter(country == input$country) %>%
           # ggPlot
           ggplot(aes(x = date, y = e)) +
           # Geoms:
           geom_hline(yintercept = 0, color = "gray") +
           geom_line(aes(group = code), color = "#014d64", size = .2) +
           # Axis
           xlab("Time") +
           scale_y_continuous(name = "Exchange Rate (LCU/USD)") +
           # Theme:
           theme_minimal() +
           scale_color_economist() +
           theme(legend.position = "none") %>%
           # Labs
           labs(title = paste0("Exchange Rate ", input$country))) %>%
          ggplotly(tooltip = c("date", "e")) %>%
          print()
        
      }else {
        
        (dataInput() %>%
           filter(country == input$country) %>%
           # ggPlot
           ggplot(aes(x = date, y = fx)) +
           # Geoms:
           geom_hline(yintercept = 0, color = "gray") +
           geom_line(aes(group = code), color = "#014d64", size = .2) +
           # Axis
           xlab("Time") +
           scale_y_continuous(name = "International Reserves ($ M USD)", labels = number_format(big.mark = ",")) +
           # Theme:
           theme_minimal() +
           scale_color_economist() +
           theme(legend.position = "none") %>%
           # Labs
           labs(title = paste0("International Reserves ", input$country))) %>%
          ggplotly(tooltip = c("date", "fx")) %>%
          print()
        
      }
      
    }
    
  })
  
  #* 3.2 Generate map of the data ----
  output$map <- renderPlotly({
    
    req(input$country)
    
    if(input$country == "World Map"){
      
      (dataInput() %>%
         filter(month == 12) %>%
         rename(Currency_Crisis = acc_cc)) %>%
        
        # Plotly
        plot_ly(type = 'choropleth', locations = ~code, 
                z = ~Currency_Crisis, 
                text = ~country, 
                frame = ~year,
                colorscale = "Plasma") %>%
        print()
      
    }else{
      
      ggplot(as.data.frame(1:2), aes(1, 2))+
        theme_void()+
        geom_text(aes(label = "Only available selecting World Map\nChange to Plot Tab"), size = 10)      
    }
    
  })
  
  #* 3.3 Generate a summary of the data ----
  
  output$summary <- function()  {
    
    req(input$country)
    
    if(input$country == "World Map"){
      
      as.data.frame("Not available for World Map") %>%
        
        kable() 
      
    }else{
      
      dataInput() %>%
        filter(country == input$country) %>%
        select(date, e, fx, erpi, acc_cc) %>%
        rename(Date = date,
               `Exchange Rate (LCU/USD)` = e,
               `International Reserves` = fx,
               `Exchange Rate Pressure Index` = erpi,
               `Accumulated Crisis` = acc_cc) %>%
        summary() %>%
        
        kable(caption = paste0("Currency Crisis in ", input$country), align = c("l", rep("c", 4))) %>%
        kable_styling(bootstrap_options = c("striped", "hover"))
      
    }
     
  }
  
  #* 3.4 Generate an HTML table view of the data ----
  
  output$table <- DT::renderDataTable({
    
    if(input$country == "World Map"){
      
      dw_table <- dataInput() %>%
        select(country, year, month, e, fx, erpi, trigger, cc) %>%
        mutate(year = number(year, 1, big.mark = ""),
               month = number(month, 1),
               e = number(e, .01),
               fx = number(fx, .01, prefix = "$", big.mark = ","),
               erpi = number(erpi, .001),
               trigger = number(trigger, .001),
               cc = number(cc, 1)) %>%
        rename(Country = country,
               Year = year,
               Month = month,
               `Exchange Rate (LCU/USD)` = e,
               `International Reserves` = fx,
               `Exchange Rate Pressure Index` = erpi,
               `Crisis Trigger` = trigger, 
               `Currency Crisis` = cc
        )
      
    }else{
      
      dw_table <- dataInput() %>%
        filter(country == input$country) %>%
        select(year, month, e, fx, erpi, trigger, cc) %>%
        mutate(year = number(year, 1, big.mark = ""),
               month = number(month, 1),
               e = number(e, .01),
               fx = number(fx, .01, prefix = "$", big.mark = ","),
               erpi = number(erpi, .001),
               trigger = number(trigger, .001),
               cc = number(cc, 1)) %>%
        rename(Year = year,
               Month = month,
               `Exchange Rate (LCU/USD)` = e,
               `International Reserves` = fx,
               `Exchange Rate Pressure Index` = erpi,
               `Crisis Trigger` = trigger, 
               `Currency Crisis` = cc
        )
      
    }
    
    dw_table
    
    })
  
  output$downloadData <- downloadHandler(
    
    filename <- function() {
      paste(dw_table, ".csv", sep = "")
    },
    
    content <- function(file) {
      write.csv(dw_table, file, row.names = FALSE)
    
    }
  )
  
}

# 4 Launch App -------------------------------------------------------------------------

shinyApp(ui, server)
#rsconnect::deployApp('/home/faf/Dropbox/Git_Projects/currency_crisis_monitor')
