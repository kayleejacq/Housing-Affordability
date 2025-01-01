#import libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(plotly)
library(sf)

#load the housing data
house_data <- read.csv("valid_geocodes.csv")
unique_county = sort(unique(house_data$County))
unique_desc = c("New Dwelling house /Apartment","Second-Hand Dwelling house /Apartment")
option = c('Yes', 'No')

#load the earnings data
earnings_data <- read.csv("earnings_dist.csv")
unique_sex = sort(unique(earnings_data$Sex))
unique_county = sort(unique(earnings_data$County))
unique_age = sort(unique(earnings_data$Age.Group))


ui <- fluidPage(
  tabsetPanel(
    #create tab for Landing Page
    tabPanel("Landing Page", icon = icon("face-smile"),
             fluidPage(
               titlePanel("Ireland Housing Affordability in 2023 (Kaylee - D00262128)"),
               fluidRow(
                 column(12,
                        h3("Welcome to my Dashboard!"),
                        p("This interactive web application provides insights into housing prices, 
                          earnings distribution, and their relationship across Ireland in 2023.
                          This dashboard contains 3 pages and each page has different features to filter
                          and tools for data analysis.
                          The goal of this project is to offer an understanding for regional housing 
                          affordability."),
                        
                        h4("Features:"),
                        tags$ul(
                          tags$li(
                            strong("Housing Price Distribution"),
                            tags$ul(
                              tags$li("Filters: Customize the dataset based on County, Price Range, VAT Exclusive, Full Market Price, and House Description."),
                              tags$li("Visualization: Includes an interactive map showing filtered housing data. 
                                      Each marker represents a house, it will show the description for each houses when clicked."),
                              tags$ul(
                                tags$li("The markers are sized based on the Price Ratio and colored according to the option selected.")
                              ),
                              tags$li("Summary:"),
                              tags$ul(
                                tags$li("Total properties shown."),
                                tags$li("Average housing price of the filtered dataset.")
                              ),
                              tags$li("Data Table: A downloadable table of the dataset.")
                            )
                          ),
                          tags$li(
                            strong("Earnings Distribution"),
                            tags$ul(
                              tags$li("Filters: Filter the dataset by County, Sex, and Age Group."),
                              tags$li("Visualization:"),
                              tags$ul(
                                tags$li("An interactive map show counties and the average earnings when clicked."),
                                tags$li("An interactive Plotly boxplot to show the earnings distribution based on Sex and Age Group.")
                              ),
                              tags$li("Data Table: A downloadable table of the dataset.")
                            )
                          ),
                          tags$li(
                            strong("Housing Affordability Analysis"),
                            tags$ul(
                              tags$li("Goal:"),
                              tags$ul(
                                tags$li("Combine the Housing and Earnings datasets."),
                                tags$li("Calculate the Mortgage Affordability Index, assuming:"),
                                tags$ul(
                                  tags$li("Loan to Value (LTV): 90%."),
                                  tags$li("Mortgage Term: 25 years."),
                                  tags$li("Interest Rate: 4.1%.")
                                ),
                                tags$li("Categorize affordability using the Cost Burden metric:"),
                                tags$ul(
                                  tags$li("If Cost Burden ≤ 30%, the property is categorized as affordable.")
                                )
                              ),
                              tags$li("Filters: Apply filters for County, VAT Exclusive, Full Market Price, House Description, Sex, and Age Group."),
                              tags$li("Visualization:"),
                              tags$ul(
                                tags$li("An interactive map displaying:"),
                                tags$ul(
                                  tags$li("County name."),
                                  tags$li("Average Housing Price."),
                                  tags$li("Average Earnings."),
                                  tags$li("Income Affordability Index.")
                                )
                              ),
                              tags$li("Data Table: A downloadable table of the dataset.")
                            )
                          )
                        ),
                        
                        h4("Data Sources & References:"),
                        tags$ul(
                          tags$li(
                            "Housing Prices Dataset: ",
                            tags$a(href = "https://www.propertypriceregister.ie/website/npsra/pprweb.nsf/PPRDownloads?OpenForm&File=PPR-2023.csv&County=ALL&Year=2023&Month=ALL",
                                   'PPR Housing Prices Dataset',
                                   target = "_blank")
                          ),
                          tags$li(
                            "Earnings Distribution Dataset: ",
                            tags$a(href = "https://data.cso.ie/", 
                                   "DEA06 - Earnings Distribution", target = "_blank")
                          )
                        ),
                        tags$div(
                          style = "color: red; font-style: italic; margin-top: 10px;",
                          "Disclaimer: The geolocation data displayed on the map is sourced using Mapbox geocoding API. 
                          Some geolocations may be inaccurate or approximate due to limitations in the geocoding process."
                        )
                        
                 )
               )
             )
    ),
    #create tab for Housing Prices
    tabPanel("Housing Prices", icon = icon("home"),
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Ireland Housing Prices in 2023"),
                 #create filters for the dataset
                 selectizeInput("county", 
                                "County", 
                                unique_county,
                                multiple = TRUE,
                                selected = "Dublin"), #select dublin by default
                 sliderInput("price", "Price Range (€)", value = c(100000, 30000000), min = 5000, max = 100000000),
                 selectInput("color", "Color by:",
                             choices = c("VAT Exclusive" = "VAT.Exclusive",
                                         "Not Full Market Price" = "Not.Full.Market.Price",
                                         "Description of Property" = "Description.of.Property"),
                             selected = "VAT.Exclusive"),
                 checkboxGroupInput("VAT", "VAT Exclusive", option, selected = option),
                 checkboxGroupInput("full_price", "Not Full Market Price", option, selected = option),
                 checkboxGroupInput("desc", "House Description", unique_desc, selected = unique_desc),
                 #add download button
                 downloadButton("download_filtered", "Download Filtered Data",icon = icon("download"))
               ),
               mainPanel(
                 fluidPage(
                   #output the map for housing distribution
                   fluidRow(
                     leafletOutput("map"),
                     style = "margin-bottom: 20px;"
                   ),
                   #output the summary
                   fluidRow(
                     column(12, 
                            h4("Summary"),
                            textOutput("summary"),
                            style = "margin-bottom: 20px;"
                     )
                   ),
                   #output the housing data table
                   fluidRow(
                            DTOutput("House_Table")
                 )
                 
               )
             )
    )),
    #create tab for Earnings Distribution
    tabPanel("Earnings Distribution", icon = icon("sack-dollar"),
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Ireland Earnings Distribution in 2023"),
                 #create filters
                 selectizeInput("county2", 
                                "County", 
                                unique_county,
                                multiple = TRUE,
                                selected = unique_county),
                 checkboxGroupInput("sex2", "Sex", unique_sex, selected = unique_sex),
                 checkboxGroupInput("age2", "Age Group", unique_age, selected = unique_age),
                 #add download button
                 downloadButton("download_filtered2", "Download Filtered Data",icon = icon("download"))
               ),
               mainPanel(
                 fluidPage(
                   #output the earnings distribution map
                   fluidRow(
                     leafletOutput("map2"),
                     style = "margin-bottom: 20px;"
                   ),
                   #output the plotly boxplot
                   fluidRow(
                     plotlyOutput("plotlyPlot"),
                     style = "margin-bottom: 20px;"
                   ),
                   #output the earnings data table
                   fluidRow(
                     DTOutput("Earnings_Table")
                   )
                 )
               )
             )
    ),
    #create Housing Affordability Analysis tab
    tabPanel("Housing Affordability Analysis", icon = icon("money-bill-trend-up"),
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Ireland Housing Affordability in 2023"),
                 #create filters
                 selectizeInput("county3", 
                                "County", 
                                unique_county,
                                multiple = TRUE,
                                selected = unique_county),
                 sliderInput("price3", "Price Range (€)", value = c(100000, 30000000), min = 5000, max = 100000000),
                 checkboxGroupInput("VAT3", "VAT Exclusive", option, selected = option),
                 checkboxGroupInput("full_price3", "Not Full Market Price", option, selected = option),
                 checkboxGroupInput("desc3", "House Description", unique_desc, selected = unique_desc),
                 checkboxGroupInput("sex3", "Sex", unique_sex, selected = unique_sex),
                 checkboxGroupInput("age3", "Age Group", unique_age, selected = unique_age),
                 #add download button
                 downloadButton("download_filtered3", "Download Filtered Data",icon = icon("download"))
               ),
               mainPanel(
                 fluidPage(
                   #output the map
                   fluidRow(
                     leafletOutput("map3"),
                     style = "margin-bottom: 20px;"
                   ),
                   #output table
                   fluidRow(
                     DTOutput("Affordability_Table")
                   )
                 )
               )
             )
    ))
)

server <- function(input, output, session) {

  
#HOUSING PRICES
  #create function for the housing dist. map using leaflet
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles()%>% 
    setView(lng = -8.2439, lat = 53.4129, zoom = 6) 
  })
  
  #reference: SuperZip
  #create a function to color the markers by the input from the user
  observe({
    custom_palette <- c("blue", "orange")
    #color by input
    colorBy <- input$color

    df_filtered <- filter(house_data,
                          County %in% input$county,
                          Price >= input$price[1] & Price <= input$price[2],
                          Not.Full.Market.Price %in% input$full_price,
                          VAT.Exclusive %in% input$VAT,
                          Description.of.Property %in% input$desc)
    
    colorData <- df_filtered[[colorBy]]
    #create the color palette func
    pal <- colorFactor(custom_palette, domain = unique(colorData))
    
    radius <- df_filtered$Price / max(df_filtered$Price) * 1000
    
    #create the leaflet map
    leafletProxy("map", data = df_filtered) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(
        ~Longitude, ~Latitude,
        radius = radius,
        fillColor = pal(colorData),
        stroke = FALSE,
        fillOpacity = 0.6,
        #when clicked
        popup = ~paste0(
          "<b>Address:</b> ", Address, "<br>",
          "<b>VAT Exclusive:</b> ", VAT.Exclusive, "<br>",
          "<b>Not Full Market Price:</b> ", Not.Full.Market.Price, "<br>",
          "<b>Description:</b> ", Description.of.Property, "<br>",
          "<b>Price:</b> €", format(Price, big.mark = ",")
        )
      ) %>%
      #add legend for the color
      addLegend("bottomleft", pal = pal, values = colorData, title = colorBy)
  })
  
  #create a summary
  output$summary <- renderText({
    df_filtered <- filter(house_data,
                          County %in% input$county,
                          Price >= input$price[1] & Price <= input$price[2],
                          Not.Full.Market.Price %in% input$full_price,
                          VAT.Exclusive %in% input$VAT,
                          Description.of.Property %in% input$desc)
    
    total_properties <- nrow(df_filtered)
    avg_price <- mean(df_filtered$Price, na.rm = TRUE)
    
    paste0(
      "Total Properties:", total_properties, "\n",
      "Average Price: €", format(avg_price, big.mark = ",")
    )
  })
  
  #create the housing price table
  output$House_Table <- renderDT({
    #only show these columns
    selected_columns <- c("Address", "County", "Price", "Not.Full.Market.Price", 
                          "VAT.Exclusive", "Description.of.Property")
    df_filtered <- filter(house_data,
                          County %in% input$county,
                          Price>=input$price[1] & Price<=input$price[2],
                          Not.Full.Market.Price %in% input$full_price,
                          VAT.Exclusive %in% input$VAT,
                          Description.of.Property %in% input$desc
                          )
    
  datatable(df_filtered[,selected_columns],
            options = list(pageLength = 10,
            scrollX = TRUE,
            autoWidth = TRUE)
            )
                           
  })
  
  #function to download the table
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df_filtered <- filter(house_data,
                            County %in% input$county,
                            Price >= input$price[1] & Price <= input$price[2],
                            Not.Full.Market.Price %in% input$full_price,
                            VAT.Exclusive %in% input$VAT,
                            Description.of.Property %in% input$desc)
      write.csv(df_filtered, file, row.names = FALSE)
    }
  )
  
  
  
  #EARNINGS 
  #create a function to map the earnings distribution
  output$map2 <- renderLeaflet({
    earnings_df_filtered <- filter(earnings_data, 
                                   County %in% input$county2,
                                   Sex %in% input$sex2,
                                   Age.Group %in% input$age2) %>%
      group_by(County) %>%
      summarise(avg_earnings = mean(VALUE, na.rm = TRUE), #get the average earnings per county
                Latitude = first(Latitude), 
                Longitude = first(Longitude))

    leaflet(earnings_df_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, 
        ~Latitude,
        radius = 5,  
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.6,
        popup = ~paste0(
          "<b>County:</b> ", County, "<br>",
          "<b>Average Earnings:</b> ", avg_earnings)
      )
  })
  
  #create table
  output$Earnings_Table <- renderDT({
    earnings_df_filtered <- filter(earnings_data, 
                                   County %in% input$county2,
                                   Sex %in% input$sex2,
                                   Age.Group %in% input$age2)

    datatable(earnings_df_filtered,
              options = list(pageLength = 10)
    )
    
  })
  
  #function to download the table
  output$download_filtered2 <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      earnings_df_filtered <- filter(earnings_data, 
                                     County %in% input$county2,
                                     Sex %in% input$sex2,
                                     Age.Group %in% input$age2)
      write.csv(earnings_df_filtered, file, row.names = FALSE)
    }
  )
  
  #create plotly boxplot
  output$plotlyPlot <- renderPlotly({
    earnings_df_filtered <- filter(earnings_data, 
                                   County %in% input$county2,
                                   Sex %in% input$sex2,
                                   Age.Group %in% input$age2)
    
    #create boxplot of earnings by sex and age group
    gg <- ggplot(earnings_df_filtered, aes(x = factor(Age.Group), y = VALUE)) +
      geom_boxplot(aes(fill = Sex)) +
      labs(x = "Age Group", y = "Earnings", fill = "Sex",
           title = "Earnings by Sex and Age Group") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 14, face = "bold"))
    
    ggplotly(gg) %>%
      layout(boxmode = "group")
  })
    
  #HOUSING VS EARNINGS
  #create map
  output$map3 <- renderLeaflet({
    #values for calculating the mortgage/income affordability index
    mortgage_rate <- 0.041
    loan_term <- 25 
    LTV_ratio <- 0.9
    
    #filter the housing data
    house_df_filtered <- filter(house_data,
                                County %in% input$county3,
                                Price>=input$price3[1] & Price<=input$price3[2],
                                Not.Full.Market.Price %in% input$full_price3,
                                VAT.Exclusive %in% input$VAT3,
                                Description.of.Property %in% input$desc3) %>%
      group_by(County)%>%
      summarise(avg_housing_price =  round(mean(Price, na.rm = TRUE), 2))
    
    #filter the earnings data
    earnings_df_filtered <- filter(earnings_data, 
                                   County %in% input$county3,
                                   Sex %in% input$sex3,
                                   Age.Group %in% input$age3)%>%
      group_by(County)%>%
      summarise(avg_earnings = round(mean(VALUE, na.rm = TRUE),2),
                Latitude = first(Latitude), 
                Longitude = first(Longitude))
    
    #merge the housing and earnings data based on county
    affordability_data <- merge(house_df_filtered, earnings_df_filtered, by = "County") %>%
      mutate(
        #calculate the income affordability index
        loan_amount = avg_housing_price * LTV_ratio,
        monthly_interest_rate = mortgage_rate / 12,
        total_payments = loan_term * 12,
        monthly_repayment = round(loan_amount * (monthly_interest_rate * (1 + monthly_interest_rate)^total_payments) / 
                                    ((1 + monthly_interest_rate)^total_payments - 1), 2),
        monthly_income = round(avg_earnings / 12, 2),
        income_affordability_index = round(monthly_repayment / monthly_income * 100, 2),
        cost_burden = ifelse(income_affordability_index <= 30, "Affordable", "Unaffordable")
      )
    #create map
    leaflet(affordability_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, 
        ~Latitude,
        radius = 5,  
        color =  ~ifelse(cost_burden == "Affordable", "blue", "red"),
        stroke = FALSE,
        fillOpacity = 0.6,
        #when clicked
        popup = ~paste0(
          "<b>County:</b> ", County, "<br>",
          "<b>Average Housing Price:</b> ", avg_housing_price, "<br>",
          "<b>Average Earnings:</b> ", avg_earnings, "<br>",
          "<b>Income Affordability Index:</b> ", income_affordability_index
        )
      )%>%
      #add legend based on affordability
      addLegend(
        position = "bottomleft",   
        colors = c("blue", "red"),  
        labels = c("Affordable", "Unaffordable"),  
        title = "Affordability"  
      )
  })
  
  #create  table
  output$Affordability_Table <- renderDT({
    mortgage_rate <- 0.041
    loan_term <- 25
    LTV_ratio <- 0.9
    
    house_df_filtered <- filter(house_data,
                                County %in% input$county3,
                                Price>=input$price3[1] & Price<=input$price3[2],
                                Not.Full.Market.Price %in% input$full_price3,
                                VAT.Exclusive %in% input$VAT3,
                                Description.of.Property %in% input$desc3) %>%
      group_by(County)%>%
      summarise(avg_housing_price = round(mean(Price, na.rm = TRUE),2))
    
    
    earnings_df_filtered <- filter(earnings_data, 
                                   County %in% input$county3,
                                   Sex %in% input$sex3,
                                   Age.Group %in% input$age3)%>%
      group_by(County)%>%
      summarise(avg_earnings = round(mean(VALUE, na.rm = TRUE),2))
    
    affordability_data <- merge(house_df_filtered, earnings_df_filtered, by = "County") %>%
      mutate(
        loan_amount = avg_housing_price * LTV_ratio,
        monthly_interest_rate = mortgage_rate / 12,
        total_payments = loan_term * 12,
        monthly_repayment = round(loan_amount * (monthly_interest_rate * (1 + monthly_interest_rate)^total_payments) / 
                                    ((1 + monthly_interest_rate)^total_payments - 1), 2),
        monthly_income = round(avg_earnings / 12, 2),
        income_affordability_index = round(monthly_repayment / monthly_income * 100, 2),
        cost_burden = ifelse(income_affordability_index <= 30, "Affordable", "Unaffordable")
      )
    #only show this columns
    selected_columns <- c("County","avg_housing_price","avg_earnings", "monthly_repayment",
                            "monthly_income","income_affordability_index", "cost_burden")
    
    datatable(affordability_data[,selected_columns],
              options = list(pageLength = 10,
                             scrollX = TRUE,
                             autoWidth = TRUE)
    )
    
  })
  
  #func to download the table
  output$download_filtered3 <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      mortgage_rate <- 0.041
      loan_term <- 25 
      LTV_ratio <- 0.9
      
      house_df_filtered <- filter(house_data,
                                  County %in% input$county3,
                                  Price>=input$price3[1] & Price<=input$price3[2],
                                  Not.Full.Market.Price %in% input$full_price3,
                                  VAT.Exclusive %in% input$VAT3,
                                  Description.of.Property %in% input$desc3) %>%
        group_by(County)%>%
        summarise(avg_housing_price = round(mean(Price, na.rm = TRUE),2))
      
      
      earnings_df_filtered <- filter(earnings_data, 
                                     County %in% input$county3,
                                     Sex %in% input$sex3,
                                     Age.Group %in% input$age3)%>%
        group_by(County)%>%
        summarise(avg_earnings = round(mean(VALUE, na.rm = TRUE),2))
      
      affordability_data <- merge(house_df_filtered, earnings_df_filtered, by = "County") %>%
        mutate(
          loan_amount = avg_housing_price * LTV_ratio,
          monthly_interest_rate = mortgage_rate / 12,
          total_payments = loan_term * 12,
          monthly_repayment = round(loan_amount * (monthly_interest_rate * (1 + monthly_interest_rate)^total_payments) / 
                                      ((1 + monthly_interest_rate)^total_payments - 1), 2),
          monthly_income = round(avg_earnings / 12, 2),
          income_affordability_index = round(monthly_repayment / monthly_income * 100, 2),
          cost_burden = ifelse(income_affordability_index <= 30, "Affordable", "Unaffordable")
        )
      #only show these columns
      selected_columns <- c("County","avg_housing_price","avg_earnings", "monthly_repayment",
                            "monthly_income","income_affordability_index", "cost_burden")
      write.csv(affordability_data[,selected_columns], file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)