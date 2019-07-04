
# Load Packages -----------------------------------------------------------

library(forecast)
library(leaflet)
library(plotly)
library(quantmod)
library(Quandl)
library(scales)
library(tidyverse)
library(shiny)
library(shinydashboard)

# Import Data ------------------------------------------------------------

# Load the Quandl API Key
Quandl.api_key("rn2xyN_hG9XfxN_9ibFJ")

# Define a tibble containing key information for selected commodities
commods <- tibble(
    name = c("brt", "wti", "dub", "cad", "hhb", "nbp", "sfe", "tsi", "nwc"),
    full_name = c("Brent Crude Oil", "WTI Crude Oil", "Dubai Crude Oil", "Canadian Heavy Crude Oil",
                  "Henry Hub Natural Gas", "National Balancing Point Natural Gas",
                  "SHFE Copper", "TSI 62% Iron Ore", "Newcastle Coal"),
    lat = c(56.905375, 35.985191, 25.211587, 57.897798, 
            29.89249643, 54.905764,
            31.228457, 36.063781, 54.968672),
    lon = c(3.549933, -96.765675, 55.269328, -101.469989, 
            -92.067833062, -3.246358,
            121.468519, 120.388040, -1.617063),
    code = c("ICE_B", "CME_CL", "CME_DC", "CME_WCC", "CME_NG", "ICE_M", "SHFE_CU", "CME_TIO", "ICE_NCF"),
    contracts = list(c(list()))
)

# Name each element of the contracts list
names(commods$contracts) <- commods$name

num_obs <- 50  # Maximum number of trading days to query data from
max_depth <- 13  # Maximum number of contracts to query per commodity

# Iterate through Contract Depths and Commodities to add historical data to commods$contracts
for (d in 1:max_depth) {
    for (c in 1:nrow(commods)) {
        try ({
            quandl_code <- paste0("CHRIS/", commods$code[c], d)
            commods$contracts[[c]][[d]] <- Quandl(quandl_code, limit = num_obs)
        }, silent = TRUE)
    }
}

# Query CNY/USD and GBP/USD exchange rates for futures price conversion
getFX("CNY/USD")
getFX("GBP/USD")

# Preprocess Data ------------------------------------------------------

# Store the latest exchange rates
cnyusd_rate <- as.numeric(CNYUSD)[1]
gbpusd_rate <- as.numeric(GBPUSD)[1]

# Iterate through depths then apply currency conversions
for (d in 1:max_depth) {
    try({
        # Convert NBP Natural Gas quotations to USD/MMBtu
        commods$contracts[["nbp"]][[d]]$Settle <- (commods$contracts[["nbp"]][[d]]$Settle / 10) * gbpusd_rate
        # Convert SHFE Copper Futures quotations to USD/mt
        commods$contracts[["sfe"]][[d]]$Settle <- (commods$contracts[["sfe"]][[d]]$Settle * cnyusd_rate)
        # Convert Canadian Heavy Crude quotations to a flat price in USD/bbl
        commods$contracts[["cad"]][[d]]$Settle <- (commods$contracts[["cad"]][[d]]$Settle + commods$contracts[["wti"]][[d]]$Settle)
    }, silent = FALSE)
}

# Compute the Market Structures for each commodity
commods$mkt_str <- rep(tibble(price = c(1)), 9)
names(commods$mkt_str) <- commods$name

for (c in 1:nrow(commods)) {
    for (d in 1:max_depth) {
        try({
            commods$mkt_str[[c]][d] <- commods$contracts[[c]][[d]]$Settle[1]
        }, silent = TRUE)
    }
}

# Compute the Calendar Spreads for each commodity
get_cal_spreads <- function(commodity) {
    
    # Calculate the contract depth from the commods tibble object
    contract_depth <- length(commods$mkt_str[[commodity]])
    
    # Calculate the number of 3-Month Spreads availabe
    available_spreads <- as.integer(contract_depth / 3)
    
    # Store each requisite data.frame object in df_list
    df_list <- list()
    i <- 1
    for(d in seq(1, contract_depth, by = 3)) {
        df_list[[i]] <- commods$contracts[[commodity]][[d]]
        i <- i + 1
    }
    
    # Left join all data.frames then select only Date and Settlement Price columns
    cal_spreads <- df_list %>% 
        reduce(left_join, by = "Date") %>% 
        select(matches("Date|Settle"))
    
    # Calculate the 3M, 6M etc. Spreads by iterating through available_spreads
    for (i in 1:available_spreads) {
        col_idx <- i + 2
        cal_spreads[, paste("Spread", i, sep = "_")] <- cal_spreads[, col_idx] - cal_spreads[, 2]
    }
    
    # Select only Date and Spread columns
    cal_spreads <- cal_spreads %>% select(matches("Date|Spread"))
    
    return(cal_spreads)
}

brt_cal_spreads <- get_cal_spreads("brt")  # Get Brent Calendar Spreads
wti_cal_spreads <- get_cal_spreads("wti")  # Get WTI Calendar Spreads
hhb_cal_spreads <- get_cal_spreads("hhb")  # Get Henry Hub Nat Gas Calendar Spreads
nbp_cal_spreads <- get_cal_spreads("nbp")  # Get NBP Nat Gas Calendar Spreads

# Compute Location Spreads given two commodity indices
get_loc_spreads <- function(c1, c2, cost) {

    # Left join the front-month contracts of each commodity
    joined <- left_join(commods$contracts[[c1]][[1]], commods$contracts[[c2]][[1]], by = "Date")

    # Compute the spread between both commodities and spread net of transportation costs
    loc_spreads <- joined %>% 
        select(matches("Date|Settle")) %>% 
        mutate(Spread = Settle.x - Settle.y) %>% 
        mutate(Spread = ifelse(is.na(Spread), mean(Spread, na.rm = TRUE), Spread)) %>% 
        mutate(Spread_Net = Spread - cost)
    
    return(loc_spreads)
}

brt_wti_spread <- get_loc_spreads("brt", "wti", cost = 4)  # Get Spread between Brent and WTI
brt_dub_spread <- get_loc_spreads("brt", "dub", cost = 5)  # Get Spread between Brent and Dubai
wti_cad_spread <- get_loc_spreads("wti", "cad", cost = 24)  # Get Spread betweem WTI and Canadian Heavy Crude
hhb_nbp_spread <- get_loc_spreads("hhb", "nbp", cost = 0.51)  # Get Spread between Henry Hub and NBP Natural Gas

# Server-Side Logic -------------------------------------------------------

# Define a function to visualize market structure plots
plot_mkt_str <- function(commodity) {
    
    # Convert the numeric vector of contract prices to a data.frame
    df <- as.data.frame(commods$mkt_str[[commodity]])
    colnames(df) <- "Price"
    
    p <- renderPlotly({
        ggplotly(
            df %>%
               ggplot(aes(index(df), Price)) +
               geom_point() +
               geom_line() +
               scale_y_continuous(labels = dollar) +
               labs(x = "Months from Today", y = "Price for Delivery") +
               theme_minimal()
        )
    })
    
    return(p)
}

# Define a function to visualize calendar spreads
plot_cal_spr <- function(df, spread, line_color = "green") {
    
    p <-  renderPlotly(
        ggplotly(
            df %>% 
                ggplot(aes_string("Date", paste("Spread", spread, sep = "_"))) +
                geom_line(color = line_color, alpha = 0.8) +
                geom_hline(yintercept = 0, alpha = 0.3) +
                scale_y_continuous(labels = dollar) +
                labs(x = "Date", y = "Spread") +
                theme_minimal()
        )
    )
    
    return(p)
}

# Define a function to visualize Location Spreads
plot_loc_spr <- function(df) {
    
    p <- renderPlotly(
        ggplotly(
            df %>% 
                ggplot(aes(x = Date)) +
                geom_line(aes(y = Spread, color = "Location Spread"), alpha = 1) +
                geom_line(aes(y = Spread_Net, color = "Net of Transportation"), alpha = 1) +
                scale_y_continuous(labels = dollar) +
                geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
                labs(x = "Time", y = "Spread", color = "") +
                theme(legend.title = element_blank()) +
                theme_minimal()
        ) %>% 
            layout(legend = list(x= 100, y = 0.6))
    )
    
    return(p)
}

# Back-end Logic
server <- function(input, output) {
    
    # Tab 1: Market Prices - Leaflet Geospatial Plot
    output$leaflet_price_plot <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addMarkers(
                lng = commods$lon,
                lat = commods$lat,
                popup = paste0(
                    commods$full_name, ": $", 
                    dollar(unlist(lapply(commods$mkt_str, '[[', 1), use.names = FALSE)))
                )
    })
    
    # Tab 2: Market Structures
    output$brt_mkt_str <- plot_mkt_str("brt")
    output$wti_mkt_str <- plot_mkt_str("wti")
    output$dub_mkt_str <- plot_mkt_str("dub")
    output$cad_mkt_str <- plot_mkt_str("cad")
    output$hhb_mkt_str <- plot_mkt_str("hhb")
    output$nbp_mkt_str <- plot_mkt_str("nbp")
    
    # Tab 3.1: Brent Calendar Spreads
    output$brent_spread_3M <- plot_cal_spr(brt_cal_spreads, 1)
    output$brent_spread_6M <- plot_cal_spr(brt_cal_spreads, 2)
    output$brent_spread_9M <- plot_cal_spr(brt_cal_spreads, 3)
    output$brent_spread_12M <- plot_cal_spr(brt_cal_spreads, 4)

    # Tab 3.2: WTI Calendar Spreads
    output$wti_spread_3M <- plot_cal_spr(wti_cal_spreads, 1, line_color = "red")
    output$wti_spread_6M <- plot_cal_spr(wti_cal_spreads, 2, line_color = "red")
    
    # Tab 3.3: Henry Hub Nat Gas Calendar Spreads
    output$hhb_spread_3M <- plot_cal_spr(hhb_cal_spreads, 1)
    output$hhb_spread_6M <- plot_cal_spr(hhb_cal_spreads, 2)
    output$hhb_spread_9M <- plot_cal_spr(hhb_cal_spreads, 3)
    output$hhb_spread_12M <- plot_cal_spr(hhb_cal_spreads, 4)
    
    # Tab 3.4: NBP Nat Gas Calendar Spreads
    output$nbp_spread_3M <- plot_cal_spr(nbp_cal_spreads, 1)
    output$nbp_spread_6M <- plot_cal_spr(nbp_cal_spreads, 2)
    output$nbp_spread_9M <- plot_cal_spr(nbp_cal_spreads, 3)
    output$nbp_spread_12M <- plot_cal_spr(nbp_cal_spreads, 4)
    
    # Tab 4: Location Spreads
    output$brt_wti_spread <- plot_loc_spr(brt_wti_spread)
    output$brt_dub_spread <- plot_loc_spr(brt_dub_spread)
    output$wti_cad_spread <- plot_loc_spr(wti_cad_spread)
    output$hhb_nbp_spread <- plot_loc_spr(hhb_nbp_spread)
    
    # Tab 5: Price Forecasts
    
    # Convert the front-month Brent Contract dataset to an xts object
    brent_xts <- as.xts(
        commods$contracts[["brt"]][[1]][ ,-1], 
        order.by = as.POSIXct(commods$contracts[["brt"]][[1]]$Date))
    
    # Forecast prices with an ARIMA Model
    output$brent_forecast_arima <- renderPlot({
        brent_xts$Settle %>%
            auto.arima(d = 1, D = 1) %>% 
            forecast() %>% 
            autoplot() +
            scale_y_continuous(labels = dollar) +
            theme_minimal()
        })
    
    # Forecast prices with an ETS Model
    output$brent_forecast_ets <- renderPlot({
        brent_xts$Settle %>%
            ets() %>% 
            forecast() %>%
            autoplot() +
            theme_minimal()
        })
}

# User Interface ----------------------------------------------------------

# Customize the header object of dashboard
header <- dashboardHeader(
    title = "Commodity Trading Analytics in Shiny",
    titleWidth = 400
)

# Customize the sidebar object of dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Market Prices", tabName = "prices", icon = icon("comment-dollar")),
        menuItem("Market Structures", tabName = "structures", icon = icon("balance-scale")),
        menuItem("Calendar Spreads", icon = icon("calendar"),
                 menuSubItem("Brent Crude", tabName = "brent_calendar"),
                 menuSubItem("WTI Crude", tabName = "wti_calendar"),
                 menuSubItem("Henry Hub Gas", tabName = "hhb_calendar"),
                 menuSubItem("NBP Gas", tabName = "nbp_calendar")),
        menuItem("Location Spreads", tabName = "location", icon = icon("globe-asia")),
        menuItem("Price Forecasts", tabName = "forecasts", icon = icon("chart-line"))
    )
)

# Customize the body object of dashboard
body <- dashboardBody(
    tabItems(
        # Tab 1: Market Prices
        tabItem(
            "prices",
            fluidRow(
                valueBox(
                    value = dollar(commods$contracts[[1]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of ICE Brent Crude",
                    icon = icon("tint")
                ),
                valueBox(
                    value = dollar(commods$contracts[[2]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of NYMEX WTI Crude",
                    icon = icon("tint")
                ),
                valueBox(
                    value = dollar(commods$contracts[[3]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of CME Dubai Crude",
                    icon = icon("tint")
                )
            ),
            fluidRow(
                valueBox(
                    value = dollar(commods$contracts[[4]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of Canadian Heavy Crude",
                    icon = icon("tint")
                ),
                valueBox(
                    value = dollar(commods$contracts[[5]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of Henry Hub Natural Gas",
                    icon = icon("fire")
                ),
                valueBox(
                    value = dollar(commods$contracts[[6]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of NBP Natural Gas",
                    icon = icon("fire")
                )
            ),
            fluidRow(
                valueBox(
                    value = dollar(commods$contracts[[7]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of SHFE Copper",
                    icon = icon("warehouse")
                ),
                valueBox(
                    value = dollar(commods$contracts[[8]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of TSI 62% Iron Ore CFR China",
                    icon = icon("warehouse")
                ),
                valueBox(
                    value = dollar(commods$contracts[[9]][[1]]$Settle[1]),
                    subtitle = "Latest Settlement Price of Newcastle Coal",
                    icon = icon("warehouse")
                )
            ),
            fluidRow(
                leafletOutput("leaflet_price_plot")
            )
        ),
        # Tab 2: Market Structures
        tabItem(
            "structures",
            fluidRow(
                box(
                    title = "Market Structure of Brent Crude Oil",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 4,
                    "Indexed from M to M+12",
                    plotlyOutput("brt_mkt_str", height = 350)
                ),
                box(
                    title = "Market Structure of WTI Crude Oil",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 4,
                    "Indexed from M to M+12",
                    plotlyOutput("wti_mkt_str", height = 350)
                ),
                box(
                    title = "Market Structure of Dubai Crude Oil",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 4,
                    "Indexed from M to M+1",
                    plotlyOutput("dub_mkt_str", height = 350)
                )
            ),
            fluidRow(
                box(
                    title = "Market Structure of Canadian Heavy Crude",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 4,
                    "Indexed from M to M+8",
                    plotlyOutput("cad_mkt_str", height = 350)
                ),
                box(
                    title = "Market Structure of Henry Hub Natural Gas",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 4,
                    "Indexed from M to M+12",
                    plotlyOutput("hhb_mkt_str", height = 350)
                ),
                box(
                    title = "Market Structure of NBP Natural Gas",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 4,
                    "Indexed from M to M+12",
                    plotlyOutput("nbp_mkt_str", height = 350)
                )
            )
        ),
        # Tab 3.1: Brent Calendar Spreads
        tabItem(
            "brent_calendar",
            fluidRow(
                box(
                    title = "3-Month Calendar Spread of Brent Crude Oil",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("brent_spread_3M", height = 350)
                ),
                box(
                    title = "6-Month Calendar Spread of Brent Crude Oil",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("brent_spread_6M", height = 350)
                )
            ),
            fluidRow(
                box(
                    title = "9-Month Calendar Spread of Brent Crude Oil",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("brent_spread_9M", height = 350)
                ),
                box(
                    title = "12-Month Calendar Spread of Brent Crude Oil",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("brent_spread_12M", height = 350)
                )
            )
        ),
        # Tab 3.2: WTI Calendar Spreads
        tabItem(
            "wti_calendar",
            fluidRow(
                box(
                    title = "3-Month Calendar Spread of WTI Crude Oil",
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("wti_spread_3M", height = 350)
                ),
                box(
                    title = "6-Month Calendar Spread of WTI Crude Oil",
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("wti_spread_6M", height = 350)
                )
            )
        ),
        # Tab 3.3 Henry Hub Calendar Spreads
        tabItem(
            "hhb_calendar",
            fluidRow(
                box(
                    title = "3-Month Calendar Spread of Henry Hub Natural Gas",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("hhb_spread_3M", height = 350)
                ),
                box(
                    title = "6-Month Calendar Spread of Henry Hub Natural Gas",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("hhb_spread_6M", height = 350)
                )
            ),
            fluidRow(
                box(
                    title = "9-Month Calendar Spread of Henry Hub Natural Gas",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("hhb_spread_9M", height = 350)
                ),
                box(
                    title = "12-Month Calendar Spread of Henry Hub Natural Gas",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("hhb_spread_12M", height = 350)
                )
            )
        ),
        # Tab 3.4 NBP Calendar Spreads
        tabItem(
            "nbp_calendar",
            fluidRow(
                box(
                    title = "3-Month Calendar Spread of NBP Natural Gas",
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("nbp_spread_3M", height = 350)
                ),
                box(
                    title = "6-Month Calendar Spread of NBP Natural Gas",
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("nbp_spread_6M", height = 350)
                )
            ),
            fluidRow(
                box(
                    title = "9-Month Calendar Spread of NBP Natural Gas",
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("nbp_spread_9M", height = 350)
                ),
                box(
                    title = "12-Month Calendar Spread of NBP Natural Gas",
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    "Using a 50-Day Time Series",
                    plotlyOutput("nbp_spread_12M", height = 350)
                )
            )
        ),
        # Tab 4: Location Spreads
        tabItem(
            "location",
            fluidRow(
                box(
                    title = "Brent-WTI Location Spread",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    "Using a 50-Day Time Series",
                    plotlyOutput("brt_wti_spread", height = 350)
                ),
                box(
                    title = "Brent-Dubai Location Spread",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    "Using a 50-Day Time Series",
                    plotlyOutput("brt_dub_spread", height = 350)
                )),
            fluidRow(
                box(
                    title = "WTI-Canadian Location Spread",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    "Using a 50-Day Time Series",
                    plotlyOutput("wti_cad_spread", height = 350)
                ),
                box(
                    title = "Henry Hub-NBP Location Spread",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    "Using a 50-Day Time Series",
                    plotlyOutput("hhb_nbp_spread", height = 350)
                ))
        ),
        # Tab 5: Price Forecasts
        tabItem(
            "forecasts",
            fluidRow(
                box(
                    title = "ARIMA Forecasts of Brent Crude Prices",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    "Using a 50-Day Time Series",
                    plotOutput("brent_forecast_arima", height = 350)
                )
            ),
            fluidRow(
                box(
                    title = "ETS Forecasts of Brent Crude Prices",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    "Using a 50-Day Time Series",
                    plotOutput("brent_forecast_ets", height = 350)
                )
            )
        )
    )
)

# Define UI for application
ui <- dashboardPage(
    title = "Commodity Trading Dashboards",
    header = header,
    sidebar = sidebar,
    body = body
)

# Run Application -----------------------------------------------------

shinyApp(ui = ui, server = server)
