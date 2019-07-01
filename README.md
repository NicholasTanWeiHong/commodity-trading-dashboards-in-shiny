# Commodity Trading Dashboards with Shiny

This project leverages on the ['shiny'](https://shiny.rstudio.com/) package in R to prototype a series of dashboards tailored for commodity trading.

Specifically, it uses a mix of geospatial visualizations (E.g. [leaflet](https://rstudio.github.io/leaflet/)) and Plotly graphs to inform the user of where key commodities are trading, as well as the magnitude of the spreads between them. Analysis of each type of spread opens up the possibility of executing physical arbitrages around and between the various forms of commodities.

The application is further broken down into multiple tabs with different functionalities.

### Market Prices

Contains a set of valueBox() objects and a leaflet() plot to easily identify where multiple commodities are trading at.

<p align="center">
  <img src="https://github.com/NicholasTanWeiHong/commodity-trading-dashboards-in-shiny/blob/master/images/market-prices.PNG?raw=true" alt="prices"/>
</p>

### Market Structures

Plots the market structures of various commodities to visualize the degree of backwardation/contango.

<p align="center">
  <img src="https://github.com/NicholasTanWeiHong/commodity-trading-dashboards-in-shiny/blob/master/images/market-structures.PNG?raw=true" alt="structures"/>
</p>

### Calendar Spreads

Plots multiple Calendar Spreads in 3-Month categories to identify the potential for storage arbs/time arbitrages.

<p align="center">
  <img src="https://github.com/NicholasTanWeiHong/commodity-trading-dashboards-in-shiny/blob/master/images/calendar-spreads.PNG?raw=true" alt="calendar"/>
</p>

### Location Spreads

Plots multiple Location Spreads between Crude Oil benchmarks and Natural Gas benchmarks to identify geographical arbitrages.

<p align="center">
  <img src="https://github.com/NicholasTanWeiHong/commodity-trading-dashboards-in-shiny/blob/master/images/location-spreads.PNG?raw=true" alt="location"/>
</p>

### Price Forecasts

Performs basic forecasting with the [forecast](https://www.rdocumentation.org/packages/forecast/versions/8.7) package in R.

<p align="center">
  <img src="https://github.com/NicholasTanWeiHong/commodity-trading-dashboards-in-shiny/blob/master/images/price-forecasts.PNG?raw=true" alt="forecasts"/>
</p>

# Future Plans
* Add a sixth tab 'Correlations' to identify relationships between prices and fundamental data (E.g. Supply & Demand Balances)
* Include some NLP/Text Mining functionalities using data from Twitter and qdap/tidytext packages
* Make transportation costs per Location Spread dynamic (E.g. Time Series data for Shipping Costs instead of a flat estimate)
* Add storage/transformation costs to Calendar Spreads to reflect real-world economics