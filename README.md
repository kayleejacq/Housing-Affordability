# Housing-Affordability
## Project Overview
The Housing Affordability dashboard is designed to analyze and visualize the relationship between housing prices and income distribution throughout Ireland. The goal is to provide stakeholders, policymakers, and the general public with a better understanding of regional housing affordability and economic disparity.

## Features:
### 1. Housing Price Distribution
- Filters: Customize the dataset based on County, Price Range, VAT Exclusive, Full Market Price, and House Description.
- Visualization: Includes an interactive map showing filtered housing data. Each marker represents a house, it will show the description for each houses when clicked. The markers are sized based on the Price Ratio and colored according to the option selected.
- Summary: Total properties shown and Average housing price of the filtered dataset.
- Data Table: A downloadable table of the dataset.

### 2. Earnings Distribution
- Filters: Filter the dataset by County, Sex, and Age Group.
- Visualization:
  - An interactive map show counties and the average earnings when clicked.
  - An interactive Plotly boxplot to show the earnings distribution based on Sex and Age Group.
- Data Table: A downloadable table of the dataset.

### 3. Housing Affordability Analysis
- Goal:
  - Combine the Housing and Earnings datasets.
  - Calculate the Mortgage Affordability Index, assuming:
    - Loan to Value (LTV): 90%.
    - Mortgage Term: 25 years.
    - Interest Rate: 4.1%.
  - Categorize affordability using the Cost Burden metric:
    - If Cost Burden ≤ 30%, the property is categorized as affordable.
- Filters: Apply filters for County, VAT Exclusive, Full Market Price, House Description, Sex, and Age Group.
- Visualization:
  - An interactive map displaying:
    - County name.
    - Average Housing Price.
    - Average Earnings.
    - Income Affordability Index.
- Data Table: A downloadable table of the dataset.

## Data Sources & References:
- Housing Prices Dataset: https://www.propertypriceregister.ie/website/npsra/pprweb.nsf/PPRDownloads?OpenForm&File=PPR-2023.csv&County=ALL&Year=2023&Month=ALL
- Earnings Distribution Dataset: DEA06 - https://data.cso.ie/


### Disclaimer
The geolocation data displayed on the map is sourced using Mapbox geocoding API. Some geolocations may be inaccurate or approximate due to limitations in the geocoding process.
