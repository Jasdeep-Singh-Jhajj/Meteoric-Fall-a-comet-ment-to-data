# Data
-   **Meteorite_Landings.csv**: The complete set of information of all known meteorite landings on Earth, sourced from The Meteoritical Society via NASA's Open Data Portal.

# Codebook for Meteorite_Landings.csv Dataset

## Variable Names and Descriptions:

-   **name**: The name of the meteorite (usually the location of the meteorite landing)
-   **id**: The unique identifier number assigned to the meteorite.
-   **nametype**: Can be one of two different categories:
    -   *valid*: A regular meteorite.
    -   *relict*: A meteorite that has been degraded over the years due to weather.
-   **recclass**: The recommended class of the meteorite; that is classified based on certain characteristics of the meteorite such as, chemical, isotopic, and mineralogical properties.
-   **mass(g)**: The mass of the meteorite, given in grams.
-   **fall**: Can be one of two different categories:
    -   *fell*: Classified as fell when the fall of the meteorite is observed.
    -   *found*: Classified as found when the fall of the meteorite is not observed, but the meteorite was found later.
-   **reclat**: The latitude of the meteorite's landing.
-   **reclong**: The longitude of the meteorite's landing.
-   **GeoLocation**: Combination of the latitude and longitude of the meteorite's landing.

## Data Types:

-   **name**: Character
-   **id**: Double
-   **nametype**: Categorical (2 levels)
-   **recclass**: Character
-   **mass(g)**: Double
-   **fall**: Categorical (2 levels)
-   **reclat**: Double
-   **reclong**: Double
-   **GeoLocation**: Character



