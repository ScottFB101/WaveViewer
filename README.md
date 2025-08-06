# 🏄‍♂️🏄‍♀️WaveViewer🏄‍♀️🏄‍♂️ 

## Overview

WaveViewer is a **San Francisco swell visualizer** that fetches and displays wave buoy data (e.g., height, period, direction) to help users understand both current and historical ocean swell conditions present at Ocean Beach. This is a passion project I undertook in the hopes of better understanding swell conditions at OB, and therefore catching more waves at my favorite surf spot.

The Shiny app pulls [CDIP buoy data](https://sensors.ioos.us/#metadata/103447/station/data) , processes it into visual formats, and generate charts or maps that illustrate swell trends and patterns. It focuses on clean data ingestion, visual analytics, and presentation of swell information relevant to surfers, mariners, and coastal analysts, packaged in a reproducible and sharable dashboard or report format.

## Uses

1. **See current swell characteristics, and visual historical swell trends**
![Current and Historical Swell Data](current_and_historical_snapshot.jpg)

2. **Wave roses provide a visual summary of ocean swell by showing the frequency and direction of wave energy, helping users quickly understand dominant swell patterns over time.**
![Wave Roses for direction, height, and period](wave_rose_snapshot.jpg)

3. **Location information on the buoy providing the information**
![Buoy location via Google Maps](buoy_location_snapshot.jpg)
