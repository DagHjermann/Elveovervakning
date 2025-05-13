# Elveovervakning

Maps and figures made annually for NIVA's river monitoring   

Note:   
* scripts with "2023" (and later) in the name are for *data year* 2023 (report made in 2024)  
* scripts with "2022" in the name are for *report year* 2022 (data from 2021)  

Stations are from different areas each year - pick map script based on this:
* 2020 stations: Southern Norway only, with clusters of stations in West+South Norway shown iin boxes  
* 2021 stations: Southern Norway + Finnmark  
* 2022 stations: Trøndelag + Nordland  
* 2023 stations: Southern Norway only (most stations around Oslo fjord)
* 2024 stations: Trøndelag + Nordland + a few in So. Norway (Tyrifjorden, Randsfjorden)  

Label placement (for the maps with station labels, "labelmap" in file name
* label placement done using R coding in the first years  
* label placement done using Inkscape adjustments for 2023 data

Two main parts:  

### 1. Figures for NIVA's main river monitoring program (Elveovervåkningsprogrammet)  

* Script 07a - get and fix data  

* Script 07b - map for Methods chapter (stations with labels)  
    - Using ggplot2 for maps  
    - 2021: script does the label placement  
    - 2022: label placement mostly done in Inkscape (can be copied from map to map, though)  
    
* Script 07c - map for Results chapter ("pies" with labels)  
    - Similar to 07b but adding pies  

* Script 07d - barplot figures  

* Products in Figures/2021_main, Figures/2022_main etc.  


### 2. Figures for monitoring rivers entering Mjøsa (and other big lakes?)  

* Script '20_4stations'  
    - Using interactive maps (OpenStreetmap for background map + plotrix::floating.pie)    
  
* Products in Figures/2021, Figures/2022 etc.   

### 3. Calculation of climate (temp. and precipitation) trends for a table in the report  

* See Project `Hjelp/Elveovervaakning_klima`   


