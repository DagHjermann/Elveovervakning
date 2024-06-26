# Elveovervakning

Maps and figures made annually for NIVA's river monitoring   

Note:   
* scripts with "2023" in the name are for *data year* 2023 (report made in 2024)  
* scripts with "2022" in the name are for *report year* 2022 (data from 2021)  

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


