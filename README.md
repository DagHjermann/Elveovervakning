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
    - For the post-processing in Inkscape, see "Appendix"  
    
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

### Appendix: post-processing in Inkscape  
- When you open the figure, all objects are grouped; select Object:Ungroup  
- Now labels can be moved manually  
- Mouse wheel to move up/down, shift + mouse wheel to move left/right  
- To edit text, use Text Tool (use arrow tool to resize object if needed)  
- To change the end point of lines, use the node tool (second tool from top, or F2)  
- To add lines, use Bezier and Straight lines tool (left click for each end, right click for finishing)  
    - F1 to get out of "line drawing mode"
    - Use shift + click on the colors on the bottom to change line color  
    - Use shift-ctrl F to change stroke width (used 1.4) and rounded cap  
    - F1 
    - To set this as default: 
      1) Leave the line selected. (F1 - Then click on line)
      2) Go to preferences (Shift Ctrl P). 
      3) Select Tool - Pen 
      4) Click Take from selection   
- Save as svg with "_adj" in the file name  
    


