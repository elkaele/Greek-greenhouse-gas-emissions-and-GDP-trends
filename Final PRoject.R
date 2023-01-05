# Greek emissions data analysis and trends visualizations
# GRAD-E1402-F-2022 Data Perspectives on Global Greenhouse Gas Emissions

install.packages("latticeExtra")
install.packages("lattice")
install.packages("ggrepel")                   

library(ggrepel) 
library(lattice)
library(latticeExtra)
library(tidyverse)
library(readxl)


# UN dataset on emissions (CO2, CH4, N20, F gasses and Greenhouse gasses), 
# sectors and countries from 1970 to 2020
# Obtained from Dr. William Lamb
essd_ghg_data_gwp100 <- read_excel("Documents/Data Perspectives/essd_ghg_data_gwp100.xlsx",
                                   sheet = 3)
View(essd_ghg_data_gwp100)

# World bank data on Greek GDP per capita trends from 1960 to 2021
# Obtained on from the World Bank website
gdp_greece <- read_excel("Documents/Data Perspectives/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_4701042.xls", 
                                                                     sheet = "Sheet1")

#This filters the UN dataset to show only data about Greece 
greece_em <- essd_ghg_data_gwp100 %>%
  filter(country == "Greece")

# Combines the World Bank GDP data and the UN data into one dataframe
greece_full <- left_join(greece_em, gdp_greece, by=c('year'='Year'))


# Indexes the GDP per capita values to 1, to show only the changes in GDP as we
# are interested in the trends
greece_full <- greece_full %>%
  mutate(GDP_pcap_index = GDP_pcap/greece_full$GDP_pcap[1])

# Indexes the total Green house gas emission values to 1, to show only the 
# general changes
greece_full <- greece_full %>%
  mutate(GHG_index = GHG/greece_full$GHG[1])


# Graphs general trend of total Greek emissions from 2005 to 2019
greece_full %>%
  filter(year>=2005) %>%
  filter(year<=2019) %>%
  ggplot(.,aes(x=year,y=GHG, color = "darkred")) +
  geom_smooth(method = "loess") +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    aspect.ratio = 9 / 16
  ) +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  ylab("GHG Emissions (MtCO2eq)") +
  ggtitle("Total Greenhouse Gas Emissions from 2005 to 2018") +
  theme(axis.title.x=element_blank())

#Graphs general trend of GDP per capita from 2015 to 2018
greece_full %>%
  filter(year>=2005) %>%
  filter(year<=2019) %>%
  ggplot(.,aes(x=year,y=GDP_pcap_index, color = "darkred")) +
  geom_smooth(method = "loess") +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    aspect.ratio = 9 / 16
  ) +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  ylab("GDP") +
  ggtitle("GDP per Capita from 2005 to 2018") +
  theme(axis.title.x=element_blank())

 
# Graphs emissions from the Greek energy sector, divided into its subsectors
greece_full %>%
  filter(sector_title == "Energy systems") %>%
  filter(year>=2005) %>%
  filter(year<=2019) %>%
  ggplot(.,aes(x=year,y=GHG,fill=subsector_title)) +
  geom_area() +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 1, colour = "white"),
    legend.key.width = unit(0.15, "cm"),
    legend.key.height = unit(0.04, "cm"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    aspect.ratio = 9 / 16
  ) +
  scale_x_continuous(breaks=seq(2006, 2018, by = 2)) +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  geom_line(aes(x=year,y=GDP_pcap*2500),color = "red4", size = 1.5) +
  guides(fill=guide_legend(title="Subsector Name")) +
  ylab("GHG emissions (MtCO2eq)") +
  ggtitle("GHG Emissions from Energy Systems vs. GDP per capita (2005-2018)") +
  theme(axis.title.x=element_blank()) 

# Graphs emissions from Greek Agriculture Forestry and other land uses, 
# divided into its subsectors
greece_full %>%
  filter(sector_title == "AFOLU") %>%
  filter(year>=2005) %>%
  filter(year<=2019) %>%
  ggplot(.,aes(x=year,y=GHG,fill=subsector_title)) +
  geom_area() +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 1, colour = "white"),
    legend.key.width = unit(0.15, "cm"),
    legend.key.height = unit(0.04, "cm"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    aspect.ratio = 9 / 16
  ) +
  scale_x_continuous(breaks=seq(2006, 2018, by = 2)) +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  geom_line(aes(x=year,y=GDP_pcap*300),color = "red4", size = 1.5) +
  guides(fill=guide_legend(title="Subsector Name")) +
  ylab("GHG emissions (MtCO2eq)") +
  ggtitle("GHG Emissions from AFOLU vs. GDP per capita (2005-2018)") +
  theme(axis.title.x=element_blank())

# Graphs emissions from the Greek Buildings sector, divided into its subsectors
greece_full %>%
  filter(sector_title == "Buildings") %>%
  filter(year>=2005) %>%
  filter(year<=2019) %>%
  ggplot(.,aes(x=year,y=GHG,fill=subsector_title)) +
  geom_area() +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 1, colour = "white"),
    legend.key.width = unit(0.15, "cm"),
    legend.key.height = unit(0.04, "cm"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    aspect.ratio = 9 / 16
  ) +
  scale_x_continuous(breaks=seq(2006, 2018, by = 2)) +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  geom_line(aes(x=year,y=GDP_pcap*500),color = "red4", size = 1.5) +
  guides(fill=guide_legend(title="Subsector Name")) +
  ylab("GHG emissions (MtCO2eq)") +
  ggtitle("GHG Emissions from Buildings vs. GDP per capita (2005-2018)") +
  theme(axis.title.x=element_blank())

# Graphs emissions from the Greek Industry sector, divided into its subsectors
greece_full %>%
  filter(sector_title == "Industry") %>%
  filter(year>=2005) %>%
  filter(year<=2019) %>%
  ggplot(.,aes(x=year,y=GHG,fill=subsector_title)) +
  geom_area() +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 1, colour = "white"),
    legend.key.width = unit(0.15, "cm"),
    legend.key.height = unit(0.04, "cm"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    aspect.ratio = 9 / 16
  ) +
  scale_x_continuous(breaks=seq(2006, 2018, by = 2)) +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  geom_line(aes(x=year,y=GDP_pcap*1200),color = "red4", size = 1.5) +
  guides(fill=guide_legend(title="Subsector Name")) +
  ylab("GHG emissions (MtCO2eq)") +
  ggtitle("GHG Emissions from Industry vs. GDP per capita (2005-2018)") +
  theme(axis.title.x=element_blank())

# Graphs emissions from the Greek transport sector, divided into its subsectors
greece_full %>%
  filter(sector_title == "Transport") %>%
  filter(year>=2005) %>%
  filter(year<=2019) %>%
  ggplot(.,aes(x=year,y=GHG,fill=subsector_title)) +
  geom_area() +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 1, colour = "white"),
    legend.key.width = unit(0.15, "cm"),
    legend.key.height = unit(0.04, "cm"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    aspect.ratio = 9 / 16
  ) +
  scale_x_continuous(breaks=seq(2006, 2018, by = 2)) +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  geom_line(aes(x=year,y=GDP_pcap*1200),color = "red4", size = 1.5) +
  guides(fill=guide_legend(title="Subsector Name")) +
  ylab("GHG emissions (MtCO2eq)") +
  ggtitle("GHG Emissions from Transport vs. GDP per capita (2005-2018)") +
  theme(axis.title.x=element_blank())
