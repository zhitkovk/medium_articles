# Python script to load data ----------------------------------------------
# Here it is provided as a reference

# import pandas as pd
# import wbdata as wd
# 
# # define a period of time
# start_year = 2013
# end_year = 2017
# 
# # list of countries under inflation targeting monetary policy regime
# countries = ['AM', 'AU', 'AT', 'BE', 'BG', 'BR', 'CA', 'CH', 'CL', 'CO', 'CY', 'CZ', 'DE', 'DK', 'XC', 'ES', 'EE', 'FI', 'FR', 'GB', 'GR', 'HU', 'IN', 'IE', 'IS', 'IL', 'IT', 'JM', 'JP', 'KR', 'LK', 'LT', 'LU', 'LV', 'MA', 'MD', 'MX', 'MT', 'MY', 'NL', 'NO', 'NZ', 'PK', 'PE', 'PH', 'PL', 'PT', 'RO', 'RU', 'SG', 'SK', 'SI', 'SE', 'TH', 'TR', 'US', 'ZA']
# 
# # set dictionary for wbdata
# inflation = {'FP.CPI.TOTL.ZG': 'CPI_annual', 'NY.GDP.MKTP.KD.ZG': 'GDP_annual'}
# 
# # download wb data
# df = wd.get_dataframe(inflation, country = countries, data_date = (pd.datetime(start_year, 1, 1), pd.datetime(end_year, 1, 1)))
# 
# # print(df.head())
# df.to_csv('WB_data.csv', index = False)


# Libraries ---------------------------------------------------------------


library('data.table')
library('ggplot2')
library('grid')
library('gridExtra')
library('forcats')


# Prepare data ------------------------------------------------------------


dt_cpi <- fread('data/WB_data.csv')
setkey(dt_cpi, "country")


dt_cpi_s <- dt_cpi
dt_cpi_s[country == 'Czech Republic', country := "Czech\nRepublic"]
dt_cpi_s[country == 'Korea, Rep.', country := "Republic\nof Korea"]
dt_cpi_s[country == 'New Zealand', country := "New\nZealand"]
dt_cpi_s[country == 'Russian Federation', country := "Russian\nFederation"]
dt_cpi_s[country == 'South Africa', country := "South\nAfrica"]
dt_cpi_s[country == 'Slovak Republic', country := "Slovak\nRepublic"]
dt_cpi_s[country == 'United Kingdom', country := "United\nKingdom"]
dt_cpi_s[country == 'United States', country := "United\nStates"]


dt_cpi_melt <- melt.data.table(dt_cpi_s, id.vars = c("country", "date"))


# Data for additional labels inside the plot ------------------------------


# Color encoding on the first small multiple instead of a legend
dt_cpi_melt[, lbl := ""]
dt_cpi_melt[(country %in% c("Armenia", "Jamaica")) &
              (date == 2017) & (variable == "CPI_annual"), lbl := "CPI"]
dt_cpi_melt[(country %in% c("Armenia", "Jamaica")) &
              (date == 2017) & (variable == "GDP_annual"), lbl := "GDP"]

# Position of new labels
dt_cpi_melt[, y_pos := c(0)]
dt_cpi_melt[(country == "Armenia") &
              (date == 2017) & (variable == "CPI_annual"), y_pos := -9]
dt_cpi_melt[(country == "Armenia") &
              (date == 2017) & (variable == "GDP_annual"), y_pos := 8]

dt_cpi_melt[(country == "Jamaica") &
              (date == 2017) & (variable == "CPI_annual"), y_pos := 6]
dt_cpi_melt[(country == "Jamaica") &
              (date == 2017) & (variable == "GDP_annual"), y_pos := -8.5]


# Outlier labels
dt_cpi_melt[, y_pos_out := c(0)]
dt_cpi_melt[(country == "Russian\nFederation") &
              (date == 2015) & (variable == "CPI_annual"), y_pos_out := 15.9]
dt_cpi_melt[(country == "Russian\nFederation") &
              (date == 2015) & (variable == "GDP_annual"), y_pos_out := 2.5]

dt_cpi_melt[(country == "Ireland") &
              (date == 2015) & (variable == "GDP_annual"), y_pos_out := 20]


# Base plot ---------------------------------------------------------------


p <- 
  ggplot() +
  # Pseudo grid line for finer control
  geom_hline(yintercept = -10, color = "#BFBFBF", size = 0.2) +
  geom_hline(yintercept = -5, color = "#BFBFBF", size = 0.1) +
  geom_hline(yintercept = 0, color = "#707070", size = 0.3) +
  geom_hline(yintercept = 5, color = "#BFBFBF", size = 0.1) +
  geom_hline(yintercept = 10, color = "#BFBFBF", size = 0.2) +
  geom_hline(yintercept = 15, color = "#BFBFBF", size = 0.1) +
  geom_hline(yintercept = 20, color = "#BFBFBF", size = 0.2) +
  
  # Data
  geom_line(data = dt_cpi_melt, aes(x = date, y = value, color = variable)) +
  geom_point(data = dt_cpi_melt, aes(x = date, y = value, color = variable), 
             size = 0.5, shape = 20, show.legend = FALSE) +
  geom_text(data = dt_cpi_melt, aes(x = date, y = y_pos,
                                    color = variable, label = lbl),
            show.legend = FALSE,
            family = "Inter UI",
            size = 3.5,
            hjust = 0.9,
            vjust = 0) +
  
  # Outlier labels
  geom_text(data = dt_cpi_melt[(country == "Russian\nFederation") & (date == 2015), ], 
            aes(x = date, y = y_pos_out, 
                color = variable, 
                label = round(value, 2)),
            show.legend = FALSE,
            family = "Inter UI",
            size = 3.5,
            hjust = 0.5,
            vjust = 0) +
  geom_text(data = dt_cpi_melt[(country == "Ireland") & 
                                 (date == 2015) & 
                                 (variable == "GDP_annual"), ], 
            aes(x = 2015.2, y = y_pos_out, 
                color = variable, label = round(value, 2)),
            show.legend = FALSE,
            family = "Inter UI",
            hjust = 0,
            vjust = 0) +
  
  # Facets setup
  facet_wrap(~country, ncol = 6) +
  
  # Scales
  scale_x_continuous(labels = c("2013", "14", "15", "16", "17")) +
  scale_y_continuous(sec.axis = dup_axis(name = "")) +
  scale_color_manual(label = c("CPI", "GDP"), values = c("#aa8c3b", "#003831")) +
  
  # Labs
  labs(x = "",
       y = "Annual\nGDP growth rate\nand CPI, %",
       # subtitle = "Period: 2013-2017",
       title = "Inflation (CPI) and GDP Growth Rate\nin Inflation Targeting Countries") +
  
  # Theme
  theme_minimal() +
  theme(panel.spacing = unit(1.05, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(t = 15, b = 50, l = 0, r = 30, unit = "pt"),
        
        # Axes
        axis.text.x = element_text(family = "Roboto Condensed", size = 9, color = "#3B3B3B"),
        axis.text.y = element_text(family = "Roboto Condensed", size = 10, color = "#3B3B3B"),
        axis.title.y = element_text(family = "Inter UI", size = 12, color = "#3B3B3B", 
                                    hjust = 1,
                                    angle = 0,
                                    # debug = TRUE,
                                    margin = margin(r = 10, unit = "pt")),
        # Legend
        legend.position = "",
        
        # Titles
        plot.title = element_text(family = "Inter UI Black", size = 18, color = "#3B3B3B"),
        strip.text = element_text(family = "Roboto Condensed", size = 12.5, color = "#3B3B3B")
  )



# Helper plot to extract the x axis with 6 labels -------------------------


clist <- c("Armenia", "Australia", "Austria", "Belgium", "Brazil", "Bulgaria")

p6 <- 
  ggplot() +
  # Data
  geom_line(data = dt_cpi_melt[country %in% clist], aes(x = date, y = value, color = variable)) +
  geom_point(data = dt_cpi_melt[country %in% clist], aes(x = date, y = value, color = variable), 
             size = 0.5, shape = 20, show.legend = FALSE) +
  facet_wrap(~country, ncol = 6) +
  
  # Scales
  scale_x_continuous(labels = c("2013", "14", "15", "16", "17")) +
  scale_y_continuous(sec.axis = dup_axis(name = "")) +
  scale_color_manual(label = c("CPI", "GDP"), values = c("#aa8c3b", "#003831")) +
  
  # Theme
  theme_minimal() +
  theme(panel.spacing = unit(1.05, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(t = 15, b = 10, l = 0, r = 30, unit = "pt"),
        
        # Axes
        axis.text.x = element_text(family = "Roboto Condensed", size = 9, color = "#3B3B3B"),
        axis.text.y = element_text(family = "Roboto Condensed", size = 10, color = "#3B3B3B"),
        axis.title.y = element_text(family = "Inter UI", size = 12, color = "#3B3B3B", 
                                    hjust = 1,
                                    angle = 0,
                                    # debug = TRUE,
                                    margin = margin(r = 10, unit = "pt")),
        legend.position = "",
        
        # Titles
        strip.text = element_text(family = "Roboto Condensed", size = 12.5, color = "#3B3B3B")
  )


# Convert plots to grobs to add xaxis on the center of facets -------------


g <- ggplotGrob(p)
p6 <- ggplotGrob(p6)


# Locate the data panels
panels <- grep("panel", g$layout$name)
top <- unique(g$layout$t[panels])


# X axis object
xax <- p6[9, ]

# Add it to the first row
first_row <- gtable:::rbind_gtable(g[seq.int(min(top)), ],
                                   xax, "first"
)

# Add it to row starting from Jamaica
jam_row <- gtable:::rbind_gtable(g[27:29, ],
                                 xax, "first"
)

# Merge it with the rest of the data between 1st row 
# and row starting from Jamaica
first_to_jam <-
  gtable:::rbind_gtable(first_row, g[(min(top) + 1):26,], "first")

# Merge row starting from Jamaica with the rest of the rows
jam_to_end <-
  gtable:::rbind_gtable(jam_row, g[32:(max(top) + 3),], "first")


# Collect everything in one plot with caption and y axis title ------------


all <- arrangeGrob(
  
  # Combine y axis title and the rest of the plot
  arrangeGrob(textGrob("Annual\nGDP growth rate\nand CPI, %",
                       gp = gpar(fontsize = 12, family = "Inter UI", col = "#3B3B3B"),
                       x = unit(1, "npc"), y = unit(0.93, "npc"), just = c("left", "top")),
              gtable:::rbind_gtable(first_to_jam, jam_to_end, "first"),
              ncol = 2,
              widths = c(0.01, 0.99)),
  
  # Add annotation
  textGrob("Data: Worldbank::FP.CPI.TOTL.ZG & Worldbank::NY.GDP.MKTP.KD.ZG Indicators.",
           gp = gpar(fontsize = 10, family = "Inter UI", col = "#3B3B3B"),
           x = unit(0.18, "npc"), just = c("left", "bottom")),
  nrow = 2,
  heights = c(0.99, 0.01)
)


# Save result -------------------------------------------------------------


ggsave("wb_gdp_cpi.svg", all, width = 12, height = 18, scale = 0.8)


ggsave("wb_gdp_cpi.png", all, width = 12, height = 18, scale = 0.8,
       dpi = 600, type = "cairo-png")
