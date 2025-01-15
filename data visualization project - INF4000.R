#INSTALLING PACKAGES
install.packages("tidyr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("grid")
install.packages("reshape2")
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plotly)
library(grid)
library(reshape2)

# Set the path as the project folder path

#READING CSV DATA OF STOCKS

AAL_data <- read.csv("Datasets/AAL.csv", sep = ",", stringsAsFactors = FALSE)
AAPL_data <- read.csv("Datasets/AAPL.csv", sep = ",", stringsAsFactors = FALSE)
IBM_data <- read.csv("Datasets/IBM.csv", sep = ",", stringsAsFactors = FALSE)
CAT_data <- read.csv("Datasets/CAT.csv", sep = ",", stringsAsFactors = FALSE)

#DATA PROCESSING

AAL_data$Date <- as.Date(AAL_data$Date, format = "%d-%m-%Y")
AAPL_data$Date <- as.Date(AAPL_data$Date, format = "%d-%m-%Y")
IBM_data$Date <- as.Date(IBM_data$Date, format = "%d-%m-%Y" )
CAT_data$Date <- as.Date(CAT_data$Date, format = "%d-%m-%Y")
AAL_data$Company <- "American Airlines Group"
AAL_data$Index <- "NASDAQ"
AAPL_data$Company <- "Apple"
AAPL_data$Index <- "NASDAQ"
IBM_data$Company <- "IBM"
IBM_data$Index <- "NYSE"
CAT_data$Company <- "Caterpillar"
CAT_data$Index <- "NYSE"

#MERGING STOCK DATA

merged_stock_data <- rbind(AAL_data, AAPL_data, IBM_data, CAT_data)
merged_stock_data <- merged_stock_data[order(merged_stock_data$Date), ]
head(merged_stock_data)
View(merged_stock_data)
 
#FILTERING MERGED DATA - 4 STOCK CONSISTENCT FOR DATE

start_date <- as.Date("2006-01-01")
end_date <- as.Date("2020-12-31")
final_merged_data <- merged_stock_data[merged_stock_data$Date >= start_date & merged_stock_data$Date <= end_date, ]
View(final_merged_data)

#NORMALIZE STOCK PRICES FOR BETTER VISUALIZATION

final_merged_data <- final_merged_data %>%
  group_by(Company) %>%
  mutate(Normalized_Price = (Adjusted.Close / first(Adjusted.Close)) * 100) %>%
  ungroup()
View(final_merged_data)

#LINE CHART - COMPARISON OF NORMALIZED STOCK PRICES

ggplot(final_merged_data, aes(x = Date, y = Normalized_Price, color = Company)) +
  geom_line(size = 0.8) +                                            # Adjusted line thickness
  labs(
    title = "Long-Term Comparative Performance of Major Industry Stocks (2006–2020)",
    subtitle = "Normalized Stock Prices of Apple, IBM, Caterpillar, and American Airlines with Logarithmic Scaling, Highlighting Key Economic Events",
    x = " ",
    y = "Normalized Price Index (Base = 100)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",                                     # Legend positioned at bottom
    legend.box.margin = margin(-10, 0, 0, 0),                      # Fine-tune legend placement
    legend.title = element_blank(),                                # Remove legend title
    legend.text = element_text(size = 12),                         # Increased legend font size
    axis.text.x = element_text(size = 11),                         # Slightly increased x-axis label size
    panel.background = element_blank(),                            # Light gray background
    plot.background = element_blank(),                             # White plot area
    panel.grid.major = element_blank(),                            # Subtle major gridlines
    panel.grid.minor = element_blank()                             # Very subtle minor gridlines
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_log10(breaks = c(10, 50, 100, 500, 1000, 5000, 10000), limits = c(10, 15000)) +
  scale_color_manual(values = c(
    "Apple" = "blue", 
    "IBM" = "red", 
    "Caterpillar" = "green", 
    "American Airlines Group" = "purple"
  )) +
  # Long thin vertical lines extending above all data lines
  geom_segment(aes(x = as.Date("2008-09-01"), xend = as.Date("2008-09-01"), 
                   y = 10, yend = 14000), size = 0.2, color = "black") +
  geom_segment(aes(x = as.Date("2020-03-01"), xend = as.Date("2020-03-01"), 
                   y = 10, yend = 14000), size = 0.2, color = "black") +
  # Annotation text clearly above the lines
  annotate("text", x = as.Date("2008-09-01"), y = 14500, 
           label = "2008 Financial Crisis", color = "black", fontface = "bold", size = 4, hjust = 0.5) +
  annotate("text", x = as.Date("2020-03-01"), y = 14500, 
           label = "COVID-19 Pandemic", color = "black", fontface = "bold", size = 4, hjust = 0.5)

#CANDLESTICK CHART INDUSTRIAL COMPARISON DURING FINANCIAL CRISIS

#Ibm and Apple data in crisis
ibm_aapl_data <- final_merged_data %>%
              filter(Company %in% c("Apple", "IBM")&
                       Date >= as.Date("2008-09-01")&
                       Date <= as.Date("2008-12-31"))


#normalized open, close, high, low prices - Apple and IBM
ibm_aapl_data <- ibm_aapl_data %>%
  group_by(Company) %>%
  mutate(Normalized_Open = (Open / first(Open)) * 100,
         Normalized_High = (High / first(High)) * 100,
         Normalized_Low = (Low / first(Low)) * 100,
         Normalized_Close = (Close / first(Close)) * 100) %>%
  ungroup()

#actual candlestick chart - APPLE AND IBM (TECH COMPANIES)

APPLE_IBM_CANDLESSTICK <- plot_ly() %>%
  add_trace(
    data = ibm_aapl_data %>% filter(Company == "Apple"),
    type = "candlestick",
    x = ~Date, 
    open = ~Normalized_Open, 
    high = ~Normalized_High, 
    low = ~Normalized_Low, 
    close = ~Normalized_Close,
    name = "Apple",
    increasing = list(line = list(color = "black")),  
    decreasing = list(line = list(color = "black"))   
  ) %>%
  add_trace(
    data = ibm_aapl_data %>% filter(Company == "IBM"),
    type = "candlestick",
    x = ~Date, 
    open = ~Normalized_Open, 
    high = ~Normalized_High, 
    low = ~Normalized_Low, 
    close = ~Normalized_Close,
    name = "IBM",
    increasing = list(line = list(color = "blue")),  
    decreasing = list(line = list(color = "blue"))   
  ) %>%
  layout(
    title = list(
      text = "<b>Volatility of Apple and IBM During 2008 Financial Crisis</b><br><span style='font-size:14px; font-weight:normal;'>Daily stock price fluctuations of Apple and IBM, normalized to 100, during the 2008 financial crisis (September–December).</span>",
      x = 0.01,  # Align to the left
      y = 0.95,  # Vertical placement of the title
      font = list(size = 18, family = "Arial")  # Title font size and family
    ),
    xaxis = list(
      title = " ",
      rangeslider = list(visible = FALSE),
      tickformat = "%b %d",
      showgrid = FALSE
    ),
    yaxis = list(
      title = "Normalized Price Index (Base = 100)",
      showgrid = FALSE
    ),
    legend = list(
      orientation = "h",               # Horizontal legend
      x = 0.5,                         # Center-align the legend
      y = -0.1,                        # Place legend closer to the x-axis
      xanchor = "center",              # Anchor legend to the center
      font = list(size = 12)           # Increase legend font size
    ),
    plot_bgcolor = "white",            # Set plot background to white
    paper_bgcolor = "white",           # Set paper background to white
    margin = list(t = 80, b = 50)      # Adjusted top margin for better alignment
  ) 

APPLE_IBM_CANDLESSTICK

#Caterpillar and American Airlines data for candlestick
cat_aal_data <- final_merged_data %>%
    filter(Company %in% c("Caterpillar", "American Airlines Group")&
           Date >= as.Date("2008-09-01")&
           Date <= as.Date("2008-12-31"))
View(cat_aal_data)

#normalized open, close, high, low prices - Caterpillar and AAL
cat_aal_data <- cat_aal_data %>%
      group_by(Company) %>%
  mutate(Normalized_Open = (Open/ first(Open)) *100,
         Normalized_Close = (Close/ first(Close)) *100,
         Normalized_High = (High/ first(High)) * 100,
         Normalized_Low = (Low/ first(Low))* 100) %>%
      ungroup()
  
# Actual candlestick chart for Caterpillar and AAL

Cat_AAL_Candlestick <- plot_ly() %>%
  add_trace(
    data = cat_aal_data %>% filter(Company == "Caterpillar"),
    type = "candlestick",
    x = ~Date, 
    open = ~Normalized_Open, 
    high = ~Normalized_High, 
    low = ~Normalized_Low, 
    close = ~Normalized_Close,
    name = "Caterpillar",
    increasing = list(line = list(color = "gold")),  
    decreasing = list(line = list(color = "gold"))   
  ) %>%
  add_trace(
    data = cat_aal_data %>% filter(Company == "American Airlines Group"),
    type = "candlestick",
    x = ~Date, 
    open = ~Normalized_Open, 
    high = ~Normalized_High, 
    low = ~Normalized_Low, 
    close = ~Normalized_Close,
    name = "American Airlines Group",
    increasing = list(line = list(color = "red")),  
    decreasing = list(line = list(color = "red"))   
  ) %>%
  layout(
    title = list(
      text = "<b>Volatility of Caterpillar and AAL During 2008 Financial Crisis</b><br><span style='font-size:14px; font-weight:normal;'>Daily stock price fluctuations of Caterpillar and American Airlines, normalized to 100, during the 2008 financial crisis (September–December).</span>",
      x = 0.01,  # Align to the left
      y = 0.95,  # Vertical placement of the title
      font = list(size = 18, family = "Arial")  # Title font size and family
    ),
    xaxis = list(
      title = " ",
      rangeslider = list(visible = FALSE),
      tickformat = "%b %d",
      showgrid = FALSE
    ),
    yaxis = list(
      title = "Normalized Price Index (Base = 100)",
      showgrid = FALSE
    ),
    legend = list(
      orientation = "h",               # Horizontal legend
      x = 0.5,                         # Center-align the legend
      y = -0.1,                        # Place legend closer to the x-axis
      xanchor = "center",              # Anchor legend to the center
      font = list(size = 12)           # Increase legend font size
    ),
    plot_bgcolor = "white",            # Set plot background to white
    paper_bgcolor = "white",           # Set paper background to white
    margin = list(t = 80, b = 50)      # Adjusted top margin for better alignment
  )

Cat_AAL_Candlestick



#CORRELATION HEATMAP
correlation_data <- final_merged_data %>%
  select(Company, Date, Normalized_Price) %>% # Select relevant columns
  spread(key = Company, value = Normalized_Price)

correlation_matrix <- cor(correlation_data %>% select(-Date), use = "complete.obs")

correlation_long <- melt(correlation_matrix)

ggplot(correlation_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = " ",
                       guide = guide_colorbar(barheight = 15)) +
  theme_minimal() +
  labs(title = "Correlation Between Normalized Stock Prices(2006–2020)",
       subtitle = "Exploring the co-movement of normalized stock prices among selected companies during 15 years to identify trends and relationships",
       x = " ", y = " ") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12))


#TIME SERIES ARE CHART - COMPARING RETURNS
cumulative_return <- final_merged_data %>% 
  group_by(Company) %>%
  arrange(Date) %>%
  mutate(Cumulative_Return = Normalized_Price / first(Normalized_Price)) %>%
  ungroup()
View(cumulative_return)

#actual chart

ggplot(cumulative_return, aes(x = Date, y = Cumulative_Return, fill = Company)) +
  geom_area(alpha = 0.7) +
  labs(
    title = "Exploring Stock Market Growth Trends (2006–2020)",
    subtitle = "Comparative cumulative returns of major companies across 15 years",
    x = " ",
    y = "Cumulative Return (Starting at 1)"
  ) +
  scale_fill_manual(values = c("Apple" = "blue", "IBM" = "red", 
                               "Caterpillar" = "green", "American Airlines Group" = "orange")) +
  scale_x_date(
    date_breaks = "2 years",  # Set year intervals (every 2 years)
    date_labels = "%Y",       # Format as 'YYYY'
    limits = as.Date(c("2005-01-01", "20219-12-31"))  # Include 2005
  ) +
  theme_minimal() +  # Start with minimal theme
  theme(
    panel.background = element_blank(),     # Remove background
    plot.background = element_blank(),      # Remove outer plot background
    panel.grid.major = element_blank(),     # Remove major gridlines
    panel.grid.minor = element_blank(),     # Remove minor gridlines
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),  # Smaller x-axis text
    axis.text.y = element_text(size = 10),                         # Smaller y-axis values
    axis.title.y = element_text(size = 13, margin = margin(r = 15)),         # Larger y-axis title
    legend.title = element_blank(),                                # Remove legend title
    legend.text = element_text(size = 14),                         # Increase legend text size
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 14)              # Centered subtitle
  )






