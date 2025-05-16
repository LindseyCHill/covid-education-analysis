# ------------------- Load Libraries -------------------
# Core packages for data wrangling, visualization, and mapping
library(tidyverse)
library(lubridate)
library(janitor)
library(ggrepel)
library(broom)
library(scales)
library(sf)
library(tigris)     # For shapefiles
library(usmap)      # For mapping US regions

# Cache TIGRIS shapefiles for faster access
options(tigris_use_cache = TRUE)

# ------------------- Import Raw Data -------------------
# Prompt user to select each file interactively
covid2020raw <- read.csv(file.choose(), header = TRUE)
covid2021raw <- read.csv(file.choose(), header = TRUE)
educationraw <- read.csv(file.choose(), header = TRUE)

# ------------------- Clean Education Data Set -------------------
education_cleaned <- educationraw %>%
  setNames(.[1, ]) %>%               # Use second row as column names
  slice(-1) %>%                      # Remove first row (original column names)
  rename_all(~ str_replace_all(., "!", "")) %>%  # Remove "!" characters
  select(!contains("margin of error", ignore.case = TRUE)) %>%
  select(!contains("18 to 24", ignore.case = TRUE)) %>%
  select(-"EstimateTotal:") %>%      # Drop total that includes under-25s
  mutate(across(starts_with("EstimateTotal:"), as.numeric)) %>%
  separate(`Geographic Area Name`, into = c("County", "State"), sep = ", ")

# Summarize relevant education demographics
education_cleaned <- education_cleaned %>%
  mutate(
    # Total adult population (25+)
    Population = rowSums(across(contains("25 to 34") | 
                                  contains("35 to 44") | 
                                  contains("45 to 64") | 
                                  contains("65 years and over") &
                                  contains("EstimateTotal:") & 
                                  (contains("Male") | contains("Female"))), na.rm = TRUE),
    
    # Less than high school education
    Population_Sub_HS = rowSums(across(contains("Less than 9th grade") |
                                         contains("9th to 12th grade, no diploma")), na.rm = TRUE),
    
    # Bachelor's degree or higher
    Population_Bachelor_or_Higher = rowSums(across(contains("Bachelor's degree") |
                                                     contains("Graduate or professional degree")), na.rm = TRUE)
  )

# Select relevant columns, extract FIPS, and compute percentages
education_cleaned <- education_cleaned %>%
  select(Geography, County, State, Population, Population_Sub_HS, Population_Bachelor_or_Higher) %>%
  mutate(
    FIPS = substr(Geography, nchar(Geography) - 4, nchar(Geography)),
    Percent_No_HS = round((Population_Sub_HS / Population) * 100, 2),
    Percent_Bachelor_or_Higher = round((Population_Bachelor_or_Higher / Population) * 100, 2)
  ) %>%
  rename(
    Total_Population = Population,
    Population_No_HS = Population_Sub_HS
  )

# ------------------- Clean and Combine COVID Data -------------------
# Combine and format COVID data
covid_cleaned <- bind_rows(covid2020raw, covid2021raw) %>%
  mutate(
    Date = as.Date(date, format = "%Y-%m-%d"),
    FIPS = sprintf("%05d", fips),
    County = county,
    State = state,
    Cumulative_Cases = cases,
    Cumulative_Deaths = deaths
  ) %>%
  select(FIPS, County, State, Date, Cumulative_Cases, Cumulative_Deaths)

# Calculate daily new case counts
covid_cleaned <- covid_cleaned %>%
  group_by(FIPS) %>%
  arrange(Date) %>%
  mutate(New_Cases = Cumulative_Cases - lag(Cumulative_Cases)) %>%
  ungroup()

# Clean up inconsistent FIPS values
covid_cleaned <- covid_cleaned %>%
  mutate(
    FIPS = str_trim(FIPS),
    FIPS = na_if(FIPS, ""),
    FIPS = na_if(FIPS, "NA"),
    FIPS = na_if(FIPS, "na"),
    FIPS = if_else(is.na(FIPS), NA_character_, str_pad(FIPS, width = 5, side = "left", pad = "0"))
  )

# ------------------- Aggregate Monthly Data -------------------
# Group by month and summarize COVID spread
covid_monthly <- covid_cleaned %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(FIPS, County, State, YearMonth) %>%
  summarise(
    Monthly_New_Cases = sum(New_Cases, na.rm = TRUE),
    Monthly_Cumulative_Cases = max(Cumulative_Cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(YearMonth = format(YearMonth, "%Y-%m"))
# ------------------- Merge Education Data -------------------
# Join education data by FIPS
final_joined_data <- covid_monthly %>%
  left_join(
    education_cleaned %>%
      select(
        FIPS,
        Total_Population,
        Population_No_HS,
        Percent_No_HS,
        Population_Bachelor_or_Higher,
        Percent_Bachelor_or_Higher
      ),
    by = "FIPS"
  )

# Drop counties named "Unknown" with missing FIPS
final_cleaned_data <- final_joined_data %>%
  filter(!(County == "Unknown" & is.na(FIPS)))

# Review any remaining NA FIPS cases
na_fips_data <- final_cleaned_data %>%
  filter(is.na(FIPS))

# ------------------- Check City/County Duplication -------------------
# Cities treated as counties in COVID data – assess for redundancy
county_list <- c(
  # NYC
  "Bronx", "Kings", "New York", "Queens", "Richmond",
  # Kansas City
  "Johnson", "Wyandotte", "Leavenworth", "Platte", "Clay", "Clinton",
  "Caldwell", "Ray", "Lafayette", "Jackson", "Cass", "Bates", "Linn", "Miami",
  # Joplin
  "Jasper", "Newton"
)

state_list <- c("New York", "Missouri")

# Filter possibly duplicated entries
final_city_errors_data <- final_cleaned_data %>%
  filter(County %in% county_list, State %in% state_list) %>%
  select(FIPS, County, State, Total_Population)

# Count records by county/state
county_state_counts <- final_city_errors_data %>%
  group_by(County, State) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(State, County)

# ------------------- Joplin Cleanup -------------------
# Compare county-level vs city-level data for Joplin
joplin_counties <- final_cleaned_data %>%
  filter(State == "Missouri", County %in% c("Jasper", "Newton")) %>%
  group_by(YearMonth) %>%
  summarise(
    Combined_New_Cases = sum(Monthly_New_Cases, na.rm = TRUE),
    Combined_Cumulative = sum(Monthly_Cumulative_Cases, na.rm = TRUE),
    .groups = "drop"
  )

joplin_city_data <- final_cleaned_data %>%
  filter(State == "Missouri", County == "Joplin") %>%
  select(YearMonth, Joplin_New_Cases = Monthly_New_Cases, Joplin_Cumulative = Monthly_Cumulative_Cases)

joplin_comparison <- left_join(joplin_counties, joplin_city_data, by = "YearMonth")

# Drop Joplin city data to avoid duplication
final_cleaned_data <- final_cleaned_data %>%
  filter(!(County == "Joplin" & State == "Missouri"))

# ------------------- New York City Fix -------------------
# NYC county-level education data exists; COVID data only has "New York City"
# Combine education data for NYC boroughs and apply it to "New York City"

education_cleaned_nyc <- education_cleaned %>%
  filter(State == "New York", County %in% c(
    "Bronx County", "Kings County", "New York County", 
    "Queens County", "Richmond County"
  )) %>%
  summarise(
    Total_Population = sum(Total_Population, na.rm = TRUE),
    Population_No_HS = sum(Population_No_HS, na.rm = TRUE),
    Population_Bachelor_or_Higher = sum(Population_Bachelor_or_Higher, na.rm = TRUE),
    Percent_No_HS = Population_No_HS / Total_Population * 100,
    Percent_Bachelor_or_Higher = Population_Bachelor_or_Higher / Total_Population * 100
  )

# Inject NYC education data into COVID data for "New York City"
final_cleaned_data <- final_cleaned_data %>%
  mutate(
    Total_Population = if_else(County == "New York City" & State == "New York",
                               education_cleaned_nyc$Total_Population,
                               Total_Population),
    Population_No_HS = if_else(County == "New York City" & State == "New York",
                               education_cleaned_nyc$Population_No_HS,
                               Population_No_HS),
    Population_Bachelor_or_Higher = if_else(County == "New York City" & State == "New York",
                                            education_cleaned_nyc$Population_Bachelor_or_Higher,
                                            Population_Bachelor_or_Higher),
    Percent_No_HS = if_else(County == "New York City" & State == "New York",
                            education_cleaned_nyc$Percent_No_HS,
                            Percent_No_HS),
    Percent_Bachelor_or_Higher = if_else(County == "New York City" & State == "New York",
                                         education_cleaned_nyc$Percent_Bachelor_or_Higher,
                                         Percent_Bachelor_or_Higher)
  )

# ------------------- Kansas City Cleanup -------------------
# Prefer county-level data over aggregate city-level
final_cleaned_data <- final_cleaned_data %>%
  filter(!(is.na(FIPS) & County == "Kansas City"))

# Confirm remaining NA FIPS
na_fips_data <- final_cleaned_data %>%
  filter(is.na(FIPS))

# ------------------- Drop Remaining Rows Missing Key Data -------------------
# Drop rows where any key variable (besides FIPS) is NA
final_cleaned_data <- final_cleaned_data %>%
  filter(if_all(-FIPS, ~ !is.na(.)))

# ------------------- Normalize to 100k Population -------------------
final_cleaned_data <- final_cleaned_data %>%
  mutate(
    Monthly_New_Cases_per_100k = (Monthly_New_Cases / Total_Population) * 100000,
    Monthly_Cumulative_Cases_per_100k = (Monthly_Cumulative_Cases / Total_Population) * 100000,
    Population_No_HS_per_100k = (Population_No_HS / Total_Population) * 100000,
    Population_Bachelor_or_Higher_per_100k = (Population_Bachelor_or_Higher / Total_Population) * 100000
  )

# ------- Outlier Detection: New York City -------

#At first, I looked at the counties with the highest and lowest levels of educational attainment 
#to see if any patterns stood out. But the more counties I included, the more irregular the results seemed. 
#It seemed like small outliers might be throwing things off. That made me wonder if any trends would hold 
#up across all the data or if comparing just the top and bottom groups was too unstable to be useful.

# Summarize average monthly cases and education metrics
education_summary <- final_cleaned_data %>%
  group_by(County, State) %>%
  summarise(
    Avg_Cases = mean(Monthly_New_Cases_per_100k, na.rm = TRUE),
    Percent_Bachelor_or_Higher = first(Percent_Bachelor_or_Higher),
    Percent_No_HS = first(Percent_No_HS),
    Total_Population = first(Total_Population)
  )

# Scatter plot with NYC included
ggplot(education_summary, aes(x = Percent_Bachelor_or_Higher, y = Avg_Cases)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Avg Covid-19 Cases vs % with Bachelor's (Including NYC)",
       x = "% with Bachelor's or Higher",
       y = "Avg Monthly New Cases per 100k")

# Label outlier (NYC)
ggplot(education_summary, aes(x = Percent_Bachelor_or_Higher, y = Avg_Cases)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text_repel(data = education_summary %>% filter(Avg_Cases == max(Avg_Cases)),
                  aes(label = paste(County, State, sep = ", ")),
                  color = "red") +
  theme_minimal() +
  labs(title = "Outlier Identification: NYC",
       x = "% with Bachelor's or Higher",
       y = "Avg Monthly New Cases per 100k")

# Remove NYC
education_summary_no_outlier <- education_summary %>%
  filter(!(County == "New York City" & State == "New York"))

final_cleaned_data <- final_cleaned_data %>%
  filter(!(County == "New York City" & State == "New York"))

# ------- Monthly Cases vs Education Level (No NYC) -------

# Bachelor's or higher
ggplot(education_summary_no_outlier, aes(x = Percent_Bachelor_or_Higher, y = Avg_Cases)) +
  geom_point(alpha=0.3, aes(size=Total_Population)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_size_continuous(name = "County Population", labels = comma) +
  labs(
    title = "County Avg Covid-19 Cases vs Higher Education % (2020-2021)",
    x = "% Population with Bachelor's or Higher",
    y = "Avg Monthly New Cases per 100k Population") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("output/county_avg_covid_vs_higher_edu.png", width = 10, height = 6, dpi = 300)

summary(lm(Avg_Cases ~ Percent_Bachelor_or_Higher, data = education_summary_no_outlier))

# No high school diploma
ggplot(education_summary_no_outlier, aes(x = Percent_No_HS, y = Avg_Cases)) +
  geom_point(alpha=0.3, aes(size=Total_Population)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_size_continuous(name = "County Population", labels = comma) +
  labs(
    title = "County Avg Covid-19 Cases vs % Without High School Diploma (2020-2021)",
    x = "% Population Without High School Diploma",
    y = "Avg Monthly New Cases per 100k Population") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("output/county_avg_covid_vs_no_hs.png", width = 10, height = 6, dpi = 300)

summary(lm(Avg_Cases ~ Percent_No_HS, data = education_summary_no_outlier))

# ------- Monthly Cases vs Population Size -------

ggplot(education_summary_no_outlier, 
       aes(x = Total_Population, y = Avg_Cases)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10(labels = comma) +  # Log scale + comma format
  theme_minimal(base_size = 16) +
  labs(
    title = "Avg Monthly Covid-19 Cases per 100k vs. County Population (2020-2021)",
    x = "County Population (Adults 25+)",
    y = "Avg Monthly New Cases per 100k") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("output/county_avg_covid_vs_county_pop.png", width = 10, height = 6, dpi = 300)

summary(lm(Avg_Cases ~ Total_Population, data = education_summary_no_outlier))

# ------- Education-Covid Correlation Index Map -------
# Get county geometries
counties_sf <- counties(cb = TRUE, resolution = "5m", class = "sf")

# Merge FIPS codes into education summary
merged_data_for_map <- education_summary_no_outlier %>%
  left_join(final_cleaned_data %>% select(FIPS, County, State) %>% distinct(),
            by = c("County", "State"))

# Merge with spatial data
map_data <- counties_sf %>%
  left_join(merged_data_for_map, by = c("GEOID" = "FIPS"))

# Compute state-level correlation index
cor_results <- education_summary_no_outlier %>%
  group_by(State) %>%
  summarize(
    cor_no_hs = cor(Avg_Cases, Percent_No_HS, use = "complete.obs"),
    cor_bachelors = cor(Avg_Cases, Percent_Bachelor_or_Higher, use = "complete.obs")
  ) %>%
  mutate(education_covid_index = cor_no_hs - cor_bachelors)

# Merge with state shapefiles
states_sf <- states(cb = TRUE, resolution = "20m", class = "sf")
map_data <- states_sf %>%
  left_join(cor_results, by = c("NAME" = "State"))

# Clean for usmap plotting
map_df <- map_data %>%
  st_drop_geometry() %>%
  mutate(state = STUSPS) %>%
  na.omit()

# Plot choropleth map
ggsave("output/edu_lvl_vs_avg_monthly_covid_map.png",
       plot = plot_usmap(data = map_df, values = "education_covid_index", color = "white") +
  scale_fill_gradient2(
    low = "#2133ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 0,
    name = "Education-Covid-19 Index",
    breaks = c(-1, 0, 1),
    limits = c(-1, 1),
    labels = c(
      "More Covid-19 in Highly Educated Areas",
      "No Clear Trend",
      "More Covid-19 in Less Educated Areas"),
    oob = scales::squish) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)) +
  labs(
    title = "Education Level vs Average Monthly Covid-19 Cases per 100k by State (2020–2021)",
    subtitle = "Positive/Red = More cases in low-education areas; Negative/Blue = More in highly educated areas",
    caption = "Index = Cor(% No HS, Average Cases) - Cor(% Bachelor's, Average Cases)"),
  width = 10, height = 6, dpi = 300)

