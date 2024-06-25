library(dplyr)
library(plotly)
library(ggplot2)
library(cowplot)
library(corrplot)
library(gridExtra)
library(tidyr)
library(car)

data15 <- read.csv("D:/University/Semester 9/Data Science/Final-Project (World happyness report)/2015.csv", header=TRUE, sep=",")
data16 <- read.csv("D:/University/Semester 9/Data Science/Final-Project (World happyness report)/2016.csv", header=TRUE, sep=",")
data17 <- read.csv("D:/University/Semester 9/Data Science/Final-Project (World happyness report)/2017.csv", header=TRUE, sep=",")
data18 <- read.csv("D:/University/Semester 9/Data Science/Final-Project (World happyness report)/2018.csv", header=TRUE, sep=",")
data19 <- read.csv("D:/University/Semester 9/Data Science/Final-Project (World happyness report)/2019.csv", header=TRUE, sep=",")

str(data15)

num_rows <- nrow(data15)
num_cols <- ncol(data15)
cat("The dataset has", num_rows, "rows and", num_cols, "columns.\n")

num_rows <- nrow(data19)
num_cols <- ncol(data19)
cat("The dataset has", num_rows, "rows and", num_cols, "columns.\n")

datasets <- list(data15 = data15, data16 = data16, data17 = data17, data18 = data18, data19 = data19)

for (i in names(datasets)) {
  if (any(is.na(datasets[[i]]))) {
    cat("The", i, "dataset has null entries.\n")
  }
  else{
    print("no null values found")
  }
}


collection <- list(data15 = data15, data16 = data16, data17 = data17, data18 = data18, data19 = data19)

for (i in names(collection)) {
  if (anyDuplicated(collection[[i]]) > 0) {
    cat("The", i, "dataset has duplicated entries.\n")
  }
  
  else{
    print("no duplicate value found")
  }
}

data_scores <- data.frame()

data_scores <- data15 %>%
  select(country = Country, region = Region, `2015_rank` = `Happiness.Rank`, `2015_score` = `Happiness.Score`) %>%
  mutate_all(as.character)

d16 <- data16 %>%
  select(country = Country, `2016_rank` = `Happiness.Rank`, `2016_score` = `Happiness.Score`) %>%
  mutate_all(as.character)

d17 <- data17 %>%
  select(country = Country, `2017_rank` = `Happiness.Rank`, `2017_score` = `Happiness.Score`) %>%
  mutate_all(as.character)

d18 <- data18 %>%
  select(country = `Country.or.region`, `2018_rank` = `Overall.rank`, `2018_score` = `Score`) %>%
  mutate_all(as.character)

d19 <- data19 %>%
  select(country = `Country.or.region`, `2019_rank` = `Overall.rank`, `2019_score` = `Score`) %>%
  mutate_all(as.character)

data_scores <- data_scores %>%
  left_join(d16, by = "country") %>%
  left_join(d17, by = "country") %>%
  left_join(d18, by = "country") %>%
  left_join(d19, by = "country")

head(data_scores, 10)

data <- list(
  type = "choropleth",
  locations = data_scores$country,
  locationmode = "country names",
  autocolorscale = FALSE,
  colorscale = "Blues",
  text = data_scores$country,
  z = data_scores$`2015_score`,
  colorbar = list(title = "Happiness Score", len = 0.75, lenmode = "fraction")
)

happiness_map <- plot_ly(z = ~data$z, text = ~data$text, locations = ~data$locations, type = "choropleth", locationmode = "country names") %>%
  layout(title = "Happiness Score by Country (Year 2015)", geo = list(scope = "world"))

print(happiness_map)

data_scores_europe <- data_scores[data_scores$region %in% c('Western Europe', 'Central and Eastern Europe'), ]

data_europe <- list(
  type = "choropleth",
  locations = data_scores_europe$country,
  locationmode = "country names",
  autocolorscale = FALSE,
  colorscale = "Blues",
  text = data_scores_europe$country,
  z = data_scores_europe$`2015_score`,
  colorbar = list(title = "Happiness Score", len = 0.75, lenmode = "fraction")
)

layout_europe <- list(
  geo = list(scope = "europe")
)

happiness_europe <- plot_ly(z = ~data_europe$z, text = ~data_europe$text, locations = ~data_europe$locations, type = "choropleth", locationmode = "country names") %>%
  layout(title = "Happiness Score in Europe (Year 2015)", geo = list(scope = "europe"))

print(happiness_europe)

plot1 <- ggplot(data15, aes(x = `Economy..GDP.per.Capita.`, y = `Happiness.Score`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Economy (GDP per Capita)",
       x = "Economy (GDP per Capita)",
       y = "Happiness Score") +
  theme_minimal()

plot2 <- ggplot(data15, aes(x = Family, y = `Happiness.Score`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Family",
       x = "Family",
       y = "Happiness Score") +
  theme_minimal()

plot3 <- ggplot(data15, aes(x = `Health..Life.Expectancy.`, y = `Happiness.Score`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Health (Life Expectancy)",
       x = "Health (Life Expectancy)",
       y = "Happiness Score") +
  theme_minimal()

plot4 <- ggplot(data15, aes(x = Freedom, y = `Happiness.Score`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Freedom",
       x = "Freedom",
       y = "Happiness Score") +
  theme_minimal()

plot5 <- ggplot(data15, aes(x = `Trust..Government.Corruption.`, y = `Happiness.Score`)) +
  geom_point() +
  labs(title = "Happiness Score vs Trust (Government Corruption)",
       x = "Trust (Government Corruption)",
       y = "Happiness Score") +
  theme_minimal()

plot6 <- ggplot(data15, aes(x = Generosity, y = `Happiness.Score`)) +
  geom_point() +
  labs(title = "Happiness Score vs Generosity",
       x = "Generosity",
       y = "Happiness Score") +
  theme_minimal()

plots <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)

print(plots)

d15 <- data15[, !names(data15) %in% c('Country', 'Region', 'Happiness Rank', 'Standard Error', 'Dystopia Residual')]

correlation_matrix <- cor(d15, method = "pearson")

corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.cex = 0.7,  
         addCoef.col = "black", number.cex = 0.7, 
         col = colorRampPalette(c("blue", "gray", "red"))(100))
title("Correlation heatmap", cex.main = 2)

top_5_countries <- head(data15, 5)

bar_plot <- ggplot(top_5_countries, aes(x = reorder(Country, -Happiness.Score), y = Happiness.Score)) +
  geom_bar(stat = "identity", fill = "navy") +
  labs(title = "Top 5 Countries by Happiness Score in 2015",
       x = "Country",
       y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

print(bar_plot)

top_5_countries <- head(data19, 5)

bar_plot <- ggplot(top_5_countries, aes(x = reorder(Country.or.region, -Overall.rank), y = Overall.rank)) +
  geom_bar(stat = "identity", fill = "navy") +
  labs(title = "Top 5 Countries by Happiness Score in 2019",
       x = "Country",
       y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

print(bar_plot)

numeric_cols <- c("2015_score", "2016_score", "2017_score", "2018_score", "2019_score")
data_scores[numeric_cols] <- lapply(data_scores[numeric_cols], as.numeric)

region_score15 <- data_scores %>%
  filter(!is.na(`2015_score`)) %>%  
  group_by(region) %>%
  summarise(mean2015 = mean(`2015_score`)) %>%
  arrange(desc(mean2015))

region_score16 <- data_scores %>%
  filter(!is.na(`2016_score`)) %>%  
  group_by(region) %>%
  summarise(mean2016 = mean(`2016_score`)) %>%
  arrange(desc(mean2016))

region_score17 <- data_scores %>%
  filter(!is.na(`2017_score`)) %>%  
  group_by(region) %>%
  summarise(mean2017 = mean(`2017_score`)) %>%
  arrange(desc(mean2017))

region_score18 <- data_scores %>%
  filter(!is.na(`2018_score`)) %>%  
  group_by(region) %>%
  summarise(mean2018 = mean(`2018_score`)) %>%
  arrange(desc(mean2018))

region_score19 <- data_scores %>%
  filter(!is.na(`2019_score`)) %>%  
  group_by(region) %>%
  summarise(mean2019 = mean(`2019_score`)) %>%
  arrange(desc(mean2019))

region_score <- region_score15 %>%
  inner_join(region_score16, by = "region") %>%
  inner_join(region_score17, by = "region") %>%
  inner_join(region_score18, by = "region") %>%
  inner_join(region_score19, by = "region")

region_score

x <- c(2015, 2016, 2017, 2018, 2019)
y1 <- c(7.285000, 7.323500, 7.299000, 7.298000, 7.267500)
y2 <- c(7.273000, 7.254000, 7.154500, 7.107000, 7.085000)
y3 <- c(6.739350, 6.731400, 6.748400, 6.829100, 6.898400)
y4 <- c(6.137300, 6.050500, 5.947400, 5.937750, 5.942550)
y5 <- c(5.496250, 5.477000, 5.496500, 5.540250, 5.564250)
y6 <- c(5.372625, 5.396750, 5.444875, 5.399750, 5.333375)
y7 <- c(5.344571, 5.379607, 5.418321, 5.473929, 5.571786)
y8 <- c(5.330789, 5.386053, 5.369684, 5.282737, 5.237000)
y9 <- c(4.580857, 4.563286, 4.628429, 4.603857, 4.526857)
y10 <- c(4.120419, 4.074839, 4.131129, 4.247968, 4.380323)
regions <- c("Australia and New Zealand", "North America", "Western Europe", "Latin America and Caribbean", "Eastern Asia",
             "Southeastern Asia", "Central and Eastern Europe", "Middle East and Northern Africa", "Southern Asia", "Sub-Saharan Africa")

data <- data.frame(
  Year = rep(x, each = length(regions)),
  Score = c(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10),
  Region = rep(regions, times = length(x))
)

ggplot(data, aes(x = Year, y = Score, group = Region, color = Region)) +
  geom_line() +
  geom_point() +
  labs(title = "Happiness Scores in 2015-2019",
       x = "Year",
       y = "Happiness Score") +
  theme_minimal() +
  theme(legend.position = "top", legend.text = element_text(size = 8)) +
  scale_x_continuous(breaks = x, labels = x) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5))

economy_scores <- data.frame()
e16 <- data.frame()
e17 <- data.frame()
e18 <- data.frame()
e19 <- data.frame()

economy_scores <- data15 %>%
  select(Country, Region, `Economy..GDP.per.Capita.`, `Happiness.Score`) %>%
  rename(country = Country, 
         `2015_GDP_score` = `Economy..GDP.per.Capita.`, 
         `2015_score` = `Happiness.Score`)

e16 <- data16 %>%
  select(Country, `Economy..GDP.per.Capita.`, `Happiness.Score`) %>%
  rename(country = Country, 
         `2016_GDP_score` = `Economy..GDP.per.Capita.`, 
         `2016_score` = `Happiness.Score`)

e17 <- data17 %>%
  select(Country, `Economy..GDP.per.Capita.`, `Happiness.Score`) %>%
  rename(country = Country, 
         `2017_GDP_score` = `Economy..GDP.per.Capita.`, 
         `2017_score` = `Happiness.Score`)

e18 <- data18 %>%
  select(`Country.or.region`, `GDP.per.capita`, Score) %>%
  rename(country = `Country.or.region`, 
         `2018_GDP_score` = `GDP.per.capita`, 
         `2018_score` = Score)

e19 <- data19 %>%
  select(`Country.or.region`, `GDP.per.capita`, Score) %>%
  rename(country = `Country.or.region`, 
         `2019_GDP_score` = `GDP.per.capita`, 
         `2019_score` = Score)

economy_scores <- inner_join(economy_scores, e16, by = "country") %>%
  inner_join(e17, by = "country") %>%
  inner_join(e18, by = "country") %>%
  inner_join(e19, by = "country")

head(economy_scores)

economy_region_score <- economy_scores %>%
  group_by(Region) %>%
  summarise(GDP2015 = mean(`2015_GDP_score`)) %>%
  arrange(desc(GDP2015))

rscore16 <- economy_scores %>%
  group_by(Region) %>%
  summarise(GDP2016 = mean(`2016_GDP_score`)) %>%
  arrange(desc(GDP2016))

rscore17 <- economy_scores %>%
  group_by(Region) %>%
  summarise(GDP2017 = mean(`2017_GDP_score`)) %>%
  arrange(desc(GDP2017))

rscore18 <- economy_scores %>%
  group_by(Region) %>%
  summarise(GDP2018 = mean(`2018_GDP_score`)) %>%
  arrange(desc(GDP2018))

rscore19 <- economy_scores %>%
  group_by(Region) %>%
  summarise(GDP2019 = mean(`2019_GDP_score`)) %>%
  arrange(desc(GDP2019))

economy_region_score <- economy_region_score %>%
  inner_join(rscore16, by = "Region") %>%
  inner_join(rscore17, by = "Region") %>%
  inner_join(rscore18, by = "Region") %>%
  inner_join(rscore19, by = "Region")

economy_region_score$`diff GDP 2015-2019` <- economy_region_score$GDP2019 - economy_region_score$GDP2015

economy_region_score

pie_chart <- ggplot(economy_region_score, aes(x = "", y = GDP2019, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "2019 Economy Score",
       y = "Economy Score",
       fill = "Region") +
  theme_void() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, size = 20))

print(pie_chart)

boxplot1 <- ggplot(data15, aes(x = factor(1), y = `Happiness.Score`, fill = "Happiness Score")) +
  geom_boxplot(color = "blue", alpha = 0.7) +
  labs(title = "Happiness Score", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

boxplot2 <- ggplot(data15, aes(x = factor(1), y = `Economy..GDP.per.Capita.`, fill = "Economy (GDP per Capita)")) +
  geom_boxplot(color = "green", alpha = 0.7) +
  labs(title = "Economy (GDP per Capita)", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

boxplot3 <- ggplot(data15, aes(x = factor(1), y = Family, fill = "Family")) +
  geom_boxplot(color = "gold", alpha = 0.7) +
  labs(title = "Family (Social Support)", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

boxplot4 <- ggplot(data15, aes(x = factor(1), y = `Health..Life.Expectancy.`, fill = "Health (Life Expectancy)")) +
  geom_boxplot(color = "red", alpha = 0.7) +
  labs(title = "Health (Life Expectancy)", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

top_row <- grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4, ncol = 4)

boxplot5 <- ggplot(data15, aes(x = factor(1), y = Freedom, fill = "Freedom")) +
  geom_boxplot(color = "grey", alpha = 0.7) +
  labs(title = "Freedom", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

boxplot6 <- ggplot(data15, aes(x = factor(1), y = `Trust..Government.Corruption.`, fill = "Trust (Government Corruption)")) +
  geom_boxplot(color = "purple", alpha = 0.7) +
  labs(title = "Trust (Government Corruption)", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

boxplot7 <- ggplot(data15, aes(x = factor(1), y = Generosity, fill = "Generosity")) +
  geom_boxplot(color = "orange", alpha = 0.7) +
  labs(title = "Generosity", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

boxplot8 <- ggplot(data15, aes(x = factor(1), y = `Dystopia.Residual`, fill = "Dystopia Residual")) +
  geom_boxplot(color = "cyan", alpha = 0.7) +
  labs(title = "Dystopia Residual", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank())

bottom_row <- grid.arrange(boxplot5, boxplot6, boxplot7, boxplot8, ncol = 4)
boxplots_combined <- grid.arrange(top_row, bottom_row, nrow = 2)
print(boxplots_combined)

mean15 <- mean(data15$`Happiness.Score`)
mean19 <- mean(data19$Score)
delta_mean <- mean19 - mean15
cat(sprintf("mean 2015 = %.3f; mean 2019 = %.3f; mean 2019 - mean 2015 = %.3f", mean15, mean19, delta_mean))

####################################AJKER CODE START FROM HERE##########################################

h16 <- data16 %>%
  select(Country, `Health..Life.Expectancy.`, `Happiness.Score`) %>%
  rename(country = Country, `2016_health_score` = `Health..Life.Expectancy.`, `2016_score` = `Happiness.Score`)

h17 <- data17 %>%
  select(Country, `Health..Life.Expectancy.`, `Happiness.Score`) %>%
  rename(country = Country, `2017_health_score` = `Health..Life.Expectancy.`, `2017_score` = `Happiness.Score`)

h18 <- data18 %>%
  select(`Country.or.region`, `Healthy.life.expectancy`, Score) %>%
  rename(country = `Country.or.region`, `2018_health_score` = `Healthy.life.expectancy`, `2018_score` = Score)

h19 <- data19 %>%
  select(`Country.or.region`, `Healthy.life.expectancy`, Score) %>%
  rename(country = `Country.or.region`, `2019_health_score` = `Healthy.life.expectancy`, `2019_score` = Score)


health_scores <- data15 %>%
  select(Country, Region, `Health..Life.Expectancy.`, `Happiness.Score`) %>%
  rename(country = Country, 
         `2015_health_score` = `Health..Life.Expectancy.`, 
         `2015_score` = `Happiness.Score`) %>%
  inner_join(h16, by = "country") %>%
  inner_join(h17, by = "country") %>%
  inner_join(h18, by = "country") %>%
  inner_join(h19, by = "country")

head(health_scores, 5)

health_scores_2015 <- health_scores[, c("2015_health_score", "2015_score")]
health_scores_2016 <- health_scores[, c("2016_health_score", "2016_score")]
health_scores_2017 <- health_scores[, c("2017_health_score", "2017_score")]
health_scores_2018 <- health_scores[, c("2018_health_score", "2018_score")]
health_scores_2019 <- health_scores[, c("2019_health_score", "2019_score")]

scatterplotMatrix(health_scores_2015, main = "Scatter Matrix (2015)")
scatterplotMatrix(health_scores_2016, main = "Scatter Matrix (2016)")
scatterplotMatrix(health_scores_2017, main = "Scatter Matrix (2017)")
scatterplotMatrix(health_scores_2018, main = "Scatter Matrix (2018)")
scatterplotMatrix(health_scores_2019, main = "Scatter Matrix (2019)")

health_scores <- data.frame(
  Year = c(rep(2015, nrow(health_scores)), rep(2016, nrow(health_scores)),
           rep(2017, nrow(health_scores)), rep(2018, nrow(health_scores)), rep(2019, nrow(health_scores))),
  Health_Score = c(health_scores$`2015_health_score`, health_scores$`2016_health_score`,
                   health_scores$`2017_health_score`, health_scores$`2018_health_score`, health_scores$`2019_health_score`)
)

ggplot(health_scores, aes(x = as.factor(Year), y = Health_Score, fill = as.factor(Year))) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.5) + 
  labs(title = "Distribution of Health (Life Expectancy) Scores Over Years",
       x = "Year",
       y = "Health Score") +
  scale_fill_discrete(name = "Year") +
  theme_minimal()