library(readxl)
data <- read_excel(file.choose())

library(dplyr)
library(ggplot2)

ls()
clean_data <- data %>%
  filter(
    !is.na(shares),
    !is.na(num_imgs),
    !is.na(num_videos),
    !is.na(num_hrefs),
    shares >= 0,
    num_imgs >= 0,
    num_videos >= 0,
    num_hrefs >= 0
  )

tech_clean <- clean_data %>%
  filter(data_channel_is_tech == 1)

tech_clean <- clean_data %>%
  filter(data_channel_is_tech == 1)

nrow(tech_clean)

# Prepare technology articles for regression
reg_data <- tech_clean %>%
  select(
    shares,
    num_imgs,
    num_videos,
    num_hrefs,
    n_tokens_content,
    global_sentiment_polarity
  ) %>%
  filter(
    complete.cases(.),
    shares >= 0
  ) %>%
  mutate(
    log_shares = log1p(shares)
  )

# Check sample size
nrow(reg_data)

# Check variables
summary(reg_data)

# Model 1: Basic regression
model1 <- lm(
  log_shares ~ num_imgs + num_videos + num_hrefs,
  data = reg_data
)

summary(model1)

# Model 2: Add content and sentiment variables
model2 <- lm(
  log_shares ~
    num_imgs +
    num_videos +
    num_hrefs +
    n_tokens_content +
    global_sentiment_polarity,
  data = reg_data
)

summary(model2)

# Compare two models
data.frame(
  Model = c("Basic Model", "Extended Model"),
  Adjusted_R2 = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared
  ),
  AIC = c(
    AIC(model1),
    AIC(model2)
  )
)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

#add variable
reg_data3 <- tech_clean %>%
  mutate(
    weekday = case_when(
      weekday_is_monday == 1 ~ "Monday",
      weekday_is_tuesday == 1 ~ "Tuesday",
      weekday_is_wednesday == 1 ~ "Wednesday",
      weekday_is_thursday == 1 ~ "Thursday",
      weekday_is_friday == 1 ~ "Friday",
      weekday_is_saturday == 1 ~ "Saturday",
      weekday_is_sunday == 1 ~ "Sunday",
      TRUE ~ NA_character_
    ),
    weekday = factor(
      weekday,
      levels = c(
        "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
      )
    )
  ) %>%
  filter(
    complete.cases(
      shares, num_imgs, num_videos,
      num_hrefs, n_tokens_content,
      global_sentiment_polarity, weekday
    ),
    shares >= 0
  )

nrow(reg_data3)

# Model 3: Original shares with weekday
model3 <- lm(
  shares ~
    num_imgs +
    num_videos +
    num_hrefs +
    n_tokens_content +
    global_sentiment_polarity +
    weekday,
  data = reg_data3
)

summary(model3)


# Model without weekday, using original shares
model3_no_weekday <- lm(
  shares ~ num_imgs + num_videos +
    num_hrefs + n_tokens_content +
    global_sentiment_polarity,
  data = reg_data3
)

# Test whether weekday improves the model
anova(model3_no_weekday, model3)

model3_log <- lm(
  log1p(shares) ~ num_imgs + num_videos +
    num_hrefs + n_tokens_content +
    global_sentiment_polarity + weekday,
  data = reg_data3
)

model_log_no_weekday <- lm(
  log1p(shares) ~ num_imgs + num_videos +
    num_hrefs + n_tokens_content +
    global_sentiment_polarity,
  data = reg_data3
)

anova(model_log_no_weekday, model3_log)

# View coefficients and model performance
summary(model3_log)

# Check model assumptions
par(mfrow = c(2, 2))
plot(model3_log)
par(mfrow = c(1, 1))