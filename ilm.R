# Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)


# ----------- SEADISTUSED -----------

lat <- 58.9978 
lon <- 22.7492

end_date <- Sys.Date()
start_date <- end_date - 30   # viimased 30 päeva

# ----------- API PÄRING (AJALUGU) -----------

url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=", lat,
  "&longitude=", lon,
  "&start_date=", start_date,
  "&end_date=", end_date,
  "&hourly=temperature_2m",
  "&timezone=auto"
)

response <- GET(url)
data <- fromJSON(content(response, "text", encoding = "UTF-8"))

temps <- data$hourly

df <- data.frame(
  datetime = as.POSIXct(temps$time, format = "%Y-%m-%dT%H:%M", tz = "Europe/Tallinn"),
  temp = temps$temperature_2m
)

df <- df %>%
  mutate(
    day = as.Date(datetime, tz = "Europe/Tallinn"),
    hour = format(datetime, "%H", tz = "Europe/Tallinn"),
    hour_num = as.integer(hour)
  )

# ----------- ÖÖKÜLMA KONTROLL -----------

night_data <- df %>%
  filter(hour_num >= 0 & hour_num <= 7)

# Eelmine öö 

tz <- "Europe/Tallinn"

today <- Sys.Date()

start_night <- as.POSIXct(paste0(today - 1, " 22:00:00"), tz = tz)
end_night   <- as.POSIXct(paste0(today,     " 08:00:00"), tz = tz)

last_night <- df %>%
  filter(datetime >= start_night &
           datetime <= end_night)

range(last_night$datetime)

min_temp <- min(last_night$temp, na.rm = TRUE)

avg_temp <- mean(last_night$temp, na.rm = T)

if (min_temp < 0) {
  frost_msg <- paste0("Eile oli ÖÖKÜLM! Min temp: ", round(min_temp,2), "°C", 
                      " (öö keskmine ", round(avg_temp,2), "°C")
} else {
  frost_msg <- paste0("Eile öösel öökülma ei olnud. Min temp: ", round(min_temp,2), "°C",
                      " (öö keskmine ", round(avg_temp,2), "°C)")
}

print(frost_msg)

# ----------- HEATMAP -----------
df <- df %>%
  mutate(day = factor(day, levels = rev(sort(unique(day))))) # Paneb viimase kuupäeva alla

df <- df %>%
  complete(day, hour, fill = list(temp = NA)) # Täidab "augud" maatriksis

p <- ggplot(df, aes(x = hour, y = day, fill = temp)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = paste("Temperatuurid Kärdlas (viimased 30 päeva)\n", frost_msg),
    x = "Tund",
    y = "Kuupäev"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

p_interactive <- ggplotly(p)

# Line chart

ggplot(df, aes(x = datetime, y = temp)) +
  geom_line(color = "blue", linewidth = 0.7) +
  geom_hline(yintercept = 0, color ="red", linetype = "dashed", alpha = 0.4) +
  scale_x_datetime(date_labels = "%d Apr", date_breaks = "2 days") +
  labs(
    title = "Temperatuurid Kärdlas (58.99°N 22.74°E)",
    subtitle = "Viimased 30 päeva",
    x = NULL,
    y = "°C"
  ) +
  theme_minimal()

# ----------- SALVESTUS -----------

# Line chart salvestus

# ----------- SALVESTUS -----------
output_dir <- "temperatuur_graafikud"
dir.create(output_dir, showWarnings = FALSE)

# Save line graph as image
ggsave(
  filename = file.path(output_dir, paste0(Sys.Date(), "_temp_line.png"))
)

# HTML 
htmlwidgets::saveWidget(
  p_interactive,
  "docs/heatmap.html",
  selfcontained = FALSE
)

cat("Valmis! Kontrolli HTML faili.\n")


# ----------- TELEGRAM TEAVITUS -----------

send_telegram <- function(message, token, chat_id) {
  url <- paste0("https://api.telegram.org/bot", token, "/sendMessage")
  POST(url, body = list(
    chat_id = chat_id,
    text = message,
    parse_mode = "Markdown"
  ), encode = "form")
}

TG_TOKEN <- Sys.getenv("TG_TOKEN")
TG_CHAT  <- Sys.getenv("TG_CHAT")

heatmap_url <- "https://kaisaroots.github.io/ilmaapp/heatmap.html"

tg_message <- paste0(
  "🌡 *Öökülma raport – ", Sys.Date(), "*\n",
  frost_msg, "\n",
  "📊 Heatmap: ", heatmap_url
)

send_telegram(tg_message, TG_TOKEN, TG_CHAT)
cat("Telegram teavitus saadetud!\n")


