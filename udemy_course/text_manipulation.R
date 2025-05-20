# Text Manipulation

# Require libraries
library(stringr)
library(tibble)
library(dplyr)

# Create dataset
contacts <- tribble(
  ~name,              ~email,                      ~code,
  " ana garcía ",     "ana.garcia@email.com",      "abc-123",
  "LUIS fernández",   "luis.fernandez@email.org", "DEF-456",
  "mArTa pÉRez",      "marta.perez@data.net",      "ghi-789"
)

# Deleting extra spaces
contacts1 <- contacts %>%
  mutate(name = str_trim(name))

# Chage character presetation
contacts2 <- contacts1 %>%
  mutate(
    upper_nanme = str_to_upper(name), # All letters in Upper case
    low_name = str_to_lower(name), # All letter in lower case
    title_name = str_to_title(name) # Just the first letter of each word in upper
  )

# Cleaning values
contacts_email <- contacts %>%
  mutate(
    clean_email = str_replace(email, "@.*", "@dominio.com")
  )

# Replacing
contacts_code <- contacts %>%
  mutate(
    code_std = str_replace_all(code, c("abc" = "ABC", "ghi" = "GHI"))
  )

# Extracting sections
contacts %>%
  mutate(
    dominio = str_extract(email, "(?<=@)[^\\.]+")
  )


# USING REGEX

# New Dataset
messages <- tribble(
  ~user, ~text,
  "ana",   "Hola, mi correo es ana.garcia@email.com y mi código es abc-123.",
  "luis",  "Mi número es 987-654-321. 987-654-521 Escribime a luis.f@empresa.org.",
  "marta", "date estimada: 2024-12-31. Contacto: marta@dominio.net"
)

# Review if there are specific character
messages %>%
  mutate(has_at_symbol = str_detect(text, "@"))

# Extract mail
messages %>%
  mutate(mail = str_extract(text, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-z]{2,}"))

# Extract phone number
messages_phone_number <- messages %>%
  mutate(phone_number = str_extract(text, "\\d{3}-\\d{3}-\\d{3}"))

# Extracting all numbers
messages_phone_number <- messages %>%
  mutate(phone_number = str_extract_all(text, "\\d{3}-\\d{3}-\\d{3}"))

# Detecting if there are dates
messages %>%
  mutate(has_date = str_detect(text, "\\d{4}-\\d{2}-\\d{2}"))

# Replacing values
replace_email <- messages %>%
  mutate(hidden_text = str_replace_all(text, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-z]{2,}", "REDACTADO"))

# DATE MANIPULATION
# Required libraries
library(lubridate)

# Date Dataset
dates <- tribble(
  ~id, ~date_text,
  1, "2025-03-29",
  2, "15/04/2024",
  3, "2023/12/01",
  4, "Jun 10, 2022"
)

# Clear date dataset
dates_cleaned <- dates %>%
  mutate(date_text = str_trim(date_text)) %>%  # drop extra spaces
  mutate(date_text = na_if(date_text, ""))     # Replacing null values

# Conver Dates in a single format
dates_converted <- dates %>%
  mutate(
    date = case_when(
      str_detect(date_text, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(date_text),
      str_detect(date_text, "^\\d{2}/\\d{2}/\\d{4}$") ~ dmy(date_text),
      str_detect(date_text, "^\\d{4}/\\d{2}/\\d{2}$") ~ ymd(str_replace_all(date_text, "/", "-")),
      str_detect(date_text, "^[A-Za-z]{3,} \\d{1,2}, \\d{4}$") ~ mdy(date_text),
      TRUE ~ NA_Date_
    )
  )

#Extrac data components
dates_components <- dates_converted %>%
  mutate(
    año = year(date),
    mes = month(date, label = TRUE),
    dia = day(date),
    dia_semana = wday(date, label = TRUE)
  )

dates_limpias <- dates %>%
  mutate(
    date = parse_date_time(date_text, orders = c("ymd", "dmy", "ymd", "mdy")),
    año = year(date),
    mes = month(date, label = TRUE),
    dia_semana = wday(date, label = TRUE)
  )


# Date agregationns
# New dataset 
events <- tribble(
  ~usuario, ~date_start,     ~date_end,
  "Ana",    "2023-03-01",      "2023-04-15",
  "Luis",   "2024-11-10",      "2024-11-20",
  "Marta",  "2025-01-05",      NA
)

# Unified date format
events <- events %>%
  mutate(
    date_start = ymd(date_start),
    date_end = ymd(date_end)
  )

# Add a column with days duration between start and end date
events <- events %>%
  mutate(
    duration_days = as.numeric(date_end - date_start)
  )

# Add columns with differente duration types
events <- events %>%
  mutate(
    seconds_duration = as.duration(interval(date_start, date_end)),
    days_duration = seconds_duration / ddays(1)
  )

# Add a expiration date based in new parameters
events <- events %>%
  mutate(
    exp_30d = date_start + days(30),
    exp_2m = date_start + months(2),
    exp_1a = date_start + years(1)
  )


