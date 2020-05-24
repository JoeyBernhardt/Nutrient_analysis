

### no traits

trait_data <- read_csv("data-processed/n.long_lat3.csv")

t2 <- trait_data %>%
  filter(!grepl("^Mohanty", ref_info))

View(t2)
