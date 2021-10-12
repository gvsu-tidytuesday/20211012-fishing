library(tidyverse)
library(viridis)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 42)

farmed_production <- tuesdata$`aquaculture-farmed-fish-production`
capture_v_farmed <- tuesdata$`capture-fisheries-vs-aquaculture`
capture_production <- tuesdata$`capture-fishery-production`
consumption_per_capita <- tuesdata$`fish-and-seafood-consumption-per-capita`
fish_stocks <- tuesdata$`fish-stocks-within-sustainable-levels`
catch_by_sector <- tuesdata$`global-fishery-catch-by-sector`
production_tonnes <- tuesdata$`seafood-and-fish-production-thousand-tonnesna


# catch_by_sector polor stacked bargrph

scales::show_col(viridis_pal()(5))

pallet_five_cat <- viridis_pal()(5)

catch_by_sector %>% 
  pivot_longer(cols = -(Entity:Year),
               names_to = "Sector",
               values_to = "n") %>% 
  group_by(Entity, Year) %>% 
  mutate(percent = n / sum(n)) %>% 
ggplot(aes(x = Year, y = percent, fill = fct_reorder(Sector, percent))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  coord_polar() +
  labs(title = glue::glue("Percent of fish caught by 
                          <span style = 'color:{pallet_five_cat[5]};'>Recreational</span>, 
                          <span style = 'color:{pallet_five_cat[4]};'>Subsistence</span>,\n
                          <span style = 'color:{pallet_five_cat[3]};'>Discards</span>, 
                          <span style = 'color:{pallet_five_cat[2]};'>Artisanal</span>, and 
                          <span style = 'color:{pallet_five_cat[1]};'>Industrial</span>")) +
  theme_minimal() +
  theme(plot.title = element_markdown(),
        axis.text.x = element_text(angle=45),
        axis.text.y = element_blank(),
        legend.position = "none")

# Would like to attempt:
# - highlighting specific chunks with percentages
# - remove outer circle
