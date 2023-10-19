### Code for Exeter Data Analytics Hub Data Wrangling workshop ###
### Written by Alice Trevail, October 2023 ###

### Load packages ####

# use pacman as a convenient way to install and load packages
pacman::p_load(tidyverse, # loads all tidyverse packages
               palmerpenguins, # example data
               janitor) # to clean names


### Load example data ####

glimpse(penguins_raw)
glimpse(penguins)

### full example2: raw data -> plot ####
vars_measurements <- c("culmen_length_mm", "culmen_depth_mm", "flipper_length_mm", "body_mass_g")


penguins_example <- penguins_raw %>%
  janitor::clean_names() %>%
  rename(carbon = delta_13_c_o_oo,
         nitrogen = delta_15_n_o_oo) %>%
  mutate(year = lubridate::year(date_egg), 
         sex = str_to_lower(sex), 
         species = str_split(species, pattern = " ") %>% map(.,1) %>% unlist()) %>% 
  filter(!is.na(sex))

ggplot(penguins_example, aes(x = flipper_length_mm, y = body_mass_g, 
                             col = fct_reorder2(sex, flipper_length_mm, body_mass_g))) +
  facet_grid(cols = vars(species), scales = "free")+
  geom_point()+
  geom_smooth(method = lm)+
  scale_colour_viridis_d(option = "magma", begin = 0.7, end = 0.3, name = "Sex")+
  labs(title = "Sexual dimporphism in Palmer Penguins")+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA))

ggsave(filename = "palmer_measurements.png", width = 20, height = 12, units = "cm")

penguins_summary_isotopes <- penguins_example %>% 
  pivot_longer(cols = carbon:nitrogen, names_to = "isotope", values_to = "value") %>%
  group_by(species, isotope) %>% 
  summarize(mean = mean(value, na.rm = T),
            sd =  sd(value, na.rm = T)) %>%
  pivot_wider(id_cols = species, names_from = isotope, values_from=c(mean, sd))

ggplot(penguins_example, aes(x = carbon, y = nitrogen, col = species)) +
  geom_point(alpha = 0.6)+
  geom_errorbar(data = penguins_summary_isotopes, 
                aes(x = mean_carbon, ymax = mean_nitrogen+sd_nitrogen, ymin = mean_nitrogen-sd_nitrogen, col = species), 
                inherit.aes = F, width = 0.1)+
  geom_errorbar(data = penguins_summary_isotopes, 
                aes(y = mean_nitrogen, xmax = mean_carbon+sd_carbon, xmin = mean_carbon-sd_carbon, col = species), 
                inherit.aes = F, width = 0.1)+
  scale_colour_viridis_d(option = "mako", begin = 0.75, end = 0.1, name = "Species")+
  labs(title = "Isotope signatures among Palmer Penguins")+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA))

ggsave(filename = "palmer_isotopes.png", width = 15, height = 12, units = "cm")
