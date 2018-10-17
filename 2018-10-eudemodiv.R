# Init --------------------------------------------------------------------

library(eurostat)      # eurostat data
library(rnaturalearth) # worldwide map data
library(tidyverse)     # tidy data transformation
library(lubridate)     # date and time support
library(sf)            # simple features GIS
library(tricolore)     # ternary color coding
library(ggtern)        # ternary ggplots

# download geospatial data for European, Asian and African countries,
# project to crs 3035 and crop to Europe
eura_sf <-
  # download geospatial data for European, Asian and African countries
  ne_countries(continent = c('europe', 'asia', 'africa'),
               returnclass = 'sf', scale = 50) %>%
  # project to crs 3035
  st_transform(crs = 3035) %>%
  # merge into single polygon
  st_union(by_feature = FALSE) %>%
  st_crop(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5)

# download geospatial data for NUTS-2 regions,
# project to crs 3035 and crop to Europe
euro_nuts2_sf <-
  get_eurostat_geospatial(
    output_class = 'sf',
    year = 2013,
    resolution = '60',
    nuts_level = 2
  ) %>%
  st_transform(
    crs = 3035
  ) %>%
  st_crop(
    xmin = 25e5, xmax = 75e5,
    ymin = 13.5e5, ymax = 54.5e5
  )

# download geospatial data for NUTS-3 regions,
# project to crs 3035 and crop to Europe
euro_nuts3_sf <-
  get_eurostat_geospatial(
    output_class = 'sf',
    year = 2013,
    resolution = '60',
    nuts_level = 3
  ) %>%
  st_transform(
    crs = 3035
  ) %>%
  st_crop(
    xmin = 25e5, xmax = 75e5,
    ymin = 13.5e5, ymax = 54.5e5
  )

# Regional age structures -------------------------------------------------

# download the data on pop counts by age at NUTS-3 level
euro_age <-
  get_eurostat(
    'demo_r_pjanaggr3',
    stringsAsFactors = FALSE
  ) %>%
  # filter NUTS-3, 2015, total population
  filter(
    sex == 'T',
    str_length(geo) == 5,
    year(time) == 2015,
    age %in% c('Y_LT15', 'Y15-64', 'Y_GE65', 'TOTAL')
  ) %>%
  # cleaning
  select(
    age, geo, values
  ) %>%
  spread(
    age, values
  ) %>%
  rename(
    total = TOTAL,
    age65plus = Y_GE65,
    age0to15 = Y_LT15,
    age15to65 = `Y15-64`
  ) %>%
  drop_na()

# average European age structure in 2015
euro_age_center <-
  with(
    euro_age,
    c(p_age0to15 = sum(age0to15)/sum(total),
      p_age15to65 = sum(age15to65)/sum(total),
      p_age65plus = sum(age65plus)/sum(total))
  )

# merge geodata and regional age structures and
# add population shares by age and
# differences from European average
euro_age_sf <-
  euro_nuts3_sf %>%
  left_join(
    euro_age,
    by = c('id' = 'geo')
  ) %>%
  mutate(
    p_age0to15 = age0to15/total,
    p_age15to65 = age15to65/total,
    p_age65plus = age65plus/total
  )

# generate centered ternary colors
tric_age <-
  Tricolore(
    euro_age_sf,
    p1 = 'p_age65plus', p2 = 'p_age15to65', p3 = 'p_age0to15',
    label_as = 'pct_diff', crop = TRUE,
    center = rev(euro_age_center), spread = 2.9, breaks = Inf,
    contrast = .5, lightness = 1, chroma = 1, hue = 2/12
  )

key_age <-
  tric_age$key +
  labs(
    caption = '',
    x = '65+', y = '15-65', z = '0-15'
  ) +
  theme(
    plot.background = element_rect(color = 'grey60')
  )
euro_age_sf$col <- tric_age$rgb

plot_age <-
  ggplot(euro_age_sf) +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = col),
    color = NA
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(key_centered),
  #   xmin = 55e5, xmax = 75e5, ymin = 37e5, ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  # labs(subtitle = 'Colors show deviations from avg. European age strucure',
  #      caption = 'The greypoint marks regions with age structure equal to the European average.\nData: Eurostat', fill = NULL) +
  theme_void()

ggsave(
  filename = 'plot_age.svg',
  plot = plot_age,
  path = './fig/raw_svg/',
  width = 27,
  height = 23,
  units = 'in'
)

# Population density ------------------------------------------------------

# divide the european continent into a 150 by 150 cell grid
# 100 by 100 km rectangular grid
euro_grid <-
  st_make_grid(
    euro_nuts3_sf,
    crs = 3035,
    what = 'polygons',
    cellsize = 100e3
  )

plot_pop_dens <-
  euro_age_sf %>%
  select(total) %>%
  st_interpolate_aw(
    to = euro_grid,
    extensive = TRUE
  ) %>%
  st_centroid() %>%
  arrange(total) %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(size = total),
    fill = 'black',
    color = 'white',
    shape = 21,
    show.legend = 'point'
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  scale_size_area(
    'Population per 100,000km2',
    max_size = 8,
    breaks = c(10e3, 100e3, 1e6, 10e6),
    labels = c('10,000', '100,000', '1,000,000', '10,000,000')
  ) +
  theme(
    legend.position = c(0.83, 0.7),
    plot.background = element_rect(color = 'grey95', size = 1)
  )

ggsave(
  filename = 'plot_pop_dens.svg',
  plot = plot_pop_dens,
  path = './fig',
  width = 8.5,
  height = 7,
  units = 'in'
)

# Population change -------------------------------------------------------

# download eurostat data of population counts by NUTS-3 region
euro_pop <-
  get_eurostat(
    'demo_r_pjanaggr3',
    stringsAsFactors = FALSE
  ) %>%
  filter(
    sex == 'T',
    str_length(geo) == 5, # NUTS-3
    age == 'TOTAL'
  )

# calculate difference in absolute population numbers from 2012 to 2017
euro_pop_diff <-
  euro_pop %>%
  filter(
    year(time) %in% c(2012, 2017)
  ) %>%
  spread(
    time, values
  ) %>%
  mutate(
    pop_diff = `2017-01-01` - `2012-01-01`
  ) %>%
  drop_na()

# merge geodata and regional population change
euro_pop_diff_sf <-
  euro_nuts3_sf %>%
  left_join(
    euro_pop_diff,
    by = c('id' = 'geo')
  )

plot_pop_diff <-
  euro_pop_diff_sf %>%
  select(pop_diff) %>%
  st_interpolate_aw(
    to = euro_grid,
    extensive = TRUE
  ) %>%
  st_centroid() %>%
  arrange(abs(pop_diff)) %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(
      size = abs(pop_diff),
      fill = ifelse(pop_diff >= 0, 'pos', 'neg')
    ),
    color = 'grey95',
    shape = 21,
    show.legend = 'point'
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  scale_size_area(
    'Population change\n2012 to 2017\ndecline red, increase blue',
    max_size = 8,
    breaks = c(1e3, 1e4, 1e5, 5e5),
    labels = c('1,000', '10,000', '100,000', '500,000'),
    guide = guide_legend(override.aes = list(color = 'black',
                                             fill = 'black'))
  ) +
  scale_fill_manual(
    values = c(pos = '#2166AC', neg = '#B2182B'),
    guide = FALSE
  ) +
  theme(
    legend.position = c(0.83, 0.7),
    plot.background = element_rect(color = 'grey95', size = 1)
  )

ggsave(
  filename = 'plot_pop_diff.svg',
  plot = plot_pop_diff,
  path = './fig',
  width = 8.5,
  height = 7,
  units = 'in'
)

# Labor force composition -------------------------------------------------

# download data on labor-force composition by NUTS-2 level for Europe
lf <- get_eurostat("lfst_r_lfe2en2")

euro_lf <-
  # download data on labor-force composition by NUTS-2 level for Europe
  get_eurostat("lfst_r_lfe2en2") %>%
  # recode time as year
  mutate(year = as.integer(lubridate::year(time))) %>%
  # subset to total age, year 2017 and NUTS-2 regions
  filter(
    age == 'Y_GE15',
    str_length(geo) == 4,
    year == 2017
  ) %>%
  # recode into three sectors
  mutate(
    sector = recode(as.character(nace_r2),
                    `A` = 'primary',
                    `B-E` = 'secondary',
                    `F` = 'secondary'),
    sector = ifelse(!sector %in% c('primary', 'secondary', 'TOTAL'),
                    'tertiary',
                    sector)
  ) %>%
  group_by(year, geo, sector) %>%
  summarise(N = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  # calculate shares on total
  spread(sector, N) %>%
  mutate(
    p_primary = primary/TOTAL,
    p_secondary = secondary/TOTAL,
    p_tertiary = tertiary/TOTAL
  )

# average European laborforce structure in 2017
euro_lf_center <-
  with(
    euro_lf,
    c(p_primary = sum(primary, na.rm = TRUE)/sum(TOTAL),
      p_secondary = sum(secondary, na.rm = TRUE)/sum(TOTAL),
      p_tertiary = sum(tertiary, na.rm = TRUE)/sum(TOTAL))
  ) %>% prop.table()

# generate colors based on compositions in `euro_sectors`, default options
tric_lf <-
  Tricolore(
    euro_lf,
    'primary', 'secondary', 'tertiary',
    center = euro_lf_center, show_center = TRUE,
    hue = 0.35, crop = TRUE
  )

# add vector of colors with with map data
euro_lf$col <- tric_lf$rgb

# merge geodata and regional labor force structures and
euro_lf_sf <-
  euro_nuts2_sf %>%
  left_join(
    euro_lf,
    by = c('id' = 'geo')
  )

plot_lf <-
  ggplot(euro_lf_sf) +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = col),
    color = NA
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(key_centered),
  #   xmin = 55e5, xmax = 75e5, ymin = 37e5, ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  # labs(subtitle = 'Colors show deviations from avg. European age strucure',
  #      caption = 'The greypoint marks regions with age structure equal to the European average.\nData: Eurostat', fill = NULL) +
  theme_void()

ggsave(
  filename = 'plot_lf.svg',
  plot = plot_lf,
  path = './fig',
  width = 8.5,
  height = 7,
  units = 'in'
)

ggsave(
  filename = 'key_lf.svg',
  plot = tric_lf$key,
  path = './fig',
  width = 3,
  height = 3,
  units = 'in'
)

# Education ---------------------------------------------------------------

euro_educ <-
  get_eurostat('edat_lfse_04') %>%
  mutate(year = lubridate::year(time)) %>%
  filter(year == 2017,
         str_length(geo) == 4,
         isced11 %in% c('ED0-2', 'ED3_4', 'ED5-8'),
         sex == 'T') %>%
  spread(isced11, values) %>%
  select(geo, ed0_2 = `ED0-2`, ed3_4 = `ED3_4`, ed5_8 = `ED5-8`)

tric_educ <-
  Tricolore(
    euro_educ,
    p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
    breaks = 4,
    h = 0.2, lightness = 0.9, chroma = 1,
    contrast = 0.7, show_center = FALSE
  )
euro_educ$col <- tric_educ$rgb

# merge geodata and regional educational attainment
euro_educ_sf <-
  euro_nuts2_sf %>%
  left_join(
    euro_educ,
    by = c('id' = 'geo')
  )

plot_educ <-
  ggplot(euro_educ_sf) +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = col),
    color = NA
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(key_centered),
  #   xmin = 55e5, xmax = 75e5, ymin = 37e5, ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  # labs(subtitle = 'Colors show deviations from avg. European age strucure',
  #      caption = 'The greypoint marks regions with age structure equal to the European average.\nData: Eurostat', fill = NULL) +
  theme_void()

ggsave(
  filename = 'plot_educ.svg',
  plot = plot_educ,
  path = './fig',
  width = 8.5,
  height = 7,
  units = 'in'
)

ggsave(
  filename = 'key_educ.pdf',
  plot = tric_educ$key,
  path = './fig',
  width = 8.5,
  height = 7,
  units = 'in'
)

# Dependency ratio --------------------------------------------------------

euro_depratio <-
  get_eurostat(
    'demo_r_pjanind3',
    stringsAsFactors = FALSE
  ) %>%
  # filter NUTS-3, 2015, total population
  filter(
    str_length(geo) == 5,
    year(time) == 2015,
    indic_de == 'DEPRATIO1'
  ) %>%
  drop_na()

# merge geodata and regional age structures and
# add population shares by age and
# differences from European average
euro_depratio_sf <-
  euro_nuts3_sf %>%
  left_join(
    euro_depratio,
    by = c('id' = 'geo')
  )

plot_depratio <-
  ggplot(euro_depratio_sf) +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = values),
    color = NA,
    show.legend = FALSE
  ) +
  scale_fill_gradient(
    low = 'black',
    high = 'white'
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  labs(fill = 'Dependency\nratio')

ggsave(
  filename = 'plot_depratio.svg',
  plot = plot_depratio,
  path = './fig/raw_svg/',
  width = 27,
  height = 23,
  units = 'in'
)

# Population pyramid ------------------------------------------------------

euro_pyra_eu <-
  get_eurostat(
    'demo_r_pjanind3',
    stringsAsFactors = FALSE
  ) %>%
  # filter NUTS-3, 2015, total population
  filter(
    geo == 'DE',
    year(time) == 2015
  ) %>%
  mutate(
    age_fct =
      fct_recode(
        indic_de,
        '00-04' = 'PC_Y0_4',
        '05-09' = 'PC_Y5_9',
        '10-14' = 'PC_Y10_14',
        '15-19' = 'PC_Y15_19',
        '20-24' = 'PC_Y20_24',
        '25-29' = 'PC_Y25_29',
        '30-34' = 'PC_Y30_34',
        '35-39' = 'PC_Y35_39',
        '40-44' = 'PC_Y40_44',
        '45-49' = 'PC_Y45_49',
        '50-54' = 'PC_Y50_54',
        '55-59' = 'PC_Y55_59',
        '60-64' = 'PC_Y60_64',
        '65-69' = 'PC_Y65_69',
        '70-74' = 'PC_Y70_74',
        '75-79' = 'PC_Y75_79',
        '80-84' = 'PC_Y80_84'
      ),
    age_fct =
      fct_relevel(
        age_fct,
        c('00-04', '05-09')
      )
  ) %>%
  filter(grepl('[0-9]{2}-[0-9]{2}',
               as.character(age_fct))) %>%
  drop_na()

plot_pyra <-
  ggplot(euro_pyra_eu,
         aes(x = age_fct, y = values)) +
  geom_col() +
  coord_flip(expand = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = '%')

ggsave(
  filename = 'plot_pyra.svg',
  plot = plot_pyra,
  path = './fig/raw_svg/',
  width = 2,
  height = 5,
  units = 'in'
)
