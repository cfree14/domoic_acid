





# Estimate DA contamination risk
risk1 <- predict_da_contam(date="2014-10-15")
risk2 <- predict_da_contam(date="2015-10-15")
risk3 <- predict_da_contam(date="2015-11-15")
risk4 <- predict_da_contam(date="2015-12-15")
risk5 <- predict_da_contam(date="2016-01-15")
risk6 <- predict_da_contam(date="2016-02-15")

# Merge risk estimates
da_risks <- bind_rows(risk1, risk2, risk3,
                      risk4, risk5, risk6)


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_blank(),
                  plot.title=element_blank(),
                  strip.text=element_text(size=9),
                  legend.title=element_text(size=8),
                  legend.text=element_text(size=7),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot p(contamination)
  geom_raster(da_risks, mapping=aes(x=long_dd, y=lat_dd, fill=pcontam)) +
  facet_wrap(~date, ncol=3) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Plot depth contours
  geom_sf(data=contours100, fill=NA, col="black") +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="p(contaminated)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="bottom",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig6_dungness_da_contam_maps.png"), 
       width=6.5, height=7, units="in", dpi=600)

