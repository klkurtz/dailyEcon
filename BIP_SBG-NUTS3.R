library(eurostat)
library(ggplot2)

# Set language "de" = Deutsch, "en" = English:
language <- "en"

#==============================================================================
# Ausgewählte Regionen:
Regions <- c("AT323", "AT322", "AT321", "AT311", "AT315", "AT222",
             "AT226", "AT212", "AT333", "AT335", "DE215", "DE21M",
             "ITH10")


#==============================================================================
# Kennzahl für gewählte Regionen:
BIP <- get_eurostat("nama_10r_3gdp", time_format = "raw")
BIP$time <- eurotime2num(BIP$time)

BIP2 <- subset(BIP, geo %in% Regions & unit == "PPS_HAB" &  time == max(time-1))
BIP2 <- droplevels(BIP2)

BIP2$Region <- label_eurostat(BIP2$geo, dic = "geo", lang = language)


#====================================================================
# colorscheme for plot (EU<-blue, Euroraum <- lightblue, Österreich <- #DA291C):

ifelse(language == "de",
       matchColour <- match(c("Salzburg und Umgebung", "Pinzgau-Pongau", "Lungau"), sort(BIP2$Region)),
       matchColour <- match(c("Salzburg und Umgebung", "Pinzgau-Pongau", "Lungau"), sort(BIP2$Region))
)

colorscheme <- c(rep("#54585A", length(Regions)))
colorscheme[matchColour[1]] <- "#DA291C"
colorscheme[matchColour[2]] <- "#DA291C"
colorscheme[matchColour[3]] <- "#DA291C"
colorscheme2 <- scale_fill_manual(values = colorscheme)


#====================================================================
# Plot a bar-chart:
title <- ifelse(language == "de", paste("Salzburgs Nachbarregionen", BIP2$time),
                paste("Salzburg neighboring regions", BIP2$time))
yLab <- ifelse(language == "de", "Kaufkraftstandards (KKS) pro Einwohner",
                                 "Purchasing power standard (PPS) per inhabitant")

p <- ggplot(BIP2, aes(x=reorder(Region, -`values`), y=values, fill = Region))+
  colorscheme2+
  geom_bar(stat = "identity")+
  geom_text(aes(label=`values`), angle = 90, hjust = 1.2, vjust=0.4,
            size = 3, colour="white") +
  labs(title = title, x = "", y = yLab) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")
p


# save the plot
#ggsave(plot = p, filename = "BIP_SBG_NUTS3.jpg", device = "jpg", dpi = 800,
#       width = 27, height = 13, units = "cm",
#       limitsize = TRUE,
#       path = "...")
