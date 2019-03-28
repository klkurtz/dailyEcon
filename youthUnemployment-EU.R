# Load packages:

library(eurostat)
library(dplyr)
library(knitr)
library(ggplot2)

# Set language "de" = Deutsch, "en" = English:
language <- "en"

#===============================================================================
# Dataset - unemployment rate (Filters: age , sex, country, etc.):

unemprt <- get_eurostat("une_rt_m") # Get the dataset

month <- "2018-09-01"
unemprt_filter <- filter(unemprt, unit == "PC_ACT",
                         s_adj == "SA", sex == "T", age == "Y_LT25",
                         time == month,
                         geo %in% c("EL", "ES", "HR", "IT", "CY", "PT", "BE",
                                    "FR", "SK", "FI", "EA", "RO", "BG", "IE",
                                    "PL", "EU28", "SE", "LV", "TR", "SI", "EE",
                                    "LU", "LT", "HU", "UK", "AT", "NL", "MT",
                                    "DK", "US", "CZ", "DE")) # Filter the data

#====================================================================
# change names:

matchGeo <- match(c("DE", "EA", "EU28"), unemprt_filter$geo)

unemprt_label <- label_eurostat(unemprt_filter, lang = language) # Label your data

unemprt_label$geo[matchGeo[2]] <- "Eurozone"
unemprt_label$geo[matchGeo[3]] <- "EU"
ifelse(language == "de", unemprt_label$geo[matchGeo[1]] <- "Deutschland",
                         unemprt_label$geo[matchGeo[1]] <- "Germany")

#====================================================================
# colorscheme for plot (EU<-blue, Euroraum <- lightblue, Oesterreich <- #DA291C):

ifelse(language == "de",
  matchColour <- match(c("Österreich", "Eurozone", "EU"), sort(unemprt_label$geo)),
  matchColour <- match(c("Austria", "Eurozone", "EU"), sort(unemprt_label$geo))
)

colorscheme <- c(rep("#54585A", length(unemprt_label$geo)))
colorscheme[matchColour[1]] <- "#DA291C"
colorscheme[matchColour[2]] <- "lightblue"
colorscheme[matchColour[3]] <- "blue"
colorscheme2 <- scale_fill_manual(values = colorscheme)


#====================================================================
# Plot:
title <- ifelse(language == "de", paste("Stand:", month), paste("Time:", month))
yLab <- ifelse(language == "de", "in % der Erwerbspersonen", "% of active population")

p <- ggplot(unemprt_label, aes(x = reorder(geo, -values), y = values, fill = geo))
p <- p + colorscheme2
p <- p + geom_col()
p <- p + ylab(yLab)
p <- p + xlab(title)
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
               legend.position = "none")
p <- p + geom_text(aes(label=`values`), angle = 90, hjust = 1.3,
                   vjust=0.4, size=3, colour="white")
p


#ggsave(plot = p, filename = "Jugendarbeitslosenquote.tiff", device = "tiff", dpi = 800,
#       width = 25, height = 13, units = "cm",
#       limitsize = TRUE,
#       path = "...")





