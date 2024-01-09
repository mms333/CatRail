# Funció generadora de malles
# Input:
#	- Secció d'una linia
#	- Valor mínim d'ajust de les x
#	- Valor màxim d'ajust de les x
# Output: Malla generada a partir d'una secció d'una línia

GenMalla <- function(liniaSeccio, xMin, xMax){
  GetMalla <- function(liniaSeccio, xMin, xMax)
    malla <- ggplot(data = liniaSeccio, aes(x = station, y = departure, group = train_number)) +
      geom_line(aes(colour = train_number)) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.6)
      ) +
      coord_cartesian(
        expand = FALSE,
        xlim = c(xMin, xMax),
        ylim = c(ymd_hms("1899-12-31 00:00:00"), ymd_hms("1900-1-1 12:00:00"))
      ) +
      labs(
        x = "Estació",
        y = "Hora",
        colour = "Número de tren"
      ) +
      scale_x_discrete(limits = liniaSeccio$station) +
      scale_y_datetime(date_label = "%H:%M") +
      geom_point(aes(colour = train_number))
    return(malla)
    ExportMalla <- function(malla, liniaSeccio) 
    ggsave(here("malles", liniaSeccio), plot = malla, dpi = 300,  width = 80, height = 120, units = "cm")
}

GetMalla <- function(liniaSeccio, xMin, xMax) {
	malla <- ggplot(data = liniaSeccio, aes(x = station, y = departure, group = train_number)) +
	geom_line(aes(colour = train_number)) +
	theme(
		axis.text.x = element_text(angle = 90, vjust = 0.6)
	) +
	coord_cartesian(
		expand = FALSE,
		xlim = c(xMin, xMax),
		ylim = c(ymd_hms("1899-12-31 00:00:00"), ymd_hms("1900-1-1 12:00:00"))
	) +
	labs(
		x = "Estació",
		y = "Hora",
		colour = "Número de tren"
	) +
	scale_x_discrete(limits = liniaSeccio$station) +
	scale_y_datetime(date_label = "%H:%M") +
	geom_point(aes(colour = train_number))
	
	return(malla)
}

# Exporta la malla com a JPEG
ExportMalla <- function(malla, nomExport) {
  ggsave(here("malles", nomExport), plot = malla, dpi = 300,  width = 80, height = 80, units = "cm")
}