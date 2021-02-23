# AT Shapefile per district (Bezirk)
# Last update: 23.02.2021


# Set working directory
setwd("C:/Users/robertog09/Data/GIS")


## Packages
# Load packages
packages_install <- function(packages){
	new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
	if (length(new.packages))
	install.packages(new.packages, dependencies = TRUE)
	sapply(packages, require, character.only = TRUE)
}

packages_required <- c("data.table", "janitor", "readxl", "sf", "tidyverse")
packages_install(packages_required)



##########################
# ---- at_postalcodes ----
##########################
## Austrian postal codes per district
# Source: Post AG, https://www.post.at/g/c/postlexikon

# Download file
download.file("https://assets.post.at/-/media/Dokumente/De/Geschaeftlich/Werben/PLZ_Verzeichnis_20210201.xls", "AT Postal Codes.xls", mode = "wb")

# Import
at_postalcodes <- read_excel(path = "AT Postal Codes.xls", sheet = "Plz_Anhang", range = "A1:I2554", col_names = TRUE, col_types = "text") %>%
	clean_names() %>%
	filter(adressierbar == "Ja") %>%
	rename(PostalCode = plz, State = bundesland) %>%
	select(PostalCode, State) %>%
	arrange(PostalCode) %>%
	mutate(
		State = case_when(
			State == "W" ~ "Wien",
			State == "N" ~ "Niederösterreich",
			State == "B" ~ "Burgenland",
			State == "O" ~ "Oberösterreich",
			State == "Sa" ~ "Salzburg",
			State == "T" ~ "Tirol",
			State == "V" ~ "Vorarlberg",
			State == "St" ~ "Steiermark",
			State == "K" ~ "Kärnten",
		),
	)


########################
# ---- at_gemeinden ----
########################
## Gemeinden sortiert nach Gemeindekennziffer mit Status und Postleitzahlen
# Source: Statistik Austria, http://www.statistik.at/web_de/klassifikationen/regionale_gliederungen/gemeinden/index.html

# Download file
download.file("http://www.statistik.at/verzeichnis/reglisten/gemliste_knz.xls", "AT Gemeinden.xls", mode = "wb")

# Import
at_gemeinden <- read_excel(path = "AT Gemeinden.xls", sheet = "gemliste_knz", range = "A4:F2121", col_names = TRUE, col_types = "text") %>%
	clean_names() %>%
	select(gemeinde_code, plz_des_gem_amtes) %>%
	rename(id = gemeinde_code, PostalCode = plz_des_gem_amtes)


########################
# ---- at_shapefile ----
########################
## Gliederung Österreichs in Gemeinden
# Source: Statistik Austria, https://data.statistik.gv.at/web/meta.jsp?dataset=OGDEXT_GEM_1

# Download file
download.file("https://data.statistik.gv.at/data/OGDEXT_GEM_1_STATISTIK_AUSTRIA_20200101.zip", "AT Shapefile.zip", mode = "wb")
unzip(zip = "AT Shapefile.zip", exdir = "AT Shapefile")

# Import
at_shapefile <- st_read(dsn = "AT Shapefile", layer = "STATISTIK_AUSTRIA_GEM_20200101Polygon") %>%
	left_join(at_gemeinden) %>%
	group_by(PostalCode) %>%
	summarise(geometry = st_union(geometry)) %>%
	left_join(at_postalcodes) # %>%
	# filter(State %in% c("Wien"))

# Remove objects
rm(at_gemeinden)


## Save shapefile
# st_write(obj = at_shapefile, dsn = "AT Shapefile New/at_shapefile.shp")


## Load shapefile
# at_shapefile_test <- st_read(dsn = "AT Shapefile New", layer = "STATISTIK_AUSTRIA_GEM_20200101Polygon")
