# AT Shapefile per district (Bezirk)
# Last update: 2021-03-22


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
download.file("https://assets.post.at/-/media/Dokumente/De/Geschaeftlich/Werben/PLZ_Verzeichnis-20210301.xls", "AT Postal Codes.xls", mode = "wb")

# Import
at_postalcodes <- read_excel(path = "AT Postal Codes.xls", sheet = "Plz_Anhang", range = "A1:I2554", col_names = TRUE, col_types = "text") %>%
	clean_names() %>%
	filter(adressierbar == "Ja") %>%
	rename(PostalCode = plz, State = bundesland, City = ort) %>%
	select(PostalCode, State, City) %>%
	arrange(PostalCode) %>%
	mutate(
		State = case_when(
			State == "W" ~ "Vienna",
			State == "N" ~ "Lower Austria",
			State == "B" ~ "Burgenland",
			State == "O" ~ "Upper Austria",
			State == "Sa" ~ "Salzburg",
			State == "T" ~ "Tyrol",
			State == "V" ~ "Vorarlberg",
			State == "St" ~ "Styria",
			State == "K" ~ "Carinthia",
		),
	)

paste("'", as.character((at_postalcodes %>% filter(State == "Vienna"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Lower Austria"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Burgenland"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Upper Austria"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Salzburg"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Tyrol"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Vorarlberg"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Styria"))$PostalCode), "'", collapse = ", ", sep = "")
paste("'", as.character((at_postalcodes %>% filter(State == "Carinthia"))$PostalCode), "'", collapse = ", ", sep = "")


########################
# ---- at_gemeinden ----
########################
## Gemeinden sortiert nach Gemeindekennziffer mit Status und Postleitzahlen
# Source: Statistik Austria, http://www.statistik.at/web_de/klassifikationen/regionale_gliederungen/gemeinden/index.html

# Download file
download.file("http://www.statistik.at/verzeichnis/reglisten/gemliste_knz.csv", "AT Gemeinden.csv", mode = "wb")

# Import
at_gemeinden <- read_delim("AT Gemeinden.csv", delim = ";", skip = 2) %>%
	clean_names() %>%
	filter(!is.na(gemeindekennziffer)) %>%
	mutate(across(everything(), as.character)) %>%
	rename(id = gemeindecode, PostalCode = plz_des_gem_amtes) %>%
	select(id, PostalCode)


#################################
# ---- at_politische_bezirke ----
#################################
## Politische Bezirke
# Source: Statistik Austria, https://www.statistik.at/web_de/klassifikationen/regionale_gliederungen/politische_bezirke/index.html

# Download file
download.file("http://www.statistik.at/verzeichnis/reglisten/polbezirke.csv", "AT Politische Bezirke.csv", mode = "wb")

# Import
at_politische_bezirke <- read_delim("AT Politische Bezirke.csv", delim = ";", skip = 2) %>%
	clean_names() %>%
	filter(!is.na(bundeslandkennziffer)) %>%
	mutate(across(everything(), as.character)) %>%
	rename(State = bundesland, PolitischerBezirk = politischer_bezirk, PolitischerBezirkCode = politischer_bez_code) %>%
	select(State, PolitischerBezirk, PolitischerBezirkCode) %>%
	mutate(
		State = case_when(
			State == "Wien" ~ "Vienna",
			State == "Niederösterreich" ~ "Lower Austria",
			State == "Burgenland" ~ "Burgenland",
			State == "Oberösterreich" ~ "Upper Austria",
			State == "Salzburg" ~ "Salzburg",
			State == "Tirol" ~ "Tyrol",
			State == "Vorarlberg" ~ "Vorarlberg",
			State == "Steiermark" ~ "Styria",
			State == "Kärnten" ~ "Carinthia",
		),
	)


###########################
# ---- at_ortschaften ----
###########################
## Ortschaften (ohne Wien) sortiert nach Gemeindekennziffer mit Postleitzahlen
# Source: Statistik Austria, https://www.statistik.at/web_de/klassifikationen/regionale_gliederungen/ortschaften/index.html

# Download file
download.file("http://www.statistik.at/verzeichnis/reglisten/ortsliste.csv", "AT Ortschaften.csv", mode = "wb")

# Import
at_ortschaften <- read_delim("AT Ortschaften.csv", delim = ";", skip = 2) %>%
	clean_names() %>%
	filter(!is.na(gemeindekennziffer)) %>%
	mutate(across(everything(), as.character)) %>%
	rename(Gemeinde_Kennziffer = gemeindekennziffer, Gemeinde = gemeindename, City = ortschaftsname, PostalCode = postleitzahl) %>%
	separate_rows(PostalCode, sep = " ") %>%
	mutate(PolitischerBezirkCode = substr(Gemeinde_Kennziffer, 1, 3)) %>%
	distinct(PolitischerBezirkCode, PostalCode) %>%
	left_join(at_politische_bezirke) %>%
	select(State, PolitischerBezirk, PostalCode)


########################
# ---- at_shapefile ----
########################
## Gliederung Österreichs in Gemeinden
# Source: Statistik Austria, https://data.statistik.gv.at/web/meta.jsp?dataset=OGDEXT_GEM_1

# Download file
download.file("https://data.statistik.gv.at/data/OGDEXT_GEM_1_STATISTIK_AUSTRIA_20210101.zip", "AT Shapefile.zip", mode = "wb")
unzip(zip = "AT Shapefile.zip", exdir = "AT Shapefile")

# Import
at_shapefile <- st_read(dsn = "AT Shapefile", layer = "STATISTIK_AUSTRIA_GEM_20210101") %>%
	left_join(at_gemeinden) %>%
	group_by(PostalCode) %>%
	summarise(geometry = st_union(geometry)) %>%
	left_join((at_postalcodes %>% select(PostalCode, State))) # %>%
	# filter(State %in% c("Vienna"))

# Remove objects
rm(at_gemeinden)

# Test for duplicate PostalCode
# at_shapefile %>% st_drop_geometry() %>% group_by(PostalCode) %>% filter(n() >= 2) %>% ungroup()

# Plot
# plot(at_shapefile)

## Save shapefile
# st_write(obj = at_shapefile, dsn = "AT Shapefile New/at_shapefile.shp")


## Load shapefile
# at_shapefile_test <- st_read(dsn = "AT Shapefile New", layer = "STATISTIK_AUSTRIA_GEM_20210101")
