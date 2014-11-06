options(stringsAsFactors=F)
library(rCharts)
library(data.table)
library(ggmap)

setwd("~/Desktop/Github/Aragorn/data/Data/Datasets for Launch/Sports/NBA/Players/NBA-ABA Birthplaces")
players <- 
	read.csv(list.files()[2])

names(players) <- tolower(names(players))
players$x <- NULL
players$hometown <- ''
us <-
	players[players$countrycode %in% 'US',]
us$hometown <-
	paste(us$city,us$statecode,'USA', sep = ', ')

non_us <-
	players[!players$countrycode %in% 'US',]
non_us$hometown <-
	paste(non_us$city,non_us$country, sep = ', ')

players <-	rbind(non_us,us)

hometowns <- 
	unique(players$hometown)
hometowns<- 
	hometowns[order(hometowns,decreasing=F)]

all_geo <- data.frame()

h <- hometowns[201:1389]
geo <- geocode(h)
all_geo <- rbind(all_geo,geo)
g <- all_geo[-c(6),]

all_locations <- cbind(hometowns,g)

names(all_locations)[1:2] <- c('longitude','latitude')
all_locations <- 
	all_locations[,c(3,2,1)]
names(all_locations)[1] <- 'hometown'
all_locations <- read.csv(list.files()[7])
merged_players <- merge(players,all_locations)
names(merged_players)
merged_players$X <- NULL
ds <- read.csv(list.files()[3])
ds$iframe_url <- paste('http://nflcombineresults.com/testembed2.php#advanced/search-advanced-query=',ds$player,'+NBA',sep='')
write.csv(merged_players,'geocoded_merged_players.csv')
cols <- c(34:36)
ds <- merged_players[ , -cols]

dt <- data.table(ds)
nbakeys <- c('player','hometown','latitude','longitude')
setkeyv(dt,cols=nbakeys)

ds <- data.frame(dt[ , j = list(
	years = yrs, mpg = mp.1, ppg = pts.1, fg = fg., rpg = trb.1, apg = ast.1, pt3 = x3p., ft = ft.
	),
	by = list(
		player, birthdate = date, hometown, latitude, longitude, iframe_url
		)]
)
ds$ft <- ds$ft*100
ds$pt3 <- 
	ds$pt3*100
ds$fg <- 
	ds$fg*100

ds$popup <-
		sprintf("<table cellpadding='3' style='line-height:1.25'><tr><th colspan='2.5'>%1$s</th></tr><tr><td><iframe src='%2$s' height='200' width='140'></iframe></td><td align='left'>Hometown: %3$s<br>Years: %4$s<br>PPG: %5$s<br>MPG: %6$s<br>FG: %7$s<br>APG: %8$s<br>RPG: %9$s<br>3PT Percentage: %10$s<br>FT Percentage: %11$s</td></tr></table>",
			ds$player,
			ds$iframe_url,
			ds$hometown,
			ds$years,
			ds$ppg,
			ds$mpg,
			ds$fg,
			ds$apg,
			ds$rpg,
			ds$pt3,
			ds$ft
			)

summary(ds$ppg)

ds$label <- cut(ds$ppg, breaks=
		length(c(0,5,10,15,20,25,28,30.1)), labels = (c('h','g','f','e','d','c','b','a')))

label <- as.character(unique(ds$label))
label <- 
	as.character(label[order(label,decreasing=F)])
fillColors <- 
	c(
		'#000000',
		'#0000bf',
		'#036c26',
		'#800000',
		'#808000',
		'#00bfbf',
		'#bf6900',
		'#dc592d'
		)

colors <- data.frame(
	label = label, fillColor = fillColors
	)

final_ds <- merge(colors,ds)
final_dt <- data.table(final_ds)

data_ <- final_ds[,c('latitude','longitude','popup','fillColor')]
write.csv(data_,'plot_data.csv')
write.csv(final_ds,'all_data.csv')

data_ <- 
	data_[!is.na(data_$longitude),]
d <- data.table(data_)
names(d)



data_ <- toJSONArray(data_, json = F)
L1 <- Leaflet$new()
L1$setView(c(38.92397, -77.05016), zoom = 3)
L1$tileLayer(provider = 'Stamen.TonerLite')
L1$set(height = 800, width = 1600)
L1$geoJson(toGeoJSON(data_), 
  onEachFeature = '#! function(feature, layer){
    layer.bindPopup(feature.properties.popup)
  } !#',
  pointToLayer =  "#! function(feature, latlng){
   return L.circleMarker(latlng, {
    radius: 3,
    fillColor: feature.properties.fillColor || 'red',    
    color: '#000',
    weight: 3,
    fillOpacity: 0.9
  })
} !#")
L1
L1$enablePopover(F)
L1$fullScreen(TRUE)
L1
L1$publish('all_nba_birthplaces', host = 'gist')
