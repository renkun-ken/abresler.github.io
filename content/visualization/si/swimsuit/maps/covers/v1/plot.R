library(rCharts)
ds <- read.csv('data_for_plot.csv')
ds$X <- NULL
names(ds)[2:5] <- c('fillColor','popup','longitude','latitude')
data_ <- toJSONArray(ds, json = F)

L1 <- Leaflet$new()
L1$setView(c(14.09,-38.50), zoom = 3)
L1$tileLayer(provider = 'Stamen.TonerLite')
L1$set(height = 800, width = 1600)
L1$enablePopover(T)
L1$geoJson(toGeoJSON(data_), 
	onEachFeature = '#! function(feature, layer){
	layer.bindPopup(feature.properties.popup)
	} !#',
	pointToLayer =  "#! function(feature, latlng){
	return L.circleMarker(latlng, {
	radius: 2.5,
	fillColor: feature.properties.fillColor || 'red',    
	color: '#000',
	weight: 1,
	fillOpacity: 0.9
	})
	} !#")

L1$enablePopover(F)
L1$fullScreen(TRUE)
L1
L1$legend(position = 'bottomright', 
	colors   =  names(legend_vec), 
	labels   =  as.vector(legend_vec))
L1
L1$publish('models', host = 'gist')
abresler