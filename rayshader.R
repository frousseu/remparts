
#options(rgl.debug = TRUE)

# powershell
# wget ftp://RGE_ALTI_ext:Thae5eerohsei8ve@ftp3.ign.fr/RGEALTI_2-0_1M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11.7z -outfile "C:/Users/God/Downloads/RGEALTI_2-0_1M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11.7z"

# also https://doi.org/10.1016/j.softx.2019.100331


library(rayshader)
library(sf)
library(raster)
library(terra)
library(leastcostpath)

run<-st_read("C:/Users/God/Downloads","Reunion_2015_region")
run<-st_buffer(st_buffer(run,100),-100)
lf<-list.files("C:/Users/God/Downloads/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/3_SUPPLEMENTS_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",full.names=TRUE,pattern=".shp")
source<-st_read(lf[1])
lf<-list.files("C:/Users/God/Downloads/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/1_DONNEES_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",full.names=TRUE)
l<-lapply(lf,rast)
r<-do.call("merge",l)
crs(r)<-"+proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # proj of source
#r<-focal(r,7,mean) # smooth pixels and look
#r<-aggregate(r,2)

x<-list.files("C:/Users/God/Downloads",pattern="Randopitons",full=TRUE)
x<-x[grep("Neiges",x)]
x<-lapply(x,function(i){
  lay<-st_layers(i)
  st_read(i,layer=lay$name[2])
})
x<-do.call("rbind",x)
x<-st_transform(x,crs(r))

com<-list.files("C:/Users/God/Downloads/communes-millesime-france",pattern=".shp",full=TRUE)
com<-st_read(com)
com<-st_transform(com,crs(r))

#keep<-com[com$com_name%in%c("La Possession"),]
#r<-crop(r,vect(keep))
#r<-mask(r,vect(keep))

#plot(r);p<-locator(1);p<-st_sfc(sf::st_point(c(p$x,p$y)))
p <- sf::st_sfc(sf::st_point(st_coordinates(st_centroid(st_as_sfc(st_bbox(x))))[1,]))
p <- st_buffer(p,7500)
#r<-crop(r,vect(p))
#r<-mask(r,vect(p))


mouseMode<-rgl::par3d("mouseMode")
mouseMode[5]<-"zoom"

r<-aggregate(r,5)

elmat<-raster_to_matrix(raster(r))

#filename_obj <- "C:/Users/God/Downloads/run3d.obj"

elmat %>%
  sphere_shade(texture = "desert") %>%
  #add_shadow(ray_shade(elmat, zscale = 250), 0.5) %>%
  #add_shadow(ambient_shade(elmat), 0) %>%
  add_overlay(generate_line_overlay(x,extent=extent(raster(r)), heightmap = elmat,offset=c(0,0),linewidth=0.2),alphalayer=0.95) %>% 
  #add_overlay(generate_point_overlay(colonies, color="red", size=12,attr(elmat,"extent"), heightmap = elmat)) %>% 
  
  plot_3d(elmat, zscale = 1*res(r)[1], zoom=0.5, water=FALSE,windowsize=c(1800,1000),mouseMode=mouseMode)#, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(400, 300))

#save_obj(filename_obj)

#observer3d(0, 0, 140) 

#library(archive)
#a<-archive_extract("C:/Users/God/Downloads/test.7z",dir="C:/Users/God/Downloads")
#a<-archive_extract("C:/Users/God/Downloads/RGEALTI01.7z",dir="C:/Users/God/Downloads")

if(FALSE){

grid<-st_read("C:/Users/God/Downloads/RGEALTI_2-0_TA-5M_SHP_WGS84G_WLD_2022-08-02/TA_RGEALTI_1-0_PPK_5M_WGS84G.shp")
grid<-st_transform(grid,st_crs(run))
grid<-st_crop(grid,st_buffer(run,5000))

plot(r)
plot(st_geometry(grid),add=TRUE)
plot(st_geometry(run),add=TRUE)
p<-locator(1);p<-st_sfc(sf::st_point(c(p$x,p$y)),crs=st_crs(grid))
o<-st_intersects(p,grid)[[1]]
dalle<-grid$NOM_DALLE[o]




x<-list.files("C:/Users/God/Downloads/RGEALTI_2-0_5M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/RGEALTI/1_DONNEES_LIVRAISON_2020-06-00374/RGEALTI_MNT_5M_ASC_RGR92UTM40S_REUN89_D974",full=TRUE)
ras<-rast(x[grep(dalle,x)])
elmat<-raster_to_matrix(raster(ras))

elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_3d(elmat, zscale = 1*res(ras)[1], zoom=0.5, water=FALSE,windowsize=c(1800,1000),mouseMode=mouseMode)


#library(archive)
#a<-archive("C:/Users/God/Downloads/test.7z",dir="C:/Users/God/Downloads")

#a<-archive("C:/Users/God/Downloads/RGEALTI01.7z")

prj<-"+proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

path<-list.files("C:/Users/God/Downloads",pattern="Randopitons",full=TRUE)
path<-path[grep("Littoral",path)]
path<-lapply(path,function(i){
  lay<-st_layers(i)
  st_read(i,layer=lay$name[2])
})
path<-do.call("rbind",path)
path<-st_transform(path,prj)

lf<-list.files("C:/Users/God/Downloads/RGEALTI_2-0_1M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/RGEALTI_1-0_EXT_1M_PPK_ASC/RGEALTI_2-0_1M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/RGEALTI/3_SUPPLEMENTS_LIVRAISON_2021-01-00163/RGEALTI_MNT_1M_ASC_RGR92UTM40S_REUN89_D974_20210119",full.names=TRUE,pattern=".shp")
grid<-st_read(lf[1])
grid<-st_transform(grid,st_crs(run))
plot(r)
plot(st_geometry(grid),add=TRUE)
plot(st_geometry(run),add=TRUE)
plot(st_geometry(path),add=TRUE)
p<-locator()
p<-st_sfc(sf::st_multipoint(matrix(c(p$x,p$y),ncol=2)),crs=st_crs(grid))
o<-st_intersects(p,grid)[[1]]
dalles<-unique(grid$NOM_DALLE[o])
pols<-st_as_sf(st_buffer(p,100))


lf<-list.files("C:/Users/God/Downloads/RGEALTI_2-0_1M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/RGEALTI_1-0_EXT_1M_PPK_ASC/RGEALTI_2-0_1M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/RGEALTI/1_DONNEES_LIVRAISON_2021-01-00163/RGEALTI_MNT_1M_ASC_RGR92UTM40S_REUN89_D974_20210119",full.names=TRUE)
lf<-lf[sapply(dalles,function(i){grep(i,lf)})]
l<-lapply(lf,rast)
if(length(l)>1){
  ras<-do.call("merge",l)
}else{
  ras<-l[[1]]
}
crs(ras)<-prj

te<-terrain(ras,v="slope",neighbors=8)
te[te<45]<-NA
p<-as.polygons(te)
p<-st_as_sf(p)
p<-st_as_sf(st_union(p))
slopes<-st_cast(p,"POLYGON")

path<-st_crop(path,ras)
path<-st_segmentize(path,dfMaxLength=1)

ras<-aggregate(ras,2)

#ras<-crop(ras,ext(ras)-1) # hack for basename bug
#ras<-crop(ras,vect(st_buffer(p,200)))
#ras<-mask(ras,vect(st_buffer(p,200)))


te<-terrain(ras,v="slope",neighbors=8)
te<-te/global(te,"max",na.rm=TRUE)[,1]*255
te<-c(te,te,te,te)
#RGB(te)<-1:3
names(te) = c("r","g","b","a")
te_r = rayshader::raster_to_matrix(raster(te$r))
te_g = rayshader::raster_to_matrix(raster(te$g))
te_b = rayshader::raster_to_matrix(raster(te$b))
te_a = rayshader::raster_to_matrix(raster(te$a))
elev = rayshader::raster_to_matrix(raster(ras))
te_rgb = array(0,dim=c(nrow(te_r),ncol(te_r),4))
te_rgb[,,1] = 1-te_r/255 #Red layer
te_rgb[,,2] = 1-te_g/255 #Blue layer
te_rgb[,,3] = 1-te_b/255 #Green layer
te_rgb[,,4] = 1-te_a/255 #Alpha layer
te_rgb = aperm(te_rgb,c(2,1,3))



elmat<-raster_to_matrix(raster(ras))


elmat %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(generate_line_overlay(path,extent=extent(raster(ras)), heightmap = elmat,offset=c(0,0),linewidth=0.2),alphalayer=0.45) %>% 
  add_overlay(generate_polygon_overlay(slopes,extent=extent(raster(ras)), heightmap = elmat,offset=c(0,0),palette="red",linecolor=NA),alphalayer=0.50) %>% 
  #add_overlay(generate_polygon_overlay(pols,extent=extent(raster(ras)), heightmap = elmat,offset=c(0,0),palette="darkgreen",linecolor=NA),alphalayer=0.75) %>% 
  plot_3d(elmat, zscale = 1*res(ras)[1], zoom=0.5, water=FALSE,windowsize=c(1800,1000),mouseMode=mouseMode,triangulate=FALSE)


render_path(extent=extent(raster(ras)), 
            lat = path, 
            zscale=1*res(ras)[1],color="darkgreen", heightmap= elmat, offset=2, linewidth=3)

render_path(extent=extent(raster(ras)), 
            lat = lcp, 
            zscale=1*res(ras)[1],color="darkred", heightmap= elmat, offset=2, linewidth=3)

render_points(extent=extent(raster(ras)),lat = st_coordinates(ab)[,2], lon = st_coordinates(ab)[,1],zscale=1*res(ras)[1],color="black", heightmap= elmat, size=10)


#inat2<-st_crop(inat,ras)
#render_points(extent=extent(raster(ras)),lat = st_coordinates(inat2)[,2], lon = st_coordinates(inat2)[,1],zscale=1*res(ras)[1],color="darkgreen", heightmap= elmat, size=10)



te<-terrain(ras,v="slope",neighbors=4)

mailles<-st_read("C:/Users/God/Downloads/carto_maille22_uicn_all_V1.gpkg")
mailles<-st_read("C:/Users/God/Downloads/maille22.gpkg")
st_layers("C:/Users/God/Downloads/carto_maille22_uicn_all_V1.gpkg")

}


library(sf)
URL <- "/vsicurl_streaming/https://object-arbutus.cloud.computecanada.ca/bq-io/io/IUCN_rangemaps/mammals.fgb"
system.time(x <- st_read(URL, query = "SELECT * FROM MAMMALS WHERE binomial='Puma concolor'", options='VERIFY_BUFFERS=NO'))





#####################
#####################


elmat<-raster_to_matrix(raster(ras))

te<-terrain(ras,v="slope",neighbors=8)
te<-te/global(te,"max",na.rm=TRUE)[,1]*255

te<-c(te,te,te)
RGB(te)<-1:3


names(te) = c("r","g","b")

te_r = rayshader::raster_to_matrix(raster(te$r))
te_g = rayshader::raster_to_matrix(raster(te$g))
te_b = rayshader::raster_to_matrix(raster(te$b))

elev = rayshader::raster_to_matrix(raster(ras))

te_rgb = array(0,dim=c(nrow(te_r),ncol(te_r),3))

te_rgb[,,1] = 1-te_r/255 #Red layer
te_rgb[,,2] = 1-te_g/255 #Blue layer
te_rgb[,,3] = 1-te_b/255 #Green layer

te_rgb = aperm(te_rgb,c(2,1,3))



te_rgb %>% 
add_overlay(generate_polygon_overlay(p,extent=extent(raster(ras)), heightmap = elmat,offset=c(0,0),palette="red",linecolor=NA),alphalayer=0.45) %>% 
plot_3d(elmat, zscale = 1*res(ras)[1], zoom=0.5, water=FALSE,windowsize=c(1800,1000),mouseMode=mouseMode,triangulate=FALSE)

  


te<-terrain(ras,v="slope",neighbors=4)
te[te<55]<-NA
#te<-aggregate(te)
p<-as.polygons(te)
p<-st_as_sf(p)
p<-st_as_sf(st_union(p))
p<-st_cast(p,"POLYGON")

plot(p,border=NA,col="tomato")


library(leastcostpath)


neigh <- 4
slope_cs <- create_slope_cs(dem = raster(ras), cost_function = "tobler", neighbours = neigh)
plot(raster(slope_cs), col = grey.colors(100))
#AB<-locator(2)
#AB<-setNames(lapply(asplit(st_coordinates(ab),1),unname),c("x","y"))
A <- sp::SpatialPoints(cbind(AB$x[1],AB$y[1]))
B <- sp::SpatialPoints(cbind(AB$x[2],AB$y[2]))

maxslope <- te > 65
maxslope[maxslope == 0] <- NA
maxslope<-raster(maxslope)
maxslope <- leastcostpath::create_barrier_cs(raster = maxslope, barrier = maxslope, neighbours = neigh, field = 0, background = 1)
#slope_cs <- slope_cs * maxslope


# if cost function is anisotropic (e.g. Tobler's Hiking Function) then LCP from A-to-B and B-to-A may be different. To create LCP in both directions set the directional argument to FALSE (default)
lcp<-create_lcp(cost_surface = slope_cs, origin = A, destination = B, directional = TRUE)
lcpw<-create_wide_lcp(cost_surface = slope_cs, origin = A,destination = B, path_ncells = 15)


plot(raster(slope_cs), col = grey.colors(100))
plot(A, add = T, col = "black")
plot(B, add = T, col = "black")
plot(lcp, add = T, col = "red")


lcp<-st_as_sf(lcp)
lcpw<-st_as_sf(lcpw)
ab<-st_as_sf(rbind(A,B))
ab <-structure(
    list(
      geometry = structure(
        list(structure(
          c(330874.969924812,
            7678922.96240602),
          class = c("XY", "POINT", "sfg")
        ), structure(
          c(330710.496240602,
            7678662.15413534),
          class = c("XY", "POINT", "sfg")
        )),
        class = c("sfc_POINT",
                  "sfc"),
        precision = 0,
        bbox = structure(
          c(
            xmin = 330710.496240602,
            ymin = 7678662.15413534,
            xmax = 330874.969924812,
            ymax = 7678922.96240602
          ),
          class = "bbox"
        ),
        crs = structure(list(input = NA_character_,
                             wkt = NA_character_), class = "crs"),
        n_empty = 0L
      )
    ),
    row.names = 1:2,
    class = c("sf",
              "data.frame"),
    sf_column = "geometry",
    agr = structure(
      integer(0),
      class = "factor",
      levels = c("constant",
                 "aggregate", "identity"),
      names = character(0)
    )
  )



side<-30
m<-matrix(c(rep(4,(side^2)/2),rep(1,(side^2)/2)),ncol=side,byrow=TRUE)
r<-rast(m)
plot(r)
plot(terrain(r,"slope"))

w<-matrix(c(1,1,1,1,1,1,1,1,1),nrow=3)
r2<-focal(r,w=w,fun=function(i){abs(diff(range(i,na.rm=TRUE)))})
#r2<-abs(r2[[1]]-r2[[2]])
plot(r2)

r2<-focal(ras,w=w,fun=range)


