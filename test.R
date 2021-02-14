library(spatstat)
library(sp)
library(maptools)
library(sf)
library(INLA)

# reading the rds ---------------------------------------------------------
shp <- read_sf('1_d_covariables_spatial.gpkg')%>% as('Spatial')
pal <- colorNumeric(palette = cpt(pal = 'mpl_viridis'), domain =shp$rate_fal)

# Models ------------------------------------------------------------------
shp$idarea <- 1:nrow(shp@data)
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)

nb <- poly2nb(shp)
nb2INLA("map.adj", nb)
W.boston <- nb2mat(nb, style = "B") 
W.boston.rs <- nb2mat(nb, style = "W") 
g <- inla.read.graph(filename = "map.adj")

# Besag's improper

formula <- fal ~ pp+tmax+esco+hsoil+etp+evi+ndvi+savi+aurbana+
  f(idarea,model = "besag", graph = W.boston)

amazon.besag <- inla(formula, 
                     family ="nbinomial",
                     data = shp@data,
                     E = pob,
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     control.predictor = list(compute = TRUE)
                     )

# Besag proper
formula <- fal ~ pp+tmax+esco+hsoil+etp+evi+ndvi+savi+aurbana+
  f(idarea,model = "besagproper", graph = W.boston)

amazon.besagprop <- inla(formula, 
                         family = 'nbinomial',
                         data = shp@data,
                         E = pob,
                         control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                         control.predictor = list(compute = TRUE))


# BYM
formula <- fal ~ pp+tmax+esco+hsoil+etp+evi+ndvi+savi+aurbana+
  f(idarea,model = "bym", graph = W.boston)

amazon.bym <- inla(formula,
                   family = 'nbinomial',
                   data = shp@data,
                   E = pob,
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                   control.predictor = list(compute = TRUE)
)

summary(amazon.besag)     #WAIC: 540.20, DIC:490.60 ,CPO: -461.47
summary(amazon.besagprop) #WAIC: 536.41, DIC:530.71 ,CPO: -424.25
summary(amazon.bym)       #WAIC: 529.95, DIC:530.37 ,CPO: -415.51


# Maps --------------------------------------------------------------------

shp$RR <- amazon.besag$summary.fitted.values[, "mean"]
shp$LL <- amazon.besag$summary.fitted.values[, "0.025quant"]
shp$UL <- amazon.besag$summary.fitted.values[, "0.975quant"]

summary(shp@data[, c("RR", "LL", "UL")])

mapsf <- st_as_sf(shp)
gRR <- ggplot(mapsf) + geom_sf(aes(fill = RR)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.001, 195)
  ) +
  theme_bw()

gLL <- ggplot(mapsf) + geom_sf(aes(fill = LL)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.0, 136)
  ) +
  theme_bw()



gUL <- ggplot(mapsf) + geom_sf(aes(fill = UL)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.01, 273)
  ) +
  theme_bw()

library(cowplot)
plot_grid(gRR, gLL, gUL, ncol = 1)

