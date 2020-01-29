# Carga/Instalacion de Librerias ---------------------------------------------------------
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load("googleAnalyticsR",
               "lubridate",
               "ggplot2",
               "gridExtra",
               "tidyverse")

# Configuracion de Google Analytics----
ga_auth()
1
# ID de la vista de GA.
ga.cuentas <- ga_account_list()
# Modificar el regex del nombre la cuenta para obtener el ID requerido
cuentas <- ga.cuentas %>%
  select(accountName, webPropertyName, viewName, viewId) %>% 
  filter(grepl("coppel",accountName,ignore.case = TRUE))
cuentas
# Colocar el numero de fila de la vista requerida
ViewId <- cuentas$viewId[7]
# Fechas
StartDate <- "2019-12-01"
EndDate <- "2019-12-31"
# TRUE para dejar el sampleo de la data y FALSE para desactivarlo.
sampling <- FALSE

# Funnel Steps----
DetalleProducto <- "-(pm|pr|im)-"
Carrito <- "\\/AjaxOrderItemDisplayView?.*"
DatosEntrega <- "\\/OrderShippingBillingView?.*"
RevisarOrden <- "\\/SingleShipmentOrderSummaryView?.*"
ThankYouPage <- "\\/OrderShippingBillingConfirmationView?.*"

# Extracción de la Data de la API de Google Analytics (userType)-------------------------
#Dimension
Dims <- c("pagePath")
#Metricas
Mets <- c("users")
# Segmentos
seg <- segment_ga4(PCAT.Muebles, segment_id = "gaid::J_CellbqT8KGfVJDSZAfLg")
# Filtros
DFilterS1 <- dim_filter(
  dimension = "pagePath",
  operator = "REGEXP",
  expressions = DetalleProducto)
DFilterS2 <- dim_filter(
  dimension = "pagePath",
  operator = "REGEXP",
  expressions = Carrito)
DFilterS3 <- dim_filter(
  dimension = "pagePath",
  operator = "REGEXP",
  expressions = DatosEntrega)
DFilterS4 <- dim_filter(
  dimension = "pagePath",
  operator = "REGEXP",
  expressions = RevisarOrden)
DFilterS5 <- dim_filter(
  dimension = "pagePath",
  operator = "REGEXP",
  expressions = ThankYouPage)
MFilter1 <- met_filter("users", "GREATER", 10)

DimFilters <- filter_clause_ga4(list(DFilterS1,DFilterS2,DFilterS3,DFilterS4,DFilterS5), operator = "OR")
MetFilters <- filter_clause_ga4(list(MFilter1), operator = "AND")

# Primera llamada a la API de Google Analytics----
df <-  google_analytics(
  viewId = ViewId,
  date_range = c(StartDate,
                 EndDate),
  # segments = seg,
  metrics = Mets,
  met_filters = MetFilters,
  dimensions = Dims,
  dim_filters = DimFilters,
  anti_sample = sampling,
  max = 100000
)
head(df)

            