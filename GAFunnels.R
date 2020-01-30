# Carga/Instalacion de Librerias ---------------------------------------------------------
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load("googleAnalyticsR",
               "lubridate",
               "ggplot2",
               "gridExtra",
               "tidyverse",
               "plotly")

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
segs <- ga_segment_list()
segmnents <- segs[,c("name","id")]
segmnents <- segmnents %>%
  select(name, id) %>% 
  filter(grepl("PCAT",name,ignore.case = TRUE))
segmnents
SegId <- segmnents$id[2]

seg <- segment_ga4("PCAT.Muebles", segment_id = SegId)
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
  segments = seg,
  metrics = Mets,
  met_filters = MetFilters,
  dimensions = Dims,
  dim_filters = DimFilters,
  anti_sample = sampling,
  max = 100000
)
head(df)
colnames(df)

dat <- df %>% 
  select(pagePath,users) %>% 
  mutate(steps = case_when(
  grepl("-(pm|pr|im)-", pagePath, ignore.case = TRUE) ~ "1.Detalle de Producto",
  grepl("\\/AjaxOrderItemDisplayView?.*", pagePath, ignore.case = TRUE) ~ "2.Carrito",
  grepl("\\/OrderShippingBillingView?.*", pagePath, ignore.case = TRUE) ~ "3.Datos de Entrega",
  grepl("\\/SingleShipmentOrderSummaryView?.*", pagePath, ignore.case = TRUE) ~ "4.Revisar Orden",
  grepl("\\/OrderShippingBillingConfirmationView?.*", pagePath, ignore.case = TRUE) ~ "5.Compra")) %>% 
  select(-pagePath) %>% 
  group_by(steps) %>% 
  summarise(numbers = sum(users)) %>% 
  mutate(rate = (numbers/sum(numbers)))

#Graficar----
p <- plot_ly() %>%
  add_trace(type = "funnel",
            name = "Test",
            y = dat$steps,
            x = dat$numbers,
            textposition = "auto",
            textinfo = "value+percent initial",
            opacity = 0.45) %>%
  layout(yaxis = list(categoryarray = dat$steps))
p