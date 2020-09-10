## ---- message=FALSE-----------------------------------------------------------
library(survey)
library(calidad)
dc <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf_personas, weights = ~fe)
dc

