
library(qgisprocess)

has_qgis()
if (is_macos()) qgis_detect_macos()
if (is_windows()) qgis_detect_windows()

if (has_qgis()) qgis_version()
if (has_qgis()) qgis_providers()
if (has_qgis()) qgis_algorithms()
if (has_qgis()) as.data.frame(qgis_algorithms()[c("algorithm", "algorithm_title")])
