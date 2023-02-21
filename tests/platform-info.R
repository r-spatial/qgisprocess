library(qgisprocess)

qgis_configure()

has_qgis()
if (is_macos()) qgis_detect_macos()
if (is_windows()) qgis_detect_windows()

if (has_qgis()) qgis_path()
if (has_qgis()) qgis_version()
if (has_qgis()) cat(qgis_run()$stdout)
if (has_qgis()) cat(qgis_run()$stderr)

if (has_qgis()) qgis_providers()

if (has_qgis()) qgis_algorithms()

if (has_qgis()) qgis_algorithms()$algorithm
