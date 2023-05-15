packageVersion("qgisprocess")

library(qgisprocess)

qgis_configure(use_cached_data = TRUE) # triggers reconfiguration if needed

has_qgis()

if (qgisprocess:::is_macos()) qgis_detect_macos_paths()
if (qgisprocess:::is_windows()) qgis_detect_windows_paths()

if (has_qgis()) qgis_path()
if (has_qgis()) qgis_version()

if (has_qgis()) qgis_version(debug = TRUE)

if (has_qgis()) qgis_using_json_output()
if (has_qgis()) qgis_using_json_input()

if (has_qgis()) cat(qgis_run()$stdout)
if (has_qgis()) cat(qgis_run()$stderr)

if (has_qgis()) qgis_plugins()

if (has_qgis()) qgis_providers()

if (has_qgis()) qgis_algorithms()

if (has_qgis()) qgis_algorithms()$algorithm
