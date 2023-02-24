library(qgisprocess)

qgis_configure(use_cached_data = TRUE) # triggers reconfiguration if needed

has_qgis()

if (is_macos()) qgis_detect_macos()
if (is_windows()) qgis_detect_windows()

if (has_qgis()) qgis_path()
if (has_qgis()) qgis_version()

if (has_qgis()) qgis_use_json_output()
if (has_qgis()) qgis_use_json_input()

if (has_qgis()) cat(qgis_run()$stdout)
if (has_qgis()) cat(qgis_run()$stderr)

if (has_qgis()) cat(qgis_run(args = "--version")$stdout)

if (has_qgis()) qgis_plugins()

if (has_qgis()) qgis_providers()

if (has_qgis()) qgis_algorithms()

if (has_qgis()) qgis_algorithms()$algorithm
