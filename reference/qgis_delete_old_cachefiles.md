# Delete old cache files

Delete old cache files

## Usage

``` r
qgis_delete_old_cachefiles(
  type = "all",
  age_days = NULL,
  quiet = FALSE,
  startup = FALSE
)
```

## Arguments

- type:

  A string; either `"all"`, `"package"` or `"help"`. This selects the
  type of cache files to delete.

- age_days:

  A number that expresses a cache file's age that must be exceeded for
  it to be deleted, with age defined as days elapsed since the file's
  last modification date. The default value of 90 days can also be
  changed with the option `qgisprocess.cachefiles_days_keep` or the
  environment variable `R_QGISPROCESS_CACHEFILES_DAYS_KEEP`.

- quiet:

  Use `FALSE` to display more information, possibly useful for
  debugging.

- startup:

  Logical. Is this command being run while loading the package?

## Details

Note that a currently used package cache file will never be deleted.

This function is called when loading the package.

## Note

This is an internal function.
