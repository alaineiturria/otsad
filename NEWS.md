<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->
otsad 0.2.0
-----------

-   New Features
    -   `ReduceAnomalies` is now available for the incremental processing
-   Minor improvements and fixes
    -   Fixed the problems with `CpSdEwma` `IpSdEwma` `OcpSdEwma` and `OipSdEwma` that in some cases made return more results than rows in the dataset.
    -   Fixed the error in `GetWindowsLimits` that gave an error when there were not anomalies in the dataset.
    -   `IpTsSdEwma` and `OipTsSdEwma` implementation changed to its easier use.
    -   Fixed an error in `IpKnnCad`.
