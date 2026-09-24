# Changelog

## [1.2.0] — Changes from the previously published GitHub version

### Model changes

- Introduced chronological validation, early stopping on validation loss, full-period refitting, an adaptive 1–5 year lookback and reproducible 30-initialization ensembles.
- Reworked train/test analysis as an autonomous recursive forecast: normalization uses only pre-holdout data and observed holdout catches are replaced by Baranov estimates.
- Added a hybrid neural–mechanistic treatment of fishing mortality: the complete abundance-at-age vector predicted by the neural network is retained, while scenario F modifies age-specific survival, catches and spawning-time SSB.
- Added support for annual F schedules by stock and cohort. Extra rows are ignored, while the last available row is carried forward when the forecast horizon is longer.
- Corrected SSB, plus-group, unit-conversion and sensitivity calculations.

### Graphic changes

- Redesigned test and forecast SSB plots with clearer observed/predicted series, ensemble intervals, prediction-period highlighting and complete yearly x axes.
- Improved recruitment and training-diagnostic plots, including clearer uncertainty and validation information.
- Improved population, catch and weight-at-age facets; standardized Taylor-diagram colours and removed unnecessary diagnostic segments.

### Minor fixes

- Strengthened FLStock and F-matrix validation, session isolation and named-feature matching.
- Fixed tensor dimension dropping, recursive feature assembly and plotting failures affecting short or low-dimensional series.
- Removed model-complexity warnings, the F-related plot-cleaning step and the modal displayed after importing an F schedule.