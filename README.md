Guide to reproducing analysis

1. Create a new QGIS project
2. Run load_data.py in QGIS (make sure to put in the right directory for the shapefile and the csv)
3. Run get_centroids.py in QGIS (make sure to put in the right directory for the export)
4. Run regressions.R
	- set the right working directory!
	- regression results loaded as html
	- new data saved in data/bulgaria_data_regressions.csv
5. Run lisa_moran.R
	- set the right working directory
	- results loaded in .csv (need to be mapped out later)
6. Run load_data_complete.py in QGIS
6. Generate maps in the same QGIS project with the layer from load_data_complete.py still loaded
	- IMPORTANT! - For every map, change the export path to your directory
	- automated_export_observed.py (exploratory analysis of key variables)
	- automated_export_predicted.py (Predicted values from key OLS models)
	- automated_export_residuals.py (Residuals from key OLS models)
	- automated_export_lisa_clusters.py (Clusters based on LISA Moran analysis)
7. Run lm_tests.R to determine which spatial model should be used
	- results in terminal of R Studio
8. Run sdm_test.R
	- results will come out in three separate files