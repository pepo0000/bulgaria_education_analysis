from qgis.PyQt.QtCore import QVariant

layer_name = "NUTS_joined"   
nuts_layer = QgsProject.instance().mapLayersByName(layer_name)[0]

layer = nuts_layer   # your joined polygon layer
provider = layer.dataProvider()

# 1. Add new fields
provider.addAttributes([
    QgsField("centroid_x", QVariant.Double),
    QgsField("centroid_y", QVariant.Double)
])
layer.updateFields()

# 2. Get field indices
idx_x = layer.fields().indexFromName("centroid_x")
idx_y = layer.fields().indexFromName("centroid_y")

# 3. Edit layer and calculate centroids
layer.startEditing()

for f in layer.getFeatures():
    geom = f.geometry()
    centroid = geom.centroid().asPoint()
    layer.changeAttributeValue(f.id(), idx_x, centroid.x())
    layer.changeAttributeValue(f.id(), idx_y, centroid.y())

layer.commitChanges()

print("Centroid coordinates added successfully!")

output_path = "/data/bulgaria_data_centroids.csv"   # <-- change this

QgsVectorFileWriter.writeAsVectorFormat(
    layer,
    output_path,
    "UTF-8",
    layer.crs(),
    "CSV"
)

print("Exported to:", output_path)
