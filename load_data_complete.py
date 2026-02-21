from qgis.core import QgsProject, QgsVectorLayer, QgsVectorLayerJoinInfo, QgsField
from PyQt5.QtCore import QVariant
import unicodedata

QgsProject.instance().removeAllMapLayers()

# -------------------------
# 1. Load NUTS layer
# -------------------------
nuts_path = "/shapefiles/LAU_RG_01M_2024_3035.shp" # <- change this to your current directory
nuts = QgsVectorLayer(nuts_path, "NUTS4", "ogr")

if not nuts.isValid():
    raise Exception("Failed to load NUTS layer")
else:
    print("loaded successfully")
    
# Filter to Bulgaria
nuts.setSubsetString('"CNTR_CODE" = \'BG\'')
    
QgsProject.instance().addMapLayer(nuts)

#2. Load fat csv

csv_path = "/data/bulgaria_data_complete_lisa.csv" # <- change this
uri = f"file:///{csv_path}?type=csv&detectTypes=yes&geomType=none"

table = QgsVectorLayer(uri, "bulgaria_data", "delimitedtext")

if not table.isValid():
    raise Exception("Failed to load CSV layer")

QgsProject.instance().addMapLayer(table)

# -------------------------
# 3. Make a memory copy with all NUTS attributes
# -------------------------
mem = QgsVectorLayer("Polygon?crs=EPSG:4326", "NUTS_joined", "memory")
prov = mem.dataProvider()

# Add NUTS fields
prov.addAttributes(nuts.fields())
mem.updateFields()


# Build lookup from CSV: region -> row
csv_index = {}
for r in table.getFeatures():
    csv_index[r["LAU_NAME"]] = r

# Add new fields from CSV table
csv_fields = [f for f in table.fields() if f.name() != "LAU_NAME"]

prov.addAttributes(csv_fields)
mem.updateFields()

# -------------------------
# 4. Copy features + attach CSV attributes
# -------------------------
new_features = []
for f in nuts.getFeatures():
    new = QgsFeature(mem.fields())
    new.setGeometry(f.geometry())
    attrs = list(f.attributes())

    region_name = f["LAU_NAME"]

    if region_name in csv_index:
        attrs += [csv_index[region_name][c.name()] for c in csv_fields]
    else:
        attrs += [None] * len(csv_fields)
    new.setAttributes(attrs)
    new_features.append(new)

prov.addFeatures(new_features)
mem.updateExtents()

QgsProject.instance().addMapLayer(mem)

print("✔ Memory layer created with joined attributes!")


# -------------------------
# 5. OPTIONAL: remove temporary layers
# -------------------------
project = QgsProject.instance()
project.removeMapLayer(nuts.id())
project.removeMapLayer(table.id())
# Keep the joined memory layer
print("✔ Removed temporary layers")
