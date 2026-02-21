from qgis.core import (
    QgsProject,
    QgsLayout,
    QgsLayoutItemMap,
    QgsLayoutItemLegend,
    QgsLayoutItemScaleBar,
    QgsLayoutItemPicture,
    QgsLayoutSize,
    QgsLayoutPoint,
    QgsUnitTypes,
    QgsLayoutExporter
)

lisa_maps = {
    "cluster_without_secondary": {
        "layout_name": "LISA_Map",
        "layer_name": "NUTS_joined",
        "title_text": "LISA analysis of the residuals from an OLS regression",
        "subtitle_text": "Clusters based on a regression with the rate of people without secondary education as DV, \nand the rate of Roma population as a factor",
        "new_layer_name": "Type of clusters",
        "export_path_png": "maps/cluster_without_secondary.png",
        "export_path_pdf": "maps/cluster_without_secondary.pdf",
        "field_name": "cluster_without_secondary"
    },
    "cluster_without_secondary_tongues": {
        "layout_name": "Observed_Map",
        "layer_name": "NUTS_joined",
        "title_text": "LISA analysis of the residuals from an OLS regression",
        "subtitle_text": "Based on a regression with the rate of people without secondary education as DV, \nand the share of the population who declare to not speak Bulgarian as their mother tongue",
        "new_layer_name": "Type of clusters",
        "export_path_png": "maps/cluster_without_secondary_tongues.png",
        "export_path_pdf": "maps/cluster_without_secondary_tongues.pdf",
        "field_name": "cluster_without_secondary_tongues"
    },
    "cluster_roma": {
        "layout_name": "Observed_Map",
        "layer_name": "NUTS_joined",
        "title_text": "LISA analysis of the share of Roma population",
        "subtitle_text": "Clusters based on the share of the population that self-identifies as Roma",
        "new_layer_name": "Type of clusters",
        "export_path_png": "maps/cluster_roma.png",
        "export_path_pdf": "maps/cluster_roma.pdf",
        "field_name": "cluster_roma"
    },
    "cluster_rate_lang_non_bg": {
        "layout_name": "Observed_Map",
        "layer_name": "NUTS_joined",
        "title_text": "LISA analysis of the share of different mother tongue speakers",
        "subtitle_text": "Clusters based on the share of the population who declare to not speak Bulgarian as their mother tongue",
        "new_layer_name": "Type of clusters",
        "export_path_png": "maps/cluster_rate_lang_non_bg.png",
        "export_path_pdf": "maps/cluster_rate_lang_non_bg.pdf",
        "field_name": "cluster_rate_lang_non_bg"
    }
}

def export_lisa_map(lisa_maps):
    for name, map_info in lisa_maps.items():
        layout_name = map_info["layout_name"]
        layer_name = map_info["layer_name"]
        title_text = map_info["title_text"]
        subtitle_text = map_info["subtitle_text"]
        new_layer_name = map_info["new_layer_name"]
        export_path_png = map_info["export_path_png"]
        export_path_pdf = map_info["export_path_pdf"]
        field_name = map_info["field_name"]
        
        project = QgsProject.instance()
        layer = project.mapLayersByName(layer_name)[0]

        layer.setName(new_layer_name)

        color_map = {
            "High-High": "#ff0000",        # red
            "Low-Low": "#0000ff",          # blue
            "High-Low": "#ff8c00",         # orange
            "Low-High": "#87cefa",         # light blue
            "Not significant": "#d3d3d3"   # light grey
        }

        categories = []

        for cluster_value, color in color_map.items():
            symbol = QgsSymbol.defaultSymbol(layer.geometryType())
            symbol.setColor(QColor(color))
            symbol.setOpacity(1.0)

            category = QgsRendererCategory(
                cluster_value,
                symbol,
                cluster_value
            )
            categories.append(category)

        renderer = QgsCategorizedSymbolRenderer(field_name, categories)

        # Apply renderer
        layer.setRenderer(renderer)
        layer.triggerRepaint()
        iface.mapCanvas().refresh()


        # Remove layout with same name if it exists
        manager = project.layoutManager()
        for l in manager.layouts():
            if l.name() == layout_name:
                manager.removeLayout(l)

        # Create new layout
        layout = QgsPrintLayout(project)
        layout.initializeDefaults()
        layout.setName(layout_name)
        manager.addLayout(layout)

        # -------------------------------------------------------
        # MAP ITEM
        # -------------------------------------------------------
        map_item = QgsLayoutItemMap(layout)
        map_item.setRect(20, 20, 200, 140)
        map_item.setLayers([layer])
        map_item.zoomToExtent(layer.extent())
        map_item.setId("main_map")
        layout.addLayoutItem(map_item)
        map_item.attemptMove(QgsLayoutPoint(55, 30, QgsUnitTypes.LayoutMillimeters))
        map_item.attemptResize(QgsLayoutSize(220, 170, QgsUnitTypes.LayoutMillimeters))
        map_item.setRotation(0)
        map_item.refresh()
        layout.refresh()

        # -------------------------------------------------------
        # TITLE
        # -------------------------------------------------------
        title = QgsLayoutItemLabel(layout)
        title.setText(title_text)
        title.setFont(QFont("Roboto", 22, QFont.Bold))
        title.attemptResize(QgsLayoutSize(270, 20, QgsUnitTypes.LayoutMillimeters))
        title.attemptMove(QgsLayoutPoint(13, 13, QgsUnitTypes.LayoutMillimeters))
        layout.addLayoutItem(title)

        # -------------------------------------------------------
        # SUBTITLE
        # -------------------------------------------------------
        subtitle = QgsLayoutItemLabel(layout)
        subtitle.setText(subtitle_text)
        subtitle.setFont(QFont("Roboto", 13))
        subtitle.setOpacity(0.75)
        subtitle.adjustSizeToText()
        subtitle.attemptMove(QgsLayoutPoint(13, 23, QgsUnitTypes.LayoutMillimeters))
        layout.addLayoutItem(subtitle)

        # -------------------------------------------------------
        # LEGEND
        # -------------------------------------------------------
        legend = QgsLayoutItemLegend(layout)
        legend.setTitle("Legend")
        legend.setLegendFilterByMapEnabled(True)
        legend.setFrameEnabled(True)
        legend.setBackgroundColor(QColor(255, 255, 255, 230))  # semi-transparent white
        legend.setFrameStrokeColor(QColor(40, 40, 40))
        legend.setFrameStrokeWidth(QgsLayoutMeasurement(0.3, QgsUnitTypes.LayoutMillimeters))
        legend.attemptMove(QgsLayoutPoint(13, 147, QgsUnitTypes.LayoutMillimeters))
        legend.attemptResize(QgsLayoutSize(50, 50, QgsUnitTypes.LayoutMillimeters))
        layout.addLayoutItem(legend)


        # -------------------------------------------------------
        # NORTH ARROW
        # -------------------------------------------------------
        north = QgsLayoutItemPicture(layout)
        north.setPicturePath("C:/Uni/Roma_qgis_project/north_arrow.jpg")
        north.attemptMove(QgsLayoutPoint(245, 175, QgsUnitTypes.LayoutMillimeters))
        north.attemptResize(QgsLayoutSize(30, 30, QgsUnitTypes.LayoutMillimeters))
        north.setOpacity(0.9)
        layout.addLayoutItem(north)
        
        # -------------------------------------------------------
        # SCALE BAR
        # -------------------------------------------------------
        # scale_bar = QgsLayoutItemScaleBar(layout)
        # scale_bar.setStyle('Single Box')
        # scale_bar.setLinkedMap(map_item)
        # scale_bar.applyDefaultSize()
        
        # scale_bar.setUnits(QgsUnitTypes.DistanceKilometers)
        # scale_bar.setUnitLabel("km")
        # scale_bar.setNumberOfSegments(4)
        # scale_bar.setNumberOfSegmentsLeft(0)

        # layout.addLayoutItem(scale_bar)
        # scale_bar.attemptMove(QgsLayoutPoint(100, 175, QgsUnitTypes.LayoutMillimeters))
        # scale_bar.setZValue(1000)
        #scale_bar.attemptResize(QgsLayoutSize(60, 10, QgsUnitTypes.LayoutMillimeters))
        
        #layout.refresh()
        

        # -------------------------------------------------------
        # EXPORT PNG
        # -------------------------------------------------------
        exporter = QgsLayoutExporter(layout)
        settings = QgsLayoutExporter.ImageExportSettings()
        settings.dpi = 300
        exporter.exportToImage(export_path_png, settings)

        # -------------------------------------------------------
        # EXPORT PDF
        # -------------------------------------------------------
        pdf_settings = QgsLayoutExporter.PdfExportSettings()
        pdf_settings.dpi = 300
        exporter.exportToPdf(export_path_pdf, pdf_settings)

        layer.setName("NUTS_joined")


export_lisa_map(lisa_maps)
