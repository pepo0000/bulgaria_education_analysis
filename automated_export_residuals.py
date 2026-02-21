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

import numpy as np
import pandas as pd

residuals_maps = {
     "without_secondary": {
         "layout_name": "Residuals_Map",
         "layer_name": "NUTS_joined",
         "title_text": "Residuals from an OLS Model",
         "subtitle_text": "Predicting the share of the population without secondary education",
         "new_layer_name": "Residuals",
         "export_path_png": "maps/residuals_without_secondary.png",
         "export_path_pdf": "maps/residuals_without_secondary.pdf",
         "field_name": "residuals_without_secondary"
     },
     "without_secondary_tongues": {
         "layout_name": "Residuals_Map",
         "layer_name": "NUTS_joined",
         "title_text": "Residuals from an OLS Model",
         "subtitle_text": "Predicting the share of the population without secondary education (using the rate of non-Bulgarian mother tongue speakers)",
         "new_layer_name": "Residuals",
         "export_path_png": "maps/residuals_without_secondary_tongues.png",
         "export_path_pdf": "maps/residuals_without_secondary_tongues.pdf",
         "field_name": "residuals_without_secondary_tongues"
     },
     "pca_comp1": {
        "layout_name": "Residuals_Map",
        "layer_name": "NUTS_joined",
        "title_text": "Residuals from an OLS Model",
        "subtitle_text": "Predicting Componenet 1 of PCA",
        "new_layer_name": "Residuals",
        "export_path_png": "maps/residuals_pca.png",
        "export_path_pdf": "maps/residuals_pca.pdf",
        "field_name": "residuals_pca"
    }
}


def export_residuals_map(residual_maps):
    for name, map_info in residual_maps.items():
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

        orig_ramp = QgsStyle().defaultStyle().colorRamp("RdBu")
        color_ramp = orig_ramp.clone()
        color_ramp.invert()

        values_field = [f[field_name] for f in layer.getFeatures()]


        def make_interval_ranges(data):
            series = pd.Series(data)
            
            # Separate negative and positive values
            neg = series[series < 0]
            pos = series[series > 0]
            
            # Compute negative edges
            if not neg.empty:
                neg_edges = list(np.percentile(neg, [0, 5, 33.33, 66.66, 100]))
                neg_edges[-1] = 0  # last negative bin ends at 0
            else:
                neg_edges = [-1, -1, -1, -1, 0]
            
            # Compute positive edges
            if not pos.empty:
                pos_edges = list(np.percentile(pos, [0, 33.33, 66.66, 95, 100]))
                pos_edges[0] = 0  # first positive bin starts at 0
            else:
                pos_edges = [0, 1, 1, 1, 1]
            
            # Determine whether to use decimals based on 5–95% range
            lower_95 = neg_edges[1] if not neg.empty else 0
            upper_95 = pos_edges[3] if not pos.empty else 0
            use_decimals = (-5 < lower_95) and (upper_95 < 5)
            
            # Combine edges
            edges = neg_edges[:-1] + pos_edges
            
            # Create labels
            labels = []
            for i in range(len(edges)-1):
                start, end = edges[i], edges[i+1]
                if use_decimals:
                    start_fmt = f"{start:.1f}"
                    end_fmt = f"{end:.1f}"
                else:
                    start_fmt = str(int(start))
                    end_fmt = str(int(end))
                
                if i == 0:
                    labels.append(f"<{end_fmt}")  # extreme negative
                    edges[i] = edges[i] - 1
                elif i == len(edges)-2:
                    labels.append(f">{start_fmt}")  # extreme positive
                    edges[i+1] = edges[i+1] + 1
                else:
                    labels.append(f"{start_fmt}–{end_fmt}")
            
            # Create bins
            bins = []
            for i in range(len(edges)-1):
                start, end = edges[i], edges[i+1]
                if use_decimals:
                    bins.append((round(start, 1), round(end, 1), labels[i]))
                else:
                    bins.append((int(start), int(end), labels[i]))
            
            return bins
            
        ranges = make_interval_ranges(values_field)    

        renderer_ranges = []
        num_classes = len(ranges)

        for i, (low, high, label) in enumerate(ranges):
            fraction = i / (num_classes - 1) if num_classes > 1 else 0
            color = color_ramp.color(fraction)

            symbol = QgsSymbol.defaultSymbol(layer.geometryType())
            symbol.setColor(color)

            r = QgsRendererRange(low, high, symbol, label)
            renderer_ranges.append(r)

        renderer = QgsGraduatedSymbolRenderer(field_name, renderer_ranges)
        renderer.setMode(QgsGraduatedSymbolRenderer.Custom)

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
        layout.addLayoutItem(map_item)
        map_item.attemptMove(QgsLayoutPoint(47, 28, QgsUnitTypes.LayoutMillimeters))
        map_item.attemptResize(QgsLayoutSize(220, 170, QgsUnitTypes.LayoutMillimeters))
        map_item.setRotation(0)
        map_item.refresh()

        # -------------------------------------------------------
        # TITLE
        # -------------------------------------------------------
        title = QgsLayoutItemLabel(layout)
        title.setText(title_text)
        title.setFont(QFont("Roboto", 22, QFont.Bold))
        title.adjustSizeToText()
        title.attemptMove(QgsLayoutPoint(15, 8, QgsUnitTypes.LayoutMillimeters))
        layout.addLayoutItem(title)

        # -------------------------------------------------------
        # SUBTITLE
        # -------------------------------------------------------
        subtitle = QgsLayoutItemLabel(layout)
        subtitle.setText(subtitle_text)
        subtitle.setFont(QFont("Roboto", 13))
        subtitle.setOpacity(0.75)
        subtitle.adjustSizeToText()
        subtitle.attemptMove(QgsLayoutPoint(15, 18, QgsUnitTypes.LayoutMillimeters))
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
        legend.attemptMove(QgsLayoutPoint(15, 125, QgsUnitTypes.LayoutMillimeters))
        legend.attemptResize(QgsLayoutSize(50, 50, QgsUnitTypes.LayoutMillimeters))
        layout.addLayoutItem(legend)


        # -------------------------------------------------------
        # NORTH ARROW
        # -------------------------------------------------------
        north = QgsLayoutItemPicture(layout)
        north.setPicturePath("C:/Uni/Roma_qgis_project/north_arrow.jpg")
        north.attemptMove(QgsLayoutPoint(235, 170, QgsUnitTypes.LayoutMillimeters))
        north.attemptResize(QgsLayoutSize(30, 30, QgsUnitTypes.LayoutMillimeters))
        north.setOpacity(0.9)
        layout.addLayoutItem(north)

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

export_residuals_map(residuals_maps)
