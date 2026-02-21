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


observed_maps = {
    "without_secondary": {
        "layout_name": "Observed_Map",
        "layer_name": "NUTS_joined",
        "title_text": "Observed values for educational outcomes",
        "subtitle_text": "Share of the population without secondary education",
        "new_layer_name": "Values in %",
        "export_path_png": "maps/without_secondary.png",
        "export_path_pdf": "maps/without_secondary.pdf",
        "field_name": "without_secondary"
    },
    "percentage_roma": {
        "layout_name": "Observed_Map",
        "layer_name": "NUTS_joined",
        "title_text": "Observed values for the share of Roma population",
        "subtitle_text": "Share of the population that self-identifies as Roma",
        "new_layer_name": "Values in %",
        "export_path_png": "maps/percentage_roma.png",
        "export_path_pdf": "maps/percentage_roma.pdf",
        "field_name": "percentage_roma"
    },
    "rate_lang_non_bg": {
        "layout_name": "Observed_Map",
        "layer_name": "NUTS_joined",
        "title_text": "Observed values for the share of minority mother tongue speakers",
        "subtitle_text": "Share of the population who declare to not speak Bulgarian as their mother tongue",
        "new_layer_name": "Values in %",
        "export_path_png": "maps/rate_lang_non_bg.png",
        "export_path_pdf": "maps/rate_lang_non_bg.pdf",
        "field_name": "rate_lang_non_bg"
    },
    "pca_comp1": {
        "layout_name": "Observed_Map",
        "layer_name": "NUTS_joined",
        "title_text": "Observed values of Component 1 of PCA",
        "subtitle_text": "Componenet 1 of PCA forming a deprivation index",
        "new_layer_name": "Component 1",
        "export_path_png": "maps/pca1.png",
        "export_path_pdf": "maps/pca1.pdf",
        "field_name": "pca_comp1"
    }
}

def export_observed_map(observed_maps):
    for name, map_info in observed_maps.items():
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

        values_field = [f[field_name] for f in layer.getFeatures()]

        def make_integer_range(data, n=8):
            decimal_threshold = 10
            series = pd.Series(data)
            
            min_val, max_val = series.min(), series.max()
            
            # Compute 95% percentile to check for decimal formatting
            perc_95 = np.percentile(series, 95)
            use_decimals = perc_95 < decimal_threshold
            
            # Compute bin edges
            edges = list(np.percentile(series, np.linspace(0, 100, n+1)))
            
            # Format labels
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
                    labels.append(f"< {end_fmt}")
                    edges[i] = edges[i] - 1
                elif i == len(edges)-2:
                    labels.append(f"> {start_fmt}")
                    edges[i+1] = edges[i+1] + 1
                else:
                    labels.append(f"{start_fmt} – {end_fmt}")
            
            # Create bins
            bins = []
            for i in range(len(edges)-1):
                start, end = edges[i], edges[i+1]
                if use_decimals:
                    bins.append((round(start, 1), round(end, 1), labels[i]))
                else:
                    bins.append((int(start), int(end), labels[i]))
            
            return bins
            
    
        color_ramp = QgsStyle().defaultStyle().colorRamp("YlOrRd")
        #color_ramp = orig_ramp.clone()
        #color_ramp.invert()

        ranges = make_integer_range(values_field)

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
        map_item.setId("main_map")
        layout.addLayoutItem(map_item)
        map_item.attemptMove(QgsLayoutPoint(55, 25, QgsUnitTypes.LayoutMillimeters))
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
        title.attemptMove(QgsLayoutPoint(15, 13, QgsUnitTypes.LayoutMillimeters))
        layout.addLayoutItem(title)

        # -------------------------------------------------------
        # SUBTITLE
        # -------------------------------------------------------
        subtitle = QgsLayoutItemLabel(layout)
        subtitle.setText(subtitle_text)
        subtitle.setFont(QFont("Roboto", 13))
        subtitle.setOpacity(0.75)
        subtitle.adjustSizeToText()
        subtitle.attemptMove(QgsLayoutPoint(15, 23, QgsUnitTypes.LayoutMillimeters))
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
        legend.attemptMove(QgsLayoutPoint(15, 130, QgsUnitTypes.LayoutMillimeters))
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


export_observed_map(observed_maps)
