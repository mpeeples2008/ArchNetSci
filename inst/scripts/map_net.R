map_net <-
  function(nodes,
           net,
           bounds,
           zoom_lev,
           gg_maptype,
           map_title = "",
           edge_col = "black",
           node_col = "white",
           node_size = 2) {
    # Initialize required libraries
    require(ggmap)
    require(igraph)
    require(sf)
    require(tibble)

    # Convert name, lat, and long data into sf coordinates
    locations_sf <-
      st_as_sf(nodes, coords = c("long", "lat"), crs = 4326)
    coord1 <- do.call(rbind, st_geometry(locations_sf)) %>%
      tibble::as_tibble() %>%
      setNames(c("long", "lat"))

    # Create data.frame of long and lat as xy coordinates
    xy <- as.data.frame(coord1)
    colnames(xy) <- c("x", "y")

    # Normalize legacy Stamen map names to Stadia's current naming scheme.
    stadia_maptype <- switch(
      gg_maptype,
      watercolor = "stamen_watercolor",
      toner = "stamen_toner",
      terrain = "stamen_terrain",
      terrain_background = "stamen_terrain_background",
      terrain_labels = "stamen_terrain_labels",
      terrain_lines = "stamen_terrain_lines",
      toner_background = "stamen_toner_background",
      toner_labels = "stamen_toner_labels",
      toner_lines = "stamen_toner_lines",
      gg_maptype
    )

    # Use a bundled fallback map when no Stadia API key is configured.
    if (nzchar(Sys.getenv("STADIAMAPS_API_KEY"))) {
      my_map <-
        ggmap::get_stadiamap(
          bbox = bounds,
          maptype = stadia_maptype,
          zoom = zoom_lev
        )
    } else {
      load("data/road_base.Rdata")
    }

    # Extract edgelist from network object for road_net
    edgelist1 <- igraph::as_edgelist(net)

    # Create dataframe of beginning and ending points of edges
    edges1 <- as.data.frame(matrix(NA, nrow(edgelist1), 4))
    colnames(edges1) <- c("X1", "Y1", "X2", "Y2")
    for (i in seq_len(nrow(edgelist1))) {
      edges1[i, ] <- c(nodes[which(nodes$Id == edgelist1[i, 1]), 3],
                      nodes[which(nodes$Id == edgelist1[i, 1]), 2],
                      nodes[which(nodes$Id == edgelist1[i, 2]), 3],
                      nodes[which(nodes$Id == edgelist1[i, 2]), 2])
    }
    # Plot ggmap object with network on top
    ggmap(my_map) +
      geom_segment(
        data = edges1,
        aes(
          x = X1,
          y = Y1,
          xend = X2,
          yend = Y2
        ),
        col = edge_col,
        size = 1
      ) +
      geom_point(
        data = xy,
        aes(x, y),
        alpha = 0.8,
        col = "black",
        fill = node_col,
        shape = 21,
        size = node_size,
        show.legend = F
      ) +
      ggtitle(map_title) +
      theme_void()
}
