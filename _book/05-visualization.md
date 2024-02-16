# Network Visualization{#Visualization}

![](images/image_break.png){width=100%}

This section follows along with Brughmans and Peeples (2023) chapter 6 to illustrate the wide variety of techniques which can be used for network visualization. We begin with some general examples of network plotting and then demonstrate how to replicate all of the specific examples that appear in the book. For most of the examples below we rely on R but in a few cases we use other software and provide additional details and data formats. 

There are already some excellent resources online for learning how to create beautiful and informative network visuals. We recommend the excellent online materials produced by Dr. Katherine Ognyanova [available on her website](https://kateto.net/) and her [Static and dynamic network visualization with R](https://kateto.net/network-visualization) workshop materials in particular. Many of the examples here and in the book take inspiration from her work. In addition to this, the [R Graph Gallery](https://www.r-graph-gallery.com/) website created by [Holtz Yan](https://github.com/holtzy) provides numerous excellent examples of plots in R using the `ggplot2` and `ggraph` packages among many others. If you are new to R, it will probably be helpful for you to read a bit about basic graphic functions (including in the tutorials listed here) before getting started.

## Data and R Setup{#VizDatasets}

In order to make it as easy as possible for users to replicate specific visuals from the book and the other examples in this tutorial we have tried to make the examples as modular as possible. This means that we provide calls to initialize the required libraries for each plot within each relevant chunk of code (so that you can more easily tell what package does what) and we also provide links to download the data required to replicate each figure in the description of that figure below. The data sets we use here include both .csv and other format files as well as .Rdata files that contain sets of specific R objects formatted as required for individual chunks of code.

If you plan on working through this entire tutorial and would like to download all of the associated data at once [you can download this zip file](All_data.zip). Simply extract this zip folder into your R working directory and the examples below will then work. Note that all of the examples below are setup such that the data should be contained in a sub-folder of your working directory called "data" (note that directories and file names are case sensitive).

## Visualizing Networks in R{#ViZInR}

<div class="rmdnote">
<p>There are many tools available for creating network visualizations in
R including functions built directly into the <code>igraph</code> and
<code>statnet</code> packages. Before we get into the details, we first
briefly illustrate the primary network plotting options for
<code>igraph</code>, <code>statnet</code> and a visualization package
called <code>ggraph</code>. We start here by initializing our required
libraries and reading in an adjacency matrix and creating network
objects in both the <code>igraph</code> and <code>statnet</code> format.
These will be the basis for all examples in this section.</p>
</div>

Let's start by reading in our example data and then we describe each package in turn:


```r
library(igraph)
library(statnet)
library(ggraph)
library(intergraph)


cibola <-
  read.csv(file = "data/Cibola_adj.csv",
           header = TRUE,
           row.names = 1)

cibola_attr <- read.csv(file = "data/Cibola_attr.csv", header = TRUE)

# Create network in igraph format
cibola_i <- igraph::graph_from_adjacency_matrix(as.matrix(cibola),
                                                mode = "undirected")
cibola_i
```

```
## IGRAPH 64d5f28 UN-- 31 167 -- 
## + attr: name (v/c)
## + edges from 64d5f28 (vertex names):
##  [1] Apache.Creek--Casa.Malpais          Apache.Creek--Coyote.Creek         
##  [3] Apache.Creek--Hooper.Ranch          Apache.Creek--Horse.Camp.Mill      
##  [5] Apache.Creek--Hubble.Corner         Apache.Creek--Mineral.Creek.Pueblo 
##  [7] Apache.Creek--Rudd.Creek.Ruin       Apache.Creek--Techado.Springs      
##  [9] Apache.Creek--Tri.R.Pueblo          Apache.Creek--UG481                
## [11] Apache.Creek--UG494                 Atsinna     --Cienega              
## [13] Atsinna     --Los.Gigantes          Atsinna     --Mirabal              
## [15] Atsinna     --Ojo.Bonito            Atsinna     --Pueblo.de.los.Muertos
## + ... omitted several edges
```

```r
# Create network object in statnet/network format
cibola_n <- asNetwork(cibola_i)
cibola_n
```

```
##  Network attributes:
##   vertices = 31 
##   directed = FALSE 
##   hyper = FALSE 
##   loops = FALSE 
##   multiple = FALSE 
##   bipartite = FALSE 
##   total edges= 167 
##     missing edges= 0 
##     non-missing edges= 167 
## 
##  Vertex attribute names: 
##     vertex.names 
## 
## No edge attributes
```

### `network` package{#networkpackage}

All you need to do to plot a `network/statnet` network object is to simply type `plot(nameofnetwork)`. By default, this creates a network plot where all nodes and edges are shown the same color and weight using the Fruchterman-Reingold graph layout by default. There are, however, many options that can be altered for this basic plot. In order to see the details you can type `?plot.network` at the console for the associated document. 


```r
set.seed(6332)
plot(cibola_n)
```

<img src="05-visualization_files/figure-html/Fig_net_simple-1.png" width="672" />

In order to change the color of nodes, the layout, symbols, or any other features, you can add arguments as detailed in the help document. These arguments can include calls to other functions, mathematical expressions, or even additional data in other attribute files. For example in the following plot, we calculate degree centrality directly within the plot call and then divide the result by 10 to ensure that the nodes are a reasonable size in the plot. We use the `vertex.cex` argument to set node size based on the results of that expression. Further we change the layout using the "mode" argument to produce a network graph using the Kamada-Kawai layout. We change the color of the nodes so that they represent the `Region` variable in the associated attribute file using the `vertex.col` argument and and set change all edge colors using the `edge.col` argument. Finally, we use `displayisolates = FALSE` to indicate that we do not want the single isolated node to be plotted. These are but a few of the many options.


```r
set.seed(436)
plot(
  cibola_n,
  vertex.cex = sna::degree(cibola_n) / 10,
  mode = "kamadakawai",
  vertex.col = as.factor(cibola_attr$Region),
  edge.col = "darkgray",
  displayisolates = FALSE
)
```

<img src="05-visualization_files/figure-html/Fig_network_net-1.png" width="672" />

### `igraph` package{#igraphpackage}

The `igraph` package also has a built in plotting function called `plot.igraph`. To call this you again just need to type `plot(yournetworkhere)` and provide an igraph object (R can tell what kind of object you have if you simply type plot). The default igraph plot again uses a Fruchterman-Reingold layout just like `statnet/network` but by default each node is labeled. 


```r
set.seed(435)
plot(cibola_i)
```

<img src="05-visualization_files/figure-html/Fig_igraph_simple-1.png" width="672" />

Let"s take a look at a few of the options we can alter to change this plot. There are again many options to explore here and the help documents for igraph.plotting describe them in detail (type ?igraph.plotting at the console for more). If you want to explore `igraph` further, we suggest you check the [Network Visualization](https://kateto.net/network-visualization) tutorial linked above which provides a discussion of the wide variety of options. 


```r
set.seed(3463)
plot(
  cibola_i,
  vertex.size = igraph::eigen_centrality(cibola_i)$vector * 20,
  layout = layout_with_kk,
  vertex.color = as.factor(cibola_attr$Great.Kiva),
  edge.color = "darkblue",
  vertex.frame.color = "red",
  vertex.label = NA
)
```

<img src="05-visualization_files/figure-html/Fig_igraph-1.png" width="672" />

### `ggraph` package{#ggraphpackage}

The `ggraph` package provides a powerful set of tools for plotting and visualizing network data in R. The format used for this package is a bit different from what we saw above and instead relies on the `ggplot2` style of plots where a plot type is called and modifications are made with sets of lines with additional arguments separated by `+`. Although this takes a bit of getting used to we have found that the ggplot format is often more intuitive for making complex graphics once you understand the basics.

Essentially, the way the `ggraph` call works is you start with a `ggraph` function call which includes the network object and the layout information. You then provide lines specifying the edges `geom_edge_link` and nodes `geom_node_point` features and so on. Conveniently the `ggraph` function call will take either an `igraph` or a `network` object so you do not need to convert.

Here is an example. Here we first the call for the igraph network object `Cibola_i` and specify the Fruchterman-Reingold layout using `layout = "fr"`. Next, we call the `geom_edge_link` and specify edge colors. The `geom_node_point` call then specifies many attributes of the nodes including the fill color, outline color, transparency (alpha), shape, and size using the `igraph::degree` function. The `scale_size` call then tells the plot to scale the node size specified in the previous line to range between 1 and 4. Finally `theme_graph` is a basic call to the `ggraph` theme that tells the plot to make the background white and to remove the margins around the edge of the plot. Let's see how this looks. 

In the next section we go over the most common options in `ggraph` in detail.


```r
set.seed(4368)
# Specify network to use and layout
ggraph(cibola_i, layout = "fr") +
  # Specify edge features
  geom_edge_link(color = "darkgray") +
  # Specify node features
  geom_node_point(
    fill = "blue",
    color = "red",
    alpha = 0.5,
    shape = 22,
    aes(size = igraph::degree(cibola_i)),
    show.legend = FALSE
  ) +
  # Set the upper and lower limit of the "size" variable
  scale_size(range = c(1, 10)) +
  # Set the theme "theme_graph" is the default theme for networks
  theme_graph()
```

```
## Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="05-visualization_files/figure-html/Fig_ggraph-1.png" width="672" />

There are many options for the `ggraph` package and we recommend exploring the help document (`?ggraph`) as well as the [Data Imaginist](https://www.data-imaginist.com/tags/visualization) `ggraph` tutorial online for more. Most of the examples below will use the `ggraph` format.

## Network Visualization Options{#NetVizOptions}

In this section we illustrate some of the most useful graphical options for visualizing networks, focusing in particular on the `ggraph` format. In most cases there are similar options available in the plotting functions for both `network` and `igraph`. Where relevant we reference specific figures from the book and this tutorial and the code for all of the figures produced in R is presented in the next session. For all of the examples in this section we will use the [Cibola technological similarity data (click here to download)](data/Peeples2018.Rdata). First we call the required packages and import the data.


```r
library(igraph)
library(statnet)
library(intergraph)
library(ggraph)

load("data/Peeples2018.Rdata")

# Create igraph object for plots below
net <- asIgraph(brnet)
```

### Graph Layout{#GraphLayouts}

Graph layout simply refers to the placement and organization in 2-dimensional or 3-dimensional space of nodes and edges in a network. 

#### Manual or User Defined Layouts{#ManualLayouts}

There are a few options for manually defining node placement and graph layout in R and the easiest is to simply provide x and y coordinates directly. In this example, we plot the Cibola technological similarity network with a set of x and y coordinates that group sites in the same region in a grid configuration. For another example of this approach see [Figure 6.1 below](#Figure_6_1). For an example of how you can interactively define a layout see [Figure 6.5](#Figure_6_5) 


```r
# site_info - site location and attribute data

# Create xy coordinates grouped by region
xy <-
  matrix(
    c(1, 1, 3, 3, 2, 1, 2, 1.2, 3, 3.2, 2, 1.4, 1, 1.2, 2, 2.2, 3,
      2, 3, 1, 2.2, 1, 2, 3, 2, 3.2, 3, 1.2, 3, 3.4, 1, 2, 3.2, 3.2,
      3, 1.4, 3, 2.2, 2, 2, 3.2, 3.4, 2.2, 1.2, 3.4, 3.2, 3.2, 1, 2,
      3.4, 3.4, 3.4, 2.2, 3, 2.2, 3.2, 2.2, 3.4, 1, 1.4, 3, 2.4),
    nrow = 31,
    ncol = 2,
    byrow = TRUE
)

# Plot using "manual" layout and specify xy coordinates
ggraph(net,
       layout = "manual",
       x = xy[, 1],
       y = xy[, 2]) +
  geom_edge_link(edge_color = "gray") +
  geom_node_point(aes(size = 4, col = site_info$Region),
                  show.legend = FALSE) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/manual_layout-1.png" width="672" />

#### Geographic Layouts{#GeographicLayouts}

Plotting networks using a a geographic layout is essentially the same as plotting with a manual layout except that you specify geographic coordinates instead of other coordinates. See [Figure 6.2](#Figure_6_2) for another example.


```r
ggraph(net,
       layout = "manual",
       x = site_info$x,
       y = site_info$y) +
  geom_edge_link(edge_color = "gray") +
  geom_node_point(aes(size = 4, col = site_info$Region),
                  show.legend = FALSE) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/map1-1.png" width="672" />

When working with geographic data, it is also sometimes useful to plot directly on top of some sort of base map. There are many options for this but one of the most convenient is to use the `sf` and `ggmap` packages to directly download the relevant base map layer and plot directly on top of it. This first requires converting points to latitude and longitude in decimal degrees if they are not already in that format. See the details on the [sf package](https://r-spatial.github.io/sf/) and [ggmap package](https://github.com/dkahle/ggmap) for more details. 

Here we demonstrate the use of the `ggmap` and the `get_stadiamap` function which requires a bit of additional explanation. This function automatically retrieves a background map for you using a few arguments:

* **`bbox`** - the bounding box which represents the decimal degrees longitude and latitude coordinates of the lower left and upper right area you wish to map.
* **`maptype`** - a name that indicates the style of map to use ([check here for options](https://rdrr.io/github/dkahle/ggmap/man/get_stadiamap.html)).
* **`zoom`** - a variable denoting the detail or zoom level to be retrieved. Higher number give more detail but take longer to detail.

As of early 2024 the `get_stadiamap` function also requires that you sign up for an account at [stadiamaps.com](https://stadiamaps.com). This account is free and allows you to download a large number of background maps in R per month (likely FAR more than an individual would ever use). There are a few setup steps required to get this to work. You can follow the steps below or [click here for a YouTube video outlining steps 1 thorugh 3 below](https://www.youtube-nocookie.com/embed/6jUSyI6x3xg).

1) First, you need to sign up for a free account at Stadiamaps.

2) Once you sign in, you will be asked to great a Property Name, designating where you will be using data. You can simply call it "R analysis" or anything you'd like.

3) Once you create this property you'll be able to assign an API key to it by clicking the "Add API" button.

4) Now you simply need to let R know your API to allow map download access. In order to do this copy the API key that is visible on the stadiamaps page from the property you created and then run the following line of code adding your actual API key in the place of [YOUR KEY HERE]

<div class="rmdtip">
<p>We describe the specifics of spatial data handling, geographic
coordinates, and projection in the section on <a
href="#SpatialNetworks">Spatial Networks</a>. See that section for a
full description and how R deals with geographic information.</p>
</div>

Note, for the ease of demonstration, in the remainder of this online guide (other than the code chunk below) we pre-download the maps and provide them as a file instead of using the `get_stadiamap` function.


```
## Warning: package 'ggmap' was built under R version 4.2.3
```

```
## ℹ Google's Terms of Service: <https://mapsplatform.google.com>
##   Stadia Maps' Terms of Service: <https://stadiamaps.com/terms-of-service/>
##   OpenStreetMap's Tile Usage Policy: <https://operations.osmfoundation.org/policies/tiles/>
## ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.
```




```r
library(sf)
library(ggmap)

# Convert attribute location data to sf coordinates and change
# map projection
locations_sf <-
  st_as_sf(site_info, coords = c("x", "y"), crs = 26912)
loc_trans <- st_transform(locations_sf, crs = 4326)
coord1 <- do.call(rbind, st_geometry(loc_trans)) %>%
  tibble::as_tibble() %>%
  setNames(c("lon", "lat"))

xy <- as.data.frame(coord1)
colnames(xy) <- c("x", "y")

# Get basemap "stamen_terrain_background" data for map in black and white
# the bbox argument is used to specify the corners of the box to be
# used and zoom determines the detail.
base_cibola <- get_stadiamap(
  bbox = c(-110.2, 33.4, -107.8, 35.3),
  zoom = 10,
  maptype = "stamen_terrain_background",
  color = "bw"
)

# Extract edge list from network object
edgelist <- get.edgelist(net)

# Create data frame of beginning and ending points of edges
edges <- data.frame(xy[edgelist[, 1], ], xy[edgelist[, 2], ])
colnames(edges) <- c("X1", "Y1", "X2", "Y2")

# Plot original data on map
ggmap(base_cibola, darken = 0.35) +
  geom_segment(
    data = edges,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2
    ),
    col = "white",
    alpha = 0.8,
    size = 1
  ) +
  geom_point(
    data = xy,
    aes(x, y, col = site_info$Region),
    alpha = 0.8,
    size = 5,
    show.legend = FALSE
  ) +
  theme_void()
```

<img src="05-visualization_files/figure-html/geo_layout-1.png" width="672" />

#### Shape-Based and Algorithmic Layouts{#AlgorithmicLayouts}

There are a wide variety of shape-based and algorithmic layouts available for use in R. In most cases, all it takes to change layouts is to simply modify a single line the `ggraph` call to specify our desired layout. The `ggraph` package can use any of the `igraph` layouts as well as many that are built directly into the package. See `?ggraph` for more details and to see the options. Here we show a few examples. Note that we leave the figures calls the same except for the argument `layout = "yourlayout"` in each `ggraph` call and the `ggtitle` name. For the layouts that involve randomization, we use the `set.seed()` function to make sure they will always plot the same. See the discussion of [Figure 6.8](#Figure_6_8) below for more details. Beyond this [Figure 6.9](#Figure_6_9) provides additional options that can be used for hierarchical network data.

<div class="rmdtip">
<p>If you do not specify a graph layout in <code>ggraph</code>, the
plotting function will automatically choose a layout using the
<code>layout_nicely()</code> function. Although this sometimes produces
useful the layout used is not specified in the call so we recommend
supplying a <code>layout</code> argument directly.</p>
</div>


```r
# circular layout
circ_net <- ggraph(net, layout = "circle") +
  geom_edge_link(edge_color = "gray") +
  geom_node_point(aes(size = 4, col = site_info$Region),
                  show.legend = FALSE) +
  ggtitle("Circle") +
  theme_graph() +
  theme(plot.title = element_text(size = rel(1)))

# Fruchcterman-Reingold layout
set.seed(4366)
fr_net <- ggraph(net, layout = "fr") +
  geom_edge_link(edge_color = "gray") +
  geom_node_point(aes(size = 4, col = site_info$Region),
                  show.legend = FALSE) +
  ggtitle("Fruchterman-Reingold") +
  theme_graph() +
  theme(plot.title = element_text(size = rel(1)))

# Davidsons and Harels annealing algorithm layout
set.seed(3467)
dh_net <- ggraph(net, layout = "dh") +
  geom_edge_link(edge_color = "gray") +
  geom_node_point(aes(size = 4, col = site_info$Region),
                  show.legend = FALSE) +
  ggtitle("Davidson-Harel") +
  theme_graph() +
  theme(plot.title = element_text(size = rel(1)))

library(ggpubr)
ggarrange(circ_net, fr_net, dh_net, nrow = 1)
```

<img src="05-visualization_files/figure-html/layouts-1.png" width="672" />

<div class="rmdnote">
<p>In the code above we used the <code>ggarrange</code> function within
the <code>ggpubr</code> package to combine the figures into a single
output. This function works with any <code>ggplot2</code> or
<code>ggraph</code> format output when you supply the names of each
figure in the order you want them to appear and the number of rows
<code>nrow</code> and number of columns <code>ncol</code> you want the
resulting combined figure to have. If you want to label each figure
using the <code>ggarrange</code> function you can use the
<code>labels</code> argument.</p>
</div>


### Node and Edge Options{#NodeEdgeOptions}

There are many options for altering color and symbol for nodes and edges within R. In this section we very briefly discuss some of the most common options. For more details see the discussion of [figures 6.10 through 6.16](#Figure_6_10) below.

#### Nodes {#NodeOptions}

In `ggraph` changing node options mostly consists of changing options within the `geom_node_point` call within the `ggraph` figure call. As we have already seen it is possible to set color for all nodes or by some variable, to change the size of points, and we can also scale points by some metric like centrality. Indeed, it is even possible to make the call to the centrality function in question directly within the figure code. 

When selecting point shapes you can use any of the shapes available in base R using `pch` point codes. Here are all of the available options:


```r
library(ggpubr)
ggpubr::show_point_shapes()
```

<img src="05-visualization_files/figure-html/pch_points-1.png" width="672" />

There are many options for selecting colors for nodes and edges. These can be assigned using standard color names or can be assigned using rgb or hex codes. It is also possible to use standard palettes in packages like `RColorBrewer` or `scales` to specify categorical or continuous color schemes. This is often done using either the `scale_fill_brewer` or `scale_color_brewer` calls from `RColorBrewer`. Here are a couple of examples. In these examples, colors are grouped by site region, node size is scaled to degree centrality, and node and edge color and shape are specified in each call. Note the `alpha` command which controls the transparency of the relevant part of the plot. The scale_size call specifies the maximum and minimum size of points in the plot.

The [R Graph Gallery](https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html) has a good overview of the available color palettes in `RColorBrewer` and when the can be used. The "Set2" palette used here is a good one for people with many kinds of color vision deficiencies. 


```r
library(RColorBrewer)

set.seed(347)
g1 <- ggraph(net, layout = "kk") +
  geom_edge_link(edge_color = "gray", alpha = 0.7) +
  geom_node_point(
    aes(fill = site_info$Region),
    shape = 21,
    size = igraph::degree(net) / 2,
    alpha = 0.5
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_graph() +
  theme(legend.position = "none")

set.seed(347)
g2 <- ggraph(net, layout = "kk") +
  geom_edge_link(edge_color = "blue", alpha = 0.3) +
  geom_node_point(
    aes(col = site_info$Region),
    shape = 15,
    size = igraph::degree(net) / 2,
    alpha = 1
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() +
  theme(legend.position = "none")

ggarrange(g1, g2, nrow = 1)
```

<img src="05-visualization_files/figure-html/color_brewer-1.png" width="672" />

There are also a number of more advanced methods for displaying nodes including displaying figures or other data visualizations in the place of nodes or using images for nodes. There are examples of each of these in the book and code outlining how to create such visuals in the discussions of [Figure 6.3](#Figure_6_3) and [Figure 6.13](#Figure_6_13) below. 

#### Edges{#EdgeOptions}

Edges can be modified in terms of color, line type, thickness and many other features just like nodes and this is typically done using the `geom_edge_link` call within `ggraph`. Let"s take a look at a couple of additional examples. In this case we"re going to use a weighted network object in the original [Peeples2018.Rdata](data/Peeples2018.Rdata) file to show how we can vary edges in relation to edge attributes like weight. 

In the example here we plot both the line thickness and transparency using the edge weights associated with the network object. We also are using the `scale_edge_color_gradient2` to specify a continuous edge color scheme with three anchors. For more details see `?scale_edge_color`


```r
library(intergraph)
net2 <- asIgraph(brnet_w)

set.seed(436)
ggraph(net2, "stress") +
  geom_edge_link(aes(width = weight, alpha = weight, col = weight)) +
  scale_edge_color_gradient2(
    low = "#440154FF",
    mid = "#238A8DFF",
    high = "#FDE725FF",
    midpoint = 0.8
  ) +
  scale_edge_width(range = c(1, 5)) +
  geom_node_point(size = 4, col = "blue") +
  labs(edge_color = "Edge Weight Color Scale") +
  theme_graph()
```

<img src="05-visualization_files/figure-html/edge_options1-1.png" width="672" />

Another feature of edges that is often important in visualizations is the presence or absence and type of arrows. Arrows can be modified in `ggraph` using the `arrow` argument within a `geom_edge_link` call. The most relevant options are the length of the arrow (which determines size), the `type` argument which specifies an open or closed arrow, and the spacing of the arrow which can be set by the `end_cap` and `start_cap` respectively which define the gap between the arrow point and the node. These values can all be set using absolute measurements as shown in the example below. Since this is an undirected network we use the argument `ends = "first"` to simulated a directed network so that arrowheads will only be drawn the first time an edge appears in the edge list. See `?arrow` for more details on options. 


```r
set.seed(436)
ggraph(net, "stress") +
  geom_edge_link(
    arrow = arrow(
      length = unit(2, "mm"),
      ends = "first",
      type = "closed"
    ),
    end_cap = circle(0, "mm"),
    start_cap = circle(3, "mm"),
    edge_colour = "black"
  ) +
  geom_node_point(size = 4, col = "blue") +
  theme_graph()
```

<img src="05-visualization_files/figure-html/edge_options2-1.png" width="672" />

Another common consideration with edges is the shape of the edges themselves. So far we have used examples where the edges are all straight lines, but it is also possible to draw them as arcs or so that they fan out from nodes so that multiple connections are visible. In general, all you need to do to change this option is to use another command in the `geom_edge_` family of commands. For example, in the following chunk of code we produce a network with arcs rather than straight lines. In this case the argument `strength` controls the amount of bend in the lines.


```r
set.seed(436)
ggraph(net, "kk") +
  geom_edge_arc(edge_colour = "black", strength = 0.1) +
  geom_node_point(size = 4, col = "blue") +
  theme_graph()
```

<img src="05-visualization_files/figure-html/edge_arc-1.png" width="672" />

It is also possible to not show edges at all but instead just a gradient scale representing the density of edges using the `geom_edge_density` call. This could be useful in very large and complex networks.


```r
set.seed(436)
ggraph(net2, "kk") +
  geom_edge_density() +
  geom_node_point(size = 4, col = "blue") +
  theme_graph()
```

<img src="05-visualization_files/figure-html/edge_density-1.png" width="672" />

<div class="rmdtip">
<p>If you want to see all of the possible options for
<code>geom_edge_</code> commands, simply use the help command on any one
of the functions (i.e., <code>?geom_edge_arc</code>) and scroll down in
the help window to the section labeled “See Also.”</p>
</div>

### Labels {#LabelOptions}

In many cases you may want to label either the nodes, edges, or other features of a network. This is relatively easy to do in `ggraph` with the `geom_node_text()` command. This will place labels as specified on each node. If you use the `repel = TRUE` argument it will repel the names slightly from the node to make them more readable. As shown in the example for [Figure 6.4](#Figure_6_4) it is also possible to filter labels to label only certain nodes.


```r
set.seed(436)
ggraph(net2, "fr") +
  geom_edge_link() +
  geom_node_point(size = 4, col = "blue") +
  geom_node_text(aes(label = vertex.names), size = 3, repel = TRUE) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/node_label-1.png" width="672" />

It is also possible to label edges by adding an argument directly into the `geom_edge_` command. In practice, this really only works with very small networks. In the next chunk of code, we create a small network and demonstrate this function.


```r
g <- graph(c("A", "B",
             "B", "C",
             "A", "C",
             "A", "A",
             "C", "B",
             "D", "C"))

E(g)$weight <- c(3, 1, 6, 8, 4, 2)

set.seed(4351)
ggraph(g, layout = "stress") +
  geom_edge_fan(aes(label = weight),
                angle_calc = "along",
                label_dodge = unit(2, "mm")) +
  geom_node_point(size = 20, col = "lightblue") +
  geom_node_text(label = V(g)$name) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/edge_label-1.png" width="672" />

### Be Kind to the Color Blind{#Colorblind}

When selecting your color schemes, it is important to consider the impact of a particular color scheme on color blind readers. There is an excellent set of R scripts on GitHub in a package called [colorblindr](https://github.com/clauswilke/colorblindr) by Claus Wilke which can help you do just that. I have slightly modified the code from the `colorblindr` package and created a script called [colorblindr.R](data/colorblindr.R) which you can download and use to test out your network. Simply run the code in the script and then use the `cvd_grid2()` function on a `ggplot` or `ggraph` object to see simulated colors.

The chunk of code below loads the `colorblindr.R` script and then plots a figure using `RColorBrewer` color `Set2` in its original unmodified format and then as it might look to readers with some of the most common forms of color vision issues. Download the [colorblindr.R script](scripts/colorblindr.R) to follow along.


```r
library(colorspace)
source("scripts/colorblindr.R")
cvd_grid2(g1)
```

<img src="05-visualization_files/figure-html/colorblind-1.png" width="672" />

### Communities and Groups{#VizCommunities}

Showing communities or other groups in network visualizations can be as simple as color coding nodes or edges as we have seen in many examples here. It is sometimes also useful to highlight groups by creating a convex hull or circle around the relevant points. This can be done in `ggraph` using the `geom_mark_hull` command within the `ggforce` package. You will also need a package called `concaveman` that allows you to set the concavity of the hulls around points.

The following chunk of code provides a simple example using the Louvain clustering algorithm.


```r
library(ggforce)
library(concaveman)

# Define clusters
grp <- as.factor(cluster_louvain(net2)$membership)

set.seed(4343)
ggraph(net2, layout = "fr") +
  geom_edge_link0(width = 0.2) +
  geom_node_point(aes(fill = grp),
                  shape = 21,
                  size = 5,
                  alpha = 0.75) +
  # Create hull around points within group and label
  geom_mark_hull(
    aes(
      x,
      y,
      group = grp,
      fill = grp,
    ),
    concavity = 4,
    expand = ggplot2::unit(2, "mm"),
    alpha = 0.25,
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_graph()
```

<img src="05-visualization_files/figure-html/ggforce-1.png" width="672" />

The discussion of [Figure 6.4](#Figure_6_4) below provides another similar example. There are many more complicated ways of showing network groups provided by the examples covering figures from the book. For example, [Figure 6.18](#Figure_6_18) provides an example of the "group-in-a-box" technique using the NodeXL software package. [Figure 6.19](#Figure_6_19) illustrates the use of matrices as visualization tools and [Figure 6.20](#Figure_6_20) provides links to the Nodetrix hybrid visualization software. 

## Replicating the Book Figures{#ReplicatingBookFigures}

In this section we go through each figure in Chapter 6 of Brughmans and Peeples (2023) and detail how the final graph was created for all figures that were created using R. For those figures not created in R we describe what software and data were used and provide additional resources where available. We hope these examples will serve as inspiration for your own network visualization experiments. Some of these figures are relatively simple while others are quite complex. They are presented in the order they appear in the book. 

### Figure 6.1: Manual Layout {- #Figure_6_1}

Figure 6.1. An example of an early hand drawn network graph (sociogram) published by Moreno (1932: 101). Moreno noted that the nodes at the top and bottom of the sociogram have the most connections and therefore represent the nodes of greatest importance. These specific “important” points are emphasized through both their size and their placement.

Note that the hand drawn version of this figure is presented in the book and this digital example is presented only for illustrative purposes. This shows how you can employ user defined layouts by directly supplying coordinates for the nodes in the plot. [Download the Moreno data to follow along]("data/Moreno.csv").


```r
library(igraph)
library(ggraph)

# Read in adjacency matrix of Moreno data and covert to network
moreno <-
  as.matrix(read.csv("data/Moreno.csv", header = TRUE, row.names = 1))
g_moreno <- graph_from_adjacency_matrix(moreno)

# Create xy coordinates associated with each node
xy <- matrix(
  c(4, 7, 1, 5, 6, 5, 2, 4, 3, 4, 5, 4, 1, 2.5, 6, 2.5, 4, 1),
  nrow = 9,
  ncol = 2,
  byrow = TRUE
)

# Plot the network using layout = "manual" to place nodes using xy coordinates
ggraph(g_moreno,
       layout = "manual",
       x = xy[, 1],
       y = xy[, 2]) +
  geom_edge_link() +
  geom_node_point(fill = "white",
                  shape = 21,
                  size = igraph::degree(g_moreno)) +
  scale_size(range = c(2, 3)) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/Fig6_1-1.png" width="192" />

### Figure 6.2: Examples of Common Network Plot Formats {- #Figure_6_2}

Figure. 6.2. These plots are all different visual representations of the same network data from Peeples’s (2018) data where edges are defined based on the technological similarities of cooking pots from each node which represent archaeological settlements. 

The code below creates each of the individual figures and then compiles them into a single composite figure for plotting. 

First read in the data ([all data are combined in a single RData file here](data/Peeples2018.Rdata)).


```r
library(igraph)
library(statnet)
library(intergraph)
library(ggplotify)
library(ggraph)
library(ggpubr)

load(file = "data/Peeples2018.Rdata")
## contains objects
# site_info - site locations and attributes
# ceramic_br - raw Brainerd-Robinson similarity among sites
# brnet - binary network with similarity values > 0.65
#     defined as edges in statnet/network format
# brnet_w - weighted network with edges (>0.65) given weight
#     values based on BR similarity in statnet/network format
##
```

Fig 6.2a - A simple network graph with nodes placed based on the Fruchterman-Reingold algorithm


```r
## create simple graph with Fruchterman - Reingold layout
set.seed(423)
f6_2a <- ggraph(brnet, "fr") +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(aes(size = 5), col = "red", show.legend = FALSE) +
  theme_graph()
f6_2a
```

<img src="05-visualization_files/figure-html/Fig6_2a-1.png" width="672" />

Fig 6.2b - Network graph nodes with placed based on the real geographic locations of settlements and are color coded based on sub-regions.


```r
## create graph with layout determined by site location and
## nodes color coded by region
f6_2b <- ggraph(brnet, "manual",
                x = site_info$x,
                y = site_info$y) +
  geom_edge_link(edge_colour = "grey66") +
  geom_node_point(aes(size = 2, col = site_info$Region),
                  show.legend = FALSE) +
  theme_graph()
f6_2b
```

<img src="05-visualization_files/figure-html/Fig6_2b-1.png" width="672" />

Fig 6.2c - A graph designed to show how many different kinds of information can be combined in a single network plot. In this network graph node placement is defined by the stress majorization algorithm (see below), with nodes color coded based on region, with different symbols for different kinds of public architectural features found at those sites, and with nodes scaled based on betweenness centrality scores. The line weight of each edge is used to indicate relative tie-strength.


```r
# create vectors of attributes and betweenness centrality and plot
# network with nodes color coded by region, sized by betweenness,
# with symbols representing public architectural features, and
# with edges weighted by BR similarity
col1 <- as.factor((site_info$Great.Kiva))
col2 <- as.factor((site_info$Region))
bw <- sna::betweenness(brnet_w)

f6_2c <- ggraph(brnet_w, "stress") +
  geom_edge_link(aes(width = weight, alpha = weight),
                 edge_colour = "black",
                 show.legend = FALSE) +
  scale_edge_width(range = c(1, 2)) +
  geom_node_point(aes(
    size = bw,
    shape = col1,
    fill = col1,
    col = site_info$Region
  ),
  show.legend = FALSE) +
  scale_fill_discrete() +
  scale_size(range = c(4, 12)) +
  theme_graph()
f6_2c
```

<img src="05-visualization_files/figure-html/Fig6_2c-1.png" width="672" />

Fig. 6.2d - This network graph is laid out using the Kamada-Kawai force directed algorithm with nodes color coded based on communities detected using the Louvain community detection algorithm. Each community is also indicated by a circle highlighting the relevant nodes. Edges within communities are shown in black and edges between communities are shown in red.

In this plot we use the `as.ggplot` function to convert a traditional `igraph` plot to a `ggraph` plot to illustrate how this can be done.


```r
# convert network object to igraph object and calculate Louvain
# cluster membership plot and convert to grob to combine in ggplot
g <- asIgraph(brnet_w)
clst <- cluster_louvain(g)

f6_2d <- as.ggplot(
  ~ plot(
    clst,
    g,
    layout = layout_with_kk,
    vertex.label = NA,
    vertex.size = 10,
    col = rainbow(4)[clst$membership]
  )
)
f6_2d
```

<img src="05-visualization_files/figure-html/Fig6_2d-1.png" width="672" />

Finally, we use the `ggarrange` function from the `ggpubr` package to combine all of these plots into a single composite plot.


```r
# Combine all plots into a single figure using ggarrange
figure_6_2 <- ggarrange(
  f6_2a,
  f6_2b,
  f6_2c,
  f6_2d,
  nrow = 2,
  ncol = 2,
  labels = c("(a)", "(b)", "(c)", "(d)"),
  font.label = list(size = 22)
)

figure_6_2
```

<img src="05-visualization_files/figure-html/Fig6_2_all-1.png" width="672" />

### Figure 6.3: Examples of Rare Network Plot Formats {- #Figure_6_3}

Figure 6.3. Examples of less common network visuals techniques for Peeples’s (2018) ceramic technological similarity data. 

Fig 6.3a - A weighted heat plot of the underlying similarity matrix with hierarchical clusters shown on each axis. This plot relies on a packages called `superheat` that produces plots formatted as we see here. The required input is a symmetric similarity matrix object. 

<div class="rmdnote">
<p>In the chunk of code below we use the <code>as.ggplot</code> function
from the <code>ggplotify</code> package. This function converts a non
<code>ggplot2</code> style function into a <code>ggplot2</code> format
so that it can be further used with packages like <code>ggpubr</code>
and <code>colorblindr</code>.</p>
</div>



```r
library(igraph)
library(statnet)
library(intergraph)
library(ggraph)
library(ggplotify)
library(superheat)

ceramic_br_a <- ceramic_br
diag(ceramic_br_a) <- NA

f6_3a <- as.ggplot(
  ~ superheat(
    ceramic_br_a,
    row.dendrogram = TRUE,
    col.dendrogram = TRUE,
    grid.hline.col = "white",
    grid.vline.col = "white",
    legend = FALSE,
    left.label.size = 0,
    bottom.label.size = 0
  )
)
f6_3a
```

<img src="05-visualization_files/figure-html/Fig6_3a-1.png" width="672" />

Fig. 6.3b - An arcplot with within group ties shown above the plot and between group ties shown below.

For this plot, we read in a adjacency matrix that is ordered in the order we want it to show up in the final plot. [Download the file here](data/Peeples_arcplot.csv) to follow along. Note that the object `grp` must be produced in the same order that the nodes appear in the original adjacency matrix file.



```r
arc_dat <- read.csv("data/Peeples_arcplot.csv",
                    header = TRUE,
                    row.names = 1)
g <- graph_from_adjacency_matrix(as.matrix(t(arc_dat)))

# set groups for color
grp <- as.factor(c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                   3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1))


# Make the graph
f6_3b <- ggraph(g, layout = "linear") +
  geom_edge_arc(
    edge_colour = "black",
    edge_alpha = 0.2,
    edge_width = 0.7,
    fold = FALSE,
    strength = 1,
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(
      size = igraph::degree(g),
      color = grp,
      fill = grp
    ),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_size_continuous(range = c(4, 8)) +
  theme_graph()
f6_3b
```

<img src="05-visualization_files/figure-html/Fig6_3b-1.png" width="672" />

Fig. 6.3c - Network plot with sites in geographic locations and edges bundled using the edge bundling hammer routine.

<div class="rmdwarning">
<p>This function requires the <code>edgebundle</code> package be
installed along with <code>reticulate</code> and Python 3.8 (see <a
href="#Packages">Packages</a>) and uses the <a
href="data/Peeples2018.Rdata">Cibola technological similarity data</a>.
Check <a href="#ShouldIInstall">Data and Workspace Setup</a> section for
more details on getting the edge bundling package and Python up and
running.</p>
<p>Be aware that this function may take a long time on your computer
depending on your processing power and RAM.</p>
</div>




```r
library(edgebundle)
load("data/Peeples2018.Rdata")

# Create attribute file with required data
xy <- as.data.frame(site_info[, 1:2])
xy <- cbind(xy, site_info$Region)
colnames(xy) <- c("x", "y", "Region")

# Run hammer bundling routine
g <- asIgraph(brnet)
hbundle <- edge_bundle_hammer(g, xy, bw = 5, decay = 0.3)

f6_3c <-   ggplot() +
  geom_path(data = hbundle, aes(x, y, group = group),
            col = "gray66", size = 0.5) +
  geom_point(data = xy, aes(x, y, col = Region),
             size = 5, alpha = 0.75, show.legend = FALSE) +
  theme_void()
f6_3c
```

<img src="05-visualization_files/figure-html/Fig6_3c-1.png" width="672" />

Fig. 6.3d - Network graph where nodes are replaced by waffle plots that show relative frequencies of the most common ceramic technological clusters.

This is a somewhat complicated plot that requires a couple of specialized libraries and additional steps along the way. We provide comments in the code below to help you follow along. Essentially the routine creates a series of waffle plots and then uses them as annotations to replace the nodes in the final `ggraph`. This plot requires that you install a development package called `ggwaffle`. Run the line of code below before creating the figure if you need to add this package. 


```r
devtools::install_github("liamgilbey/ggwaffle")
```

<div class="rmdtip">
<p>There are numerous projects that are in the R CRAN archive and those
packages have been peer reviewed and evaluated. There are many other
packages and compendiums designed for use in R that are not yet in the
CRAN archive. Frequently these are found as packages in development on
GitHub. In order to use these packages in development, you can use the
<code>install_github</code> function wrapped inside the
<code>devtools</code> package (though it originates in the
<code>remotes</code> package). In order to install a package from
GitHub, you type supply “username/packagename” inside the
<code>install_github</code> call.</p>
</div>

Let's now look at the figure code:


```r
# Initialize libraries

library(ggwaffle)
library(tidyverse)

# Create igraph object from data imported above
cibola_adj <-
  read.csv(file = "data/Cibola_adj.csv",
           header = TRUE,
           row.names = 1)
g <- graph_from_adjacency_matrix(as.matrix(cibola_adj),
                                 mode = "undirected")

# Import raw ceramic data and convert to proportions
ceramic_clust <- read.csv(file = "data/Cibola_clust.csv",
                          header = TRUE,
                          row.names = 1)
ceramic_p <- prop.table(as.matrix(ceramic_clust), margin = 1)

# Assign vertex attributes to the network object g which represent
# columns in the ceramic.p table
V(g)$c1 <- ceramic_p[, 1]
V(g)$c2 <- ceramic_p[, 2]
V(g)$c3 <- ceramic_p[, 3]
V(g)$c4 <- ceramic_p[, 4]
V(g)$c5 <- ceramic_p[, 5]
V(g)$c6 <- ceramic_p[, 6]
V(g)$c7 <- ceramic_p[, 7]
V(g)$c8 <- ceramic_p[, 8]
V(g)$c9 <- ceramic_p[, 9]
V(g)$c10 <- ceramic_p[, 10]

# Precompute the layout and assign coordinates as x and y in network g
set.seed(345434534)
xy <- layout_with_fr(g)
V(g)$x <- xy[, 1]
V(g)$y <- xy[, 2]

# Create a data frame that contains the 4 most common
# categories in the ceramic table, the node id, and the proportion
# of that ceramic category at that node
nodes_wide <- igraph::as_data_frame(g, "vertices")
nodes_long <- nodes_wide %>%
  dplyr::select(c1:c4) %>%
  mutate(id = seq_len(nrow(nodes_wide))) %>%
  gather("attr", "value", c1:c4)
nodes_out <- NULL
for (j in seq_len(nrow(nodes_long))) {
  temp <- do.call("rbind", replicate(round(nodes_long[j, ]$value * 50, 0),
                                     nodes_long[j, ], simplify = FALSE))
  nodes_out <- rbind(nodes_out, temp)
}

# Create a list object for the call to each bar chart by node
bar_list <- lapply(1:vcount(g), function(i) {
  gt_plot <- ggplotGrob(
    ggplot(waffle_iron(nodes_out[nodes_out$id == i, ],
                       aes_d(group = attr))) +
      geom_waffle(aes(x, y, fill = group), size = 10) +
      coord_equal() +
      labs(x = NULL, y = NULL) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = NA),
        line = element_blank(),
        text = element_blank()
      )
  )
  panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel", ]
  gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
})

# Convert the results above into custom annotation
annot_list <- lapply(1:vcount(g), function(i) {
  xmin <- nodes_wide$x[i] - .25
  xmax <- nodes_wide$x[i] + .25
  ymin <- nodes_wide$y[i] - .25
  ymax <- nodes_wide$y[i] + .25
  annotation_custom(
    bar_list[[i]],
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )
})

# create basic network
p <- ggraph(g, "manual", x = V(g)$x, y = V(g)$y) +
  geom_edge_link0() +
  theme_graph() +
  coord_fixed()

# put everything together by combining with the annotation (bar plots + network)
f6_3d <- Reduce("+", annot_list, p)
f6_3d
```

<img src="05-visualization_files/figure-html/Fig6_3d-1.png" width="960" />

<div class="rmdtip">
<p>The inspiration for the example above came from a <a
href="https://www.r-bloggers.com/2020/03/ggraph-tricks-for-common-problems/">R
blogpost by schochastics (David Schoch)</a>. As that post shows, any
figures that can be treated as <code>ggplot2</code> objects can be used
in the place of nodes by defining them as “annotations.” See the post
for more details.</p>
</div>

Now let's look at all of the figures together.

![](images/Figure_6_3.jpg){width=100%}

### Figure 6.4: Simple Network with Clusters {- #Figure_6_4}

Figure 6.4. A network among Clovis era sites in the Western U.S. with connections based on shared lithic raw material sources. Nodes are scaled based on betweenness centrality with the top seven sites labelled. Color-coded clusters were defined using the Louvain algorithm. 
 
<div class="rmdtip">
<p>This example shows how to define and indicate groups and label points
based on their values. Note the use of the <code>ifelse</code> call in
the <code>geom_node_text</code> portion of the plot. See <a
href="#Conditionals">here</a> for more information on how
<code>ifelse</code> statements work.</p>
</div>


```r
library(ggforce)
library(ggraph)
library(statnet)
library(igraph)

clovis <- read.csv("data/Clovis.csv", header = TRUE, row.names = 1)
colnames(clovis) <- row.names(clovis)
graph <- graph_from_adjacency_matrix(as.matrix(clovis),
                                     mode = "undirected",
                                     diag = FALSE)

bw <- igraph::betweenness(graph)

grp <- as.factor(cluster_louvain(graph)$membership)

set.seed(43643548)
ggraph(graph, layout = "fr") +
  geom_edge_link(edge_width = 1, color = "gray") +
  geom_node_point(aes(fill = grp, size = bw, color = grp),
                  shape = 21,
                  alpha = 0.75) +
  scale_size(range = c(2, 20)) +
  geom_mark_hull(
    aes(
      x,
      y,
      group = grp,
      fill = grp,
      color = NA
    ),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25,
    label.fontsize = 12
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3),
                                     rgb(0, 0, 0, 1))) +
  # If else statement only labels points that meet the condition
  geom_node_text(aes(label = ifelse(bw > 40,
                                    as.character(name),
                                    NA_character_)),
                 size = 4) +
  theme_graph() +
  theme(legend.position = "none")
```

<img src="05-visualization_files/figure-html/Fig6_4-1.png" width="672" />

### Figure 6.5: Interactive Layout {- #Figure_6_5}

Figure 6.5. An example of the same network graph with two simple user defined layouts created interactively.

Figure 6.5 was produced in [NetDraw](https://sites.google.com/site/netdrawsoftware/download) by creating a simple network and taking screen shots of two configurations of nodes. There are a few options for creating a similar figures in R. The simplest is to use an igraph network object and the `tkplot` function. This function brings up a window that lets you drag and move nodes (with or without an initial algorithmic layout) and when you"re done you can assign the new positions to a variable to use for plotting. [Use these data](data/Peeples2018.Rdata) to follow along.

<div class="rmdwarning">
<p>Note that if you are running this package in your browser via binder,
the function below will not work as you do not have permission to open
the tkplot on the virtual server. To follow along with the plotting of
this figure you can use the pre-determined locations by reading in this
file <code>load(file="data/Coords.Rdata")</code></p>
</div>



```r
library(igraph)
library(intergraph)

load("data/Peeples2018.Rdata")

cibola_i <- asIgraph(brnet)

locs <- tkplot(cibola_i)
coords <- tkplot.getcoords(locs)
```

This will bring up a window like the example below and when you click "Close" it will automatically create the variables with the node location information for plotting. 

![](images/interactive.jpg){width=60%}



```r
plot(cibola_i, layout = coords)
```

<img src="05-visualization_files/figure-html/Fig6_5c-1.png" width="672" />

### Figure 6.6: Absolute Geographic Layout {- #Figure_6_6}

Fig. 6.6. Map of major Roman roads and major settlements on the Iberian Peninsula, (a) with roads mapped along their actual geographic paths and (b) roads shown as simple line segments between nodes. 

The figure that appears in the book was originally created using GIS software but it is possible to prepare a quite similar figure in R using the tools we outlined above. To reproduce the results presented here you will need to download [the node information file](data/Hispania_nodes.csv) and the [road edge list](data/Hispania_roads.csv). We have created a script called [map_net.R](scripts/map_net.R) which will produce similar maps when supplied with a network object and a file with node locations in lat/long coordinates. For more information on how R works with geographic data see the [spatial networks](#SpatialNetworks) section of this document.


```r
library(igraph)
library(ggmap)
library(sf)

# Load my_map background map
load("data/Figure6_6.Rdata")

edges1 <- read.csv("data/Hispania_roads.csv", header = TRUE)
edges1 <- edges1[which(edges1$Weight > 25), ]
nodes <- read.csv("data/Hispania_nodes.csv", header = TRUE)
nodes <- nodes[which(nodes$Id %in% c(edges1$Source, edges1$Target)), ]

road_net <-
  graph_from_edgelist(as.matrix(edges1[, 1:2]), directed = FALSE)

# Convert attribute location data to sf coordinates
locations_sf <-
  st_as_sf(nodes, coords = c("long", "lat"), crs = 4326)
coord1 <- do.call(rbind, st_geometry(locations_sf)) %>%
  tibble::as_tibble() %>%
  setNames(c("lon", "lat"))

xy <- as.data.frame(coord1)
colnames(xy) <- c("x", "y")

# Extract edge list from network object
edgelist <- get.edgelist(road_net)

# Create data frame of beginning and ending points of edges
edges <- as.data.frame(matrix(NA, nrow(edgelist), 4))
colnames(edges) <- c("X1", "Y1", "X2", "Y2")
for (i in seq_len(nrow(edgelist))) {
  edges[i, ] <- c(nodes[which(nodes$Id == edgelist[i, 1]), 3],
                  nodes[which(nodes$Id == edgelist[i, 1]), 2],
                  nodes[which(nodes$Id == edgelist[i, 2]), 3],
                  nodes[which(nodes$Id == edgelist[i, 2]), 2])
}


ggmap(my_map) +
  geom_segment(
    data = edges,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2
    ),
    col = "black",
    size = 1
  ) +
  geom_point(
    data = xy,
    aes(x, y),
    alpha = 0.8,
    col = "black",
    fill = "white",
    shape = 21,
    size = 1.5,
    show.legend = FALSE
  ) +
  theme_void()
```

<img src="05-visualization_files/figure-html/Fig6_6-1.png" width="672" />

### Figure 6.7: Distorted Geographic Layout {- #Figure_6_7}

Figure 6.7. This ceramic similarity network of the San Pedro River Valley in Arizona shows the challenges of creating geographic network layouts. (a) Shows sites in their original locations whereas (b) shifts locations to improve the visibility of network structure. Note how the distorted geographic layout retains the basic relationships among the nodes while altering their locations slightly.

Unfortunately as the first map contains real site locations we cannot share those data here. The second map can still be reproduced given nothing but the code below. The only difference required to produce Figure 6.7a would be to replace the `coord` site coordinates with the actual site locations. the `coord` object used here was created by taking the original site locations and applying the `jitter` function, which jitters x and y coordinates by a specified amount.



```r
library(igraph)
library(sf)
library(ggmap)
library(ggrepel)
library(ggpubr)

load("data/Figure6_7.Rdata")
# g.net - igraph network object of San Pedro sites based on
# ceramic similarity
# base3 - basemap background terrain

# Define coordinates of "jittered" points
# These points were originally created using the "jitter" function
# until a reasonable set of points were found.
coord <- c(-110.7985, 32.97888,
-110.7472, 32.89950,
-110.6965, 32.83496,
-110.6899, 32.91499,
-110.5508, 32.72260,
-110.4752, 32.60533,
-110.3367, 32.33341,
-110.5930, 32.43487,
-110.8160, 32.86185,
-110.6650, 32.64882,
-110.4558, 32.56866,
-110.6879, 32.60055,
-110.7428, 32.93124,
-110.4173, 32.34401,
-110.7000, 32.73344)

attr <- c("Swingle's Sample", "Ash Terrace", "Lost Mound",
          "Dudleyville Mound", "Leaverton", "High Mesa",
          "Elliott Site", "Bayless Ruin", "Flieger",
          "Big Bell", "111 Ranch", "Twin Hawks", "Artifact Hill",
          "Jose Solas Ruin", "Wright")


# Convert coordinates to data frame
zz <- as.data.frame(matrix(coord, nrow = 15, byrow = TRUE))
colnames(zz) <- c("x", "y")


# Extract edge list from network object
edgelist <- get.edgelist(g.net)

# Create data frame of beginning and ending points of edges
edges2 <- data.frame(zz[edgelist[, 1], ], zz[edgelist[, 2], ])
colnames(edges2) <- c("X1", "Y1", "X2", "Y2")

# Plot jittered coordinates on map
figure_6_7 <- ggmap(base3, darken = 0.35) +
  geom_segment(
    data = edges2,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2
    ),
    col = "white",
    size = 1
  ) +
  geom_point(
    data = zz,
    aes(x, y),
    alpha = 0.8,
    col = "red",
    size = 5,
    show.legend = FALSE
  ) +
  geom_text_repel(aes(x = x, y = y, label = attr), data = zz, size = 3) +
  theme_void()

figure_6_7
```

<img src="05-visualization_files/figure-html/unnamed-chunk-15-1.png" width="672" />

### Figure 6.8: Graph Layout Algorithms {- #Figure_6_8}

Fig. 6.8. Several different graph layouts all using the Bronze Age Aegean geographic network (Evans et al. 2011). In each graph, nodes are scaled based on betweenness centrality and color-coded based on clusters defined using modularity maximisation.

In the code below the only thing we change between each plot is the `layout` argument in `ggraph`. See [the CRAN project page on ggraph](https://cran.r-project.org/web/packages/ggraph/vignettes/Layouts.html) for more information on available layouts. We plot clusters by color here to make it easier to track differences between the layout options. [Use these data](data/aegean.Rdata) to dowload the background map of the Aegean area.


```r
library(igraph)
library(ggraph)
library(ggpubr)
library(igraphdata)
library(graphlayouts)
library(sf)
library(ggmap)

# Load igraph Aegean_net data

aegean <- read.csv("data/aegean.csv", row.names = 1, header = T)
aegean_dist <- aegean
aegean_dist[aegean_dist > 124] <- 0
aegean_dist[aegean_dist > 0] <- 1
aegean_net <- graph_from_adjacency_matrix(as.matrix(aegean_dist))
load("data/aegean_map.Rdata")

# Define cluster membership and betweenness centrality for plotting
grp <- as.factor(cluster_optimal(aegean_net)$membership)
bw <- as.numeric(igraph::betweenness(aegean_net))

# Create geographic network and plot
nodes <- read.csv("data/aegean_locs.csv")

# Convert attribute location data to sf coordinates
locations_sf <-
  st_as_sf(nodes,
           coords = c("Longitude", "Latitude"),
           crs = 4326)
coord1 <- do.call(rbind, st_geometry(locations_sf)) %>%
  tibble::as_tibble() %>%
  setNames(c("lon", "lat"))

xy <- as.data.frame(coord1)
colnames(xy) <- c("x", "y")


# Extract edge list from network object for road_net
edgelist1 <- get.edgelist(aegean_net)

# Create data frame of beginning and ending points of edges
edges1 <- as.data.frame(matrix(NA, nrow(edgelist1), 4))
colnames(edges1) <- c("X1", "Y1", "X2", "Y2")
for (i in seq_len(nrow(edgelist1))) {
  edges1[i, ] <-
    c(nodes[which(nodes$Name == edgelist1[i, 1]), ]$Longitude,
      nodes[which(nodes$Name == edgelist1[i, 1]), ]$Latitude,
      nodes[which(nodes$Name == edgelist1[i, 2]), ]$Longitude,
      nodes[which(nodes$Name == edgelist1[i, 2]), ]$Latitude)
}

geo_net <- ggmap(my_map) +
  geom_segment(
    data = edges1,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2
    ),
    col = "black",
    size = 1
  ) +
  geom_point(
    data = xy,
    aes(x, y, size = bw, fill = grp),
    alpha = 0.8,
    shape = 21,
    show.legend = FALSE
  ) +
  scale_size(range = c(4, 12)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_graph() +
  ggtitle("Geographic") +
  theme(plot.title = element_text(size = rel(1)))

# Multidimensional Scaling Layout with color by cluster and node
# size by betweenness
set.seed(435353)
g_mds <- ggraph(aegean_net, layout = "mds") +
  geom_edge_link0(width = 0.2) +
  geom_node_point(aes(fill = grp, size = bw),
                  shape = 21,
                  show.legend = FALSE) +
  scale_size(range = c(4, 12)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3),
                                     rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(plot.title = element_text(size = rel(1))) +
  ggtitle("Multi-Dimensional Scaling") +
  theme(legend.position = "none")

# Fruchterman-Reingold Layout with color by cluster and node size
# by betweenness
set.seed(435353)
g_fr <- ggraph(aegean_net, layout = "fr") +
  geom_edge_link0(width = 0.2) +
  geom_node_point(aes(fill = grp, size = bw),
                  shape = 21,
                  show.legend = FALSE) +
  scale_size(range = c(4, 12)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3),
                                     rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(plot.title = element_text(size = rel(1))) +
  ggtitle("Fruchterman-Reingold") +
  theme(legend.position = "none")

# Kamada-Kawai Layout with color by cluster and node size by betweenness
set.seed(435353)
g_kk <- ggraph(aegean_net, layout = "kk") +
  geom_edge_link0(width = 0.2) +
  geom_node_point(aes(fill = grp, size = bw),
                  shape = 21,
                  show.legend = FALSE) +
  scale_size(range = c(4, 12)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3),
                                     rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(plot.title = element_text(size = rel(1))) +
  ggtitle("Kamada-Kawai") +
  theme(legend.position = "none")

# Radial Centrality Layout with color by cluster and node size by
# betweenness
set.seed(435353)
g_cent <- ggraph(aegean_net,
                 layout = "centrality",
                 centrality = igraph::betweenness(aegean_net)) +
  geom_edge_link0(width = 0.2) +
  geom_node_point(aes(fill = grp, size = bw),
                  shape = 21,
                  show.legend = FALSE) +
  scale_size(range = c(4, 12)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3),
                                     rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(plot.title = element_text(size = rel(1))) +
  ggtitle("Radial Centrality") +
  theme(legend.position = "none")

# Spectral Layout with color by cluster and node size by betweenness
u1 <- layout_with_eigen(aegean_net)
g_spec <- ggraph(aegean_net,
                 layout = "manual",
                 x = u1[, 1],
                 y = u1[, 2]) +
  geom_edge_link0(width = 0.2) +
  geom_node_point(aes(fill = grp, size = bw),
                  shape = 21,
                  show.legend = FALSE) +
  scale_size(range = c(4, 12)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3),
                                     rgb(0, 0, 0, 1))) +

  theme_graph() +
  theme(plot.title = element_text(size = rel(1))) +
  ggtitle("Spectral") +
  theme(legend.position = "none")


figure_6_8 <-
  ggarrange(geo_net,
            g_mds,
            g_fr,
            g_kk,
            g_cent,
            g_spec,
            ncol = 2,
            nrow = 3)
figure_6_8
```

<img src="05-visualization_files/figure-html/Fig6_8-1.png" width="672" />

### Figure 6.9: Heirarchical Graph Layouts {- #Figure_6_9}

Fig. 6.9. Examples of visualisations based on hierarchical graph data. (a) Graph with nodes color-coded by hierarchical level. (b) Bubble plot where nodes are scaled proportional to the sub-group size. (c) Dendrogram of hierarchical cluster data. (d) Radial graph with edges bundled based on similarity in relations. Edges are colour-coded such that they are red at the origin and purple at the destination to help visualise direction.

These graphs are based on a hierarchical graph that was created by assigning nodes to the leaves of a hierarchical cluster analysis performed on the Cibola ceramic technological cluster data. The data for 6.9d were randomly generated following an example on the R Graph Gallery. [Use these data](data/Figure6_9.Rdata) to follow along.


```r
# initialize libraries
library(igraph)
library(ggraph)
library(ape)
library(RColorBrewer)
library(ggpubr)

load(file = "data/Figure6_9.Rdata")

set.seed(4353543)
h1 <- ggraph(h_graph, "circlepack") +
  geom_edge_link() +
  geom_node_point(aes(colour = depth, size = (max(depth) - depth) / 2),
                  show.legend = FALSE) +
  scale_color_viridis() +
  theme_graph() +
  coord_fixed()

set.seed(643346463)
h2 <- ggraph(h_graph, "circlepack") +
  geom_node_circle(aes(fill = depth),
                   size = 0.25,
                   n = 50,
                   show.legend = FALSE) +
  scale_fill_viridis() +
  theme_graph() +
  coord_fixed()

h3 <- ggraph(h_graph, "dendrogram") +
  geom_node_point(aes(filter = leaf),
                  color = "blue",
                  alpha = 0.7,
                  size = 3) +
  theme_graph() +
  geom_edge_link()

h4 <-
  ggraph(sub_grp_graph, layout = "dendrogram", circular = TRUE) +
  geom_conn_bundle(
    data = get_con(from = from, to = to),
    alpha = 0.2,
    width = 0.9,
    tension = 0.9,
    aes(colour = ..index..)
  ) +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_point(aes(
    filter = leaf,
    x = x * 1.05,
    y = y * 1.05,
    colour = group),
    size = 3) +
  scale_colour_manual(values = rep(brewer.pal(9, "Paired"), 30)) +
  theme_graph() +
  theme(legend.position = "none")

figure_6_9 <- ggarrange(
  h1,
  h2,
  h3,
  h4,
  ncol = 2,
  nrow = 2,
  labels = c("(a)", "(b)", "(c)", "(d)")
)
figure_6_9
```

<img src="05-visualization_files/figure-html/Fig6_9-1.png" width="672" />

### Figure 6.10: Be kind to the color blind {- #Figure_6_10}

Fig. 6.10. Examples of a simple network graph with color-coded clusters. The top left example shows the unmodified figure and the remaining examples simulate what such a figure might look like to people with various kinds of colour vision deficiencies. 

This function calls a script that we modified from the `colorblindr` package by [Claus Wilke](https://github.com/clauswilke/colorblindr) which is available here. The function `cv2_grid` take any ggplot object and outputs a 2 x 2 grid with the original figure and examples of what the figure might look like to people with three of the most common forms of color vision deficiency. Use [these data](data/Peeples2018) and [this script](scripts/colorblindr.R) to follow along.



```r
library(igraph)
library(statnet)
library(intergraph)
library(ggraph)
library(RColorBrewer)
library(colorspace)
source("scripts/colorblindr.R")

load("data/Peeples2018.Rdata")

# Create igraph object for plots below
net <- asIgraph(brnet)

set.seed(347)
g1 <- ggraph(net, layout = "kk") +
  geom_edge_link(edge_color = "gray", alpha = 0.7) +
  geom_node_point(
    aes(fill = site_info$Region),
    shape = 21,
    size = igraph::degree(net) / 2,
    alpha = 0.5
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_graph() +
  theme(legend.position = "none")

cvd_grid2(g1)
```

<img src="05-visualization_files/figure-html/Fig6_10-1.png" width="672" />


### Figure 6.11: Node Symbol and Color Schemes {- #Figure_6_11}

Fig. 6.11. Examples of different node color and symbol schemes. Note how adding color and size eases the identification of particular values, in particular with closely spaced points. Using transparency can similarly aid in showing multiple overlapping nodes.

The version that appears in the book was compiled and labeled in Adobe Illustrator using the output created here.


```r
library(scales)

plot(
  x = 1:5,
  y = rep(2, 5),
  pch = 16,
  cex = seq(5:10),
  col = "blue",
  ylim = c(0, 4),
  bty = "n",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
points(
  x = 1:5,
  y = rep(1.5, 5),
  pch = 21,
  cex = seq(5:10),
  bg = heat.colors(5, rev = TRUE)
)
points(
  x = 1:5,
  y = rep(1, 5),
  pch = c(1, 2, 3, 4, 5),
  cex = seq(5:10),
  bg = "skyblue",
  col = "blue",
  lwd = 2
)
```

<img src="05-visualization_files/figure-html/Fig6_11-1.png" width="672" />

```r
set.seed(34456)
x <- rnorm(15, 1, 0.5)
y <- rnorm(15, 1, 0.5)
xy <- cbind(x, y)
xy2 <- cbind(x + 5, y)
xy3 <- cbind(x + 10, y)
xy4 <- cbind(x + 15, y)
xy5 <- cbind(x + 20, y)

size <- sample(c(5, 6, 7, 8, 9), size = 15, replace = TRUE)
size <- size - 4

h_col <- heat.colors(5, rev = TRUE)

plot(
  xy[order(size, decreasing = TRUE), ],
  pch = 16,
  col = "blue",
  cex = size[order(size, decreasing = TRUE)],
  xlim = c(0, 22),
  ylim = c(-1, 3),
  bty = "n",
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = ""
)
points(xy2[order(size, decreasing = TRUE), ],
       pch = 21,
       bg = h_col[size[order(size, decreasing = TRUE)]],
       cex = size[order(size, decreasing = TRUE)])
points(xy3[order(size, decreasing = TRUE), ],
       pch = size[order(size, decreasing = TRUE)],
       col = "blue",
       cex = size[order(size, decreasing = TRUE)])
points(
  xy4[order(size, decreasing = TRUE), ],
  pch = 21,
  col = "gray66",
  bg = alpha("blue", 0.7),
  cex = size[order(size, decreasing = TRUE)]
)
points(xy5[order(size, decreasing = TRUE), ],
       pch = 21,
       bg = alpha(h_col[size[order(size, decreasing = TRUE)]], 0.7),
       cex = size[order(size, decreasing = TRUE)])
```

<img src="05-visualization_files/figure-html/Fig6_11-2.png" width="672" />


### Figure 6.12: Image for Node {- #Figure_6_12}

Fig. 6.12. Network graph showing similarity among carved faces from Banés, Holguín province, Cuba. Nodes are depicted as the objects in question themselves and edges represent shared attributes with numbers indicating the number of shared attributes for each pair of faces.

Figure 6.12 was used with permission by Angus Mol and the original was produced for his 2014 book. 

![](images/Fig6_12.jpg){width=100%}

### Figure 6.13: Images for Nodes {- #Figure_6_13}

Fig. 6.13. Two-mode network of ceramics and sites in the San Pedro Valley with ceramic ware categories represented by a graphic example of each type.

The version of Figure 6.13 in the Brughmans and Peeples (2023) book was originally created in NetDraw and modified to add the node pictures in Adobe Photoshop. This approach was preferred as it produced higher resolution and more consistent images than the graphics we could produce directly in R for this particular feature. It is, however, possible to use images in the place of nodes in R networks as the example below illustrates. 

We have found in practice that this feature in R works best for simple icons. If you are using high resolution images or lots of color or detail in your images it works better to create an initial image format in something like R or NetDraw and then to modify the network in a graphical editing software after the fact. 

In the place of the example in the book, we here demonstrate how you can use image files with R to create nodes as pictures. You can [download the data](data/Figure6_13.Rdata) to follow along. This .RData file also includes the images used here in R format and the code used to read in .png images is shown below but commented out.


```r
library(png)
library(igraph)

load("data/Figure6_13.Rdata")
# two_mode_net - igraph two mode network object

# Set Vector property to images by mode
# Note that if you want to set a different image
# for each node you can simply create a long list
# containing image names for node type 1 followed
# by image names for node type 2.
V(two_mode_net)$raster <- list(img.1, img.2)[V(two_mode_net)$type + 1]

set.seed(34673)
plot(
  two_mode_net,
  vertex.shape = "raster",
  vertex.label = NA,
  vertex.size = 16,
  vertex.size2 = 16,
  edge.color = "gray"
)
```

<img src="05-visualization_files/figure-html/Fig6_13-1.png" width="672" />

If you want to use images in a one mode network you can follow the sample below using [these data](data/Cibola_adj.csv). Note that in the line with `V(Cibola_i)$raster` you can either assign a single image or an image for each node in the network.


```r
library(png)
library(igraph)

cibola <-
  read.csv(file = "data/Cibola_adj.csv",
           header = TRUE,
           row.names = 1)

# Create network in igraph format
cibola_i <- igraph::graph_from_adjacency_matrix(as.matrix(cibola),
                                                mode = "undirected")
# Set Vector property to images using a list with a length
# determined by the number of nodes in the network.
# Here we divide the northern and southern portions of the
# study area.
V(cibola_i)$raster <- list(img.2, img.1, img.2, img.2,
                           img.1, img.2, img.2, img.1,
                           img.1, img.1, img.2, img.2,
                           img.2, img.1, img.1, img.2,
                           img.1, img.1, img.1, img.1,
                           img.1, img.2, img.1, img.1,
                           img.2, img.1, img.2, img.2,
                           img.2, img.2, img.1)

set.seed(34673)
plot(
  cibola_i,
  vertex.shape = "raster",
  vertex.label = NA,
  vertex.size = 16,
  vertex.size2 = 16,
  edge.color = "gray"
)
```

<img src="05-visualization_files/figure-html/Fig6_13b-1.png" width="672" />

### Figure 6.14: Edge Thickness and Color {- #Figure_6_14}

Fig. 6.14. A random weighted graph where edge line thickness and color are both used to indicate weight in 5 categories. 

You can [download the data](data/Figure6_14.Rdata) to follow along.


```r
library(igraph)
library(ggraph)

load("data/Figure6_14.Rdata")

edge_cols <- colorRampPalette(c("gray", "darkblue"))(5)

set.seed(43644)
ggraph(g_net, layout = "fr") +
  geom_edge_link0(aes(width = E(g_net)$weight),
                  edge_colour = edge_cols[E(g_net)$weight]) +
  geom_node_point(shape = 21,
                  size = igraph::degree(g_net) + 3,
                  fill = "red") +
  theme_graph() +
  theme(legend.title = element_blank())
```

<img src="05-visualization_files/figure-html/Fig6_14-1.png" width="672" />

### Figure 6.15: Edge Direction {- #Figure_6_15}

Fig. 6.15. Two methods of displaying directed ties using arrows (left) and arcs (right). Both of these simple networks represent the same relationships shown in the adjacency matrix in the center.

See the tutorial on [edges](#EdgeOptions) above for more details on using arrows in `ggraph`. We use the `grid.table` function here from the `gridExtra` package to plot tabular data as a figure.


```r
library(igraph)
library(grid)
library(gridExtra)

g <- graph(c("A", "B",
              "B", "C",
              "A", "C",
              "A", "A",
              "C", "B",
              "D", "C"))

layout(matrix(c(1, 1, 2, 3, 3), 1, 5, byrow = TRUE))

set.seed(4355467)
plot(
  g,
  edge.arrow.size = 1,
  vertex.color = "black",
  vertex.size = 50,
  vertex.frame.color = "gray",
  vertex.label.color = "white",
  edge.width = 2,
  vertex.label.cex = 2.75,
  vertex.label.dist = 0,
  vertex.label.family = "Helvetica"
)

plot.new()
adj1 <- as.data.frame(as.matrix(as_adjacency_matrix(g)))
tt2 <- ttheme_minimal(base_size = 25)
grid.table(adj1, theme = tt2)

plot(
  g,
  edge.arrow.size = 1.25,
  vertex.color = "black",
  vertex.size = 50,
  vertex.frame.color = "gray",
  vertex.label.color = "white",
  edge.width = 2,
  edge.curved = 0.3,
  vertex.label.cex = 2.75,
  vertex.label.dist = 0,
  vertex.label.family = "Helvetica"
)
```

<img src="05-visualization_files/figure-html/Fig6_15-1.png" width="672" />

### Figure 6.16: Edge Binarization{- #Figure_6_16}

Fig. 6.16. These networks all show the same data based on similarity scores among sites in the U.S. Southwest (ca. AD 1350–1400) but each has a different cutoff for binarization.

The following chunk of code uses [ceramic similarity data from the SWSN database](data/Figure6_16.Rdata) and defines three different cutoff thresholds for defining edges. Note the only difference is the `thresh` argument in the `event2dichot` function.


```r
library(igraph)
library(statnet)
library(intergraph)
library(ggraph)
library(ggpubr)

load("data/Figure6_16.Rdata")
# Contains similarity matrix AD1350sim

ad1350sim_cut0_5 <- asIgraph(network(
  event2dichot(ad1350sim,
               method = "absolute",
               thresh = 0.25),
  directed = FALSE
))
ad1350sim_cut0_75 <- asIgraph(network(
  event2dichot(ad1350sim,
               method = "absolute",
               thresh = 0.5),
  directed = FALSE
))
ad1350sim_cut0_9 <- asIgraph(network(
  event2dichot(ad1350sim,
               method = "absolute",
               thresh = 0.75),
  directed = FALSE
))

set.seed(4637)
g0_50 <- ggraph(ad1350sim_cut0_5, layout = "fr") +
  geom_edge_link0(edge_colour = "black") +
  geom_node_point(shape = 21, fill = "gray") +
  ggtitle("0.25") +
  theme_graph()

set.seed(574578)
g0_75 <- ggraph(ad1350sim_cut0_75, layout = "fr") +
  geom_edge_link0(edge_colour = "black") +
  geom_node_point(shape = 21, fill = "gray") +
  ggtitle("0.50") +
  theme_graph()

set.seed(7343)
g0_90 <- ggraph(ad1350sim_cut0_9, layout = "fr") +
  geom_edge_link0(edge_colour = "black") +
  geom_node_point(shape = 21, fill = "gray") +
  ggtitle("0.75") +
  theme_graph()

ggarrange(g0_50, g0_75, g0_90, nrow = 1, ncol = 3)
```

<img src="05-visualization_files/figure-html/Fig6_16-1.png" width="672" />

### Figure 6.17: Edge Bundling {- #Figure_6_17}

Fig. 6.17. Network map of ceramic similarity from the U.S. Southwest/Mexican Northwest ca. AD 1350–1400 based on the hammer bundling algorithm. Note that this figure will look somewhat different from the one in the book as the locations of sites have been jittered for data security

<div class="rmdwarning">
<p>This function relies on the <code>edgebundle</code> package to
combine sets of nodes with similar relations into single paths. This
package also requires that you install the <code>reticulate</code>
package which connects R to Python 3.7 and you must also have Python
installed on your computer with the <code>datashader</code> Python
libraries.</p>
<p>Note that this will require about 1.4 GB of disk space and several
minutes so make sure you have adequate space and time before
beginning.</p>
</div>

To install an instance of Python with all of the required libraries you can use the following call: 


```r
edgebundle::install_bundle_py(method = "auto", conda = "auto")
```

[Use these data](data/Figure6_17.Rdata) to follow along. 


```r
library(igraph)
library(ggraph)
library(edgebundle)
library(ggmap)
library(sf)

load("data/Figure6_17.Rdata")
# attr.dat - site attribute data
# g.net - igraph network object
load("data/map.RData")
# map3 - state outlines
# base2 - terrain basemap in black and white

locations_sf <- st_as_sf(attr.dat, coords = c("V3", "V4"),
                         crs = 26912)
z <- st_transform(locations_sf, crs = 4326)
coord1 <- do.call(rbind, st_geometry(z)) %>%
  tibble::as_tibble() %>%
  setNames(c("lon", "lat"))

xy <- as.data.frame(coord1)
colnames(xy) <- c("x", "y")

hbundle <- edge_bundle_hammer(g.net, xy, bw = 0.9, decay = 0.2)

ggmap(base2, darken = 0.15) +
  geom_polygon(
    data = map3,
    aes(x, y,
        group = Group.1),
    col = "black",
    size = 0.5,
    fill = NA
  ) +
  geom_path(
    data = hbundle,
    aes(x, y, group = group),
    color = "white",
    show.legend = FALSE
  ) +
  geom_path(
    data = hbundle,
    aes(x, y, group = group),
    color = "darkorchid4",
    show.legend = FALSE
  ) +
  geom_point(
    data = xy,
    aes(x, y),
    alpha = 0.4,
    size = 2.5,
    show.legend = FALSE
  ) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/Fig6_17-1.png" width="672" />

### Figure 6.18: Group-in-a-box {- #Figure_6_18}

Fig. 6.18. Example of a group-in-a-box custom graph layout created in NodeXL based on ceramic similarity data from the U.S. Southwest/Mexican Northwest ca. AD 1350-1400. 

The group-in-a-box network format is, as far as we are aware, currently only implemented in the [NodeXL](https://www.smrfoundation.org/nodexl/) platform. This software package is an add-in for Microsoft Excel that allows for the creation and analysis of network graphs using a wide variety of useful visualization tools. To produce a "Group-in-a-box" layout you simply need to paste a set of edge list values into the NodeXL Excel Template, define groups (based on an algorithm or some vertex attribute), and be sure to select "Layout each of the graph's groups in its own box" in the layout options. 

For more details on how to use NodeXL see the extensive documentation online. There are commercial versions of the software available but the group-in-a-box example shown here can be produced in the free version.

![](images/group-in-a-box.jpg){width=100%} 

To download an Excel workbook set up for the example provided in the book [click here]("data/NodeXLGraph1.xlsx"). When you open this In Excel, it will ask you if it can install the necessary extensions. Say yes to continue and replicate the results in the book.

### Figure 6.19: Weighted Adjacency Matrix {- #Figure_6_19}

Fig. 6.19. Dual display of a network graph and associated weighted adjacency matrix based on Peeples (2018) ceramic technology data.

This plot uses a sub-set of the [Cibola technological similarity network](#Cibola) data to produce both a typical node-link diagram and an associated weighted adjacency matrix. [Use these data](data/Figure6_19.Rdata) to follow along.


```r
library(igraph)
library(ggraph)
library(ggpubr)

load("data/Figure6_19.Rdata")
# graph6.18 - graph object in igraph format
# node_list - data frame with node details
# edge_list - edge_list which contains information on groups
# and edge weight

set.seed(343645)
coords <- layout_with_fr(graph6.18)
g1 <- ggraph(graph6.18, "manual",
             x = coords[, 1],
             y = coords[, 2]) +
  geom_edge_link(aes(),
                 color = "gray75",
                 alpha = 0.5,
                 show.legend = FALSE) +
  geom_node_point(aes(color = as.factor(V(graph6.18)$comm), size = 5),
                  show.legend = FALSE) +
  scale_color_manual(values = c("#8da0cb", "#66c2a5", "#fc8d62"),
                     guide = FALSE) +
  theme_graph()

# Set order of nodes to order in which they appear in the y axis in
# the network graph above
name_order <- node_list[order(coords[, 2]), ]$name

# Adjust the "to" and "from" factor levels so they are equal
# to this complete list of node names
plot_data <- edge_list %>% mutate(to = factor(to, levels = name_order),
                                  from = factor(from, levels = rev(name_order)))

# Now run the ggplot code again
# Create the adjacency matrix plot
g2 <- ggplot(plot_data, aes(
  x = from,
  y = to,
  fill = group,
  alpha = (weight * 1.5)
)) +
  geom_tile() +
  theme_bw() +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    axis.text.x = element_text(
      angle = 270,
      hjust = 0,
      size = rel(0.5)
    ),
    axis.text.y = element_text(size = rel(0.5)),
    aspect.ratio = 1,
    legend.position = "none"
  ) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("#8da0cb", "#66c2a5", "#fc8d62", "black"),
                    guide = FALSE)

# Combine into a single figure
figure6_19 <- ggarrange(g1, g2, nrow = 1)

figure6_19
```

<img src="05-visualization_files/figure-html/Fig6_18-1.png" width="672" />

### Figure 6.20: Nodetrix Diagram {- #Figure_6_20}

Fig. 6.20. Nodetrix visualisation of the Peeples (2018) ceramic technological data showing one dense cluster as an adjacency matrix and the remainder of the graph as a node-link diagram.

![Nodetrix visualization](images/nodetrix.jpg){width=100%}

This Nodetrix interactive visualization was created using the Javascript implementation available on [GitHub](https://github.com/IRT-SystemX/nodetrix) by user [jdfekete](https://github.com/jdfekete/), Jean-Daniel Fekete who was one of the original authors of the method (Henry et al. 2007). To see a live demo of the Nodetrix Application in use with the Cibola technological similarity data [click here](https://mattpeeples.net/nodetrix/).

The details of running the Javascript program are described on the GitHub page and are beyond this scope of this tutorial. We do illustrate below, however, how you can export R in the *.json format required by this program using the `d3r` and `rjson` packages. The code below expects and `igraph` network object. 

<div class="rmdwarning">
<p>Note that the Nodetrix.js application expects node names/designations
with no spaces in a node attribute called “name” so be sure to check
before you run the code below.</p>
</div>



```r
library(d3r)
library(rjson)

# net <- igraph network object

data_json <- d3_igraph(net)


dj <- jsonlite::fromJSON(data_json)
dj$links[[1]] <- as.numeric(dj$links[[1]])
dj$links[[2]] <- as.numeric(dj$links[[2]])
dj <- jsonlite::toJSON(dj)

write(dj, "network.json")
```


### Figure 6.21: The Filmstrip Approach {- #Figure_6_21}

Fig. 6.21. A demonstration of the filmstrip approach to plotting longitudinal network data. These data represent networks of ceramic similarity in the San Pedro Valley of Arizona for three consecutive 50-year intervals. 

[Use these data](data/Figure6_21.Rdata) to replicate the figures shown here.


```r
library(igraph)
library(ggraph)
library(ggpubr)

load("data/Figure6_21.Rdata")

set.seed(4543)
g1 <- ggraph(AD1250net, "kk") +
  geom_edge_link(aes(), color = "gray75", show.legend = FALSE) +
  geom_node_point(aes(),
                  size = 1,
                  show.legend = FALSE,
                  color = "blue") +
  ggtitle("AD1250-1300") +
  theme_graph()

set.seed(4543)
g2 <- ggraph(AD1300net, "kk") +
  geom_edge_link(aes(), color = "gray75", show.legend = FALSE) +
  geom_node_point(aes(),
                  size = 1,
                  show.legend = FALSE,
                  color = "blue") +
  ggtitle("AD1300-1350") +
  theme_graph()


set.seed(4543)
g3 <- ggraph(AD1350net, "kk") +
  geom_edge_link(aes(), color = "gray75", show.legend = FALSE) +
  geom_node_point(aes(),
                  size = 1,
                  show.legend = FALSE,
                  color = "blue") +
  ggtitle("AD1350-1400") +
  theme_graph()

figure6_21 <- ggarrange(g1, g2, g3, nrow = 1)

figure6_21
```

<img src="05-visualization_files/figure-html/Fig6_21-1.png" width="672" />

### Figure 6.22: Similtaneous Display {- #Figure_6_22}

Fig. 6.22. Examples of simultaneous display of two consecutive intervals for the San Pedro valley ceramic similarity network. (a) A network using the Kamada-Kawai algorithm with edges color-coded based on time period. (b) An arc plot showing ties in consecutive intervals above and below the line.

[Use these data](data/Figure6_22.Rdata) to follow along. Note in the first plot we add the `colour` argument to the `aes()` statement to include our period designation.


```r
library(igraph)
library(ggraph)
library(ggpubr)
library(ggrepel)

load("data/Figure6_22.Rdata")

graph <- graph_from_data_frame(net_all)

xy <- layout_with_kk(graph)
xy <- cbind(sites, xy)
xy <- as.data.frame(xy)
colnames(xy) <- c("site", "x", "y")
xy$x <- as.numeric(xy$x)
xy$y <- as.numeric(xy$y)

set.seed(6436)
similt_net <- ggraph(graph, layout = "manual",
                     x = xy$x, y = xy$y) +
  geom_edge_link(aes(colour = Period), alpha = 0.3, width = 1) +
  geom_node_point(size = 3) +
  theme_graph() +
  theme(legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1)),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(2, "cm"))

# Make the graph
lin_net <- ggraph(spgraph, layout = "linear") +
  geom_edge_arc(edge_colour = "black", edge_alpha = 0.4, edge_width = 0.3,
                fold = FALSE, strength = 1) +
  geom_node_point(aes(size = igraph::degree(spgraph)), col = "red",
                  alpha = 0.5) +
  scale_size_continuous(range = c(4, 8)) +
  theme_graph() +
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0, 0, 0.4, 0), "null"),
        panel.spacing = unit(c(0, 0, 3.4, 0), "null")) +
  annotate("text", x = 3, y = 3, label = "AD 1250-1300",
           size = 4) +
  annotate("text", x = 3, y = -3, label = "AD 1300-1350",
           size = 4)

similt_net
```

<img src="05-visualization_files/figure-html/unnamed-chunk-19-1.png" width="672" />

```r
lin_net
```

<img src="05-visualization_files/figure-html/unnamed-chunk-19-2.png" width="672" />

### Figure 6:23: Timelines and Time Prisms {- #Figure_6_23}

Fig. 6.23. This plot shows two displays of the same ceramic similarity data from the Sonoran Desert in the U.S. Southwest as a time prism (top) and timeline (bottom).

These examples were drawn from work outline on a workshop focused on temporal networks by Skye Bender-deMoll. [Click here](https://statnet.org/Workshops/ndtv_workshop.html) to see the detailed workshop overview. The functions for animating and plotting temporal networks used here come from the `ndtv` and `networkDynamic` packages.

<div class="rmdwarning">
<p>Note that the data required is a list object that contains multiple
temporal slices of the same network in <code>network</code> format from
the <code>statnet</code> suite of packages. Each network must have the
same number of nodes and the same node identifiers must be used in every
network in the list.</p>
</div>

[Use these data](data/Figure6_23.Rdata) to follow along.


```r
library(networkDynamic)
library(ndtv)
library(scatterplot3d)
library(prettyGraphs)
library(statnet)

load("data/Figure6_23.Rdata")

# create networkDynamic object from list containing multiple
# sna network objects
sanpedro <- networkDynamic(network.list = sp_nets)
```

```
## Neither start or onsets specified, assuming start=0
## Onsets and termini not specified, assuming each network in network.list should have a discrete spell of length 1
## Argument base.net not specified, using first element of network.list instead
## Created net.obs.period to describe network
##  Network observation period info:
##   Number of observation spells: 1 
##   Maximal time range observed: 0 until 5 
##   Temporal mode: discrete 
##   Time unit: step 
##   Suggested time increment: 1
```

```r
# Compute animation
compute.animation(sanpedro, default.dist = 7, animation.mode = "kamadakawai")
```

```
## slice parameters:
##   start:0
##   end:5
##   interval:1
##   aggregate.dur:1
##   rule:latest
```

```r
# Define colors for regions
mycol <- c(
  add.alpha("#1b9e77", 0.75),
  add.alpha("#d95f02", 0.75),
  add.alpha("#7570b3", 0.75),
  add.alpha("#e7298a", 0.75),
  add.alpha("#66a61e", 0.75),
  add.alpha("#e6ab02", 0.75)
)

# Plot time prism
set.seed(364467)
timePrism(
  sanpedro,
  at = c(1, 2, 3),
  displaylabels = FALSE,
  planes = TRUE,
  display.isolates = FALSE,
  label.cex = 0.5,
  usearrows = FALSE,
  vertex.cex = 0.5,
  edge.col = "gray50",
  vertex.col = mycol[factor(sp_attr$SWSN_MacroGroup)]
)
```

<img src="05-visualization_files/figure-html/Fig6_23-1.png" width="672" />

```r
# Plot proximity timeline
set.seed(235254)
proximity.timeline(
  sanpedro,
  default.dist = 10,
  mode = "sammon",
  labels.at = 17,
  vertex.cex = 4,
  render.edges = FALSE,
  vertex.col = mycol[factor(sp_attr$SWSN_MacroGroup)],
  chain.direction = "reverse",
  xaxt = "n"
)
```

<img src="05-visualization_files/figure-html/Fig6_23-2.png" width="672" />

### Figure 6.24: Animation {- #Figure_6_24}

Fig. 6.24. An example of three frames from a network animation. 

Figure 6.24 was created using the `ndtv` package and the same data produced above for figure 6.23. We simply rendered the animation as above and then output to an interactive html widget. The figure in the book represents 3 screen shots from the interactive plot. See the `ndtv` documentation for more details.


```r
render.d3movie(sanpedro, vertex.col = mycol[factor(sp_attr$SWSN_MacroGroup)])
```


```r
render.d3movie(sanpedro, vertex.col = mycol[factor(sp_attr$SWSN_MacroGroup)],
               output.mode = "inline")
```

<iframe style="width: 100%; height: 500px;" src="data:text/html;base64,PCFET0NUWVBFIGh0bWw+DQo8aHRtbCBsYW5nPSdlbic+DQogIDxoZWFkPg0KICAg
IDxtZXRhIGNoYXJzZXQ9J3V0Zi04Jz4NCiAgICAgIDwhLS0gY3NzIGZvciBzdHls
aW5nIHRoZSBkMy5zbGlkZXIgbGliIC0tPg0KICAgICAgPHN0eWxlIHR5cGU9J3Rl
eHQvY3NzJz4uZDMtc2xpZGVyIHsKICAgIHBvc2l0aW9uOiByZWxhdGl2ZTsKICAg
IGZvbnQtZmFtaWx5OiBWZXJkYW5hLEFyaWFsLHNhbnMtc2VyaWY7CiAgICBmb250
LXNpemU6IDEuMWVtOwogICAgYm9yZGVyOiAxcHggc29saWQgI2FhYWFhYTsKICAg
IHotaW5kZXg6IDI7Cn0KCi5kMy1zbGlkZXItaG9yaXpvbnRhbCB7CiAgICBoZWln
aHQ6IC44ZW07Cn0gIAoKLmQzLXNsaWRlci1yYW5nZSB7CiAgYmFja2dyb3VuZDoj
Mjk4MGI5OwogIGxlZnQ6MHB4OwogIHJpZ2h0OjBweDsKICBoZWlnaHQ6IDAuOGVt
OwogIHBvc2l0aW9uOiBhYnNvbHV0ZTsKfQoKLmQzLXNsaWRlci1yYW5nZS12ZXJ0
aWNhbCB7CiAgYmFja2dyb3VuZDojMjk4MGI5OwogIGxlZnQ6MHB4OwogIHJpZ2h0
OjBweDsKICBwb3NpdGlvbjogYWJzb2x1dGU7CiAgdG9wOjA7Cn0KCi5kMy1zbGlk
ZXItdmVydGljYWwgewogICAgd2lkdGg6IC44ZW07CiAgICBoZWlnaHQ6IDEwMHB4
Owp9ICAgICAgCgouZDMtc2xpZGVyLWhhbmRsZSB7CiAgICBwb3NpdGlvbjogYWJz
b2x1dGU7CiAgICB3aWR0aDogMS4yZW07CiAgICBoZWlnaHQ6IDEuMmVtOwogICAg
Ym9yZGVyOiAxcHggc29saWQgI2QzZDNkMzsKICAgIGJvcmRlci1yYWRpdXM6IDRw
eDsKICAgIGJhY2tncm91bmQ6ICNlZWU7CiAgICBiYWNrZ3JvdW5kOiBsaW5lYXIt
Z3JhZGllbnQodG8gYm90dG9tLCAjZWVlIDAlLCAjZGRkIDEwMCUpOwogICAgei1p
bmRleDogMzsKICAgIGN1cnNvcjogbW92ZTsKfQoKLmQzLXNsaWRlci1oYW5kbGU6
aG92ZXIgewogICAgYm9yZGVyOiAxcHggc29saWQgIzk5OTk5OTsKfQoKLmQzLXNs
aWRlci1ob3Jpem9udGFsIC5kMy1zbGlkZXItaGFuZGxlIHsKICAgIHRvcDogLS4z
ZW07Cn0KCi5kMy1zbGlkZXItYXhpcyB7CiAgICBwb3NpdGlvbjogcmVsYXRpdmU7
CiAgICB6LWluZGV4OiAxOyAgICAKfQoKLmQzLXNsaWRlci1heGlzLWJvdHRvbSB7
CiAgICB0b3A6IC44ZW07Cn0KCi5kMy1zbGlkZXItYXhpcy1yaWdodCB7CiAgICBs
ZWZ0OiAuOGVtOwp9CgouZDMtc2xpZGVyLWF4aXMgcGF0aCB7CiAgICBzdHJva2Ut
d2lkdGg6IDA7CiAgICBmaWxsOiBub25lOwp9CgouZDMtc2xpZGVyLWF4aXMgbGlu
ZSB7CiAgICBmaWxsOiBub25lOwogICAgc3Ryb2tlOiAjYWFhOwogICAgc2hhcGUt
cmVuZGVyaW5nOiBjcmlzcEVkZ2VzOwp9CgouZDMtc2xpZGVyLWF4aXMgdGV4dCB7
CiAgICBmb250LXNpemU6IDExcHg7Cn0KCi5kMy1zbGlkZXItdmVydGljYWwgLmQz
LXNsaWRlci1oYW5kbGUgewogICAgbGVmdDogLS4yNWVtOwogICAgbWFyZ2luLWxl
ZnQ6IDA7CiAgICBtYXJnaW4tYm90dG9tOiAtLjZlbTsgICAgICAKfTwvc3R5bGU+
DQogICAgICA8IS0tIGNzcyBmb3Igc3R5bGluZyB0aGUgbmR0di1kMyByZW5kZXIg
YW5kIGNvbXBvbmVudHMgLS0+DQogICAgICA8c3R5bGUgdHlwZT0ndGV4dC9jc3Mn
Pi5uZHR2LWZ1bGxzY3JlZW4geyAKICBwYWRkaW5nOiAwOwogIG1hcmdpbjogMDsK
ICB3aWR0aDogMTAwJTsKICBoZWlnaHQ6IDEwMCU7Cn0KCi5uZHR2LWQzLWNvbnRh
aW5lciB7CiAgcG9zaXRpb246IHJlbGF0aXZlOwp9CgoubWFpbiwgLnhsYWIgewog
IHRleHQtYW5jaG9yOiBtaWRkbGU7Cn0KCi5sYWJlbCwgLm1haW4sIC54bGFiLCAu
dG9vbHRpcCB7CiAgZm9udC1mYW1pbHk6IHNhbnMtc2VyaWY7Cn0KCi5kcmFnZ2lu
ZyB7CiAgY3Vyc29yOiBtb3ZlOwp9Cgoubm9kZSB7CiAgc3Ryb2tlLXdpZHRoOiAx
cHg7CiAgZmlsbDogcmVkOwogIHN0cm9rZTogYmxhY2s7Cn0KCi5ub2RlLCAuZWRn
ZSB7CiAgY3Vyc29yOiBwb2ludGVyOwogIC8qdHJhbnNpdGlvbjogc3Ryb2tlLXdp
ZHRoIDAuMnMgZWFzZTsqLwp9CgovKi5ub2RlOmhvdmVyLCAuZWRnZTpob3ZlciB7
CiAgc3Ryb2tlLXdpZHRoOiA1cHggIWltcG9ydGFudDsKfSovCgouZWRnZSB7CiAg
c3Ryb2tlOiBibGFjazsKfQoKLmxhYmVsIHsKICBwb2ludGVyLWV2ZW50czogbm9u
ZTsKfQoKLmdyYXBoIHsgCiAgcG9zaXRpb246IHJlbGF0aXZlOwogIG92ZXJmbG93
OiBoaWRkZW47Cn0KCi5rZXkgewogIHBhZGRpbmc6IDEwcHg7CiAgdGV4dC1hbGln
bjogY2VudGVyOwp9CgouY29udHJvbHMgeyAKICBtYXJnaW46IDEwcHggMjBweDsK
ICBoZWlnaHQ6IDQwcHg7CiAgYm94LXNpemluZzogYm9yZGVyLWJveDsKICBwb3Np
dGlvbjogYWJzb2x1dGU7CiAgYm90dG9tOiAwcHg7CiAgbGVmdDogMHB4OwogIHJp
Z2h0OiAwcHg7Cn0KCi5wbGF5LWNvbnRyb2wtY29udGFpbmVyPmRpdiB7CiAgd2lk
dGg6IDI1cHg7CiAgaGVpZ2h0OiAyNXB4OwogIG92ZXJmbG93OiBoaWRkZW47CiAg
ZGlzcGxheTogaW5saW5lLWJsb2NrOwp9CgouY29udHJvbHMgLmljb24sIC5uZHR2
LW1lbnUtY29udGFpbmVyIC5pY29uIHsKICB3aWR0aDogMTAwJTsKICBoZWlnaHQ6
IDEwMCU7CiAgY3Vyc29yOiBwb2ludGVyOwp9CgouY29udHJvbHMgLmljb246aG92
ZXIgewogIGZpbGw6IGJsdWU7Cn0KCi5kYXRhX2Nob29zZXJfY29udGFpbmVyIHsK
ICBwb3NpdGlvbjogYWJzb2x1dGU7IHRvcDogMTBweDsgbGVmdDogMTBweDsKfQoK
LnBsYXktY29udHJvbC1jb250YWluZXIgewogIGZsb2F0OiByaWdodDsgCiAgYmFj
a2dyb3VuZDogI2NjYzsgCiAgYm9yZGVyLXJhZGl1czogNXB4OyAKICBwYWRkaW5n
OiA3cHg7CiAgcGFkZGluZy1ib3R0b206IDNweDsKICBib3gtc2l6aW5nOiBib3Jk
ZXItYm94Owp9CgoubmR0di1kMy1jb250YWluZXIgeyAKICBwb3NpdGlvbjogcmVs
YXRpdmU7CiAgYm94LXNpemluZzogYm9yZGVyLWJveDsKICBib3JkZXI6IDFweCBz
b2xpZDsKfQoKLnNsaWRlci1jb250cm9sLWNvbnRhaW5lciB7IAogIG1hcmdpbi1y
aWdodDogMTYwcHg7CiAgYm94LXNpemluZzogYm9yZGVyLWJveDsKfQoKLmQzLXNs
aWRlci1oYW5kbGUgewogIG1pbi13aWR0aDogMTBweDsKfQoKLmQzLXNsaWRlci1h
eGlzIHsKICBoZWlnaHQ6IDIwcHg7Cn0KCi50b29sdGlwIHsKICBkaXNwbGF5OiBu
b25lOwogIHBvc2l0aW9uOiBhYnNvbHV0ZTsKICB6LWluZGV4OiAxMDAwOwogIHBh
ZGRpbmc6IDEwcHggMTBweDsKICBiYWNrZ3JvdW5kOiByZ2JhKDI1NSwyNTUsMjU1
LCAxKTsKICBib3JkZXI6IDFweCBzb2xpZCAjNjY2OwogIG9wYWNpdHk6IDE7ICAv
KiB0aGlzIGlzIHRvIHByZXZlbnQgYm9vdHN0cmFwJ3MgdG9vbHRpcCBydWxlIGZy
b20gb3ZlcnJkaW5nICovCn0KCi5mcmFtZUluZm8gewogIGRpc3BsYXk6IG5vbmU7
CiAgcG9zaXRpb246IGFic29sdXRlOwogIGJvdHRvbTogMHB4OwogIGxlZnQ6IDBw
eDsKICBwYWRkaW5nOiA1cHg7Cn0KCi5kdXJhdGlvbkNvbnRyb2wgewogIHdpZHRo
OiAxNTBweDsKICBtYXJnaW4tYm90dG9tOiAyMHB4OyAKfQoKLmNvbnRhaW5lciAu
c2NyZWVuIHsKICBvcGFjaXR5OiAwOwogIHBvaW50ZXItZXZlbnRzOiBub25lOwog
IGZpbGw6IG5vbmU7Cn0KCi5jb250YWluZXIgLnNjcmVlbi5uZXR3b3JrLXNlbGVj
dGVkIHsKICBvcGFjaXR5OiAuNjsKfQoKLm5kdHYtbWVudS1jb250YWluZXIgewog
IHBvc2l0aW9uOiBhYnNvbHV0ZTsKICB0b3A6IDVweDsKICByaWdodDogNXB4Owp9
CgoubmR0di1tZW51LWljb24gewogIHdpZHRoOiAyNXB4OwogIGhlaWdodDogMjVw
eDsKICBmbG9hdDogcmlnaHQ7CiAgei1pbmRleDogMTAwOwp9CgoubmR0di1tZW51
LWljb24ubWVudS1hY3RpdmUgewogIGJhY2tncm91bmQ6ICNmZmY7CiAgYm9yZGVy
OiAxcHggc29saWQgIzY2NjsKICBib3JkZXItYm90dG9tOiAxcHggc29saWQgI2Zm
ZjsKfQoKLm5kdHYtbWVudSB7CiAgYmFja2dyb3VuZDogd2hpdGU7CiAgYm9yZGVy
OiAxcHggc29saWQgIzY2NjsKICBib3gtc2hhZG93OiAwcHggMnB4IDRweCByZ2Jh
KDAsMCwwLDAuMik7CiAgcGFkZGluZzogMTBweDsKICBtYXJnaW4tdG9wOiAyNXB4
OwogIGRpc3BsYXk6IG5vbmU7Cn0KCi5tZW51LWl0ZW0gewogIHBhZGRpbmc6IDEw
cHggMHB4OwogIGJvcmRlci1ib3R0b206IDFweCBzb2xpZCAjQ0NDOwp9CgoubWVu
dS1pdGVtOmZpcnN0LWNoaWxkIHsKICBwYWRkaW5nLXRvcDogMHB4Owp9CgoubWVu
dS1pdGVtOmxhc3QtY2hpbGQgewogIHBhZGRpbmctYm90dG9tOiAwcHg7CiAgYm9y
ZGVyOiBub25lOwp9Cjwvc3R5bGU+DQogICAgPCEtLSBtaW5pbWl6ZWQgZDMuanMg
bGlicmFyeSAtLT4NCiAgICAgIDxzY3JpcHQ+IWZ1bmN0aW9uKCl7ZnVuY3Rpb24g
bihuLHQpe3JldHVybiB0Pm4/LTE6bj50PzE6bj49dD8wOjAvMH1mdW5jdGlvbiB0
KG4pe3JldHVybiBudWxsIT1uJiYhaXNOYU4obil9ZnVuY3Rpb24gZShuKXtyZXR1
cm57bGVmdDpmdW5jdGlvbih0LGUscix1KXtmb3IoYXJndW1lbnRzLmxlbmd0aDwz
JiYocj0wKSxhcmd1bWVudHMubGVuZ3RoPDQmJih1PXQubGVuZ3RoKTt1PnI7KXt2
YXIgaT1yK3U+Pj4xO24odFtpXSxlKTwwP3I9aSsxOnU9aX1yZXR1cm4gcn0scmln
aHQ6ZnVuY3Rpb24odCxlLHIsdSl7Zm9yKGFyZ3VtZW50cy5sZW5ndGg8MyYmKHI9
MCksYXJndW1lbnRzLmxlbmd0aDw0JiYodT10Lmxlbmd0aCk7dT5yOyl7dmFyIGk9
cit1Pj4+MTtuKHRbaV0sZSk+MD91PWk6cj1pKzF9cmV0dXJuIHJ9fX1mdW5jdGlv
biByKG4pe3JldHVybiBuLmxlbmd0aH1mdW5jdGlvbiB1KG4pe2Zvcih2YXIgdD0x
O24qdCUxOyl0Kj0xMDtyZXR1cm4gdH1mdW5jdGlvbiBpKG4sdCl7dHJ5e2Zvcih2
YXIgZSBpbiB0KU9iamVjdC5kZWZpbmVQcm9wZXJ0eShuLnByb3RvdHlwZSxlLHt2
YWx1ZTp0W2VdLGVudW1lcmFibGU6ITF9KX1jYXRjaChyKXtuLnByb3RvdHlwZT10
fX1mdW5jdGlvbiBvKCl7fWZ1bmN0aW9uIGEobil7cmV0dXJuIG9hK24gaW4gdGhp
c31mdW5jdGlvbiBjKG4pe3JldHVybiBuPW9hK24sbiBpbiB0aGlzJiZkZWxldGUg
dGhpc1tuXX1mdW5jdGlvbiBsKCl7dmFyIG49W107cmV0dXJuIHRoaXMuZm9yRWFj
aChmdW5jdGlvbih0KXtuLnB1c2godCl9KSxufWZ1bmN0aW9uIHMoKXt2YXIgbj0w
O2Zvcih2YXIgdCBpbiB0aGlzKXQuY2hhckNvZGVBdCgwKT09PWFhJiYrK247cmV0
dXJuIG59ZnVuY3Rpb24gZigpe2Zvcih2YXIgbiBpbiB0aGlzKWlmKG4uY2hhckNv
ZGVBdCgwKT09PWFhKXJldHVybiExO3JldHVybiEwfWZ1bmN0aW9uIGgoKXt9ZnVu
Y3Rpb24gZyhuLHQsZSl7cmV0dXJuIGZ1bmN0aW9uKCl7dmFyIHI9ZS5hcHBseSh0
LGFyZ3VtZW50cyk7cmV0dXJuIHI9PT10P246cn19ZnVuY3Rpb24gcChuLHQpe2lm
KHQgaW4gbilyZXR1cm4gdDt0PXQuY2hhckF0KDApLnRvVXBwZXJDYXNlKCkrdC5z
bGljZSgxKTtmb3IodmFyIGU9MCxyPWNhLmxlbmd0aDtyPmU7KytlKXt2YXIgdT1j
YVtlXSt0O2lmKHUgaW4gbilyZXR1cm4gdX19ZnVuY3Rpb24gdigpe31mdW5jdGlv
biBkKCl7fWZ1bmN0aW9uIG0obil7ZnVuY3Rpb24gdCgpe2Zvcih2YXIgdCxyPWUs
dT0tMSxpPXIubGVuZ3RoOysrdTxpOykodD1yW3VdLm9uKSYmdC5hcHBseSh0aGlz
LGFyZ3VtZW50cyk7cmV0dXJuIG59dmFyIGU9W10scj1uZXcgbztyZXR1cm4gdC5v
bj1mdW5jdGlvbih0LHUpe3ZhciBpLG89ci5nZXQodCk7cmV0dXJuIGFyZ3VtZW50
cy5sZW5ndGg8Mj9vJiZvLm9uOihvJiYoby5vbj1udWxsLGU9ZS5zbGljZSgwLGk9
ZS5pbmRleE9mKG8pKS5jb25jYXQoZS5zbGljZShpKzEpKSxyLnJlbW92ZSh0KSks
dSYmZS5wdXNoKHIuc2V0KHQse29uOnV9KSksbil9LHR9ZnVuY3Rpb24geSgpe1Zv
LmV2ZW50LnByZXZlbnREZWZhdWx0KCl9ZnVuY3Rpb24geCgpe2Zvcih2YXIgbix0
PVZvLmV2ZW50O249dC5zb3VyY2VFdmVudDspdD1uO3JldHVybiB0fWZ1bmN0aW9u
IE0obil7Zm9yKHZhciB0PW5ldyBkLGU9MCxyPWFyZ3VtZW50cy5sZW5ndGg7Kytl
PHI7KXRbYXJndW1lbnRzW2VdXT1tKHQpO3JldHVybiB0Lm9mPWZ1bmN0aW9uKGUs
cil7cmV0dXJuIGZ1bmN0aW9uKHUpe3RyeXt2YXIgaT11LnNvdXJjZUV2ZW50PVZv
LmV2ZW50O3UudGFyZ2V0PW4sVm8uZXZlbnQ9dSx0W3UudHlwZV0uYXBwbHkoZSxy
KX1maW5hbGx5e1ZvLmV2ZW50PWl9fX0sdH1mdW5jdGlvbiBfKG4pe3JldHVybiBz
YShuLHZhKSxufWZ1bmN0aW9uIGIobil7cmV0dXJuImZ1bmN0aW9uIj09dHlwZW9m
IG4/bjpmdW5jdGlvbigpe3JldHVybiBmYShuLHRoaXMpfX1mdW5jdGlvbiB3KG4p
e3JldHVybiJmdW5jdGlvbiI9PXR5cGVvZiBuP246ZnVuY3Rpb24oKXtyZXR1cm4g
aGEobix0aGlzKX19ZnVuY3Rpb24gUyhuLHQpe2Z1bmN0aW9uIGUoKXt0aGlzLnJl
bW92ZUF0dHJpYnV0ZShuKX1mdW5jdGlvbiByKCl7dGhpcy5yZW1vdmVBdHRyaWJ1
dGVOUyhuLnNwYWNlLG4ubG9jYWwpfWZ1bmN0aW9uIHUoKXt0aGlzLnNldEF0dHJp
YnV0ZShuLHQpfWZ1bmN0aW9uIGkoKXt0aGlzLnNldEF0dHJpYnV0ZU5TKG4uc3Bh
Y2Usbi5sb2NhbCx0KX1mdW5jdGlvbiBvKCl7dmFyIGU9dC5hcHBseSh0aGlzLGFy
Z3VtZW50cyk7bnVsbD09ZT90aGlzLnJlbW92ZUF0dHJpYnV0ZShuKTp0aGlzLnNl
dEF0dHJpYnV0ZShuLGUpfWZ1bmN0aW9uIGEoKXt2YXIgZT10LmFwcGx5KHRoaXMs
YXJndW1lbnRzKTtudWxsPT1lP3RoaXMucmVtb3ZlQXR0cmlidXRlTlMobi5zcGFj
ZSxuLmxvY2FsKTp0aGlzLnNldEF0dHJpYnV0ZU5TKG4uc3BhY2Usbi5sb2NhbCxl
KX1yZXR1cm4gbj1Wby5ucy5xdWFsaWZ5KG4pLG51bGw9PXQ/bi5sb2NhbD9yOmU6
ImZ1bmN0aW9uIj09dHlwZW9mIHQ/bi5sb2NhbD9hOm86bi5sb2NhbD9pOnV9ZnVu
Y3Rpb24gayhuKXtyZXR1cm4gbi50cmltKCkucmVwbGFjZSgvXHMrL2csIiAiKX1m
dW5jdGlvbiBFKG4pe3JldHVybiBuZXcgUmVnRXhwKCIoPzpefFxccyspIitWby5y
ZXF1b3RlKG4pKyIoPzpcXHMrfCQpIiwiZyIpfWZ1bmN0aW9uIEEobil7cmV0dXJu
KG4rIiIpLnRyaW0oKS5zcGxpdCgvXnxccysvKX1mdW5jdGlvbiBDKG4sdCl7ZnVu
Y3Rpb24gZSgpe2Zvcih2YXIgZT0tMTsrK2U8dTspbltlXSh0aGlzLHQpfWZ1bmN0
aW9uIHIoKXtmb3IodmFyIGU9LTEscj10LmFwcGx5KHRoaXMsYXJndW1lbnRzKTsr
K2U8dTspbltlXSh0aGlzLHIpfW49QShuKS5tYXAoTik7dmFyIHU9bi5sZW5ndGg7
cmV0dXJuImZ1bmN0aW9uIj09dHlwZW9mIHQ/cjplfWZ1bmN0aW9uIE4obil7dmFy
IHQ9RShuKTtyZXR1cm4gZnVuY3Rpb24oZSxyKXtpZih1PWUuY2xhc3NMaXN0KXJl
dHVybiByP3UuYWRkKG4pOnUucmVtb3ZlKG4pO3ZhciB1PWUuZ2V0QXR0cmlidXRl
KCJjbGFzcyIpfHwiIjtyPyh0Lmxhc3RJbmRleD0wLHQudGVzdCh1KXx8ZS5zZXRB
dHRyaWJ1dGUoImNsYXNzIixrKHUrIiAiK24pKSk6ZS5zZXRBdHRyaWJ1dGUoImNs
YXNzIixrKHUucmVwbGFjZSh0LCIgIikpKX19ZnVuY3Rpb24geihuLHQsZSl7ZnVu
Y3Rpb24gcigpe3RoaXMuc3R5bGUucmVtb3ZlUHJvcGVydHkobil9ZnVuY3Rpb24g
dSgpe3RoaXMuc3R5bGUuc2V0UHJvcGVydHkobix0LGUpfWZ1bmN0aW9uIGkoKXt2
YXIgcj10LmFwcGx5KHRoaXMsYXJndW1lbnRzKTtudWxsPT1yP3RoaXMuc3R5bGUu
cmVtb3ZlUHJvcGVydHkobik6dGhpcy5zdHlsZS5zZXRQcm9wZXJ0eShuLHIsZSl9
cmV0dXJuIG51bGw9PXQ/cjoiZnVuY3Rpb24iPT10eXBlb2YgdD9pOnV9ZnVuY3Rp
b24gTChuLHQpe2Z1bmN0aW9uIGUoKXtkZWxldGUgdGhpc1tuXX1mdW5jdGlvbiBy
KCl7dGhpc1tuXT10fWZ1bmN0aW9uIHUoKXt2YXIgZT10LmFwcGx5KHRoaXMsYXJn
dW1lbnRzKTtudWxsPT1lP2RlbGV0ZSB0aGlzW25dOnRoaXNbbl09ZX1yZXR1cm4g
bnVsbD09dD9lOiJmdW5jdGlvbiI9PXR5cGVvZiB0P3U6cn1mdW5jdGlvbiBUKG4p
e3JldHVybiJmdW5jdGlvbiI9PXR5cGVvZiBuP246KG49Vm8ubnMucXVhbGlmeShu
KSkubG9jYWw/ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5vd25lckRvY3VtZW50LmNy
ZWF0ZUVsZW1lbnROUyhuLnNwYWNlLG4ubG9jYWwpfTpmdW5jdGlvbigpe3JldHVy
biB0aGlzLm93bmVyRG9jdW1lbnQuY3JlYXRlRWxlbWVudE5TKHRoaXMubmFtZXNw
YWNlVVJJLG4pfX1mdW5jdGlvbiBxKG4pe3JldHVybntfX2RhdGFfXzpufX1mdW5j
dGlvbiBSKG4pe3JldHVybiBmdW5jdGlvbigpe3JldHVybiBwYSh0aGlzLG4pfX1m
dW5jdGlvbiBEKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RofHwodD1uKSxmdW5j
dGlvbihuLGUpe3JldHVybiBuJiZlP3Qobi5fX2RhdGFfXyxlLl9fZGF0YV9fKToh
bi0hZX19ZnVuY3Rpb24gUChuLHQpe2Zvcih2YXIgZT0wLHI9bi5sZW5ndGg7cj5l
O2UrKylmb3IodmFyIHUsaT1uW2VdLG89MCxhPWkubGVuZ3RoO2E+bztvKyspKHU9
aVtvXSkmJnQodSxvLGUpO3JldHVybiBufWZ1bmN0aW9uIFUobil7cmV0dXJuIHNh
KG4sbWEpLG59ZnVuY3Rpb24gaihuKXt2YXIgdCxlO3JldHVybiBmdW5jdGlvbihy
LHUsaSl7dmFyIG8sYT1uW2ldLnVwZGF0ZSxjPWEubGVuZ3RoO2ZvcihpIT1lJiYo
ZT1pLHQ9MCksdT49dCYmKHQ9dSsxKTshKG89YVt0XSkmJisrdDxjOyk7cmV0dXJu
IG99fWZ1bmN0aW9uIEgoKXt2YXIgbj10aGlzLl9fdHJhbnNpdGlvbl9fO24mJisr
bi5hY3RpdmV9ZnVuY3Rpb24gRihuLHQsZSl7ZnVuY3Rpb24gcigpe3ZhciB0PXRo
aXNbb107dCYmKHRoaXMucmVtb3ZlRXZlbnRMaXN0ZW5lcihuLHQsdC4kKSxkZWxl
dGUgdGhpc1tvXSl9ZnVuY3Rpb24gdSgpe3ZhciB1PWModCwkbyhhcmd1bWVudHMp
KTtyLmNhbGwodGhpcyksdGhpcy5hZGRFdmVudExpc3RlbmVyKG4sdGhpc1tvXT11
LHUuJD1lKSx1Ll89dH1mdW5jdGlvbiBpKCl7dmFyIHQsZT1uZXcgUmVnRXhwKCJe
X19vbihbXi5dKykiK1ZvLnJlcXVvdGUobikrIiQiKTtmb3IodmFyIHIgaW4gdGhp
cylpZih0PXIubWF0Y2goZSkpe3ZhciB1PXRoaXNbcl07dGhpcy5yZW1vdmVFdmVu
dExpc3RlbmVyKHRbMV0sdSx1LiQpLGRlbGV0ZSB0aGlzW3JdfX12YXIgbz0iX19v
biIrbixhPW4uaW5kZXhPZigiLiIpLGM9TzthPjAmJihuPW4uc2xpY2UoMCxhKSk7
dmFyIGw9eGEuZ2V0KG4pO3JldHVybiBsJiYobj1sLGM9WSksYT90P3U6cjp0P3Y6
aX1mdW5jdGlvbiBPKG4sdCl7cmV0dXJuIGZ1bmN0aW9uKGUpe3ZhciByPVZvLmV2
ZW50O1ZvLmV2ZW50PWUsdFswXT10aGlzLl9fZGF0YV9fO3RyeXtuLmFwcGx5KHRo
aXMsdCl9ZmluYWxseXtWby5ldmVudD1yfX19ZnVuY3Rpb24gWShuLHQpe3ZhciBl
PU8obix0KTtyZXR1cm4gZnVuY3Rpb24obil7dmFyIHQ9dGhpcyxyPW4ucmVsYXRl
ZFRhcmdldDtyJiYocj09PXR8fDgmci5jb21wYXJlRG9jdW1lbnRQb3NpdGlvbih0
KSl8fGUuY2FsbCh0LG4pfX1mdW5jdGlvbiBJKCl7dmFyIG49Ii5kcmFnc3VwcHJl
c3MtIisgKytfYSx0PSJjbGljayIrbixlPVZvLnNlbGVjdChKbykub24oInRvdWNo
bW92ZSIrbix5KS5vbigiZHJhZ3N0YXJ0IituLHkpLm9uKCJzZWxlY3RzdGFydCIr
bix5KTtpZihNYSl7dmFyIHI9V28uc3R5bGUsdT1yW01hXTtyW01hXT0ibm9uZSJ9
cmV0dXJuIGZ1bmN0aW9uKGkpe2Z1bmN0aW9uIG8oKXtlLm9uKHQsbnVsbCl9ZS5v
bihuLG51bGwpLE1hJiYocltNYV09dSksaSYmKGUub24odCxmdW5jdGlvbigpe3ko
KSxvKCl9LCEwKSxzZXRUaW1lb3V0KG8sMCkpfX1mdW5jdGlvbiBaKG4sdCl7dC5j
aGFuZ2VkVG91Y2hlcyYmKHQ9dC5jaGFuZ2VkVG91Y2hlc1swXSk7dmFyIGU9bi5v
d25lclNWR0VsZW1lbnR8fG47aWYoZS5jcmVhdGVTVkdQb2ludCl7dmFyIHI9ZS5j
cmVhdGVTVkdQb2ludCgpO2lmKDA+YmEmJihKby5zY3JvbGxYfHxKby5zY3JvbGxZ
KSl7ZT1Wby5zZWxlY3QoImJvZHkiKS5hcHBlbmQoInN2ZyIpLnN0eWxlKHtwb3Np
dGlvbjoiYWJzb2x1dGUiLHRvcDowLGxlZnQ6MCxtYXJnaW46MCxwYWRkaW5nOjAs
Ym9yZGVyOiJub25lIn0sImltcG9ydGFudCIpO3ZhciB1PWVbMF1bMF0uZ2V0U2Ny
ZWVuQ1RNKCk7YmE9ISh1LmZ8fHUuZSksZS5yZW1vdmUoKX1yZXR1cm4gYmE/KHIu
eD10LnBhZ2VYLHIueT10LnBhZ2VZKTooci54PXQuY2xpZW50WCxyLnk9dC5jbGll
bnRZKSxyPXIubWF0cml4VHJhbnNmb3JtKG4uZ2V0U2NyZWVuQ1RNKCkuaW52ZXJz
ZSgpKSxbci54LHIueV19dmFyIGk9bi5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKTty
ZXR1cm5bdC5jbGllbnRYLWkubGVmdC1uLmNsaWVudExlZnQsdC5jbGllbnRZLWku
dG9wLW4uY2xpZW50VG9wXX1mdW5jdGlvbiBWKCl7cmV0dXJuIFZvLmV2ZW50LmNo
YW5nZWRUb3VjaGVzWzBdLmlkZW50aWZpZXJ9ZnVuY3Rpb24gWCgpe3JldHVybiBW
by5ldmVudC50YXJnZXR9ZnVuY3Rpb24gJCgpe3JldHVybiBKb31mdW5jdGlvbiBC
KG4pe3JldHVybiBuPjA/MTowPm4/LTE6MH1mdW5jdGlvbiBXKG4sdCxlKXtyZXR1
cm4odFswXS1uWzBdKSooZVsxXS1uWzFdKS0odFsxXS1uWzFdKSooZVswXS1uWzBd
KX1mdW5jdGlvbiBKKG4pe3JldHVybiBuPjE/MDotMT5uP3dhOk1hdGguYWNvcyhu
KX1mdW5jdGlvbiBHKG4pe3JldHVybiBuPjE/a2E6LTE+bj8ta2E6TWF0aC5hc2lu
KG4pfWZ1bmN0aW9uIEsobil7cmV0dXJuKChuPU1hdGguZXhwKG4pKS0xL24pLzJ9
ZnVuY3Rpb24gUShuKXtyZXR1cm4oKG49TWF0aC5leHAobikpKzEvbikvMn1mdW5j
dGlvbiBudChuKXtyZXR1cm4oKG49TWF0aC5leHAoMipuKSktMSkvKG4rMSl9ZnVu
Y3Rpb24gdHQobil7cmV0dXJuKG49TWF0aC5zaW4obi8yKSkqbn1mdW5jdGlvbiBl
dCgpe31mdW5jdGlvbiBydChuLHQsZSl7cmV0dXJuIHRoaXMgaW5zdGFuY2VvZiBy
dD8odGhpcy5oPStuLHRoaXMucz0rdCx2b2lkKHRoaXMubD0rZSkpOmFyZ3VtZW50
cy5sZW5ndGg8Mj9uIGluc3RhbmNlb2YgcnQ/bmV3IHJ0KG4uaCxuLnMsbi5sKTpt
dCgiIituLHl0LHJ0KTpuZXcgcnQobix0LGUpfWZ1bmN0aW9uIHV0KG4sdCxlKXtm
dW5jdGlvbiByKG4pe3JldHVybiBuPjM2MD9uLT0zNjA6MD5uJiYobis9MzYwKSw2
MD5uP2krKG8taSkqbi82MDoxODA+bj9vOjI0MD5uP2krKG8taSkqKDI0MC1uKS82
MDppfWZ1bmN0aW9uIHUobil7cmV0dXJuIE1hdGgucm91bmQoMjU1KnIobikpfXZh
ciBpLG87cmV0dXJuIG49aXNOYU4obik/MDoobiU9MzYwKTwwP24rMzYwOm4sdD1p
c05hTih0KT8wOjA+dD8wOnQ+MT8xOnQsZT0wPmU/MDplPjE/MTplLG89LjU+PWU/
ZSooMSt0KTplK3QtZSp0LGk9MiplLW8sbmV3IGd0KHUobisxMjApLHUobiksdShu
LTEyMCkpfWZ1bmN0aW9uIGl0KG4sdCxlKXtyZXR1cm4gdGhpcyBpbnN0YW5jZW9m
IGl0Pyh0aGlzLmg9K24sdGhpcy5jPSt0LHZvaWQodGhpcy5sPStlKSk6YXJndW1l
bnRzLmxlbmd0aDwyP24gaW5zdGFuY2VvZiBpdD9uZXcgaXQobi5oLG4uYyxuLmwp
Om4gaW5zdGFuY2VvZiBhdD9sdChuLmwsbi5hLG4uYik6bHQoKG49eHQoKG49Vm8u
cmdiKG4pKS5yLG4uZyxuLmIpKS5sLG4uYSxuLmIpOm5ldyBpdChuLHQsZSl9ZnVu
Y3Rpb24gb3Qobix0LGUpe3JldHVybiBpc05hTihuKSYmKG49MCksaXNOYU4odCkm
Jih0PTApLG5ldyBhdChlLE1hdGguY29zKG4qPUNhKSp0LE1hdGguc2luKG4pKnQp
fWZ1bmN0aW9uIGF0KG4sdCxlKXtyZXR1cm4gdGhpcyBpbnN0YW5jZW9mIGF0Pyh0
aGlzLmw9K24sdGhpcy5hPSt0LHZvaWQodGhpcy5iPStlKSk6YXJndW1lbnRzLmxl
bmd0aDwyP24gaW5zdGFuY2VvZiBhdD9uZXcgYXQobi5sLG4uYSxuLmIpOm4gaW5z
dGFuY2VvZiBpdD9vdChuLmwsbi5jLG4uaCk6eHQoKG49Z3QobikpLnIsbi5nLG4u
Yik6bmV3IGF0KG4sdCxlKX1mdW5jdGlvbiBjdChuLHQsZSl7dmFyIHI9KG4rMTYp
LzExNix1PXIrdC81MDAsaT1yLWUvMjAwO3JldHVybiB1PXN0KHUpKkhhLHI9c3Qo
cikqRmEsaT1zdChpKSpPYSxuZXcgZ3QoaHQoMy4yNDA0NTQyKnUtMS41MzcxMzg1
KnItLjQ5ODUzMTQqaSksaHQoLS45NjkyNjYqdSsxLjg3NjAxMDgqcisuMDQxNTU2
KmkpLGh0KC4wNTU2NDM0KnUtLjIwNDAyNTkqcisxLjA1NzIyNTIqaSkpfWZ1bmN0
aW9uIGx0KG4sdCxlKXtyZXR1cm4gbj4wP25ldyBpdChNYXRoLmF0YW4yKGUsdCkq
TmEsTWF0aC5zcXJ0KHQqdCtlKmUpLG4pOm5ldyBpdCgwLzAsMC8wLG4pfWZ1bmN0
aW9uIHN0KG4pe3JldHVybiBuPi4yMDY4OTMwMzQ/bipuKm46KG4tNC8yOSkvNy43
ODcwMzd9ZnVuY3Rpb24gZnQobil7cmV0dXJuIG4+LjAwODg1Nj9NYXRoLnBvdyhu
LDEvMyk6Ny43ODcwMzcqbis0LzI5fWZ1bmN0aW9uIGh0KG4pe3JldHVybiBNYXRo
LnJvdW5kKDI1NSooLjAwMzA0Pj1uPzEyLjkyKm46MS4wNTUqTWF0aC5wb3cobiwx
LzIuNCktLjA1NSkpfWZ1bmN0aW9uIGd0KG4sdCxlKXtyZXR1cm4gdGhpcyBpbnN0
YW5jZW9mIGd0Pyh0aGlzLnI9fn5uLHRoaXMuZz1+fnQsdm9pZCh0aGlzLmI9fn5l
KSk6YXJndW1lbnRzLmxlbmd0aDwyP24gaW5zdGFuY2VvZiBndD9uZXcgZ3Qobi5y
LG4uZyxuLmIpOm10KCIiK24sZ3QsdXQpOm5ldyBndChuLHQsZSl9ZnVuY3Rpb24g
cHQobil7cmV0dXJuIG5ldyBndChuPj4xNiwyNTUmbj4+OCwyNTUmbil9ZnVuY3Rp
b24gdnQobil7cmV0dXJuIHB0KG4pKyIifWZ1bmN0aW9uIGR0KG4pe3JldHVybiAx
Nj5uPyIwIitNYXRoLm1heCgwLG4pLnRvU3RyaW5nKDE2KTpNYXRoLm1pbigyNTUs
bikudG9TdHJpbmcoMTYpfWZ1bmN0aW9uIG10KG4sdCxlKXt2YXIgcix1LGksbz0w
LGE9MCxjPTA7aWYocj0vKFthLXpdKylcKCguKilcKS9pLmV4ZWMobikpc3dpdGNo
KHU9clsyXS5zcGxpdCgiLCIpLHJbMV0pe2Nhc2UiaHNsIjpyZXR1cm4gZShwYXJz
ZUZsb2F0KHVbMF0pLHBhcnNlRmxvYXQodVsxXSkvMTAwLHBhcnNlRmxvYXQodVsy
XSkvMTAwKTtjYXNlInJnYiI6cmV0dXJuIHQoX3QodVswXSksX3QodVsxXSksX3Qo
dVsyXSkpfXJldHVybihpPVphLmdldChuKSk/dChpLnIsaS5nLGkuYik6KG51bGw9
PW58fCIjIiE9PW4uY2hhckF0KDApfHxpc05hTihpPXBhcnNlSW50KG4uc2xpY2Uo
MSksMTYpKXx8KDQ9PT1uLmxlbmd0aD8obz0oMzg0MCZpKT4+NCxvPW8+PjR8byxh
PTI0MCZpLGE9YT4+NHxhLGM9MTUmaSxjPWM8PDR8Yyk6Nz09PW4ubGVuZ3RoJiYo
bz0oMTY3MTE2ODAmaSk+PjE2LGE9KDY1MjgwJmkpPj44LGM9MjU1JmkpKSx0KG8s
YSxjKSl9ZnVuY3Rpb24geXQobix0LGUpe3ZhciByLHUsaT1NYXRoLm1pbihuLz0y
NTUsdC89MjU1LGUvPTI1NSksbz1NYXRoLm1heChuLHQsZSksYT1vLWksYz0obytp
KS8yO3JldHVybiBhPyh1PS41PmM/YS8obytpKTphLygyLW8taSkscj1uPT1vPyh0
LWUpL2ErKGU+dD82OjApOnQ9PW8/KGUtbikvYSsyOihuLXQpL2ErNCxyKj02MCk6
KHI9MC8wLHU9Yz4wJiYxPmM/MDpyKSxuZXcgcnQocix1LGMpfWZ1bmN0aW9uIHh0
KG4sdCxlKXtuPU10KG4pLHQ9TXQodCksZT1NdChlKTt2YXIgcj1mdCgoLjQxMjQ1
NjQqbisuMzU3NTc2MSp0Ky4xODA0Mzc1KmUpL0hhKSx1PWZ0KCguMjEyNjcyOSpu
Ky43MTUxNTIyKnQrLjA3MjE3NSplKS9GYSksaT1mdCgoLjAxOTMzMzkqbisuMTE5
MTkyKnQrLjk1MDMwNDEqZSkvT2EpO3JldHVybiBhdCgxMTYqdS0xNiw1MDAqKHIt
dSksMjAwKih1LWkpKX1mdW5jdGlvbiBNdChuKXtyZXR1cm4obi89MjU1KTw9LjA0
MDQ1P24vMTIuOTI6TWF0aC5wb3coKG4rLjA1NSkvMS4wNTUsMi40KX1mdW5jdGlv
biBfdChuKXt2YXIgdD1wYXJzZUZsb2F0KG4pO3JldHVybiIlIj09PW4uY2hhckF0
KG4ubGVuZ3RoLTEpP01hdGgucm91bmQoMi41NSp0KTp0fWZ1bmN0aW9uIGJ0KG4p
e3JldHVybiJmdW5jdGlvbiI9PXR5cGVvZiBuP246ZnVuY3Rpb24oKXtyZXR1cm4g
bn19ZnVuY3Rpb24gd3Qobil7cmV0dXJuIG59ZnVuY3Rpb24gU3Qobil7cmV0dXJu
IGZ1bmN0aW9uKHQsZSxyKXtyZXR1cm4gMj09PWFyZ3VtZW50cy5sZW5ndGgmJiJm
dW5jdGlvbiI9PXR5cGVvZiBlJiYocj1lLGU9bnVsbCksa3QodCxlLG4scil9fWZ1
bmN0aW9uIGt0KG4sdCxlLHIpe2Z1bmN0aW9uIHUoKXt2YXIgbix0PWMuc3RhdHVz
O2lmKCF0JiZBdChjKXx8dD49MjAwJiYzMDA+dHx8MzA0PT09dCl7dHJ5e249ZS5j
YWxsKGksYyl9Y2F0Y2gocil7cmV0dXJuIG8uZXJyb3IuY2FsbChpLHIpLHZvaWQg
MH1vLmxvYWQuY2FsbChpLG4pfWVsc2Ugby5lcnJvci5jYWxsKGksYyl9dmFyIGk9
e30sbz1Wby5kaXNwYXRjaCgiYmVmb3Jlc2VuZCIsInByb2dyZXNzIiwibG9hZCIs
ImVycm9yIiksYT17fSxjPW5ldyBYTUxIdHRwUmVxdWVzdCxsPW51bGw7cmV0dXJu
IUpvLlhEb21haW5SZXF1ZXN0fHwid2l0aENyZWRlbnRpYWxzImluIGN8fCEvXiho
dHRwKHMpPzopP1wvXC8vLnRlc3Qobil8fChjPW5ldyBYRG9tYWluUmVxdWVzdCks
Im9ubG9hZCJpbiBjP2Mub25sb2FkPWMub25lcnJvcj11OmMub25yZWFkeXN0YXRl
Y2hhbmdlPWZ1bmN0aW9uKCl7Yy5yZWFkeVN0YXRlPjMmJnUoKX0sYy5vbnByb2dy
ZXNzPWZ1bmN0aW9uKG4pe3ZhciB0PVZvLmV2ZW50O1ZvLmV2ZW50PW47dHJ5e28u
cHJvZ3Jlc3MuY2FsbChpLGMpfWZpbmFsbHl7Vm8uZXZlbnQ9dH19LGkuaGVhZGVy
PWZ1bmN0aW9uKG4sdCl7cmV0dXJuIG49KG4rIiIpLnRvTG93ZXJDYXNlKCksYXJn
dW1lbnRzLmxlbmd0aDwyP2Fbbl06KG51bGw9PXQ/ZGVsZXRlIGFbbl06YVtuXT10
KyIiLGkpfSxpLm1pbWVUeXBlPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMu
bGVuZ3RoPyh0PW51bGw9PW4/bnVsbDpuKyIiLGkpOnR9LGkucmVzcG9uc2VUeXBl
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhsPW4saSk6bH0s
aS5yZXNwb25zZT1mdW5jdGlvbihuKXtyZXR1cm4gZT1uLGl9LFsiZ2V0IiwicG9z
dCJdLmZvckVhY2goZnVuY3Rpb24obil7aVtuXT1mdW5jdGlvbigpe3JldHVybiBp
LnNlbmQuYXBwbHkoaSxbbl0uY29uY2F0KCRvKGFyZ3VtZW50cykpKX19KSxpLnNl
bmQ9ZnVuY3Rpb24oZSxyLHUpe2lmKDI9PT1hcmd1bWVudHMubGVuZ3RoJiYiZnVu
Y3Rpb24iPT10eXBlb2YgciYmKHU9cixyPW51bGwpLGMub3BlbihlLG4sITApLG51
bGw9PXR8fCJhY2NlcHQiaW4gYXx8KGEuYWNjZXB0PXQrIiwqLyoiKSxjLnNldFJl
cXVlc3RIZWFkZXIpZm9yKHZhciBzIGluIGEpYy5zZXRSZXF1ZXN0SGVhZGVyKHMs
YVtzXSk7cmV0dXJuIG51bGwhPXQmJmMub3ZlcnJpZGVNaW1lVHlwZSYmYy5vdmVy
cmlkZU1pbWVUeXBlKHQpLG51bGwhPWwmJihjLnJlc3BvbnNlVHlwZT1sKSxudWxs
IT11JiZpLm9uKCJlcnJvciIsdSkub24oImxvYWQiLGZ1bmN0aW9uKG4pe3UobnVs
bCxuKX0pLG8uYmVmb3Jlc2VuZC5jYWxsKGksYyksYy5zZW5kKG51bGw9PXI/bnVs
bDpyKSxpfSxpLmFib3J0PWZ1bmN0aW9uKCl7cmV0dXJuIGMuYWJvcnQoKSxpfSxW
by5yZWJpbmQoaSxvLCJvbiIpLG51bGw9PXI/aTppLmdldChFdChyKSl9ZnVuY3Rp
b24gRXQobil7cmV0dXJuIDE9PT1uLmxlbmd0aD9mdW5jdGlvbih0LGUpe24obnVs
bD09dD9lOm51bGwpfTpufWZ1bmN0aW9uIEF0KG4pe3ZhciB0PW4ucmVzcG9uc2VU
eXBlO3JldHVybiB0JiYidGV4dCIhPT10P24ucmVzcG9uc2U6bi5yZXNwb25zZVRl
eHR9ZnVuY3Rpb24gQ3QoKXt2YXIgbj1OdCgpLHQ9enQoKS1uO3Q+MjQ/KGlzRmlu
aXRlKHQpJiYoY2xlYXJUaW1lb3V0KEJhKSxCYT1zZXRUaW1lb3V0KEN0LHQpKSwk
YT0wKTooJGE9MSxKYShDdCkpfWZ1bmN0aW9uIE50KCl7dmFyIG49RGF0ZS5ub3co
KTtmb3IoV2E9VmE7V2E7KW4+PVdhLnQmJihXYS5mPVdhLmMobi1XYS50KSksV2E9
V2EubjtyZXR1cm4gbn1mdW5jdGlvbiB6dCgpe2Zvcih2YXIgbix0PVZhLGU9MS8w
O3Q7KXQuZj90PW4/bi5uPXQubjpWYT10Lm46KHQudDxlJiYoZT10LnQpLHQ9KG49
dCkubik7cmV0dXJuIFhhPW4sZX1mdW5jdGlvbiBMdChuLHQpe3JldHVybiB0LShu
P01hdGguY2VpbChNYXRoLmxvZyhuKS9NYXRoLkxOMTApOjEpfWZ1bmN0aW9uIFR0
KG4sdCl7dmFyIGU9TWF0aC5wb3coMTAsMyppYSg4LXQpKTtyZXR1cm57c2NhbGU6
dD44P2Z1bmN0aW9uKG4pe3JldHVybiBuL2V9OmZ1bmN0aW9uKG4pe3JldHVybiBu
KmV9LHN5bWJvbDpufX1mdW5jdGlvbiBxdChuKXt2YXIgdD1uLmRlY2ltYWwsZT1u
LnRob3VzYW5kcyxyPW4uZ3JvdXBpbmcsdT1uLmN1cnJlbmN5LGk9cj9mdW5jdGlv
bihuKXtmb3IodmFyIHQ9bi5sZW5ndGgsdT1bXSxpPTAsbz1yWzBdO28+MCYmdD4w
Oyl1LnB1c2gobi5zdWJzdHJpbmcodC09byx0K28pKSxvPXJbaT0oaSsxKSVyLmxl
bmd0aF07cmV0dXJuIHUucmV2ZXJzZSgpLmpvaW4oZSl9Ond0O3JldHVybiBmdW5j
dGlvbihuKXt2YXIgZT1LYS5leGVjKG4pLHI9ZVsxXXx8IiAiLG89ZVsyXXx8Ij4i
LGE9ZVszXXx8IiIsYz1lWzRdfHwiIixsPWVbNV0scz0rZVs2XSxmPWVbN10saD1l
WzhdLGc9ZVs5XSxwPTEsdj0iIixkPSIiLG09ITE7c3dpdGNoKGgmJihoPStoLnN1
YnN0cmluZygxKSksKGx8fCIwIj09PXImJiI9Ij09PW8pJiYobD1yPSIwIixvPSI9
IixmJiYocy09TWF0aC5mbG9vcigocy0xKS80KSkpLGcpe2Nhc2UibiI6Zj0hMCxn
PSJnIjticmVhaztjYXNlIiUiOnA9MTAwLGQ9IiUiLGc9ImYiO2JyZWFrO2Nhc2Ui
cCI6cD0xMDAsZD0iJSIsZz0iciI7YnJlYWs7Y2FzZSJiIjpjYXNlIm8iOmNhc2Ui
eCI6Y2FzZSJYIjoiIyI9PT1jJiYodj0iMCIrZy50b0xvd2VyQ2FzZSgpKTtjYXNl
ImMiOmNhc2UiZCI6bT0hMCxoPTA7YnJlYWs7Y2FzZSJzIjpwPS0xLGc9InIifSIk
Ij09PWMmJih2PXVbMF0sZD11WzFdKSwiciIhPWd8fGh8fChnPSJnIiksbnVsbCE9
aCYmKCJnIj09Zz9oPU1hdGgubWF4KDEsTWF0aC5taW4oMjEsaCkpOigiZSI9PWd8
fCJmIj09ZykmJihoPU1hdGgubWF4KDAsTWF0aC5taW4oMjAsaCkpKSksZz1RYS5n
ZXQoZyl8fFJ0O3ZhciB5PWwmJmY7cmV0dXJuIGZ1bmN0aW9uKG4pe3ZhciBlPWQ7
aWYobSYmbiUxKXJldHVybiIiO3ZhciB1PTA+bnx8MD09PW4mJjA+MS9uPyhuPS1u
LCItIik6YTtpZigwPnApe3ZhciBjPVZvLmZvcm1hdFByZWZpeChuLGgpO249Yy5z
Y2FsZShuKSxlPWMuc3ltYm9sK2R9ZWxzZSBuKj1wO249ZyhuLGgpO3ZhciB4PW4u
bGFzdEluZGV4T2YoIi4iKSxNPTA+eD9uOm4uc3Vic3RyaW5nKDAseCksXz0wPng/
IiI6dCtuLnN1YnN0cmluZyh4KzEpOyFsJiZmJiYoTT1pKE0pKTt2YXIgYj12Lmxl
bmd0aCtNLmxlbmd0aCtfLmxlbmd0aCsoeT8wOnUubGVuZ3RoKSx3PXM+Yj9uZXcg
QXJyYXkoYj1zLWIrMSkuam9pbihyKToiIjtyZXR1cm4geSYmKE09aSh3K00pKSx1
Kz12LG49TStfLCgiPCI9PT1vP3Urbit3OiI+Ij09PW8/dyt1K246Il4iPT09bz93
LnN1YnN0cmluZygwLGI+Pj0xKSt1K24rdy5zdWJzdHJpbmcoYik6dSsoeT9uOncr
bikpK2V9fX1mdW5jdGlvbiBSdChuKXtyZXR1cm4gbisiIn1mdW5jdGlvbiBEdCgp
e3RoaXMuXz1uZXcgRGF0ZShhcmd1bWVudHMubGVuZ3RoPjE/RGF0ZS5VVEMuYXBw
bHkodGhpcyxhcmd1bWVudHMpOmFyZ3VtZW50c1swXSl9ZnVuY3Rpb24gUHQobix0
LGUpe2Z1bmN0aW9uIHIodCl7dmFyIGU9bih0KSxyPWkoZSwxKTtyZXR1cm4gci10
PnQtZT9lOnJ9ZnVuY3Rpb24gdShlKXtyZXR1cm4gdChlPW4obmV3IHRjKGUtMSkp
LDEpLGV9ZnVuY3Rpb24gaShuLGUpe3JldHVybiB0KG49bmV3IHRjKCtuKSxlKSxu
fWZ1bmN0aW9uIG8obixyLGkpe3ZhciBvPXUobiksYT1bXTtpZihpPjEpZm9yKDty
Pm87KWUobyklaXx8YS5wdXNoKG5ldyBEYXRlKCtvKSksdChvLDEpO2Vsc2UgZm9y
KDtyPm87KWEucHVzaChuZXcgRGF0ZSgrbykpLHQobywxKTtyZXR1cm4gYX1mdW5j
dGlvbiBhKG4sdCxlKXt0cnl7dGM9RHQ7dmFyIHI9bmV3IER0O3JldHVybiByLl89
bixvKHIsdCxlKX1maW5hbGx5e3RjPURhdGV9fW4uZmxvb3I9bixuLnJvdW5kPXIs
bi5jZWlsPXUsbi5vZmZzZXQ9aSxuLnJhbmdlPW87dmFyIGM9bi51dGM9VXQobik7
cmV0dXJuIGMuZmxvb3I9YyxjLnJvdW5kPVV0KHIpLGMuY2VpbD1VdCh1KSxjLm9m
ZnNldD1VdChpKSxjLnJhbmdlPWEsbn1mdW5jdGlvbiBVdChuKXtyZXR1cm4gZnVu
Y3Rpb24odCxlKXt0cnl7dGM9RHQ7dmFyIHI9bmV3IER0O3JldHVybiByLl89dCxu
KHIsZSkuX31maW5hbGx5e3RjPURhdGV9fX1mdW5jdGlvbiBqdChuKXtmdW5jdGlv
biB0KG4pe2Z1bmN0aW9uIHQodCl7Zm9yKHZhciBlLHUsaSxvPVtdLGE9LTEsYz0w
OysrYTxyOykzNz09PW4uY2hhckNvZGVBdChhKSYmKG8ucHVzaChuLnNsaWNlKGMs
YSkpLG51bGwhPSh1PXJjW2U9bi5jaGFyQXQoKythKV0pJiYoZT1uLmNoYXJBdCgr
K2EpKSwoaT1DW2VdKSYmKGU9aSh0LG51bGw9PXU/ImUiPT09ZT8iICI6IjAiOnUp
KSxvLnB1c2goZSksYz1hKzEpO3JldHVybiBvLnB1c2gobi5zbGljZShjLGEpKSxv
LmpvaW4oIiIpfXZhciByPW4ubGVuZ3RoO3JldHVybiB0LnBhcnNlPWZ1bmN0aW9u
KHQpe3ZhciByPXt5OjE5MDAsbTowLGQ6MSxIOjAsTTowLFM6MCxMOjAsWjpudWxs
fSx1PWUocixuLHQsMCk7aWYodSE9dC5sZW5ndGgpcmV0dXJuIG51bGw7InAiaW4g
ciYmKHIuSD1yLkglMTIrMTIqci5wKTt2YXIgaT1udWxsIT1yLlomJnRjIT09RHQs
bz1uZXcoaT9EdDp0Yyk7cmV0dXJuImoiaW4gcj9vLnNldEZ1bGxZZWFyKHIueSww
LHIuaik6InciaW4gciYmKCJXImluIHJ8fCJVImluIHIpPyhvLnNldEZ1bGxZZWFy
KHIueSwwLDEpLG8uc2V0RnVsbFllYXIoci55LDAsIlciaW4gcj8oci53KzYpJTcr
NypyLlctKG8uZ2V0RGF5KCkrNSklNzpyLncrNypyLlUtKG8uZ2V0RGF5KCkrNikl
NykpOm8uc2V0RnVsbFllYXIoci55LHIubSxyLmQpLG8uc2V0SG91cnMoci5IKygw
fHIuWi8xMDApLHIuTStyLlolMTAwLHIuUyxyLkwpLGk/by5fOm99LHQudG9TdHJp
bmc9ZnVuY3Rpb24oKXtyZXR1cm4gbn0sdH1mdW5jdGlvbiBlKG4sdCxlLHIpe2Zv
cih2YXIgdSxpLG8sYT0wLGM9dC5sZW5ndGgsbD1lLmxlbmd0aDtjPmE7KXtpZihy
Pj1sKXJldHVybi0xO2lmKHU9dC5jaGFyQ29kZUF0KGErKyksMzc9PT11KXtpZihv
PXQuY2hhckF0KGErKyksaT1OW28gaW4gcmM/dC5jaGFyQXQoYSsrKTpvXSwhaXx8
KHI9aShuLGUscikpPDApcmV0dXJuLTF9ZWxzZSBpZih1IT1lLmNoYXJDb2RlQXQo
cisrKSlyZXR1cm4tMX1yZXR1cm4gcn1mdW5jdGlvbiByKG4sdCxlKXtiLmxhc3RJ
bmRleD0wO3ZhciByPWIuZXhlYyh0LnNsaWNlKGUpKTtyZXR1cm4gcj8obi53PXcu
Z2V0KHJbMF0udG9Mb3dlckNhc2UoKSksZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rp
b24gdShuLHQsZSl7TS5sYXN0SW5kZXg9MDt2YXIgcj1NLmV4ZWModC5zbGljZShl
KSk7cmV0dXJuIHI/KG4udz1fLmdldChyWzBdLnRvTG93ZXJDYXNlKCkpLGUrclsw
XS5sZW5ndGgpOi0xfWZ1bmN0aW9uIGkobix0LGUpe0UubGFzdEluZGV4PTA7dmFy
IHI9RS5leGVjKHQuc2xpY2UoZSkpO3JldHVybiByPyhuLm09QS5nZXQoclswXS50
b0xvd2VyQ2FzZSgpKSxlK3JbMF0ubGVuZ3RoKTotMX1mdW5jdGlvbiBvKG4sdCxl
KXtTLmxhc3RJbmRleD0wO3ZhciByPVMuZXhlYyh0LnNsaWNlKGUpKTtyZXR1cm4g
cj8obi5tPWsuZ2V0KHJbMF0udG9Mb3dlckNhc2UoKSksZStyWzBdLmxlbmd0aCk6
LTF9ZnVuY3Rpb24gYShuLHQscil7cmV0dXJuIGUobixDLmMudG9TdHJpbmcoKSx0
LHIpfWZ1bmN0aW9uIGMobix0LHIpe3JldHVybiBlKG4sQy54LnRvU3RyaW5nKCks
dCxyKX1mdW5jdGlvbiBsKG4sdCxyKXtyZXR1cm4gZShuLEMuWC50b1N0cmluZygp
LHQscil9ZnVuY3Rpb24gcyhuLHQsZSl7dmFyIHI9eC5nZXQodC5zbGljZShlLGUr
PTIpLnRvTG93ZXJDYXNlKCkpO3JldHVybiBudWxsPT1yPy0xOihuLnA9cixlKX12
YXIgZj1uLmRhdGVUaW1lLGg9bi5kYXRlLGc9bi50aW1lLHA9bi5wZXJpb2RzLHY9
bi5kYXlzLGQ9bi5zaG9ydERheXMsbT1uLm1vbnRocyx5PW4uc2hvcnRNb250aHM7
dC51dGM9ZnVuY3Rpb24obil7ZnVuY3Rpb24gZShuKXt0cnl7dGM9RHQ7dmFyIHQ9
bmV3IHRjO3JldHVybiB0Ll89bixyKHQpfWZpbmFsbHl7dGM9RGF0ZX19dmFyIHI9
dChuKTtyZXR1cm4gZS5wYXJzZT1mdW5jdGlvbihuKXt0cnl7dGM9RHQ7dmFyIHQ9
ci5wYXJzZShuKTtyZXR1cm4gdCYmdC5ffWZpbmFsbHl7dGM9RGF0ZX19LGUudG9T
dHJpbmc9ci50b1N0cmluZyxlfSx0Lm11bHRpPXQudXRjLm11bHRpPXVlO3ZhciB4
PVZvLm1hcCgpLE09RnQodiksXz1PdCh2KSxiPUZ0KGQpLHc9T3QoZCksUz1GdCht
KSxrPU90KG0pLEU9RnQoeSksQT1PdCh5KTtwLmZvckVhY2goZnVuY3Rpb24obix0
KXt4LnNldChuLnRvTG93ZXJDYXNlKCksdCl9KTt2YXIgQz17YTpmdW5jdGlvbihu
KXtyZXR1cm4gZFtuLmdldERheSgpXX0sQTpmdW5jdGlvbihuKXtyZXR1cm4gdltu
LmdldERheSgpXX0sYjpmdW5jdGlvbihuKXtyZXR1cm4geVtuLmdldE1vbnRoKCld
fSxCOmZ1bmN0aW9uKG4pe3JldHVybiBtW24uZ2V0TW9udGgoKV19LGM6dChmKSxk
OmZ1bmN0aW9uKG4sdCl7cmV0dXJuIEh0KG4uZ2V0RGF0ZSgpLHQsMil9LGU6ZnVu
Y3Rpb24obix0KXtyZXR1cm4gSHQobi5nZXREYXRlKCksdCwyKX0sSDpmdW5jdGlv
bihuLHQpe3JldHVybiBIdChuLmdldEhvdXJzKCksdCwyKX0sSTpmdW5jdGlvbihu
LHQpe3JldHVybiBIdChuLmdldEhvdXJzKCklMTJ8fDEyLHQsMil9LGo6ZnVuY3Rp
b24obix0KXtyZXR1cm4gSHQoMStuYy5kYXlPZlllYXIobiksdCwzKX0sTDpmdW5j
dGlvbihuLHQpe3JldHVybiBIdChuLmdldE1pbGxpc2Vjb25kcygpLHQsMyl9LG06
ZnVuY3Rpb24obix0KXtyZXR1cm4gSHQobi5nZXRNb250aCgpKzEsdCwyKX0sTTpm
dW5jdGlvbihuLHQpe3JldHVybiBIdChuLmdldE1pbnV0ZXMoKSx0LDIpfSxwOmZ1
bmN0aW9uKG4pe3JldHVybiBwWysobi5nZXRIb3VycygpPj0xMildfSxTOmZ1bmN0
aW9uKG4sdCl7cmV0dXJuIEh0KG4uZ2V0U2Vjb25kcygpLHQsMil9LFU6ZnVuY3Rp
b24obix0KXtyZXR1cm4gSHQobmMuc3VuZGF5T2ZZZWFyKG4pLHQsMil9LHc6ZnVu
Y3Rpb24obil7cmV0dXJuIG4uZ2V0RGF5KCl9LFc6ZnVuY3Rpb24obix0KXtyZXR1
cm4gSHQobmMubW9uZGF5T2ZZZWFyKG4pLHQsMil9LHg6dChoKSxYOnQoZykseTpm
dW5jdGlvbihuLHQpe3JldHVybiBIdChuLmdldEZ1bGxZZWFyKCklMTAwLHQsMil9
LFk6ZnVuY3Rpb24obix0KXtyZXR1cm4gSHQobi5nZXRGdWxsWWVhcigpJTFlNCx0
LDQpfSxaOmVlLCIlIjpmdW5jdGlvbigpe3JldHVybiIlIn19LE49e2E6cixBOnUs
YjppLEI6byxjOmEsZDpKdCxlOkp0LEg6S3QsSTpLdCxqOkd0LEw6dGUsbTpXdCxN
OlF0LHA6cyxTOm5lLFU6SXQsdzpZdCxXOlp0LHg6YyxYOmwseTpYdCxZOlZ0LFo6
JHQsIiUiOnJlfTtyZXR1cm4gdH1mdW5jdGlvbiBIdChuLHQsZSl7dmFyIHI9MD5u
PyItIjoiIix1PShyPy1uOm4pKyIiLGk9dS5sZW5ndGg7cmV0dXJuIHIrKGU+aT9u
ZXcgQXJyYXkoZS1pKzEpLmpvaW4odCkrdTp1KX1mdW5jdGlvbiBGdChuKXtyZXR1
cm4gbmV3IFJlZ0V4cCgiXig/OiIrbi5tYXAoVm8ucmVxdW90ZSkuam9pbigifCIp
KyIpIiwiaSIpfWZ1bmN0aW9uIE90KG4pe2Zvcih2YXIgdD1uZXcgbyxlPS0xLHI9
bi5sZW5ndGg7KytlPHI7KXQuc2V0KG5bZV0udG9Mb3dlckNhc2UoKSxlKTtyZXR1
cm4gdH1mdW5jdGlvbiBZdChuLHQsZSl7dWMubGFzdEluZGV4PTA7dmFyIHI9dWMu
ZXhlYyh0LnNsaWNlKGUsZSsxKSk7cmV0dXJuIHI/KG4udz0rclswXSxlK3JbMF0u
bGVuZ3RoKTotMX1mdW5jdGlvbiBJdChuLHQsZSl7dWMubGFzdEluZGV4PTA7dmFy
IHI9dWMuZXhlYyh0LnNsaWNlKGUpKTtyZXR1cm4gcj8obi5VPStyWzBdLGUrclsw
XS5sZW5ndGgpOi0xfWZ1bmN0aW9uIFp0KG4sdCxlKXt1Yy5sYXN0SW5kZXg9MDt2
YXIgcj11Yy5leGVjKHQuc2xpY2UoZSkpO3JldHVybiByPyhuLlc9K3JbMF0sZSty
WzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gVnQobix0LGUpe3VjLmxhc3RJbmRleD0w
O3ZhciByPXVjLmV4ZWModC5zbGljZShlLGUrNCkpO3JldHVybiByPyhuLnk9K3Jb
MF0sZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gWHQobix0LGUpe3VjLmxhc3RJ
bmRleD0wO3ZhciByPXVjLmV4ZWModC5zbGljZShlLGUrMikpO3JldHVybiByPyhu
Lnk9QnQoK3JbMF0pLGUrclswXS5sZW5ndGgpOi0xfWZ1bmN0aW9uICR0KG4sdCxl
KXtyZXR1cm4vXlsrLV1cZHs0fSQvLnRlc3QodD10LnNsaWNlKGUsZSs1KSk/KG4u
Wj0tdCxlKzUpOi0xfWZ1bmN0aW9uIEJ0KG4pe3JldHVybiBuKyhuPjY4PzE5MDA6
MmUzKX1mdW5jdGlvbiBXdChuLHQsZSl7dWMubGFzdEluZGV4PTA7dmFyIHI9dWMu
ZXhlYyh0LnNsaWNlKGUsZSsyKSk7cmV0dXJuIHI/KG4ubT1yWzBdLTEsZStyWzBd
Lmxlbmd0aCk6LTF9ZnVuY3Rpb24gSnQobix0LGUpe3VjLmxhc3RJbmRleD0wO3Zh
ciByPXVjLmV4ZWModC5zbGljZShlLGUrMikpO3JldHVybiByPyhuLmQ9K3JbMF0s
ZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gR3Qobix0LGUpe3VjLmxhc3RJbmRl
eD0wO3ZhciByPXVjLmV4ZWModC5zbGljZShlLGUrMykpO3JldHVybiByPyhuLmo9
K3JbMF0sZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gS3Qobix0LGUpe3VjLmxh
c3RJbmRleD0wO3ZhciByPXVjLmV4ZWModC5zbGljZShlLGUrMikpO3JldHVybiBy
PyhuLkg9K3JbMF0sZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gUXQobix0LGUp
e3VjLmxhc3RJbmRleD0wO3ZhciByPXVjLmV4ZWModC5zbGljZShlLGUrMikpO3Jl
dHVybiByPyhuLk09K3JbMF0sZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rpb24gbmUo
bix0LGUpe3VjLmxhc3RJbmRleD0wO3ZhciByPXVjLmV4ZWModC5zbGljZShlLGUr
MikpO3JldHVybiByPyhuLlM9K3JbMF0sZStyWzBdLmxlbmd0aCk6LTF9ZnVuY3Rp
b24gdGUobix0LGUpe3VjLmxhc3RJbmRleD0wO3ZhciByPXVjLmV4ZWModC5zbGlj
ZShlLGUrMykpO3JldHVybiByPyhuLkw9K3JbMF0sZStyWzBdLmxlbmd0aCk6LTF9
ZnVuY3Rpb24gZWUobil7dmFyIHQ9bi5nZXRUaW1lem9uZU9mZnNldCgpLGU9dD4w
PyItIjoiKyIscj0wfGlhKHQpLzYwLHU9aWEodCklNjA7cmV0dXJuIGUrSHQociwi
MCIsMikrSHQodSwiMCIsMil9ZnVuY3Rpb24gcmUobix0LGUpe2ljLmxhc3RJbmRl
eD0wO3ZhciByPWljLmV4ZWModC5zbGljZShlLGUrMSkpO3JldHVybiByP2Urclsw
XS5sZW5ndGg6LTF9ZnVuY3Rpb24gdWUobil7Zm9yKHZhciB0PW4ubGVuZ3RoLGU9
LTE7KytlPHQ7KW5bZV1bMF09dGhpcyhuW2VdWzBdKTtyZXR1cm4gZnVuY3Rpb24o
dCl7Zm9yKHZhciBlPTAscj1uW2VdOyFyWzFdKHQpOylyPW5bKytlXTtyZXR1cm4g
clswXSh0KX19ZnVuY3Rpb24gaWUoKXt9ZnVuY3Rpb24gb2Uobix0LGUpe3ZhciBy
PWUucz1uK3QsdT1yLW4saT1yLXU7ZS50PW4taSsodC11KX1mdW5jdGlvbiBhZShu
LHQpe24mJmxjLmhhc093blByb3BlcnR5KG4udHlwZSkmJmxjW24udHlwZV0obix0
KX1mdW5jdGlvbiBjZShuLHQsZSl7dmFyIHIsdT0tMSxpPW4ubGVuZ3RoLWU7Zm9y
KHQubGluZVN0YXJ0KCk7Kyt1PGk7KXI9blt1XSx0LnBvaW50KHJbMF0sclsxXSxy
WzJdKTt0LmxpbmVFbmQoKX1mdW5jdGlvbiBsZShuLHQpe3ZhciBlPS0xLHI9bi5s
ZW5ndGg7Zm9yKHQucG9seWdvblN0YXJ0KCk7KytlPHI7KWNlKG5bZV0sdCwxKTt0
LnBvbHlnb25FbmQoKX1mdW5jdGlvbiBzZSgpe2Z1bmN0aW9uIG4obix0KXtuKj1D
YSx0PXQqQ2EvMit3YS80O3ZhciBlPW4tcixvPWU+PTA/MTotMSxhPW8qZSxjPU1h
dGguY29zKHQpLGw9TWF0aC5zaW4odCkscz1pKmwsZj11KmMrcypNYXRoLmNvcyhh
KSxoPXMqbypNYXRoLnNpbihhKTtmYy5hZGQoTWF0aC5hdGFuMihoLGYpKSxyPW4s
dT1jLGk9bH12YXIgdCxlLHIsdSxpO2hjLnBvaW50PWZ1bmN0aW9uKG8sYSl7aGMu
cG9pbnQ9bixyPSh0PW8pKkNhLHU9TWF0aC5jb3MoYT0oZT1hKSpDYS8yK3dhLzQp
LGk9TWF0aC5zaW4oYSl9LGhjLmxpbmVFbmQ9ZnVuY3Rpb24oKXtuKHQsZSl9fWZ1
bmN0aW9uIGZlKG4pe3ZhciB0PW5bMF0sZT1uWzFdLHI9TWF0aC5jb3MoZSk7cmV0
dXJuW3IqTWF0aC5jb3ModCkscipNYXRoLnNpbih0KSxNYXRoLnNpbihlKV19ZnVu
Y3Rpb24gaGUobix0KXtyZXR1cm4gblswXSp0WzBdK25bMV0qdFsxXStuWzJdKnRb
Ml19ZnVuY3Rpb24gZ2Uobix0KXtyZXR1cm5bblsxXSp0WzJdLW5bMl0qdFsxXSxu
WzJdKnRbMF0tblswXSp0WzJdLG5bMF0qdFsxXS1uWzFdKnRbMF1dfWZ1bmN0aW9u
IHBlKG4sdCl7blswXSs9dFswXSxuWzFdKz10WzFdLG5bMl0rPXRbMl19ZnVuY3Rp
b24gdmUobix0KXtyZXR1cm5bblswXSp0LG5bMV0qdCxuWzJdKnRdfWZ1bmN0aW9u
IGRlKG4pe3ZhciB0PU1hdGguc3FydChuWzBdKm5bMF0rblsxXSpuWzFdK25bMl0q
blsyXSk7blswXS89dCxuWzFdLz10LG5bMl0vPXR9ZnVuY3Rpb24gbWUobil7cmV0
dXJuW01hdGguYXRhbjIoblsxXSxuWzBdKSxHKG5bMl0pXX1mdW5jdGlvbiB5ZShu
LHQpe3JldHVybiBpYShuWzBdLXRbMF0pPEVhJiZpYShuWzFdLXRbMV0pPEVhfWZ1
bmN0aW9uIHhlKG4sdCl7bio9Q2E7dmFyIGU9TWF0aC5jb3ModCo9Q2EpO01lKGUq
TWF0aC5jb3MobiksZSpNYXRoLnNpbihuKSxNYXRoLnNpbih0KSl9ZnVuY3Rpb24g
TWUobix0LGUpeysrZ2MsdmMrPShuLXZjKS9nYyxkYys9KHQtZGMpL2djLG1jKz0o
ZS1tYykvZ2N9ZnVuY3Rpb24gX2UoKXtmdW5jdGlvbiBuKG4sdSl7bio9Q2E7dmFy
IGk9TWF0aC5jb3ModSo9Q2EpLG89aSpNYXRoLmNvcyhuKSxhPWkqTWF0aC5zaW4o
biksYz1NYXRoLnNpbih1KSxsPU1hdGguYXRhbjIoTWF0aC5zcXJ0KChsPWUqYy1y
KmEpKmwrKGw9cipvLXQqYykqbCsobD10KmEtZSpvKSpsKSx0Km8rZSphK3IqYyk7
cGMrPWwseWMrPWwqKHQrKHQ9bykpLHhjKz1sKihlKyhlPWEpKSxNYys9bCoociso
cj1jKSksTWUodCxlLHIpfXZhciB0LGUscjtTYy5wb2ludD1mdW5jdGlvbih1LGkp
e3UqPUNhO3ZhciBvPU1hdGguY29zKGkqPUNhKTt0PW8qTWF0aC5jb3ModSksZT1v
Kk1hdGguc2luKHUpLHI9TWF0aC5zaW4oaSksU2MucG9pbnQ9bixNZSh0LGUscil9
fWZ1bmN0aW9uIGJlKCl7U2MucG9pbnQ9eGV9ZnVuY3Rpb24gd2UoKXtmdW5jdGlv
biBuKG4sdCl7bio9Q2E7dmFyIGU9TWF0aC5jb3ModCo9Q2EpLG89ZSpNYXRoLmNv
cyhuKSxhPWUqTWF0aC5zaW4obiksYz1NYXRoLnNpbih0KSxsPXUqYy1pKmEscz1p
Km8tcipjLGY9ciphLXUqbyxoPU1hdGguc3FydChsKmwrcypzK2YqZiksZz1yKm8r
dSphK2kqYyxwPWgmJi1KKGcpL2gsdj1NYXRoLmF0YW4yKGgsZyk7X2MrPXAqbCxi
Yys9cCpzLHdjKz1wKmYscGMrPXYseWMrPXYqKHIrKHI9bykpLHhjKz12Kih1Kyh1
PWEpKSxNYys9diooaSsoaT1jKSksTWUocix1LGkpfXZhciB0LGUscix1LGk7U2Mu
cG9pbnQ9ZnVuY3Rpb24obyxhKXt0PW8sZT1hLFNjLnBvaW50PW4sbyo9Q2E7dmFy
IGM9TWF0aC5jb3MoYSo9Q2EpO3I9YypNYXRoLmNvcyhvKSx1PWMqTWF0aC5zaW4o
byksaT1NYXRoLnNpbihhKSxNZShyLHUsaSl9LFNjLmxpbmVFbmQ9ZnVuY3Rpb24o
KXtuKHQsZSksU2MubGluZUVuZD1iZSxTYy5wb2ludD14ZX19ZnVuY3Rpb24gU2Uo
KXtyZXR1cm4hMH1mdW5jdGlvbiBrZShuLHQsZSxyLHUpe3ZhciBpPVtdLG89W107
aWYobi5mb3JFYWNoKGZ1bmN0aW9uKG4pe2lmKCEoKHQ9bi5sZW5ndGgtMSk8PTAp
KXt2YXIgdCxlPW5bMF0scj1uW3RdO2lmKHllKGUscikpe3UubGluZVN0YXJ0KCk7
Zm9yKHZhciBhPTA7dD5hOysrYSl1LnBvaW50KChlPW5bYV0pWzBdLGVbMV0pO3Jl
dHVybiB1LmxpbmVFbmQoKSx2b2lkIDB9dmFyIGM9bmV3IEFlKGUsbixudWxsLCEw
KSxsPW5ldyBBZShlLG51bGwsYywhMSk7Yy5vPWwsaS5wdXNoKGMpLG8ucHVzaChs
KSxjPW5ldyBBZShyLG4sbnVsbCwhMSksbD1uZXcgQWUocixudWxsLGMsITApLGMu
bz1sLGkucHVzaChjKSxvLnB1c2gobCl9fSksby5zb3J0KHQpLEVlKGkpLEVlKG8p
LGkubGVuZ3RoKXtmb3IodmFyIGE9MCxjPWUsbD1vLmxlbmd0aDtsPmE7KythKW9b
YV0uZT1jPSFjO2Zvcih2YXIgcyxmLGg9aVswXTs7KXtmb3IodmFyIGc9aCxwPSEw
O2cudjspaWYoKGc9Zy5uKT09PWgpcmV0dXJuO3M9Zy56LHUubGluZVN0YXJ0KCk7
ZG97aWYoZy52PWcuby52PSEwLGcuZSl7aWYocClmb3IodmFyIGE9MCxsPXMubGVu
Z3RoO2w+YTsrK2EpdS5wb2ludCgoZj1zW2FdKVswXSxmWzFdKTtlbHNlIHIoZy54
LGcubi54LDEsdSk7Zz1nLm59ZWxzZXtpZihwKXtzPWcucC56O2Zvcih2YXIgYT1z
Lmxlbmd0aC0xO2E+PTA7LS1hKXUucG9pbnQoKGY9c1thXSlbMF0sZlsxXSl9ZWxz
ZSByKGcueCxnLnAueCwtMSx1KTtnPWcucH1nPWcubyxzPWcueixwPSFwfXdoaWxl
KCFnLnYpO3UubGluZUVuZCgpfX19ZnVuY3Rpb24gRWUobil7aWYodD1uLmxlbmd0
aCl7Zm9yKHZhciB0LGUscj0wLHU9blswXTsrK3I8dDspdS5uPWU9bltyXSxlLnA9
dSx1PWU7dS5uPWU9blswXSxlLnA9dX19ZnVuY3Rpb24gQWUobix0LGUscil7dGhp
cy54PW4sdGhpcy56PXQsdGhpcy5vPWUsdGhpcy5lPXIsdGhpcy52PSExLHRoaXMu
bj10aGlzLnA9bnVsbH1mdW5jdGlvbiBDZShuLHQsZSxyKXtyZXR1cm4gZnVuY3Rp
b24odSxpKXtmdW5jdGlvbiBvKHQsZSl7dmFyIHI9dSh0LGUpO24odD1yWzBdLGU9
clsxXSkmJmkucG9pbnQodCxlKX1mdW5jdGlvbiBhKG4sdCl7dmFyIGU9dShuLHQp
O2QucG9pbnQoZVswXSxlWzFdKX1mdW5jdGlvbiBjKCl7eS5wb2ludD1hLGQubGlu
ZVN0YXJ0KCl9ZnVuY3Rpb24gbCgpe3kucG9pbnQ9byxkLmxpbmVFbmQoKX1mdW5j
dGlvbiBzKG4sdCl7di5wdXNoKFtuLHRdKTt2YXIgZT11KG4sdCk7TS5wb2ludChl
WzBdLGVbMV0pfWZ1bmN0aW9uIGYoKXtNLmxpbmVTdGFydCgpLHY9W119ZnVuY3Rp
b24gaCgpe3ModlswXVswXSx2WzBdWzFdKSxNLmxpbmVFbmQoKTt2YXIgbix0PU0u
Y2xlYW4oKSxlPXguYnVmZmVyKCkscj1lLmxlbmd0aDtpZih2LnBvcCgpLHAucHVz
aCh2KSx2PW51bGwscilpZigxJnQpe249ZVswXTt2YXIgdSxyPW4ubGVuZ3RoLTEs
bz0tMTtpZihyPjApe2ZvcihffHwoaS5wb2x5Z29uU3RhcnQoKSxfPSEwKSxpLmxp
bmVTdGFydCgpOysrbzxyOylpLnBvaW50KCh1PW5bb10pWzBdLHVbMV0pO2kubGlu
ZUVuZCgpfX1lbHNlIHI+MSYmMiZ0JiZlLnB1c2goZS5wb3AoKS5jb25jYXQoZS5z
aGlmdCgpKSksZy5wdXNoKGUuZmlsdGVyKE5lKSl9dmFyIGcscCx2LGQ9dChpKSxt
PXUuaW52ZXJ0KHJbMF0sclsxXSkseT17cG9pbnQ6byxsaW5lU3RhcnQ6YyxsaW5l
RW5kOmwscG9seWdvblN0YXJ0OmZ1bmN0aW9uKCl7eS5wb2ludD1zLHkubGluZVN0
YXJ0PWYseS5saW5lRW5kPWgsZz1bXSxwPVtdfSxwb2x5Z29uRW5kOmZ1bmN0aW9u
KCl7eS5wb2ludD1vLHkubGluZVN0YXJ0PWMseS5saW5lRW5kPWwsZz1Wby5tZXJn
ZShnKTt2YXIgbj1EZShtLHApO2cubGVuZ3RoPyhffHwoaS5wb2x5Z29uU3RhcnQo
KSxfPSEwKSxrZShnLExlLG4sZSxpKSk6biYmKF98fChpLnBvbHlnb25TdGFydCgp
LF89ITApLGkubGluZVN0YXJ0KCksZShudWxsLG51bGwsMSxpKSxpLmxpbmVFbmQo
KSksXyYmKGkucG9seWdvbkVuZCgpLF89ITEpLGc9cD1udWxsfSxzcGhlcmU6ZnVu
Y3Rpb24oKXtpLnBvbHlnb25TdGFydCgpLGkubGluZVN0YXJ0KCksZShudWxsLG51
bGwsMSxpKSxpLmxpbmVFbmQoKSxpLnBvbHlnb25FbmQoKX19LHg9emUoKSxNPXQo
eCksXz0hMTtyZXR1cm4geX19ZnVuY3Rpb24gTmUobil7cmV0dXJuIG4ubGVuZ3Ro
PjF9ZnVuY3Rpb24gemUoKXt2YXIgbix0PVtdO3JldHVybntsaW5lU3RhcnQ6ZnVu
Y3Rpb24oKXt0LnB1c2gobj1bXSl9LHBvaW50OmZ1bmN0aW9uKHQsZSl7bi5wdXNo
KFt0LGVdKX0sbGluZUVuZDp2LGJ1ZmZlcjpmdW5jdGlvbigpe3ZhciBlPXQ7cmV0
dXJuIHQ9W10sbj1udWxsLGV9LHJlam9pbjpmdW5jdGlvbigpe3QubGVuZ3RoPjEm
JnQucHVzaCh0LnBvcCgpLmNvbmNhdCh0LnNoaWZ0KCkpKX19fWZ1bmN0aW9uIExl
KG4sdCl7cmV0dXJuKChuPW4ueClbMF08MD9uWzFdLWthLUVhOmthLW5bMV0pLSgo
dD10LngpWzBdPDA/dFsxXS1rYS1FYTprYS10WzFdKX1mdW5jdGlvbiBUZShuKXt2
YXIgdCxlPTAvMCxyPTAvMCx1PTAvMDtyZXR1cm57bGluZVN0YXJ0OmZ1bmN0aW9u
KCl7bi5saW5lU3RhcnQoKSx0PTF9LHBvaW50OmZ1bmN0aW9uKGksbyl7dmFyIGE9
aT4wP3dhOi13YSxjPWlhKGktZSk7aWEoYy13YSk8RWE/KG4ucG9pbnQoZSxyPShy
K28pLzI+MD9rYTota2EpLG4ucG9pbnQodSxyKSxuLmxpbmVFbmQoKSxuLmxpbmVT
dGFydCgpLG4ucG9pbnQoYSxyKSxuLnBvaW50KGksciksdD0wKTp1IT09YSYmYz49
d2EmJihpYShlLXUpPEVhJiYoZS09dSpFYSksaWEoaS1hKTxFYSYmKGktPWEqRWEp
LHI9cWUoZSxyLGksbyksbi5wb2ludCh1LHIpLG4ubGluZUVuZCgpLG4ubGluZVN0
YXJ0KCksbi5wb2ludChhLHIpLHQ9MCksbi5wb2ludChlPWkscj1vKSx1PWF9LGxp
bmVFbmQ6ZnVuY3Rpb24oKXtuLmxpbmVFbmQoKSxlPXI9MC8wfSxjbGVhbjpmdW5j
dGlvbigpe3JldHVybiAyLXR9fX1mdW5jdGlvbiBxZShuLHQsZSxyKXt2YXIgdSxp
LG89TWF0aC5zaW4obi1lKTtyZXR1cm4gaWEobyk+RWE/TWF0aC5hdGFuKChNYXRo
LnNpbih0KSooaT1NYXRoLmNvcyhyKSkqTWF0aC5zaW4oZSktTWF0aC5zaW4ocikq
KHU9TWF0aC5jb3ModCkpKk1hdGguc2luKG4pKS8odSppKm8pKToodCtyKS8yfWZ1
bmN0aW9uIFJlKG4sdCxlLHIpe3ZhciB1O2lmKG51bGw9PW4pdT1lKmthLHIucG9p
bnQoLXdhLHUpLHIucG9pbnQoMCx1KSxyLnBvaW50KHdhLHUpLHIucG9pbnQod2Es
MCksci5wb2ludCh3YSwtdSksci5wb2ludCgwLC11KSxyLnBvaW50KC13YSwtdSks
ci5wb2ludCgtd2EsMCksci5wb2ludCgtd2EsdSk7ZWxzZSBpZihpYShuWzBdLXRb
MF0pPkVhKXt2YXIgaT1uWzBdPHRbMF0/d2E6LXdhO3U9ZSppLzIsci5wb2ludCgt
aSx1KSxyLnBvaW50KDAsdSksci5wb2ludChpLHUpfWVsc2Ugci5wb2ludCh0WzBd
LHRbMV0pfWZ1bmN0aW9uIERlKG4sdCl7dmFyIGU9blswXSxyPW5bMV0sdT1bTWF0
aC5zaW4oZSksLU1hdGguY29zKGUpLDBdLGk9MCxvPTA7ZmMucmVzZXQoKTtmb3Io
dmFyIGE9MCxjPXQubGVuZ3RoO2M+YTsrK2Epe3ZhciBsPXRbYV0scz1sLmxlbmd0
aDtpZihzKWZvcih2YXIgZj1sWzBdLGg9ZlswXSxnPWZbMV0vMit3YS80LHA9TWF0
aC5zaW4oZyksdj1NYXRoLmNvcyhnKSxkPTE7Oyl7ZD09PXMmJihkPTApLG49bFtk
XTt2YXIgbT1uWzBdLHk9blsxXS8yK3dhLzQseD1NYXRoLnNpbih5KSxNPU1hdGgu
Y29zKHkpLF89bS1oLGI9Xz49MD8xOi0xLHc9YipfLFM9dz53YSxrPXAqeDtpZihm
Yy5hZGQoTWF0aC5hdGFuMihrKmIqTWF0aC5zaW4odyksdipNK2sqTWF0aC5jb3Mo
dykpKSxpKz1TP18rYipTYTpfLFNeaD49ZV5tPj1lKXt2YXIgRT1nZShmZShmKSxm
ZShuKSk7ZGUoRSk7dmFyIEE9Z2UodSxFKTtkZShBKTt2YXIgQz0oU15fPj0wPy0x
OjEpKkcoQVsyXSk7KHI+Q3x8cj09PUMmJihFWzBdfHxFWzFdKSkmJihvKz1TXl8+
PTA/MTotMSl9aWYoIWQrKylicmVhaztoPW0scD14LHY9TSxmPW59fXJldHVybigt
RWE+aXx8RWE+aSYmMD5mYyleMSZvfWZ1bmN0aW9uIFBlKG4pe2Z1bmN0aW9uIHQo
bix0KXtyZXR1cm4gTWF0aC5jb3MobikqTWF0aC5jb3ModCk+aX1mdW5jdGlvbiBl
KG4pe3ZhciBlLGksYyxsLHM7cmV0dXJue2xpbmVTdGFydDpmdW5jdGlvbigpe2w9
Yz0hMSxzPTF9LHBvaW50OmZ1bmN0aW9uKGYsaCl7dmFyIGcscD1bZixoXSx2PXQo
ZixoKSxkPW8/dj8wOnUoZixoKTp2P3UoZisoMD5mP3dhOi13YSksaCk6MDtpZigh
ZSYmKGw9Yz12KSYmbi5saW5lU3RhcnQoKSx2IT09YyYmKGc9cihlLHApLCh5ZShl
LGcpfHx5ZShwLGcpKSYmKHBbMF0rPUVhLHBbMV0rPUVhLHY9dChwWzBdLHBbMV0p
KSksdiE9PWMpcz0wLHY/KG4ubGluZVN0YXJ0KCksZz1yKHAsZSksbi5wb2ludChn
WzBdLGdbMV0pKTooZz1yKGUscCksbi5wb2ludChnWzBdLGdbMV0pLG4ubGluZUVu
ZCgpKSxlPWc7ZWxzZSBpZihhJiZlJiZvXnYpe3ZhciBtO2QmaXx8IShtPXIocCxl
LCEwKSl8fChzPTAsbz8obi5saW5lU3RhcnQoKSxuLnBvaW50KG1bMF1bMF0sbVsw
XVsxXSksbi5wb2ludChtWzFdWzBdLG1bMV1bMV0pLG4ubGluZUVuZCgpKToobi5w
b2ludChtWzFdWzBdLG1bMV1bMV0pLG4ubGluZUVuZCgpLG4ubGluZVN0YXJ0KCks
bi5wb2ludChtWzBdWzBdLG1bMF1bMV0pKSl9IXZ8fGUmJnllKGUscCl8fG4ucG9p
bnQocFswXSxwWzFdKSxlPXAsYz12LGk9ZH0sbGluZUVuZDpmdW5jdGlvbigpe2Mm
Jm4ubGluZUVuZCgpLGU9bnVsbH0sY2xlYW46ZnVuY3Rpb24oKXtyZXR1cm4gc3wo
bCYmYyk8PDF9fX1mdW5jdGlvbiByKG4sdCxlKXt2YXIgcj1mZShuKSx1PWZlKHQp
LG89WzEsMCwwXSxhPWdlKHIsdSksYz1oZShhLGEpLGw9YVswXSxzPWMtbCpsO2lm
KCFzKXJldHVybiFlJiZuO3ZhciBmPWkqYy9zLGg9LWkqbC9zLGc9Z2UobyxhKSxw
PXZlKG8sZiksdj12ZShhLGgpO3BlKHAsdik7dmFyIGQ9ZyxtPWhlKHAsZCkseT1o
ZShkLGQpLHg9bSptLXkqKGhlKHAscCktMSk7aWYoISgwPngpKXt2YXIgTT1NYXRo
LnNxcnQoeCksXz12ZShkLCgtbS1NKS95KTtpZihwZShfLHApLF89bWUoXyksIWUp
cmV0dXJuIF87dmFyIGIsdz1uWzBdLFM9dFswXSxrPW5bMV0sRT10WzFdO3c+UyYm
KGI9dyx3PVMsUz1iKTt2YXIgQT1TLXcsQz1pYShBLXdhKTxFYSxOPUN8fEVhPkE7
aWYoIUMmJms+RSYmKGI9ayxrPUUsRT1iKSxOP0M/aytFPjBeX1sxXTwoaWEoX1sw
XS13KTxFYT9rOkUpOms8PV9bMV0mJl9bMV08PUU6QT53YV4odzw9X1swXSYmX1sw
XTw9Uykpe3ZhciB6PXZlKGQsKC1tK00pL3kpO3JldHVybiBwZSh6LHApLFtfLG1l
KHopXX19fWZ1bmN0aW9uIHUodCxlKXt2YXIgcj1vP246d2Etbix1PTA7cmV0dXJu
LXI+dD91fD0xOnQ+ciYmKHV8PTIpLC1yPmU/dXw9NDplPnImJih1fD04KSx1fXZh
ciBpPU1hdGguY29zKG4pLG89aT4wLGE9aWEoaSk+RWEsYz1zcihuLDYqQ2EpO3Jl
dHVybiBDZSh0LGUsYyxvP1swLC1uXTpbLXdhLG4td2FdKX1mdW5jdGlvbiBVZShu
LHQsZSxyKXtyZXR1cm4gZnVuY3Rpb24odSl7dmFyIGksbz11LmEsYT11LmIsYz1v
LngsbD1vLnkscz1hLngsZj1hLnksaD0wLGc9MSxwPXMtYyx2PWYtbDtpZihpPW4t
YyxwfHwhKGk+MCkpe2lmKGkvPXAsMD5wKXtpZihoPmkpcmV0dXJuO2c+aSYmKGc9
aSl9ZWxzZSBpZihwPjApe2lmKGk+ZylyZXR1cm47aT5oJiYoaD1pKX1pZihpPWUt
YyxwfHwhKDA+aSkpe2lmKGkvPXAsMD5wKXtpZihpPmcpcmV0dXJuO2k+aCYmKGg9
aSl9ZWxzZSBpZihwPjApe2lmKGg+aSlyZXR1cm47Zz5pJiYoZz1pKX1pZihpPXQt
bCx2fHwhKGk+MCkpe2lmKGkvPXYsMD52KXtpZihoPmkpcmV0dXJuO2c+aSYmKGc9
aSl9ZWxzZSBpZih2PjApe2lmKGk+ZylyZXR1cm47aT5oJiYoaD1pKX1pZihpPXIt
bCx2fHwhKDA+aSkpe2lmKGkvPXYsMD52KXtpZihpPmcpcmV0dXJuO2k+aCYmKGg9
aSl9ZWxzZSBpZih2PjApe2lmKGg+aSlyZXR1cm47Zz5pJiYoZz1pKX1yZXR1cm4g
aD4wJiYodS5hPXt4OmMraCpwLHk6bCtoKnZ9KSwxPmcmJih1LmI9e3g6YytnKnAs
eTpsK2cqdn0pLHV9fX19fX1mdW5jdGlvbiBqZShuLHQsZSxyKXtmdW5jdGlvbiB1
KHIsdSl7cmV0dXJuIGlhKHJbMF0tbik8RWE/dT4wPzA6MzppYShyWzBdLWUpPEVh
P3U+MD8yOjE6aWEoclsxXS10KTxFYT91PjA/MTowOnU+MD8zOjJ9ZnVuY3Rpb24g
aShuLHQpe3JldHVybiBvKG4ueCx0LngpfWZ1bmN0aW9uIG8obix0KXt2YXIgZT11
KG4sMSkscj11KHQsMSk7cmV0dXJuIGUhPT1yP2UtcjowPT09ZT90WzFdLW5bMV06
MT09PWU/blswXS10WzBdOjI9PT1lP25bMV0tdFsxXTp0WzBdLW5bMF19cmV0dXJu
IGZ1bmN0aW9uKGEpe2Z1bmN0aW9uIGMobil7Zm9yKHZhciB0PTAsZT1kLmxlbmd0
aCxyPW5bMV0sdT0wO2U+dTsrK3UpZm9yKHZhciBpLG89MSxhPWRbdV0sYz1hLmxl
bmd0aCxsPWFbMF07Yz5vOysrbylpPWFbb10sbFsxXTw9cj9pWzFdPnImJlcobCxp
LG4pPjAmJisrdDppWzFdPD1yJiZXKGwsaSxuKTwwJiYtLXQsbD1pO3JldHVybiAw
IT09dH1mdW5jdGlvbiBsKGksYSxjLGwpe3ZhciBzPTAsZj0wO2lmKG51bGw9PWl8
fChzPXUoaSxjKSkhPT0oZj11KGEsYykpfHxvKGksYSk8MF5jPjApe2RvIGwucG9p
bnQoMD09PXN8fDM9PT1zP246ZSxzPjE/cjp0KTt3aGlsZSgocz0ocytjKzQpJTQp
IT09Zil9ZWxzZSBsLnBvaW50KGFbMF0sYVsxXSl9ZnVuY3Rpb24gcyh1LGkpe3Jl
dHVybiB1Pj1uJiZlPj11JiZpPj10JiZyPj1pfWZ1bmN0aW9uIGYobix0KXtzKG4s
dCkmJmEucG9pbnQobix0KX1mdW5jdGlvbiBoKCl7Ti5wb2ludD1wLGQmJmQucHVz
aChtPVtdKSxTPSEwLHc9ITEsXz1iPTAvMH1mdW5jdGlvbiBnKCl7diYmKHAoeSx4
KSxNJiZ3JiZBLnJlam9pbigpLHYucHVzaChBLmJ1ZmZlcigpKSksTi5wb2ludD1m
LHcmJmEubGluZUVuZCgpfWZ1bmN0aW9uIHAobix0KXtuPU1hdGgubWF4KC1FYyxN
YXRoLm1pbihFYyxuKSksdD1NYXRoLm1heCgtRWMsTWF0aC5taW4oRWMsdCkpO3Zh
ciBlPXMobix0KTtpZihkJiZtLnB1c2goW24sdF0pLFMpeT1uLHg9dCxNPWUsUz0h
MSxlJiYoYS5saW5lU3RhcnQoKSxhLnBvaW50KG4sdCkpO2Vsc2UgaWYoZSYmdylh
LnBvaW50KG4sdCk7ZWxzZXt2YXIgcj17YTp7eDpfLHk6Yn0sYjp7eDpuLHk6dH19
O0Mocik/KHd8fChhLmxpbmVTdGFydCgpLGEucG9pbnQoci5hLngsci5hLnkpKSxh
LnBvaW50KHIuYi54LHIuYi55KSxlfHxhLmxpbmVFbmQoKSxrPSExKTplJiYoYS5s
aW5lU3RhcnQoKSxhLnBvaW50KG4sdCksaz0hMSl9Xz1uLGI9dCx3PWV9dmFyIHYs
ZCxtLHkseCxNLF8sYix3LFMsayxFPWEsQT16ZSgpLEM9VWUobix0LGUsciksTj17
cG9pbnQ6ZixsaW5lU3RhcnQ6aCxsaW5lRW5kOmcscG9seWdvblN0YXJ0OmZ1bmN0
aW9uKCl7YT1BLHY9W10sZD1bXSxrPSEwfSxwb2x5Z29uRW5kOmZ1bmN0aW9uKCl7
YT1FLHY9Vm8ubWVyZ2Uodik7dmFyIHQ9YyhbbixyXSksZT1rJiZ0LHU9di5sZW5n
dGg7KGV8fHUpJiYoYS5wb2x5Z29uU3RhcnQoKSxlJiYoYS5saW5lU3RhcnQoKSxs
KG51bGwsbnVsbCwxLGEpLGEubGluZUVuZCgpKSx1JiZrZSh2LGksdCxsLGEpLGEu
cG9seWdvbkVuZCgpKSx2PWQ9bT1udWxsfX07cmV0dXJuIE59fWZ1bmN0aW9uIEhl
KG4sdCl7ZnVuY3Rpb24gZShlLHIpe3JldHVybiBlPW4oZSxyKSx0KGVbMF0sZVsx
XSl9cmV0dXJuIG4uaW52ZXJ0JiZ0LmludmVydCYmKGUuaW52ZXJ0PWZ1bmN0aW9u
KGUscil7cmV0dXJuIGU9dC5pbnZlcnQoZSxyKSxlJiZuLmludmVydChlWzBdLGVb
MV0pfSksZX1mdW5jdGlvbiBGZShuKXt2YXIgdD0wLGU9d2EvMyxyPWVyKG4pLHU9
cih0LGUpO3JldHVybiB1LnBhcmFsbGVscz1mdW5jdGlvbihuKXtyZXR1cm4gYXJn
dW1lbnRzLmxlbmd0aD9yKHQ9blswXSp3YS8xODAsZT1uWzFdKndhLzE4MCk6WzE4
MCoodC93YSksMTgwKihlL3dhKV19LHV9ZnVuY3Rpb24gT2Uobix0KXtmdW5jdGlv
biBlKG4sdCl7dmFyIGU9TWF0aC5zcXJ0KGktMip1Kk1hdGguc2luKHQpKS91O3Jl
dHVybltlKk1hdGguc2luKG4qPXUpLG8tZSpNYXRoLmNvcyhuKV19dmFyIHI9TWF0
aC5zaW4obiksdT0ocitNYXRoLnNpbih0KSkvMixpPTErciooMip1LXIpLG89TWF0
aC5zcXJ0KGkpL3U7cmV0dXJuIGUuaW52ZXJ0PWZ1bmN0aW9uKG4sdCl7dmFyIGU9
by10O3JldHVybltNYXRoLmF0YW4yKG4sZSkvdSxHKChpLShuKm4rZSplKSp1KnUp
LygyKnUpKV19LGV9ZnVuY3Rpb24gWWUoKXtmdW5jdGlvbiBuKG4sdCl7Q2MrPXUq
bi1yKnQscj1uLHU9dH12YXIgdCxlLHIsdTtxYy5wb2ludD1mdW5jdGlvbihpLG8p
e3FjLnBvaW50PW4sdD1yPWksZT11PW99LHFjLmxpbmVFbmQ9ZnVuY3Rpb24oKXtu
KHQsZSl9fWZ1bmN0aW9uIEllKG4sdCl7TmM+biYmKE5jPW4pLG4+TGMmJihMYz1u
KSx6Yz50JiYoemM9dCksdD5UYyYmKFRjPXQpfWZ1bmN0aW9uIFplKCl7ZnVuY3Rp
b24gbihuLHQpe28ucHVzaCgiTSIsbiwiLCIsdCxpKX1mdW5jdGlvbiB0KG4sdCl7
by5wdXNoKCJNIixuLCIsIix0KSxhLnBvaW50PWV9ZnVuY3Rpb24gZShuLHQpe28u
cHVzaCgiTCIsbiwiLCIsdCl9ZnVuY3Rpb24gcigpe2EucG9pbnQ9bn1mdW5jdGlv
biB1KCl7by5wdXNoKCJaIil9dmFyIGk9VmUoNC41KSxvPVtdLGE9e3BvaW50Om4s
bGluZVN0YXJ0OmZ1bmN0aW9uKCl7YS5wb2ludD10fSxsaW5lRW5kOnIscG9seWdv
blN0YXJ0OmZ1bmN0aW9uKCl7YS5saW5lRW5kPXV9LHBvbHlnb25FbmQ6ZnVuY3Rp
b24oKXthLmxpbmVFbmQ9cixhLnBvaW50PW59LHBvaW50UmFkaXVzOmZ1bmN0aW9u
KG4pe3JldHVybiBpPVZlKG4pLGF9LHJlc3VsdDpmdW5jdGlvbigpe2lmKG8ubGVu
Z3RoKXt2YXIgbj1vLmpvaW4oIiIpO3JldHVybiBvPVtdLG59fX07cmV0dXJuIGF9
ZnVuY3Rpb24gVmUobil7cmV0dXJuIm0wLCIrbisiYSIrbisiLCIrbisiIDAgMSwx
IDAsIistMipuKyJhIituKyIsIituKyIgMCAxLDEgMCwiKzIqbisieiJ9ZnVuY3Rp
b24gWGUobix0KXt2Yys9bixkYys9dCwrK21jfWZ1bmN0aW9uICRlKCl7ZnVuY3Rp
b24gbihuLHIpe3ZhciB1PW4tdCxpPXItZSxvPU1hdGguc3FydCh1KnUraSppKTt5
Yys9byoodCtuKS8yLHhjKz1vKihlK3IpLzIsTWMrPW8sWGUodD1uLGU9cil9dmFy
IHQsZTtEYy5wb2ludD1mdW5jdGlvbihyLHUpe0RjLnBvaW50PW4sWGUodD1yLGU9
dSl9fWZ1bmN0aW9uIEJlKCl7RGMucG9pbnQ9WGV9ZnVuY3Rpb24gV2UoKXtmdW5j
dGlvbiBuKG4sdCl7dmFyIGU9bi1yLGk9dC11LG89TWF0aC5zcXJ0KGUqZStpKmkp
O3ljKz1vKihyK24pLzIseGMrPW8qKHUrdCkvMixNYys9byxvPXUqbi1yKnQsX2Mr
PW8qKHIrbiksYmMrPW8qKHUrdCksd2MrPTMqbyxYZShyPW4sdT10KX12YXIgdCxl
LHIsdTtEYy5wb2ludD1mdW5jdGlvbihpLG8pe0RjLnBvaW50PW4sWGUodD1yPWks
ZT11PW8pfSxEYy5saW5lRW5kPWZ1bmN0aW9uKCl7bih0LGUpfX1mdW5jdGlvbiBK
ZShuKXtmdW5jdGlvbiB0KHQsZSl7bi5tb3ZlVG8odCxlKSxuLmFyYyh0LGUsbyww
LFNhKX1mdW5jdGlvbiBlKHQsZSl7bi5tb3ZlVG8odCxlKSxhLnBvaW50PXJ9ZnVu
Y3Rpb24gcih0LGUpe24ubGluZVRvKHQsZSl9ZnVuY3Rpb24gdSgpe2EucG9pbnQ9
dH1mdW5jdGlvbiBpKCl7bi5jbG9zZVBhdGgoKX12YXIgbz00LjUsYT17cG9pbnQ6
dCxsaW5lU3RhcnQ6ZnVuY3Rpb24oKXthLnBvaW50PWV9LGxpbmVFbmQ6dSxwb2x5
Z29uU3RhcnQ6ZnVuY3Rpb24oKXthLmxpbmVFbmQ9aX0scG9seWdvbkVuZDpmdW5j
dGlvbigpe2EubGluZUVuZD11LGEucG9pbnQ9dH0scG9pbnRSYWRpdXM6ZnVuY3Rp
b24obil7cmV0dXJuIG89bixhfSxyZXN1bHQ6dn07cmV0dXJuIGF9ZnVuY3Rpb24g
R2Uobil7ZnVuY3Rpb24gdChuKXtyZXR1cm4oYT9yOmUpKG4pfWZ1bmN0aW9uIGUo
dCl7cmV0dXJuIG5yKHQsZnVuY3Rpb24oZSxyKXtlPW4oZSxyKSx0LnBvaW50KGVb
MF0sZVsxXSl9KX1mdW5jdGlvbiByKHQpe2Z1bmN0aW9uIGUoZSxyKXtlPW4oZSxy
KSx0LnBvaW50KGVbMF0sZVsxXSl9ZnVuY3Rpb24gcigpe3g9MC8wLFMucG9pbnQ9
aSx0LmxpbmVTdGFydCgpfWZ1bmN0aW9uIGkoZSxyKXt2YXIgaT1mZShbZSxyXSks
bz1uKGUscik7dSh4LE0seSxfLGIsdyx4PW9bMF0sTT1vWzFdLHk9ZSxfPWlbMF0s
Yj1pWzFdLHc9aVsyXSxhLHQpLHQucG9pbnQoeCxNKX1mdW5jdGlvbiBvKCl7Uy5w
b2ludD1lLHQubGluZUVuZCgpfWZ1bmN0aW9uIGMoKXtyKCksUy5wb2ludD1sLFMu
bGluZUVuZD1zfWZ1bmN0aW9uIGwobix0KXtpKGY9bixoPXQpLGc9eCxwPU0sdj1f
LGQ9YixtPXcsUy5wb2ludD1pfWZ1bmN0aW9uIHMoKXt1KHgsTSx5LF8sYix3LGcs
cCxmLHYsZCxtLGEsdCksUy5saW5lRW5kPW8sbygpfXZhciBmLGgsZyxwLHYsZCxt
LHkseCxNLF8sYix3LFM9e3BvaW50OmUsbGluZVN0YXJ0OnIsbGluZUVuZDpvLHBv
bHlnb25TdGFydDpmdW5jdGlvbigpe3QucG9seWdvblN0YXJ0KCksUy5saW5lU3Rh
cnQ9Y30scG9seWdvbkVuZDpmdW5jdGlvbigpe3QucG9seWdvbkVuZCgpLFMubGlu
ZVN0YXJ0PXJ9fTtyZXR1cm4gU31mdW5jdGlvbiB1KHQsZSxyLGEsYyxsLHMsZixo
LGcscCx2LGQsbSl7dmFyIHk9cy10LHg9Zi1lLE09eSp5K3gqeDtpZihNPjQqaSYm
ZC0tKXt2YXIgXz1hK2csYj1jK3Asdz1sK3YsUz1NYXRoLnNxcnQoXypfK2IqYit3
KncpLGs9TWF0aC5hc2luKHcvPVMpLEU9aWEoaWEodyktMSk8RWF8fGlhKHItaCk8
RWE/KHIraCkvMjpNYXRoLmF0YW4yKGIsXyksQT1uKEUsayksQz1BWzBdLE49QVsx
XSx6PUMtdCxMPU4tZSxUPXgqei15Kkw7KFQqVC9NPml8fGlhKCh5KnoreCpMKS9N
LS41KT4uM3x8bz5hKmcrYypwK2wqdikmJih1KHQsZSxyLGEsYyxsLEMsTixFLF8v
PVMsYi89Uyx3LGQsbSksbS5wb2ludChDLE4pLHUoQyxOLEUsXyxiLHcscyxmLGgs
ZyxwLHYsZCxtKSl9fXZhciBpPS41LG89TWF0aC5jb3MoMzAqQ2EpLGE9MTY7cmV0
dXJuIHQucHJlY2lzaW9uPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyhhPShpPW4qbik+MCYmMTYsdCk6TWF0aC5zcXJ0KGkpCn0sdH1mdW5jdGlv
biBLZShuKXt2YXIgdD1HZShmdW5jdGlvbih0LGUpe3JldHVybiBuKFt0Kk5hLGUq
TmFdKX0pO3JldHVybiBmdW5jdGlvbihuKXtyZXR1cm4gcnIodChuKSl9fWZ1bmN0
aW9uIFFlKG4pe3RoaXMuc3RyZWFtPW59ZnVuY3Rpb24gbnIobix0KXtyZXR1cm57
cG9pbnQ6dCxzcGhlcmU6ZnVuY3Rpb24oKXtuLnNwaGVyZSgpfSxsaW5lU3RhcnQ6
ZnVuY3Rpb24oKXtuLmxpbmVTdGFydCgpfSxsaW5lRW5kOmZ1bmN0aW9uKCl7bi5s
aW5lRW5kKCl9LHBvbHlnb25TdGFydDpmdW5jdGlvbigpe24ucG9seWdvblN0YXJ0
KCl9LHBvbHlnb25FbmQ6ZnVuY3Rpb24oKXtuLnBvbHlnb25FbmQoKX19fWZ1bmN0
aW9uIHRyKG4pe3JldHVybiBlcihmdW5jdGlvbigpe3JldHVybiBufSkoKX1mdW5j
dGlvbiBlcihuKXtmdW5jdGlvbiB0KG4pe3JldHVybiBuPWEoblswXSpDYSxuWzFd
KkNhKSxbblswXSpoK2MsbC1uWzFdKmhdfWZ1bmN0aW9uIGUobil7cmV0dXJuIG49
YS5pbnZlcnQoKG5bMF0tYykvaCwobC1uWzFdKS9oKSxuJiZbblswXSpOYSxuWzFd
Kk5hXX1mdW5jdGlvbiByKCl7YT1IZShvPW9yKG0seSx4KSxpKTt2YXIgbj1pKHYs
ZCk7cmV0dXJuIGM9Zy1uWzBdKmgsbD1wK25bMV0qaCx1KCl9ZnVuY3Rpb24gdSgp
e3JldHVybiBzJiYocy52YWxpZD0hMSxzPW51bGwpLHR9dmFyIGksbyxhLGMsbCxz
LGY9R2UoZnVuY3Rpb24obix0KXtyZXR1cm4gbj1pKG4sdCksW25bMF0qaCtjLGwt
blsxXSpoXX0pLGg9MTUwLGc9NDgwLHA9MjUwLHY9MCxkPTAsbT0wLHk9MCx4PTAs
TT1rYyxfPXd0LGI9bnVsbCx3PW51bGw7cmV0dXJuIHQuc3RyZWFtPWZ1bmN0aW9u
KG4pe3JldHVybiBzJiYocy52YWxpZD0hMSkscz1ycihNKG8sZihfKG4pKSkpLHMu
dmFsaWQ9ITAsc30sdC5jbGlwQW5nbGU9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KE09bnVsbD09bj8oYj1uLGtjKTpQZSgoYj0rbikqQ2EpLHUo
KSk6Yn0sdC5jbGlwRXh0ZW50PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMu
bGVuZ3RoPyh3PW4sXz1uP2plKG5bMF1bMF0sblswXVsxXSxuWzFdWzBdLG5bMV1b
MV0pOnd0LHUoKSk6d30sdC5zY2FsZT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1l
bnRzLmxlbmd0aD8oaD0rbixyKCkpOmh9LHQudHJhbnNsYXRlPWZ1bmN0aW9uKG4p
e3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhnPStuWzBdLHA9K25bMV0scigpKTpb
ZyxwXX0sdC5jZW50ZXI9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KHY9blswXSUzNjAqQ2EsZD1uWzFdJTM2MCpDYSxyKCkpOlt2Kk5hLGQqTmFd
fSx0LnJvdGF0ZT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8o
bT1uWzBdJTM2MCpDYSx5PW5bMV0lMzYwKkNhLHg9bi5sZW5ndGg+Mj9uWzJdJTM2
MCpDYTowLHIoKSk6W20qTmEseSpOYSx4Kk5hXX0sVm8ucmViaW5kKHQsZiwicHJl
Y2lzaW9uIiksZnVuY3Rpb24oKXtyZXR1cm4gaT1uLmFwcGx5KHRoaXMsYXJndW1l
bnRzKSx0LmludmVydD1pLmludmVydCYmZSxyKCl9fWZ1bmN0aW9uIHJyKG4pe3Jl
dHVybiBucihuLGZ1bmN0aW9uKHQsZSl7bi5wb2ludCh0KkNhLGUqQ2EpfSl9ZnVu
Y3Rpb24gdXIobix0KXtyZXR1cm5bbix0XX1mdW5jdGlvbiBpcihuLHQpe3JldHVy
bltuPndhP24tU2E6LXdhPm4/bitTYTpuLHRdfWZ1bmN0aW9uIG9yKG4sdCxlKXty
ZXR1cm4gbj90fHxlP0hlKGNyKG4pLGxyKHQsZSkpOmNyKG4pOnR8fGU/bHIodCxl
KTppcn1mdW5jdGlvbiBhcihuKXtyZXR1cm4gZnVuY3Rpb24odCxlKXtyZXR1cm4g
dCs9bixbdD53YT90LVNhOi13YT50P3QrU2E6dCxlXX19ZnVuY3Rpb24gY3Iobil7
dmFyIHQ9YXIobik7cmV0dXJuIHQuaW52ZXJ0PWFyKC1uKSx0fWZ1bmN0aW9uIGxy
KG4sdCl7ZnVuY3Rpb24gZShuLHQpe3ZhciBlPU1hdGguY29zKHQpLGE9TWF0aC5j
b3MobikqZSxjPU1hdGguc2luKG4pKmUsbD1NYXRoLnNpbih0KSxzPWwqcithKnU7
cmV0dXJuW01hdGguYXRhbjIoYyppLXMqbyxhKnItbCp1KSxHKHMqaStjKm8pXX12
YXIgcj1NYXRoLmNvcyhuKSx1PU1hdGguc2luKG4pLGk9TWF0aC5jb3ModCksbz1N
YXRoLnNpbih0KTtyZXR1cm4gZS5pbnZlcnQ9ZnVuY3Rpb24obix0KXt2YXIgZT1N
YXRoLmNvcyh0KSxhPU1hdGguY29zKG4pKmUsYz1NYXRoLnNpbihuKSplLGw9TWF0
aC5zaW4odCkscz1sKmktYypvO3JldHVybltNYXRoLmF0YW4yKGMqaStsKm8sYSpy
K3MqdSksRyhzKnItYSp1KV19LGV9ZnVuY3Rpb24gc3Iobix0KXt2YXIgZT1NYXRo
LmNvcyhuKSxyPU1hdGguc2luKG4pO3JldHVybiBmdW5jdGlvbih1LGksbyxhKXt2
YXIgYz1vKnQ7bnVsbCE9dT8odT1mcihlLHUpLGk9ZnIoZSxpKSwobz4wP2k+dTp1
PmkpJiYodSs9bypTYSkpOih1PW4rbypTYSxpPW4tLjUqYyk7Zm9yKHZhciBsLHM9
dTtvPjA/cz5pOmk+cztzLT1jKWEucG9pbnQoKGw9bWUoW2UsLXIqTWF0aC5jb3Mo
cyksLXIqTWF0aC5zaW4ocyldKSlbMF0sbFsxXSl9fWZ1bmN0aW9uIGZyKG4sdCl7
dmFyIGU9ZmUodCk7ZVswXS09bixkZShlKTt2YXIgcj1KKC1lWzFdKTtyZXR1cm4o
KC1lWzJdPDA/LXI6cikrMipNYXRoLlBJLUVhKSUoMipNYXRoLlBJKX1mdW5jdGlv
biBocihuLHQsZSl7dmFyIHI9Vm8ucmFuZ2Uobix0LUVhLGUpLmNvbmNhdCh0KTty
ZXR1cm4gZnVuY3Rpb24obil7cmV0dXJuIHIubWFwKGZ1bmN0aW9uKHQpe3JldHVy
bltuLHRdfSl9fWZ1bmN0aW9uIGdyKG4sdCxlKXt2YXIgcj1Wby5yYW5nZShuLHQt
RWEsZSkuY29uY2F0KHQpO3JldHVybiBmdW5jdGlvbihuKXtyZXR1cm4gci5tYXAo
ZnVuY3Rpb24odCl7cmV0dXJuW3Qsbl19KX19ZnVuY3Rpb24gcHIobil7cmV0dXJu
IG4uc291cmNlfWZ1bmN0aW9uIHZyKG4pe3JldHVybiBuLnRhcmdldH1mdW5jdGlv
biBkcihuLHQsZSxyKXt2YXIgdT1NYXRoLmNvcyh0KSxpPU1hdGguc2luKHQpLG89
TWF0aC5jb3MociksYT1NYXRoLnNpbihyKSxjPXUqTWF0aC5jb3MobiksbD11Kk1h
dGguc2luKG4pLHM9bypNYXRoLmNvcyhlKSxmPW8qTWF0aC5zaW4oZSksaD0yKk1h
dGguYXNpbihNYXRoLnNxcnQodHQoci10KSt1Km8qdHQoZS1uKSkpLGc9MS9NYXRo
LnNpbihoKSxwPWg/ZnVuY3Rpb24obil7dmFyIHQ9TWF0aC5zaW4obio9aCkqZyxl
PU1hdGguc2luKGgtbikqZyxyPWUqYyt0KnMsdT1lKmwrdCpmLG89ZSppK3QqYTty
ZXR1cm5bTWF0aC5hdGFuMih1LHIpKk5hLE1hdGguYXRhbjIobyxNYXRoLnNxcnQo
cipyK3UqdSkpKk5hXX06ZnVuY3Rpb24oKXtyZXR1cm5bbipOYSx0Kk5hXX07cmV0
dXJuIHAuZGlzdGFuY2U9aCxwfWZ1bmN0aW9uIG1yKCl7ZnVuY3Rpb24gbihuLHUp
e3ZhciBpPU1hdGguc2luKHUqPUNhKSxvPU1hdGguY29zKHUpLGE9aWEoKG4qPUNh
KS10KSxjPU1hdGguY29zKGEpO1BjKz1NYXRoLmF0YW4yKE1hdGguc3FydCgoYT1v
Kk1hdGguc2luKGEpKSphKyhhPXIqaS1lKm8qYykqYSksZSppK3IqbypjKSx0PW4s
ZT1pLHI9b312YXIgdCxlLHI7VWMucG9pbnQ9ZnVuY3Rpb24odSxpKXt0PXUqQ2Es
ZT1NYXRoLnNpbihpKj1DYSkscj1NYXRoLmNvcyhpKSxVYy5wb2ludD1ufSxVYy5s
aW5lRW5kPWZ1bmN0aW9uKCl7VWMucG9pbnQ9VWMubGluZUVuZD12fX1mdW5jdGlv
biB5cihuLHQpe2Z1bmN0aW9uIGUodCxlKXt2YXIgcj1NYXRoLmNvcyh0KSx1PU1h
dGguY29zKGUpLGk9bihyKnUpO3JldHVybltpKnUqTWF0aC5zaW4odCksaSpNYXRo
LnNpbihlKV19cmV0dXJuIGUuaW52ZXJ0PWZ1bmN0aW9uKG4sZSl7dmFyIHI9TWF0
aC5zcXJ0KG4qbitlKmUpLHU9dChyKSxpPU1hdGguc2luKHUpLG89TWF0aC5jb3Mo
dSk7cmV0dXJuW01hdGguYXRhbjIobippLHIqbyksTWF0aC5hc2luKHImJmUqaS9y
KV19LGV9ZnVuY3Rpb24geHIobix0KXtmdW5jdGlvbiBlKG4sdCl7bz4wPy1rYStF
YT50JiYodD0ta2ErRWEpOnQ+a2EtRWEmJih0PWthLUVhKTt2YXIgZT1vL01hdGgu
cG93KHUodCksaSk7cmV0dXJuW2UqTWF0aC5zaW4oaSpuKSxvLWUqTWF0aC5jb3Mo
aSpuKV19dmFyIHI9TWF0aC5jb3MobiksdT1mdW5jdGlvbihuKXtyZXR1cm4gTWF0
aC50YW4od2EvNCtuLzIpfSxpPW49PT10P01hdGguc2luKG4pOk1hdGgubG9nKHIv
TWF0aC5jb3ModCkpL01hdGgubG9nKHUodCkvdShuKSksbz1yKk1hdGgucG93KHUo
biksaSkvaTtyZXR1cm4gaT8oZS5pbnZlcnQ9ZnVuY3Rpb24obix0KXt2YXIgZT1v
LXQscj1CKGkpKk1hdGguc3FydChuKm4rZSplKTtyZXR1cm5bTWF0aC5hdGFuMihu
LGUpL2ksMipNYXRoLmF0YW4oTWF0aC5wb3coby9yLDEvaSkpLWthXX0sZSk6X3J9
ZnVuY3Rpb24gTXIobix0KXtmdW5jdGlvbiBlKG4sdCl7dmFyIGU9aS10O3JldHVy
bltlKk1hdGguc2luKHUqbiksaS1lKk1hdGguY29zKHUqbildfXZhciByPU1hdGgu
Y29zKG4pLHU9bj09PXQ/TWF0aC5zaW4obik6KHItTWF0aC5jb3ModCkpLyh0LW4p
LGk9ci91K247cmV0dXJuIGlhKHUpPEVhP3VyOihlLmludmVydD1mdW5jdGlvbihu
LHQpe3ZhciBlPWktdDtyZXR1cm5bTWF0aC5hdGFuMihuLGUpL3UsaS1CKHUpKk1h
dGguc3FydChuKm4rZSplKV19LGUpfWZ1bmN0aW9uIF9yKG4sdCl7cmV0dXJuW24s
TWF0aC5sb2coTWF0aC50YW4od2EvNCt0LzIpKV19ZnVuY3Rpb24gYnIobil7dmFy
IHQsZT10cihuKSxyPWUuc2NhbGUsdT1lLnRyYW5zbGF0ZSxpPWUuY2xpcEV4dGVu
dDtyZXR1cm4gZS5zY2FsZT1mdW5jdGlvbigpe3ZhciBuPXIuYXBwbHkoZSxhcmd1
bWVudHMpO3JldHVybiBuPT09ZT90P2UuY2xpcEV4dGVudChudWxsKTplOm59LGUu
dHJhbnNsYXRlPWZ1bmN0aW9uKCl7dmFyIG49dS5hcHBseShlLGFyZ3VtZW50cyk7
cmV0dXJuIG49PT1lP3Q/ZS5jbGlwRXh0ZW50KG51bGwpOmU6bn0sZS5jbGlwRXh0
ZW50PWZ1bmN0aW9uKG4pe3ZhciBvPWkuYXBwbHkoZSxhcmd1bWVudHMpO2lmKG89
PT1lKXtpZih0PW51bGw9PW4pe3ZhciBhPXdhKnIoKSxjPXUoKTtpKFtbY1swXS1h
LGNbMV0tYV0sW2NbMF0rYSxjWzFdK2FdXSl9fWVsc2UgdCYmKG89bnVsbCk7cmV0
dXJuIG99LGUuY2xpcEV4dGVudChudWxsKX1mdW5jdGlvbiB3cihuLHQpe3JldHVy
bltNYXRoLmxvZyhNYXRoLnRhbih3YS80K3QvMikpLC1uXX1mdW5jdGlvbiBTcihu
KXtyZXR1cm4gblswXX1mdW5jdGlvbiBrcihuKXtyZXR1cm4gblsxXX1mdW5jdGlv
biBFcihuKXtmb3IodmFyIHQ9bi5sZW5ndGgsZT1bMCwxXSxyPTIsdT0yO3Q+dTt1
Kyspe2Zvcig7cj4xJiZXKG5bZVtyLTJdXSxuW2Vbci0xXV0sblt1XSk8PTA7KS0t
cjtlW3IrK109dX1yZXR1cm4gZS5zbGljZSgwLHIpfWZ1bmN0aW9uIEFyKG4sdCl7
cmV0dXJuIG5bMF0tdFswXXx8blsxXS10WzFdfWZ1bmN0aW9uIENyKG4sdCxlKXty
ZXR1cm4oZVswXS10WzBdKSooblsxXS10WzFdKTwoZVsxXS10WzFdKSooblswXS10
WzBdKX1mdW5jdGlvbiBOcihuLHQsZSxyKXt2YXIgdT1uWzBdLGk9ZVswXSxvPXRb
MF0tdSxhPXJbMF0taSxjPW5bMV0sbD1lWzFdLHM9dFsxXS1jLGY9clsxXS1sLGg9
KGEqKGMtbCktZioodS1pKSkvKGYqby1hKnMpO3JldHVyblt1K2gqbyxjK2gqc119
ZnVuY3Rpb24genIobil7dmFyIHQ9blswXSxlPW5bbi5sZW5ndGgtMV07cmV0dXJu
ISh0WzBdLWVbMF18fHRbMV0tZVsxXSl9ZnVuY3Rpb24gTHIoKXtLcih0aGlzKSx0
aGlzLmVkZ2U9dGhpcy5zaXRlPXRoaXMuY2lyY2xlPW51bGx9ZnVuY3Rpb24gVHIo
bil7dmFyIHQ9V2MucG9wKCl8fG5ldyBMcjtyZXR1cm4gdC5zaXRlPW4sdH1mdW5j
dGlvbiBxcihuKXtJcihuKSxYYy5yZW1vdmUobiksV2MucHVzaChuKSxLcihuKX1m
dW5jdGlvbiBScihuKXt2YXIgdD1uLmNpcmNsZSxlPXQueCxyPXQuY3ksdT17eDpl
LHk6cn0saT1uLlAsbz1uLk4sYT1bbl07cXIobik7Zm9yKHZhciBjPWk7Yy5jaXJj
bGUmJmlhKGUtYy5jaXJjbGUueCk8RWEmJmlhKHItYy5jaXJjbGUuY3kpPEVhOylp
PWMuUCxhLnVuc2hpZnQoYykscXIoYyksYz1pO2EudW5zaGlmdChjKSxJcihjKTtm
b3IodmFyIGw9bztsLmNpcmNsZSYmaWEoZS1sLmNpcmNsZS54KTxFYSYmaWEoci1s
LmNpcmNsZS5jeSk8RWE7KW89bC5OLGEucHVzaChsKSxxcihsKSxsPW87YS5wdXNo
KGwpLElyKGwpO3ZhciBzLGY9YS5sZW5ndGg7Zm9yKHM9MTtmPnM7KytzKWw9YVtz
XSxjPWFbcy0xXSxXcihsLmVkZ2UsYy5zaXRlLGwuc2l0ZSx1KTtjPWFbMF0sbD1h
W2YtMV0sbC5lZGdlPSRyKGMuc2l0ZSxsLnNpdGUsbnVsbCx1KSxZcihjKSxZcihs
KX1mdW5jdGlvbiBEcihuKXtmb3IodmFyIHQsZSxyLHUsaT1uLngsbz1uLnksYT1Y
Yy5fO2E7KWlmKHI9UHIoYSxvKS1pLHI+RWEpYT1hLkw7ZWxzZXtpZih1PWktVXIo
YSxvKSwhKHU+RWEpKXtyPi1FYT8odD1hLlAsZT1hKTp1Pi1FYT8odD1hLGU9YS5O
KTp0PWU9YTticmVha31pZighYS5SKXt0PWE7YnJlYWt9YT1hLlJ9dmFyIGM9VHIo
bik7aWYoWGMuaW5zZXJ0KHQsYyksdHx8ZSl7aWYodD09PWUpcmV0dXJuIElyKHQp
LGU9VHIodC5zaXRlKSxYYy5pbnNlcnQoYyxlKSxjLmVkZ2U9ZS5lZGdlPSRyKHQu
c2l0ZSxjLnNpdGUpLFlyKHQpLFlyKGUpLHZvaWQgMDtpZighZSlyZXR1cm4gYy5l
ZGdlPSRyKHQuc2l0ZSxjLnNpdGUpLHZvaWQgMDtJcih0KSxJcihlKTt2YXIgbD10
LnNpdGUscz1sLngsZj1sLnksaD1uLngtcyxnPW4ueS1mLHA9ZS5zaXRlLHY9cC54
LXMsZD1wLnktZixtPTIqKGgqZC1nKnYpLHk9aCpoK2cqZyx4PXYqditkKmQsTT17
eDooZCp5LWcqeCkvbStzLHk6KGgqeC12KnkpL20rZn07V3IoZS5lZGdlLGwscCxN
KSxjLmVkZ2U9JHIobCxuLG51bGwsTSksZS5lZGdlPSRyKG4scCxudWxsLE0pLFly
KHQpLFlyKGUpfX1mdW5jdGlvbiBQcihuLHQpe3ZhciBlPW4uc2l0ZSxyPWUueCx1
PWUueSxpPXUtdDtpZighaSlyZXR1cm4gcjt2YXIgbz1uLlA7aWYoIW8pcmV0dXJu
LTEvMDtlPW8uc2l0ZTt2YXIgYT1lLngsYz1lLnksbD1jLXQ7aWYoIWwpcmV0dXJu
IGE7dmFyIHM9YS1yLGY9MS9pLTEvbCxoPXMvbDtyZXR1cm4gZj8oLWgrTWF0aC5z
cXJ0KGgqaC0yKmYqKHMqcy8oLTIqbCktYytsLzIrdS1pLzIpKSkvZityOihyK2Ep
LzJ9ZnVuY3Rpb24gVXIobix0KXt2YXIgZT1uLk47aWYoZSlyZXR1cm4gUHIoZSx0
KTt2YXIgcj1uLnNpdGU7cmV0dXJuIHIueT09PXQ/ci54OjEvMH1mdW5jdGlvbiBq
cihuKXt0aGlzLnNpdGU9bix0aGlzLmVkZ2VzPVtdfWZ1bmN0aW9uIEhyKG4pe2Zv
cih2YXIgdCxlLHIsdSxpLG8sYSxjLGwscyxmPW5bMF1bMF0saD1uWzFdWzBdLGc9
blswXVsxXSxwPW5bMV1bMV0sdj1WYyxkPXYubGVuZ3RoO2QtLTspaWYoaT12W2Rd
LGkmJmkucHJlcGFyZSgpKWZvcihhPWkuZWRnZXMsYz1hLmxlbmd0aCxvPTA7Yz5v
OylzPWFbb10uZW5kKCkscj1zLngsdT1zLnksbD1hWysrbyVjXS5zdGFydCgpLHQ9
bC54LGU9bC55LChpYShyLXQpPkVhfHxpYSh1LWUpPkVhKSYmKGEuc3BsaWNlKG8s
MCxuZXcgSnIoQnIoaS5zaXRlLHMsaWEoci1mKTxFYSYmcC11PkVhP3t4OmYseTpp
YSh0LWYpPEVhP2U6cH06aWEodS1wKTxFYSYmaC1yPkVhP3t4OmlhKGUtcCk8RWE/
dDpoLHk6cH06aWEoci1oKTxFYSYmdS1nPkVhP3t4OmgseTppYSh0LWgpPEVhP2U6
Z306aWEodS1nKTxFYSYmci1mPkVhP3t4OmlhKGUtZyk8RWE/dDpmLHk6Z306bnVs
bCksaS5zaXRlLG51bGwpKSwrK2MpfWZ1bmN0aW9uIEZyKG4sdCl7cmV0dXJuIHQu
YW5nbGUtbi5hbmdsZX1mdW5jdGlvbiBPcigpe0tyKHRoaXMpLHRoaXMueD10aGlz
Lnk9dGhpcy5hcmM9dGhpcy5zaXRlPXRoaXMuY3k9bnVsbH1mdW5jdGlvbiBZcihu
KXt2YXIgdD1uLlAsZT1uLk47aWYodCYmZSl7dmFyIHI9dC5zaXRlLHU9bi5zaXRl
LGk9ZS5zaXRlO2lmKHIhPT1pKXt2YXIgbz11LngsYT11LnksYz1yLngtbyxsPXIu
eS1hLHM9aS54LW8sZj1pLnktYSxoPTIqKGMqZi1sKnMpO2lmKCEoaD49LUFhKSl7
dmFyIGc9YypjK2wqbCxwPXMqcytmKmYsdj0oZipnLWwqcCkvaCxkPShjKnAtcypn
KS9oLGY9ZCthLG09SmMucG9wKCl8fG5ldyBPcjttLmFyYz1uLG0uc2l0ZT11LG0u
eD12K28sbS55PWYrTWF0aC5zcXJ0KHYqditkKmQpLG0uY3k9ZixuLmNpcmNsZT1t
O2Zvcih2YXIgeT1udWxsLHg9QmMuXzt4OylpZihtLnk8eC55fHxtLnk9PT14Lnkm
Jm0ueDw9eC54KXtpZigheC5MKXt5PXguUDticmVha314PXguTH1lbHNle2lmKCF4
LlIpe3k9eDticmVha314PXguUn1CYy5pbnNlcnQoeSxtKSx5fHwoJGM9bSl9fX19
ZnVuY3Rpb24gSXIobil7dmFyIHQ9bi5jaXJjbGU7dCYmKHQuUHx8KCRjPXQuTiks
QmMucmVtb3ZlKHQpLEpjLnB1c2godCksS3IodCksbi5jaXJjbGU9bnVsbCl9ZnVu
Y3Rpb24gWnIobil7Zm9yKHZhciB0LGU9WmMscj1VZShuWzBdWzBdLG5bMF1bMV0s
blsxXVswXSxuWzFdWzFdKSx1PWUubGVuZ3RoO3UtLTspdD1lW3VdLCghVnIodCxu
KXx8IXIodCl8fGlhKHQuYS54LXQuYi54KTxFYSYmaWEodC5hLnktdC5iLnkpPEVh
KSYmKHQuYT10LmI9bnVsbCxlLnNwbGljZSh1LDEpKX1mdW5jdGlvbiBWcihuLHQp
e3ZhciBlPW4uYjtpZihlKXJldHVybiEwO3ZhciByLHUsaT1uLmEsbz10WzBdWzBd
LGE9dFsxXVswXSxjPXRbMF1bMV0sbD10WzFdWzFdLHM9bi5sLGY9bi5yLGg9cy54
LGc9cy55LHA9Zi54LHY9Zi55LGQ9KGgrcCkvMixtPShnK3YpLzI7aWYodj09PWcp
e2lmKG8+ZHx8ZD49YSlyZXR1cm47aWYoaD5wKXtpZihpKXtpZihpLnk+PWwpcmV0
dXJufWVsc2UgaT17eDpkLHk6Y307ZT17eDpkLHk6bH19ZWxzZXtpZihpKXtpZihp
Lnk8YylyZXR1cm59ZWxzZSBpPXt4OmQseTpsfTtlPXt4OmQseTpjfX19ZWxzZSBp
ZihyPShoLXApLyh2LWcpLHU9bS1yKmQsLTE+cnx8cj4xKWlmKGg+cCl7aWYoaSl7
aWYoaS55Pj1sKXJldHVybn1lbHNlIGk9e3g6KGMtdSkvcix5OmN9O2U9e3g6KGwt
dSkvcix5Omx9fWVsc2V7aWYoaSl7aWYoaS55PGMpcmV0dXJufWVsc2UgaT17eDoo
bC11KS9yLHk6bH07ZT17eDooYy11KS9yLHk6Y319ZWxzZSBpZih2Pmcpe2lmKGkp
e2lmKGkueD49YSlyZXR1cm59ZWxzZSBpPXt4Om8seTpyKm8rdX07ZT17eDphLHk6
ciphK3V9fWVsc2V7aWYoaSl7aWYoaS54PG8pcmV0dXJufWVsc2UgaT17eDphLHk6
ciphK3V9O2U9e3g6byx5OnIqbyt1fX1yZXR1cm4gbi5hPWksbi5iPWUsITB9ZnVu
Y3Rpb24gWHIobix0KXt0aGlzLmw9bix0aGlzLnI9dCx0aGlzLmE9dGhpcy5iPW51
bGx9ZnVuY3Rpb24gJHIobix0LGUscil7dmFyIHU9bmV3IFhyKG4sdCk7cmV0dXJu
IFpjLnB1c2godSksZSYmV3IodSxuLHQsZSksciYmV3IodSx0LG4sciksVmNbbi5p
XS5lZGdlcy5wdXNoKG5ldyBKcih1LG4sdCkpLFZjW3QuaV0uZWRnZXMucHVzaChu
ZXcgSnIodSx0LG4pKSx1fWZ1bmN0aW9uIEJyKG4sdCxlKXt2YXIgcj1uZXcgWHIo
bixudWxsKTtyZXR1cm4gci5hPXQsci5iPWUsWmMucHVzaChyKSxyfWZ1bmN0aW9u
IFdyKG4sdCxlLHIpe24uYXx8bi5iP24ubD09PWU/bi5iPXI6bi5hPXI6KG4uYT1y
LG4ubD10LG4ucj1lKX1mdW5jdGlvbiBKcihuLHQsZSl7dmFyIHI9bi5hLHU9bi5i
O3RoaXMuZWRnZT1uLHRoaXMuc2l0ZT10LHRoaXMuYW5nbGU9ZT9NYXRoLmF0YW4y
KGUueS10LnksZS54LXQueCk6bi5sPT09dD9NYXRoLmF0YW4yKHUueC1yLngsci55
LXUueSk6TWF0aC5hdGFuMihyLngtdS54LHUueS1yLnkpfWZ1bmN0aW9uIEdyKCl7
dGhpcy5fPW51bGx9ZnVuY3Rpb24gS3Iobil7bi5VPW4uQz1uLkw9bi5SPW4uUD1u
Lk49bnVsbH1mdW5jdGlvbiBRcihuLHQpe3ZhciBlPXQscj10LlIsdT1lLlU7dT91
Lkw9PT1lP3UuTD1yOnUuUj1yOm4uXz1yLHIuVT11LGUuVT1yLGUuUj1yLkwsZS5S
JiYoZS5SLlU9ZSksci5MPWV9ZnVuY3Rpb24gbnUobix0KXt2YXIgZT10LHI9dC5M
LHU9ZS5VO3U/dS5MPT09ZT91Lkw9cjp1LlI9cjpuLl89cixyLlU9dSxlLlU9cixl
Lkw9ci5SLGUuTCYmKGUuTC5VPWUpLHIuUj1lfWZ1bmN0aW9uIHR1KG4pe2Zvcig7
bi5MOyluPW4uTDtyZXR1cm4gbn1mdW5jdGlvbiBldShuLHQpe3ZhciBlLHIsdSxp
PW4uc29ydChydSkucG9wKCk7Zm9yKFpjPVtdLFZjPW5ldyBBcnJheShuLmxlbmd0
aCksWGM9bmV3IEdyLEJjPW5ldyBHcjs7KWlmKHU9JGMsaSYmKCF1fHxpLnk8dS55
fHxpLnk9PT11LnkmJmkueDx1LngpKShpLnghPT1lfHxpLnkhPT1yKSYmKFZjW2ku
aV09bmV3IGpyKGkpLERyKGkpLGU9aS54LHI9aS55KSxpPW4ucG9wKCk7ZWxzZXtp
ZighdSlicmVhaztScih1LmFyYyl9dCYmKFpyKHQpLEhyKHQpKTt2YXIgbz17Y2Vs
bHM6VmMsZWRnZXM6WmN9O3JldHVybiBYYz1CYz1aYz1WYz1udWxsLG99ZnVuY3Rp
b24gcnUobix0KXtyZXR1cm4gdC55LW4ueXx8dC54LW4ueH1mdW5jdGlvbiB1dShu
LHQsZSl7cmV0dXJuKG4ueC1lLngpKih0Lnktbi55KS0obi54LXQueCkqKGUueS1u
LnkpfWZ1bmN0aW9uIGl1KG4pe3JldHVybiBuLnh9ZnVuY3Rpb24gb3Uobil7cmV0
dXJuIG4ueX1mdW5jdGlvbiBhdSgpe3JldHVybntsZWFmOiEwLG5vZGVzOltdLHBv
aW50Om51bGwseDpudWxsLHk6bnVsbH19ZnVuY3Rpb24gY3Uobix0LGUscix1LGkp
e2lmKCFuKHQsZSxyLHUsaSkpe3ZhciBvPS41KihlK3UpLGE9LjUqKHIraSksYz10
Lm5vZGVzO2NbMF0mJmN1KG4sY1swXSxlLHIsbyxhKSxjWzFdJiZjdShuLGNbMV0s
byxyLHUsYSksY1syXSYmY3UobixjWzJdLGUsYSxvLGkpLGNbM10mJmN1KG4sY1sz
XSxvLGEsdSxpKX19ZnVuY3Rpb24gbHUobix0KXtuPVZvLnJnYihuKSx0PVZvLnJn
Yih0KTt2YXIgZT1uLnIscj1uLmcsdT1uLmIsaT10LnItZSxvPXQuZy1yLGE9dC5i
LXU7cmV0dXJuIGZ1bmN0aW9uKG4pe3JldHVybiIjIitkdChNYXRoLnJvdW5kKGUr
aSpuKSkrZHQoTWF0aC5yb3VuZChyK28qbikpK2R0KE1hdGgucm91bmQodSthKm4p
KX19ZnVuY3Rpb24gc3Uobix0KXt2YXIgZSxyPXt9LHU9e307Zm9yKGUgaW4gbill
IGluIHQ/cltlXT1ndShuW2VdLHRbZV0pOnVbZV09bltlXTtmb3IoZSBpbiB0KWUg
aW4gbnx8KHVbZV09dFtlXSk7cmV0dXJuIGZ1bmN0aW9uKG4pe2ZvcihlIGluIHIp
dVtlXT1yW2VdKG4pO3JldHVybiB1fX1mdW5jdGlvbiBmdShuLHQpe3JldHVybiB0
LT1uPStuLGZ1bmN0aW9uKGUpe3JldHVybiBuK3QqZX19ZnVuY3Rpb24gaHUobix0
KXt2YXIgZSxyLHUsaT1LYy5sYXN0SW5kZXg9UWMubGFzdEluZGV4PTAsbz0tMSxh
PVtdLGM9W107Zm9yKG4rPSIiLHQrPSIiOyhlPUtjLmV4ZWMobikpJiYocj1RYy5l
eGVjKHQpKTspKHU9ci5pbmRleCk+aSYmKHU9dC5zbGljZShpLHUpLGFbb10/YVtv
XSs9dTphWysrb109dSksKGU9ZVswXSk9PT0ocj1yWzBdKT9hW29dP2Fbb10rPXI6
YVsrK29dPXI6KGFbKytvXT1udWxsLGMucHVzaCh7aTpvLHg6ZnUoZSxyKX0pKSxp
PVFjLmxhc3RJbmRleDtyZXR1cm4gaTx0Lmxlbmd0aCYmKHU9dC5zbGljZShpKSxh
W29dP2Fbb10rPXU6YVsrK29dPXUpLGEubGVuZ3RoPDI/Y1swXT8odD1jWzBdLngs
ZnVuY3Rpb24obil7cmV0dXJuIHQobikrIiJ9KTpmdW5jdGlvbigpe3JldHVybiB0
fToodD1jLmxlbmd0aCxmdW5jdGlvbihuKXtmb3IodmFyIGUscj0wO3Q+cjsrK3Ip
YVsoZT1jW3JdKS5pXT1lLngobik7cmV0dXJuIGEuam9pbigiIil9KX1mdW5jdGlv
biBndShuLHQpe2Zvcih2YXIgZSxyPVZvLmludGVycG9sYXRvcnMubGVuZ3RoOy0t
cj49MCYmIShlPVZvLmludGVycG9sYXRvcnNbcl0obix0KSk7KTtyZXR1cm4gZX1m
dW5jdGlvbiBwdShuLHQpe3ZhciBlLHI9W10sdT1bXSxpPW4ubGVuZ3RoLG89dC5s
ZW5ndGgsYT1NYXRoLm1pbihuLmxlbmd0aCx0Lmxlbmd0aCk7Zm9yKGU9MDthPmU7
KytlKXIucHVzaChndShuW2VdLHRbZV0pKTtmb3IoO2k+ZTsrK2UpdVtlXT1uW2Vd
O2Zvcig7bz5lOysrZSl1W2VdPXRbZV07cmV0dXJuIGZ1bmN0aW9uKG4pe2Zvcihl
PTA7YT5lOysrZSl1W2VdPXJbZV0obik7cmV0dXJuIHV9fWZ1bmN0aW9uIHZ1KG4p
e3JldHVybiBmdW5jdGlvbih0KXtyZXR1cm4gMD49dD8wOnQ+PTE/MTpuKHQpfX1m
dW5jdGlvbiBkdShuKXtyZXR1cm4gZnVuY3Rpb24odCl7cmV0dXJuIDEtbigxLXQp
fX1mdW5jdGlvbiBtdShuKXtyZXR1cm4gZnVuY3Rpb24odCl7cmV0dXJuLjUqKC41
PnQ/bigyKnQpOjItbigyLTIqdCkpfX1mdW5jdGlvbiB5dShuKXtyZXR1cm4gbipu
fWZ1bmN0aW9uIHh1KG4pe3JldHVybiBuKm4qbn1mdW5jdGlvbiBNdShuKXtpZigw
Pj1uKXJldHVybiAwO2lmKG4+PTEpcmV0dXJuIDE7dmFyIHQ9bipuLGU9dCpuO3Jl
dHVybiA0KiguNT5uP2U6Myoobi10KStlLS43NSl9ZnVuY3Rpb24gX3Uobil7cmV0
dXJuIGZ1bmN0aW9uKHQpe3JldHVybiBNYXRoLnBvdyh0LG4pfX1mdW5jdGlvbiBi
dShuKXtyZXR1cm4gMS1NYXRoLmNvcyhuKmthKX1mdW5jdGlvbiB3dShuKXtyZXR1
cm4gTWF0aC5wb3coMiwxMCoobi0xKSl9ZnVuY3Rpb24gU3Uobil7cmV0dXJuIDEt
TWF0aC5zcXJ0KDEtbipuKX1mdW5jdGlvbiBrdShuLHQpe3ZhciBlO3JldHVybiBh
cmd1bWVudHMubGVuZ3RoPDImJih0PS40NSksYXJndW1lbnRzLmxlbmd0aD9lPXQv
U2EqTWF0aC5hc2luKDEvbik6KG49MSxlPXQvNCksZnVuY3Rpb24ocil7cmV0dXJu
IDErbipNYXRoLnBvdygyLC0xMCpyKSpNYXRoLnNpbigoci1lKSpTYS90KX19ZnVu
Y3Rpb24gRXUobil7cmV0dXJuIG58fChuPTEuNzAxNTgpLGZ1bmN0aW9uKHQpe3Jl
dHVybiB0KnQqKChuKzEpKnQtbil9fWZ1bmN0aW9uIEF1KG4pe3JldHVybiAxLzIu
NzU+bj83LjU2MjUqbipuOjIvMi43NT5uPzcuNTYyNSoobi09MS41LzIuNzUpKm4r
Ljc1OjIuNS8yLjc1Pm4/Ny41NjI1KihuLT0yLjI1LzIuNzUpKm4rLjkzNzU6Ny41
NjI1KihuLT0yLjYyNS8yLjc1KSpuKy45ODQzNzV9ZnVuY3Rpb24gQ3Uobix0KXtu
PVZvLmhjbChuKSx0PVZvLmhjbCh0KTt2YXIgZT1uLmgscj1uLmMsdT1uLmwsaT10
LmgtZSxvPXQuYy1yLGE9dC5sLXU7cmV0dXJuIGlzTmFOKG8pJiYobz0wLHI9aXNO
YU4ocik/dC5jOnIpLGlzTmFOKGkpPyhpPTAsZT1pc05hTihlKT90Lmg6ZSk6aT4x
ODA/aS09MzYwOi0xODA+aSYmKGkrPTM2MCksZnVuY3Rpb24obil7cmV0dXJuIG90
KGUraSpuLHIrbypuLHUrYSpuKSsiIn19ZnVuY3Rpb24gTnUobix0KXtuPVZvLmhz
bChuKSx0PVZvLmhzbCh0KTt2YXIgZT1uLmgscj1uLnMsdT1uLmwsaT10LmgtZSxv
PXQucy1yLGE9dC5sLXU7cmV0dXJuIGlzTmFOKG8pJiYobz0wLHI9aXNOYU4ocik/
dC5zOnIpLGlzTmFOKGkpPyhpPTAsZT1pc05hTihlKT90Lmg6ZSk6aT4xODA/aS09
MzYwOi0xODA+aSYmKGkrPTM2MCksZnVuY3Rpb24obil7cmV0dXJuIHV0KGUraSpu
LHIrbypuLHUrYSpuKSsiIn19ZnVuY3Rpb24genUobix0KXtuPVZvLmxhYihuKSx0
PVZvLmxhYih0KTt2YXIgZT1uLmwscj1uLmEsdT1uLmIsaT10LmwtZSxvPXQuYS1y
LGE9dC5iLXU7cmV0dXJuIGZ1bmN0aW9uKG4pe3JldHVybiBjdChlK2kqbixyK28q
bix1K2EqbikrIiJ9fWZ1bmN0aW9uIEx1KG4sdCl7cmV0dXJuIHQtPW4sZnVuY3Rp
b24oZSl7cmV0dXJuIE1hdGgucm91bmQobit0KmUpfX1mdW5jdGlvbiBUdShuKXt2
YXIgdD1bbi5hLG4uYl0sZT1bbi5jLG4uZF0scj1SdSh0KSx1PXF1KHQsZSksaT1S
dShEdShlLHQsLXUpKXx8MDt0WzBdKmVbMV08ZVswXSp0WzFdJiYodFswXSo9LTEs
dFsxXSo9LTEscio9LTEsdSo9LTEpLHRoaXMucm90YXRlPShyP01hdGguYXRhbjIo
dFsxXSx0WzBdKTpNYXRoLmF0YW4yKC1lWzBdLGVbMV0pKSpOYSx0aGlzLnRyYW5z
bGF0ZT1bbi5lLG4uZl0sdGhpcy5zY2FsZT1bcixpXSx0aGlzLnNrZXc9aT9NYXRo
LmF0YW4yKHUsaSkqTmE6MH1mdW5jdGlvbiBxdShuLHQpe3JldHVybiBuWzBdKnRb
MF0rblsxXSp0WzFdfWZ1bmN0aW9uIFJ1KG4pe3ZhciB0PU1hdGguc3FydChxdShu
LG4pKTtyZXR1cm4gdCYmKG5bMF0vPXQsblsxXS89dCksdH1mdW5jdGlvbiBEdShu
LHQsZSl7cmV0dXJuIG5bMF0rPWUqdFswXSxuWzFdKz1lKnRbMV0sbn1mdW5jdGlv
biBQdShuLHQpe3ZhciBlLHI9W10sdT1bXSxpPVZvLnRyYW5zZm9ybShuKSxvPVZv
LnRyYW5zZm9ybSh0KSxhPWkudHJhbnNsYXRlLGM9by50cmFuc2xhdGUsbD1pLnJv
dGF0ZSxzPW8ucm90YXRlLGY9aS5za2V3LGg9by5za2V3LGc9aS5zY2FsZSxwPW8u
c2NhbGU7cmV0dXJuIGFbMF0hPWNbMF18fGFbMV0hPWNbMV0/KHIucHVzaCgidHJh
bnNsYXRlKCIsbnVsbCwiLCIsbnVsbCwiKSIpLHUucHVzaCh7aToxLHg6ZnUoYVsw
XSxjWzBdKX0se2k6Myx4OmZ1KGFbMV0sY1sxXSl9KSk6Y1swXXx8Y1sxXT9yLnB1
c2goInRyYW5zbGF0ZSgiK2MrIikiKTpyLnB1c2goIiIpLGwhPXM/KGwtcz4xODA/
cys9MzYwOnMtbD4xODAmJihsKz0zNjApLHUucHVzaCh7aTpyLnB1c2goci5wb3Ao
KSsicm90YXRlKCIsbnVsbCwiKSIpLTIseDpmdShsLHMpfSkpOnMmJnIucHVzaChy
LnBvcCgpKyJyb3RhdGUoIitzKyIpIiksZiE9aD91LnB1c2goe2k6ci5wdXNoKHIu
cG9wKCkrInNrZXdYKCIsbnVsbCwiKSIpLTIseDpmdShmLGgpfSk6aCYmci5wdXNo
KHIucG9wKCkrInNrZXdYKCIraCsiKSIpLGdbMF0hPXBbMF18fGdbMV0hPXBbMV0/
KGU9ci5wdXNoKHIucG9wKCkrInNjYWxlKCIsbnVsbCwiLCIsbnVsbCwiKSIpLHUu
cHVzaCh7aTplLTQseDpmdShnWzBdLHBbMF0pfSx7aTplLTIseDpmdShnWzFdLHBb
MV0pfSkpOigxIT1wWzBdfHwxIT1wWzFdKSYmci5wdXNoKHIucG9wKCkrInNjYWxl
KCIrcCsiKSIpLGU9dS5sZW5ndGgsZnVuY3Rpb24obil7Zm9yKHZhciB0LGk9LTE7
KytpPGU7KXJbKHQ9dVtpXSkuaV09dC54KG4pO3JldHVybiByLmpvaW4oIiIpfX1m
dW5jdGlvbiBVdShuLHQpe3JldHVybiB0PXQtKG49K24pPzEvKHQtbik6MCxmdW5j
dGlvbihlKXtyZXR1cm4oZS1uKSp0fX1mdW5jdGlvbiBqdShuLHQpe3JldHVybiB0
PXQtKG49K24pPzEvKHQtbik6MCxmdW5jdGlvbihlKXtyZXR1cm4gTWF0aC5tYXgo
MCxNYXRoLm1pbigxLChlLW4pKnQpKX19ZnVuY3Rpb24gSHUobil7Zm9yKHZhciB0
PW4uc291cmNlLGU9bi50YXJnZXQscj1PdSh0LGUpLHU9W3RdO3QhPT1yOyl0PXQu
cGFyZW50LHUucHVzaCh0KTtmb3IodmFyIGk9dS5sZW5ndGg7ZSE9PXI7KXUuc3Bs
aWNlKGksMCxlKSxlPWUucGFyZW50O3JldHVybiB1fWZ1bmN0aW9uIEZ1KG4pe2Zv
cih2YXIgdD1bXSxlPW4ucGFyZW50O251bGwhPWU7KXQucHVzaChuKSxuPWUsZT1l
LnBhcmVudDtyZXR1cm4gdC5wdXNoKG4pLHR9ZnVuY3Rpb24gT3Uobix0KXtpZihu
PT09dClyZXR1cm4gbjtmb3IodmFyIGU9RnUobikscj1GdSh0KSx1PWUucG9wKCks
aT1yLnBvcCgpLG89bnVsbDt1PT09aTspbz11LHU9ZS5wb3AoKSxpPXIucG9wKCk7
cmV0dXJuIG99ZnVuY3Rpb24gWXUobil7bi5maXhlZHw9Mn1mdW5jdGlvbiBJdShu
KXtuLmZpeGVkJj0tN31mdW5jdGlvbiBadShuKXtuLmZpeGVkfD00LG4ucHg9bi54
LG4ucHk9bi55fWZ1bmN0aW9uIFZ1KG4pe24uZml4ZWQmPS01fWZ1bmN0aW9uIFh1
KG4sdCxlKXt2YXIgcj0wLHU9MDtpZihuLmNoYXJnZT0wLCFuLmxlYWYpZm9yKHZh
ciBpLG89bi5ub2RlcyxhPW8ubGVuZ3RoLGM9LTE7KytjPGE7KWk9b1tjXSxudWxs
IT1pJiYoWHUoaSx0LGUpLG4uY2hhcmdlKz1pLmNoYXJnZSxyKz1pLmNoYXJnZSpp
LmN4LHUrPWkuY2hhcmdlKmkuY3kpO2lmKG4ucG9pbnQpe24ubGVhZnx8KG4ucG9p
bnQueCs9TWF0aC5yYW5kb20oKS0uNSxuLnBvaW50LnkrPU1hdGgucmFuZG9tKCkt
LjUpO3ZhciBsPXQqZVtuLnBvaW50LmluZGV4XTtuLmNoYXJnZSs9bi5wb2ludENo
YXJnZT1sLHIrPWwqbi5wb2ludC54LHUrPWwqbi5wb2ludC55fW4uY3g9ci9uLmNo
YXJnZSxuLmN5PXUvbi5jaGFyZ2V9ZnVuY3Rpb24gJHUobix0KXtyZXR1cm4gVm8u
cmViaW5kKG4sdCwic29ydCIsImNoaWxkcmVuIiwidmFsdWUiKSxuLm5vZGVzPW4s
bi5saW5rcz1RdSxufWZ1bmN0aW9uIEJ1KG4sdCl7Zm9yKHZhciBlPVtuXTtudWxs
IT0obj1lLnBvcCgpKTspaWYodChuKSwodT1uLmNoaWxkcmVuKSYmKHI9dS5sZW5n
dGgpKWZvcih2YXIgcix1Oy0tcj49MDspZS5wdXNoKHVbcl0pfWZ1bmN0aW9uIFd1
KG4sdCl7Zm9yKHZhciBlPVtuXSxyPVtdO251bGwhPShuPWUucG9wKCkpOylpZihy
LnB1c2gobiksKGk9bi5jaGlsZHJlbikmJih1PWkubGVuZ3RoKSlmb3IodmFyIHUs
aSxvPS0xOysrbzx1OyllLnB1c2goaVtvXSk7Zm9yKDtudWxsIT0obj1yLnBvcCgp
KTspdChuKX1mdW5jdGlvbiBKdShuKXtyZXR1cm4gbi5jaGlsZHJlbn1mdW5jdGlv
biBHdShuKXtyZXR1cm4gbi52YWx1ZX1mdW5jdGlvbiBLdShuLHQpe3JldHVybiB0
LnZhbHVlLW4udmFsdWV9ZnVuY3Rpb24gUXUobil7cmV0dXJuIFZvLm1lcmdlKG4u
bWFwKGZ1bmN0aW9uKG4pe3JldHVybihuLmNoaWxkcmVufHxbXSkubWFwKGZ1bmN0
aW9uKHQpe3JldHVybntzb3VyY2U6bix0YXJnZXQ6dH19KX0pKX1mdW5jdGlvbiBu
aShuKXtyZXR1cm4gbi54fWZ1bmN0aW9uIHRpKG4pe3JldHVybiBuLnl9ZnVuY3Rp
b24gZWkobix0LGUpe24ueTA9dCxuLnk9ZX1mdW5jdGlvbiByaShuKXtyZXR1cm4g
Vm8ucmFuZ2Uobi5sZW5ndGgpfWZ1bmN0aW9uIHVpKG4pe2Zvcih2YXIgdD0tMSxl
PW5bMF0ubGVuZ3RoLHI9W107Kyt0PGU7KXJbdF09MDtyZXR1cm4gcn1mdW5jdGlv
biBpaShuKXtmb3IodmFyIHQsZT0xLHI9MCx1PW5bMF1bMV0saT1uLmxlbmd0aDtp
PmU7KytlKSh0PW5bZV1bMV0pPnUmJihyPWUsdT10KTtyZXR1cm4gcn1mdW5jdGlv
biBvaShuKXtyZXR1cm4gbi5yZWR1Y2UoYWksMCl9ZnVuY3Rpb24gYWkobix0KXty
ZXR1cm4gbit0WzFdfWZ1bmN0aW9uIGNpKG4sdCl7cmV0dXJuIGxpKG4sTWF0aC5j
ZWlsKE1hdGgubG9nKHQubGVuZ3RoKS9NYXRoLkxOMisxKSl9ZnVuY3Rpb24gbGko
bix0KXtmb3IodmFyIGU9LTEscj0rblswXSx1PShuWzFdLXIpL3QsaT1bXTsrK2U8
PXQ7KWlbZV09dSplK3I7cmV0dXJuIGl9ZnVuY3Rpb24gc2kobil7cmV0dXJuW1Zv
Lm1pbihuKSxWby5tYXgobildfWZ1bmN0aW9uIGZpKG4sdCl7cmV0dXJuIG4udmFs
dWUtdC52YWx1ZX1mdW5jdGlvbiBoaShuLHQpe3ZhciBlPW4uX3BhY2tfbmV4dDtu
Ll9wYWNrX25leHQ9dCx0Ll9wYWNrX3ByZXY9bix0Ll9wYWNrX25leHQ9ZSxlLl9w
YWNrX3ByZXY9dH1mdW5jdGlvbiBnaShuLHQpe24uX3BhY2tfbmV4dD10LHQuX3Bh
Y2tfcHJldj1ufWZ1bmN0aW9uIHBpKG4sdCl7dmFyIGU9dC54LW4ueCxyPXQueS1u
LnksdT1uLnIrdC5yO3JldHVybi45OTkqdSp1PmUqZStyKnJ9ZnVuY3Rpb24gdmko
bil7ZnVuY3Rpb24gdChuKXtzPU1hdGgubWluKG4ueC1uLnIscyksZj1NYXRoLm1h
eChuLngrbi5yLGYpLGg9TWF0aC5taW4obi55LW4ucixoKSxnPU1hdGgubWF4KG4u
eStuLnIsZyl9aWYoKGU9bi5jaGlsZHJlbikmJihsPWUubGVuZ3RoKSl7dmFyIGUs
cix1LGksbyxhLGMsbCxzPTEvMCxmPS0xLzAsaD0xLzAsZz0tMS8wO2lmKGUuZm9y
RWFjaChkaSkscj1lWzBdLHIueD0tci5yLHIueT0wLHQociksbD4xJiYodT1lWzFd
LHUueD11LnIsdS55PTAsdCh1KSxsPjIpKWZvcihpPWVbMl0seGkocix1LGkpLHQo
aSksaGkocixpKSxyLl9wYWNrX3ByZXY9aSxoaShpLHUpLHU9ci5fcGFja19uZXh0
LG89MztsPm87bysrKXt4aShyLHUsaT1lW29dKTt2YXIgcD0wLHY9MSxkPTE7Zm9y
KGE9dS5fcGFja19uZXh0O2EhPT11O2E9YS5fcGFja19uZXh0LHYrKylpZihwaShh
LGkpKXtwPTE7YnJlYWt9aWYoMT09cClmb3IoYz1yLl9wYWNrX3ByZXY7YyE9PWEu
X3BhY2tfcHJldiYmIXBpKGMsaSk7Yz1jLl9wYWNrX3ByZXYsZCsrKTtwPyhkPnZ8
fHY9PWQmJnUucjxyLnI/Z2kocix1PWEpOmdpKHI9Yyx1KSxvLS0pOihoaShyLGkp
LHU9aSx0KGkpKX12YXIgbT0ocytmKS8yLHk9KGgrZykvMix4PTA7Zm9yKG89MDts
Pm87bysrKWk9ZVtvXSxpLngtPW0saS55LT15LHg9TWF0aC5tYXgoeCxpLnIrTWF0
aC5zcXJ0KGkueCppLngraS55KmkueSkpO24ucj14LGUuZm9yRWFjaChtaSl9fWZ1
bmN0aW9uIGRpKG4pe24uX3BhY2tfbmV4dD1uLl9wYWNrX3ByZXY9bn1mdW5jdGlv
biBtaShuKXtkZWxldGUgbi5fcGFja19uZXh0LGRlbGV0ZSBuLl9wYWNrX3ByZXZ9
ZnVuY3Rpb24geWkobix0LGUscil7dmFyIHU9bi5jaGlsZHJlbjtpZihuLng9dCs9
cipuLngsbi55PWUrPXIqbi55LG4ucio9cix1KWZvcih2YXIgaT0tMSxvPXUubGVu
Z3RoOysraTxvOyl5aSh1W2ldLHQsZSxyKX1mdW5jdGlvbiB4aShuLHQsZSl7dmFy
IHI9bi5yK2Uucix1PXQueC1uLngsaT10Lnktbi55O2lmKHImJih1fHxpKSl7dmFy
IG89dC5yK2UucixhPXUqdStpKmk7byo9byxyKj1yO3ZhciBjPS41KyhyLW8pLygy
KmEpLGw9TWF0aC5zcXJ0KE1hdGgubWF4KDAsMipvKihyK2EpLShyLT1hKSpyLW8q
bykpLygyKmEpO2UueD1uLngrYyp1K2wqaSxlLnk9bi55K2MqaS1sKnV9ZWxzZSBl
Lng9bi54K3IsZS55PW4ueX1mdW5jdGlvbiBNaShuLHQpe3JldHVybiBuLnBhcmVu
dD09dC5wYXJlbnQ/MToyfWZ1bmN0aW9uIF9pKG4pe3ZhciB0PW4uY2hpbGRyZW47
cmV0dXJuIHQubGVuZ3RoP3RbMF06bi50fWZ1bmN0aW9uIGJpKG4pe3ZhciB0LGU9
bi5jaGlsZHJlbjtyZXR1cm4odD1lLmxlbmd0aCk/ZVt0LTFdOm4udH1mdW5jdGlv
biB3aShuLHQsZSl7dmFyIHI9ZS8odC5pLW4uaSk7dC5jLT1yLHQucys9ZSxuLmMr
PXIsdC56Kz1lLHQubSs9ZX1mdW5jdGlvbiBTaShuKXtmb3IodmFyIHQsZT0wLHI9
MCx1PW4uY2hpbGRyZW4saT11Lmxlbmd0aDstLWk+PTA7KXQ9dVtpXSx0LnorPWUs
dC5tKz1lLGUrPXQucysocis9dC5jKX1mdW5jdGlvbiBraShuLHQsZSl7cmV0dXJu
IG4uYS5wYXJlbnQ9PT10LnBhcmVudD9uLmE6ZX1mdW5jdGlvbiBFaShuKXtyZXR1
cm4gMStWby5tYXgobixmdW5jdGlvbihuKXtyZXR1cm4gbi55fSl9ZnVuY3Rpb24g
QWkobil7cmV0dXJuIG4ucmVkdWNlKGZ1bmN0aW9uKG4sdCl7cmV0dXJuIG4rdC54
fSwwKS9uLmxlbmd0aH1mdW5jdGlvbiBDaShuKXt2YXIgdD1uLmNoaWxkcmVuO3Jl
dHVybiB0JiZ0Lmxlbmd0aD9DaSh0WzBdKTpufWZ1bmN0aW9uIE5pKG4pe3ZhciB0
LGU9bi5jaGlsZHJlbjtyZXR1cm4gZSYmKHQ9ZS5sZW5ndGgpP05pKGVbdC0xXSk6
bn1mdW5jdGlvbiB6aShuKXtyZXR1cm57eDpuLngseTpuLnksZHg6bi5keCxkeTpu
LmR5fX1mdW5jdGlvbiBMaShuLHQpe3ZhciBlPW4ueCt0WzNdLHI9bi55K3RbMF0s
dT1uLmR4LXRbMV0tdFszXSxpPW4uZHktdFswXS10WzJdO3JldHVybiAwPnUmJihl
Kz11LzIsdT0wKSwwPmkmJihyKz1pLzIsaT0wKSx7eDplLHk6cixkeDp1LGR5Oml9
fWZ1bmN0aW9uIFRpKG4pe3ZhciB0PW5bMF0sZT1uW24ubGVuZ3RoLTFdO3JldHVy
biBlPnQ/W3QsZV06W2UsdF19ZnVuY3Rpb24gcWkobil7cmV0dXJuIG4ucmFuZ2VF
eHRlbnQ/bi5yYW5nZUV4dGVudCgpOlRpKG4ucmFuZ2UoKSl9ZnVuY3Rpb24gUmko
bix0LGUscil7dmFyIHU9ZShuWzBdLG5bMV0pLGk9cih0WzBdLHRbMV0pO3JldHVy
biBmdW5jdGlvbihuKXtyZXR1cm4gaSh1KG4pKX19ZnVuY3Rpb24gRGkobix0KXt2
YXIgZSxyPTAsdT1uLmxlbmd0aC0xLGk9bltyXSxvPW5bdV07cmV0dXJuIGk+byYm
KGU9cixyPXUsdT1lLGU9aSxpPW8sbz1lKSxuW3JdPXQuZmxvb3IoaSksblt1XT10
LmNlaWwobyksbn1mdW5jdGlvbiBQaShuKXtyZXR1cm4gbj97Zmxvb3I6ZnVuY3Rp
b24odCl7cmV0dXJuIE1hdGguZmxvb3IodC9uKSpufSxjZWlsOmZ1bmN0aW9uKHQp
e3JldHVybiBNYXRoLmNlaWwodC9uKSpufX06c2x9ZnVuY3Rpb24gVWkobix0LGUs
cil7dmFyIHU9W10saT1bXSxvPTAsYT1NYXRoLm1pbihuLmxlbmd0aCx0Lmxlbmd0
aCktMTtmb3IoblthXTxuWzBdJiYobj1uLnNsaWNlKCkucmV2ZXJzZSgpLHQ9dC5z
bGljZSgpLnJldmVyc2UoKSk7KytvPD1hOyl1LnB1c2goZShuW28tMV0sbltvXSkp
LGkucHVzaChyKHRbby0xXSx0W29dKSk7cmV0dXJuIGZ1bmN0aW9uKHQpe3ZhciBl
PVZvLmJpc2VjdChuLHQsMSxhKS0xO3JldHVybiBpW2VdKHVbZV0odCkpfX1mdW5j
dGlvbiBqaShuLHQsZSxyKXtmdW5jdGlvbiB1KCl7dmFyIHU9TWF0aC5taW4obi5s
ZW5ndGgsdC5sZW5ndGgpPjI/VWk6UmksYz1yP2p1OlV1O3JldHVybiBvPXUobix0
LGMsZSksYT11KHQsbixjLGd1KSxpfWZ1bmN0aW9uIGkobil7cmV0dXJuIG8obil9
dmFyIG8sYTtyZXR1cm4gaS5pbnZlcnQ9ZnVuY3Rpb24obil7cmV0dXJuIGEobil9
LGkuZG9tYWluPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhu
PXQubWFwKE51bWJlciksdSgpKTpufSxpLnJhbmdlPWZ1bmN0aW9uKG4pe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyh0PW4sdSgpKTp0fSxpLnJhbmdlUm91bmQ9ZnVu
Y3Rpb24obil7cmV0dXJuIGkucmFuZ2UobikuaW50ZXJwb2xhdGUoTHUpfSxpLmNs
YW1wPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPW4sdSgp
KTpyfSxpLmludGVycG9sYXRlPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMu
bGVuZ3RoPyhlPW4sdSgpKTplfSxpLnRpY2tzPWZ1bmN0aW9uKHQpe3JldHVybiBZ
aShuLHQpfSxpLnRpY2tGb3JtYXQ9ZnVuY3Rpb24odCxlKXtyZXR1cm4gSWkobix0
LGUpfSxpLm5pY2U9ZnVuY3Rpb24odCl7cmV0dXJuIEZpKG4sdCksdSgpfSxpLmNv
cHk9ZnVuY3Rpb24oKXtyZXR1cm4gamkobix0LGUscil9LHUoKX1mdW5jdGlvbiBI
aShuLHQpe3JldHVybiBWby5yZWJpbmQobix0LCJyYW5nZSIsInJhbmdlUm91bmQi
LCJpbnRlcnBvbGF0ZSIsImNsYW1wIil9ZnVuY3Rpb24gRmkobix0KXtyZXR1cm4g
RGkobixQaShPaShuLHQpWzJdKSl9ZnVuY3Rpb24gT2kobix0KXtudWxsPT10JiYo
dD0xMCk7dmFyIGU9VGkobikscj1lWzFdLWVbMF0sdT1NYXRoLnBvdygxMCxNYXRo
LmZsb29yKE1hdGgubG9nKHIvdCkvTWF0aC5MTjEwKSksaT10L3IqdTtyZXR1cm4u
MTU+PWk/dSo9MTA6LjM1Pj1pP3UqPTU6Ljc1Pj1pJiYodSo9MiksZVswXT1NYXRo
LmNlaWwoZVswXS91KSp1LGVbMV09TWF0aC5mbG9vcihlWzFdL3UpKnUrLjUqdSxl
WzJdPXUsZX1mdW5jdGlvbiBZaShuLHQpe3JldHVybiBWby5yYW5nZS5hcHBseShW
byxPaShuLHQpKX1mdW5jdGlvbiBJaShuLHQsZSl7dmFyIHI9T2kobix0KTtpZihl
KXt2YXIgdT1LYS5leGVjKGUpO2lmKHUuc2hpZnQoKSwicyI9PT11WzhdKXt2YXIg
aT1Wby5mb3JtYXRQcmVmaXgoTWF0aC5tYXgoaWEoclswXSksaWEoclsxXSkpKTty
ZXR1cm4gdVs3XXx8KHVbN109Ii4iK1ppKGkuc2NhbGUoclsyXSkpKSx1WzhdPSJm
IixlPVZvLmZvcm1hdCh1LmpvaW4oIiIpKSxmdW5jdGlvbihuKXtyZXR1cm4gZShp
LnNjYWxlKG4pKStpLnN5bWJvbH19dVs3XXx8KHVbN109Ii4iK1ZpKHVbOF0scikp
LGU9dS5qb2luKCIiKX1lbHNlIGU9IiwuIitaaShyWzJdKSsiZiI7cmV0dXJuIFZv
LmZvcm1hdChlKX1mdW5jdGlvbiBaaShuKXtyZXR1cm4tTWF0aC5mbG9vcihNYXRo
LmxvZyhuKS9NYXRoLkxOMTArLjAxKX1mdW5jdGlvbiBWaShuLHQpe3ZhciBlPVpp
KHRbMl0pO3JldHVybiBuIGluIGZsP01hdGguYWJzKGUtWmkoTWF0aC5tYXgoaWEo
dFswXSksaWEodFsxXSkpKSkrICsoImUiIT09bik6ZS0yKigiJSI9PT1uKX1mdW5j
dGlvbiBYaShuLHQsZSxyKXtmdW5jdGlvbiB1KG4pe3JldHVybihlP01hdGgubG9n
KDA+bj8wOm4pOi1NYXRoLmxvZyhuPjA/MDotbikpL01hdGgubG9nKHQpfWZ1bmN0
aW9uIGkobil7cmV0dXJuIGU/TWF0aC5wb3codCxuKTotTWF0aC5wb3codCwtbil9
ZnVuY3Rpb24gbyh0KXtyZXR1cm4gbih1KHQpKX1yZXR1cm4gby5pbnZlcnQ9ZnVu
Y3Rpb24odCl7cmV0dXJuIGkobi5pbnZlcnQodCkpfSxvLmRvbWFpbj1mdW5jdGlv
bih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT10WzBdPj0wLG4uZG9tYWlu
KChyPXQubWFwKE51bWJlcikpLm1hcCh1KSksbyk6cn0sby5iYXNlPWZ1bmN0aW9u
KGUpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh0PStlLG4uZG9tYWluKHIubWFw
KHUpKSxvKTp0fSxvLm5pY2U9ZnVuY3Rpb24oKXt2YXIgdD1EaShyLm1hcCh1KSxl
P01hdGg6Z2wpO3JldHVybiBuLmRvbWFpbih0KSxyPXQubWFwKGkpLG99LG8udGlj
a3M9ZnVuY3Rpb24oKXt2YXIgbj1UaShyKSxvPVtdLGE9blswXSxjPW5bMV0sbD1N
YXRoLmZsb29yKHUoYSkpLHM9TWF0aC5jZWlsKHUoYykpLGY9dCUxPzI6dDtpZihp
c0Zpbml0ZShzLWwpKXtpZihlKXtmb3IoO3M+bDtsKyspZm9yKHZhciBoPTE7Zj5o
O2grKylvLnB1c2goaShsKSpoKTtvLnB1c2goaShsKSl9ZWxzZSBmb3Ioby5wdXNo
KGkobCkpO2wrKzxzOylmb3IodmFyIGg9Zi0xO2g+MDtoLS0pby5wdXNoKGkobCkq
aCk7Zm9yKGw9MDtvW2xdPGE7bCsrKTtmb3Iocz1vLmxlbmd0aDtvW3MtMV0+Yztz
LS0pO289by5zbGljZShsLHMpfXJldHVybiBvfSxvLnRpY2tGb3JtYXQ9ZnVuY3Rp
b24obix0KXtpZighYXJndW1lbnRzLmxlbmd0aClyZXR1cm4gaGw7YXJndW1lbnRz
Lmxlbmd0aDwyP3Q9aGw6ImZ1bmN0aW9uIiE9dHlwZW9mIHQmJih0PVZvLmZvcm1h
dCh0KSk7dmFyIHIsYT1NYXRoLm1heCguMSxuL28udGlja3MoKS5sZW5ndGgpLGM9
ZT8ocj0xZS0xMixNYXRoLmNlaWwpOihyPS0xZS0xMixNYXRoLmZsb29yKTtyZXR1
cm4gZnVuY3Rpb24obil7cmV0dXJuIG4vaShjKHUobikrcikpPD1hP3Qobik6IiJ9
fSxvLmNvcHk9ZnVuY3Rpb24oKXtyZXR1cm4gWGkobi5jb3B5KCksdCxlLHIpfSxI
aShvLG4pfWZ1bmN0aW9uICRpKG4sdCxlKXtmdW5jdGlvbiByKHQpe3JldHVybiBu
KHUodCkpfXZhciB1PUJpKHQpLGk9QmkoMS90KTtyZXR1cm4gci5pbnZlcnQ9ZnVu
Y3Rpb24odCl7cmV0dXJuIGkobi5pbnZlcnQodCkpfSxyLmRvbWFpbj1mdW5jdGlv
bih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obi5kb21haW4oKGU9dC5tYXAo
TnVtYmVyKSkubWFwKHUpKSxyKTplfSxyLnRpY2tzPWZ1bmN0aW9uKG4pe3JldHVy
biBZaShlLG4pfSxyLnRpY2tGb3JtYXQ9ZnVuY3Rpb24obix0KXtyZXR1cm4gSWko
ZSxuLHQpfSxyLm5pY2U9ZnVuY3Rpb24obil7cmV0dXJuIHIuZG9tYWluKEZpKGUs
bikpfSxyLmV4cG9uZW50PWZ1bmN0aW9uKG8pe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyh1PUJpKHQ9byksaT1CaSgxL3QpLG4uZG9tYWluKGUubWFwKHUpKSxyKTp0
fSxyLmNvcHk9ZnVuY3Rpb24oKXtyZXR1cm4gJGkobi5jb3B5KCksdCxlKX0sSGko
cixuKX1mdW5jdGlvbiBCaShuKXtyZXR1cm4gZnVuY3Rpb24odCl7cmV0dXJuIDA+
dD8tTWF0aC5wb3coLXQsbik6TWF0aC5wb3codCxuKX19ZnVuY3Rpb24gV2kobix0
KXtmdW5jdGlvbiBlKGUpe3JldHVybiBpWygodS5nZXQoZSl8fCgicmFuZ2UiPT09
dC50P3Uuc2V0KGUsbi5wdXNoKGUpKTowLzApKS0xKSVpLmxlbmd0aF19ZnVuY3Rp
b24gcih0LGUpe3JldHVybiBWby5yYW5nZShuLmxlbmd0aCkubWFwKGZ1bmN0aW9u
KG4pe3JldHVybiB0K2Uqbn0pfXZhciB1LGksYTtyZXR1cm4gZS5kb21haW49ZnVu
Y3Rpb24ocil7aWYoIWFyZ3VtZW50cy5sZW5ndGgpcmV0dXJuIG47bj1bXSx1PW5l
dyBvO2Zvcih2YXIgaSxhPS0xLGM9ci5sZW5ndGg7KythPGM7KXUuaGFzKGk9clth
XSl8fHUuc2V0KGksbi5wdXNoKGkpKTtyZXR1cm4gZVt0LnRdLmFwcGx5KGUsdC5h
KX0sZS5yYW5nZT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8o
aT1uLGE9MCx0PXt0OiJyYW5nZSIsYTphcmd1bWVudHN9LGUpOml9LGUucmFuZ2VQ
b2ludHM9ZnVuY3Rpb24odSxvKXthcmd1bWVudHMubGVuZ3RoPDImJihvPTApO3Zh
ciBjPXVbMF0sbD11WzFdLHM9KGwtYykvKE1hdGgubWF4KDEsbi5sZW5ndGgtMSkr
byk7cmV0dXJuIGk9cihuLmxlbmd0aDwyPyhjK2wpLzI6YytzKm8vMixzKSxhPTAs
dD17dDoicmFuZ2VQb2ludHMiLGE6YXJndW1lbnRzfSxlfSxlLnJhbmdlQmFuZHM9
ZnVuY3Rpb24odSxvLGMpe2FyZ3VtZW50cy5sZW5ndGg8MiYmKG89MCksYXJndW1l
bnRzLmxlbmd0aDwzJiYoYz1vKTt2YXIgbD11WzFdPHVbMF0scz11W2wtMF0sZj11
WzEtbF0saD0oZi1zKS8obi5sZW5ndGgtbysyKmMpO3JldHVybiBpPXIocytoKmMs
aCksbCYmaS5yZXZlcnNlKCksYT1oKigxLW8pLHQ9e3Q6InJhbmdlQmFuZHMiLGE6
YXJndW1lbnRzfSxlfSxlLnJhbmdlUm91bmRCYW5kcz1mdW5jdGlvbih1LG8sYyl7
YXJndW1lbnRzLmxlbmd0aDwyJiYobz0wKSxhcmd1bWVudHMubGVuZ3RoPDMmJihj
PW8pO3ZhciBsPXVbMV08dVswXSxzPXVbbC0wXSxmPXVbMS1sXSxoPU1hdGguZmxv
b3IoKGYtcykvKG4ubGVuZ3RoLW8rMipjKSksZz1mLXMtKG4ubGVuZ3RoLW8pKmg7
cmV0dXJuIGk9cihzK01hdGgucm91bmQoZy8yKSxoKSxsJiZpLnJldmVyc2UoKSxh
PU1hdGgucm91bmQoaCooMS1vKSksdD17dDoicmFuZ2VSb3VuZEJhbmRzIixhOmFy
Z3VtZW50c30sZX0sZS5yYW5nZUJhbmQ9ZnVuY3Rpb24oKXtyZXR1cm4gYX0sZS5y
YW5nZUV4dGVudD1mdW5jdGlvbigpe3JldHVybiBUaSh0LmFbMF0pfSxlLmNvcHk9
ZnVuY3Rpb24oKXtyZXR1cm4gV2kobix0KX0sZS5kb21haW4obil9ZnVuY3Rpb24g
SmkoZSxyKXtmdW5jdGlvbiB1KCl7dmFyIG49MCx0PXIubGVuZ3RoO2ZvcihvPVtd
Oysrbjx0OylvW24tMV09Vm8ucXVhbnRpbGUoZSxuL3QpO3JldHVybiBpfWZ1bmN0
aW9uIGkobil7cmV0dXJuIGlzTmFOKG49K24pP3ZvaWQgMDpyW1ZvLmJpc2VjdChv
LG4pXX12YXIgbztyZXR1cm4gaS5kb21haW49ZnVuY3Rpb24ocil7cmV0dXJuIGFy
Z3VtZW50cy5sZW5ndGg/KGU9ci5maWx0ZXIodCkuc29ydChuKSx1KCkpOmV9LGku
cmFuZ2U9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHI9bix1
KCkpOnJ9LGkucXVhbnRpbGVzPWZ1bmN0aW9uKCl7cmV0dXJuIG99LGkuaW52ZXJ0
RXh0ZW50PWZ1bmN0aW9uKG4pe3JldHVybiBuPXIuaW5kZXhPZihuKSwwPm4/WzAv
MCwwLzBdOltuPjA/b1tuLTFdOmVbMF0sbjxvLmxlbmd0aD9vW25dOmVbZS5sZW5n
dGgtMV1dfSxpLmNvcHk9ZnVuY3Rpb24oKXtyZXR1cm4gSmkoZSxyKX0sdSgpfWZ1
bmN0aW9uIEdpKG4sdCxlKXtmdW5jdGlvbiByKHQpe3JldHVybiBlW01hdGgubWF4
KDAsTWF0aC5taW4obyxNYXRoLmZsb29yKGkqKHQtbikpKSldfWZ1bmN0aW9uIHUo
KXtyZXR1cm4gaT1lLmxlbmd0aC8odC1uKSxvPWUubGVuZ3RoLTEscn12YXIgaSxv
O3JldHVybiByLmRvbWFpbj1mdW5jdGlvbihlKXtyZXR1cm4gYXJndW1lbnRzLmxl
bmd0aD8obj0rZVswXSx0PStlW2UubGVuZ3RoLTFdLHUoKSk6W24sdF19LHIucmFu
Z2U9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9bix1KCkp
OmV9LHIuaW52ZXJ0RXh0ZW50PWZ1bmN0aW9uKHQpe3JldHVybiB0PWUuaW5kZXhP
Zih0KSx0PTA+dD8wLzA6dC9pK24sW3QsdCsxL2ldfSxyLmNvcHk9ZnVuY3Rpb24o
KXtyZXR1cm4gR2kobix0LGUpfSx1KCl9ZnVuY3Rpb24gS2kobix0KXtmdW5jdGlv
biBlKGUpe3JldHVybiBlPj1lP3RbVm8uYmlzZWN0KG4sZSldOnZvaWQgMH1yZXR1
cm4gZS5kb21haW49ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/
KG49dCxlKTpufSxlLnJhbmdlPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMu
bGVuZ3RoPyh0PW4sZSk6dH0sZS5pbnZlcnRFeHRlbnQ9ZnVuY3Rpb24oZSl7cmV0
dXJuIGU9dC5pbmRleE9mKGUpLFtuW2UtMV0sbltlXV19LGUuY29weT1mdW5jdGlv
bigpe3JldHVybiBLaShuLHQpfSxlfWZ1bmN0aW9uIFFpKG4pe2Z1bmN0aW9uIHQo
bil7cmV0dXJuK259cmV0dXJuIHQuaW52ZXJ0PXQsdC5kb21haW49dC5yYW5nZT1m
dW5jdGlvbihlKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obj1lLm1hcCh0KSx0
KTpufSx0LnRpY2tzPWZ1bmN0aW9uKHQpe3JldHVybiBZaShuLHQpfSx0LnRpY2tG
b3JtYXQ9ZnVuY3Rpb24odCxlKXtyZXR1cm4gSWkobix0LGUpfSx0LmNvcHk9ZnVu
Y3Rpb24oKXtyZXR1cm4gUWkobil9LHR9ZnVuY3Rpb24gbm8obil7cmV0dXJuIG4u
aW5uZXJSYWRpdXN9ZnVuY3Rpb24gdG8obil7cmV0dXJuIG4ub3V0ZXJSYWRpdXN9
ZnVuY3Rpb24gZW8obil7cmV0dXJuIG4uc3RhcnRBbmdsZX1mdW5jdGlvbiBybyhu
KXtyZXR1cm4gbi5lbmRBbmdsZX1mdW5jdGlvbiB1byhuKXtmdW5jdGlvbiB0KHQp
e2Z1bmN0aW9uIG8oKXtsLnB1c2goIk0iLGkobihzKSxhKSl9Zm9yKHZhciBjLGw9
W10scz1bXSxmPS0xLGg9dC5sZW5ndGgsZz1idChlKSxwPWJ0KHIpOysrZjxoOyl1
LmNhbGwodGhpcyxjPXRbZl0sZik/cy5wdXNoKFsrZy5jYWxsKHRoaXMsYyxmKSwr
cC5jYWxsKHRoaXMsYyxmKV0pOnMubGVuZ3RoJiYobygpLHM9W10pO3JldHVybiBz
Lmxlbmd0aCYmbygpLGwubGVuZ3RoP2wuam9pbigiIik6bnVsbH12YXIgZT1Tcixy
PWtyLHU9U2UsaT1pbyxvPWkua2V5LGE9Ljc7cmV0dXJuIHQueD1mdW5jdGlvbihu
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT1uLHQpOmV9LHQueT1mdW5jdGlv
bihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocj1uLHQpOnJ9LHQuZGVmaW5l
ZD1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odT1uLHQpOnV9
LHQuaW50ZXJwb2xhdGU9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KG89ImZ1bmN0aW9uIj09dHlwZW9mIG4/aT1uOihpPU1sLmdldChuKXx8aW8p
LmtleSx0KTpvfSx0LnRlbnNpb249ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50
cy5sZW5ndGg/KGE9bix0KTphfSx0fWZ1bmN0aW9uIGlvKG4pe3JldHVybiBuLmpv
aW4oIkwiKX1mdW5jdGlvbiBvbyhuKXtyZXR1cm4gaW8obikrIloifWZ1bmN0aW9u
IGFvKG4pe2Zvcih2YXIgdD0wLGU9bi5sZW5ndGgscj1uWzBdLHU9W3JbMF0sIiwi
LHJbMV1dOysrdDxlOyl1LnB1c2goIkgiLChyWzBdKyhyPW5bdF0pWzBdKS8yLCJW
IixyWzFdKTtyZXR1cm4gZT4xJiZ1LnB1c2goIkgiLHJbMF0pLHUuam9pbigiIil9
ZnVuY3Rpb24gY28obil7Zm9yKHZhciB0PTAsZT1uLmxlbmd0aCxyPW5bMF0sdT1b
clswXSwiLCIsclsxXV07Kyt0PGU7KXUucHVzaCgiViIsKHI9blt0XSlbMV0sIkgi
LHJbMF0pO3JldHVybiB1LmpvaW4oIiIpfWZ1bmN0aW9uIGxvKG4pe2Zvcih2YXIg
dD0wLGU9bi5sZW5ndGgscj1uWzBdLHU9W3JbMF0sIiwiLHJbMV1dOysrdDxlOyl1
LnB1c2goIkgiLChyPW5bdF0pWzBdLCJWIixyWzFdKTtyZXR1cm4gdS5qb2luKCIi
KX1mdW5jdGlvbiBzbyhuLHQpe3JldHVybiBuLmxlbmd0aDw0P2lvKG4pOm5bMV0r
Z28obi5zbGljZSgxLG4ubGVuZ3RoLTEpLHBvKG4sdCkpfWZ1bmN0aW9uIGZvKG4s
dCl7cmV0dXJuIG4ubGVuZ3RoPDM/aW8obik6blswXStnbygobi5wdXNoKG5bMF0p
LG4pLHBvKFtuW24ubGVuZ3RoLTJdXS5jb25jYXQobixbblsxXV0pLHQpKX1mdW5j
dGlvbiBobyhuLHQpe3JldHVybiBuLmxlbmd0aDwzP2lvKG4pOm5bMF0rZ28obixw
byhuLHQpKX1mdW5jdGlvbiBnbyhuLHQpe2lmKHQubGVuZ3RoPDF8fG4ubGVuZ3Ro
IT10Lmxlbmd0aCYmbi5sZW5ndGghPXQubGVuZ3RoKzIpcmV0dXJuIGlvKG4pO3Zh
ciBlPW4ubGVuZ3RoIT10Lmxlbmd0aCxyPSIiLHU9blswXSxpPW5bMV0sbz10WzBd
LGE9byxjPTE7aWYoZSYmKHIrPSJRIisoaVswXS0yKm9bMF0vMykrIiwiKyhpWzFd
LTIqb1sxXS8zKSsiLCIraVswXSsiLCIraVsxXSx1PW5bMV0sYz0yKSx0Lmxlbmd0
aD4xKXthPXRbMV0saT1uW2NdLGMrKyxyKz0iQyIrKHVbMF0rb1swXSkrIiwiKyh1
WzFdK29bMV0pKyIsIisoaVswXS1hWzBdKSsiLCIrKGlbMV0tYVsxXSkrIiwiK2lb
MF0rIiwiK2lbMV07Zm9yKHZhciBsPTI7bDx0Lmxlbmd0aDtsKyssYysrKWk9bltj
XSxhPXRbbF0scis9IlMiKyhpWzBdLWFbMF0pKyIsIisoaVsxXS1hWzFdKSsiLCIr
aVswXSsiLCIraVsxXX1pZihlKXt2YXIgcz1uW2NdO3IrPSJRIisoaVswXSsyKmFb
MF0vMykrIiwiKyhpWzFdKzIqYVsxXS8zKSsiLCIrc1swXSsiLCIrc1sxXX1yZXR1
cm4gcn1mdW5jdGlvbiBwbyhuLHQpe2Zvcih2YXIgZSxyPVtdLHU9KDEtdCkvMixp
PW5bMF0sbz1uWzFdLGE9MSxjPW4ubGVuZ3RoOysrYTxjOyllPWksaT1vLG89blth
XSxyLnB1c2goW3UqKG9bMF0tZVswXSksdSoob1sxXS1lWzFdKV0pO3JldHVybiBy
fWZ1bmN0aW9uIHZvKG4pe2lmKG4ubGVuZ3RoPDMpcmV0dXJuIGlvKG4pO3ZhciB0
PTEsZT1uLmxlbmd0aCxyPW5bMF0sdT1yWzBdLGk9clsxXSxvPVt1LHUsdSwocj1u
WzFdKVswXV0sYT1baSxpLGksclsxXV0sYz1bdSwiLCIsaSwiTCIsTW8od2wsbyks
IiwiLE1vKHdsLGEpXTtmb3Iobi5wdXNoKG5bZS0xXSk7Kyt0PD1lOylyPW5bdF0s
by5zaGlmdCgpLG8ucHVzaChyWzBdKSxhLnNoaWZ0KCksYS5wdXNoKHJbMV0pLF9v
KGMsbyxhKTtyZXR1cm4gbi5wb3AoKSxjLnB1c2goIkwiLHIpLGMuam9pbigiIil9
ZnVuY3Rpb24gbW8obil7aWYobi5sZW5ndGg8NClyZXR1cm4gaW8obik7Zm9yKHZh
ciB0LGU9W10scj0tMSx1PW4ubGVuZ3RoLGk9WzBdLG89WzBdOysrcjwzOyl0PW5b
cl0saS5wdXNoKHRbMF0pLG8ucHVzaCh0WzFdKTtmb3IoZS5wdXNoKE1vKHdsLGkp
KyIsIitNbyh3bCxvKSksLS1yOysrcjx1Oyl0PW5bcl0saS5zaGlmdCgpLGkucHVz
aCh0WzBdKSxvLnNoaWZ0KCksby5wdXNoKHRbMV0pLF9vKGUsaSxvKTtyZXR1cm4g
ZS5qb2luKCIiKX1mdW5jdGlvbiB5byhuKXtmb3IodmFyIHQsZSxyPS0xLHU9bi5s
ZW5ndGgsaT11KzQsbz1bXSxhPVtdOysrcjw0OyllPW5bciV1XSxvLnB1c2goZVsw
XSksYS5wdXNoKGVbMV0pO2Zvcih0PVtNbyh3bCxvKSwiLCIsTW8od2wsYSldLC0t
cjsrK3I8aTspZT1uW3IldV0sby5zaGlmdCgpLG8ucHVzaChlWzBdKSxhLnNoaWZ0
KCksYS5wdXNoKGVbMV0pLF9vKHQsbyxhKTtyZXR1cm4gdC5qb2luKCIiKX1mdW5j
dGlvbiB4byhuLHQpe3ZhciBlPW4ubGVuZ3RoLTE7aWYoZSlmb3IodmFyIHIsdSxp
PW5bMF1bMF0sbz1uWzBdWzFdLGE9bltlXVswXS1pLGM9bltlXVsxXS1vLGw9LTE7
KytsPD1lOylyPW5bbF0sdT1sL2UsclswXT10KnJbMF0rKDEtdCkqKGkrdSphKSxy
WzFdPXQqclsxXSsoMS10KSoobyt1KmMpO3JldHVybiB2byhuKX1mdW5jdGlvbiBN
byhuLHQpe3JldHVybiBuWzBdKnRbMF0rblsxXSp0WzFdK25bMl0qdFsyXStuWzNd
KnRbM119ZnVuY3Rpb24gX28obix0LGUpe24ucHVzaCgiQyIsTW8oX2wsdCksIiwi
LE1vKF9sLGUpLCIsIixNbyhibCx0KSwiLCIsTW8oYmwsZSksIiwiLE1vKHdsLHQp
LCIsIixNbyh3bCxlKSl9ZnVuY3Rpb24gYm8obix0KXtyZXR1cm4odFsxXS1uWzFd
KS8odFswXS1uWzBdKX1mdW5jdGlvbiB3byhuKXtmb3IodmFyIHQ9MCxlPW4ubGVu
Z3RoLTEscj1bXSx1PW5bMF0saT1uWzFdLG89clswXT1ibyh1LGkpOysrdDxlOyly
W3RdPShvKyhvPWJvKHU9aSxpPW5bdCsxXSkpKS8yO3JldHVybiByW3RdPW8scn1m
dW5jdGlvbiBTbyhuKXtmb3IodmFyIHQsZSxyLHUsaT1bXSxvPXdvKG4pLGE9LTEs
Yz1uLmxlbmd0aC0xOysrYTxjOyl0PWJvKG5bYV0sblthKzFdKSxpYSh0KTxFYT9v
W2FdPW9bYSsxXT0wOihlPW9bYV0vdCxyPW9bYSsxXS90LHU9ZSplK3Iqcix1Pjkm
Jih1PTMqdC9NYXRoLnNxcnQodSksb1thXT11KmUsb1thKzFdPXUqcikpO2Zvcihh
PS0xOysrYTw9YzspdT0obltNYXRoLm1pbihjLGErMSldWzBdLW5bTWF0aC5tYXgo
MCxhLTEpXVswXSkvKDYqKDErb1thXSpvW2FdKSksaS5wdXNoKFt1fHwwLG9bYV0q
dXx8MF0pO3JldHVybiBpfWZ1bmN0aW9uIGtvKG4pe3JldHVybiBuLmxlbmd0aDwz
P2lvKG4pOm5bMF0rZ28obixTbyhuKSl9ZnVuY3Rpb24gRW8obil7Zm9yKHZhciB0
LGUscix1PS0xLGk9bi5sZW5ndGg7Kyt1PGk7KXQ9blt1XSxlPXRbMF0scj10WzFd
K3lsLHRbMF09ZSpNYXRoLmNvcyhyKSx0WzFdPWUqTWF0aC5zaW4ocik7cmV0dXJu
IG59ZnVuY3Rpb24gQW8obil7ZnVuY3Rpb24gdCh0KXtmdW5jdGlvbiBjKCl7di5w
dXNoKCJNIixhKG4obSksZikscyxsKG4oZC5yZXZlcnNlKCkpLGYpLCJaIil9Zm9y
KHZhciBoLGcscCx2PVtdLGQ9W10sbT1bXSx5PS0xLHg9dC5sZW5ndGgsTT1idChl
KSxfPWJ0KHUpLGI9ZT09PXI/ZnVuY3Rpb24oKXtyZXR1cm4gZ306YnQociksdz11
PT09aT9mdW5jdGlvbigpe3JldHVybiBwfTpidChpKTsrK3k8eDspby5jYWxsKHRo
aXMsaD10W3ldLHkpPyhkLnB1c2goW2c9K00uY2FsbCh0aGlzLGgseSkscD0rXy5j
YWxsKHRoaXMsaCx5KV0pLG0ucHVzaChbK2IuY2FsbCh0aGlzLGgseSksK3cuY2Fs
bCh0aGlzLGgseSldKSk6ZC5sZW5ndGgmJihjKCksZD1bXSxtPVtdKTtyZXR1cm4g
ZC5sZW5ndGgmJmMoKSx2Lmxlbmd0aD92LmpvaW4oIiIpOm51bGx9dmFyIGU9U3Is
cj1Tcix1PTAsaT1rcixvPVNlLGE9aW8sYz1hLmtleSxsPWEscz0iTCIsZj0uNzty
ZXR1cm4gdC54PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhl
PXI9bix0KTpyfSx0LngwPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyhlPW4sdCk6ZX0sdC54MT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRz
Lmxlbmd0aD8ocj1uLHQpOnJ9LHQueT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1l
bnRzLmxlbmd0aD8odT1pPW4sdCk6aX0sdC55MD1mdW5jdGlvbihuKXtyZXR1cm4g
YXJndW1lbnRzLmxlbmd0aD8odT1uLHQpOnV9LHQueTE9ZnVuY3Rpb24obil7cmV0
dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGk9bix0KTppfSx0LmRlZmluZWQ9ZnVuY3Rp
b24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KG89bix0KTpvfSx0LmludGVy
cG9sYXRlPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhjPSJm
dW5jdGlvbiI9PXR5cGVvZiBuP2E9bjooYT1NbC5nZXQobil8fGlvKS5rZXksbD1h
LnJldmVyc2V8fGEscz1hLmNsb3NlZD8iTSI6IkwiLHQpOmN9LHQudGVuc2lvbj1m
dW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZj1uLHQpOmZ9LHR9
ZnVuY3Rpb24gQ28obil7cmV0dXJuIG4ucmFkaXVzfWZ1bmN0aW9uIE5vKG4pe3Jl
dHVybltuLngsbi55XX1mdW5jdGlvbiB6byhuKXtyZXR1cm4gZnVuY3Rpb24oKXt2
YXIgdD1uLmFwcGx5KHRoaXMsYXJndW1lbnRzKSxlPXRbMF0scj10WzFdK3lsO3Jl
dHVybltlKk1hdGguY29zKHIpLGUqTWF0aC5zaW4ocildfX1mdW5jdGlvbiBMbygp
e3JldHVybiA2NH1mdW5jdGlvbiBUbygpe3JldHVybiJjaXJjbGUifWZ1bmN0aW9u
IHFvKG4pe3ZhciB0PU1hdGguc3FydChuL3dhKTtyZXR1cm4iTTAsIit0KyJBIit0
KyIsIit0KyIgMCAxLDEgMCwiKy10KyJBIit0KyIsIit0KyIgMCAxLDEgMCwiK3Qr
IloifWZ1bmN0aW9uIFJvKG4sdCl7cmV0dXJuIHNhKG4sTmwpLG4uaWQ9dCxufWZ1
bmN0aW9uIERvKG4sdCxlLHIpe3ZhciB1PW4uaWQ7cmV0dXJuIFAobiwiZnVuY3Rp
b24iPT10eXBlb2YgZT9mdW5jdGlvbihuLGksbyl7bi5fX3RyYW5zaXRpb25fX1t1
XS50d2Vlbi5zZXQodCxyKGUuY2FsbChuLG4uX19kYXRhX18saSxvKSkpfTooZT1y
KGUpLGZ1bmN0aW9uKG4pe24uX190cmFuc2l0aW9uX19bdV0udHdlZW4uc2V0KHQs
ZSl9KSl9ZnVuY3Rpb24gUG8obil7cmV0dXJuIG51bGw9PW4mJihuPSIiKSxmdW5j
dGlvbigpe3RoaXMudGV4dENvbnRlbnQ9bn19ZnVuY3Rpb24gVW8obix0LGUscil7
dmFyIHU9bi5fX3RyYW5zaXRpb25fX3x8KG4uX190cmFuc2l0aW9uX189e2FjdGl2
ZTowLGNvdW50OjB9KSxpPXVbZV07aWYoIWkpe3ZhciBhPXIudGltZTtpPXVbZV09
e3R3ZWVuOm5ldyBvLHRpbWU6YSxlYXNlOnIuZWFzZSxkZWxheTpyLmRlbGF5LGR1
cmF0aW9uOnIuZHVyYXRpb259LCsrdS5jb3VudCxWby50aW1lcihmdW5jdGlvbihy
KXtmdW5jdGlvbiBvKHIpe3JldHVybiB1LmFjdGl2ZT5lP2woKToodS5hY3RpdmU9
ZSxpLmV2ZW50JiZpLmV2ZW50LnN0YXJ0LmNhbGwobixzLHQpLGkudHdlZW4uZm9y
RWFjaChmdW5jdGlvbihlLHIpeyhyPXIuY2FsbChuLHMsdCkpJiZ2LnB1c2gocil9
KSxWby50aW1lcihmdW5jdGlvbigpe3JldHVybiBwLmM9YyhyfHwxKT9TZTpjLDF9
LDAsYSksdm9pZCAwKX1mdW5jdGlvbiBjKHIpe2lmKHUuYWN0aXZlIT09ZSlyZXR1
cm4gbCgpO2Zvcih2YXIgbz1yL2csYT1mKG8pLGM9di5sZW5ndGg7Yz4wOyl2Wy0t
Y10uY2FsbChuLGEpO3JldHVybiBvPj0xPyhpLmV2ZW50JiZpLmV2ZW50LmVuZC5j
YWxsKG4scyx0KSxsKCkpOnZvaWQgMAp9ZnVuY3Rpb24gbCgpe3JldHVybi0tdS5j
b3VudD9kZWxldGUgdVtlXTpkZWxldGUgbi5fX3RyYW5zaXRpb25fXywxfXZhciBz
PW4uX19kYXRhX18sZj1pLmVhc2UsaD1pLmRlbGF5LGc9aS5kdXJhdGlvbixwPVdh
LHY9W107cmV0dXJuIHAudD1oK2Escj49aD9vKHItaCk6KHAuYz1vLHZvaWQgMCl9
LDAsYSl9fWZ1bmN0aW9uIGpvKG4sdCl7bi5hdHRyKCJ0cmFuc2Zvcm0iLGZ1bmN0
aW9uKG4pe3JldHVybiJ0cmFuc2xhdGUoIit0KG4pKyIsMCkifSl9ZnVuY3Rpb24g
SG8obix0KXtuLmF0dHIoInRyYW5zZm9ybSIsZnVuY3Rpb24obil7cmV0dXJuInRy
YW5zbGF0ZSgwLCIrdChuKSsiKSJ9KX1mdW5jdGlvbiBGbyhuKXtyZXR1cm4gbi50
b0lTT1N0cmluZygpfWZ1bmN0aW9uIE9vKG4sdCxlKXtmdW5jdGlvbiByKHQpe3Jl
dHVybiBuKHQpfWZ1bmN0aW9uIHUobixlKXt2YXIgcj1uWzFdLW5bMF0sdT1yL2Us
aT1Wby5iaXNlY3QoamwsdSk7cmV0dXJuIGk9PWpsLmxlbmd0aD9bdC55ZWFyLE9p
KG4ubWFwKGZ1bmN0aW9uKG4pe3JldHVybiBuLzMxNTM2ZTZ9KSxlKVsyXV06aT90
W3UvamxbaS0xXTxqbFtpXS91P2ktMTppXTpbT2wsT2kobixlKVsyXV19cmV0dXJu
IHIuaW52ZXJ0PWZ1bmN0aW9uKHQpe3JldHVybiBZbyhuLmludmVydCh0KSl9LHIu
ZG9tYWluPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhuLmRv
bWFpbih0KSxyKTpuLmRvbWFpbigpLm1hcChZbyl9LHIubmljZT1mdW5jdGlvbihu
LHQpe2Z1bmN0aW9uIGUoZSl7cmV0dXJuIWlzTmFOKGUpJiYhbi5yYW5nZShlLFlv
KCtlKzEpLHQpLmxlbmd0aH12YXIgaT1yLmRvbWFpbigpLG89VGkoaSksYT1udWxs
PT1uP3UobywxMCk6Im51bWJlciI9PXR5cGVvZiBuJiZ1KG8sbik7cmV0dXJuIGEm
JihuPWFbMF0sdD1hWzFdKSxyLmRvbWFpbihEaShpLHQ+MT97Zmxvb3I6ZnVuY3Rp
b24odCl7Zm9yKDtlKHQ9bi5mbG9vcih0KSk7KXQ9WW8odC0xKTtyZXR1cm4gdH0s
Y2VpbDpmdW5jdGlvbih0KXtmb3IoO2UodD1uLmNlaWwodCkpOyl0PVlvKCt0KzEp
O3JldHVybiB0fX06bikpfSxyLnRpY2tzPWZ1bmN0aW9uKG4sdCl7dmFyIGU9VGko
ci5kb21haW4oKSksaT1udWxsPT1uP3UoZSwxMCk6Im51bWJlciI9PXR5cGVvZiBu
P3UoZSxuKTohbi5yYW5nZSYmW3tyYW5nZTpufSx0XTtyZXR1cm4gaSYmKG49aVsw
XSx0PWlbMV0pLG4ucmFuZ2UoZVswXSxZbygrZVsxXSsxKSwxPnQ/MTp0KX0sci50
aWNrRm9ybWF0PWZ1bmN0aW9uKCl7cmV0dXJuIGV9LHIuY29weT1mdW5jdGlvbigp
e3JldHVybiBPbyhuLmNvcHkoKSx0LGUpfSxIaShyLG4pfWZ1bmN0aW9uIFlvKG4p
e3JldHVybiBuZXcgRGF0ZShuKX1mdW5jdGlvbiBJbyhuKXtyZXR1cm4gSlNPTi5w
YXJzZShuLnJlc3BvbnNlVGV4dCl9ZnVuY3Rpb24gWm8obil7dmFyIHQ9Qm8uY3Jl
YXRlUmFuZ2UoKTtyZXR1cm4gdC5zZWxlY3ROb2RlKEJvLmJvZHkpLHQuY3JlYXRl
Q29udGV4dHVhbEZyYWdtZW50KG4ucmVzcG9uc2VUZXh0KX12YXIgVm89e3ZlcnNp
b246IjMuNC4xMiJ9O0RhdGUubm93fHwoRGF0ZS5ub3c9ZnVuY3Rpb24oKXtyZXR1
cm4rbmV3IERhdGV9KTt2YXIgWG89W10uc2xpY2UsJG89ZnVuY3Rpb24obil7cmV0
dXJuIFhvLmNhbGwobil9LEJvPWRvY3VtZW50LFdvPUJvLmRvY3VtZW50RWxlbWVu
dCxKbz13aW5kb3c7dHJ5eyRvKFdvLmNoaWxkTm9kZXMpWzBdLm5vZGVUeXBlfWNh
dGNoKEdvKXskbz1mdW5jdGlvbihuKXtmb3IodmFyIHQ9bi5sZW5ndGgsZT1uZXcg
QXJyYXkodCk7dC0tOyllW3RdPW5bdF07cmV0dXJuIGV9fXRyeXtCby5jcmVhdGVF
bGVtZW50KCJkaXYiKS5zdHlsZS5zZXRQcm9wZXJ0eSgib3BhY2l0eSIsMCwiIil9
Y2F0Y2goS28pe3ZhciBRbz1Kby5FbGVtZW50LnByb3RvdHlwZSxuYT1Rby5zZXRB
dHRyaWJ1dGUsdGE9UW8uc2V0QXR0cmlidXRlTlMsZWE9Sm8uQ1NTU3R5bGVEZWNs
YXJhdGlvbi5wcm90b3R5cGUscmE9ZWEuc2V0UHJvcGVydHk7UW8uc2V0QXR0cmli
dXRlPWZ1bmN0aW9uKG4sdCl7bmEuY2FsbCh0aGlzLG4sdCsiIil9LFFvLnNldEF0
dHJpYnV0ZU5TPWZ1bmN0aW9uKG4sdCxlKXt0YS5jYWxsKHRoaXMsbix0LGUrIiIp
fSxlYS5zZXRQcm9wZXJ0eT1mdW5jdGlvbihuLHQsZSl7cmEuY2FsbCh0aGlzLG4s
dCsiIixlKX19Vm8uYXNjZW5kaW5nPW4sVm8uZGVzY2VuZGluZz1mdW5jdGlvbihu
LHQpe3JldHVybiBuPnQ/LTE6dD5uPzE6dD49bj8wOjAvMH0sVm8ubWluPWZ1bmN0
aW9uKG4sdCl7dmFyIGUscix1PS0xLGk9bi5sZW5ndGg7aWYoMT09PWFyZ3VtZW50
cy5sZW5ndGgpe2Zvcig7Kyt1PGkmJiEobnVsbCE9KGU9blt1XSkmJmU+PWUpOyll
PXZvaWQgMDtmb3IoOysrdTxpOyludWxsIT0ocj1uW3VdKSYmZT5yJiYoZT1yKX1l
bHNle2Zvcig7Kyt1PGkmJiEobnVsbCE9KGU9dC5jYWxsKG4sblt1XSx1KSkmJmU+
PWUpOyllPXZvaWQgMDtmb3IoOysrdTxpOyludWxsIT0ocj10LmNhbGwobixuW3Vd
LHUpKSYmZT5yJiYoZT1yKX1yZXR1cm4gZX0sVm8ubWF4PWZ1bmN0aW9uKG4sdCl7
dmFyIGUscix1PS0xLGk9bi5sZW5ndGg7aWYoMT09PWFyZ3VtZW50cy5sZW5ndGgp
e2Zvcig7Kyt1PGkmJiEobnVsbCE9KGU9blt1XSkmJmU+PWUpOyllPXZvaWQgMDtm
b3IoOysrdTxpOyludWxsIT0ocj1uW3VdKSYmcj5lJiYoZT1yKX1lbHNle2Zvcig7
Kyt1PGkmJiEobnVsbCE9KGU9dC5jYWxsKG4sblt1XSx1KSkmJmU+PWUpOyllPXZv
aWQgMDtmb3IoOysrdTxpOyludWxsIT0ocj10LmNhbGwobixuW3VdLHUpKSYmcj5l
JiYoZT1yKX1yZXR1cm4gZX0sVm8uZXh0ZW50PWZ1bmN0aW9uKG4sdCl7dmFyIGUs
cix1LGk9LTEsbz1uLmxlbmd0aDtpZigxPT09YXJndW1lbnRzLmxlbmd0aCl7Zm9y
KDsrK2k8byYmIShudWxsIT0oZT11PW5baV0pJiZlPj1lKTspZT11PXZvaWQgMDtm
b3IoOysraTxvOyludWxsIT0ocj1uW2ldKSYmKGU+ciYmKGU9cikscj51JiYodT1y
KSl9ZWxzZXtmb3IoOysraTxvJiYhKG51bGwhPShlPXU9dC5jYWxsKG4sbltpXSxp
KSkmJmU+PWUpOyllPXZvaWQgMDtmb3IoOysraTxvOyludWxsIT0ocj10LmNhbGwo
bixuW2ldLGkpKSYmKGU+ciYmKGU9cikscj51JiYodT1yKSl9cmV0dXJuW2UsdV19
LFZvLnN1bT1mdW5jdGlvbihuLHQpe3ZhciBlLHI9MCx1PW4ubGVuZ3RoLGk9LTE7
aWYoMT09PWFyZ3VtZW50cy5sZW5ndGgpZm9yKDsrK2k8dTspaXNOYU4oZT0rbltp
XSl8fChyKz1lKTtlbHNlIGZvcig7KytpPHU7KWlzTmFOKGU9K3QuY2FsbChuLG5b
aV0saSkpfHwocis9ZSk7cmV0dXJuIHJ9LFZvLm1lYW49ZnVuY3Rpb24obixlKXt2
YXIgcix1PTAsaT1uLmxlbmd0aCxvPS0xLGE9aTtpZigxPT09YXJndW1lbnRzLmxl
bmd0aClmb3IoOysrbzxpOyl0KHI9bltvXSk/dSs9cjotLWE7ZWxzZSBmb3IoOysr
bzxpOyl0KHI9ZS5jYWxsKG4sbltvXSxvKSk/dSs9cjotLWE7cmV0dXJuIGE/dS9h
OnZvaWQgMH0sVm8ucXVhbnRpbGU9ZnVuY3Rpb24obix0KXt2YXIgZT0obi5sZW5n
dGgtMSkqdCsxLHI9TWF0aC5mbG9vcihlKSx1PStuW3ItMV0saT1lLXI7cmV0dXJu
IGk/dStpKihuW3JdLXUpOnV9LFZvLm1lZGlhbj1mdW5jdGlvbihlLHIpe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPjEmJihlPWUubWFwKHIpKSxlPWUuZmlsdGVyKHQp
LGUubGVuZ3RoP1ZvLnF1YW50aWxlKGUuc29ydChuKSwuNSk6dm9pZCAwfTt2YXIg
dWE9ZShuKTtWby5iaXNlY3RMZWZ0PXVhLmxlZnQsVm8uYmlzZWN0PVZvLmJpc2Vj
dFJpZ2h0PXVhLnJpZ2h0LFZvLmJpc2VjdG9yPWZ1bmN0aW9uKHQpe3JldHVybiBl
KDE9PT10Lmxlbmd0aD9mdW5jdGlvbihlLHIpe3JldHVybiBuKHQoZSkscil9OnQp
fSxWby5zaHVmZmxlPWZ1bmN0aW9uKG4pe2Zvcih2YXIgdCxlLHI9bi5sZW5ndGg7
cjspZT0wfE1hdGgucmFuZG9tKCkqci0tLHQ9bltyXSxuW3JdPW5bZV0sbltlXT10
O3JldHVybiBufSxWby5wZXJtdXRlPWZ1bmN0aW9uKG4sdCl7Zm9yKHZhciBlPXQu
bGVuZ3RoLHI9bmV3IEFycmF5KGUpO2UtLTspcltlXT1uW3RbZV1dO3JldHVybiBy
fSxWby5wYWlycz1mdW5jdGlvbihuKXtmb3IodmFyIHQsZT0wLHI9bi5sZW5ndGgt
MSx1PW5bMF0saT1uZXcgQXJyYXkoMD5yPzA6cik7cj5lOylpW2VdPVt0PXUsdT1u
WysrZV1dO3JldHVybiBpfSxWby56aXA9ZnVuY3Rpb24oKXtpZighKHU9YXJndW1l
bnRzLmxlbmd0aCkpcmV0dXJuW107Zm9yKHZhciBuPS0xLHQ9Vm8ubWluKGFyZ3Vt
ZW50cyxyKSxlPW5ldyBBcnJheSh0KTsrK248dDspZm9yKHZhciB1LGk9LTEsbz1l
W25dPW5ldyBBcnJheSh1KTsrK2k8dTspb1tpXT1hcmd1bWVudHNbaV1bbl07cmV0
dXJuIGV9LFZvLnRyYW5zcG9zZT1mdW5jdGlvbihuKXtyZXR1cm4gVm8uemlwLmFw
cGx5KFZvLG4pfSxWby5rZXlzPWZ1bmN0aW9uKG4pe3ZhciB0PVtdO2Zvcih2YXIg
ZSBpbiBuKXQucHVzaChlKTtyZXR1cm4gdH0sVm8udmFsdWVzPWZ1bmN0aW9uKG4p
e3ZhciB0PVtdO2Zvcih2YXIgZSBpbiBuKXQucHVzaChuW2VdKTtyZXR1cm4gdH0s
Vm8uZW50cmllcz1mdW5jdGlvbihuKXt2YXIgdD1bXTtmb3IodmFyIGUgaW4gbil0
LnB1c2goe2tleTplLHZhbHVlOm5bZV19KTtyZXR1cm4gdH0sVm8ubWVyZ2U9ZnVu
Y3Rpb24obil7Zm9yKHZhciB0LGUscix1PW4ubGVuZ3RoLGk9LTEsbz0wOysraTx1
OylvKz1uW2ldLmxlbmd0aDtmb3IoZT1uZXcgQXJyYXkobyk7LS11Pj0wOylmb3Io
cj1uW3VdLHQ9ci5sZW5ndGg7LS10Pj0wOyllWy0tb109clt0XTtyZXR1cm4gZX07
dmFyIGlhPU1hdGguYWJzO1ZvLnJhbmdlPWZ1bmN0aW9uKG4sdCxlKXtpZihhcmd1
bWVudHMubGVuZ3RoPDMmJihlPTEsYXJndW1lbnRzLmxlbmd0aDwyJiYodD1uLG49
MCkpLDEvMD09PSh0LW4pL2UpdGhyb3cgbmV3IEVycm9yKCJpbmZpbml0ZSByYW5n
ZSIpO3ZhciByLGk9W10sbz11KGlhKGUpKSxhPS0xO2lmKG4qPW8sdCo9byxlKj1v
LDA+ZSlmb3IoOyhyPW4rZSorK2EpPnQ7KWkucHVzaChyL28pO2Vsc2UgZm9yKDso
cj1uK2UqKythKTx0OylpLnB1c2goci9vKTtyZXR1cm4gaX0sVm8ubWFwPWZ1bmN0
aW9uKG4pe3ZhciB0PW5ldyBvO2lmKG4gaW5zdGFuY2VvZiBvKW4uZm9yRWFjaChm
dW5jdGlvbihuLGUpe3Quc2V0KG4sZSl9KTtlbHNlIGZvcih2YXIgZSBpbiBuKXQu
c2V0KGUsbltlXSk7cmV0dXJuIHR9LGkobyx7aGFzOmEsZ2V0OmZ1bmN0aW9uKG4p
e3JldHVybiB0aGlzW29hK25dfSxzZXQ6ZnVuY3Rpb24obix0KXtyZXR1cm4gdGhp
c1tvYStuXT10fSxyZW1vdmU6YyxrZXlzOmwsdmFsdWVzOmZ1bmN0aW9uKCl7dmFy
IG49W107cmV0dXJuIHRoaXMuZm9yRWFjaChmdW5jdGlvbih0LGUpe24ucHVzaChl
KX0pLG59LGVudHJpZXM6ZnVuY3Rpb24oKXt2YXIgbj1bXTtyZXR1cm4gdGhpcy5m
b3JFYWNoKGZ1bmN0aW9uKHQsZSl7bi5wdXNoKHtrZXk6dCx2YWx1ZTplfSl9KSxu
fSxzaXplOnMsZW1wdHk6Zixmb3JFYWNoOmZ1bmN0aW9uKG4pe2Zvcih2YXIgdCBp
biB0aGlzKXQuY2hhckNvZGVBdCgwKT09PWFhJiZuLmNhbGwodGhpcyx0LnNsaWNl
KDEpLHRoaXNbdF0pfX0pO3ZhciBvYT0iXHgwMCIsYWE9b2EuY2hhckNvZGVBdCgw
KTtWby5uZXN0PWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbih0LGEsYyl7aWYoYz49aS5s
ZW5ndGgpcmV0dXJuIHI/ci5jYWxsKHUsYSk6ZT9hLnNvcnQoZSk6YTtmb3IodmFy
IGwscyxmLGgsZz0tMSxwPWEubGVuZ3RoLHY9aVtjKytdLGQ9bmV3IG87KytnPHA7
KShoPWQuZ2V0KGw9dihzPWFbZ10pKSk/aC5wdXNoKHMpOmQuc2V0KGwsW3NdKTty
ZXR1cm4gdD8ocz10KCksZj1mdW5jdGlvbihlLHIpe3Muc2V0KGUsbih0LHIsYykp
fSk6KHM9e30sZj1mdW5jdGlvbihlLHIpe3NbZV09bih0LHIsYyl9KSxkLmZvckVh
Y2goZiksc31mdW5jdGlvbiB0KG4sZSl7aWYoZT49aS5sZW5ndGgpcmV0dXJuIG47
dmFyIHI9W10sdT1hW2UrK107cmV0dXJuIG4uZm9yRWFjaChmdW5jdGlvbihuLHUp
e3IucHVzaCh7a2V5Om4sdmFsdWVzOnQodSxlKX0pfSksdT9yLnNvcnQoZnVuY3Rp
b24obix0KXtyZXR1cm4gdShuLmtleSx0LmtleSl9KTpyfXZhciBlLHIsdT17fSxp
PVtdLGE9W107cmV0dXJuIHUubWFwPWZ1bmN0aW9uKHQsZSl7cmV0dXJuIG4oZSx0
LDApfSx1LmVudHJpZXM9ZnVuY3Rpb24oZSl7cmV0dXJuIHQobihWby5tYXAsZSww
KSwwKX0sdS5rZXk9ZnVuY3Rpb24obil7cmV0dXJuIGkucHVzaChuKSx1fSx1LnNv
cnRLZXlzPWZ1bmN0aW9uKG4pe3JldHVybiBhW2kubGVuZ3RoLTFdPW4sdX0sdS5z
b3J0VmFsdWVzPWZ1bmN0aW9uKG4pe3JldHVybiBlPW4sdX0sdS5yb2xsdXA9ZnVu
Y3Rpb24obil7cmV0dXJuIHI9bix1fSx1fSxWby5zZXQ9ZnVuY3Rpb24obil7dmFy
IHQ9bmV3IGg7aWYobilmb3IodmFyIGU9MCxyPW4ubGVuZ3RoO3I+ZTsrK2UpdC5h
ZGQobltlXSk7cmV0dXJuIHR9LGkoaCx7aGFzOmEsYWRkOmZ1bmN0aW9uKG4pe3Jl
dHVybiB0aGlzW29hK25dPSEwLG59LHJlbW92ZTpmdW5jdGlvbihuKXtyZXR1cm4g
bj1vYStuLG4gaW4gdGhpcyYmZGVsZXRlIHRoaXNbbl19LHZhbHVlczpsLHNpemU6
cyxlbXB0eTpmLGZvckVhY2g6ZnVuY3Rpb24obil7Zm9yKHZhciB0IGluIHRoaXMp
dC5jaGFyQ29kZUF0KDApPT09YWEmJm4uY2FsbCh0aGlzLHQuc2xpY2UoMSkpfX0p
LFZvLmJlaGF2aW9yPXt9LFZvLnJlYmluZD1mdW5jdGlvbihuLHQpe2Zvcih2YXIg
ZSxyPTEsdT1hcmd1bWVudHMubGVuZ3RoOysrcjx1OyluW2U9YXJndW1lbnRzW3Jd
XT1nKG4sdCx0W2VdKTtyZXR1cm4gbn07dmFyIGNhPVsid2Via2l0IiwibXMiLCJt
b3oiLCJNb3oiLCJvIiwiTyJdO1ZvLmRpc3BhdGNoPWZ1bmN0aW9uKCl7Zm9yKHZh
ciBuPW5ldyBkLHQ9LTEsZT1hcmd1bWVudHMubGVuZ3RoOysrdDxlOyluW2FyZ3Vt
ZW50c1t0XV09bShuKTtyZXR1cm4gbn0sZC5wcm90b3R5cGUub249ZnVuY3Rpb24o
bix0KXt2YXIgZT1uLmluZGV4T2YoIi4iKSxyPSIiO2lmKGU+PTAmJihyPW4uc2xp
Y2UoZSsxKSxuPW4uc2xpY2UoMCxlKSksbilyZXR1cm4gYXJndW1lbnRzLmxlbmd0
aDwyP3RoaXNbbl0ub24ocik6dGhpc1tuXS5vbihyLHQpO2lmKDI9PT1hcmd1bWVu
dHMubGVuZ3RoKXtpZihudWxsPT10KWZvcihuIGluIHRoaXMpdGhpcy5oYXNPd25Q
cm9wZXJ0eShuKSYmdGhpc1tuXS5vbihyLG51bGwpO3JldHVybiB0aGlzfX0sVm8u
ZXZlbnQ9bnVsbCxWby5yZXF1b3RlPWZ1bmN0aW9uKG4pe3JldHVybiBuLnJlcGxh
Y2UobGEsIlxcJCYiKX07dmFyIGxhPS9bXFxcXlwkXCpcK1w/XHxcW1xdXChcKVwu
XHtcfV0vZyxzYT17fS5fX3Byb3RvX18/ZnVuY3Rpb24obix0KXtuLl9fcHJvdG9f
Xz10fTpmdW5jdGlvbihuLHQpe2Zvcih2YXIgZSBpbiB0KW5bZV09dFtlXX0sZmE9
ZnVuY3Rpb24obix0KXtyZXR1cm4gdC5xdWVyeVNlbGVjdG9yKG4pfSxoYT1mdW5j
dGlvbihuLHQpe3JldHVybiB0LnF1ZXJ5U2VsZWN0b3JBbGwobil9LGdhPVdvLm1h
dGNoZXN8fFdvW3AoV28sIm1hdGNoZXNTZWxlY3RvciIpXSxwYT1mdW5jdGlvbihu
LHQpe3JldHVybiBnYS5jYWxsKG4sdCl9OyJmdW5jdGlvbiI9PXR5cGVvZiBTaXp6
bGUmJihmYT1mdW5jdGlvbihuLHQpe3JldHVybiBTaXp6bGUobix0KVswXXx8bnVs
bH0saGE9U2l6emxlLHBhPVNpenpsZS5tYXRjaGVzU2VsZWN0b3IpLFZvLnNlbGVj
dGlvbj1mdW5jdGlvbigpe3JldHVybiB5YX07dmFyIHZhPVZvLnNlbGVjdGlvbi5w
cm90b3R5cGU9W107dmEuc2VsZWN0PWZ1bmN0aW9uKG4pe3ZhciB0LGUscix1LGk9
W107bj1iKG4pO2Zvcih2YXIgbz0tMSxhPXRoaXMubGVuZ3RoOysrbzxhOyl7aS5w
dXNoKHQ9W10pLHQucGFyZW50Tm9kZT0ocj10aGlzW29dKS5wYXJlbnROb2RlO2Zv
cih2YXIgYz0tMSxsPXIubGVuZ3RoOysrYzxsOykodT1yW2NdKT8odC5wdXNoKGU9
bi5jYWxsKHUsdS5fX2RhdGFfXyxjLG8pKSxlJiYiX19kYXRhX18iaW4gdSYmKGUu
X19kYXRhX189dS5fX2RhdGFfXykpOnQucHVzaChudWxsKX1yZXR1cm4gXyhpKX0s
dmEuc2VsZWN0QWxsPWZ1bmN0aW9uKG4pe3ZhciB0LGUscj1bXTtuPXcobik7Zm9y
KHZhciB1PS0xLGk9dGhpcy5sZW5ndGg7Kyt1PGk7KWZvcih2YXIgbz10aGlzW3Vd
LGE9LTEsYz1vLmxlbmd0aDsrK2E8YzspKGU9b1thXSkmJihyLnB1c2godD0kbyhu
LmNhbGwoZSxlLl9fZGF0YV9fLGEsdSkpKSx0LnBhcmVudE5vZGU9ZSk7cmV0dXJu
IF8ocil9O3ZhciBkYT17c3ZnOiJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIs
eGh0bWw6Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGh0bWwiLHhsaW5rOiJodHRw
Oi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIix4bWw6Imh0dHA6Ly93d3cudzMub3Jn
L1hNTC8xOTk4L25hbWVzcGFjZSIseG1sbnM6Imh0dHA6Ly93d3cudzMub3JnLzIw
MDAveG1sbnMvIn07Vm8ubnM9e3ByZWZpeDpkYSxxdWFsaWZ5OmZ1bmN0aW9uKG4p
e3ZhciB0PW4uaW5kZXhPZigiOiIpLGU9bjtyZXR1cm4gdD49MCYmKGU9bi5zbGlj
ZSgwLHQpLG49bi5zbGljZSh0KzEpKSxkYS5oYXNPd25Qcm9wZXJ0eShlKT97c3Bh
Y2U6ZGFbZV0sbG9jYWw6bn06bn19LHZhLmF0dHI9ZnVuY3Rpb24obix0KXtpZihh
cmd1bWVudHMubGVuZ3RoPDIpe2lmKCJzdHJpbmciPT10eXBlb2Ygbil7dmFyIGU9
dGhpcy5ub2RlKCk7cmV0dXJuIG49Vm8ubnMucXVhbGlmeShuKSxuLmxvY2FsP2Uu
Z2V0QXR0cmlidXRlTlMobi5zcGFjZSxuLmxvY2FsKTplLmdldEF0dHJpYnV0ZShu
KX1mb3IodCBpbiBuKXRoaXMuZWFjaChTKHQsblt0XSkpO3JldHVybiB0aGlzfXJl
dHVybiB0aGlzLmVhY2goUyhuLHQpKX0sdmEuY2xhc3NlZD1mdW5jdGlvbihuLHQp
e2lmKGFyZ3VtZW50cy5sZW5ndGg8Mil7aWYoInN0cmluZyI9PXR5cGVvZiBuKXt2
YXIgZT10aGlzLm5vZGUoKSxyPShuPUEobikpLmxlbmd0aCx1PS0xO2lmKHQ9ZS5j
bGFzc0xpc3Qpe2Zvcig7Kyt1PHI7KWlmKCF0LmNvbnRhaW5zKG5bdV0pKXJldHVy
biExfWVsc2UgZm9yKHQ9ZS5nZXRBdHRyaWJ1dGUoImNsYXNzIik7Kyt1PHI7KWlm
KCFFKG5bdV0pLnRlc3QodCkpcmV0dXJuITE7cmV0dXJuITB9Zm9yKHQgaW4gbil0
aGlzLmVhY2goQyh0LG5bdF0pKTtyZXR1cm4gdGhpc31yZXR1cm4gdGhpcy5lYWNo
KEMobix0KSl9LHZhLnN0eWxlPWZ1bmN0aW9uKG4sdCxlKXt2YXIgcj1hcmd1bWVu
dHMubGVuZ3RoO2lmKDM+cil7aWYoInN0cmluZyIhPXR5cGVvZiBuKXsyPnImJih0
PSIiKTtmb3IoZSBpbiBuKXRoaXMuZWFjaCh6KGUsbltlXSx0KSk7cmV0dXJuIHRo
aXN9aWYoMj5yKXJldHVybiBKby5nZXRDb21wdXRlZFN0eWxlKHRoaXMubm9kZSgp
LG51bGwpLmdldFByb3BlcnR5VmFsdWUobik7ZT0iIn1yZXR1cm4gdGhpcy5lYWNo
KHoobix0LGUpKX0sdmEucHJvcGVydHk9ZnVuY3Rpb24obix0KXtpZihhcmd1bWVu
dHMubGVuZ3RoPDIpe2lmKCJzdHJpbmciPT10eXBlb2YgbilyZXR1cm4gdGhpcy5u
b2RlKClbbl07Zm9yKHQgaW4gbil0aGlzLmVhY2goTCh0LG5bdF0pKTtyZXR1cm4g
dGhpc31yZXR1cm4gdGhpcy5lYWNoKEwobix0KSl9LHZhLnRleHQ9ZnVuY3Rpb24o
bil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/dGhpcy5lYWNoKCJmdW5jdGlvbiI9
PXR5cGVvZiBuP2Z1bmN0aW9uKCl7dmFyIHQ9bi5hcHBseSh0aGlzLGFyZ3VtZW50
cyk7dGhpcy50ZXh0Q29udGVudD1udWxsPT10PyIiOnR9Om51bGw9PW4/ZnVuY3Rp
b24oKXt0aGlzLnRleHRDb250ZW50PSIifTpmdW5jdGlvbigpe3RoaXMudGV4dENv
bnRlbnQ9bn0pOnRoaXMubm9kZSgpLnRleHRDb250ZW50fSx2YS5odG1sPWZ1bmN0
aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoP3RoaXMuZWFjaCgiZnVuY3Rp
b24iPT10eXBlb2Ygbj9mdW5jdGlvbigpe3ZhciB0PW4uYXBwbHkodGhpcyxhcmd1
bWVudHMpO3RoaXMuaW5uZXJIVE1MPW51bGw9PXQ/IiI6dH06bnVsbD09bj9mdW5j
dGlvbigpe3RoaXMuaW5uZXJIVE1MPSIifTpmdW5jdGlvbigpe3RoaXMuaW5uZXJI
VE1MPW59KTp0aGlzLm5vZGUoKS5pbm5lckhUTUx9LHZhLmFwcGVuZD1mdW5jdGlv
bihuKXtyZXR1cm4gbj1UKG4pLHRoaXMuc2VsZWN0KGZ1bmN0aW9uKCl7cmV0dXJu
IHRoaXMuYXBwZW5kQ2hpbGQobi5hcHBseSh0aGlzLGFyZ3VtZW50cykpfSl9LHZh
Lmluc2VydD1mdW5jdGlvbihuLHQpe3JldHVybiBuPVQobiksdD1iKHQpLHRoaXMu
c2VsZWN0KGZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuaW5zZXJ0QmVmb3JlKG4uYXBw
bHkodGhpcyxhcmd1bWVudHMpLHQuYXBwbHkodGhpcyxhcmd1bWVudHMpfHxudWxs
KX0pfSx2YS5yZW1vdmU9ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5lYWNoKGZ1bmN0
aW9uKCl7dmFyIG49dGhpcy5wYXJlbnROb2RlO24mJm4ucmVtb3ZlQ2hpbGQodGhp
cyl9KX0sdmEuZGF0YT1mdW5jdGlvbihuLHQpe2Z1bmN0aW9uIGUobixlKXt2YXIg
cix1LGksYT1uLmxlbmd0aCxmPWUubGVuZ3RoLGg9TWF0aC5taW4oYSxmKSxnPW5l
dyBBcnJheShmKSxwPW5ldyBBcnJheShmKSx2PW5ldyBBcnJheShhKTtpZih0KXt2
YXIgZCxtPW5ldyBvLHk9bmV3IG8seD1bXTtmb3Iocj0tMTsrK3I8YTspZD10LmNh
bGwodT1uW3JdLHUuX19kYXRhX18sciksbS5oYXMoZCk/dltyXT11Om0uc2V0KGQs
dSkseC5wdXNoKGQpO2ZvcihyPS0xOysrcjxmOylkPXQuY2FsbChlLGk9ZVtyXSxy
KSwodT1tLmdldChkKSk/KGdbcl09dSx1Ll9fZGF0YV9fPWkpOnkuaGFzKGQpfHwo
cFtyXT1xKGkpKSx5LnNldChkLGkpLG0ucmVtb3ZlKGQpO2ZvcihyPS0xOysrcjxh
OyltLmhhcyh4W3JdKSYmKHZbcl09bltyXSl9ZWxzZXtmb3Iocj0tMTsrK3I8aDsp
dT1uW3JdLGk9ZVtyXSx1Pyh1Ll9fZGF0YV9fPWksZ1tyXT11KTpwW3JdPXEoaSk7
Zm9yKDtmPnI7KytyKXBbcl09cShlW3JdKTtmb3IoO2E+cjsrK3IpdltyXT1uW3Jd
fXAudXBkYXRlPWcscC5wYXJlbnROb2RlPWcucGFyZW50Tm9kZT12LnBhcmVudE5v
ZGU9bi5wYXJlbnROb2RlLGMucHVzaChwKSxsLnB1c2goZykscy5wdXNoKHYpfXZh
ciByLHUsaT0tMSxhPXRoaXMubGVuZ3RoO2lmKCFhcmd1bWVudHMubGVuZ3RoKXtm
b3Iobj1uZXcgQXJyYXkoYT0ocj10aGlzWzBdKS5sZW5ndGgpOysraTxhOykodT1y
W2ldKSYmKG5baV09dS5fX2RhdGFfXyk7cmV0dXJuIG59dmFyIGM9VShbXSksbD1f
KFtdKSxzPV8oW10pO2lmKCJmdW5jdGlvbiI9PXR5cGVvZiBuKWZvcig7KytpPGE7
KWUocj10aGlzW2ldLG4uY2FsbChyLHIucGFyZW50Tm9kZS5fX2RhdGFfXyxpKSk7
ZWxzZSBmb3IoOysraTxhOyllKHI9dGhpc1tpXSxuKTtyZXR1cm4gbC5lbnRlcj1m
dW5jdGlvbigpe3JldHVybiBjfSxsLmV4aXQ9ZnVuY3Rpb24oKXtyZXR1cm4gc30s
bH0sdmEuZGF0dW09ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/
dGhpcy5wcm9wZXJ0eSgiX19kYXRhX18iLG4pOnRoaXMucHJvcGVydHkoIl9fZGF0
YV9fIil9LHZhLmZpbHRlcj1mdW5jdGlvbihuKXt2YXIgdCxlLHIsdT1bXTsiZnVu
Y3Rpb24iIT10eXBlb2YgbiYmKG49UihuKSk7Zm9yKHZhciBpPTAsbz10aGlzLmxl
bmd0aDtvPmk7aSsrKXt1LnB1c2godD1bXSksdC5wYXJlbnROb2RlPShlPXRoaXNb
aV0pLnBhcmVudE5vZGU7Zm9yKHZhciBhPTAsYz1lLmxlbmd0aDtjPmE7YSsrKShy
PWVbYV0pJiZuLmNhbGwocixyLl9fZGF0YV9fLGEsaSkmJnQucHVzaChyKX1yZXR1
cm4gXyh1KX0sdmEub3JkZXI9ZnVuY3Rpb24oKXtmb3IodmFyIG49LTEsdD10aGlz
Lmxlbmd0aDsrK248dDspZm9yKHZhciBlLHI9dGhpc1tuXSx1PXIubGVuZ3RoLTEs
aT1yW3VdOy0tdT49MDspKGU9clt1XSkmJihpJiZpIT09ZS5uZXh0U2libGluZyYm
aS5wYXJlbnROb2RlLmluc2VydEJlZm9yZShlLGkpLGk9ZSk7cmV0dXJuIHRoaXN9
LHZhLnNvcnQ9ZnVuY3Rpb24obil7bj1ELmFwcGx5KHRoaXMsYXJndW1lbnRzKTtm
b3IodmFyIHQ9LTEsZT10aGlzLmxlbmd0aDsrK3Q8ZTspdGhpc1t0XS5zb3J0KG4p
O3JldHVybiB0aGlzLm9yZGVyKCl9LHZhLmVhY2g9ZnVuY3Rpb24obil7cmV0dXJu
IFAodGhpcyxmdW5jdGlvbih0LGUscil7bi5jYWxsKHQsdC5fX2RhdGFfXyxlLHIp
fSl9LHZhLmNhbGw9ZnVuY3Rpb24obil7dmFyIHQ9JG8oYXJndW1lbnRzKTtyZXR1
cm4gbi5hcHBseSh0WzBdPXRoaXMsdCksdGhpc30sdmEuZW1wdHk9ZnVuY3Rpb24o
KXtyZXR1cm4hdGhpcy5ub2RlKCl9LHZhLm5vZGU9ZnVuY3Rpb24oKXtmb3IodmFy
IG49MCx0PXRoaXMubGVuZ3RoO3Q+bjtuKyspZm9yKHZhciBlPXRoaXNbbl0scj0w
LHU9ZS5sZW5ndGg7dT5yO3IrKyl7dmFyIGk9ZVtyXTtpZihpKXJldHVybiBpfXJl
dHVybiBudWxsfSx2YS5zaXplPWZ1bmN0aW9uKCl7dmFyIG49MDtyZXR1cm4gUCh0
aGlzLGZ1bmN0aW9uKCl7KytufSksbn07dmFyIG1hPVtdO1ZvLnNlbGVjdGlvbi5l
bnRlcj1VLFZvLnNlbGVjdGlvbi5lbnRlci5wcm90b3R5cGU9bWEsbWEuYXBwZW5k
PXZhLmFwcGVuZCxtYS5lbXB0eT12YS5lbXB0eSxtYS5ub2RlPXZhLm5vZGUsbWEu
Y2FsbD12YS5jYWxsLG1hLnNpemU9dmEuc2l6ZSxtYS5zZWxlY3Q9ZnVuY3Rpb24o
bil7Zm9yKHZhciB0LGUscix1LGksbz1bXSxhPS0xLGM9dGhpcy5sZW5ndGg7Kyth
PGM7KXtyPSh1PXRoaXNbYV0pLnVwZGF0ZSxvLnB1c2godD1bXSksdC5wYXJlbnRO
b2RlPXUucGFyZW50Tm9kZTtmb3IodmFyIGw9LTEscz11Lmxlbmd0aDsrK2w8czsp
KGk9dVtsXSk/KHQucHVzaChyW2xdPWU9bi5jYWxsKHUucGFyZW50Tm9kZSxpLl9f
ZGF0YV9fLGwsYSkpLGUuX19kYXRhX189aS5fX2RhdGFfXyk6dC5wdXNoKG51bGwp
fXJldHVybiBfKG8pfSxtYS5pbnNlcnQ9ZnVuY3Rpb24obix0KXtyZXR1cm4gYXJn
dW1lbnRzLmxlbmd0aDwyJiYodD1qKHRoaXMpKSx2YS5pbnNlcnQuY2FsbCh0aGlz
LG4sdCl9LHZhLnRyYW5zaXRpb249ZnVuY3Rpb24oKXtmb3IodmFyIG4sdCxlPWts
fHwrK3psLHI9W10sdT1FbHx8e3RpbWU6RGF0ZS5ub3coKSxlYXNlOk11LGRlbGF5
OjAsZHVyYXRpb246MjUwfSxpPS0xLG89dGhpcy5sZW5ndGg7KytpPG87KXtyLnB1
c2gobj1bXSk7Zm9yKHZhciBhPXRoaXNbaV0sYz0tMSxsPWEubGVuZ3RoOysrYzxs
OykodD1hW2NdKSYmVW8odCxjLGUsdSksbi5wdXNoKHQpfXJldHVybiBSbyhyLGUp
fSx2YS5pbnRlcnJ1cHQ9ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5lYWNoKEgpfSxW
by5zZWxlY3Q9ZnVuY3Rpb24obil7dmFyIHQ9WyJzdHJpbmciPT10eXBlb2Ygbj9m
YShuLEJvKTpuXTtyZXR1cm4gdC5wYXJlbnROb2RlPVdvLF8oW3RdKX0sVm8uc2Vs
ZWN0QWxsPWZ1bmN0aW9uKG4pe3ZhciB0PSRvKCJzdHJpbmciPT10eXBlb2Ygbj9o
YShuLEJvKTpuKTtyZXR1cm4gdC5wYXJlbnROb2RlPVdvLF8oW3RdKX07dmFyIHlh
PVZvLnNlbGVjdChXbyk7dmEub249ZnVuY3Rpb24obix0LGUpe3ZhciByPWFyZ3Vt
ZW50cy5sZW5ndGg7aWYoMz5yKXtpZigic3RyaW5nIiE9dHlwZW9mIG4pezI+ciYm
KHQ9ITEpO2ZvcihlIGluIG4pdGhpcy5lYWNoKEYoZSxuW2VdLHQpKTtyZXR1cm4g
dGhpc31pZigyPnIpcmV0dXJuKHI9dGhpcy5ub2RlKClbIl9fb24iK25dKSYmci5f
O2U9ITF9cmV0dXJuIHRoaXMuZWFjaChGKG4sdCxlKSl9O3ZhciB4YT1Wby5tYXAo
e21vdXNlZW50ZXI6Im1vdXNlb3ZlciIsbW91c2VsZWF2ZToibW91c2VvdXQifSk7
eGEuZm9yRWFjaChmdW5jdGlvbihuKXsib24iK24gaW4gQm8mJnhhLnJlbW92ZShu
KX0pO3ZhciBNYT0ib25zZWxlY3RzdGFydCJpbiBCbz9udWxsOnAoV28uc3R5bGUs
InVzZXJTZWxlY3QiKSxfYT0wO1ZvLm1vdXNlPWZ1bmN0aW9uKG4pe3JldHVybiBa
KG4seCgpKX07dmFyIGJhPS9XZWJLaXQvLnRlc3QoSm8ubmF2aWdhdG9yLnVzZXJB
Z2VudCk/LTE6MDtWby50b3VjaD1mdW5jdGlvbihuLHQsZSl7aWYoYXJndW1lbnRz
Lmxlbmd0aDwzJiYoZT10LHQ9eCgpLmNoYW5nZWRUb3VjaGVzKSx0KWZvcih2YXIg
cix1PTAsaT10Lmxlbmd0aDtpPnU7Kyt1KWlmKChyPXRbdV0pLmlkZW50aWZpZXI9
PT1lKXJldHVybiBaKG4scil9LFZvLmJlaGF2aW9yLmRyYWc9ZnVuY3Rpb24oKXtm
dW5jdGlvbiBuKCl7dGhpcy5vbigibW91c2Vkb3duLmRyYWciLHUpLm9uKCJ0b3Vj
aHN0YXJ0LmRyYWciLGkpfWZ1bmN0aW9uIHQobix0LHUsaSxvKXtyZXR1cm4gZnVu
Y3Rpb24oKXtmdW5jdGlvbiBhKCl7dmFyIG4sZSxyPXQoaCx2KTtyJiYobj1yWzBd
LXhbMF0sZT1yWzFdLXhbMV0scHw9bnxlLHg9cixnKHt0eXBlOiJkcmFnIix4OnJb
MF0rbFswXSx5OnJbMV0rbFsxXSxkeDpuLGR5OmV9KSl9ZnVuY3Rpb24gYygpe3Qo
aCx2KSYmKG0ub24oaStkLG51bGwpLm9uKG8rZCxudWxsKSx5KHAmJlZvLmV2ZW50
LnRhcmdldD09PWYpLGcoe3R5cGU6ImRyYWdlbmQifSkpfXZhciBsLHM9dGhpcyxm
PVZvLmV2ZW50LnRhcmdldCxoPXMucGFyZW50Tm9kZSxnPWUub2Yocyxhcmd1bWVu
dHMpLHA9MCx2PW4oKSxkPSIuZHJhZyIrKG51bGw9PXY/IiI6Ii0iK3YpLG09Vm8u
c2VsZWN0KHUoKSkub24oaStkLGEpLm9uKG8rZCxjKSx5PUkoKSx4PXQoaCx2KTty
PyhsPXIuYXBwbHkocyxhcmd1bWVudHMpLGw9W2wueC14WzBdLGwueS14WzFdXSk6
bD1bMCwwXSxnKHt0eXBlOiJkcmFnc3RhcnQifSl9fXZhciBlPU0obiwiZHJhZyIs
ImRyYWdzdGFydCIsImRyYWdlbmQiKSxyPW51bGwsdT10KHYsVm8ubW91c2UsJCwi
bW91c2Vtb3ZlIiwibW91c2V1cCIpLGk9dChWLFZvLnRvdWNoLFgsInRvdWNobW92
ZSIsInRvdWNoZW5kIik7cmV0dXJuIG4ub3JpZ2luPWZ1bmN0aW9uKHQpe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyhyPXQsbik6cn0sVm8ucmViaW5kKG4sZSwib24i
KX0sVm8udG91Y2hlcz1mdW5jdGlvbihuLHQpe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPDImJih0PXgoKS50b3VjaGVzKSx0PyRvKHQpLm1hcChmdW5jdGlvbih0KXt2
YXIgZT1aKG4sdCk7cmV0dXJuIGUuaWRlbnRpZmllcj10LmlkZW50aWZpZXIsZX0p
OltdfTt2YXIgd2E9TWF0aC5QSSxTYT0yKndhLGthPXdhLzIsRWE9MWUtNixBYT1F
YSpFYSxDYT13YS8xODAsTmE9MTgwL3dhLHphPU1hdGguU1FSVDIsTGE9MixUYT00
O1ZvLmludGVycG9sYXRlWm9vbT1mdW5jdGlvbihuLHQpe2Z1bmN0aW9uIGUobil7
dmFyIHQ9bip5O2lmKG0pe3ZhciBlPVEodiksbz1pLyhMYSpoKSooZSpudCh6YSp0
K3YpLUsodikpO3JldHVybltyK28qbCx1K28qcyxpKmUvUSh6YSp0K3YpXX1yZXR1
cm5bcituKmwsdStuKnMsaSpNYXRoLmV4cCh6YSp0KV19dmFyIHI9blswXSx1PW5b
MV0saT1uWzJdLG89dFswXSxhPXRbMV0sYz10WzJdLGw9by1yLHM9YS11LGY9bCps
K3MqcyxoPU1hdGguc3FydChmKSxnPShjKmMtaSppK1RhKmYpLygyKmkqTGEqaCks
cD0oYypjLWkqaS1UYSpmKS8oMipjKkxhKmgpLHY9TWF0aC5sb2coTWF0aC5zcXJ0
KGcqZysxKS1nKSxkPU1hdGgubG9nKE1hdGguc3FydChwKnArMSktcCksbT1kLXYs
eT0obXx8TWF0aC5sb2coYy9pKSkvemE7cmV0dXJuIGUuZHVyYXRpb249MWUzKnks
ZX0sVm8uYmVoYXZpb3Iuem9vbT1mdW5jdGlvbigpe2Z1bmN0aW9uIG4obil7bi5v
bihBLGwpLm9uKERhKyIuem9vbSIsZikub24oImRibGNsaWNrLnpvb20iLGgpLm9u
KHoscyl9ZnVuY3Rpb24gdChuKXtyZXR1cm5bKG5bMF0tUy54KS9TLmssKG5bMV0t
Uy55KS9TLmtdfWZ1bmN0aW9uIGUobil7cmV0dXJuW25bMF0qUy5rK1MueCxuWzFd
KlMuaytTLnldfWZ1bmN0aW9uIHIobil7Uy5rPU1hdGgubWF4KEVbMF0sTWF0aC5t
aW4oRVsxXSxuKSl9ZnVuY3Rpb24gdShuLHQpe3Q9ZSh0KSxTLngrPW5bMF0tdFsw
XSxTLnkrPW5bMV0tdFsxXX1mdW5jdGlvbiBpKCl7XyYmXy5kb21haW4oeC5yYW5n
ZSgpLm1hcChmdW5jdGlvbihuKXtyZXR1cm4obi1TLngpL1Mua30pLm1hcCh4Lmlu
dmVydCkpLHcmJncuZG9tYWluKGIucmFuZ2UoKS5tYXAoZnVuY3Rpb24obil7cmV0
dXJuKG4tUy55KS9TLmt9KS5tYXAoYi5pbnZlcnQpKX1mdW5jdGlvbiBvKG4pe24o
e3R5cGU6Inpvb21zdGFydCJ9KX1mdW5jdGlvbiBhKG4pe2koKSxuKHt0eXBlOiJ6
b29tIixzY2FsZTpTLmssdHJhbnNsYXRlOltTLngsUy55XX0pfWZ1bmN0aW9uIGMo
bil7bih7dHlwZToiem9vbWVuZCJ9KX1mdW5jdGlvbiBsKCl7ZnVuY3Rpb24gbigp
e3M9MSx1KFZvLm1vdXNlKHIpLGgpLGEobCl9ZnVuY3Rpb24gZSgpe2Yub24oQyxu
dWxsKS5vbihOLG51bGwpLGcocyYmVm8uZXZlbnQudGFyZ2V0PT09aSksYyhsKX12
YXIgcj10aGlzLGk9Vm8uZXZlbnQudGFyZ2V0LGw9TC5vZihyLGFyZ3VtZW50cyks
cz0wLGY9Vm8uc2VsZWN0KEpvKS5vbihDLG4pLm9uKE4sZSksaD10KFZvLm1vdXNl
KHIpKSxnPUkoKTtILmNhbGwociksbyhsKX1mdW5jdGlvbiBzKCl7ZnVuY3Rpb24g
bigpe3ZhciBuPVZvLnRvdWNoZXMoZyk7cmV0dXJuIGg9Uy5rLG4uZm9yRWFjaChm
dW5jdGlvbihuKXtuLmlkZW50aWZpZXIgaW4gdiYmKHZbbi5pZGVudGlmaWVyXT10
KG4pKX0pLG59ZnVuY3Rpb24gZSgpe3ZhciB0PVZvLmV2ZW50LnRhcmdldDtWby5z
ZWxlY3QodCkub24oTSxpKS5vbihfLGYpLGIucHVzaCh0KTtmb3IodmFyIGU9Vm8u
ZXZlbnQuY2hhbmdlZFRvdWNoZXMsbz0wLGM9ZS5sZW5ndGg7Yz5vOysrbyl2W2Vb
b10uaWRlbnRpZmllcl09bnVsbDt2YXIgbD1uKCkscz1EYXRlLm5vdygpO2lmKDE9
PT1sLmxlbmd0aCl7aWYoNTAwPnMtbSl7dmFyIGg9bFswXSxnPXZbaC5pZGVudGlm
aWVyXTtyKDIqUy5rKSx1KGgsZykseSgpLGEocCl9bT1zfWVsc2UgaWYobC5sZW5n
dGg+MSl7dmFyIGg9bFswXSx4PWxbMV0sdz1oWzBdLXhbMF0saz1oWzFdLXhbMV07
ZD13KncrayprfX1mdW5jdGlvbiBpKCl7Zm9yKHZhciBuLHQsZSxpLG89Vm8udG91
Y2hlcyhnKSxjPTAsbD1vLmxlbmd0aDtsPmM7KytjLGk9bnVsbClpZihlPW9bY10s
aT12W2UuaWRlbnRpZmllcl0pe2lmKHQpYnJlYWs7bj1lLHQ9aX1pZihpKXt2YXIg
cz0ocz1lWzBdLW5bMF0pKnMrKHM9ZVsxXS1uWzFdKSpzLGY9ZCYmTWF0aC5zcXJ0
KHMvZCk7bj1bKG5bMF0rZVswXSkvMiwoblsxXStlWzFdKS8yXSx0PVsodFswXStp
WzBdKS8yLCh0WzFdK2lbMV0pLzJdLHIoZipoKX1tPW51bGwsdShuLHQpLGEocCl9
ZnVuY3Rpb24gZigpe2lmKFZvLmV2ZW50LnRvdWNoZXMubGVuZ3RoKXtmb3IodmFy
IHQ9Vm8uZXZlbnQuY2hhbmdlZFRvdWNoZXMsZT0wLHI9dC5sZW5ndGg7cj5lOysr
ZSlkZWxldGUgdlt0W2VdLmlkZW50aWZpZXJdO2Zvcih2YXIgdSBpbiB2KXJldHVy
biB2b2lkIG4oKX1Wby5zZWxlY3RBbGwoYikub24oeCxudWxsKSx3Lm9uKEEsbCku
b24oeixzKSxrKCksYyhwKX12YXIgaCxnPXRoaXMscD1MLm9mKGcsYXJndW1lbnRz
KSx2PXt9LGQ9MCx4PSIuem9vbS0iK1ZvLmV2ZW50LmNoYW5nZWRUb3VjaGVzWzBd
LmlkZW50aWZpZXIsTT0idG91Y2htb3ZlIit4LF89InRvdWNoZW5kIit4LGI9W10s
dz1Wby5zZWxlY3QoZyksaz1JKCk7SC5jYWxsKGcpLGUoKSxvKHApLHcub24oQSxu
dWxsKS5vbih6LGUpfWZ1bmN0aW9uIGYoKXt2YXIgbj1MLm9mKHRoaXMsYXJndW1l
bnRzKTtkP2NsZWFyVGltZW91dChkKTooZz10KHA9dnx8Vm8ubW91c2UodGhpcykp
LEguY2FsbCh0aGlzKSxvKG4pKSxkPXNldFRpbWVvdXQoZnVuY3Rpb24oKXtkPW51
bGwsYyhuKX0sNTApLHkoKSxyKE1hdGgucG93KDIsLjAwMipxYSgpKSpTLmspLHUo
cCxnKSxhKG4pfWZ1bmN0aW9uIGgoKXt2YXIgbj1MLm9mKHRoaXMsYXJndW1lbnRz
KSxlPVZvLm1vdXNlKHRoaXMpLGk9dChlKSxsPU1hdGgubG9nKFMuaykvTWF0aC5M
TjI7byhuKSxyKE1hdGgucG93KDIsVm8uZXZlbnQuc2hpZnRLZXk/TWF0aC5jZWls
KGwpLTE6TWF0aC5mbG9vcihsKSsxKSksdShlLGkpLGEobiksYyhuKX12YXIgZyxw
LHYsZCxtLHgsXyxiLHcsUz17eDowLHk6MCxrOjF9LGs9Wzk2MCw1MDBdLEU9UmEs
QT0ibW91c2Vkb3duLnpvb20iLEM9Im1vdXNlbW92ZS56b29tIixOPSJtb3VzZXVw
Lnpvb20iLHo9InRvdWNoc3RhcnQuem9vbSIsTD1NKG4sInpvb21zdGFydCIsInpv
b20iLCJ6b29tZW5kIik7cmV0dXJuIG4uZXZlbnQ9ZnVuY3Rpb24obil7bi5lYWNo
KGZ1bmN0aW9uKCl7dmFyIG49TC5vZih0aGlzLGFyZ3VtZW50cyksdD1TO2tsP1Zv
LnNlbGVjdCh0aGlzKS50cmFuc2l0aW9uKCkuZWFjaCgic3RhcnQuem9vbSIsZnVu
Y3Rpb24oKXtTPXRoaXMuX19jaGFydF9ffHx7eDowLHk6MCxrOjF9LG8obil9KS50
d2Vlbigiem9vbTp6b29tIixmdW5jdGlvbigpe3ZhciBlPWtbMF0scj1rWzFdLHU9
ZS8yLGk9ci8yLG89Vm8uaW50ZXJwb2xhdGVab29tKFsodS1TLngpL1MuaywoaS1T
LnkpL1MuayxlL1Mua10sWyh1LXQueCkvdC5rLChpLXQueSkvdC5rLGUvdC5rXSk7
cmV0dXJuIGZ1bmN0aW9uKHQpe3ZhciByPW8odCksYz1lL3JbMl07dGhpcy5fX2No
YXJ0X189Uz17eDp1LXJbMF0qYyx5OmktclsxXSpjLGs6Y30sYShuKX19KS5lYWNo
KCJlbmQuem9vbSIsZnVuY3Rpb24oKXtjKG4pfSk6KHRoaXMuX19jaGFydF9fPVMs
byhuKSxhKG4pLGMobikpfSl9LG4udHJhbnNsYXRlPWZ1bmN0aW9uKHQpe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyhTPXt4Oit0WzBdLHk6K3RbMV0sazpTLmt9LGko
KSxuKTpbUy54LFMueV19LG4uc2NhbGU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KFM9e3g6Uy54LHk6Uy55LGs6K3R9LGkoKSxuKTpTLmt9LG4u
c2NhbGVFeHRlbnQ9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/
KEU9bnVsbD09dD9SYTpbK3RbMF0sK3RbMV1dLG4pOkV9LG4uY2VudGVyPWZ1bmN0
aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh2PXQmJlsrdFswXSwrdFsx
XV0sbik6dn0sbi5zaXplPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyhrPXQmJlsrdFswXSwrdFsxXV0sbik6a30sbi54PWZ1bmN0aW9uKHQpe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyhfPXQseD10LmNvcHkoKSxTPXt4OjAseTow
LGs6MX0sbik6X30sbi55PWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyh3PXQsYj10LmNvcHkoKSxTPXt4OjAseTowLGs6MX0sbik6d30sVm8ucmVi
aW5kKG4sTCwib24iKX07dmFyIHFhLFJhPVswLDEvMF0sRGE9Im9ud2hlZWwiaW4g
Qm8/KHFhPWZ1bmN0aW9uKCl7cmV0dXJuLVZvLmV2ZW50LmRlbHRhWSooVm8uZXZl
bnQuZGVsdGFNb2RlPzEyMDoxKX0sIndoZWVsIik6Im9ubW91c2V3aGVlbCJpbiBC
bz8ocWE9ZnVuY3Rpb24oKXtyZXR1cm4gVm8uZXZlbnQud2hlZWxEZWx0YX0sIm1v
dXNld2hlZWwiKToocWE9ZnVuY3Rpb24oKXtyZXR1cm4tVm8uZXZlbnQuZGV0YWls
fSwiTW96TW91c2VQaXhlbFNjcm9sbCIpO1ZvLmNvbG9yPWV0LGV0LnByb3RvdHlw
ZS50b1N0cmluZz1mdW5jdGlvbigpe3JldHVybiB0aGlzLnJnYigpKyIifSxWby5o
c2w9cnQ7dmFyIFBhPXJ0LnByb3RvdHlwZT1uZXcgZXQ7UGEuYnJpZ2h0ZXI9ZnVu
Y3Rpb24obil7cmV0dXJuIG49TWF0aC5wb3coLjcsYXJndW1lbnRzLmxlbmd0aD9u
OjEpLG5ldyBydCh0aGlzLmgsdGhpcy5zLHRoaXMubC9uKX0sUGEuZGFya2VyPWZ1
bmN0aW9uKG4pe3JldHVybiBuPU1hdGgucG93KC43LGFyZ3VtZW50cy5sZW5ndGg/
bjoxKSxuZXcgcnQodGhpcy5oLHRoaXMucyxuKnRoaXMubCl9LFBhLnJnYj1mdW5j
dGlvbigpe3JldHVybiB1dCh0aGlzLmgsdGhpcy5zLHRoaXMubCl9LFZvLmhjbD1p
dDt2YXIgVWE9aXQucHJvdG90eXBlPW5ldyBldDtVYS5icmlnaHRlcj1mdW5jdGlv
bihuKXtyZXR1cm4gbmV3IGl0KHRoaXMuaCx0aGlzLmMsTWF0aC5taW4oMTAwLHRo
aXMubCtqYSooYXJndW1lbnRzLmxlbmd0aD9uOjEpKSl9LFVhLmRhcmtlcj1mdW5j
dGlvbihuKXtyZXR1cm4gbmV3IGl0KHRoaXMuaCx0aGlzLmMsTWF0aC5tYXgoMCx0
aGlzLmwtamEqKGFyZ3VtZW50cy5sZW5ndGg/bjoxKSkpfSxVYS5yZ2I9ZnVuY3Rp
b24oKXtyZXR1cm4gb3QodGhpcy5oLHRoaXMuYyx0aGlzLmwpLnJnYigpfSxWby5s
YWI9YXQ7dmFyIGphPTE4LEhhPS45NTA0NyxGYT0xLE9hPTEuMDg4ODMsWWE9YXQu
cHJvdG90eXBlPW5ldyBldDtZYS5icmlnaHRlcj1mdW5jdGlvbihuKXtyZXR1cm4g
bmV3IGF0KE1hdGgubWluKDEwMCx0aGlzLmwramEqKGFyZ3VtZW50cy5sZW5ndGg/
bjoxKSksdGhpcy5hLHRoaXMuYil9LFlhLmRhcmtlcj1mdW5jdGlvbihuKXtyZXR1
cm4gbmV3IGF0KE1hdGgubWF4KDAsdGhpcy5sLWphKihhcmd1bWVudHMubGVuZ3Ro
P246MSkpLHRoaXMuYSx0aGlzLmIpfSxZYS5yZ2I9ZnVuY3Rpb24oKXtyZXR1cm4g
Y3QodGhpcy5sLHRoaXMuYSx0aGlzLmIpfSxWby5yZ2I9Z3Q7dmFyIElhPWd0LnBy
b3RvdHlwZT1uZXcgZXQ7SWEuYnJpZ2h0ZXI9ZnVuY3Rpb24obil7bj1NYXRoLnBv
dyguNyxhcmd1bWVudHMubGVuZ3RoP246MSk7dmFyIHQ9dGhpcy5yLGU9dGhpcy5n
LHI9dGhpcy5iLHU9MzA7cmV0dXJuIHR8fGV8fHI/KHQmJnU+dCYmKHQ9dSksZSYm
dT5lJiYoZT11KSxyJiZ1PnImJihyPXUpLG5ldyBndChNYXRoLm1pbigyNTUsdC9u
KSxNYXRoLm1pbigyNTUsZS9uKSxNYXRoLm1pbigyNTUsci9uKSkpOm5ldyBndCh1
LHUsdSl9LElhLmRhcmtlcj1mdW5jdGlvbihuKXtyZXR1cm4gbj1NYXRoLnBvdygu
Nyxhcmd1bWVudHMubGVuZ3RoP246MSksbmV3IGd0KG4qdGhpcy5yLG4qdGhpcy5n
LG4qdGhpcy5iKX0sSWEuaHNsPWZ1bmN0aW9uKCl7cmV0dXJuIHl0KHRoaXMucix0
aGlzLmcsdGhpcy5iKX0sSWEudG9TdHJpbmc9ZnVuY3Rpb24oKXtyZXR1cm4iIyIr
ZHQodGhpcy5yKStkdCh0aGlzLmcpK2R0KHRoaXMuYil9O3ZhciBaYT1Wby5tYXAo
e2FsaWNlYmx1ZToxNTc5MjM4MyxhbnRpcXVld2hpdGU6MTY0NDQzNzUsYXF1YTo2
NTUzNSxhcXVhbWFyaW5lOjgzODg1NjQsYXp1cmU6MTU3OTQxNzUsYmVpZ2U6MTYx
MTkyNjAsYmlzcXVlOjE2NzcwMjQ0LGJsYWNrOjAsYmxhbmNoZWRhbG1vbmQ6MTY3
NzIwNDUsYmx1ZToyNTUsYmx1ZXZpb2xldDo5MDU1MjAyLGJyb3duOjEwODI0MjM0
LGJ1cmx5d29vZDoxNDU5NjIzMSxjYWRldGJsdWU6NjI2NjUyOCxjaGFydHJldXNl
OjgzODgzNTIsY2hvY29sYXRlOjEzNzg5NDcwLGNvcmFsOjE2NzQ0MjcyLGNvcm5m
bG93ZXJibHVlOjY1OTE5ODEsY29ybnNpbGs6MTY3NzUzODgsY3JpbXNvbjoxNDQy
MzEwMCxjeWFuOjY1NTM1LGRhcmtibHVlOjEzOSxkYXJrY3lhbjozNTcyMyxkYXJr
Z29sZGVucm9kOjEyMDkyOTM5LGRhcmtncmF5OjExMTE5MDE3LGRhcmtncmVlbjoy
NTYwMCxkYXJrZ3JleToxMTExOTAxNyxkYXJra2hha2k6MTI0MzMyNTksZGFya21h
Z2VudGE6OTEwOTY0MyxkYXJrb2xpdmVncmVlbjo1NTk3OTk5LGRhcmtvcmFuZ2U6
MTY3NDc1MjAsZGFya29yY2hpZDoxMDA0MDAxMixkYXJrcmVkOjkxMDk1MDQsZGFy
a3NhbG1vbjoxNTMwODQxMCxkYXJrc2VhZ3JlZW46OTQxOTkxOSxkYXJrc2xhdGVi
bHVlOjQ3MzQzNDcsZGFya3NsYXRlZ3JheTozMTAwNDk1LGRhcmtzbGF0ZWdyZXk6
MzEwMDQ5NSxkYXJrdHVycXVvaXNlOjUyOTQ1LGRhcmt2aW9sZXQ6OTY5OTUzOSxk
ZWVwcGluazoxNjcxNjk0NyxkZWVwc2t5Ymx1ZTo0OTE1MSxkaW1ncmF5OjY5MDgy
NjUsZGltZ3JleTo2OTA4MjY1LGRvZGdlcmJsdWU6MjAwMzE5OSxmaXJlYnJpY2s6
MTE2NzQxNDYsZmxvcmFsd2hpdGU6MTY3NzU5MjAsZm9yZXN0Z3JlZW46MjI2Mzg0
MixmdWNoc2lhOjE2NzExOTM1LGdhaW5zYm9ybzoxNDQ3NDQ2MCxnaG9zdHdoaXRl
OjE2MzE2NjcxLGdvbGQ6MTY3NjY3MjAsZ29sZGVucm9kOjE0MzI5MTIwLGdyYXk6
ODQyMTUwNCxncmVlbjozMjc2OCxncmVlbnllbGxvdzoxMTQwMzA1NSxncmV5Ojg0
MjE1MDQsaG9uZXlkZXc6MTU3OTQxNjAsaG90cGluazoxNjczODc0MCxpbmRpYW5y
ZWQ6MTM0NTg1MjQsaW5kaWdvOjQ5MTUzMzAsaXZvcnk6MTY3NzcyMDAsa2hha2k6
MTU3ODc2NjAsbGF2ZW5kZXI6MTUxMzI0MTAsbGF2ZW5kZXJibHVzaDoxNjc3MzM2
NSxsYXduZ3JlZW46ODE5MDk3NixsZW1vbmNoaWZmb246MTY3NzU4ODUsbGlnaHRi
bHVlOjExMzkzMjU0LGxpZ2h0Y29yYWw6MTU3NjE1MzYsbGlnaHRjeWFuOjE0NzQ1
NTk5LGxpZ2h0Z29sZGVucm9keWVsbG93OjE2NDQ4MjEwLGxpZ2h0Z3JheToxMzg4
MjMyMyxsaWdodGdyZWVuOjk0OTgyNTYsbGlnaHRncmV5OjEzODgyMzIzLGxpZ2h0
cGluazoxNjc1ODQ2NSxsaWdodHNhbG1vbjoxNjc1Mjc2MixsaWdodHNlYWdyZWVu
OjIxNDI4OTAsbGlnaHRza3libHVlOjg5MDAzNDYsbGlnaHRzbGF0ZWdyYXk6Nzgz
Mzc1MyxsaWdodHNsYXRlZ3JleTo3ODMzNzUzLGxpZ2h0c3RlZWxibHVlOjExNTg0
NzM0LGxpZ2h0eWVsbG93OjE2Nzc3MTg0LGxpbWU6NjUyODAsbGltZWdyZWVuOjMz
MjkzMzAsbGluZW46MTY0NDU2NzAsbWFnZW50YToxNjcxMTkzNSxtYXJvb246ODM4
ODYwOCxtZWRpdW1hcXVhbWFyaW5lOjY3MzczMjIsbWVkaXVtYmx1ZToyMDUsbWVk
aXVtb3JjaGlkOjEyMjExNjY3LG1lZGl1bXB1cnBsZTo5NjYyNjgzLG1lZGl1bXNl
YWdyZWVuOjM5NzgwOTcsbWVkaXVtc2xhdGVibHVlOjgwODc3OTAsbWVkaXVtc3By
aW5nZ3JlZW46NjQxNTQsbWVkaXVtdHVycXVvaXNlOjQ3NzIzMDAsbWVkaXVtdmlv
bGV0cmVkOjEzMDQ3MTczLG1pZG5pZ2h0Ymx1ZToxNjQ0OTEyLG1pbnRjcmVhbTox
NjEyMTg1MCxtaXN0eXJvc2U6MTY3NzAyNzMsbW9jY2FzaW46MTY3NzAyMjksbmF2
YWpvd2hpdGU6MTY3Njg2ODUsbmF2eToxMjgsb2xkbGFjZToxNjY0MzU1OCxvbGl2
ZTo4NDIxMzc2LG9saXZlZHJhYjo3MDQ4NzM5LG9yYW5nZToxNjc1MzkyMCxvcmFu
Z2VyZWQ6MTY3MjkzNDQsb3JjaGlkOjE0MzE1NzM0LHBhbGVnb2xkZW5yb2Q6MTU2
NTcxMzAscGFsZWdyZWVuOjEwMDI1ODgwLHBhbGV0dXJxdW9pc2U6MTE1Mjk5NjYs
cGFsZXZpb2xldHJlZDoxNDM4MTIwMyxwYXBheWF3aGlwOjE2NzczMDc3LHBlYWNo
cHVmZjoxNjc2NzY3MyxwZXJ1OjEzNDY4OTkxLHBpbms6MTY3NjEwMzUscGx1bTox
NDUyNDYzNyxwb3dkZXJibHVlOjExNTkxOTEwLHB1cnBsZTo4Mzg4NzM2LHJlZDox
NjcxMTY4MCxyb3N5YnJvd246MTIzNTc1MTkscm95YWxibHVlOjQyODY5NDUsc2Fk
ZGxlYnJvd246OTEyNzE4NyxzYWxtb246MTY0MTY4ODIsc2FuZHlicm93bjoxNjAz
Mjg2NCxzZWFncmVlbjozMDUwMzI3LHNlYXNoZWxsOjE2Nzc0NjM4LHNpZW5uYTox
MDUwNjc5NyxzaWx2ZXI6MTI2MzIyNTYsc2t5Ymx1ZTo4OTAwMzMxLHNsYXRlYmx1
ZTo2OTcwMDYxLHNsYXRlZ3JheTo3MzcyOTQ0LHNsYXRlZ3JleTo3MzcyOTQ0LHNu
b3c6MTY3NzU5MzAsc3ByaW5nZ3JlZW46NjU0MDcsc3RlZWxibHVlOjQ2MjA5ODAs
dGFuOjEzODA4NzgwLHRlYWw6MzI4OTYsdGhpc3RsZToxNDIwNDg4OCx0b21hdG86
MTY3MzcwOTUsdHVycXVvaXNlOjQyNTE4NTYsdmlvbGV0OjE1NjMxMDg2LHdoZWF0
OjE2MTEzMzMxLHdoaXRlOjE2Nzc3MjE1LHdoaXRlc21va2U6MTYxMTkyODUseWVs
bG93OjE2Nzc2OTYwLHllbGxvd2dyZWVuOjEwMTQ1MDc0fSk7WmEuZm9yRWFjaChm
dW5jdGlvbihuLHQpe1phLnNldChuLHB0KHQpKX0pLFZvLmZ1bmN0b3I9YnQsVm8u
eGhyPVN0KHd0KSxWby5kc3Y9ZnVuY3Rpb24obix0KXtmdW5jdGlvbiBlKG4sZSxp
KXthcmd1bWVudHMubGVuZ3RoPDMmJihpPWUsZT1udWxsKTt2YXIgbz1rdChuLHQs
bnVsbD09ZT9yOnUoZSksaSk7cmV0dXJuIG8ucm93PWZ1bmN0aW9uKG4pe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoP28ucmVzcG9uc2UobnVsbD09KGU9bik/cjp1KG4p
KTplfSxvfWZ1bmN0aW9uIHIobil7cmV0dXJuIGUucGFyc2Uobi5yZXNwb25zZVRl
eHQpfWZ1bmN0aW9uIHUobil7cmV0dXJuIGZ1bmN0aW9uKHQpe3JldHVybiBlLnBh
cnNlKHQucmVzcG9uc2VUZXh0LG4pfX1mdW5jdGlvbiBpKHQpe3JldHVybiB0Lm1h
cChvKS5qb2luKG4pfWZ1bmN0aW9uIG8obil7cmV0dXJuIGEudGVzdChuKT8nIicr
bi5yZXBsYWNlKC9cIi9nLCciIicpKyciJzpufXZhciBhPW5ldyBSZWdFeHAoJ1si
JytuKyJcbl0iKSxjPW4uY2hhckNvZGVBdCgwKTtyZXR1cm4gZS5wYXJzZT1mdW5j
dGlvbihuLHQpe3ZhciByO3JldHVybiBlLnBhcnNlUm93cyhuLGZ1bmN0aW9uKG4s
ZSl7aWYocilyZXR1cm4gcihuLGUtMSk7dmFyIHU9bmV3IEZ1bmN0aW9uKCJkIiwi
cmV0dXJuIHsiK24ubWFwKGZ1bmN0aW9uKG4sdCl7cmV0dXJuIEpTT04uc3RyaW5n
aWZ5KG4pKyI6IGRbIit0KyJdIn0pLmpvaW4oIiwiKSsifSIpO3I9dD9mdW5jdGlv
bihuLGUpe3JldHVybiB0KHUobiksZSl9OnV9KX0sZS5wYXJzZVJvd3M9ZnVuY3Rp
b24obix0KXtmdW5jdGlvbiBlKCl7aWYocz49bClyZXR1cm4gbztpZih1KXJldHVy
biB1PSExLGk7dmFyIHQ9cztpZigzND09PW4uY2hhckNvZGVBdCh0KSl7Zm9yKHZh
ciBlPXQ7ZSsrPGw7KWlmKDM0PT09bi5jaGFyQ29kZUF0KGUpKXtpZigzNCE9PW4u
Y2hhckNvZGVBdChlKzEpKWJyZWFrOysrZX1zPWUrMjt2YXIgcj1uLmNoYXJDb2Rl
QXQoZSsxKTtyZXR1cm4gMTM9PT1yPyh1PSEwLDEwPT09bi5jaGFyQ29kZUF0KGUr
MikmJisrcyk6MTA9PT1yJiYodT0hMCksbi5zbGljZSh0KzEsZSkucmVwbGFjZSgv
IiIvZywnIicpfWZvcig7bD5zOyl7dmFyIHI9bi5jaGFyQ29kZUF0KHMrKyksYT0x
O2lmKDEwPT09cil1PSEwO2Vsc2UgaWYoMTM9PT1yKXU9ITAsMTA9PT1uLmNoYXJD
b2RlQXQocykmJigrK3MsKythKTtlbHNlIGlmKHIhPT1jKWNvbnRpbnVlO3JldHVy
biBuLnNsaWNlKHQscy1hKX1yZXR1cm4gbi5zbGljZSh0KX1mb3IodmFyIHIsdSxp
PXt9LG89e30sYT1bXSxsPW4ubGVuZ3RoLHM9MCxmPTA7KHI9ZSgpKSE9PW87KXtm
b3IodmFyIGg9W107ciE9PWkmJnIhPT1vOyloLnB1c2gocikscj1lKCk7KCF0fHwo
aD10KGgsZisrKSkpJiZhLnB1c2goaCl9cmV0dXJuIGF9LGUuZm9ybWF0PWZ1bmN0
aW9uKHQpe2lmKEFycmF5LmlzQXJyYXkodFswXSkpcmV0dXJuIGUuZm9ybWF0Um93
cyh0KTt2YXIgcj1uZXcgaCx1PVtdO3JldHVybiB0LmZvckVhY2goZnVuY3Rpb24o
bil7Zm9yKHZhciB0IGluIG4pci5oYXModCl8fHUucHVzaChyLmFkZCh0KSl9KSxb
dS5tYXAobykuam9pbihuKV0uY29uY2F0KHQubWFwKGZ1bmN0aW9uKHQpe3JldHVy
biB1Lm1hcChmdW5jdGlvbihuKXtyZXR1cm4gbyh0W25dKX0pLmpvaW4obil9KSku
am9pbigiXG4iKX0sZS5mb3JtYXRSb3dzPWZ1bmN0aW9uKG4pe3JldHVybiBuLm1h
cChpKS5qb2luKCJcbiIpfSxlfSxWby5jc3Y9Vm8uZHN2KCIsIiwidGV4dC9jc3Yi
KSxWby50c3Y9Vm8uZHN2KCIJIiwidGV4dC90YWItc2VwYXJhdGVkLXZhbHVlcyIp
O3ZhciBWYSxYYSwkYSxCYSxXYSxKYT1Kb1twKEpvLCJyZXF1ZXN0QW5pbWF0aW9u
RnJhbWUiKV18fGZ1bmN0aW9uKG4pe3NldFRpbWVvdXQobiwxNyl9O1ZvLnRpbWVy
PWZ1bmN0aW9uKG4sdCxlKXt2YXIgcj1hcmd1bWVudHMubGVuZ3RoOzI+ciYmKHQ9
MCksMz5yJiYoZT1EYXRlLm5vdygpKTt2YXIgdT1lK3QsaT17YzpuLHQ6dSxmOiEx
LG46bnVsbH07WGE/WGEubj1pOlZhPWksWGE9aSwkYXx8KEJhPWNsZWFyVGltZW91
dChCYSksJGE9MSxKYShDdCkpfSxWby50aW1lci5mbHVzaD1mdW5jdGlvbigpe050
KCksenQoKX0sVm8ucm91bmQ9ZnVuY3Rpb24obix0KXtyZXR1cm4gdD9NYXRoLnJv
dW5kKG4qKHQ9TWF0aC5wb3coMTAsdCkpKS90Ok1hdGgucm91bmQobil9O3ZhciBH
YT1bInkiLCJ6IiwiYSIsImYiLCJwIiwibiIsIlx4YjUiLCJtIiwiIiwiayIsIk0i
LCJHIiwiVCIsIlAiLCJFIiwiWiIsIlkiXS5tYXAoVHQpO1ZvLmZvcm1hdFByZWZp
eD1mdW5jdGlvbihuLHQpe3ZhciBlPTA7cmV0dXJuIG4mJigwPm4mJihuKj0tMSks
dCYmKG49Vm8ucm91bmQobixMdChuLHQpKSksZT0xK01hdGguZmxvb3IoMWUtMTIr
TWF0aC5sb2cobikvTWF0aC5MTjEwKSxlPU1hdGgubWF4KC0yNCxNYXRoLm1pbigy
NCwzKk1hdGguZmxvb3IoKGUtMSkvMykpKSksR2FbOCtlLzNdfTt2YXIgS2E9Lyg/
OihbXntdKT8oWzw+PV5dKSk/KFsrXC0gXSk/KFskI10pPygwKT8oXGQrKT8oLCk/
KFwuLT9cZCspPyhbYS16JV0pPy9pLFFhPVZvLm1hcCh7YjpmdW5jdGlvbihuKXty
ZXR1cm4gbi50b1N0cmluZygyKX0sYzpmdW5jdGlvbihuKXtyZXR1cm4gU3RyaW5n
LmZyb21DaGFyQ29kZShuKX0sbzpmdW5jdGlvbihuKXtyZXR1cm4gbi50b1N0cmlu
Zyg4KX0seDpmdW5jdGlvbihuKXtyZXR1cm4gbi50b1N0cmluZygxNil9LFg6ZnVu
Y3Rpb24obil7cmV0dXJuIG4udG9TdHJpbmcoMTYpLnRvVXBwZXJDYXNlKCl9LGc6
ZnVuY3Rpb24obix0KXtyZXR1cm4gbi50b1ByZWNpc2lvbih0KX0sZTpmdW5jdGlv
bihuLHQpe3JldHVybiBuLnRvRXhwb25lbnRpYWwodCl9LGY6ZnVuY3Rpb24obix0
KXtyZXR1cm4gbi50b0ZpeGVkKHQpfSxyOmZ1bmN0aW9uKG4sdCl7cmV0dXJuKG49
Vm8ucm91bmQobixMdChuLHQpKSkudG9GaXhlZChNYXRoLm1heCgwLE1hdGgubWlu
KDIwLEx0KG4qKDErMWUtMTUpLHQpKSkpfX0pLG5jPVZvLnRpbWU9e30sdGM9RGF0
ZTtEdC5wcm90b3R5cGU9e2dldERhdGU6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5f
LmdldFVUQ0RhdGUoKX0sZ2V0RGF5OmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuXy5n
ZXRVVENEYXkoKX0sZ2V0RnVsbFllYXI6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5f
LmdldFVUQ0Z1bGxZZWFyKCl9LGdldEhvdXJzOmZ1bmN0aW9uKCl7cmV0dXJuIHRo
aXMuXy5nZXRVVENIb3VycygpfSxnZXRNaWxsaXNlY29uZHM6ZnVuY3Rpb24oKXty
ZXR1cm4gdGhpcy5fLmdldFVUQ01pbGxpc2Vjb25kcygpfSxnZXRNaW51dGVzOmZ1
bmN0aW9uKCl7cmV0dXJuIHRoaXMuXy5nZXRVVENNaW51dGVzKCl9LGdldE1vbnRo
OmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuXy5nZXRVVENNb250aCgpfSxnZXRTZWNv
bmRzOmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuXy5nZXRVVENTZWNvbmRzKCl9LGdl
dFRpbWU6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5fLmdldFRpbWUoKX0sZ2V0VGlt
ZXpvbmVPZmZzZXQ6ZnVuY3Rpb24oKXtyZXR1cm4gMH0sdmFsdWVPZjpmdW5jdGlv
bigpe3JldHVybiB0aGlzLl8udmFsdWVPZigpfSxzZXREYXRlOmZ1bmN0aW9uKCl7
ZWMuc2V0VVRDRGF0ZS5hcHBseSh0aGlzLl8sYXJndW1lbnRzKX0sc2V0RGF5OmZ1
bmN0aW9uKCl7ZWMuc2V0VVRDRGF5LmFwcGx5KHRoaXMuXyxhcmd1bWVudHMpfSxz
ZXRGdWxsWWVhcjpmdW5jdGlvbigpe2VjLnNldFVUQ0Z1bGxZZWFyLmFwcGx5KHRo
aXMuXyxhcmd1bWVudHMpfSxzZXRIb3VyczpmdW5jdGlvbigpe2VjLnNldFVUQ0hv
dXJzLmFwcGx5KHRoaXMuXyxhcmd1bWVudHMpfSxzZXRNaWxsaXNlY29uZHM6ZnVu
Y3Rpb24oKXtlYy5zZXRVVENNaWxsaXNlY29uZHMuYXBwbHkodGhpcy5fLGFyZ3Vt
ZW50cyl9LHNldE1pbnV0ZXM6ZnVuY3Rpb24oKXtlYy5zZXRVVENNaW51dGVzLmFw
cGx5KHRoaXMuXyxhcmd1bWVudHMpfSxzZXRNb250aDpmdW5jdGlvbigpe2VjLnNl
dFVUQ01vbnRoLmFwcGx5KHRoaXMuXyxhcmd1bWVudHMpfSxzZXRTZWNvbmRzOmZ1
bmN0aW9uKCl7ZWMuc2V0VVRDU2Vjb25kcy5hcHBseSh0aGlzLl8sYXJndW1lbnRz
KX0sc2V0VGltZTpmdW5jdGlvbigpe2VjLnNldFRpbWUuYXBwbHkodGhpcy5fLGFy
Z3VtZW50cyl9fTt2YXIgZWM9RGF0ZS5wcm90b3R5cGU7bmMueWVhcj1QdChmdW5j
dGlvbihuKXtyZXR1cm4gbj1uYy5kYXkobiksbi5zZXRNb250aCgwLDEpLG59LGZ1
bmN0aW9uKG4sdCl7bi5zZXRGdWxsWWVhcihuLmdldEZ1bGxZZWFyKCkrdCl9LGZ1
bmN0aW9uKG4pe3JldHVybiBuLmdldEZ1bGxZZWFyKCl9KSxuYy55ZWFycz1uYy55
ZWFyLnJhbmdlLG5jLnllYXJzLnV0Yz1uYy55ZWFyLnV0Yy5yYW5nZSxuYy5kYXk9
UHQoZnVuY3Rpb24obil7dmFyIHQ9bmV3IHRjKDJlMywwKTtyZXR1cm4gdC5zZXRG
dWxsWWVhcihuLmdldEZ1bGxZZWFyKCksbi5nZXRNb250aCgpLG4uZ2V0RGF0ZSgp
KSx0fSxmdW5jdGlvbihuLHQpe24uc2V0RGF0ZShuLmdldERhdGUoKSt0KX0sZnVu
Y3Rpb24obil7cmV0dXJuIG4uZ2V0RGF0ZSgpLTF9KSxuYy5kYXlzPW5jLmRheS5y
YW5nZSxuYy5kYXlzLnV0Yz1uYy5kYXkudXRjLnJhbmdlLG5jLmRheU9mWWVhcj1m
dW5jdGlvbihuKXt2YXIgdD1uYy55ZWFyKG4pO3JldHVybiBNYXRoLmZsb29yKChu
LXQtNmU0KihuLmdldFRpbWV6b25lT2Zmc2V0KCktdC5nZXRUaW1lem9uZU9mZnNl
dCgpKSkvODY0ZTUpfSxbInN1bmRheSIsIm1vbmRheSIsInR1ZXNkYXkiLCJ3ZWRu
ZXNkYXkiLCJ0aHVyc2RheSIsImZyaWRheSIsInNhdHVyZGF5Il0uZm9yRWFjaChm
dW5jdGlvbihuLHQpe3Q9Ny10O3ZhciBlPW5jW25dPVB0KGZ1bmN0aW9uKG4pe3Jl
dHVybihuPW5jLmRheShuKSkuc2V0RGF0ZShuLmdldERhdGUoKS0obi5nZXREYXko
KSt0KSU3KSxufSxmdW5jdGlvbihuLHQpe24uc2V0RGF0ZShuLmdldERhdGUoKSs3
Kk1hdGguZmxvb3IodCkpfSxmdW5jdGlvbihuKXt2YXIgZT1uYy55ZWFyKG4pLmdl
dERheSgpO3JldHVybiBNYXRoLmZsb29yKChuYy5kYXlPZlllYXIobikrKGUrdCkl
NykvNyktKGUhPT10KX0pO25jW24rInMiXT1lLnJhbmdlLG5jW24rInMiXS51dGM9
ZS51dGMucmFuZ2UsbmNbbisiT2ZZZWFyIl09ZnVuY3Rpb24obil7dmFyIGU9bmMu
eWVhcihuKS5nZXREYXkoKTtyZXR1cm4gTWF0aC5mbG9vcigobmMuZGF5T2ZZZWFy
KG4pKyhlK3QpJTcpLzcpfX0pLG5jLndlZWs9bmMuc3VuZGF5LG5jLndlZWtzPW5j
LnN1bmRheS5yYW5nZSxuYy53ZWVrcy51dGM9bmMuc3VuZGF5LnV0Yy5yYW5nZSxu
Yy53ZWVrT2ZZZWFyPW5jLnN1bmRheU9mWWVhcjt2YXIgcmM9eyItIjoiIixfOiIg
IiwwOiIwIn0sdWM9L15ccypcZCsvLGljPS9eJS87Vm8ubG9jYWxlPWZ1bmN0aW9u
KG4pe3JldHVybntudW1iZXJGb3JtYXQ6cXQobiksdGltZUZvcm1hdDpqdChuKX19
O3ZhciBvYz1Wby5sb2NhbGUoe2RlY2ltYWw6Ii4iLHRob3VzYW5kczoiLCIsZ3Jv
dXBpbmc6WzNdLGN1cnJlbmN5OlsiJCIsIiJdLGRhdGVUaW1lOiIlYSAlYiAlZSAl
WCAlWSIsZGF0ZToiJW0vJWQvJVkiLHRpbWU6IiVIOiVNOiVTIixwZXJpb2RzOlsi
QU0iLCJQTSJdLGRheXM6WyJTdW5kYXkiLCJNb25kYXkiLCJUdWVzZGF5IiwiV2Vk
bmVzZGF5IiwiVGh1cnNkYXkiLCJGcmlkYXkiLCJTYXR1cmRheSJdLHNob3J0RGF5
czpbIlN1biIsIk1vbiIsIlR1ZSIsIldlZCIsIlRodSIsIkZyaSIsIlNhdCJdLG1v
bnRoczpbIkphbnVhcnkiLCJGZWJydWFyeSIsIk1hcmNoIiwiQXByaWwiLCJNYXki
LCJKdW5lIiwiSnVseSIsIkF1Z3VzdCIsIlNlcHRlbWJlciIsIk9jdG9iZXIiLCJO
b3ZlbWJlciIsIkRlY2VtYmVyIl0sc2hvcnRNb250aHM6WyJKYW4iLCJGZWIiLCJN
YXIiLCJBcHIiLCJNYXkiLCJKdW4iLCJKdWwiLCJBdWciLCJTZXAiLCJPY3QiLCJO
b3YiLCJEZWMiXX0pO1ZvLmZvcm1hdD1vYy5udW1iZXJGb3JtYXQsVm8uZ2VvPXt9
LGllLnByb3RvdHlwZT17czowLHQ6MCxhZGQ6ZnVuY3Rpb24obil7b2Uobix0aGlz
LnQsYWMpLG9lKGFjLnMsdGhpcy5zLHRoaXMpLHRoaXMucz90aGlzLnQrPWFjLnQ6
dGhpcy5zPWFjLnR9LHJlc2V0OmZ1bmN0aW9uKCl7dGhpcy5zPXRoaXMudD0wfSx2
YWx1ZU9mOmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuc319O3ZhciBhYz1uZXcgaWU7
Vm8uZ2VvLnN0cmVhbT1mdW5jdGlvbihuLHQpe24mJmNjLmhhc093blByb3BlcnR5
KG4udHlwZSk/Y2Nbbi50eXBlXShuLHQpOmFlKG4sdCl9O3ZhciBjYz17RmVhdHVy
ZTpmdW5jdGlvbihuLHQpe2FlKG4uZ2VvbWV0cnksdCl9LEZlYXR1cmVDb2xsZWN0
aW9uOmZ1bmN0aW9uKG4sdCl7Zm9yKHZhciBlPW4uZmVhdHVyZXMscj0tMSx1PWUu
bGVuZ3RoOysrcjx1OylhZShlW3JdLmdlb21ldHJ5LHQpfX0sbGM9e1NwaGVyZTpm
dW5jdGlvbihuLHQpe3Quc3BoZXJlKCl9LFBvaW50OmZ1bmN0aW9uKG4sdCl7bj1u
LmNvb3JkaW5hdGVzLHQucG9pbnQoblswXSxuWzFdLG5bMl0pfSxNdWx0aVBvaW50
OmZ1bmN0aW9uKG4sdCl7Zm9yKHZhciBlPW4uY29vcmRpbmF0ZXMscj0tMSx1PWUu
bGVuZ3RoOysrcjx1OyluPWVbcl0sdC5wb2ludChuWzBdLG5bMV0sblsyXSl9LExp
bmVTdHJpbmc6ZnVuY3Rpb24obix0KXtjZShuLmNvb3JkaW5hdGVzLHQsMCl9LE11
bHRpTGluZVN0cmluZzpmdW5jdGlvbihuLHQpe2Zvcih2YXIgZT1uLmNvb3JkaW5h
dGVzLHI9LTEsdT1lLmxlbmd0aDsrK3I8dTspY2UoZVtyXSx0LDApfSxQb2x5Z29u
OmZ1bmN0aW9uKG4sdCl7bGUobi5jb29yZGluYXRlcyx0KX0sTXVsdGlQb2x5Z29u
OmZ1bmN0aW9uKG4sdCl7Zm9yKHZhciBlPW4uY29vcmRpbmF0ZXMscj0tMSx1PWUu
bGVuZ3RoOysrcjx1OylsZShlW3JdLHQpfSxHZW9tZXRyeUNvbGxlY3Rpb246ZnVu
Y3Rpb24obix0KXtmb3IodmFyIGU9bi5nZW9tZXRyaWVzLHI9LTEsdT1lLmxlbmd0
aDsrK3I8dTspYWUoZVtyXSx0KX19O1ZvLmdlby5hcmVhPWZ1bmN0aW9uKG4pe3Jl
dHVybiBzYz0wLFZvLmdlby5zdHJlYW0obixoYyksc2N9O3ZhciBzYyxmYz1uZXcg
aWUsaGM9e3NwaGVyZTpmdW5jdGlvbigpe3NjKz00KndhfSxwb2ludDp2LGxpbmVT
dGFydDp2LGxpbmVFbmQ6dixwb2x5Z29uU3RhcnQ6ZnVuY3Rpb24oKXtmYy5yZXNl
dCgpLGhjLmxpbmVTdGFydD1zZX0scG9seWdvbkVuZDpmdW5jdGlvbigpe3ZhciBu
PTIqZmM7c2MrPTA+bj80KndhK246bixoYy5saW5lU3RhcnQ9aGMubGluZUVuZD1o
Yy5wb2ludD12fX07Vm8uZ2VvLmJvdW5kcz1mdW5jdGlvbigpe2Z1bmN0aW9uIG4o
bix0KXt4LnB1c2goTT1bcz1uLGg9bl0pLGY+dCYmKGY9dCksdD5nJiYoZz10KX1m
dW5jdGlvbiB0KHQsZSl7dmFyIHI9ZmUoW3QqQ2EsZSpDYV0pO2lmKG0pe3ZhciB1
PWdlKG0sciksaT1bdVsxXSwtdVswXSwwXSxvPWdlKGksdSk7ZGUobyksbz1tZShv
KTt2YXIgYz10LXAsbD1jPjA/MTotMSx2PW9bMF0qTmEqbCxkPWlhKGMpPjE4MDtp
ZihkXih2PmwqcCYmbCp0PnYpKXt2YXIgeT1vWzFdKk5hO3k+ZyYmKGc9eSl9ZWxz
ZSBpZih2PSh2KzM2MCklMzYwLTE4MCxkXih2PmwqcCYmbCp0PnYpKXt2YXIgeT0t
b1sxXSpOYTtmPnkmJihmPXkpfWVsc2UgZj5lJiYoZj1lKSxlPmcmJihnPWUpO2Q/
cD50P2Eocyx0KT5hKHMsaCkmJihoPXQpOmEodCxoKT5hKHMsaCkmJihzPXQpOmg+
PXM/KHM+dCYmKHM9dCksdD5oJiYoaD10KSk6dD5wP2Eocyx0KT5hKHMsaCkmJiho
PXQpOmEodCxoKT5hKHMsaCkmJihzPXQpfWVsc2Ugbih0LGUpO209cixwPXR9ZnVu
Y3Rpb24gZSgpe18ucG9pbnQ9dH1mdW5jdGlvbiByKCl7TVswXT1zLE1bMV09aCxf
LnBvaW50PW4sbT1udWxsfWZ1bmN0aW9uIHUobixlKXtpZihtKXt2YXIgcj1uLXA7
eSs9aWEocik+MTgwP3IrKHI+MD8zNjA6LTM2MCk6cn1lbHNlIHY9bixkPWU7aGMu
cG9pbnQobixlKSx0KG4sZSl9ZnVuY3Rpb24gaSgpe2hjLmxpbmVTdGFydCgpfWZ1
bmN0aW9uIG8oKXt1KHYsZCksaGMubGluZUVuZCgpLGlhKHkpPkVhJiYocz0tKGg9
MTgwKSksTVswXT1zLE1bMV09aCxtPW51bGx9ZnVuY3Rpb24gYShuLHQpe3JldHVy
bih0LT1uKTwwP3QrMzYwOnR9ZnVuY3Rpb24gYyhuLHQpe3JldHVybiBuWzBdLXRb
MF19ZnVuY3Rpb24gbChuLHQpe3JldHVybiB0WzBdPD10WzFdP3RbMF08PW4mJm48
PXRbMV06bjx0WzBdfHx0WzFdPG59dmFyIHMsZixoLGcscCx2LGQsbSx5LHgsTSxf
PXtwb2ludDpuLGxpbmVTdGFydDplLGxpbmVFbmQ6cixwb2x5Z29uU3RhcnQ6ZnVu
Y3Rpb24oKXtfLnBvaW50PXUsXy5saW5lU3RhcnQ9aSxfLmxpbmVFbmQ9byx5PTAs
aGMucG9seWdvblN0YXJ0KCl9LHBvbHlnb25FbmQ6ZnVuY3Rpb24oKXtoYy5wb2x5
Z29uRW5kKCksXy5wb2ludD1uLF8ubGluZVN0YXJ0PWUsXy5saW5lRW5kPXIsMD5m
Yz8ocz0tKGg9MTgwKSxmPS0oZz05MCkpOnk+RWE/Zz05MDotRWE+eSYmKGY9LTkw
KSxNWzBdPXMsTVsxXT1ofX07cmV0dXJuIGZ1bmN0aW9uKG4pe2c9aD0tKHM9Zj0x
LzApLHg9W10sVm8uZ2VvLnN0cmVhbShuLF8pO3ZhciB0PXgubGVuZ3RoO2lmKHQp
e3guc29ydChjKTtmb3IodmFyIGUscj0xLHU9eFswXSxpPVt1XTt0PnI7KytyKWU9
eFtyXSxsKGVbMF0sdSl8fGwoZVsxXSx1KT8oYSh1WzBdLGVbMV0pPmEodVswXSx1
WzFdKSYmKHVbMV09ZVsxXSksYShlWzBdLHVbMV0pPmEodVswXSx1WzFdKSYmKHVb
MF09ZVswXSkpOmkucHVzaCh1PWUpO2Zvcih2YXIgbyxlLHA9LTEvMCx0PWkubGVu
Z3RoLTEscj0wLHU9aVt0XTt0Pj1yO3U9ZSwrK3IpZT1pW3JdLChvPWEodVsxXSxl
WzBdKSk+cCYmKHA9byxzPWVbMF0saD11WzFdKQp9cmV0dXJuIHg9TT1udWxsLDEv
MD09PXN8fDEvMD09PWY/W1swLzAsMC8wXSxbMC8wLDAvMF1dOltbcyxmXSxbaCxn
XV19fSgpLFZvLmdlby5jZW50cm9pZD1mdW5jdGlvbihuKXtnYz1wYz12Yz1kYz1t
Yz15Yz14Yz1NYz1fYz1iYz13Yz0wLFZvLmdlby5zdHJlYW0obixTYyk7dmFyIHQ9
X2MsZT1iYyxyPXdjLHU9dCp0K2UqZStyKnI7cmV0dXJuIEFhPnUmJih0PXljLGU9
eGMscj1NYyxFYT5wYyYmKHQ9dmMsZT1kYyxyPW1jKSx1PXQqdCtlKmUrcipyLEFh
PnUpP1swLzAsMC8wXTpbTWF0aC5hdGFuMihlLHQpKk5hLEcoci9NYXRoLnNxcnQo
dSkpKk5hXX07dmFyIGdjLHBjLHZjLGRjLG1jLHljLHhjLE1jLF9jLGJjLHdjLFNj
PXtzcGhlcmU6dixwb2ludDp4ZSxsaW5lU3RhcnQ6X2UsbGluZUVuZDpiZSxwb2x5
Z29uU3RhcnQ6ZnVuY3Rpb24oKXtTYy5saW5lU3RhcnQ9d2V9LHBvbHlnb25FbmQ6
ZnVuY3Rpb24oKXtTYy5saW5lU3RhcnQ9X2V9fSxrYz1DZShTZSxUZSxSZSxbLXdh
LC13YS8yXSksRWM9MWU5O1ZvLmdlby5jbGlwRXh0ZW50PWZ1bmN0aW9uKCl7dmFy
IG4sdCxlLHIsdSxpLG89e3N0cmVhbTpmdW5jdGlvbihuKXtyZXR1cm4gdSYmKHUu
dmFsaWQ9ITEpLHU9aShuKSx1LnZhbGlkPSEwLHV9LGV4dGVudDpmdW5jdGlvbihh
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaT1qZShuPSthWzBdWzBdLHQ9K2Fb
MF1bMV0sZT0rYVsxXVswXSxyPSthWzFdWzFdKSx1JiYodS52YWxpZD0hMSx1PW51
bGwpLG8pOltbbix0XSxbZSxyXV19fTtyZXR1cm4gby5leHRlbnQoW1swLDBdLFs5
NjAsNTAwXV0pfSwoVm8uZ2VvLmNvbmljRXF1YWxBcmVhPWZ1bmN0aW9uKCl7cmV0
dXJuIEZlKE9lKX0pLnJhdz1PZSxWby5nZW8uYWxiZXJzPWZ1bmN0aW9uKCl7cmV0
dXJuIFZvLmdlby5jb25pY0VxdWFsQXJlYSgpLnJvdGF0ZShbOTYsMF0pLmNlbnRl
cihbLS42LDM4LjddKS5wYXJhbGxlbHMoWzI5LjUsNDUuNV0pLnNjYWxlKDEwNzAp
fSxWby5nZW8uYWxiZXJzVXNhPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuKXt2YXIg
aT1uWzBdLG89blsxXTtyZXR1cm4gdD1udWxsLGUoaSxvKSx0fHwocihpLG8pLHQp
fHx1KGksbyksdH12YXIgdCxlLHIsdSxpPVZvLmdlby5hbGJlcnMoKSxvPVZvLmdl
by5jb25pY0VxdWFsQXJlYSgpLnJvdGF0ZShbMTU0LDBdKS5jZW50ZXIoWy0yLDU4
LjVdKS5wYXJhbGxlbHMoWzU1LDY1XSksYT1Wby5nZW8uY29uaWNFcXVhbEFyZWEo
KS5yb3RhdGUoWzE1NywwXSkuY2VudGVyKFstMywxOS45XSkucGFyYWxsZWxzKFs4
LDE4XSksYz17cG9pbnQ6ZnVuY3Rpb24obixlKXt0PVtuLGVdfX07cmV0dXJuIG4u
aW52ZXJ0PWZ1bmN0aW9uKG4pe3ZhciB0PWkuc2NhbGUoKSxlPWkudHJhbnNsYXRl
KCkscj0oblswXS1lWzBdKS90LHU9KG5bMV0tZVsxXSkvdDtyZXR1cm4odT49LjEy
JiYuMjM0PnUmJnI+PS0uNDI1JiYtLjIxND5yP286dT49LjE2NiYmLjIzND51JiZy
Pj0tLjIxNCYmLS4xMTU+cj9hOmkpLmludmVydChuKX0sbi5zdHJlYW09ZnVuY3Rp
b24obil7dmFyIHQ9aS5zdHJlYW0obiksZT1vLnN0cmVhbShuKSxyPWEuc3RyZWFt
KG4pO3JldHVybntwb2ludDpmdW5jdGlvbihuLHUpe3QucG9pbnQobix1KSxlLnBv
aW50KG4sdSksci5wb2ludChuLHUpfSxzcGhlcmU6ZnVuY3Rpb24oKXt0LnNwaGVy
ZSgpLGUuc3BoZXJlKCksci5zcGhlcmUoKX0sbGluZVN0YXJ0OmZ1bmN0aW9uKCl7
dC5saW5lU3RhcnQoKSxlLmxpbmVTdGFydCgpLHIubGluZVN0YXJ0KCl9LGxpbmVF
bmQ6ZnVuY3Rpb24oKXt0LmxpbmVFbmQoKSxlLmxpbmVFbmQoKSxyLmxpbmVFbmQo
KX0scG9seWdvblN0YXJ0OmZ1bmN0aW9uKCl7dC5wb2x5Z29uU3RhcnQoKSxlLnBv
bHlnb25TdGFydCgpLHIucG9seWdvblN0YXJ0KCl9LHBvbHlnb25FbmQ6ZnVuY3Rp
b24oKXt0LnBvbHlnb25FbmQoKSxlLnBvbHlnb25FbmQoKSxyLnBvbHlnb25FbmQo
KX19fSxuLnByZWNpc2lvbj1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxl
bmd0aD8oaS5wcmVjaXNpb24odCksby5wcmVjaXNpb24odCksYS5wcmVjaXNpb24o
dCksbik6aS5wcmVjaXNpb24oKX0sbi5zY2FsZT1mdW5jdGlvbih0KXtyZXR1cm4g
YXJndW1lbnRzLmxlbmd0aD8oaS5zY2FsZSh0KSxvLnNjYWxlKC4zNSp0KSxhLnNj
YWxlKHQpLG4udHJhbnNsYXRlKGkudHJhbnNsYXRlKCkpKTppLnNjYWxlKCl9LG4u
dHJhbnNsYXRlPWZ1bmN0aW9uKHQpe2lmKCFhcmd1bWVudHMubGVuZ3RoKXJldHVy
biBpLnRyYW5zbGF0ZSgpO3ZhciBsPWkuc2NhbGUoKSxzPSt0WzBdLGY9K3RbMV07
cmV0dXJuIGU9aS50cmFuc2xhdGUodCkuY2xpcEV4dGVudChbW3MtLjQ1NSpsLGYt
LjIzOCpsXSxbcysuNDU1KmwsZisuMjM4KmxdXSkuc3RyZWFtKGMpLnBvaW50LHI9
by50cmFuc2xhdGUoW3MtLjMwNypsLGYrLjIwMSpsXSkuY2xpcEV4dGVudChbW3Mt
LjQyNSpsK0VhLGYrLjEyKmwrRWFdLFtzLS4yMTQqbC1FYSxmKy4yMzQqbC1FYV1d
KS5zdHJlYW0oYykucG9pbnQsdT1hLnRyYW5zbGF0ZShbcy0uMjA1KmwsZisuMjEy
KmxdKS5jbGlwRXh0ZW50KFtbcy0uMjE0KmwrRWEsZisuMTY2KmwrRWFdLFtzLS4x
MTUqbC1FYSxmKy4yMzQqbC1FYV1dKS5zdHJlYW0oYykucG9pbnQsbn0sbi5zY2Fs
ZSgxMDcwKX07dmFyIEFjLENjLE5jLHpjLExjLFRjLHFjPXtwb2ludDp2LGxpbmVT
dGFydDp2LGxpbmVFbmQ6dixwb2x5Z29uU3RhcnQ6ZnVuY3Rpb24oKXtDYz0wLHFj
LmxpbmVTdGFydD1ZZX0scG9seWdvbkVuZDpmdW5jdGlvbigpe3FjLmxpbmVTdGFy
dD1xYy5saW5lRW5kPXFjLnBvaW50PXYsQWMrPWlhKENjLzIpfX0sUmM9e3BvaW50
OkllLGxpbmVTdGFydDp2LGxpbmVFbmQ6dixwb2x5Z29uU3RhcnQ6dixwb2x5Z29u
RW5kOnZ9LERjPXtwb2ludDpYZSxsaW5lU3RhcnQ6JGUsbGluZUVuZDpCZSxwb2x5
Z29uU3RhcnQ6ZnVuY3Rpb24oKXtEYy5saW5lU3RhcnQ9V2V9LHBvbHlnb25FbmQ6
ZnVuY3Rpb24oKXtEYy5wb2ludD1YZSxEYy5saW5lU3RhcnQ9JGUsRGMubGluZUVu
ZD1CZX19O1ZvLmdlby5wYXRoPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuKXtyZXR1
cm4gbiYmKCJmdW5jdGlvbiI9PXR5cGVvZiBhJiZpLnBvaW50UmFkaXVzKCthLmFw
cGx5KHRoaXMsYXJndW1lbnRzKSksbyYmby52YWxpZHx8KG89dShpKSksVm8uZ2Vv
LnN0cmVhbShuLG8pKSxpLnJlc3VsdCgpfWZ1bmN0aW9uIHQoKXtyZXR1cm4gbz1u
dWxsLG59dmFyIGUscix1LGksbyxhPTQuNTtyZXR1cm4gbi5hcmVhPWZ1bmN0aW9u
KG4pe3JldHVybiBBYz0wLFZvLmdlby5zdHJlYW0obix1KHFjKSksQWN9LG4uY2Vu
dHJvaWQ9ZnVuY3Rpb24obil7cmV0dXJuIHZjPWRjPW1jPXljPXhjPU1jPV9jPWJj
PXdjPTAsVm8uZ2VvLnN0cmVhbShuLHUoRGMpKSx3Yz9bX2Mvd2MsYmMvd2NdOk1j
P1t5Yy9NYyx4Yy9NY106bWM/W3ZjL21jLGRjL21jXTpbMC8wLDAvMF19LG4uYm91
bmRzPWZ1bmN0aW9uKG4pe3JldHVybiBMYz1UYz0tKE5jPXpjPTEvMCksVm8uZ2Vv
LnN0cmVhbShuLHUoUmMpKSxbW05jLHpjXSxbTGMsVGNdXX0sbi5wcm9qZWN0aW9u
PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PShlPW4pP24u
c3RyZWFtfHxLZShuKTp3dCx0KCkpOmV9LG4uY29udGV4dD1mdW5jdGlvbihuKXty
ZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaT1udWxsPT0ocj1uKT9uZXcgWmU6bmV3
IEplKG4pLCJmdW5jdGlvbiIhPXR5cGVvZiBhJiZpLnBvaW50UmFkaXVzKGEpLHQo
KSk6cn0sbi5wb2ludFJhZGl1cz1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRz
Lmxlbmd0aD8oYT0iZnVuY3Rpb24iPT10eXBlb2YgdD90OihpLnBvaW50UmFkaXVz
KCt0KSwrdCksbik6YX0sbi5wcm9qZWN0aW9uKFZvLmdlby5hbGJlcnNVc2EoKSku
Y29udGV4dChudWxsKX0sVm8uZ2VvLnRyYW5zZm9ybT1mdW5jdGlvbihuKXtyZXR1
cm57c3RyZWFtOmZ1bmN0aW9uKHQpe3ZhciBlPW5ldyBRZSh0KTtmb3IodmFyIHIg
aW4gbillW3JdPW5bcl07cmV0dXJuIGV9fX0sUWUucHJvdG90eXBlPXtwb2ludDpm
dW5jdGlvbihuLHQpe3RoaXMuc3RyZWFtLnBvaW50KG4sdCl9LHNwaGVyZTpmdW5j
dGlvbigpe3RoaXMuc3RyZWFtLnNwaGVyZSgpfSxsaW5lU3RhcnQ6ZnVuY3Rpb24o
KXt0aGlzLnN0cmVhbS5saW5lU3RhcnQoKX0sbGluZUVuZDpmdW5jdGlvbigpe3Ro
aXMuc3RyZWFtLmxpbmVFbmQoKX0scG9seWdvblN0YXJ0OmZ1bmN0aW9uKCl7dGhp
cy5zdHJlYW0ucG9seWdvblN0YXJ0KCl9LHBvbHlnb25FbmQ6ZnVuY3Rpb24oKXt0
aGlzLnN0cmVhbS5wb2x5Z29uRW5kKCl9fSxWby5nZW8ucHJvamVjdGlvbj10cixW
by5nZW8ucHJvamVjdGlvbk11dGF0b3I9ZXIsKFZvLmdlby5lcXVpcmVjdGFuZ3Vs
YXI9ZnVuY3Rpb24oKXtyZXR1cm4gdHIodXIpfSkucmF3PXVyLmludmVydD11cixW
by5nZW8ucm90YXRpb249ZnVuY3Rpb24obil7ZnVuY3Rpb24gdCh0KXtyZXR1cm4g
dD1uKHRbMF0qQ2EsdFsxXSpDYSksdFswXSo9TmEsdFsxXSo9TmEsdH1yZXR1cm4g
bj1vcihuWzBdJTM2MCpDYSxuWzFdKkNhLG4ubGVuZ3RoPjI/blsyXSpDYTowKSx0
LmludmVydD1mdW5jdGlvbih0KXtyZXR1cm4gdD1uLmludmVydCh0WzBdKkNhLHRb
MV0qQ2EpLHRbMF0qPU5hLHRbMV0qPU5hLHR9LHR9LGlyLmludmVydD11cixWby5n
ZW8uY2lyY2xlPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbigpe3ZhciBuPSJmdW5jdGlv
biI9PXR5cGVvZiByP3IuYXBwbHkodGhpcyxhcmd1bWVudHMpOnIsdD1vcigtblsw
XSpDYSwtblsxXSpDYSwwKS5pbnZlcnQsdT1bXTtyZXR1cm4gZShudWxsLG51bGws
MSx7cG9pbnQ6ZnVuY3Rpb24obixlKXt1LnB1c2gobj10KG4sZSkpLG5bMF0qPU5h
LG5bMV0qPU5hfX0pLHt0eXBlOiJQb2x5Z29uIixjb29yZGluYXRlczpbdV19fXZh
ciB0LGUscj1bMCwwXSx1PTY7cmV0dXJuIG4ub3JpZ2luPWZ1bmN0aW9uKHQpe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPXQsbik6cn0sbi5hbmdsZT1mdW5jdGlv
bihyKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT1zcigodD0rcikqQ2EsdSpD
YSksbik6dH0sbi5wcmVjaXNpb249ZnVuY3Rpb24ocil7cmV0dXJuIGFyZ3VtZW50
cy5sZW5ndGg/KGU9c3IodCpDYSwodT0rcikqQ2EpLG4pOnV9LG4uYW5nbGUoOTAp
fSxWby5nZW8uZGlzdGFuY2U9ZnVuY3Rpb24obix0KXt2YXIgZSxyPSh0WzBdLW5b
MF0pKkNhLHU9blsxXSpDYSxpPXRbMV0qQ2Esbz1NYXRoLnNpbihyKSxhPU1hdGgu
Y29zKHIpLGM9TWF0aC5zaW4odSksbD1NYXRoLmNvcyh1KSxzPU1hdGguc2luKGkp
LGY9TWF0aC5jb3MoaSk7cmV0dXJuIE1hdGguYXRhbjIoTWF0aC5zcXJ0KChlPWYq
bykqZSsoZT1sKnMtYypmKmEpKmUpLGMqcytsKmYqYSl9LFZvLmdlby5ncmF0aWN1
bGU9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKCl7cmV0dXJue3R5cGU6Ik11bHRpTGlu
ZVN0cmluZyIsY29vcmRpbmF0ZXM6dCgpfX1mdW5jdGlvbiB0KCl7cmV0dXJuIFZv
LnJhbmdlKE1hdGguY2VpbChpL2QpKmQsdSxkKS5tYXAoaCkuY29uY2F0KFZvLnJh
bmdlKE1hdGguY2VpbChsL20pKm0sYyxtKS5tYXAoZykpLmNvbmNhdChWby5yYW5n
ZShNYXRoLmNlaWwoci9wKSpwLGUscCkuZmlsdGVyKGZ1bmN0aW9uKG4pe3JldHVy
biBpYShuJWQpPkVhfSkubWFwKHMpKS5jb25jYXQoVm8ucmFuZ2UoTWF0aC5jZWls
KGEvdikqdixvLHYpLmZpbHRlcihmdW5jdGlvbihuKXtyZXR1cm4gaWEobiVtKT5F
YX0pLm1hcChmKSl9dmFyIGUscix1LGksbyxhLGMsbCxzLGYsaCxnLHA9MTAsdj1w
LGQ9OTAsbT0zNjAseT0yLjU7cmV0dXJuIG4ubGluZXM9ZnVuY3Rpb24oKXtyZXR1
cm4gdCgpLm1hcChmdW5jdGlvbihuKXtyZXR1cm57dHlwZToiTGluZVN0cmluZyIs
Y29vcmRpbmF0ZXM6bn19KX0sbi5vdXRsaW5lPWZ1bmN0aW9uKCl7cmV0dXJue3R5
cGU6IlBvbHlnb24iLGNvb3JkaW5hdGVzOltoKGkpLmNvbmNhdChnKGMpLnNsaWNl
KDEpLGgodSkucmV2ZXJzZSgpLnNsaWNlKDEpLGcobCkucmV2ZXJzZSgpLnNsaWNl
KDEpKV19fSxuLmV4dGVudD1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxl
bmd0aD9uLm1ham9yRXh0ZW50KHQpLm1pbm9yRXh0ZW50KHQpOm4ubWlub3JFeHRl
bnQoKX0sbi5tYWpvckV4dGVudD1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRz
Lmxlbmd0aD8oaT0rdFswXVswXSx1PSt0WzFdWzBdLGw9K3RbMF1bMV0sYz0rdFsx
XVsxXSxpPnUmJih0PWksaT11LHU9dCksbD5jJiYodD1sLGw9YyxjPXQpLG4ucHJl
Y2lzaW9uKHkpKTpbW2ksbF0sW3UsY11dfSxuLm1pbm9yRXh0ZW50PWZ1bmN0aW9u
KHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPSt0WzBdWzBdLGU9K3RbMV1b
MF0sYT0rdFswXVsxXSxvPSt0WzFdWzFdLHI+ZSYmKHQ9cixyPWUsZT10KSxhPm8m
Jih0PWEsYT1vLG89dCksbi5wcmVjaXNpb24oeSkpOltbcixhXSxbZSxvXV19LG4u
c3RlcD1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD9uLm1ham9y
U3RlcCh0KS5taW5vclN0ZXAodCk6bi5taW5vclN0ZXAoKX0sbi5tYWpvclN0ZXA9
ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGQ9K3RbMF0sbT0r
dFsxXSxuKTpbZCxtXX0sbi5taW5vclN0ZXA9ZnVuY3Rpb24odCl7cmV0dXJuIGFy
Z3VtZW50cy5sZW5ndGg/KHA9K3RbMF0sdj0rdFsxXSxuKTpbcCx2XX0sbi5wcmVj
aXNpb249ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHk9K3Qs
cz1ocihhLG8sOTApLGY9Z3IocixlLHkpLGg9aHIobCxjLDkwKSxnPWdyKGksdSx5
KSxuKTp5fSxuLm1ham9yRXh0ZW50KFtbLTE4MCwtOTArRWFdLFsxODAsOTAtRWFd
XSkubWlub3JFeHRlbnQoW1stMTgwLC04MC1FYV0sWzE4MCw4MCtFYV1dKX0sVm8u
Z2VvLmdyZWF0QXJjPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbigpe3JldHVybnt0eXBl
OiJMaW5lU3RyaW5nIixjb29yZGluYXRlczpbdHx8ci5hcHBseSh0aGlzLGFyZ3Vt
ZW50cyksZXx8dS5hcHBseSh0aGlzLGFyZ3VtZW50cyldfX12YXIgdCxlLHI9cHIs
dT12cjtyZXR1cm4gbi5kaXN0YW5jZT1mdW5jdGlvbigpe3JldHVybiBWby5nZW8u
ZGlzdGFuY2UodHx8ci5hcHBseSh0aGlzLGFyZ3VtZW50cyksZXx8dS5hcHBseSh0
aGlzLGFyZ3VtZW50cykpfSxuLnNvdXJjZT1mdW5jdGlvbihlKXtyZXR1cm4gYXJn
dW1lbnRzLmxlbmd0aD8ocj1lLHQ9ImZ1bmN0aW9uIj09dHlwZW9mIGU/bnVsbDpl
LG4pOnJ9LG4udGFyZ2V0PWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyh1PXQsZT0iZnVuY3Rpb24iPT10eXBlb2YgdD9udWxsOnQsbik6dX0sbi5w
cmVjaXNpb249ZnVuY3Rpb24oKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD9uOjB9
LG59LFZvLmdlby5pbnRlcnBvbGF0ZT1mdW5jdGlvbihuLHQpe3JldHVybiBkcihu
WzBdKkNhLG5bMV0qQ2EsdFswXSpDYSx0WzFdKkNhKX0sVm8uZ2VvLmxlbmd0aD1m
dW5jdGlvbihuKXtyZXR1cm4gUGM9MCxWby5nZW8uc3RyZWFtKG4sVWMpLFBjfTt2
YXIgUGMsVWM9e3NwaGVyZTp2LHBvaW50OnYsbGluZVN0YXJ0Om1yLGxpbmVFbmQ6
dixwb2x5Z29uU3RhcnQ6dixwb2x5Z29uRW5kOnZ9LGpjPXlyKGZ1bmN0aW9uKG4p
e3JldHVybiBNYXRoLnNxcnQoMi8oMStuKSl9LGZ1bmN0aW9uKG4pe3JldHVybiAy
Kk1hdGguYXNpbihuLzIpfSk7KFZvLmdlby5hemltdXRoYWxFcXVhbEFyZWE9ZnVu
Y3Rpb24oKXtyZXR1cm4gdHIoamMpfSkucmF3PWpjO3ZhciBIYz15cihmdW5jdGlv
bihuKXt2YXIgdD1NYXRoLmFjb3Mobik7cmV0dXJuIHQmJnQvTWF0aC5zaW4odCl9
LHd0KTsoVm8uZ2VvLmF6aW11dGhhbEVxdWlkaXN0YW50PWZ1bmN0aW9uKCl7cmV0
dXJuIHRyKEhjKX0pLnJhdz1IYywoVm8uZ2VvLmNvbmljQ29uZm9ybWFsPWZ1bmN0
aW9uKCl7cmV0dXJuIEZlKHhyKX0pLnJhdz14ciwoVm8uZ2VvLmNvbmljRXF1aWRp
c3RhbnQ9ZnVuY3Rpb24oKXtyZXR1cm4gRmUoTXIpfSkucmF3PU1yO3ZhciBGYz15
cihmdW5jdGlvbihuKXtyZXR1cm4gMS9ufSxNYXRoLmF0YW4pOyhWby5nZW8uZ25v
bW9uaWM9ZnVuY3Rpb24oKXtyZXR1cm4gdHIoRmMpfSkucmF3PUZjLF9yLmludmVy
dD1mdW5jdGlvbihuLHQpe3JldHVybltuLDIqTWF0aC5hdGFuKE1hdGguZXhwKHQp
KS1rYV19LChWby5nZW8ubWVyY2F0b3I9ZnVuY3Rpb24oKXtyZXR1cm4gYnIoX3Ip
fSkucmF3PV9yO3ZhciBPYz15cihmdW5jdGlvbigpe3JldHVybiAxfSxNYXRoLmFz
aW4pOyhWby5nZW8ub3J0aG9ncmFwaGljPWZ1bmN0aW9uKCl7cmV0dXJuIHRyKE9j
KX0pLnJhdz1PYzt2YXIgWWM9eXIoZnVuY3Rpb24obil7cmV0dXJuIDEvKDErbil9
LGZ1bmN0aW9uKG4pe3JldHVybiAyKk1hdGguYXRhbihuKX0pOyhWby5nZW8uc3Rl
cmVvZ3JhcGhpYz1mdW5jdGlvbigpe3JldHVybiB0cihZYyl9KS5yYXc9WWMsd3Iu
aW52ZXJ0PWZ1bmN0aW9uKG4sdCl7cmV0dXJuWy10LDIqTWF0aC5hdGFuKE1hdGgu
ZXhwKG4pKS1rYV19LChWby5nZW8udHJhbnN2ZXJzZU1lcmNhdG9yPWZ1bmN0aW9u
KCl7dmFyIG49YnIod3IpLHQ9bi5jZW50ZXIsZT1uLnJvdGF0ZTtyZXR1cm4gbi5j
ZW50ZXI9ZnVuY3Rpb24obil7cmV0dXJuIG4/dChbLW5bMV0sblswXV0pOihuPXQo
KSxbblsxXSwtblswXV0pfSxuLnJvdGF0ZT1mdW5jdGlvbihuKXtyZXR1cm4gbj9l
KFtuWzBdLG5bMV0sbi5sZW5ndGg+Mj9uWzJdKzkwOjkwXSk6KG49ZSgpLFtuWzBd
LG5bMV0sblsyXS05MF0pfSxlKFswLDAsOTBdKX0pLnJhdz13cixWby5nZW9tPXt9
LFZvLmdlb20uaHVsbD1mdW5jdGlvbihuKXtmdW5jdGlvbiB0KG4pe2lmKG4ubGVu
Z3RoPDMpcmV0dXJuW107dmFyIHQsdT1idChlKSxpPWJ0KHIpLG89bi5sZW5ndGgs
YT1bXSxjPVtdO2Zvcih0PTA7bz50O3QrKylhLnB1c2goWyt1LmNhbGwodGhpcyxu
W3RdLHQpLCtpLmNhbGwodGhpcyxuW3RdLHQpLHRdKTtmb3IoYS5zb3J0KEFyKSx0
PTA7bz50O3QrKyljLnB1c2goW2FbdF1bMF0sLWFbdF1bMV1dKTt2YXIgbD1Fcihh
KSxzPUVyKGMpLGY9c1swXT09PWxbMF0saD1zW3MubGVuZ3RoLTFdPT09bFtsLmxl
bmd0aC0xXSxnPVtdO2Zvcih0PWwubGVuZ3RoLTE7dD49MDstLXQpZy5wdXNoKG5b
YVtsW3RdXVsyXV0pO2Zvcih0PStmO3Q8cy5sZW5ndGgtaDsrK3QpZy5wdXNoKG5b
YVtzW3RdXVsyXV0pO3JldHVybiBnfXZhciBlPVNyLHI9a3I7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/dChuKToodC54PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVu
dHMubGVuZ3RoPyhlPW4sdCk6ZX0sdC55PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1
bWVudHMubGVuZ3RoPyhyPW4sdCk6cn0sdCl9LFZvLmdlb20ucG9seWdvbj1mdW5j
dGlvbihuKXtyZXR1cm4gc2EobixJYyksbn07dmFyIEljPVZvLmdlb20ucG9seWdv
bi5wcm90b3R5cGU9W107SWMuYXJlYT1mdW5jdGlvbigpe2Zvcih2YXIgbix0PS0x
LGU9dGhpcy5sZW5ndGgscj10aGlzW2UtMV0sdT0wOysrdDxlOyluPXIscj10aGlz
W3RdLHUrPW5bMV0qclswXS1uWzBdKnJbMV07cmV0dXJuLjUqdX0sSWMuY2VudHJv
aWQ9ZnVuY3Rpb24obil7dmFyIHQsZSxyPS0xLHU9dGhpcy5sZW5ndGgsaT0wLG89
MCxhPXRoaXNbdS0xXTtmb3IoYXJndW1lbnRzLmxlbmd0aHx8KG49LTEvKDYqdGhp
cy5hcmVhKCkpKTsrK3I8dTspdD1hLGE9dGhpc1tyXSxlPXRbMF0qYVsxXS1hWzBd
KnRbMV0saSs9KHRbMF0rYVswXSkqZSxvKz0odFsxXSthWzFdKSplO3JldHVybltp
Km4sbypuXX0sSWMuY2xpcD1mdW5jdGlvbihuKXtmb3IodmFyIHQsZSxyLHUsaSxv
LGE9enIobiksYz0tMSxsPXRoaXMubGVuZ3RoLXpyKHRoaXMpLHM9dGhpc1tsLTFd
OysrYzxsOyl7Zm9yKHQ9bi5zbGljZSgpLG4ubGVuZ3RoPTAsdT10aGlzW2NdLGk9
dFsocj10Lmxlbmd0aC1hKS0xXSxlPS0xOysrZTxyOylvPXRbZV0sQ3IobyxzLHUp
PyhDcihpLHMsdSl8fG4ucHVzaChOcihpLG8scyx1KSksbi5wdXNoKG8pKTpDcihp
LHMsdSkmJm4ucHVzaChOcihpLG8scyx1KSksaT1vO2EmJm4ucHVzaChuWzBdKSxz
PXV9cmV0dXJuIG59O3ZhciBaYyxWYyxYYywkYyxCYyxXYz1bXSxKYz1bXTtqci5w
cm90b3R5cGUucHJlcGFyZT1mdW5jdGlvbigpe2Zvcih2YXIgbix0PXRoaXMuZWRn
ZXMsZT10Lmxlbmd0aDtlLS07KW49dFtlXS5lZGdlLG4uYiYmbi5hfHx0LnNwbGlj
ZShlLDEpO3JldHVybiB0LnNvcnQoRnIpLHQubGVuZ3RofSxKci5wcm90b3R5cGU9
e3N0YXJ0OmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuZWRnZS5sPT09dGhpcy5zaXRl
P3RoaXMuZWRnZS5hOnRoaXMuZWRnZS5ifSxlbmQ6ZnVuY3Rpb24oKXtyZXR1cm4g
dGhpcy5lZGdlLmw9PT10aGlzLnNpdGU/dGhpcy5lZGdlLmI6dGhpcy5lZGdlLmF9
fSxHci5wcm90b3R5cGU9e2luc2VydDpmdW5jdGlvbihuLHQpe3ZhciBlLHIsdTtp
ZihuKXtpZih0LlA9bix0Lk49bi5OLG4uTiYmKG4uTi5QPXQpLG4uTj10LG4uUil7
Zm9yKG49bi5SO24uTDspbj1uLkw7bi5MPXR9ZWxzZSBuLlI9dDtlPW59ZWxzZSB0
aGlzLl8/KG49dHUodGhpcy5fKSx0LlA9bnVsbCx0Lk49bixuLlA9bi5MPXQsZT1u
KToodC5QPXQuTj1udWxsLHRoaXMuXz10LGU9bnVsbCk7Zm9yKHQuTD10LlI9bnVs
bCx0LlU9ZSx0LkM9ITAsbj10O2UmJmUuQzspcj1lLlUsZT09PXIuTD8odT1yLlIs
dSYmdS5DPyhlLkM9dS5DPSExLHIuQz0hMCxuPXIpOihuPT09ZS5SJiYoUXIodGhp
cyxlKSxuPWUsZT1uLlUpLGUuQz0hMSxyLkM9ITAsbnUodGhpcyxyKSkpOih1PXIu
TCx1JiZ1LkM/KGUuQz11LkM9ITEsci5DPSEwLG49cik6KG49PT1lLkwmJihudSh0
aGlzLGUpLG49ZSxlPW4uVSksZS5DPSExLHIuQz0hMCxRcih0aGlzLHIpKSksZT1u
LlU7dGhpcy5fLkM9ITF9LHJlbW92ZTpmdW5jdGlvbihuKXtuLk4mJihuLk4uUD1u
LlApLG4uUCYmKG4uUC5OPW4uTiksbi5OPW4uUD1udWxsO3ZhciB0LGUscix1PW4u
VSxpPW4uTCxvPW4uUjtpZihlPWk/bz90dShvKTppOm8sdT91Lkw9PT1uP3UuTD1l
OnUuUj1lOnRoaXMuXz1lLGkmJm8/KHI9ZS5DLGUuQz1uLkMsZS5MPWksaS5VPWUs
ZSE9PW8/KHU9ZS5VLGUuVT1uLlUsbj1lLlIsdS5MPW4sZS5SPW8sby5VPWUpOihl
LlU9dSx1PWUsbj1lLlIpKToocj1uLkMsbj1lKSxuJiYobi5VPXUpLCFyKXtpZihu
JiZuLkMpcmV0dXJuIG4uQz0hMSx2b2lkIDA7ZG97aWYobj09PXRoaXMuXylicmVh
aztpZihuPT09dS5MKXtpZih0PXUuUix0LkMmJih0LkM9ITEsdS5DPSEwLFFyKHRo
aXMsdSksdD11LlIpLHQuTCYmdC5MLkN8fHQuUiYmdC5SLkMpe3QuUiYmdC5SLkN8
fCh0LkwuQz0hMSx0LkM9ITAsbnUodGhpcyx0KSx0PXUuUiksdC5DPXUuQyx1LkM9
dC5SLkM9ITEsUXIodGhpcyx1KSxuPXRoaXMuXzticmVha319ZWxzZSBpZih0PXUu
TCx0LkMmJih0LkM9ITEsdS5DPSEwLG51KHRoaXMsdSksdD11LkwpLHQuTCYmdC5M
LkN8fHQuUiYmdC5SLkMpe3QuTCYmdC5MLkN8fCh0LlIuQz0hMSx0LkM9ITAsUXIo
dGhpcyx0KSx0PXUuTCksdC5DPXUuQyx1LkM9dC5MLkM9ITEsbnUodGhpcyx1KSxu
PXRoaXMuXzticmVha310LkM9ITAsbj11LHU9dS5VfXdoaWxlKCFuLkMpO24mJihu
LkM9ITEpfX19LFZvLmdlb20udm9yb25vaT1mdW5jdGlvbihuKXtmdW5jdGlvbiB0
KG4pe3ZhciB0PW5ldyBBcnJheShuLmxlbmd0aCkscj1hWzBdWzBdLHU9YVswXVsx
XSxpPWFbMV1bMF0sbz1hWzFdWzFdO3JldHVybiBldShlKG4pLGEpLmNlbGxzLmZv
ckVhY2goZnVuY3Rpb24oZSxhKXt2YXIgYz1lLmVkZ2VzLGw9ZS5zaXRlLHM9dFth
XT1jLmxlbmd0aD9jLm1hcChmdW5jdGlvbihuKXt2YXIgdD1uLnN0YXJ0KCk7cmV0
dXJuW3QueCx0LnldfSk6bC54Pj1yJiZsLng8PWkmJmwueT49dSYmbC55PD1vP1tb
cixvXSxbaSxvXSxbaSx1XSxbcix1XV06W107cy5wb2ludD1uW2FdfSksdH1mdW5j
dGlvbiBlKG4pe3JldHVybiBuLm1hcChmdW5jdGlvbihuLHQpe3JldHVybnt4Ok1h
dGgucm91bmQoaShuLHQpL0VhKSpFYSx5Ok1hdGgucm91bmQobyhuLHQpL0VhKSpF
YSxpOnR9fSl9dmFyIHI9U3IsdT1rcixpPXIsbz11LGE9R2M7cmV0dXJuIG4/dChu
KToodC5saW5rcz1mdW5jdGlvbihuKXtyZXR1cm4gZXUoZShuKSkuZWRnZXMuZmls
dGVyKGZ1bmN0aW9uKG4pe3JldHVybiBuLmwmJm4ucn0pLm1hcChmdW5jdGlvbih0
KXtyZXR1cm57c291cmNlOm5bdC5sLmldLHRhcmdldDpuW3Quci5pXX19KX0sdC50
cmlhbmdsZXM9ZnVuY3Rpb24obil7dmFyIHQ9W107cmV0dXJuIGV1KGUobikpLmNl
bGxzLmZvckVhY2goZnVuY3Rpb24oZSxyKXtmb3IodmFyIHUsaSxvPWUuc2l0ZSxh
PWUuZWRnZXMuc29ydChGciksYz0tMSxsPWEubGVuZ3RoLHM9YVtsLTFdLmVkZ2Us
Zj1zLmw9PT1vP3MucjpzLmw7KytjPGw7KXU9cyxpPWYscz1hW2NdLmVkZ2UsZj1z
Lmw9PT1vP3MucjpzLmwscjxpLmkmJnI8Zi5pJiZ1dShvLGksZik8MCYmdC5wdXNo
KFtuW3JdLG5baS5pXSxuW2YuaV1dKX0pLHR9LHQueD1mdW5jdGlvbihuKXtyZXR1
cm4gYXJndW1lbnRzLmxlbmd0aD8oaT1idChyPW4pLHQpOnJ9LHQueT1mdW5jdGlv
bihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obz1idCh1PW4pLHQpOnV9LHQu
Y2xpcEV4dGVudD1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8o
YT1udWxsPT1uP0djOm4sdCk6YT09PUdjP251bGw6YX0sdC5zaXplPWZ1bmN0aW9u
KG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoP3QuY2xpcEV4dGVudChuJiZbWzAs
MF0sbl0pOmE9PT1HYz9udWxsOmEmJmFbMV19LHQpfTt2YXIgR2M9W1stMWU2LC0x
ZTZdLFsxZTYsMWU2XV07Vm8uZ2VvbS5kZWxhdW5heT1mdW5jdGlvbihuKXtyZXR1
cm4gVm8uZ2VvbS52b3Jvbm9pKCkudHJpYW5nbGVzKG4pfSxWby5nZW9tLnF1YWR0
cmVlPWZ1bmN0aW9uKG4sdCxlLHIsdSl7ZnVuY3Rpb24gaShuKXtmdW5jdGlvbiBp
KG4sdCxlLHIsdSxpLG8sYSl7aWYoIWlzTmFOKGUpJiYhaXNOYU4ocikpaWYobi5s
ZWFmKXt2YXIgYz1uLngscz1uLnk7aWYobnVsbCE9YylpZihpYShjLWUpK2lhKHMt
cik8LjAxKWwobix0LGUscix1LGksbyxhKTtlbHNle3ZhciBmPW4ucG9pbnQ7bi54
PW4ueT1uLnBvaW50PW51bGwsbChuLGYsYyxzLHUsaSxvLGEpLGwobix0LGUscix1
LGksbyxhKX1lbHNlIG4ueD1lLG4ueT1yLG4ucG9pbnQ9dH1lbHNlIGwobix0LGUs
cix1LGksbyxhKX1mdW5jdGlvbiBsKG4sdCxlLHIsdSxvLGEsYyl7dmFyIGw9LjUq
KHUrYSkscz0uNSoobytjKSxmPWU+PWwsaD1yPj1zLGc9KGg8PDEpK2Y7bi5sZWFm
PSExLG49bi5ub2Rlc1tnXXx8KG4ubm9kZXNbZ109YXUoKSksZj91PWw6YT1sLGg/
bz1zOmM9cyxpKG4sdCxlLHIsdSxvLGEsYyl9dmFyIHMsZixoLGcscCx2LGQsbSx5
LHg9YnQoYSksTT1idChjKTtpZihudWxsIT10KXY9dCxkPWUsbT1yLHk9dTtlbHNl
IGlmKG09eT0tKHY9ZD0xLzApLGY9W10saD1bXSxwPW4ubGVuZ3RoLG8pZm9yKGc9
MDtwPmc7KytnKXM9bltnXSxzLng8diYmKHY9cy54KSxzLnk8ZCYmKGQ9cy55KSxz
Lng+bSYmKG09cy54KSxzLnk+eSYmKHk9cy55KSxmLnB1c2gocy54KSxoLnB1c2go
cy55KTtlbHNlIGZvcihnPTA7cD5nOysrZyl7dmFyIF89K3gocz1uW2ddLGcpLGI9
K00ocyxnKTt2Pl8mJih2PV8pLGQ+YiYmKGQ9YiksXz5tJiYobT1fKSxiPnkmJih5
PWIpLGYucHVzaChfKSxoLnB1c2goYil9dmFyIHc9bS12LFM9eS1kO3c+Uz95PWQr
dzptPXYrUzt2YXIgaz1hdSgpO2lmKGsuYWRkPWZ1bmN0aW9uKG4pe2koayxuLCt4
KG4sKytnKSwrTShuLGcpLHYsZCxtLHkpfSxrLnZpc2l0PWZ1bmN0aW9uKG4pe2N1
KG4sayx2LGQsbSx5KX0sZz0tMSxudWxsPT10KXtmb3IoOysrZzxwOylpKGssbltn
XSxmW2ddLGhbZ10sdixkLG0seSk7LS1nfWVsc2Ugbi5mb3JFYWNoKGsuYWRkKTty
ZXR1cm4gZj1oPW49cz1udWxsLGt9dmFyIG8sYT1TcixjPWtyO3JldHVybihvPWFy
Z3VtZW50cy5sZW5ndGgpPyhhPWl1LGM9b3UsMz09PW8mJih1PWUscj10LGU9dD0w
KSxpKG4pKTooaS54PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3Ro
PyhhPW4saSk6YX0saS55PWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVu
Z3RoPyhjPW4saSk6Y30saS5leHRlbnQ9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KG51bGw9PW4/dD1lPXI9dT1udWxsOih0PStuWzBdWzBdLGU9
K25bMF1bMV0scj0rblsxXVswXSx1PStuWzFdWzFdKSxpKTpudWxsPT10P251bGw6
W1t0LGVdLFtyLHVdXX0saS5zaXplPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVu
dHMubGVuZ3RoPyhudWxsPT1uP3Q9ZT1yPXU9bnVsbDoodD1lPTAscj0rblswXSx1
PStuWzFdKSxpKTpudWxsPT10P251bGw6W3ItdCx1LWVdfSxpKX0sVm8uaW50ZXJw
b2xhdGVSZ2I9bHUsVm8uaW50ZXJwb2xhdGVPYmplY3Q9c3UsVm8uaW50ZXJwb2xh
dGVOdW1iZXI9ZnUsVm8uaW50ZXJwb2xhdGVTdHJpbmc9aHU7dmFyIEtjPS9bLStd
Pyg/OlxkK1wuP1xkKnxcLj9cZCspKD86W2VFXVstK10/XGQrKT8vZyxRYz1uZXcg
UmVnRXhwKEtjLnNvdXJjZSwiZyIpO1ZvLmludGVycG9sYXRlPWd1LFZvLmludGVy
cG9sYXRvcnM9W2Z1bmN0aW9uKG4sdCl7dmFyIGU9dHlwZW9mIHQ7cmV0dXJuKCJz
dHJpbmciPT09ZT9aYS5oYXModCl8fC9eKCN8cmdiXCh8aHNsXCgpLy50ZXN0KHQp
P2x1Omh1OnQgaW5zdGFuY2VvZiBldD9sdTpBcnJheS5pc0FycmF5KHQpP3B1OiJv
YmplY3QiPT09ZSYmaXNOYU4odCk/c3U6ZnUpKG4sdCl9XSxWby5pbnRlcnBvbGF0
ZUFycmF5PXB1O3ZhciBubD1mdW5jdGlvbigpe3JldHVybiB3dH0sdGw9Vm8ubWFw
KHtsaW5lYXI6bmwscG9seTpfdSxxdWFkOmZ1bmN0aW9uKCl7cmV0dXJuIHl1fSxj
dWJpYzpmdW5jdGlvbigpe3JldHVybiB4dX0sc2luOmZ1bmN0aW9uKCl7cmV0dXJu
IGJ1fSxleHA6ZnVuY3Rpb24oKXtyZXR1cm4gd3V9LGNpcmNsZTpmdW5jdGlvbigp
e3JldHVybiBTdX0sZWxhc3RpYzprdSxiYWNrOkV1LGJvdW5jZTpmdW5jdGlvbigp
e3JldHVybiBBdX19KSxlbD1Wby5tYXAoeyJpbiI6d3Qsb3V0OmR1LCJpbi1vdXQi
Om11LCJvdXQtaW4iOmZ1bmN0aW9uKG4pe3JldHVybiBtdShkdShuKSl9fSk7Vm8u
ZWFzZT1mdW5jdGlvbihuKXt2YXIgdD1uLmluZGV4T2YoIi0iKSxlPXQ+PTA/bi5z
bGljZSgwLHQpOm4scj10Pj0wP24uc2xpY2UodCsxKToiaW4iO3JldHVybiBlPXRs
LmdldChlKXx8bmwscj1lbC5nZXQocil8fHd0LHZ1KHIoZS5hcHBseShudWxsLFhv
LmNhbGwoYXJndW1lbnRzLDEpKSkpfSxWby5pbnRlcnBvbGF0ZUhjbD1DdSxWby5p
bnRlcnBvbGF0ZUhzbD1OdSxWby5pbnRlcnBvbGF0ZUxhYj16dSxWby5pbnRlcnBv
bGF0ZVJvdW5kPUx1LFZvLnRyYW5zZm9ybT1mdW5jdGlvbihuKXt2YXIgdD1Cby5j
cmVhdGVFbGVtZW50TlMoVm8ubnMucHJlZml4LnN2ZywiZyIpO3JldHVybihWby50
cmFuc2Zvcm09ZnVuY3Rpb24obil7aWYobnVsbCE9bil7dC5zZXRBdHRyaWJ1dGUo
InRyYW5zZm9ybSIsbik7dmFyIGU9dC50cmFuc2Zvcm0uYmFzZVZhbC5jb25zb2xp
ZGF0ZSgpfXJldHVybiBuZXcgVHUoZT9lLm1hdHJpeDpybCl9KShuKX0sVHUucHJv
dG90eXBlLnRvU3RyaW5nPWZ1bmN0aW9uKCl7cmV0dXJuInRyYW5zbGF0ZSgiK3Ro
aXMudHJhbnNsYXRlKyIpcm90YXRlKCIrdGhpcy5yb3RhdGUrIilza2V3WCgiK3Ro
aXMuc2tldysiKXNjYWxlKCIrdGhpcy5zY2FsZSsiKSJ9O3ZhciBybD17YToxLGI6
MCxjOjAsZDoxLGU6MCxmOjB9O1ZvLmludGVycG9sYXRlVHJhbnNmb3JtPVB1LFZv
LmxheW91dD17fSxWby5sYXlvdXQuYnVuZGxlPWZ1bmN0aW9uKCl7cmV0dXJuIGZ1
bmN0aW9uKG4pe2Zvcih2YXIgdD1bXSxlPS0xLHI9bi5sZW5ndGg7KytlPHI7KXQu
cHVzaChIdShuW2VdKSk7cmV0dXJuIHR9fSxWby5sYXlvdXQuY2hvcmQ9ZnVuY3Rp
b24oKXtmdW5jdGlvbiBuKCl7dmFyIG4sbCxmLGgsZyxwPXt9LHY9W10sZD1Wby5y
YW5nZShpKSxtPVtdO2ZvcihlPVtdLHI9W10sbj0wLGg9LTE7KytoPGk7KXtmb3Io
bD0wLGc9LTE7KytnPGk7KWwrPXVbaF1bZ107di5wdXNoKGwpLG0ucHVzaChWby5y
YW5nZShpKSksbis9bH1mb3IobyYmZC5zb3J0KGZ1bmN0aW9uKG4sdCl7cmV0dXJu
IG8odltuXSx2W3RdKX0pLGEmJm0uZm9yRWFjaChmdW5jdGlvbihuLHQpe24uc29y
dChmdW5jdGlvbihuLGUpe3JldHVybiBhKHVbdF1bbl0sdVt0XVtlXSl9KX0pLG49
KFNhLXMqaSkvbixsPTAsaD0tMTsrK2g8aTspe2ZvcihmPWwsZz0tMTsrK2c8aTsp
e3ZhciB5PWRbaF0seD1tW3ldW2ddLE09dVt5XVt4XSxfPWwsYj1sKz1NKm47cFt5
KyItIit4XT17aW5kZXg6eSxzdWJpbmRleDp4LHN0YXJ0QW5nbGU6XyxlbmRBbmds
ZTpiLHZhbHVlOk19fXJbeV09e2luZGV4Onksc3RhcnRBbmdsZTpmLGVuZEFuZ2xl
OmwsdmFsdWU6KGwtZikvbn0sbCs9c31mb3IoaD0tMTsrK2g8aTspZm9yKGc9aC0x
OysrZzxpOyl7dmFyIHc9cFtoKyItIitnXSxTPXBbZysiLSIraF07KHcudmFsdWV8
fFMudmFsdWUpJiZlLnB1c2gody52YWx1ZTxTLnZhbHVlP3tzb3VyY2U6Uyx0YXJn
ZXQ6d306e3NvdXJjZTp3LHRhcmdldDpTfSl9YyYmdCgpfWZ1bmN0aW9uIHQoKXtl
LnNvcnQoZnVuY3Rpb24obix0KXtyZXR1cm4gYygobi5zb3VyY2UudmFsdWUrbi50
YXJnZXQudmFsdWUpLzIsKHQuc291cmNlLnZhbHVlK3QudGFyZ2V0LnZhbHVlKS8y
KX0pfXZhciBlLHIsdSxpLG8sYSxjLGw9e30scz0wO3JldHVybiBsLm1hdHJpeD1m
dW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaT0odT1uKSYmdS5s
ZW5ndGgsZT1yPW51bGwsbCk6dX0sbC5wYWRkaW5nPWZ1bmN0aW9uKG4pe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyhzPW4sZT1yPW51bGwsbCk6c30sbC5zb3J0R3Jv
dXBzPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhvPW4sZT1y
PW51bGwsbCk6b30sbC5zb3J0U3ViZ3JvdXBzPWZ1bmN0aW9uKG4pe3JldHVybiBh
cmd1bWVudHMubGVuZ3RoPyhhPW4sZT1udWxsLGwpOmF9LGwuc29ydENob3Jkcz1m
dW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oYz1uLGUmJnQoKSxs
KTpjfSxsLmNob3Jkcz1mdW5jdGlvbigpe3JldHVybiBlfHxuKCksZX0sbC5ncm91
cHM9ZnVuY3Rpb24oKXtyZXR1cm4gcnx8bigpLHJ9LGx9LFZvLmxheW91dC5mb3Jj
ZT1mdW5jdGlvbigpe2Z1bmN0aW9uIG4obil7cmV0dXJuIGZ1bmN0aW9uKHQsZSxy
LHUpe2lmKHQucG9pbnQhPT1uKXt2YXIgaT10LmN4LW4ueCxvPXQuY3ktbi55LGE9
dS1lLGM9aSppK28qbztpZihjPmEqYS9kKXtpZihwPmMpe3ZhciBsPXQuY2hhcmdl
L2M7bi5weC09aSpsLG4ucHktPW8qbH1yZXR1cm4hMH1pZih0LnBvaW50JiZjJiZw
PmMpe3ZhciBsPXQucG9pbnRDaGFyZ2UvYztuLnB4LT1pKmwsbi5weS09bypsfX1y
ZXR1cm4hdC5jaGFyZ2V9fWZ1bmN0aW9uIHQobil7bi5weD1Wby5ldmVudC54LG4u
cHk9Vm8uZXZlbnQueSxhLnJlc3VtZSgpfXZhciBlLHIsdSxpLG8sYT17fSxjPVZv
LmRpc3BhdGNoKCJzdGFydCIsInRpY2siLCJlbmQiKSxsPVsxLDFdLHM9LjksZj11
bCxoPWlsLGc9LTMwLHA9b2wsdj0uMSxkPS42NCxtPVtdLHk9W107cmV0dXJuIGEu
dGljaz1mdW5jdGlvbigpe2lmKChyKj0uOTkpPC4wMDUpcmV0dXJuIGMuZW5kKHt0
eXBlOiJlbmQiLGFscGhhOnI9MH0pLCEwO3ZhciB0LGUsYSxmLGgscCxkLHgsTSxf
PW0ubGVuZ3RoLGI9eS5sZW5ndGg7Zm9yKGU9MDtiPmU7KytlKWE9eVtlXSxmPWEu
c291cmNlLGg9YS50YXJnZXQseD1oLngtZi54LE09aC55LWYueSwocD14KngrTSpN
KSYmKHA9cippW2VdKigocD1NYXRoLnNxcnQocCkpLXVbZV0pL3AseCo9cCxNKj1w
LGgueC09eCooZD1mLndlaWdodC8oaC53ZWlnaHQrZi53ZWlnaHQpKSxoLnktPU0q
ZCxmLngrPXgqKGQ9MS1kKSxmLnkrPU0qZCk7aWYoKGQ9cip2KSYmKHg9bFswXS8y
LE09bFsxXS8yLGU9LTEsZCkpZm9yKDsrK2U8XzspYT1tW2VdLGEueCs9KHgtYS54
KSpkLGEueSs9KE0tYS55KSpkO2lmKGcpZm9yKFh1KHQ9Vm8uZ2VvbS5xdWFkdHJl
ZShtKSxyLG8pLGU9LTE7KytlPF87KShhPW1bZV0pLmZpeGVkfHx0LnZpc2l0KG4o
YSkpO2ZvcihlPS0xOysrZTxfOylhPW1bZV0sYS5maXhlZD8oYS54PWEucHgsYS55
PWEucHkpOihhLngtPShhLnB4LShhLnB4PWEueCkpKnMsYS55LT0oYS5weS0oYS5w
eT1hLnkpKSpzKTtjLnRpY2soe3R5cGU6InRpY2siLGFscGhhOnJ9KX0sYS5ub2Rl
cz1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obT1uLGEpOm19
LGEubGlua3M9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHk9
bixhKTp5fSxhLnNpemU9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KGw9bixhKTpsfSxhLmxpbmtEaXN0YW5jZT1mdW5jdGlvbihuKXtyZXR1cm4g
YXJndW1lbnRzLmxlbmd0aD8oZj0iZnVuY3Rpb24iPT10eXBlb2Ygbj9uOituLGEp
OmZ9LGEuZGlzdGFuY2U9YS5saW5rRGlzdGFuY2UsYS5saW5rU3RyZW5ndGg9ZnVu
Y3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGg9ImZ1bmN0aW9uIj09
dHlwZW9mIG4/bjorbixhKTpofSxhLmZyaWN0aW9uPWZ1bmN0aW9uKG4pe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyhzPStuLGEpOnN9LGEuY2hhcmdlPWZ1bmN0aW9u
KG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhnPSJmdW5jdGlvbiI9PXR5cGVv
ZiBuP246K24sYSk6Z30sYS5jaGFyZ2VEaXN0YW5jZT1mdW5jdGlvbihuKXtyZXR1
cm4gYXJndW1lbnRzLmxlbmd0aD8ocD1uKm4sYSk6TWF0aC5zcXJ0KHApfSxhLmdy
YXZpdHk9ZnVuY3Rpb24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHY9K24s
YSk6dn0sYS50aGV0YT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0
aD8oZD1uKm4sYSk6TWF0aC5zcXJ0KGQpfSxhLmFscGhhPWZ1bmN0aW9uKG4pe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyhuPStuLHI/cj1uPjA/bjowOm4+MCYmKGMu
c3RhcnQoe3R5cGU6InN0YXJ0IixhbHBoYTpyPW59KSxWby50aW1lcihhLnRpY2sp
KSxhKTpyfSxhLnN0YXJ0PWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuLHIpe2lmKCFl
KXtmb3IoZT1uZXcgQXJyYXkoYyksYT0wO2M+YTsrK2EpZVthXT1bXTtmb3IoYT0w
O2w+YTsrK2Epe3ZhciB1PXlbYV07ZVt1LnNvdXJjZS5pbmRleF0ucHVzaCh1LnRh
cmdldCksZVt1LnRhcmdldC5pbmRleF0ucHVzaCh1LnNvdXJjZSl9fWZvcih2YXIg
aSxvPWVbdF0sYT0tMSxsPW8ubGVuZ3RoOysrYTxsOylpZighaXNOYU4oaT1vW2Fd
W25dKSlyZXR1cm4gaTtyZXR1cm4gTWF0aC5yYW5kb20oKSpyfXZhciB0LGUscixj
PW0ubGVuZ3RoLHM9eS5sZW5ndGgscD1sWzBdLHY9bFsxXTtmb3IodD0wO2M+dDsr
K3QpKHI9bVt0XSkuaW5kZXg9dCxyLndlaWdodD0wO2Zvcih0PTA7cz50OysrdCly
PXlbdF0sIm51bWJlciI9PXR5cGVvZiByLnNvdXJjZSYmKHIuc291cmNlPW1bci5z
b3VyY2VdKSwibnVtYmVyIj09dHlwZW9mIHIudGFyZ2V0JiYoci50YXJnZXQ9bVty
LnRhcmdldF0pLCsrci5zb3VyY2Uud2VpZ2h0LCsrci50YXJnZXQud2VpZ2h0O2Zv
cih0PTA7Yz50OysrdClyPW1bdF0saXNOYU4oci54KSYmKHIueD1uKCJ4IixwKSks
aXNOYU4oci55KSYmKHIueT1uKCJ5Iix2KSksaXNOYU4oci5weCkmJihyLnB4PXIu
eCksaXNOYU4oci5weSkmJihyLnB5PXIueSk7aWYodT1bXSwiZnVuY3Rpb24iPT10
eXBlb2YgZilmb3IodD0wO3M+dDsrK3QpdVt0XT0rZi5jYWxsKHRoaXMseVt0XSx0
KTtlbHNlIGZvcih0PTA7cz50OysrdCl1W3RdPWY7aWYoaT1bXSwiZnVuY3Rpb24i
PT10eXBlb2YgaClmb3IodD0wO3M+dDsrK3QpaVt0XT0raC5jYWxsKHRoaXMseVt0
XSx0KTtlbHNlIGZvcih0PTA7cz50OysrdClpW3RdPWg7aWYobz1bXSwiZnVuY3Rp
b24iPT10eXBlb2YgZylmb3IodD0wO2M+dDsrK3Qpb1t0XT0rZy5jYWxsKHRoaXMs
bVt0XSx0KTtlbHNlIGZvcih0PTA7Yz50OysrdClvW3RdPWc7cmV0dXJuIGEucmVz
dW1lKCl9LGEucmVzdW1lPWZ1bmN0aW9uKCl7cmV0dXJuIGEuYWxwaGEoLjEpfSxh
LnN0b3A9ZnVuY3Rpb24oKXtyZXR1cm4gYS5hbHBoYSgwKX0sYS5kcmFnPWZ1bmN0
aW9uKCl7cmV0dXJuIGV8fChlPVZvLmJlaGF2aW9yLmRyYWcoKS5vcmlnaW4od3Qp
Lm9uKCJkcmFnc3RhcnQuZm9yY2UiLFl1KS5vbigiZHJhZy5mb3JjZSIsdCkub24o
ImRyYWdlbmQuZm9yY2UiLEl1KSksYXJndW1lbnRzLmxlbmd0aD8odGhpcy5vbigi
bW91c2VvdmVyLmZvcmNlIixadSkub24oIm1vdXNlb3V0LmZvcmNlIixWdSkuY2Fs
bChlKSx2b2lkIDApOmV9LFZvLnJlYmluZChhLGMsIm9uIil9O3ZhciB1bD0yMCxp
bD0xLG9sPTEvMDtWby5sYXlvdXQuaGllcmFyY2h5PWZ1bmN0aW9uKCl7ZnVuY3Rp
b24gbih1KXt2YXIgaSxvPVt1XSxhPVtdO2Zvcih1LmRlcHRoPTA7bnVsbCE9KGk9
by5wb3AoKSk7KWlmKGEucHVzaChpKSwobD1lLmNhbGwobixpLGkuZGVwdGgpKSYm
KGM9bC5sZW5ndGgpKXtmb3IodmFyIGMsbCxzOy0tYz49MDspby5wdXNoKHM9bFtj
XSkscy5wYXJlbnQ9aSxzLmRlcHRoPWkuZGVwdGgrMTtyJiYoaS52YWx1ZT0wKSxp
LmNoaWxkcmVuPWx9ZWxzZSByJiYoaS52YWx1ZT0rci5jYWxsKG4saSxpLmRlcHRo
KXx8MCksZGVsZXRlIGkuY2hpbGRyZW47cmV0dXJuIFd1KHUsZnVuY3Rpb24obil7
dmFyIGUsdTt0JiYoZT1uLmNoaWxkcmVuKSYmZS5zb3J0KHQpLHImJih1PW4ucGFy
ZW50KSYmKHUudmFsdWUrPW4udmFsdWUpfSksYX12YXIgdD1LdSxlPUp1LHI9R3U7
cmV0dXJuIG4uc29ydD1mdW5jdGlvbihlKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0
aD8odD1lLG4pOnR9LG4uY2hpbGRyZW49ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KGU9dCxuKTplfSxuLnZhbHVlPWZ1bmN0aW9uKHQpe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyhyPXQsbik6cn0sbi5yZXZhbHVlPWZ1bmN0aW9u
KHQpe3JldHVybiByJiYoQnUodCxmdW5jdGlvbihuKXtuLmNoaWxkcmVuJiYobi52
YWx1ZT0wKX0pLFd1KHQsZnVuY3Rpb24odCl7dmFyIGU7dC5jaGlsZHJlbnx8KHQu
dmFsdWU9K3IuY2FsbChuLHQsdC5kZXB0aCl8fDApLChlPXQucGFyZW50KSYmKGUu
dmFsdWUrPXQudmFsdWUpfSkpLHR9LG59LFZvLmxheW91dC5wYXJ0aXRpb249ZnVu
Y3Rpb24oKXtmdW5jdGlvbiBuKHQsZSxyLHUpe3ZhciBpPXQuY2hpbGRyZW47aWYo
dC54PWUsdC55PXQuZGVwdGgqdSx0LmR4PXIsdC5keT11LGkmJihvPWkubGVuZ3Ro
KSl7dmFyIG8sYSxjLGw9LTE7Zm9yKHI9dC52YWx1ZT9yL3QudmFsdWU6MDsrK2w8
bzspbihhPWlbbF0sZSxjPWEudmFsdWUqcix1KSxlKz1jfX1mdW5jdGlvbiB0KG4p
e3ZhciBlPW4uY2hpbGRyZW4scj0wO2lmKGUmJih1PWUubGVuZ3RoKSlmb3IodmFy
IHUsaT0tMTsrK2k8dTspcj1NYXRoLm1heChyLHQoZVtpXSkpO3JldHVybiAxK3J9
ZnVuY3Rpb24gZShlLGkpe3ZhciBvPXIuY2FsbCh0aGlzLGUsaSk7cmV0dXJuIG4o
b1swXSwwLHVbMF0sdVsxXS90KG9bMF0pKSxvfXZhciByPVZvLmxheW91dC5oaWVy
YXJjaHkoKSx1PVsxLDFdO3JldHVybiBlLnNpemU9ZnVuY3Rpb24obil7cmV0dXJu
IGFyZ3VtZW50cy5sZW5ndGg/KHU9bixlKTp1fSwkdShlLHIpfSxWby5sYXlvdXQu
cGllPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihpKXt2YXIgbz1pLm1hcChmdW5jdGlv
bihlLHIpe3JldHVybit0LmNhbGwobixlLHIpfSksYT0rKCJmdW5jdGlvbiI9PXR5
cGVvZiByP3IuYXBwbHkodGhpcyxhcmd1bWVudHMpOnIpLGM9KCgiZnVuY3Rpb24i
PT10eXBlb2YgdT91LmFwcGx5KHRoaXMsYXJndW1lbnRzKTp1KS1hKS9Wby5zdW0o
byksbD1Wby5yYW5nZShpLmxlbmd0aCk7bnVsbCE9ZSYmbC5zb3J0KGU9PT1hbD9m
dW5jdGlvbihuLHQpe3JldHVybiBvW3RdLW9bbl19OmZ1bmN0aW9uKG4sdCl7cmV0
dXJuIGUoaVtuXSxpW3RdKX0pO3ZhciBzPVtdO3JldHVybiBsLmZvckVhY2goZnVu
Y3Rpb24obil7dmFyIHQ7c1tuXT17ZGF0YTppW25dLHZhbHVlOnQ9b1tuXSxzdGFy
dEFuZ2xlOmEsZW5kQW5nbGU6YSs9dCpjfX0pLHN9dmFyIHQ9TnVtYmVyLGU9YWws
cj0wLHU9U2E7cmV0dXJuIG4udmFsdWU9ZnVuY3Rpb24oZSl7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KHQ9ZSxuKTp0fSxuLnNvcnQ9ZnVuY3Rpb24odCl7cmV0dXJu
IGFyZ3VtZW50cy5sZW5ndGg/KGU9dCxuKTplfSxuLnN0YXJ0QW5nbGU9ZnVuY3Rp
b24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHI9dCxuKTpyfSxuLmVuZEFu
Z2xlPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PXQsbik6
dX0sbn07dmFyIGFsPXt9O1ZvLmxheW91dC5zdGFjaz1mdW5jdGlvbigpe2Z1bmN0
aW9uIG4oYSxjKXt2YXIgbD1hLm1hcChmdW5jdGlvbihlLHIpe3JldHVybiB0LmNh
bGwobixlLHIpfSkscz1sLm1hcChmdW5jdGlvbih0KXtyZXR1cm4gdC5tYXAoZnVu
Y3Rpb24odCxlKXtyZXR1cm5baS5jYWxsKG4sdCxlKSxvLmNhbGwobix0LGUpXX0p
fSksZj1lLmNhbGwobixzLGMpO2w9Vm8ucGVybXV0ZShsLGYpLHM9Vm8ucGVybXV0
ZShzLGYpO3ZhciBoLGcscCx2PXIuY2FsbChuLHMsYyksZD1sLmxlbmd0aCxtPWxb
MF0ubGVuZ3RoO2ZvcihnPTA7bT5nOysrZylmb3IodS5jYWxsKG4sbFswXVtnXSxw
PXZbZ10sc1swXVtnXVsxXSksaD0xO2Q+aDsrK2gpdS5jYWxsKG4sbFtoXVtnXSxw
Kz1zW2gtMV1bZ11bMV0sc1toXVtnXVsxXSk7cmV0dXJuIGF9dmFyIHQ9d3QsZT1y
aSxyPXVpLHU9ZWksaT1uaSxvPXRpO3JldHVybiBuLnZhbHVlcz1mdW5jdGlvbihl
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD1lLG4pOnR9LG4ub3JkZXI9ZnVu
Y3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9ImZ1bmN0aW9uIj09
dHlwZW9mIHQ/dDpjbC5nZXQodCl8fHJpLG4pOmV9LG4ub2Zmc2V0PWZ1bmN0aW9u
KHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPSJmdW5jdGlvbiI9PXR5cGVv
ZiB0P3Q6bGwuZ2V0KHQpfHx1aSxuKTpyfSxuLng9ZnVuY3Rpb24odCl7cmV0dXJu
IGFyZ3VtZW50cy5sZW5ndGg/KGk9dCxuKTppfSxuLnk9ZnVuY3Rpb24odCl7cmV0
dXJuIGFyZ3VtZW50cy5sZW5ndGg/KG89dCxuKTpvfSxuLm91dD1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odT10LG4pOnV9LG59O3ZhciBjbD1W
by5tYXAoeyJpbnNpZGUtb3V0IjpmdW5jdGlvbihuKXt2YXIgdCxlLHI9bi5sZW5n
dGgsdT1uLm1hcChpaSksaT1uLm1hcChvaSksbz1Wby5yYW5nZShyKS5zb3J0KGZ1
bmN0aW9uKG4sdCl7cmV0dXJuIHVbbl0tdVt0XX0pLGE9MCxjPTAsbD1bXSxzPVtd
O2Zvcih0PTA7cj50OysrdCllPW9bdF0sYz5hPyhhKz1pW2VdLGwucHVzaChlKSk6
KGMrPWlbZV0scy5wdXNoKGUpKTtyZXR1cm4gcy5yZXZlcnNlKCkuY29uY2F0KGwp
fSxyZXZlcnNlOmZ1bmN0aW9uKG4pe3JldHVybiBWby5yYW5nZShuLmxlbmd0aCku
cmV2ZXJzZSgpfSwiZGVmYXVsdCI6cml9KSxsbD1Wby5tYXAoe3NpbGhvdWV0dGU6
ZnVuY3Rpb24obil7dmFyIHQsZSxyLHU9bi5sZW5ndGgsaT1uWzBdLmxlbmd0aCxv
PVtdLGE9MCxjPVtdO2ZvcihlPTA7aT5lOysrZSl7Zm9yKHQ9MCxyPTA7dT50O3Qr
KylyKz1uW3RdW2VdWzFdO3I+YSYmKGE9ciksby5wdXNoKHIpfWZvcihlPTA7aT5l
OysrZSljW2VdPShhLW9bZV0pLzI7cmV0dXJuIGN9LHdpZ2dsZTpmdW5jdGlvbihu
KXt2YXIgdCxlLHIsdSxpLG8sYSxjLGwscz1uLmxlbmd0aCxmPW5bMF0saD1mLmxl
bmd0aCxnPVtdO2ZvcihnWzBdPWM9bD0wLGU9MTtoPmU7KytlKXtmb3IodD0wLHU9
MDtzPnQ7Kyt0KXUrPW5bdF1bZV1bMV07Zm9yKHQ9MCxpPTAsYT1mW2VdWzBdLWZb
ZS0xXVswXTtzPnQ7Kyt0KXtmb3Iocj0wLG89KG5bdF1bZV1bMV0tblt0XVtlLTFd
WzFdKS8oMiphKTt0PnI7KytyKW8rPShuW3JdW2VdWzFdLW5bcl1bZS0xXVsxXSkv
YTtpKz1vKm5bdF1bZV1bMV19Z1tlXT1jLT11P2kvdSphOjAsbD5jJiYobD1jKX1m
b3IoZT0wO2g+ZTsrK2UpZ1tlXS09bDtyZXR1cm4gZ30sZXhwYW5kOmZ1bmN0aW9u
KG4pe3ZhciB0LGUscix1PW4ubGVuZ3RoLGk9blswXS5sZW5ndGgsbz0xL3UsYT1b
XTtmb3IoZT0wO2k+ZTsrK2Upe2Zvcih0PTAscj0wO3U+dDt0Kyspcis9blt0XVtl
XVsxXTtpZihyKWZvcih0PTA7dT50O3QrKyluW3RdW2VdWzFdLz1yO2Vsc2UgZm9y
KHQ9MDt1PnQ7dCsrKW5bdF1bZV1bMV09b31mb3IoZT0wO2k+ZTsrK2UpYVtlXT0w
O3JldHVybiBhfSx6ZXJvOnVpfSk7Vm8ubGF5b3V0Lmhpc3RvZ3JhbT1mdW5jdGlv
bigpe2Z1bmN0aW9uIG4obixpKXtmb3IodmFyIG8sYSxjPVtdLGw9bi5tYXAoZSx0
aGlzKSxzPXIuY2FsbCh0aGlzLGwsaSksZj11LmNhbGwodGhpcyxzLGwsaSksaT0t
MSxoPWwubGVuZ3RoLGc9Zi5sZW5ndGgtMSxwPXQ/MToxL2g7KytpPGc7KW89Y1tp
XT1bXSxvLmR4PWZbaSsxXS0oby54PWZbaV0pLG8ueT0wO2lmKGc+MClmb3IoaT0t
MTsrK2k8aDspYT1sW2ldLGE+PXNbMF0mJmE8PXNbMV0mJihvPWNbVm8uYmlzZWN0
KGYsYSwxLGcpLTFdLG8ueSs9cCxvLnB1c2gobltpXSkpO3JldHVybiBjfXZhciB0
PSEwLGU9TnVtYmVyLHI9c2ksdT1jaTtyZXR1cm4gbi52YWx1ZT1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT10LG4pOmV9LG4ucmFuZ2U9ZnVu
Y3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHI9YnQodCksbik6cn0s
bi5iaW5zPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PSJu
dW1iZXIiPT10eXBlb2YgdD9mdW5jdGlvbihuKXtyZXR1cm4gbGkobix0KX06YnQo
dCksbik6dX0sbi5mcmVxdWVuY3k9ZnVuY3Rpb24oZSl7cmV0dXJuIGFyZ3VtZW50
cy5sZW5ndGg/KHQ9ISFlLG4pOnR9LG59LFZvLmxheW91dC5wYWNrPWZ1bmN0aW9u
KCl7ZnVuY3Rpb24gbihuLGkpe3ZhciBvPWUuY2FsbCh0aGlzLG4saSksYT1vWzBd
LGM9dVswXSxsPXVbMV0scz1udWxsPT10P01hdGguc3FydDoiZnVuY3Rpb24iPT10
eXBlb2YgdD90OmZ1bmN0aW9uKCl7cmV0dXJuIHR9O2lmKGEueD1hLnk9MCxXdShh
LGZ1bmN0aW9uKG4pe24ucj0rcyhuLnZhbHVlKX0pLFd1KGEsdmkpLHIpe3ZhciBm
PXIqKHQ/MTpNYXRoLm1heCgyKmEuci9jLDIqYS5yL2wpKS8yO1d1KGEsZnVuY3Rp
b24obil7bi5yKz1mfSksV3UoYSx2aSksV3UoYSxmdW5jdGlvbihuKXtuLnItPWZ9
KX1yZXR1cm4geWkoYSxjLzIsbC8yLHQ/MToxL01hdGgubWF4KDIqYS5yL2MsMiph
LnIvbCkpLG99dmFyIHQsZT1Wby5sYXlvdXQuaGllcmFyY2h5KCkuc29ydChmaSks
cj0wLHU9WzEsMV07cmV0dXJuIG4uc2l6ZT1mdW5jdGlvbih0KXtyZXR1cm4gYXJn
dW1lbnRzLmxlbmd0aD8odT10LG4pOnV9LG4ucmFkaXVzPWZ1bmN0aW9uKGUpe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyh0PW51bGw9PWV8fCJmdW5jdGlvbiI9PXR5
cGVvZiBlP2U6K2Usbik6dH0sbi5wYWRkaW5nPWZ1bmN0aW9uKHQpe3JldHVybiBh
cmd1bWVudHMubGVuZ3RoPyhyPSt0LG4pOnJ9LCR1KG4sZSl9LFZvLmxheW91dC50
cmVlPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuLHUpe3ZhciBzPW8uY2FsbCh0aGlz
LG4sdSksZj1zWzBdLGg9dChmKTtpZihXdShoLGUpLGgucGFyZW50Lm09LWgueixC
dShoLHIpLGwpQnUoZixpKTtlbHNle3ZhciBnPWYscD1mLHY9ZjtCdShmLGZ1bmN0
aW9uKG4pe24ueDxnLngmJihnPW4pLG4ueD5wLngmJihwPW4pLG4uZGVwdGg+di5k
ZXB0aCYmKHY9bil9KTt2YXIgZD1hKGcscCkvMi1nLngsbT1jWzBdLyhwLngrYShw
LGcpLzIrZCkseT1jWzFdLyh2LmRlcHRofHwxKTtCdShmLGZ1bmN0aW9uKG4pe24u
eD0obi54K2QpKm0sbi55PW4uZGVwdGgqeX0pfXJldHVybiBzfWZ1bmN0aW9uIHQo
bil7Zm9yKHZhciB0LGU9e0E6bnVsbCxjaGlsZHJlbjpbbl19LHI9W2VdO251bGwh
PSh0PXIucG9wKCkpOylmb3IodmFyIHUsaT10LmNoaWxkcmVuLG89MCxhPWkubGVu
Z3RoO2E+bzsrK28pci5wdXNoKChpW29dPXU9e186aVtvXSxwYXJlbnQ6dCxjaGls
ZHJlbjoodT1pW29dLmNoaWxkcmVuKSYmdS5zbGljZSgpfHxbXSxBOm51bGwsYTpu
dWxsLHo6MCxtOjAsYzowLHM6MCx0Om51bGwsaTpvfSkuYT11KTtyZXR1cm4gZS5j
aGlsZHJlblswXX1mdW5jdGlvbiBlKG4pe3ZhciB0PW4uY2hpbGRyZW4sZT1uLnBh
cmVudC5jaGlsZHJlbixyPW4uaT9lW24uaS0xXTpudWxsO2lmKHQubGVuZ3RoKXtT
aShuKTt2YXIgaT0odFswXS56K3RbdC5sZW5ndGgtMV0ueikvMjtyPyhuLno9ci56
K2Eobi5fLHIuXyksbi5tPW4uei1pKTpuLno9aX1lbHNlIHImJihuLno9ci56K2Eo
bi5fLHIuXykpO24ucGFyZW50LkE9dShuLHIsbi5wYXJlbnQuQXx8ZVswXSl9ZnVu
Y3Rpb24gcihuKXtuLl8ueD1uLnorbi5wYXJlbnQubSxuLm0rPW4ucGFyZW50Lm19
ZnVuY3Rpb24gdShuLHQsZSl7aWYodCl7Zm9yKHZhciByLHU9bixpPW4sbz10LGM9
dS5wYXJlbnQuY2hpbGRyZW5bMF0sbD11Lm0scz1pLm0sZj1vLm0saD1jLm07bz1i
aShvKSx1PV9pKHUpLG8mJnU7KWM9X2koYyksaT1iaShpKSxpLmE9bixyPW8ueitm
LXUuei1sK2Eoby5fLHUuXykscj4wJiYod2koa2kobyxuLGUpLG4sciksbCs9cixz
Kz1yKSxmKz1vLm0sbCs9dS5tLGgrPWMubSxzKz1pLm07byYmIWJpKGkpJiYoaS50
PW8saS5tKz1mLXMpLHUmJiFfaShjKSYmKGMudD11LGMubSs9bC1oLGU9bil9cmV0
dXJuIGV9ZnVuY3Rpb24gaShuKXtuLngqPWNbMF0sbi55PW4uZGVwdGgqY1sxXX12
YXIgbz1Wby5sYXlvdXQuaGllcmFyY2h5KCkuc29ydChudWxsKS52YWx1ZShudWxs
KSxhPU1pLGM9WzEsMV0sbD1udWxsO3JldHVybiBuLnNlcGFyYXRpb249ZnVuY3Rp
b24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGE9dCxuKTphfSxuLnNpemU9
ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGw9bnVsbD09KGM9
dCk/aTpudWxsLG4pOmw/bnVsbDpjfSxuLm5vZGVTaXplPWZ1bmN0aW9uKHQpe3Jl
dHVybiBhcmd1bWVudHMubGVuZ3RoPyhsPW51bGw9PShjPXQpP251bGw6aSxuKTps
P2M6bnVsbH0sJHUobixvKX0sVm8ubGF5b3V0LmNsdXN0ZXI9ZnVuY3Rpb24oKXtm
dW5jdGlvbiBuKG4saSl7dmFyIG8sYT10LmNhbGwodGhpcyxuLGkpLGM9YVswXSxs
PTA7V3UoYyxmdW5jdGlvbihuKXt2YXIgdD1uLmNoaWxkcmVuO3QmJnQubGVuZ3Ro
PyhuLng9QWkodCksbi55PUVpKHQpKToobi54PW8/bCs9ZShuLG8pOjAsbi55PTAs
bz1uKX0pO3ZhciBzPUNpKGMpLGY9TmkoYyksaD1zLngtZShzLGYpLzIsZz1mLngr
ZShmLHMpLzI7cmV0dXJuIFd1KGMsdT9mdW5jdGlvbihuKXtuLng9KG4ueC1jLngp
KnJbMF0sbi55PShjLnktbi55KSpyWzFdfTpmdW5jdGlvbihuKXtuLng9KG4ueC1o
KS8oZy1oKSpyWzBdLG4ueT0oMS0oYy55P24ueS9jLnk6MSkpKnJbMV19KSxhfXZh
ciB0PVZvLmxheW91dC5oaWVyYXJjaHkoKS5zb3J0KG51bGwpLnZhbHVlKG51bGwp
LGU9TWkscj1bMSwxXSx1PSExO3JldHVybiBuLnNlcGFyYXRpb249ZnVuY3Rpb24o
dCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9dCxuKTplfSxuLnNpemU9ZnVu
Y3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KHU9bnVsbD09KHI9dCks
bik6dT9udWxsOnJ9LG4ubm9kZVNpemU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KHU9bnVsbCE9KHI9dCksbik6dT9yOm51bGx9LCR1KG4sdCl9
LFZvLmxheW91dC50cmVlbWFwPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuLHQpe2Zv
cih2YXIgZSxyLHU9LTEsaT1uLmxlbmd0aDsrK3U8aTspcj0oZT1uW3VdKS52YWx1
ZSooMD50PzA6dCksZS5hcmVhPWlzTmFOKHIpfHwwPj1yPzA6cn1mdW5jdGlvbiB0
KGUpe3ZhciBpPWUuY2hpbGRyZW47aWYoaSYmaS5sZW5ndGgpe3ZhciBvLGEsYyxs
PWYoZSkscz1bXSxoPWkuc2xpY2UoKSxwPTEvMCx2PSJzbGljZSI9PT1nP2wuZHg6
ImRpY2UiPT09Zz9sLmR5OiJzbGljZS1kaWNlIj09PWc/MSZlLmRlcHRoP2wuZHk6
bC5keDpNYXRoLm1pbihsLmR4LGwuZHkpO2ZvcihuKGgsbC5keCpsLmR5L2UudmFs
dWUpLHMuYXJlYT0wOyhjPWgubGVuZ3RoKT4wOylzLnB1c2gobz1oW2MtMV0pLHMu
YXJlYSs9by5hcmVhLCJzcXVhcmlmeSIhPT1nfHwoYT1yKHMsdikpPD1wPyhoLnBv
cCgpLHA9YSk6KHMuYXJlYS09cy5wb3AoKS5hcmVhLHUocyx2LGwsITEpLHY9TWF0
aC5taW4obC5keCxsLmR5KSxzLmxlbmd0aD1zLmFyZWE9MCxwPTEvMCk7cy5sZW5n
dGgmJih1KHMsdixsLCEwKSxzLmxlbmd0aD1zLmFyZWE9MCksaS5mb3JFYWNoKHQp
fX1mdW5jdGlvbiBlKHQpe3ZhciByPXQuY2hpbGRyZW47aWYociYmci5sZW5ndGgp
e3ZhciBpLG89Zih0KSxhPXIuc2xpY2UoKSxjPVtdO2ZvcihuKGEsby5keCpvLmR5
L3QudmFsdWUpLGMuYXJlYT0wO2k9YS5wb3AoKTspYy5wdXNoKGkpLGMuYXJlYSs9
aS5hcmVhLG51bGwhPWkueiYmKHUoYyxpLno/by5keDpvLmR5LG8sIWEubGVuZ3Ro
KSxjLmxlbmd0aD1jLmFyZWE9MCk7ci5mb3JFYWNoKGUpfX1mdW5jdGlvbiByKG4s
dCl7Zm9yKHZhciBlLHI9bi5hcmVhLHU9MCxpPTEvMCxvPS0xLGE9bi5sZW5ndGg7
KytvPGE7KShlPW5bb10uYXJlYSkmJihpPmUmJihpPWUpLGU+dSYmKHU9ZSkpO3Jl
dHVybiByKj1yLHQqPXQscj9NYXRoLm1heCh0KnUqcC9yLHIvKHQqaSpwKSk6MS8w
fWZ1bmN0aW9uIHUobix0LGUscil7dmFyIHUsaT0tMSxvPW4ubGVuZ3RoLGE9ZS54
LGw9ZS55LHM9dD9jKG4uYXJlYS90KTowO2lmKHQ9PWUuZHgpe2Zvcigocnx8cz5l
LmR5KSYmKHM9ZS5keSk7KytpPG87KXU9bltpXSx1Lng9YSx1Lnk9bCx1LmR5PXMs
YSs9dS5keD1NYXRoLm1pbihlLngrZS5keC1hLHM/Yyh1LmFyZWEvcyk6MCk7dS56
PSEwLHUuZHgrPWUueCtlLmR4LWEsZS55Kz1zLGUuZHktPXN9ZWxzZXtmb3IoKHJ8
fHM+ZS5keCkmJihzPWUuZHgpOysraTxvOyl1PW5baV0sdS54PWEsdS55PWwsdS5k
eD1zLGwrPXUuZHk9TWF0aC5taW4oZS55K2UuZHktbCxzP2ModS5hcmVhL3MpOjAp
O3Uuej0hMSx1LmR5Kz1lLnkrZS5keS1sLGUueCs9cyxlLmR4LT1zfX1mdW5jdGlv
biBpKHIpe3ZhciB1PW98fGEociksaT11WzBdO3JldHVybiBpLng9MCxpLnk9MCxp
LmR4PWxbMF0saS5keT1sWzFdLG8mJmEucmV2YWx1ZShpKSxuKFtpXSxpLmR4Kmku
ZHkvaS52YWx1ZSksKG8/ZTp0KShpKSxoJiYobz11KSx1fXZhciBvLGE9Vm8ubGF5
b3V0LmhpZXJhcmNoeSgpLGM9TWF0aC5yb3VuZCxsPVsxLDFdLHM9bnVsbCxmPXpp
LGg9ITEsZz0ic3F1YXJpZnkiLHA9LjUqKDErTWF0aC5zcXJ0KDUpKTtyZXR1cm4g
aS5zaXplPWZ1bmN0aW9uKG4pe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhsPW4s
aSk6bH0saS5wYWRkaW5nPWZ1bmN0aW9uKG4pe2Z1bmN0aW9uIHQodCl7dmFyIGU9
bi5jYWxsKGksdCx0LmRlcHRoKTtyZXR1cm4gbnVsbD09ZT96aSh0KTpMaSh0LCJu
dW1iZXIiPT10eXBlb2YgZT9bZSxlLGUsZV06ZSl9ZnVuY3Rpb24gZSh0KXtyZXR1
cm4gTGkodCxuKX1pZighYXJndW1lbnRzLmxlbmd0aClyZXR1cm4gczt2YXIgcjty
ZXR1cm4gZj1udWxsPT0ocz1uKT96aToiZnVuY3Rpb24iPT0ocj10eXBlb2Ygbik/
dDoibnVtYmVyIj09PXI/KG49W24sbixuLG5dLGUpOmUsaX0saS5yb3VuZD1mdW5j
dGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oYz1uP01hdGgucm91bmQ6
TnVtYmVyLGkpOmMhPU51bWJlcn0saS5zdGlja3k9ZnVuY3Rpb24obil7cmV0dXJu
IGFyZ3VtZW50cy5sZW5ndGg/KGg9bixvPW51bGwsaSk6aH0saS5yYXRpbz1mdW5j
dGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8ocD1uLGkpOnB9LGkubW9k
ZT1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZz1uKyIiLGkp
Omd9LCR1KGksYSl9LFZvLnJhbmRvbT17bm9ybWFsOmZ1bmN0aW9uKG4sdCl7dmFy
IGU9YXJndW1lbnRzLmxlbmd0aDtyZXR1cm4gMj5lJiYodD0xKSwxPmUmJihuPTAp
LGZ1bmN0aW9uKCl7dmFyIGUscix1O2RvIGU9MipNYXRoLnJhbmRvbSgpLTEscj0y
Kk1hdGgucmFuZG9tKCktMSx1PWUqZStyKnI7d2hpbGUoIXV8fHU+MSk7cmV0dXJu
IG4rdCplKk1hdGguc3FydCgtMipNYXRoLmxvZyh1KS91KX19LGxvZ05vcm1hbDpm
dW5jdGlvbigpe3ZhciBuPVZvLnJhbmRvbS5ub3JtYWwuYXBwbHkoVm8sYXJndW1l
bnRzKTtyZXR1cm4gZnVuY3Rpb24oKXtyZXR1cm4gTWF0aC5leHAobigpKX19LGJh
dGVzOmZ1bmN0aW9uKG4pe3ZhciB0PVZvLnJhbmRvbS5pcndpbkhhbGwobik7cmV0
dXJuIGZ1bmN0aW9uKCl7cmV0dXJuIHQoKS9ufX0saXJ3aW5IYWxsOmZ1bmN0aW9u
KG4pe3JldHVybiBmdW5jdGlvbigpe2Zvcih2YXIgdD0wLGU9MDtuPmU7ZSsrKXQr
PU1hdGgucmFuZG9tKCk7cmV0dXJuIHR9fX0sVm8uc2NhbGU9e307dmFyIHNsPXtm
bG9vcjp3dCxjZWlsOnd0fTtWby5zY2FsZS5saW5lYXI9ZnVuY3Rpb24oKXtyZXR1
cm4gamkoWzAsMV0sWzAsMV0sZ3UsITEpfTt2YXIgZmw9e3M6MSxnOjEscDoxLHI6
MSxlOjF9O1ZvLnNjYWxlLmxvZz1mdW5jdGlvbigpe3JldHVybiBYaShWby5zY2Fs
ZS5saW5lYXIoKS5kb21haW4oWzAsMV0pLDEwLCEwLFsxLDEwXSl9O3ZhciBobD1W
by5mb3JtYXQoIi4wZSIpLGdsPXtmbG9vcjpmdW5jdGlvbihuKXtyZXR1cm4tTWF0
aC5jZWlsKC1uKX0sY2VpbDpmdW5jdGlvbihuKXtyZXR1cm4tTWF0aC5mbG9vcigt
bil9fTtWby5zY2FsZS5wb3c9ZnVuY3Rpb24oKXtyZXR1cm4gJGkoVm8uc2NhbGUu
bGluZWFyKCksMSxbMCwxXSl9LFZvLnNjYWxlLnNxcnQ9ZnVuY3Rpb24oKXtyZXR1
cm4gVm8uc2NhbGUucG93KCkuZXhwb25lbnQoLjUpfSxWby5zY2FsZS5vcmRpbmFs
PWZ1bmN0aW9uKCl7cmV0dXJuIFdpKFtdLHt0OiJyYW5nZSIsYTpbW11dfSl9LFZv
LnNjYWxlLmNhdGVnb3J5MTA9ZnVuY3Rpb24oKXtyZXR1cm4gVm8uc2NhbGUub3Jk
aW5hbCgpLnJhbmdlKHBsKX0sVm8uc2NhbGUuY2F0ZWdvcnkyMD1mdW5jdGlvbigp
e3JldHVybiBWby5zY2FsZS5vcmRpbmFsKCkucmFuZ2UodmwpfSxWby5zY2FsZS5j
YXRlZ29yeTIwYj1mdW5jdGlvbigpe3JldHVybiBWby5zY2FsZS5vcmRpbmFsKCku
cmFuZ2UoZGwpfSxWby5zY2FsZS5jYXRlZ29yeTIwYz1mdW5jdGlvbigpe3JldHVy
biBWby5zY2FsZS5vcmRpbmFsKCkucmFuZ2UobWwpfTt2YXIgcGw9WzIwNjIyNjAs
MTY3NDQyMDYsMjkyNDU4OCwxNDAzNDcyOCw5NzI1ODg1LDkxOTcxMzEsMTQ5MDcz
MzAsODM1NTcxMSwxMjM2OTE4NiwxNTU2MTc1XS5tYXAodnQpLHZsPVsyMDYyMjYw
LDExNDU0NDQwLDE2NzQ0MjA2LDE2NzU5NjcyLDI5MjQ1ODgsMTAwMTg2OTgsMTQw
MzQ3MjgsMTY3NTA3NDIsOTcyNTg4NSwxMjk1NTg2MSw5MTk3MTMxLDEyODg1MTQw
LDE0OTA3MzMwLDE2MjM0MTk0LDgzNTU3MTEsMTMwOTI4MDcsMTIzNjkxODYsMTQ0
MDg1ODksMTU1NjE3NSwxMDQxMDcyNV0ubWFwKHZ0KSxkbD1bMzc1MDc3Nyw1Mzk1
NjE5LDcwNDA3MTksMTAyNjQyODYsNjUxOTA5Nyw5MjE2NTk0LDExOTE1MTE1LDEz
NTU2NjM2LDkyMDI5OTMsMTI0MjY4MDksMTUxODY1MTQsMTUxOTA5MzIsODY2NjE2
OSwxMTM1NjQ5MCwxNDA0OTY0MywxNTE3NzM3Miw4MDc3NjgzLDEwODM0MzI0LDEz
NTI4NTA5LDE0NTg5NjU0XS5tYXAodnQpLG1sPVszMjQ0NzMzLDcwNTcxMTAsMTA0
MDY2MjUsMTMwMzI0MzEsMTUwOTUwNTMsMTY2MTY3NjQsMTY2MjUyNTksMTY2MzQw
MTgsMzI1MzA3Niw3NjUyNDcwLDEwNjA3MDAzLDEzMTAxNTA0LDc2OTUyODEsMTAz
OTQzMTIsMTIzNjkzNzIsMTQzNDI4OTEsNjUxMzUwNyw5ODY4OTUwLDEyNDM0ODc3
LDE0Mjc3MDgxXS5tYXAodnQpO1ZvLnNjYWxlLnF1YW50aWxlPWZ1bmN0aW9uKCl7
cmV0dXJuIEppKFtdLFtdKX0sVm8uc2NhbGUucXVhbnRpemU9ZnVuY3Rpb24oKXty
ZXR1cm4gR2koMCwxLFswLDFdKX0sVm8uc2NhbGUudGhyZXNob2xkPWZ1bmN0aW9u
KCl7cmV0dXJuIEtpKFsuNV0sWzAsMV0pfSxWby5zY2FsZS5pZGVudGl0eT1mdW5j
dGlvbigpe3JldHVybiBRaShbMCwxXSl9LFZvLnN2Zz17fSxWby5zdmcuYXJjPWZ1
bmN0aW9uKCl7ZnVuY3Rpb24gbigpe3ZhciBuPXQuYXBwbHkodGhpcyxhcmd1bWVu
dHMpLGk9ZS5hcHBseSh0aGlzLGFyZ3VtZW50cyksbz1yLmFwcGx5KHRoaXMsYXJn
dW1lbnRzKSt5bCxhPXUuYXBwbHkodGhpcyxhcmd1bWVudHMpK3lsLGM9KG8+YSYm
KGM9byxvPWEsYT1jKSxhLW8pLGw9d2E+Yz8iMCI6IjEiLHM9TWF0aC5jb3Mobyks
Zj1NYXRoLnNpbihvKSxoPU1hdGguY29zKGEpLGc9TWF0aC5zaW4oYSk7CnJldHVy
biBjPj14bD9uPyJNMCwiK2krIkEiK2krIiwiK2krIiAwIDEsMSAwLCIrLWkrIkEi
K2krIiwiK2krIiAwIDEsMSAwLCIraSsiTTAsIituKyJBIituKyIsIituKyIgMCAx
LDAgMCwiKy1uKyJBIituKyIsIituKyIgMCAxLDAgMCwiK24rIloiOiJNMCwiK2kr
IkEiK2krIiwiK2krIiAwIDEsMSAwLCIrLWkrIkEiK2krIiwiK2krIiAwIDEsMSAw
LCIraSsiWiI6bj8iTSIraSpzKyIsIitpKmYrIkEiK2krIiwiK2krIiAwICIrbCsi
LDEgIitpKmgrIiwiK2kqZysiTCIrbipoKyIsIituKmcrIkEiK24rIiwiK24rIiAw
ICIrbCsiLDAgIituKnMrIiwiK24qZisiWiI6Ik0iK2kqcysiLCIraSpmKyJBIitp
KyIsIitpKyIgMCAiK2wrIiwxICIraSpoKyIsIitpKmcrIkwwLDAiKyJaIn12YXIg
dD1ubyxlPXRvLHI9ZW8sdT1ybztyZXR1cm4gbi5pbm5lclJhZGl1cz1mdW5jdGlv
bihlKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD1idChlKSxuKTp0fSxuLm91
dGVyUmFkaXVzPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhl
PWJ0KHQpLG4pOmV9LG4uc3RhcnRBbmdsZT1mdW5jdGlvbih0KXtyZXR1cm4gYXJn
dW1lbnRzLmxlbmd0aD8ocj1idCh0KSxuKTpyfSxuLmVuZEFuZ2xlPWZ1bmN0aW9u
KHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyh1PWJ0KHQpLG4pOnV9LG4uY2Vu
dHJvaWQ9ZnVuY3Rpb24oKXt2YXIgbj0odC5hcHBseSh0aGlzLGFyZ3VtZW50cykr
ZS5hcHBseSh0aGlzLGFyZ3VtZW50cykpLzIsaT0oci5hcHBseSh0aGlzLGFyZ3Vt
ZW50cykrdS5hcHBseSh0aGlzLGFyZ3VtZW50cykpLzIreWw7cmV0dXJuW01hdGgu
Y29zKGkpKm4sTWF0aC5zaW4oaSkqbl19LG59O3ZhciB5bD0ta2EseGw9U2EtRWE7
Vm8uc3ZnLmxpbmU9ZnVuY3Rpb24oKXtyZXR1cm4gdW8od3QpfTt2YXIgTWw9Vm8u
bWFwKHtsaW5lYXI6aW8sImxpbmVhci1jbG9zZWQiOm9vLHN0ZXA6YW8sInN0ZXAt
YmVmb3JlIjpjbywic3RlcC1hZnRlciI6bG8sYmFzaXM6dm8sImJhc2lzLW9wZW4i
Om1vLCJiYXNpcy1jbG9zZWQiOnlvLGJ1bmRsZTp4byxjYXJkaW5hbDpobywiY2Fy
ZGluYWwtb3BlbiI6c28sImNhcmRpbmFsLWNsb3NlZCI6Zm8sbW9ub3RvbmU6a299
KTtNbC5mb3JFYWNoKGZ1bmN0aW9uKG4sdCl7dC5rZXk9bix0LmNsb3NlZD0vLWNs
b3NlZCQvLnRlc3Qobil9KTt2YXIgX2w9WzAsMi8zLDEvMywwXSxibD1bMCwxLzMs
Mi8zLDBdLHdsPVswLDEvNiwyLzMsMS82XTtWby5zdmcubGluZS5yYWRpYWw9ZnVu
Y3Rpb24oKXt2YXIgbj11byhFbyk7cmV0dXJuIG4ucmFkaXVzPW4ueCxkZWxldGUg
bi54LG4uYW5nbGU9bi55LGRlbGV0ZSBuLnksbn0sY28ucmV2ZXJzZT1sbyxsby5y
ZXZlcnNlPWNvLFZvLnN2Zy5hcmVhPWZ1bmN0aW9uKCl7cmV0dXJuIEFvKHd0KX0s
Vm8uc3ZnLmFyZWEucmFkaWFsPWZ1bmN0aW9uKCl7dmFyIG49QW8oRW8pO3JldHVy
biBuLnJhZGl1cz1uLngsZGVsZXRlIG4ueCxuLmlubmVyUmFkaXVzPW4ueDAsZGVs
ZXRlIG4ueDAsbi5vdXRlclJhZGl1cz1uLngxLGRlbGV0ZSBuLngxLG4uYW5nbGU9
bi55LGRlbGV0ZSBuLnksbi5zdGFydEFuZ2xlPW4ueTAsZGVsZXRlIG4ueTAsbi5l
bmRBbmdsZT1uLnkxLGRlbGV0ZSBuLnkxLG59LFZvLnN2Zy5jaG9yZD1mdW5jdGlv
bigpe2Z1bmN0aW9uIG4obixhKXt2YXIgYz10KHRoaXMsaSxuLGEpLGw9dCh0aGlz
LG8sbixhKTtyZXR1cm4iTSIrYy5wMCtyKGMucixjLnAxLGMuYTEtYy5hMCkrKGUo
YyxsKT91KGMucixjLnAxLGMucixjLnAwKTp1KGMucixjLnAxLGwucixsLnAwKSty
KGwucixsLnAxLGwuYTEtbC5hMCkrdShsLnIsbC5wMSxjLnIsYy5wMCkpKyJaIn1m
dW5jdGlvbiB0KG4sdCxlLHIpe3ZhciB1PXQuY2FsbChuLGUsciksaT1hLmNhbGwo
bix1LHIpLG89Yy5jYWxsKG4sdSxyKSt5bCxzPWwuY2FsbChuLHUscikreWw7cmV0
dXJue3I6aSxhMDpvLGExOnMscDA6W2kqTWF0aC5jb3MobyksaSpNYXRoLnNpbihv
KV0scDE6W2kqTWF0aC5jb3MocyksaSpNYXRoLnNpbihzKV19fWZ1bmN0aW9uIGUo
bix0KXtyZXR1cm4gbi5hMD09dC5hMCYmbi5hMT09dC5hMX1mdW5jdGlvbiByKG4s
dCxlKXtyZXR1cm4iQSIrbisiLCIrbisiIDAgIisgKyhlPndhKSsiLDEgIit0fWZ1
bmN0aW9uIHUobix0LGUscil7cmV0dXJuIlEgMCwwICIrcn12YXIgaT1wcixvPXZy
LGE9Q28sYz1lbyxsPXJvO3JldHVybiBuLnJhZGl1cz1mdW5jdGlvbih0KXtyZXR1
cm4gYXJndW1lbnRzLmxlbmd0aD8oYT1idCh0KSxuKTphfSxuLnNvdXJjZT1mdW5j
dGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oaT1idCh0KSxuKTppfSxu
LnRhcmdldD1mdW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obz1i
dCh0KSxuKTpvfSxuLnN0YXJ0QW5nbGU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KGM9YnQodCksbik6Y30sbi5lbmRBbmdsZT1mdW5jdGlvbih0
KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8obD1idCh0KSxuKTpsfSxufSxWby5z
dmcuZGlhZ29uYWw9ZnVuY3Rpb24oKXtmdW5jdGlvbiBuKG4sdSl7dmFyIGk9dC5j
YWxsKHRoaXMsbix1KSxvPWUuY2FsbCh0aGlzLG4sdSksYT0oaS55K28ueSkvMixj
PVtpLHt4OmkueCx5OmF9LHt4Om8ueCx5OmF9LG9dO3JldHVybiBjPWMubWFwKHIp
LCJNIitjWzBdKyJDIitjWzFdKyIgIitjWzJdKyIgIitjWzNdfXZhciB0PXByLGU9
dnIscj1ObztyZXR1cm4gbi5zb3VyY2U9ZnVuY3Rpb24oZSl7cmV0dXJuIGFyZ3Vt
ZW50cy5sZW5ndGg/KHQ9YnQoZSksbik6dH0sbi50YXJnZXQ9ZnVuY3Rpb24odCl7
cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9YnQodCksbik6ZX0sbi5wcm9qZWN0
aW9uPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPXQsbik6
cn0sbn0sVm8uc3ZnLmRpYWdvbmFsLnJhZGlhbD1mdW5jdGlvbigpe3ZhciBuPVZv
LnN2Zy5kaWFnb25hbCgpLHQ9Tm8sZT1uLnByb2plY3Rpb247cmV0dXJuIG4ucHJv
amVjdGlvbj1mdW5jdGlvbihuKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD9lKHpv
KHQ9bikpOnR9LG59LFZvLnN2Zy5zeW1ib2w9ZnVuY3Rpb24oKXtmdW5jdGlvbiBu
KG4scil7cmV0dXJuKFNsLmdldCh0LmNhbGwodGhpcyxuLHIpKXx8cW8pKGUuY2Fs
bCh0aGlzLG4scikpfXZhciB0PVRvLGU9TG87cmV0dXJuIG4udHlwZT1mdW5jdGlv
bihlKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8odD1idChlKSxuKTp0fSxuLnNp
emU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGU9YnQodCks
bik6ZX0sbn07dmFyIFNsPVZvLm1hcCh7Y2lyY2xlOnFvLGNyb3NzOmZ1bmN0aW9u
KG4pe3ZhciB0PU1hdGguc3FydChuLzUpLzI7cmV0dXJuIk0iKy0zKnQrIiwiKy10
KyJIIistdCsiViIrLTMqdCsiSCIrdCsiViIrLXQrIkgiKzMqdCsiViIrdCsiSCIr
dCsiViIrMyp0KyJIIistdCsiViIrdCsiSCIrLTMqdCsiWiJ9LGRpYW1vbmQ6ZnVu
Y3Rpb24obil7dmFyIHQ9TWF0aC5zcXJ0KG4vKDIqQ2wpKSxlPXQqQ2w7cmV0dXJu
Ik0wLCIrLXQrIkwiK2UrIiwwIisiIDAsIit0KyIgIistZSsiLDAiKyJaIn0sc3F1
YXJlOmZ1bmN0aW9uKG4pe3ZhciB0PU1hdGguc3FydChuKS8yO3JldHVybiJNIist
dCsiLCIrLXQrIkwiK3QrIiwiKy10KyIgIit0KyIsIit0KyIgIistdCsiLCIrdCsi
WiJ9LCJ0cmlhbmdsZS1kb3duIjpmdW5jdGlvbihuKXt2YXIgdD1NYXRoLnNxcnQo
bi9BbCksZT10KkFsLzI7cmV0dXJuIk0wLCIrZSsiTCIrdCsiLCIrLWUrIiAiKy10
KyIsIistZSsiWiJ9LCJ0cmlhbmdsZS11cCI6ZnVuY3Rpb24obil7dmFyIHQ9TWF0
aC5zcXJ0KG4vQWwpLGU9dCpBbC8yO3JldHVybiJNMCwiKy1lKyJMIit0KyIsIitl
KyIgIistdCsiLCIrZSsiWiJ9fSk7Vm8uc3ZnLnN5bWJvbFR5cGVzPVNsLmtleXMo
KTt2YXIga2wsRWwsQWw9TWF0aC5zcXJ0KDMpLENsPU1hdGgudGFuKDMwKkNhKSxO
bD1bXSx6bD0wO05sLmNhbGw9dmEuY2FsbCxObC5lbXB0eT12YS5lbXB0eSxObC5u
b2RlPXZhLm5vZGUsTmwuc2l6ZT12YS5zaXplLFZvLnRyYW5zaXRpb249ZnVuY3Rp
b24obil7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/a2w/bi50cmFuc2l0aW9uKCk6
bjp5YS50cmFuc2l0aW9uKCl9LFZvLnRyYW5zaXRpb24ucHJvdG90eXBlPU5sLE5s
LnNlbGVjdD1mdW5jdGlvbihuKXt2YXIgdCxlLHIsdT10aGlzLmlkLGk9W107bj1i
KG4pO2Zvcih2YXIgbz0tMSxhPXRoaXMubGVuZ3RoOysrbzxhOyl7aS5wdXNoKHQ9
W10pO2Zvcih2YXIgYz10aGlzW29dLGw9LTEscz1jLmxlbmd0aDsrK2w8czspKHI9
Y1tsXSkmJihlPW4uY2FsbChyLHIuX19kYXRhX18sbCxvKSk/KCJfX2RhdGFfXyJp
biByJiYoZS5fX2RhdGFfXz1yLl9fZGF0YV9fKSxVbyhlLGwsdSxyLl9fdHJhbnNp
dGlvbl9fW3VdKSx0LnB1c2goZSkpOnQucHVzaChudWxsKX1yZXR1cm4gUm8oaSx1
KX0sTmwuc2VsZWN0QWxsPWZ1bmN0aW9uKG4pe3ZhciB0LGUscix1LGksbz10aGlz
LmlkLGE9W107bj13KG4pO2Zvcih2YXIgYz0tMSxsPXRoaXMubGVuZ3RoOysrYzxs
Oylmb3IodmFyIHM9dGhpc1tjXSxmPS0xLGg9cy5sZW5ndGg7KytmPGg7KWlmKHI9
c1tmXSl7aT1yLl9fdHJhbnNpdGlvbl9fW29dLGU9bi5jYWxsKHIsci5fX2RhdGFf
XyxmLGMpLGEucHVzaCh0PVtdKTtmb3IodmFyIGc9LTEscD1lLmxlbmd0aDsrK2c8
cDspKHU9ZVtnXSkmJlVvKHUsZyxvLGkpLHQucHVzaCh1KX1yZXR1cm4gUm8oYSxv
KX0sTmwuZmlsdGVyPWZ1bmN0aW9uKG4pe3ZhciB0LGUscix1PVtdOyJmdW5jdGlv
biIhPXR5cGVvZiBuJiYobj1SKG4pKTtmb3IodmFyIGk9MCxvPXRoaXMubGVuZ3Ro
O28+aTtpKyspe3UucHVzaCh0PVtdKTtmb3IodmFyIGU9dGhpc1tpXSxhPTAsYz1l
Lmxlbmd0aDtjPmE7YSsrKShyPWVbYV0pJiZuLmNhbGwocixyLl9fZGF0YV9fLGEs
aSkmJnQucHVzaChyKX1yZXR1cm4gUm8odSx0aGlzLmlkKX0sTmwudHdlZW49ZnVu
Y3Rpb24obix0KXt2YXIgZT10aGlzLmlkO3JldHVybiBhcmd1bWVudHMubGVuZ3Ro
PDI/dGhpcy5ub2RlKCkuX190cmFuc2l0aW9uX19bZV0udHdlZW4uZ2V0KG4pOlAo
dGhpcyxudWxsPT10P2Z1bmN0aW9uKHQpe3QuX190cmFuc2l0aW9uX19bZV0udHdl
ZW4ucmVtb3ZlKG4pfTpmdW5jdGlvbihyKXtyLl9fdHJhbnNpdGlvbl9fW2VdLnR3
ZWVuLnNldChuLHQpfSl9LE5sLmF0dHI9ZnVuY3Rpb24obix0KXtmdW5jdGlvbiBl
KCl7dGhpcy5yZW1vdmVBdHRyaWJ1dGUoYSl9ZnVuY3Rpb24gcigpe3RoaXMucmVt
b3ZlQXR0cmlidXRlTlMoYS5zcGFjZSxhLmxvY2FsKX1mdW5jdGlvbiB1KG4pe3Jl
dHVybiBudWxsPT1uP2U6KG4rPSIiLGZ1bmN0aW9uKCl7dmFyIHQsZT10aGlzLmdl
dEF0dHJpYnV0ZShhKTtyZXR1cm4gZSE9PW4mJih0PW8oZSxuKSxmdW5jdGlvbihu
KXt0aGlzLnNldEF0dHJpYnV0ZShhLHQobikpfSl9KX1mdW5jdGlvbiBpKG4pe3Jl
dHVybiBudWxsPT1uP3I6KG4rPSIiLGZ1bmN0aW9uKCl7dmFyIHQsZT10aGlzLmdl
dEF0dHJpYnV0ZU5TKGEuc3BhY2UsYS5sb2NhbCk7cmV0dXJuIGUhPT1uJiYodD1v
KGUsbiksZnVuY3Rpb24obil7dGhpcy5zZXRBdHRyaWJ1dGVOUyhhLnNwYWNlLGEu
bG9jYWwsdChuKSl9KX0pfWlmKGFyZ3VtZW50cy5sZW5ndGg8Mil7Zm9yKHQgaW4g
bil0aGlzLmF0dHIodCxuW3RdKTtyZXR1cm4gdGhpc312YXIgbz0idHJhbnNmb3Jt
Ij09bj9QdTpndSxhPVZvLm5zLnF1YWxpZnkobik7cmV0dXJuIERvKHRoaXMsImF0
dHIuIituLHQsYS5sb2NhbD9pOnUpfSxObC5hdHRyVHdlZW49ZnVuY3Rpb24obix0
KXtmdW5jdGlvbiBlKG4sZSl7dmFyIHI9dC5jYWxsKHRoaXMsbixlLHRoaXMuZ2V0
QXR0cmlidXRlKHUpKTtyZXR1cm4gciYmZnVuY3Rpb24obil7dGhpcy5zZXRBdHRy
aWJ1dGUodSxyKG4pKX19ZnVuY3Rpb24gcihuLGUpe3ZhciByPXQuY2FsbCh0aGlz
LG4sZSx0aGlzLmdldEF0dHJpYnV0ZU5TKHUuc3BhY2UsdS5sb2NhbCkpO3JldHVy
biByJiZmdW5jdGlvbihuKXt0aGlzLnNldEF0dHJpYnV0ZU5TKHUuc3BhY2UsdS5s
b2NhbCxyKG4pKX19dmFyIHU9Vm8ubnMucXVhbGlmeShuKTtyZXR1cm4gdGhpcy50
d2VlbigiYXR0ci4iK24sdS5sb2NhbD9yOmUpfSxObC5zdHlsZT1mdW5jdGlvbihu
LHQsZSl7ZnVuY3Rpb24gcigpe3RoaXMuc3R5bGUucmVtb3ZlUHJvcGVydHkobil9
ZnVuY3Rpb24gdSh0KXtyZXR1cm4gbnVsbD09dD9yOih0Kz0iIixmdW5jdGlvbigp
e3ZhciByLHU9Sm8uZ2V0Q29tcHV0ZWRTdHlsZSh0aGlzLG51bGwpLmdldFByb3Bl
cnR5VmFsdWUobik7cmV0dXJuIHUhPT10JiYocj1ndSh1LHQpLGZ1bmN0aW9uKHQp
e3RoaXMuc3R5bGUuc2V0UHJvcGVydHkobixyKHQpLGUpfSl9KX12YXIgaT1hcmd1
bWVudHMubGVuZ3RoO2lmKDM+aSl7aWYoInN0cmluZyIhPXR5cGVvZiBuKXsyPmkm
Jih0PSIiKTtmb3IoZSBpbiBuKXRoaXMuc3R5bGUoZSxuW2VdLHQpO3JldHVybiB0
aGlzfWU9IiJ9cmV0dXJuIERvKHRoaXMsInN0eWxlLiIrbix0LHUpfSxObC5zdHls
ZVR3ZWVuPWZ1bmN0aW9uKG4sdCxlKXtmdW5jdGlvbiByKHIsdSl7dmFyIGk9dC5j
YWxsKHRoaXMscix1LEpvLmdldENvbXB1dGVkU3R5bGUodGhpcyxudWxsKS5nZXRQ
cm9wZXJ0eVZhbHVlKG4pKTtyZXR1cm4gaSYmZnVuY3Rpb24odCl7dGhpcy5zdHls
ZS5zZXRQcm9wZXJ0eShuLGkodCksZSl9fXJldHVybiBhcmd1bWVudHMubGVuZ3Ro
PDMmJihlPSIiKSx0aGlzLnR3ZWVuKCJzdHlsZS4iK24scil9LE5sLnRleHQ9ZnVu
Y3Rpb24obil7cmV0dXJuIERvKHRoaXMsInRleHQiLG4sUG8pfSxObC5yZW1vdmU9
ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5lYWNoKCJlbmQudHJhbnNpdGlvbiIsZnVu
Y3Rpb24oKXt2YXIgbjt0aGlzLl9fdHJhbnNpdGlvbl9fLmNvdW50PDImJihuPXRo
aXMucGFyZW50Tm9kZSkmJm4ucmVtb3ZlQ2hpbGQodGhpcyl9KX0sTmwuZWFzZT1m
dW5jdGlvbihuKXt2YXIgdD10aGlzLmlkO3JldHVybiBhcmd1bWVudHMubGVuZ3Ro
PDE/dGhpcy5ub2RlKCkuX190cmFuc2l0aW9uX19bdF0uZWFzZTooImZ1bmN0aW9u
IiE9dHlwZW9mIG4mJihuPVZvLmVhc2UuYXBwbHkoVm8sYXJndW1lbnRzKSksUCh0
aGlzLGZ1bmN0aW9uKGUpe2UuX190cmFuc2l0aW9uX19bdF0uZWFzZT1ufSkpfSxO
bC5kZWxheT1mdW5jdGlvbihuKXt2YXIgdD10aGlzLmlkO3JldHVybiBhcmd1bWVu
dHMubGVuZ3RoPDE/dGhpcy5ub2RlKCkuX190cmFuc2l0aW9uX19bdF0uZGVsYXk6
UCh0aGlzLCJmdW5jdGlvbiI9PXR5cGVvZiBuP2Z1bmN0aW9uKGUscix1KXtlLl9f
dHJhbnNpdGlvbl9fW3RdLmRlbGF5PStuLmNhbGwoZSxlLl9fZGF0YV9fLHIsdSl9
OihuPStuLGZ1bmN0aW9uKGUpe2UuX190cmFuc2l0aW9uX19bdF0uZGVsYXk9bn0p
KX0sTmwuZHVyYXRpb249ZnVuY3Rpb24obil7dmFyIHQ9dGhpcy5pZDtyZXR1cm4g
YXJndW1lbnRzLmxlbmd0aDwxP3RoaXMubm9kZSgpLl9fdHJhbnNpdGlvbl9fW3Rd
LmR1cmF0aW9uOlAodGhpcywiZnVuY3Rpb24iPT10eXBlb2Ygbj9mdW5jdGlvbihl
LHIsdSl7ZS5fX3RyYW5zaXRpb25fX1t0XS5kdXJhdGlvbj1NYXRoLm1heCgxLG4u
Y2FsbChlLGUuX19kYXRhX18scix1KSl9OihuPU1hdGgubWF4KDEsbiksZnVuY3Rp
b24oZSl7ZS5fX3RyYW5zaXRpb25fX1t0XS5kdXJhdGlvbj1ufSkpfSxObC5lYWNo
PWZ1bmN0aW9uKG4sdCl7dmFyIGU9dGhpcy5pZDtpZihhcmd1bWVudHMubGVuZ3Ro
PDIpe3ZhciByPUVsLHU9a2w7a2w9ZSxQKHRoaXMsZnVuY3Rpb24odCxyLHUpe0Vs
PXQuX190cmFuc2l0aW9uX19bZV0sbi5jYWxsKHQsdC5fX2RhdGFfXyxyLHUpfSks
RWw9cixrbD11fWVsc2UgUCh0aGlzLGZ1bmN0aW9uKHIpe3ZhciB1PXIuX190cmFu
c2l0aW9uX19bZV07KHUuZXZlbnR8fCh1LmV2ZW50PVZvLmRpc3BhdGNoKCJzdGFy
dCIsImVuZCIpKSkub24obix0KX0pO3JldHVybiB0aGlzfSxObC50cmFuc2l0aW9u
PWZ1bmN0aW9uKCl7Zm9yKHZhciBuLHQsZSxyLHU9dGhpcy5pZCxpPSsremwsbz1b
XSxhPTAsYz10aGlzLmxlbmd0aDtjPmE7YSsrKXtvLnB1c2gobj1bXSk7Zm9yKHZh
ciB0PXRoaXNbYV0sbD0wLHM9dC5sZW5ndGg7cz5sO2wrKykoZT10W2xdKSYmKHI9
T2JqZWN0LmNyZWF0ZShlLl9fdHJhbnNpdGlvbl9fW3VdKSxyLmRlbGF5Kz1yLmR1
cmF0aW9uLFVvKGUsbCxpLHIpKSxuLnB1c2goZSl9cmV0dXJuIFJvKG8saSl9LFZv
LnN2Zy5heGlzPWZ1bmN0aW9uKCl7ZnVuY3Rpb24gbihuKXtuLmVhY2goZnVuY3Rp
b24oKXt2YXIgbixsPVZvLnNlbGVjdCh0aGlzKSxzPXRoaXMuX19jaGFydF9ffHxl
LGY9dGhpcy5fX2NoYXJ0X189ZS5jb3B5KCksaD1udWxsPT1jP2YudGlja3M/Zi50
aWNrcy5hcHBseShmLGEpOmYuZG9tYWluKCk6YyxnPW51bGw9PXQ/Zi50aWNrRm9y
bWF0P2YudGlja0Zvcm1hdC5hcHBseShmLGEpOnd0OnQscD1sLnNlbGVjdEFsbCgi
LnRpY2siKS5kYXRhKGgsZiksdj1wLmVudGVyKCkuaW5zZXJ0KCJnIiwiLmRvbWFp
biIpLmF0dHIoImNsYXNzIiwidGljayIpLnN0eWxlKCJvcGFjaXR5IixFYSksZD1W
by50cmFuc2l0aW9uKHAuZXhpdCgpKS5zdHlsZSgib3BhY2l0eSIsRWEpLnJlbW92
ZSgpLG09Vm8udHJhbnNpdGlvbihwLm9yZGVyKCkpLnN0eWxlKCJvcGFjaXR5Iiwx
KSx5PXFpKGYpLHg9bC5zZWxlY3RBbGwoIi5kb21haW4iKS5kYXRhKFswXSksTT0o
eC5lbnRlcigpLmFwcGVuZCgicGF0aCIpLmF0dHIoImNsYXNzIiwiZG9tYWluIiks
Vm8udHJhbnNpdGlvbih4KSk7di5hcHBlbmQoImxpbmUiKSx2LmFwcGVuZCgidGV4
dCIpO3ZhciBfPXYuc2VsZWN0KCJsaW5lIiksYj1tLnNlbGVjdCgibGluZSIpLHc9
cC5zZWxlY3QoInRleHQiKS50ZXh0KGcpLFM9di5zZWxlY3QoInRleHQiKSxrPW0u
c2VsZWN0KCJ0ZXh0Iik7c3dpdGNoKHIpe2Nhc2UiYm90dG9tIjpuPWpvLF8uYXR0
cigieTIiLHUpLFMuYXR0cigieSIsTWF0aC5tYXgodSwwKStvKSxiLmF0dHIoIngy
IiwwKS5hdHRyKCJ5MiIsdSksay5hdHRyKCJ4IiwwKS5hdHRyKCJ5IixNYXRoLm1h
eCh1LDApK28pLHcuYXR0cigiZHkiLCIuNzFlbSIpLnN0eWxlKCJ0ZXh0LWFuY2hv
ciIsIm1pZGRsZSIpLE0uYXR0cigiZCIsIk0iK3lbMF0rIiwiK2krIlYwSCIreVsx
XSsiViIraSk7YnJlYWs7Y2FzZSJ0b3AiOm49am8sXy5hdHRyKCJ5MiIsLXUpLFMu
YXR0cigieSIsLShNYXRoLm1heCh1LDApK28pKSxiLmF0dHIoIngyIiwwKS5hdHRy
KCJ5MiIsLXUpLGsuYXR0cigieCIsMCkuYXR0cigieSIsLShNYXRoLm1heCh1LDAp
K28pKSx3LmF0dHIoImR5IiwiMGVtIikuc3R5bGUoInRleHQtYW5jaG9yIiwibWlk
ZGxlIiksTS5hdHRyKCJkIiwiTSIreVswXSsiLCIrLWkrIlYwSCIreVsxXSsiViIr
LWkpO2JyZWFrO2Nhc2UibGVmdCI6bj1IbyxfLmF0dHIoIngyIiwtdSksUy5hdHRy
KCJ4IiwtKE1hdGgubWF4KHUsMCkrbykpLGIuYXR0cigieDIiLC11KS5hdHRyKCJ5
MiIsMCksay5hdHRyKCJ4IiwtKE1hdGgubWF4KHUsMCkrbykpLmF0dHIoInkiLDAp
LHcuYXR0cigiZHkiLCIuMzJlbSIpLnN0eWxlKCJ0ZXh0LWFuY2hvciIsImVuZCIp
LE0uYXR0cigiZCIsIk0iKy1pKyIsIit5WzBdKyJIMFYiK3lbMV0rIkgiKy1pKTti
cmVhaztjYXNlInJpZ2h0IjpuPUhvLF8uYXR0cigieDIiLHUpLFMuYXR0cigieCIs
TWF0aC5tYXgodSwwKStvKSxiLmF0dHIoIngyIix1KS5hdHRyKCJ5MiIsMCksay5h
dHRyKCJ4IixNYXRoLm1heCh1LDApK28pLmF0dHIoInkiLDApLHcuYXR0cigiZHki
LCIuMzJlbSIpLnN0eWxlKCJ0ZXh0LWFuY2hvciIsInN0YXJ0IiksTS5hdHRyKCJk
IiwiTSIraSsiLCIreVswXSsiSDBWIit5WzFdKyJIIitpKX1pZihmLnJhbmdlQmFu
ZCl7dmFyIEU9ZixBPUUucmFuZ2VCYW5kKCkvMjtzPWY9ZnVuY3Rpb24obil7cmV0
dXJuIEUobikrQX19ZWxzZSBzLnJhbmdlQmFuZD9zPWY6ZC5jYWxsKG4sZik7di5j
YWxsKG4scyksbS5jYWxsKG4sZil9KX12YXIgdCxlPVZvLnNjYWxlLmxpbmVhcigp
LHI9TGwsdT02LGk9NixvPTMsYT1bMTBdLGM9bnVsbDtyZXR1cm4gbi5zY2FsZT1m
dW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oZT10LG4pOmV9LG4u
b3JpZW50PWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhyPXQg
aW4gVGw/dCsiIjpMbCxuKTpyfSxuLnRpY2tzPWZ1bmN0aW9uKCl7cmV0dXJuIGFy
Z3VtZW50cy5sZW5ndGg/KGE9YXJndW1lbnRzLG4pOmF9LG4udGlja1ZhbHVlcz1m
dW5jdGlvbih0KXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8oYz10LG4pOmN9LG4u
dGlja0Zvcm1hdD1mdW5jdGlvbihlKXtyZXR1cm4gYXJndW1lbnRzLmxlbmd0aD8o
dD1lLG4pOnR9LG4udGlja1NpemU9ZnVuY3Rpb24odCl7dmFyIGU9YXJndW1lbnRz
Lmxlbmd0aDtyZXR1cm4gZT8odT0rdCxpPSthcmd1bWVudHNbZS0xXSxuKTp1fSxu
LmlubmVyVGlja1NpemU9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KHU9K3Qsbik6dX0sbi5vdXRlclRpY2tTaXplPWZ1bmN0aW9uKHQpe3JldHVy
biBhcmd1bWVudHMubGVuZ3RoPyhpPSt0LG4pOml9LG4udGlja1BhZGRpbmc9ZnVu
Y3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg/KG89K3Qsbik6b30sbi50
aWNrU3ViZGl2aWRlPWZ1bmN0aW9uKCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGgm
Jm59LG59O3ZhciBMbD0iYm90dG9tIixUbD17dG9wOjEscmlnaHQ6MSxib3R0b206
MSxsZWZ0OjF9O1ZvLnN2Zy5icnVzaD1mdW5jdGlvbigpe2Z1bmN0aW9uIG4oaSl7
aS5lYWNoKGZ1bmN0aW9uKCl7dmFyIGk9Vm8uc2VsZWN0KHRoaXMpLnN0eWxlKCJw
b2ludGVyLWV2ZW50cyIsImFsbCIpLnN0eWxlKCItd2Via2l0LXRhcC1oaWdobGln
aHQtY29sb3IiLCJyZ2JhKDAsMCwwLDApIikub24oIm1vdXNlZG93bi5icnVzaCIs
dSkub24oInRvdWNoc3RhcnQuYnJ1c2giLHUpLG89aS5zZWxlY3RBbGwoIi5iYWNr
Z3JvdW5kIikuZGF0YShbMF0pO28uZW50ZXIoKS5hcHBlbmQoInJlY3QiKS5hdHRy
KCJjbGFzcyIsImJhY2tncm91bmQiKS5zdHlsZSgidmlzaWJpbGl0eSIsImhpZGRl
biIpLnN0eWxlKCJjdXJzb3IiLCJjcm9zc2hhaXIiKSxpLnNlbGVjdEFsbCgiLmV4
dGVudCIpLmRhdGEoWzBdKS5lbnRlcigpLmFwcGVuZCgicmVjdCIpLmF0dHIoImNs
YXNzIiwiZXh0ZW50Iikuc3R5bGUoImN1cnNvciIsIm1vdmUiKTt2YXIgYT1pLnNl
bGVjdEFsbCgiLnJlc2l6ZSIpLmRhdGEocCx3dCk7YS5leGl0KCkucmVtb3ZlKCks
YS5lbnRlcigpLmFwcGVuZCgiZyIpLmF0dHIoImNsYXNzIixmdW5jdGlvbihuKXty
ZXR1cm4icmVzaXplICIrbn0pLnN0eWxlKCJjdXJzb3IiLGZ1bmN0aW9uKG4pe3Jl
dHVybiBxbFtuXX0pLmFwcGVuZCgicmVjdCIpLmF0dHIoIngiLGZ1bmN0aW9uKG4p
e3JldHVybi9bZXddJC8udGVzdChuKT8tMzpudWxsfSkuYXR0cigieSIsZnVuY3Rp
b24obil7cmV0dXJuL15bbnNdLy50ZXN0KG4pPy0zOm51bGx9KS5hdHRyKCJ3aWR0
aCIsNikuYXR0cigiaGVpZ2h0Iiw2KS5zdHlsZSgidmlzaWJpbGl0eSIsImhpZGRl
biIpLGEuc3R5bGUoImRpc3BsYXkiLG4uZW1wdHkoKT8ibm9uZSI6bnVsbCk7dmFy
IHMsZj1Wby50cmFuc2l0aW9uKGkpLGg9Vm8udHJhbnNpdGlvbihvKTtjJiYocz1x
aShjKSxoLmF0dHIoIngiLHNbMF0pLmF0dHIoIndpZHRoIixzWzFdLXNbMF0pLGUo
ZikpLGwmJihzPXFpKGwpLGguYXR0cigieSIsc1swXSkuYXR0cigiaGVpZ2h0Iixz
WzFdLXNbMF0pLHIoZikpLHQoZil9KX1mdW5jdGlvbiB0KG4pe24uc2VsZWN0QWxs
KCIucmVzaXplIikuYXR0cigidHJhbnNmb3JtIixmdW5jdGlvbihuKXtyZXR1cm4i
dHJhbnNsYXRlKCIrc1srL2UkLy50ZXN0KG4pXSsiLCIrZlsrL15zLy50ZXN0KG4p
XSsiKSJ9KX1mdW5jdGlvbiBlKG4pe24uc2VsZWN0KCIuZXh0ZW50IikuYXR0cigi
eCIsc1swXSksbi5zZWxlY3RBbGwoIi5leHRlbnQsLm4+cmVjdCwucz5yZWN0Iiku
YXR0cigid2lkdGgiLHNbMV0tc1swXSl9ZnVuY3Rpb24gcihuKXtuLnNlbGVjdCgi
LmV4dGVudCIpLmF0dHIoInkiLGZbMF0pLG4uc2VsZWN0QWxsKCIuZXh0ZW50LC5l
PnJlY3QsLnc+cmVjdCIpLmF0dHIoImhlaWdodCIsZlsxXS1mWzBdKX1mdW5jdGlv
biB1KCl7ZnVuY3Rpb24gdSgpezMyPT1Wby5ldmVudC5rZXlDb2RlJiYoQ3x8KHg9
bnVsbCx6WzBdLT1zWzFdLHpbMV0tPWZbMV0sQz0yKSx5KCkpfWZ1bmN0aW9uIHAo
KXszMj09Vm8uZXZlbnQua2V5Q29kZSYmMj09QyYmKHpbMF0rPXNbMV0selsxXSs9
ZlsxXSxDPTAseSgpKX1mdW5jdGlvbiB2KCl7dmFyIG49Vm8ubW91c2UoXyksdT0h
MTtNJiYoblswXSs9TVswXSxuWzFdKz1NWzFdKSxDfHwoVm8uZXZlbnQuYWx0S2V5
Pyh4fHwoeD1bKHNbMF0rc1sxXSkvMiwoZlswXStmWzFdKS8yXSkselswXT1zWyso
blswXTx4WzBdKV0selsxXT1mWysoblsxXTx4WzFdKV0pOng9bnVsbCksRSYmZChu
LGMsMCkmJihlKFMpLHU9ITApLEEmJmQobixsLDEpJiYocihTKSx1PSEwKSx1JiYo
dChTKSx3KHt0eXBlOiJicnVzaCIsbW9kZTpDPyJtb3ZlIjoicmVzaXplIn0pKX1m
dW5jdGlvbiBkKG4sdCxlKXt2YXIgcix1LGE9cWkodCksYz1hWzBdLGw9YVsxXSxw
PXpbZV0sdj1lP2Y6cyxkPXZbMV0tdlswXTtyZXR1cm4gQyYmKGMtPXAsbC09ZCtw
KSxyPShlP2c6aCk/TWF0aC5tYXgoYyxNYXRoLm1pbihsLG5bZV0pKTpuW2VdLEM/
dT0ocis9cCkrZDooeCYmKHA9TWF0aC5tYXgoYyxNYXRoLm1pbihsLDIqeFtlXS1y
KSkpLHI+cD8odT1yLHI9cCk6dT1wKSx2WzBdIT1yfHx2WzFdIT11PyhlP289bnVs
bDppPW51bGwsdlswXT1yLHZbMV09dSwhMCk6dm9pZCAwfWZ1bmN0aW9uIG0oKXt2
KCksUy5zdHlsZSgicG9pbnRlci1ldmVudHMiLCJhbGwiKS5zZWxlY3RBbGwoIi5y
ZXNpemUiKS5zdHlsZSgiZGlzcGxheSIsbi5lbXB0eSgpPyJub25lIjpudWxsKSxW
by5zZWxlY3QoImJvZHkiKS5zdHlsZSgiY3Vyc29yIixudWxsKSxMLm9uKCJtb3Vz
ZW1vdmUuYnJ1c2giLG51bGwpLm9uKCJtb3VzZXVwLmJydXNoIixudWxsKS5vbigi
dG91Y2htb3ZlLmJydXNoIixudWxsKS5vbigidG91Y2hlbmQuYnJ1c2giLG51bGwp
Lm9uKCJrZXlkb3duLmJydXNoIixudWxsKS5vbigia2V5dXAuYnJ1c2giLG51bGwp
LE4oKSx3KHt0eXBlOiJicnVzaGVuZCJ9KX12YXIgeCxNLF89dGhpcyxiPVZvLnNl
bGVjdChWby5ldmVudC50YXJnZXQpLHc9YS5vZihfLGFyZ3VtZW50cyksUz1Wby5z
ZWxlY3QoXyksaz1iLmRhdHVtKCksRT0hL14obnxzKSQvLnRlc3QoaykmJmMsQT0h
L14oZXx3KSQvLnRlc3QoaykmJmwsQz1iLmNsYXNzZWQoImV4dGVudCIpLE49SSgp
LHo9Vm8ubW91c2UoXyksTD1Wby5zZWxlY3QoSm8pLm9uKCJrZXlkb3duLmJydXNo
Iix1KS5vbigia2V5dXAuYnJ1c2giLHApO2lmKFZvLmV2ZW50LmNoYW5nZWRUb3Vj
aGVzP0wub24oInRvdWNobW92ZS5icnVzaCIsdikub24oInRvdWNoZW5kLmJydXNo
IixtKTpMLm9uKCJtb3VzZW1vdmUuYnJ1c2giLHYpLm9uKCJtb3VzZXVwLmJydXNo
IixtKSxTLmludGVycnVwdCgpLnNlbGVjdEFsbCgiKiIpLmludGVycnVwdCgpLEMp
elswXT1zWzBdLXpbMF0selsxXT1mWzBdLXpbMV07ZWxzZSBpZihrKXt2YXIgVD0r
L3ckLy50ZXN0KGspLHE9Ky9ebi8udGVzdChrKTtNPVtzWzEtVF0telswXSxmWzEt
cV0telsxXV0selswXT1zW1RdLHpbMV09ZltxXX1lbHNlIFZvLmV2ZW50LmFsdEtl
eSYmKHg9ei5zbGljZSgpKTtTLnN0eWxlKCJwb2ludGVyLWV2ZW50cyIsIm5vbmUi
KS5zZWxlY3RBbGwoIi5yZXNpemUiKS5zdHlsZSgiZGlzcGxheSIsbnVsbCksVm8u
c2VsZWN0KCJib2R5Iikuc3R5bGUoImN1cnNvciIsYi5zdHlsZSgiY3Vyc29yIikp
LHcoe3R5cGU6ImJydXNoc3RhcnQifSksdigpfXZhciBpLG8sYT1NKG4sImJydXNo
c3RhcnQiLCJicnVzaCIsImJydXNoZW5kIiksYz1udWxsLGw9bnVsbCxzPVswLDBd
LGY9WzAsMF0saD0hMCxnPSEwLHA9UmxbMF07cmV0dXJuIG4uZXZlbnQ9ZnVuY3Rp
b24obil7bi5lYWNoKGZ1bmN0aW9uKCl7dmFyIG49YS5vZih0aGlzLGFyZ3VtZW50
cyksdD17eDpzLHk6ZixpOmksajpvfSxlPXRoaXMuX19jaGFydF9ffHx0O3RoaXMu
X19jaGFydF9fPXQsa2w/Vm8uc2VsZWN0KHRoaXMpLnRyYW5zaXRpb24oKS5lYWNo
KCJzdGFydC5icnVzaCIsZnVuY3Rpb24oKXtpPWUuaSxvPWUuaixzPWUueCxmPWUu
eSxuKHt0eXBlOiJicnVzaHN0YXJ0In0pfSkudHdlZW4oImJydXNoOmJydXNoIixm
dW5jdGlvbigpe3ZhciBlPXB1KHMsdC54KSxyPXB1KGYsdC55KTtyZXR1cm4gaT1v
PW51bGwsZnVuY3Rpb24odSl7cz10Lng9ZSh1KSxmPXQueT1yKHUpLG4oe3R5cGU6
ImJydXNoIixtb2RlOiJyZXNpemUifSl9fSkuZWFjaCgiZW5kLmJydXNoIixmdW5j
dGlvbigpe2k9dC5pLG89dC5qLG4oe3R5cGU6ImJydXNoIixtb2RlOiJyZXNpemUi
fSksbih7dHlwZToiYnJ1c2hlbmQifSl9KToobih7dHlwZToiYnJ1c2hzdGFydCJ9
KSxuKHt0eXBlOiJicnVzaCIsbW9kZToicmVzaXplIn0pLG4oe3R5cGU6ImJydXNo
ZW5kIn0pKX0pfSxuLng9ZnVuY3Rpb24odCl7cmV0dXJuIGFyZ3VtZW50cy5sZW5n
dGg/KGM9dCxwPVJsWyFjPDwxfCFsXSxuKTpjfSxuLnk9ZnVuY3Rpb24odCl7cmV0
dXJuIGFyZ3VtZW50cy5sZW5ndGg/KGw9dCxwPVJsWyFjPDwxfCFsXSxuKTpsfSxu
LmNsYW1wPWZ1bmN0aW9uKHQpe3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhjJiZs
PyhoPSEhdFswXSxnPSEhdFsxXSk6Yz9oPSEhdDpsJiYoZz0hIXQpLG4pOmMmJmw/
W2gsZ106Yz9oOmw/ZzpudWxsfSxuLmV4dGVudD1mdW5jdGlvbih0KXt2YXIgZSxy
LHUsYSxoO3JldHVybiBhcmd1bWVudHMubGVuZ3RoPyhjJiYoZT10WzBdLHI9dFsx
XSxsJiYoZT1lWzBdLHI9clswXSksaT1bZSxyXSxjLmludmVydCYmKGU9YyhlKSxy
PWMocikpLGU+ciYmKGg9ZSxlPXIscj1oKSwoZSE9c1swXXx8ciE9c1sxXSkmJihz
PVtlLHJdKSksbCYmKHU9dFswXSxhPXRbMV0sYyYmKHU9dVsxXSxhPWFbMV0pLG89
W3UsYV0sbC5pbnZlcnQmJih1PWwodSksYT1sKGEpKSx1PmEmJihoPXUsdT1hLGE9
aCksKHUhPWZbMF18fGEhPWZbMV0pJiYoZj1bdSxhXSkpLG4pOihjJiYoaT8oZT1p
WzBdLHI9aVsxXSk6KGU9c1swXSxyPXNbMV0sYy5pbnZlcnQmJihlPWMuaW52ZXJ0
KGUpLHI9Yy5pbnZlcnQocikpLGU+ciYmKGg9ZSxlPXIscj1oKSkpLGwmJihvPyh1
PW9bMF0sYT1vWzFdKToodT1mWzBdLGE9ZlsxXSxsLmludmVydCYmKHU9bC5pbnZl
cnQodSksYT1sLmludmVydChhKSksdT5hJiYoaD11LHU9YSxhPWgpKSksYyYmbD9b
W2UsdV0sW3IsYV1dOmM/W2Uscl06bCYmW3UsYV0pfSxuLmNsZWFyPWZ1bmN0aW9u
KCl7cmV0dXJuIG4uZW1wdHkoKXx8KHM9WzAsMF0sZj1bMCwwXSxpPW89bnVsbCks
bn0sbi5lbXB0eT1mdW5jdGlvbigpe3JldHVybiEhYyYmc1swXT09c1sxXXx8ISFs
JiZmWzBdPT1mWzFdfSxWby5yZWJpbmQobixhLCJvbiIpfTt2YXIgcWw9e246Im5z
LXJlc2l6ZSIsZToiZXctcmVzaXplIixzOiJucy1yZXNpemUiLHc6ImV3LXJlc2l6
ZSIsbnc6Im53c2UtcmVzaXplIixuZToibmVzdy1yZXNpemUiLHNlOiJud3NlLXJl
c2l6ZSIsc3c6Im5lc3ctcmVzaXplIn0sUmw9W1sibiIsImUiLCJzIiwidyIsIm53
IiwibmUiLCJzZSIsInN3Il0sWyJlIiwidyJdLFsibiIsInMiXSxbXV0sRGw9bmMu
Zm9ybWF0PW9jLnRpbWVGb3JtYXQsUGw9RGwudXRjLFVsPVBsKCIlWS0lbS0lZFQl
SDolTTolUy4lTFoiKTtEbC5pc289RGF0ZS5wcm90b3R5cGUudG9JU09TdHJpbmcm
JituZXcgRGF0ZSgiMjAwMC0wMS0wMVQwMDowMDowMC4wMDBaIik/Rm86VWwsRm8u
cGFyc2U9ZnVuY3Rpb24obil7dmFyIHQ9bmV3IERhdGUobik7cmV0dXJuIGlzTmFO
KHQpP251bGw6dH0sRm8udG9TdHJpbmc9VWwudG9TdHJpbmcsbmMuc2Vjb25kPVB0
KGZ1bmN0aW9uKG4pe3JldHVybiBuZXcgdGMoMWUzKk1hdGguZmxvb3Iobi8xZTMp
KX0sZnVuY3Rpb24obix0KXtuLnNldFRpbWUobi5nZXRUaW1lKCkrMWUzKk1hdGgu
Zmxvb3IodCkpfSxmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRTZWNvbmRzKCl9KSxu
Yy5zZWNvbmRzPW5jLnNlY29uZC5yYW5nZSxuYy5zZWNvbmRzLnV0Yz1uYy5zZWNv
bmQudXRjLnJhbmdlLG5jLm1pbnV0ZT1QdChmdW5jdGlvbihuKXtyZXR1cm4gbmV3
IHRjKDZlNCpNYXRoLmZsb29yKG4vNmU0KSl9LGZ1bmN0aW9uKG4sdCl7bi5zZXRU
aW1lKG4uZ2V0VGltZSgpKzZlNCpNYXRoLmZsb29yKHQpKX0sZnVuY3Rpb24obil7
cmV0dXJuIG4uZ2V0TWludXRlcygpfSksbmMubWludXRlcz1uYy5taW51dGUucmFu
Z2UsbmMubWludXRlcy51dGM9bmMubWludXRlLnV0Yy5yYW5nZSxuYy5ob3VyPVB0
KGZ1bmN0aW9uKG4pe3ZhciB0PW4uZ2V0VGltZXpvbmVPZmZzZXQoKS82MDtyZXR1
cm4gbmV3IHRjKDM2ZTUqKE1hdGguZmxvb3Iobi8zNmU1LXQpK3QpKX0sZnVuY3Rp
b24obix0KXtuLnNldFRpbWUobi5nZXRUaW1lKCkrMzZlNSpNYXRoLmZsb29yKHQp
KX0sZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0SG91cnMoKX0pLG5jLmhvdXJzPW5j
LmhvdXIucmFuZ2UsbmMuaG91cnMudXRjPW5jLmhvdXIudXRjLnJhbmdlLG5jLm1v
bnRoPVB0KGZ1bmN0aW9uKG4pe3JldHVybiBuPW5jLmRheShuKSxuLnNldERhdGUo
MSksbn0sZnVuY3Rpb24obix0KXtuLnNldE1vbnRoKG4uZ2V0TW9udGgoKSt0KX0s
ZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0TW9udGgoKX0pLG5jLm1vbnRocz1uYy5t
b250aC5yYW5nZSxuYy5tb250aHMudXRjPW5jLm1vbnRoLnV0Yy5yYW5nZTt2YXIg
amw9WzFlMyw1ZTMsMTVlMywzZTQsNmU0LDNlNSw5ZTUsMThlNSwzNmU1LDEwOGU1
LDIxNmU1LDQzMmU1LDg2NGU1LDE3MjhlNSw2MDQ4ZTUsMjU5MmU2LDc3NzZlNiwz
MTUzNmU2XSxIbD1bW25jLnNlY29uZCwxXSxbbmMuc2Vjb25kLDVdLFtuYy5zZWNv
bmQsMTVdLFtuYy5zZWNvbmQsMzBdLFtuYy5taW51dGUsMV0sW25jLm1pbnV0ZSw1
XSxbbmMubWludXRlLDE1XSxbbmMubWludXRlLDMwXSxbbmMuaG91ciwxXSxbbmMu
aG91ciwzXSxbbmMuaG91ciw2XSxbbmMuaG91ciwxMl0sW25jLmRheSwxXSxbbmMu
ZGF5LDJdLFtuYy53ZWVrLDFdLFtuYy5tb250aCwxXSxbbmMubW9udGgsM10sW25j
LnllYXIsMV1dLEZsPURsLm11bHRpKFtbIi4lTCIsZnVuY3Rpb24obil7cmV0dXJu
IG4uZ2V0TWlsbGlzZWNvbmRzKCl9XSxbIjolUyIsZnVuY3Rpb24obil7cmV0dXJu
IG4uZ2V0U2Vjb25kcygpfV0sWyIlSTolTSIsZnVuY3Rpb24obil7cmV0dXJuIG4u
Z2V0TWludXRlcygpfV0sWyIlSSAlcCIsZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0
SG91cnMoKX1dLFsiJWEgJWQiLGZ1bmN0aW9uKG4pe3JldHVybiBuLmdldERheSgp
JiYxIT1uLmdldERhdGUoKX1dLFsiJWIgJWQiLGZ1bmN0aW9uKG4pe3JldHVybiAx
IT1uLmdldERhdGUoKX1dLFsiJUIiLGZ1bmN0aW9uKG4pe3JldHVybiBuLmdldE1v
bnRoKCl9XSxbIiVZIixTZV1dKSxPbD17cmFuZ2U6ZnVuY3Rpb24obix0LGUpe3Jl
dHVybiBWby5yYW5nZShNYXRoLmNlaWwobi9lKSplLCt0LGUpLm1hcChZbyl9LGZs
b29yOnd0LGNlaWw6d3R9O0hsLnllYXI9bmMueWVhcixuYy5zY2FsZT1mdW5jdGlv
bigpe3JldHVybiBPbyhWby5zY2FsZS5saW5lYXIoKSxIbCxGbCl9O3ZhciBZbD1I
bC5tYXAoZnVuY3Rpb24obil7cmV0dXJuW25bMF0udXRjLG5bMV1dfSksSWw9UGwu
bXVsdGkoW1siLiVMIixmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRVVENNaWxsaXNl
Y29uZHMoKX1dLFsiOiVTIixmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRVVENTZWNv
bmRzKCl9XSxbIiVJOiVNIixmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRVVENNaW51
dGVzKCl9XSxbIiVJICVwIixmdW5jdGlvbihuKXtyZXR1cm4gbi5nZXRVVENIb3Vy
cygpfV0sWyIlYSAlZCIsZnVuY3Rpb24obil7cmV0dXJuIG4uZ2V0VVRDRGF5KCkm
JjEhPW4uZ2V0VVRDRGF0ZSgpfV0sWyIlYiAlZCIsZnVuY3Rpb24obil7cmV0dXJu
IDEhPW4uZ2V0VVRDRGF0ZSgpfV0sWyIlQiIsZnVuY3Rpb24obil7cmV0dXJuIG4u
Z2V0VVRDTW9udGgoKX1dLFsiJVkiLFNlXV0pO1lsLnllYXI9bmMueWVhci51dGMs
bmMuc2NhbGUudXRjPWZ1bmN0aW9uKCl7cmV0dXJuIE9vKFZvLnNjYWxlLmxpbmVh
cigpLFlsLElsKX0sVm8udGV4dD1TdChmdW5jdGlvbihuKXtyZXR1cm4gbi5yZXNw
b25zZVRleHR9KSxWby5qc29uPWZ1bmN0aW9uKG4sdCl7cmV0dXJuIGt0KG4sImFw
cGxpY2F0aW9uL2pzb24iLElvLHQpfSxWby5odG1sPWZ1bmN0aW9uKG4sdCl7cmV0
dXJuIGt0KG4sInRleHQvaHRtbCIsWm8sdCl9LFZvLnhtbD1TdChmdW5jdGlvbihu
KXtyZXR1cm4gbi5yZXNwb25zZVhNTH0pLCJmdW5jdGlvbiI9PXR5cGVvZiBkZWZp
bmUmJmRlZmluZS5hbWQ/ZGVmaW5lKFZvKToib2JqZWN0Ij09dHlwZW9mIG1vZHVs
ZSYmbW9kdWxlLmV4cG9ydHMmJihtb2R1bGUuZXhwb3J0cz1WbyksdGhpcy5kMz1W
b30oKTs8L3NjcmlwdD4NCiAgICAgIDwhLS0gbWluaW1pemVkIGpxdWVyeSBqcyBs
aWJyYXJ5IC0tPg0KICAgICAgPHNjcmlwdD4vKiEgalF1ZXJ5IHYyLjEuMSB8IChj
KSAyMDA1LCAyMDE0IGpRdWVyeSBGb3VuZGF0aW9uLCBJbmMuIHwganF1ZXJ5Lm9y
Zy9saWNlbnNlICovCiFmdW5jdGlvbihhLGIpeyJvYmplY3QiPT10eXBlb2YgbW9k
dWxlJiYib2JqZWN0Ij09dHlwZW9mIG1vZHVsZS5leHBvcnRzP21vZHVsZS5leHBv
cnRzPWEuZG9jdW1lbnQ/YihhLCEwKTpmdW5jdGlvbihhKXtpZighYS5kb2N1bWVu
dCl0aHJvdyBuZXcgRXJyb3IoImpRdWVyeSByZXF1aXJlcyBhIHdpbmRvdyB3aXRo
IGEgZG9jdW1lbnQiKTtyZXR1cm4gYihhKX06YihhKX0oInVuZGVmaW5lZCIhPXR5
cGVvZiB3aW5kb3c/d2luZG93OnRoaXMsZnVuY3Rpb24oYSxiKXt2YXIgYz1bXSxk
PWMuc2xpY2UsZT1jLmNvbmNhdCxmPWMucHVzaCxnPWMuaW5kZXhPZixoPXt9LGk9
aC50b1N0cmluZyxqPWguaGFzT3duUHJvcGVydHksaz17fSxsPWEuZG9jdW1lbnQs
bT0iMi4xLjEiLG49ZnVuY3Rpb24oYSxiKXtyZXR1cm4gbmV3IG4uZm4uaW5pdChh
LGIpfSxvPS9eW1xzXHVGRUZGXHhBMF0rfFtcc1x1RkVGRlx4QTBdKyQvZyxwPS9e
LW1zLS8scT0vLShbXGRhLXpdKS9naSxyPWZ1bmN0aW9uKGEsYil7cmV0dXJuIGIu
dG9VcHBlckNhc2UoKX07bi5mbj1uLnByb3RvdHlwZT17anF1ZXJ5Om0sY29uc3Ry
dWN0b3I6bixzZWxlY3RvcjoiIixsZW5ndGg6MCx0b0FycmF5OmZ1bmN0aW9uKCl7
cmV0dXJuIGQuY2FsbCh0aGlzKX0sZ2V0OmZ1bmN0aW9uKGEpe3JldHVybiBudWxs
IT1hPzA+YT90aGlzW2ErdGhpcy5sZW5ndGhdOnRoaXNbYV06ZC5jYWxsKHRoaXMp
fSxwdXNoU3RhY2s6ZnVuY3Rpb24oYSl7dmFyIGI9bi5tZXJnZSh0aGlzLmNvbnN0
cnVjdG9yKCksYSk7cmV0dXJuIGIucHJldk9iamVjdD10aGlzLGIuY29udGV4dD10
aGlzLmNvbnRleHQsYn0sZWFjaDpmdW5jdGlvbihhLGIpe3JldHVybiBuLmVhY2go
dGhpcyxhLGIpfSxtYXA6ZnVuY3Rpb24oYSl7cmV0dXJuIHRoaXMucHVzaFN0YWNr
KG4ubWFwKHRoaXMsZnVuY3Rpb24oYixjKXtyZXR1cm4gYS5jYWxsKGIsYyxiKX0p
KX0sc2xpY2U6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5wdXNoU3RhY2soZC5hcHBs
eSh0aGlzLGFyZ3VtZW50cykpfSxmaXJzdDpmdW5jdGlvbigpe3JldHVybiB0aGlz
LmVxKDApfSxsYXN0OmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMuZXEoLTEpfSxlcTpm
dW5jdGlvbihhKXt2YXIgYj10aGlzLmxlbmd0aCxjPSthKygwPmE/YjowKTtyZXR1
cm4gdGhpcy5wdXNoU3RhY2soYz49MCYmYj5jP1t0aGlzW2NdXTpbXSl9LGVuZDpm
dW5jdGlvbigpe3JldHVybiB0aGlzLnByZXZPYmplY3R8fHRoaXMuY29uc3RydWN0
b3IobnVsbCl9LHB1c2g6Zixzb3J0OmMuc29ydCxzcGxpY2U6Yy5zcGxpY2V9LG4u
ZXh0ZW5kPW4uZm4uZXh0ZW5kPWZ1bmN0aW9uKCl7dmFyIGEsYixjLGQsZSxmLGc9
YXJndW1lbnRzWzBdfHx7fSxoPTEsaT1hcmd1bWVudHMubGVuZ3RoLGo9ITE7Zm9y
KCJib29sZWFuIj09dHlwZW9mIGcmJihqPWcsZz1hcmd1bWVudHNbaF18fHt9LGgr
KyksIm9iamVjdCI9PXR5cGVvZiBnfHxuLmlzRnVuY3Rpb24oZyl8fChnPXt9KSxo
PT09aSYmKGc9dGhpcyxoLS0pO2k+aDtoKyspaWYobnVsbCE9KGE9YXJndW1lbnRz
W2hdKSlmb3IoYiBpbiBhKWM9Z1tiXSxkPWFbYl0sZyE9PWQmJihqJiZkJiYobi5p
c1BsYWluT2JqZWN0KGQpfHwoZT1uLmlzQXJyYXkoZCkpKT8oZT8oZT0hMSxmPWMm
Jm4uaXNBcnJheShjKT9jOltdKTpmPWMmJm4uaXNQbGFpbk9iamVjdChjKT9jOnt9
LGdbYl09bi5leHRlbmQoaixmLGQpKTp2b2lkIDAhPT1kJiYoZ1tiXT1kKSk7cmV0
dXJuIGd9LG4uZXh0ZW5kKHtleHBhbmRvOiJqUXVlcnkiKyhtK01hdGgucmFuZG9t
KCkpLnJlcGxhY2UoL1xEL2csIiIpLGlzUmVhZHk6ITAsZXJyb3I6ZnVuY3Rpb24o
YSl7dGhyb3cgbmV3IEVycm9yKGEpfSxub29wOmZ1bmN0aW9uKCl7fSxpc0Z1bmN0
aW9uOmZ1bmN0aW9uKGEpe3JldHVybiJmdW5jdGlvbiI9PT1uLnR5cGUoYSl9LGlz
QXJyYXk6QXJyYXkuaXNBcnJheSxpc1dpbmRvdzpmdW5jdGlvbihhKXtyZXR1cm4g
bnVsbCE9YSYmYT09PWEud2luZG93fSxpc051bWVyaWM6ZnVuY3Rpb24oYSl7cmV0
dXJuIW4uaXNBcnJheShhKSYmYS1wYXJzZUZsb2F0KGEpPj0wfSxpc1BsYWluT2Jq
ZWN0OmZ1bmN0aW9uKGEpe3JldHVybiJvYmplY3QiIT09bi50eXBlKGEpfHxhLm5v
ZGVUeXBlfHxuLmlzV2luZG93KGEpPyExOmEuY29uc3RydWN0b3ImJiFqLmNhbGwo
YS5jb25zdHJ1Y3Rvci5wcm90b3R5cGUsImlzUHJvdG90eXBlT2YiKT8hMTohMH0s
aXNFbXB0eU9iamVjdDpmdW5jdGlvbihhKXt2YXIgYjtmb3IoYiBpbiBhKXJldHVy
biExO3JldHVybiEwfSx0eXBlOmZ1bmN0aW9uKGEpe3JldHVybiBudWxsPT1hP2Er
IiI6Im9iamVjdCI9PXR5cGVvZiBhfHwiZnVuY3Rpb24iPT10eXBlb2YgYT9oW2ku
Y2FsbChhKV18fCJvYmplY3QiOnR5cGVvZiBhfSxnbG9iYWxFdmFsOmZ1bmN0aW9u
KGEpe3ZhciBiLGM9ZXZhbDthPW4udHJpbShhKSxhJiYoMT09PWEuaW5kZXhPZigi
dXNlIHN0cmljdCIpPyhiPWwuY3JlYXRlRWxlbWVudCgic2NyaXB0IiksYi50ZXh0
PWEsbC5oZWFkLmFwcGVuZENoaWxkKGIpLnBhcmVudE5vZGUucmVtb3ZlQ2hpbGQo
YikpOmMoYSkpfSxjYW1lbENhc2U6ZnVuY3Rpb24oYSl7cmV0dXJuIGEucmVwbGFj
ZShwLCJtcy0iKS5yZXBsYWNlKHEscil9LG5vZGVOYW1lOmZ1bmN0aW9uKGEsYil7
cmV0dXJuIGEubm9kZU5hbWUmJmEubm9kZU5hbWUudG9Mb3dlckNhc2UoKT09PWIu
dG9Mb3dlckNhc2UoKX0sZWFjaDpmdW5jdGlvbihhLGIsYyl7dmFyIGQsZT0wLGY9
YS5sZW5ndGgsZz1zKGEpO2lmKGMpe2lmKGcpe2Zvcig7Zj5lO2UrKylpZihkPWIu
YXBwbHkoYVtlXSxjKSxkPT09ITEpYnJlYWt9ZWxzZSBmb3IoZSBpbiBhKWlmKGQ9
Yi5hcHBseShhW2VdLGMpLGQ9PT0hMSlicmVha31lbHNlIGlmKGcpe2Zvcig7Zj5l
O2UrKylpZihkPWIuY2FsbChhW2VdLGUsYVtlXSksZD09PSExKWJyZWFrfWVsc2Ug
Zm9yKGUgaW4gYSlpZihkPWIuY2FsbChhW2VdLGUsYVtlXSksZD09PSExKWJyZWFr
O3JldHVybiBhfSx0cmltOmZ1bmN0aW9uKGEpe3JldHVybiBudWxsPT1hPyIiOihh
KyIiKS5yZXBsYWNlKG8sIiIpfSxtYWtlQXJyYXk6ZnVuY3Rpb24oYSxiKXt2YXIg
Yz1ifHxbXTtyZXR1cm4gbnVsbCE9YSYmKHMoT2JqZWN0KGEpKT9uLm1lcmdlKGMs
InN0cmluZyI9PXR5cGVvZiBhP1thXTphKTpmLmNhbGwoYyxhKSksY30saW5BcnJh
eTpmdW5jdGlvbihhLGIsYyl7cmV0dXJuIG51bGw9PWI/LTE6Zy5jYWxsKGIsYSxj
KX0sbWVyZ2U6ZnVuY3Rpb24oYSxiKXtmb3IodmFyIGM9K2IubGVuZ3RoLGQ9MCxl
PWEubGVuZ3RoO2M+ZDtkKyspYVtlKytdPWJbZF07cmV0dXJuIGEubGVuZ3RoPWUs
YX0sZ3JlcDpmdW5jdGlvbihhLGIsYyl7Zm9yKHZhciBkLGU9W10sZj0wLGc9YS5s
ZW5ndGgsaD0hYztnPmY7ZisrKWQ9IWIoYVtmXSxmKSxkIT09aCYmZS5wdXNoKGFb
Zl0pO3JldHVybiBlfSxtYXA6ZnVuY3Rpb24oYSxiLGMpe3ZhciBkLGY9MCxnPWEu
bGVuZ3RoLGg9cyhhKSxpPVtdO2lmKGgpZm9yKDtnPmY7ZisrKWQ9YihhW2ZdLGYs
YyksbnVsbCE9ZCYmaS5wdXNoKGQpO2Vsc2UgZm9yKGYgaW4gYSlkPWIoYVtmXSxm
LGMpLG51bGwhPWQmJmkucHVzaChkKTtyZXR1cm4gZS5hcHBseShbXSxpKX0sZ3Vp
ZDoxLHByb3h5OmZ1bmN0aW9uKGEsYil7dmFyIGMsZSxmO3JldHVybiJzdHJpbmci
PT10eXBlb2YgYiYmKGM9YVtiXSxiPWEsYT1jKSxuLmlzRnVuY3Rpb24oYSk/KGU9
ZC5jYWxsKGFyZ3VtZW50cywyKSxmPWZ1bmN0aW9uKCl7cmV0dXJuIGEuYXBwbHko
Ynx8dGhpcyxlLmNvbmNhdChkLmNhbGwoYXJndW1lbnRzKSkpfSxmLmd1aWQ9YS5n
dWlkPWEuZ3VpZHx8bi5ndWlkKyssZik6dm9pZCAwfSxub3c6RGF0ZS5ub3csc3Vw
cG9ydDprfSksbi5lYWNoKCJCb29sZWFuIE51bWJlciBTdHJpbmcgRnVuY3Rpb24g
QXJyYXkgRGF0ZSBSZWdFeHAgT2JqZWN0IEVycm9yIi5zcGxpdCgiICIpLGZ1bmN0
aW9uKGEsYil7aFsiW29iamVjdCAiK2IrIl0iXT1iLnRvTG93ZXJDYXNlKCl9KTtm
dW5jdGlvbiBzKGEpe3ZhciBiPWEubGVuZ3RoLGM9bi50eXBlKGEpO3JldHVybiJm
dW5jdGlvbiI9PT1jfHxuLmlzV2luZG93KGEpPyExOjE9PT1hLm5vZGVUeXBlJiZi
PyEwOiJhcnJheSI9PT1jfHwwPT09Ynx8Im51bWJlciI9PXR5cGVvZiBiJiZiPjAm
JmItMSBpbiBhfXZhciB0PWZ1bmN0aW9uKGEpe3ZhciBiLGMsZCxlLGYsZyxoLGks
aixrLGwsbSxuLG8scCxxLHIscyx0LHU9InNpenpsZSIrLW5ldyBEYXRlLHY9YS5k
b2N1bWVudCx3PTAseD0wLHk9Z2IoKSx6PWdiKCksQT1nYigpLEI9ZnVuY3Rpb24o
YSxiKXtyZXR1cm4gYT09PWImJihsPSEwKSwwfSxDPSJ1bmRlZmluZWQiLEQ9MTw8
MzEsRT17fS5oYXNPd25Qcm9wZXJ0eSxGPVtdLEc9Ri5wb3AsSD1GLnB1c2gsST1G
LnB1c2gsSj1GLnNsaWNlLEs9Ri5pbmRleE9mfHxmdW5jdGlvbihhKXtmb3IodmFy
IGI9MCxjPXRoaXMubGVuZ3RoO2M+YjtiKyspaWYodGhpc1tiXT09PWEpcmV0dXJu
IGI7cmV0dXJuLTF9LEw9ImNoZWNrZWR8c2VsZWN0ZWR8YXN5bmN8YXV0b2ZvY3Vz
fGF1dG9wbGF5fGNvbnRyb2xzfGRlZmVyfGRpc2FibGVkfGhpZGRlbnxpc21hcHxs
b29wfG11bHRpcGxlfG9wZW58cmVhZG9ubHl8cmVxdWlyZWR8c2NvcGVkIixNPSJb
XFx4MjBcXHRcXHJcXG5cXGZdIixOPSIoPzpcXFxcLnxbXFx3LV18W15cXHgwMC1c
XHhhMF0pKyIsTz1OLnJlcGxhY2UoInciLCJ3IyIpLFA9IlxcWyIrTSsiKigiK04r
IikoPzoiK00rIiooWypeJHwhfl0/PSkiK00rIiooPzonKCg/OlxcXFwufFteXFxc
XCddKSopJ3xcIigoPzpcXFxcLnxbXlxcXFxcIl0pKilcInwoIitPKyIpKXwpIitN
KyIqXFxdIixRPSI6KCIrTisiKSg/OlxcKCgoJygoPzpcXFxcLnxbXlxcXFwnXSkq
KSd8XCIoKD86XFxcXC58W15cXFxcXCJdKSopXCIpfCgoPzpcXFxcLnxbXlxcXFwo
KVtcXF1dfCIrUCsiKSopfC4qKVxcKXwpIixSPW5ldyBSZWdFeHAoIl4iK00rIit8
KCg/Ol58W15cXFxcXSkoPzpcXFxcLikqKSIrTSsiKyQiLCJnIiksUz1uZXcgUmVn
RXhwKCJeIitNKyIqLCIrTSsiKiIpLFQ9bmV3IFJlZ0V4cCgiXiIrTSsiKihbPit+
XXwiK00rIikiK00rIioiKSxVPW5ldyBSZWdFeHAoIj0iK00rIiooW15cXF0nXCJd
Kj8pIitNKyIqXFxdIiwiZyIpLFY9bmV3IFJlZ0V4cChRKSxXPW5ldyBSZWdFeHAo
Il4iK08rIiQiKSxYPXtJRDpuZXcgUmVnRXhwKCJeIygiK04rIikiKSxDTEFTUzpu
ZXcgUmVnRXhwKCJeXFwuKCIrTisiKSIpLFRBRzpuZXcgUmVnRXhwKCJeKCIrTi5y
ZXBsYWNlKCJ3IiwidyoiKSsiKSIpLEFUVFI6bmV3IFJlZ0V4cCgiXiIrUCksUFNF
VURPOm5ldyBSZWdFeHAoIl4iK1EpLENISUxEOm5ldyBSZWdFeHAoIl46KG9ubHl8
Zmlyc3R8bGFzdHxudGh8bnRoLWxhc3QpLShjaGlsZHxvZi10eXBlKSg/OlxcKCIr
TSsiKihldmVufG9kZHwoKFsrLV18KShcXGQqKW58KSIrTSsiKig/OihbKy1dfCki
K00rIiooXFxkKyl8KSkiK00rIipcXCl8KSIsImkiKSxib29sOm5ldyBSZWdFeHAo
Il4oPzoiK0wrIikkIiwiaSIpLG5lZWRzQ29udGV4dDpuZXcgUmVnRXhwKCJeIitN
KyIqWz4rfl18OihldmVufG9kZHxlcXxndHxsdHxudGh8Zmlyc3R8bGFzdCkoPzpc
XCgiK00rIiooKD86LVxcZCk/XFxkKikiK00rIipcXCl8KSg/PVteLV18JCkiLCJp
Iil9LFk9L14oPzppbnB1dHxzZWxlY3R8dGV4dGFyZWF8YnV0dG9uKSQvaSxaPS9e
aFxkJC9pLCQ9L15bXntdK1x7XHMqXFtuYXRpdmUgXHcvLF89L14oPzojKFtcdy1d
Kyl8KFx3Kyl8XC4oW1x3LV0rKSkkLyxhYj0vWyt+XS8sYmI9Lyd8XFwvZyxjYj1u
ZXcgUmVnRXhwKCJcXFxcKFtcXGRhLWZdezEsNn0iK00rIj98KCIrTSsiKXwuKSIs
ImlnIiksZGI9ZnVuY3Rpb24oYSxiLGMpe3ZhciBkPSIweCIrYi02NTUzNjtyZXR1
cm4gZCE9PWR8fGM/YjowPmQ/U3RyaW5nLmZyb21DaGFyQ29kZShkKzY1NTM2KTpT
dHJpbmcuZnJvbUNoYXJDb2RlKGQ+PjEwfDU1Mjk2LDEwMjMmZHw1NjMyMCl9O3Ry
eXtJLmFwcGx5KEY9Si5jYWxsKHYuY2hpbGROb2Rlcyksdi5jaGlsZE5vZGVzKSxG
W3YuY2hpbGROb2Rlcy5sZW5ndGhdLm5vZGVUeXBlfWNhdGNoKGViKXtJPXthcHBs
eTpGLmxlbmd0aD9mdW5jdGlvbihhLGIpe0guYXBwbHkoYSxKLmNhbGwoYikpfTpm
dW5jdGlvbihhLGIpe3ZhciBjPWEubGVuZ3RoLGQ9MDt3aGlsZShhW2MrK109Yltk
KytdKTthLmxlbmd0aD1jLTF9fX1mdW5jdGlvbiBmYihhLGIsZCxlKXt2YXIgZixo
LGosayxsLG8scixzLHcseDtpZigoYj9iLm93bmVyRG9jdW1lbnR8fGI6dikhPT1u
JiZtKGIpLGI9Ynx8bixkPWR8fFtdLCFhfHwic3RyaW5nIiE9dHlwZW9mIGEpcmV0
dXJuIGQ7aWYoMSE9PShrPWIubm9kZVR5cGUpJiY5IT09aylyZXR1cm5bXTtpZihw
JiYhZSl7aWYoZj1fLmV4ZWMoYSkpaWYoaj1mWzFdKXtpZig5PT09ayl7aWYoaD1i
LmdldEVsZW1lbnRCeUlkKGopLCFofHwhaC5wYXJlbnROb2RlKXJldHVybiBkO2lm
KGguaWQ9PT1qKXJldHVybiBkLnB1c2goaCksZH1lbHNlIGlmKGIub3duZXJEb2N1
bWVudCYmKGg9Yi5vd25lckRvY3VtZW50LmdldEVsZW1lbnRCeUlkKGopKSYmdChi
LGgpJiZoLmlkPT09ailyZXR1cm4gZC5wdXNoKGgpLGR9ZWxzZXtpZihmWzJdKXJl
dHVybiBJLmFwcGx5KGQsYi5nZXRFbGVtZW50c0J5VGFnTmFtZShhKSksZDtpZigo
aj1mWzNdKSYmYy5nZXRFbGVtZW50c0J5Q2xhc3NOYW1lJiZiLmdldEVsZW1lbnRz
QnlDbGFzc05hbWUpcmV0dXJuIEkuYXBwbHkoZCxiLmdldEVsZW1lbnRzQnlDbGFz
c05hbWUoaikpLGR9aWYoYy5xc2EmJighcXx8IXEudGVzdChhKSkpe2lmKHM9cj11
LHc9Yix4PTk9PT1rJiZhLDE9PT1rJiYib2JqZWN0IiE9PWIubm9kZU5hbWUudG9M
b3dlckNhc2UoKSl7bz1nKGEpLChyPWIuZ2V0QXR0cmlidXRlKCJpZCIpKT9zPXIu
cmVwbGFjZShiYiwiXFwkJiIpOmIuc2V0QXR0cmlidXRlKCJpZCIscykscz0iW2lk
PSciK3MrIiddICIsbD1vLmxlbmd0aDt3aGlsZShsLS0pb1tsXT1zK3FiKG9bbF0p
O3c9YWIudGVzdChhKSYmb2IoYi5wYXJlbnROb2RlKXx8Yix4PW8uam9pbigiLCIp
fWlmKHgpdHJ5e3JldHVybiBJLmFwcGx5KGQsdy5xdWVyeVNlbGVjdG9yQWxsKHgp
KSxkfWNhdGNoKHkpe31maW5hbGx5e3J8fGIucmVtb3ZlQXR0cmlidXRlKCJpZCIp
fX19cmV0dXJuIGkoYS5yZXBsYWNlKFIsIiQxIiksYixkLGUpfWZ1bmN0aW9uIGdi
KCl7dmFyIGE9W107ZnVuY3Rpb24gYihjLGUpe3JldHVybiBhLnB1c2goYysiICIp
PmQuY2FjaGVMZW5ndGgmJmRlbGV0ZSBiW2Euc2hpZnQoKV0sYltjKyIgIl09ZX1y
ZXR1cm4gYn1mdW5jdGlvbiBoYihhKXtyZXR1cm4gYVt1XT0hMCxhfWZ1bmN0aW9u
IGliKGEpe3ZhciBiPW4uY3JlYXRlRWxlbWVudCgiZGl2Iik7dHJ5e3JldHVybiEh
YShiKX1jYXRjaChjKXtyZXR1cm4hMX1maW5hbGx5e2IucGFyZW50Tm9kZSYmYi5w
YXJlbnROb2RlLnJlbW92ZUNoaWxkKGIpLGI9bnVsbH19ZnVuY3Rpb24gamIoYSxi
KXt2YXIgYz1hLnNwbGl0KCJ8IiksZT1hLmxlbmd0aDt3aGlsZShlLS0pZC5hdHRy
SGFuZGxlW2NbZV1dPWJ9ZnVuY3Rpb24ga2IoYSxiKXt2YXIgYz1iJiZhLGQ9YyYm
MT09PWEubm9kZVR5cGUmJjE9PT1iLm5vZGVUeXBlJiYofmIuc291cmNlSW5kZXh8
fEQpLSh+YS5zb3VyY2VJbmRleHx8RCk7aWYoZClyZXR1cm4gZDtpZihjKXdoaWxl
KGM9Yy5uZXh0U2libGluZylpZihjPT09YilyZXR1cm4tMTtyZXR1cm4gYT8xOi0x
fWZ1bmN0aW9uIGxiKGEpe3JldHVybiBmdW5jdGlvbihiKXt2YXIgYz1iLm5vZGVO
YW1lLnRvTG93ZXJDYXNlKCk7cmV0dXJuImlucHV0Ij09PWMmJmIudHlwZT09PWF9
fWZ1bmN0aW9uIG1iKGEpe3JldHVybiBmdW5jdGlvbihiKXt2YXIgYz1iLm5vZGVO
YW1lLnRvTG93ZXJDYXNlKCk7cmV0dXJuKCJpbnB1dCI9PT1jfHwiYnV0dG9uIj09
PWMpJiZiLnR5cGU9PT1hfX1mdW5jdGlvbiBuYihhKXtyZXR1cm4gaGIoZnVuY3Rp
b24oYil7cmV0dXJuIGI9K2IsaGIoZnVuY3Rpb24oYyxkKXt2YXIgZSxmPWEoW10s
Yy5sZW5ndGgsYiksZz1mLmxlbmd0aDt3aGlsZShnLS0pY1tlPWZbZ11dJiYoY1tl
XT0hKGRbZV09Y1tlXSkpfSl9KX1mdW5jdGlvbiBvYihhKXtyZXR1cm4gYSYmdHlw
ZW9mIGEuZ2V0RWxlbWVudHNCeVRhZ05hbWUhPT1DJiZhfWM9ZmIuc3VwcG9ydD17
fSxmPWZiLmlzWE1MPWZ1bmN0aW9uKGEpe3ZhciBiPWEmJihhLm93bmVyRG9jdW1l
bnR8fGEpLmRvY3VtZW50RWxlbWVudDtyZXR1cm4gYj8iSFRNTCIhPT1iLm5vZGVO
YW1lOiExfSxtPWZiLnNldERvY3VtZW50PWZ1bmN0aW9uKGEpe3ZhciBiLGU9YT9h
Lm93bmVyRG9jdW1lbnR8fGE6dixnPWUuZGVmYXVsdFZpZXc7cmV0dXJuIGUhPT1u
JiY5PT09ZS5ub2RlVHlwZSYmZS5kb2N1bWVudEVsZW1lbnQ/KG49ZSxvPWUuZG9j
dW1lbnRFbGVtZW50LHA9IWYoZSksZyYmZyE9PWcudG9wJiYoZy5hZGRFdmVudExp
c3RlbmVyP2cuYWRkRXZlbnRMaXN0ZW5lcigidW5sb2FkIixmdW5jdGlvbigpe20o
KX0sITEpOmcuYXR0YWNoRXZlbnQmJmcuYXR0YWNoRXZlbnQoIm9udW5sb2FkIixm
dW5jdGlvbigpe20oKX0pKSxjLmF0dHJpYnV0ZXM9aWIoZnVuY3Rpb24oYSl7cmV0
dXJuIGEuY2xhc3NOYW1lPSJpIiwhYS5nZXRBdHRyaWJ1dGUoImNsYXNzTmFtZSIp
fSksYy5nZXRFbGVtZW50c0J5VGFnTmFtZT1pYihmdW5jdGlvbihhKXtyZXR1cm4g
YS5hcHBlbmRDaGlsZChlLmNyZWF0ZUNvbW1lbnQoIiIpKSwhYS5nZXRFbGVtZW50
c0J5VGFnTmFtZSgiKiIpLmxlbmd0aH0pLGMuZ2V0RWxlbWVudHNCeUNsYXNzTmFt
ZT0kLnRlc3QoZS5nZXRFbGVtZW50c0J5Q2xhc3NOYW1lKSYmaWIoZnVuY3Rpb24o
YSl7cmV0dXJuIGEuaW5uZXJIVE1MPSI8ZGl2IGNsYXNzPSdhJz48L2Rpdj48ZGl2
IGNsYXNzPSdhIGknPjwvZGl2PiIsYS5maXJzdENoaWxkLmNsYXNzTmFtZT0iaSIs
Mj09PWEuZ2V0RWxlbWVudHNCeUNsYXNzTmFtZSgiaSIpLmxlbmd0aH0pLGMuZ2V0
QnlJZD1pYihmdW5jdGlvbihhKXtyZXR1cm4gby5hcHBlbmRDaGlsZChhKS5pZD11
LCFlLmdldEVsZW1lbnRzQnlOYW1lfHwhZS5nZXRFbGVtZW50c0J5TmFtZSh1KS5s
ZW5ndGh9KSxjLmdldEJ5SWQ/KGQuZmluZC5JRD1mdW5jdGlvbihhLGIpe2lmKHR5
cGVvZiBiLmdldEVsZW1lbnRCeUlkIT09QyYmcCl7dmFyIGM9Yi5nZXRFbGVtZW50
QnlJZChhKTtyZXR1cm4gYyYmYy5wYXJlbnROb2RlP1tjXTpbXX19LGQuZmlsdGVy
LklEPWZ1bmN0aW9uKGEpe3ZhciBiPWEucmVwbGFjZShjYixkYik7cmV0dXJuIGZ1
bmN0aW9uKGEpe3JldHVybiBhLmdldEF0dHJpYnV0ZSgiaWQiKT09PWJ9fSk6KGRl
bGV0ZSBkLmZpbmQuSUQsZC5maWx0ZXIuSUQ9ZnVuY3Rpb24oYSl7dmFyIGI9YS5y
ZXBsYWNlKGNiLGRiKTtyZXR1cm4gZnVuY3Rpb24oYSl7dmFyIGM9dHlwZW9mIGEu
Z2V0QXR0cmlidXRlTm9kZSE9PUMmJmEuZ2V0QXR0cmlidXRlTm9kZSgiaWQiKTty
ZXR1cm4gYyYmYy52YWx1ZT09PWJ9fSksZC5maW5kLlRBRz1jLmdldEVsZW1lbnRz
QnlUYWdOYW1lP2Z1bmN0aW9uKGEsYil7cmV0dXJuIHR5cGVvZiBiLmdldEVsZW1l
bnRzQnlUYWdOYW1lIT09Qz9iLmdldEVsZW1lbnRzQnlUYWdOYW1lKGEpOnZvaWQg
MH06ZnVuY3Rpb24oYSxiKXt2YXIgYyxkPVtdLGU9MCxmPWIuZ2V0RWxlbWVudHNC
eVRhZ05hbWUoYSk7aWYoIioiPT09YSl7d2hpbGUoYz1mW2UrK10pMT09PWMubm9k
ZVR5cGUmJmQucHVzaChjKTtyZXR1cm4gZH1yZXR1cm4gZn0sZC5maW5kLkNMQVNT
PWMuZ2V0RWxlbWVudHNCeUNsYXNzTmFtZSYmZnVuY3Rpb24oYSxiKXtyZXR1cm4g
dHlwZW9mIGIuZ2V0RWxlbWVudHNCeUNsYXNzTmFtZSE9PUMmJnA/Yi5nZXRFbGVt
ZW50c0J5Q2xhc3NOYW1lKGEpOnZvaWQgMH0scj1bXSxxPVtdLChjLnFzYT0kLnRl
c3QoZS5xdWVyeVNlbGVjdG9yQWxsKSkmJihpYihmdW5jdGlvbihhKXthLmlubmVy
SFRNTD0iPHNlbGVjdCBtc2FsbG93Y2xpcD0nJz48b3B0aW9uIHNlbGVjdGVkPScn
Pjwvb3B0aW9uPjwvc2VsZWN0PiIsYS5xdWVyeVNlbGVjdG9yQWxsKCJbbXNhbGxv
d2NsaXBePScnXSIpLmxlbmd0aCYmcS5wdXNoKCJbKl4kXT0iK00rIiooPzonJ3xc
IlwiKSIpLGEucXVlcnlTZWxlY3RvckFsbCgiW3NlbGVjdGVkXSIpLmxlbmd0aHx8
cS5wdXNoKCJcXFsiK00rIiooPzp2YWx1ZXwiK0wrIikiKSxhLnF1ZXJ5U2VsZWN0
b3JBbGwoIjpjaGVja2VkIikubGVuZ3RofHxxLnB1c2goIjpjaGVja2VkIil9KSxp
YihmdW5jdGlvbihhKXt2YXIgYj1lLmNyZWF0ZUVsZW1lbnQoImlucHV0Iik7Yi5z
ZXRBdHRyaWJ1dGUoInR5cGUiLCJoaWRkZW4iKSxhLmFwcGVuZENoaWxkKGIpLnNl
dEF0dHJpYnV0ZSgibmFtZSIsIkQiKSxhLnF1ZXJ5U2VsZWN0b3JBbGwoIltuYW1l
PWRdIikubGVuZ3RoJiZxLnB1c2goIm5hbWUiK00rIipbKl4kfCF+XT89IiksYS5x
dWVyeVNlbGVjdG9yQWxsKCI6ZW5hYmxlZCIpLmxlbmd0aHx8cS5wdXNoKCI6ZW5h
YmxlZCIsIjpkaXNhYmxlZCIpLGEucXVlcnlTZWxlY3RvckFsbCgiKiw6eCIpLHEu
cHVzaCgiLC4qOiIpfSkpLChjLm1hdGNoZXNTZWxlY3Rvcj0kLnRlc3Qocz1vLm1h
dGNoZXN8fG8ud2Via2l0TWF0Y2hlc1NlbGVjdG9yfHxvLm1vek1hdGNoZXNTZWxl
Y3Rvcnx8by5vTWF0Y2hlc1NlbGVjdG9yfHxvLm1zTWF0Y2hlc1NlbGVjdG9yKSkm
JmliKGZ1bmN0aW9uKGEpe2MuZGlzY29ubmVjdGVkTWF0Y2g9cy5jYWxsKGEsImRp
diIpLHMuY2FsbChhLCJbcyE9JyddOngiKSxyLnB1c2goIiE9IixRKX0pLHE9cS5s
ZW5ndGgmJm5ldyBSZWdFeHAocS5qb2luKCJ8IikpLHI9ci5sZW5ndGgmJm5ldyBS
ZWdFeHAoci5qb2luKCJ8IikpLGI9JC50ZXN0KG8uY29tcGFyZURvY3VtZW50UG9z
aXRpb24pLHQ9Ynx8JC50ZXN0KG8uY29udGFpbnMpP2Z1bmN0aW9uKGEsYil7dmFy
IGM9OT09PWEubm9kZVR5cGU/YS5kb2N1bWVudEVsZW1lbnQ6YSxkPWImJmIucGFy
ZW50Tm9kZTtyZXR1cm4gYT09PWR8fCEoIWR8fDEhPT1kLm5vZGVUeXBlfHwhKGMu
Y29udGFpbnM/Yy5jb250YWlucyhkKTphLmNvbXBhcmVEb2N1bWVudFBvc2l0aW9u
JiYxNiZhLmNvbXBhcmVEb2N1bWVudFBvc2l0aW9uKGQpKSl9OmZ1bmN0aW9uKGEs
Yil7aWYoYil3aGlsZShiPWIucGFyZW50Tm9kZSlpZihiPT09YSlyZXR1cm4hMDty
ZXR1cm4hMX0sQj1iP2Z1bmN0aW9uKGEsYil7aWYoYT09PWIpcmV0dXJuIGw9ITAs
MDt2YXIgZD0hYS5jb21wYXJlRG9jdW1lbnRQb3NpdGlvbi0hYi5jb21wYXJlRG9j
dW1lbnRQb3NpdGlvbjtyZXR1cm4gZD9kOihkPShhLm93bmVyRG9jdW1lbnR8fGEp
PT09KGIub3duZXJEb2N1bWVudHx8Yik/YS5jb21wYXJlRG9jdW1lbnRQb3NpdGlv
bihiKToxLDEmZHx8IWMuc29ydERldGFjaGVkJiZiLmNvbXBhcmVEb2N1bWVudFBv
c2l0aW9uKGEpPT09ZD9hPT09ZXx8YS5vd25lckRvY3VtZW50PT09diYmdCh2LGEp
Py0xOmI9PT1lfHxiLm93bmVyRG9jdW1lbnQ9PT12JiZ0KHYsYik/MTprP0suY2Fs
bChrLGEpLUsuY2FsbChrLGIpOjA6NCZkPy0xOjEpfTpmdW5jdGlvbihhLGIpe2lm
KGE9PT1iKXJldHVybiBsPSEwLDA7dmFyIGMsZD0wLGY9YS5wYXJlbnROb2RlLGc9
Yi5wYXJlbnROb2RlLGg9W2FdLGk9W2JdO2lmKCFmfHwhZylyZXR1cm4gYT09PWU/
LTE6Yj09PWU/MTpmPy0xOmc/MTprP0suY2FsbChrLGEpLUsuY2FsbChrLGIpOjA7
aWYoZj09PWcpcmV0dXJuIGtiKGEsYik7Yz1hO3doaWxlKGM9Yy5wYXJlbnROb2Rl
KWgudW5zaGlmdChjKTtjPWI7d2hpbGUoYz1jLnBhcmVudE5vZGUpaS51bnNoaWZ0
KGMpO3doaWxlKGhbZF09PT1pW2RdKWQrKztyZXR1cm4gZD9rYihoW2RdLGlbZF0p
OmhbZF09PT12Py0xOmlbZF09PT12PzE6MH0sZSk6bn0sZmIubWF0Y2hlcz1mdW5j
dGlvbihhLGIpe3JldHVybiBmYihhLG51bGwsbnVsbCxiKX0sZmIubWF0Y2hlc1Nl
bGVjdG9yPWZ1bmN0aW9uKGEsYil7aWYoKGEub3duZXJEb2N1bWVudHx8YSkhPT1u
JiZtKGEpLGI9Yi5yZXBsYWNlKFUsIj0nJDEnXSIpLCEoIWMubWF0Y2hlc1NlbGVj
dG9yfHwhcHx8ciYmci50ZXN0KGIpfHxxJiZxLnRlc3QoYikpKXRyeXt2YXIgZD1z
LmNhbGwoYSxiKTtpZihkfHxjLmRpc2Nvbm5lY3RlZE1hdGNofHxhLmRvY3VtZW50
JiYxMSE9PWEuZG9jdW1lbnQubm9kZVR5cGUpcmV0dXJuIGR9Y2F0Y2goZSl7fXJl
dHVybiBmYihiLG4sbnVsbCxbYV0pLmxlbmd0aD4wfSxmYi5jb250YWlucz1mdW5j
dGlvbihhLGIpe3JldHVybihhLm93bmVyRG9jdW1lbnR8fGEpIT09biYmbShhKSx0
KGEsYil9LGZiLmF0dHI9ZnVuY3Rpb24oYSxiKXsoYS5vd25lckRvY3VtZW50fHxh
KSE9PW4mJm0oYSk7dmFyIGU9ZC5hdHRySGFuZGxlW2IudG9Mb3dlckNhc2UoKV0s
Zj1lJiZFLmNhbGwoZC5hdHRySGFuZGxlLGIudG9Mb3dlckNhc2UoKSk/ZShhLGIs
IXApOnZvaWQgMDtyZXR1cm4gdm9pZCAwIT09Zj9mOmMuYXR0cmlidXRlc3x8IXA/
YS5nZXRBdHRyaWJ1dGUoYik6KGY9YS5nZXRBdHRyaWJ1dGVOb2RlKGIpKSYmZi5z
cGVjaWZpZWQ/Zi52YWx1ZTpudWxsfSxmYi5lcnJvcj1mdW5jdGlvbihhKXt0aHJv
dyBuZXcgRXJyb3IoIlN5bnRheCBlcnJvciwgdW5yZWNvZ25pemVkIGV4cHJlc3Np
b246ICIrYSl9LGZiLnVuaXF1ZVNvcnQ9ZnVuY3Rpb24oYSl7dmFyIGIsZD1bXSxl
PTAsZj0wO2lmKGw9IWMuZGV0ZWN0RHVwbGljYXRlcyxrPSFjLnNvcnRTdGFibGUm
JmEuc2xpY2UoMCksYS5zb3J0KEIpLGwpe3doaWxlKGI9YVtmKytdKWI9PT1hW2Zd
JiYoZT1kLnB1c2goZikpO3doaWxlKGUtLSlhLnNwbGljZShkW2VdLDEpfXJldHVy
biBrPW51bGwsYX0sZT1mYi5nZXRUZXh0PWZ1bmN0aW9uKGEpe3ZhciBiLGM9IiIs
ZD0wLGY9YS5ub2RlVHlwZTtpZihmKXtpZigxPT09Znx8OT09PWZ8fDExPT09Zil7
aWYoInN0cmluZyI9PXR5cGVvZiBhLnRleHRDb250ZW50KXJldHVybiBhLnRleHRD
b250ZW50O2ZvcihhPWEuZmlyc3RDaGlsZDthO2E9YS5uZXh0U2libGluZyljKz1l
KGEpfWVsc2UgaWYoMz09PWZ8fDQ9PT1mKXJldHVybiBhLm5vZGVWYWx1ZX1lbHNl
IHdoaWxlKGI9YVtkKytdKWMrPWUoYik7cmV0dXJuIGN9LGQ9ZmIuc2VsZWN0b3Jz
PXtjYWNoZUxlbmd0aDo1MCxjcmVhdGVQc2V1ZG86aGIsbWF0Y2g6WCxhdHRySGFu
ZGxlOnt9LGZpbmQ6e30scmVsYXRpdmU6eyI+Ijp7ZGlyOiJwYXJlbnROb2RlIixm
aXJzdDohMH0sIiAiOntkaXI6InBhcmVudE5vZGUifSwiKyI6e2RpcjoicHJldmlv
dXNTaWJsaW5nIixmaXJzdDohMH0sIn4iOntkaXI6InByZXZpb3VzU2libGluZyJ9
fSxwcmVGaWx0ZXI6e0FUVFI6ZnVuY3Rpb24oYSl7cmV0dXJuIGFbMV09YVsxXS5y
ZXBsYWNlKGNiLGRiKSxhWzNdPShhWzNdfHxhWzRdfHxhWzVdfHwiIikucmVwbGFj
ZShjYixkYiksIn49Ij09PWFbMl0mJihhWzNdPSIgIithWzNdKyIgIiksYS5zbGlj
ZSgwLDQpfSxDSElMRDpmdW5jdGlvbihhKXtyZXR1cm4gYVsxXT1hWzFdLnRvTG93
ZXJDYXNlKCksIm50aCI9PT1hWzFdLnNsaWNlKDAsMyk/KGFbM118fGZiLmVycm9y
KGFbMF0pLGFbNF09KyhhWzRdP2FbNV0rKGFbNl18fDEpOjIqKCJldmVuIj09PWFb
M118fCJvZGQiPT09YVszXSkpLGFbNV09KyhhWzddK2FbOF18fCJvZGQiPT09YVsz
XSkpOmFbM10mJmZiLmVycm9yKGFbMF0pLGF9LFBTRVVETzpmdW5jdGlvbihhKXt2
YXIgYixjPSFhWzZdJiZhWzJdO3JldHVybiBYLkNISUxELnRlc3QoYVswXSk/bnVs
bDooYVszXT9hWzJdPWFbNF18fGFbNV18fCIiOmMmJlYudGVzdChjKSYmKGI9Zyhj
LCEwKSkmJihiPWMuaW5kZXhPZigiKSIsYy5sZW5ndGgtYiktYy5sZW5ndGgpJiYo
YVswXT1hWzBdLnNsaWNlKDAsYiksYVsyXT1jLnNsaWNlKDAsYikpLGEuc2xpY2Uo
MCwzKSl9fSxmaWx0ZXI6e1RBRzpmdW5jdGlvbihhKXt2YXIgYj1hLnJlcGxhY2Uo
Y2IsZGIpLnRvTG93ZXJDYXNlKCk7cmV0dXJuIioiPT09YT9mdW5jdGlvbigpe3Jl
dHVybiEwfTpmdW5jdGlvbihhKXtyZXR1cm4gYS5ub2RlTmFtZSYmYS5ub2RlTmFt
ZS50b0xvd2VyQ2FzZSgpPT09Yn19LENMQVNTOmZ1bmN0aW9uKGEpe3ZhciBiPXlb
YSsiICJdO3JldHVybiBifHwoYj1uZXcgUmVnRXhwKCIoXnwiK00rIikiK2ErIigi
K00rInwkKSIpKSYmeShhLGZ1bmN0aW9uKGEpe3JldHVybiBiLnRlc3QoInN0cmlu
ZyI9PXR5cGVvZiBhLmNsYXNzTmFtZSYmYS5jbGFzc05hbWV8fHR5cGVvZiBhLmdl
dEF0dHJpYnV0ZSE9PUMmJmEuZ2V0QXR0cmlidXRlKCJjbGFzcyIpfHwiIil9KX0s
QVRUUjpmdW5jdGlvbihhLGIsYyl7cmV0dXJuIGZ1bmN0aW9uKGQpe3ZhciBlPWZi
LmF0dHIoZCxhKTtyZXR1cm4gbnVsbD09ZT8iIT0iPT09YjpiPyhlKz0iIiwiPSI9
PT1iP2U9PT1jOiIhPSI9PT1iP2UhPT1jOiJePSI9PT1iP2MmJjA9PT1lLmluZGV4
T2YoYyk6Iio9Ij09PWI/YyYmZS5pbmRleE9mKGMpPi0xOiIkPSI9PT1iP2MmJmUu
c2xpY2UoLWMubGVuZ3RoKT09PWM6In49Ij09PWI/KCIgIitlKyIgIikuaW5kZXhP
ZihjKT4tMToifD0iPT09Yj9lPT09Y3x8ZS5zbGljZSgwLGMubGVuZ3RoKzEpPT09
YysiLSI6ITEpOiEwfX0sQ0hJTEQ6ZnVuY3Rpb24oYSxiLGMsZCxlKXt2YXIgZj0i
bnRoIiE9PWEuc2xpY2UoMCwzKSxnPSJsYXN0IiE9PWEuc2xpY2UoLTQpLGg9Im9m
LXR5cGUiPT09YjtyZXR1cm4gMT09PWQmJjA9PT1lP2Z1bmN0aW9uKGEpe3JldHVy
biEhYS5wYXJlbnROb2RlfTpmdW5jdGlvbihiLGMsaSl7dmFyIGosayxsLG0sbixv
LHA9ZiE9PWc/Im5leHRTaWJsaW5nIjoicHJldmlvdXNTaWJsaW5nIixxPWIucGFy
ZW50Tm9kZSxyPWgmJmIubm9kZU5hbWUudG9Mb3dlckNhc2UoKSxzPSFpJiYhaDtp
ZihxKXtpZihmKXt3aGlsZShwKXtsPWI7d2hpbGUobD1sW3BdKWlmKGg/bC5ub2Rl
TmFtZS50b0xvd2VyQ2FzZSgpPT09cjoxPT09bC5ub2RlVHlwZSlyZXR1cm4hMTtv
PXA9Im9ubHkiPT09YSYmIW8mJiJuZXh0U2libGluZyJ9cmV0dXJuITB9aWYobz1b
Zz9xLmZpcnN0Q2hpbGQ6cS5sYXN0Q2hpbGRdLGcmJnMpe2s9cVt1XXx8KHFbdV09
e30pLGo9a1thXXx8W10sbj1qWzBdPT09dyYmalsxXSxtPWpbMF09PT13JiZqWzJd
LGw9biYmcS5jaGlsZE5vZGVzW25dO3doaWxlKGw9KytuJiZsJiZsW3BdfHwobT1u
PTApfHxvLnBvcCgpKWlmKDE9PT1sLm5vZGVUeXBlJiYrK20mJmw9PT1iKXtrW2Fd
PVt3LG4sbV07YnJlYWt9fWVsc2UgaWYocyYmKGo9KGJbdV18fChiW3VdPXt9KSlb
YV0pJiZqWzBdPT09dyltPWpbMV07ZWxzZSB3aGlsZShsPSsrbiYmbCYmbFtwXXx8
KG09bj0wKXx8by5wb3AoKSlpZigoaD9sLm5vZGVOYW1lLnRvTG93ZXJDYXNlKCk9
PT1yOjE9PT1sLm5vZGVUeXBlKSYmKyttJiYocyYmKChsW3VdfHwobFt1XT17fSkp
W2FdPVt3LG1dKSxsPT09YikpYnJlYWs7cmV0dXJuIG0tPWUsbT09PWR8fG0lZD09
PTAmJm0vZD49MH19fSxQU0VVRE86ZnVuY3Rpb24oYSxiKXt2YXIgYyxlPWQucHNl
dWRvc1thXXx8ZC5zZXRGaWx0ZXJzW2EudG9Mb3dlckNhc2UoKV18fGZiLmVycm9y
KCJ1bnN1cHBvcnRlZCBwc2V1ZG86ICIrYSk7cmV0dXJuIGVbdV0/ZShiKTplLmxl
bmd0aD4xPyhjPVthLGEsIiIsYl0sZC5zZXRGaWx0ZXJzLmhhc093blByb3BlcnR5
KGEudG9Mb3dlckNhc2UoKSk/aGIoZnVuY3Rpb24oYSxjKXt2YXIgZCxmPWUoYSxi
KSxnPWYubGVuZ3RoO3doaWxlKGctLSlkPUsuY2FsbChhLGZbZ10pLGFbZF09IShj
W2RdPWZbZ10pfSk6ZnVuY3Rpb24oYSl7cmV0dXJuIGUoYSwwLGMpfSk6ZX19LHBz
ZXVkb3M6e25vdDpoYihmdW5jdGlvbihhKXt2YXIgYj1bXSxjPVtdLGQ9aChhLnJl
cGxhY2UoUiwiJDEiKSk7cmV0dXJuIGRbdV0/aGIoZnVuY3Rpb24oYSxiLGMsZSl7
dmFyIGYsZz1kKGEsbnVsbCxlLFtdKSxoPWEubGVuZ3RoO3doaWxlKGgtLSkoZj1n
W2hdKSYmKGFbaF09IShiW2hdPWYpKX0pOmZ1bmN0aW9uKGEsZSxmKXtyZXR1cm4g
YlswXT1hLGQoYixudWxsLGYsYyksIWMucG9wKCl9fSksaGFzOmhiKGZ1bmN0aW9u
KGEpe3JldHVybiBmdW5jdGlvbihiKXtyZXR1cm4gZmIoYSxiKS5sZW5ndGg+MH19
KSxjb250YWluczpoYihmdW5jdGlvbihhKXtyZXR1cm4gZnVuY3Rpb24oYil7cmV0
dXJuKGIudGV4dENvbnRlbnR8fGIuaW5uZXJUZXh0fHxlKGIpKS5pbmRleE9mKGEp
Pi0xfX0pLGxhbmc6aGIoZnVuY3Rpb24oYSl7cmV0dXJuIFcudGVzdChhfHwiIil8
fGZiLmVycm9yKCJ1bnN1cHBvcnRlZCBsYW5nOiAiK2EpLGE9YS5yZXBsYWNlKGNi
LGRiKS50b0xvd2VyQ2FzZSgpLGZ1bmN0aW9uKGIpe3ZhciBjO2RvIGlmKGM9cD9i
Lmxhbmc6Yi5nZXRBdHRyaWJ1dGUoInhtbDpsYW5nIil8fGIuZ2V0QXR0cmlidXRl
KCJsYW5nIikpcmV0dXJuIGM9Yy50b0xvd2VyQ2FzZSgpLGM9PT1hfHwwPT09Yy5p
bmRleE9mKGErIi0iKTt3aGlsZSgoYj1iLnBhcmVudE5vZGUpJiYxPT09Yi5ub2Rl
VHlwZSk7cmV0dXJuITF9fSksdGFyZ2V0OmZ1bmN0aW9uKGIpe3ZhciBjPWEubG9j
YXRpb24mJmEubG9jYXRpb24uaGFzaDtyZXR1cm4gYyYmYy5zbGljZSgxKT09PWIu
aWR9LHJvb3Q6ZnVuY3Rpb24oYSl7cmV0dXJuIGE9PT1vfSxmb2N1czpmdW5jdGlv
bihhKXtyZXR1cm4gYT09PW4uYWN0aXZlRWxlbWVudCYmKCFuLmhhc0ZvY3VzfHxu
Lmhhc0ZvY3VzKCkpJiYhIShhLnR5cGV8fGEuaHJlZnx8fmEudGFiSW5kZXgpfSxl
bmFibGVkOmZ1bmN0aW9uKGEpe3JldHVybiBhLmRpc2FibGVkPT09ITF9LGRpc2Fi
bGVkOmZ1bmN0aW9uKGEpe3JldHVybiBhLmRpc2FibGVkPT09ITB9LGNoZWNrZWQ6
ZnVuY3Rpb24oYSl7dmFyIGI9YS5ub2RlTmFtZS50b0xvd2VyQ2FzZSgpO3JldHVy
biJpbnB1dCI9PT1iJiYhIWEuY2hlY2tlZHx8Im9wdGlvbiI9PT1iJiYhIWEuc2Vs
ZWN0ZWR9LHNlbGVjdGVkOmZ1bmN0aW9uKGEpe3JldHVybiBhLnBhcmVudE5vZGUm
JmEucGFyZW50Tm9kZS5zZWxlY3RlZEluZGV4LGEuc2VsZWN0ZWQ9PT0hMH0sZW1w
dHk6ZnVuY3Rpb24oYSl7Zm9yKGE9YS5maXJzdENoaWxkO2E7YT1hLm5leHRTaWJs
aW5nKWlmKGEubm9kZVR5cGU8NilyZXR1cm4hMTtyZXR1cm4hMH0scGFyZW50OmZ1
bmN0aW9uKGEpe3JldHVybiFkLnBzZXVkb3MuZW1wdHkoYSl9LGhlYWRlcjpmdW5j
dGlvbihhKXtyZXR1cm4gWi50ZXN0KGEubm9kZU5hbWUpfSxpbnB1dDpmdW5jdGlv
bihhKXtyZXR1cm4gWS50ZXN0KGEubm9kZU5hbWUpfSxidXR0b246ZnVuY3Rpb24o
YSl7dmFyIGI9YS5ub2RlTmFtZS50b0xvd2VyQ2FzZSgpO3JldHVybiJpbnB1dCI9
PT1iJiYiYnV0dG9uIj09PWEudHlwZXx8ImJ1dHRvbiI9PT1ifSx0ZXh0OmZ1bmN0
aW9uKGEpe3ZhciBiO3JldHVybiJpbnB1dCI9PT1hLm5vZGVOYW1lLnRvTG93ZXJD
YXNlKCkmJiJ0ZXh0Ij09PWEudHlwZSYmKG51bGw9PShiPWEuZ2V0QXR0cmlidXRl
KCJ0eXBlIikpfHwidGV4dCI9PT1iLnRvTG93ZXJDYXNlKCkpfSxmaXJzdDpuYihm
dW5jdGlvbigpe3JldHVyblswXX0pLGxhc3Q6bmIoZnVuY3Rpb24oYSxiKXtyZXR1
cm5bYi0xXX0pLGVxOm5iKGZ1bmN0aW9uKGEsYixjKXtyZXR1cm5bMD5jP2MrYjpj
XX0pLGV2ZW46bmIoZnVuY3Rpb24oYSxiKXtmb3IodmFyIGM9MDtiPmM7Yys9Milh
LnB1c2goYyk7cmV0dXJuIGF9KSxvZGQ6bmIoZnVuY3Rpb24oYSxiKXtmb3IodmFy
IGM9MTtiPmM7Yys9MilhLnB1c2goYyk7cmV0dXJuIGF9KSxsdDpuYihmdW5jdGlv
bihhLGIsYyl7Zm9yKHZhciBkPTA+Yz9jK2I6YzstLWQ+PTA7KWEucHVzaChkKTty
ZXR1cm4gYX0pLGd0Om5iKGZ1bmN0aW9uKGEsYixjKXtmb3IodmFyIGQ9MD5jP2Mr
YjpjOysrZDxiOylhLnB1c2goZCk7cmV0dXJuIGF9KX19LGQucHNldWRvcy5udGg9
ZC5wc2V1ZG9zLmVxO2ZvcihiIGlue3JhZGlvOiEwLGNoZWNrYm94OiEwLGZpbGU6
ITAscGFzc3dvcmQ6ITAsaW1hZ2U6ITB9KWQucHNldWRvc1tiXT1sYihiKTtmb3Io
YiBpbntzdWJtaXQ6ITAscmVzZXQ6ITB9KWQucHNldWRvc1tiXT1tYihiKTtmdW5j
dGlvbiBwYigpe31wYi5wcm90b3R5cGU9ZC5maWx0ZXJzPWQucHNldWRvcyxkLnNl
dEZpbHRlcnM9bmV3IHBiLGc9ZmIudG9rZW5pemU9ZnVuY3Rpb24oYSxiKXt2YXIg
YyxlLGYsZyxoLGksaixrPXpbYSsiICJdO2lmKGspcmV0dXJuIGI/MDprLnNsaWNl
KDApO2g9YSxpPVtdLGo9ZC5wcmVGaWx0ZXI7d2hpbGUoaCl7KCFjfHwoZT1TLmV4
ZWMoaCkpKSYmKGUmJihoPWguc2xpY2UoZVswXS5sZW5ndGgpfHxoKSxpLnB1c2go
Zj1bXSkpLGM9ITEsKGU9VC5leGVjKGgpKSYmKGM9ZS5zaGlmdCgpLGYucHVzaCh7
dmFsdWU6Yyx0eXBlOmVbMF0ucmVwbGFjZShSLCIgIil9KSxoPWguc2xpY2UoYy5s
ZW5ndGgpKTtmb3IoZyBpbiBkLmZpbHRlcikhKGU9WFtnXS5leGVjKGgpKXx8altn
XSYmIShlPWpbZ10oZSkpfHwoYz1lLnNoaWZ0KCksZi5wdXNoKHt2YWx1ZTpjLHR5
cGU6ZyxtYXRjaGVzOmV9KSxoPWguc2xpY2UoYy5sZW5ndGgpKTtpZighYylicmVh
a31yZXR1cm4gYj9oLmxlbmd0aDpoP2ZiLmVycm9yKGEpOnooYSxpKS5zbGljZSgw
KX07ZnVuY3Rpb24gcWIoYSl7Zm9yKHZhciBiPTAsYz1hLmxlbmd0aCxkPSIiO2M+
YjtiKyspZCs9YVtiXS52YWx1ZTtyZXR1cm4gZH1mdW5jdGlvbiByYihhLGIsYyl7
dmFyIGQ9Yi5kaXIsZT1jJiYicGFyZW50Tm9kZSI9PT1kLGY9eCsrO3JldHVybiBi
LmZpcnN0P2Z1bmN0aW9uKGIsYyxmKXt3aGlsZShiPWJbZF0paWYoMT09PWIubm9k
ZVR5cGV8fGUpcmV0dXJuIGEoYixjLGYpfTpmdW5jdGlvbihiLGMsZyl7dmFyIGgs
aSxqPVt3LGZdO2lmKGcpe3doaWxlKGI9YltkXSlpZigoMT09PWIubm9kZVR5cGV8
fGUpJiZhKGIsYyxnKSlyZXR1cm4hMH1lbHNlIHdoaWxlKGI9YltkXSlpZigxPT09
Yi5ub2RlVHlwZXx8ZSl7aWYoaT1iW3VdfHwoYlt1XT17fSksKGg9aVtkXSkmJmhb
MF09PT13JiZoWzFdPT09ZilyZXR1cm4galsyXT1oWzJdO2lmKGlbZF09aixqWzJd
PWEoYixjLGcpKXJldHVybiEwfX19ZnVuY3Rpb24gc2IoYSl7cmV0dXJuIGEubGVu
Z3RoPjE/ZnVuY3Rpb24oYixjLGQpe3ZhciBlPWEubGVuZ3RoO3doaWxlKGUtLSlp
ZighYVtlXShiLGMsZCkpcmV0dXJuITE7cmV0dXJuITB9OmFbMF19ZnVuY3Rpb24g
dGIoYSxiLGMpe2Zvcih2YXIgZD0wLGU9Yi5sZW5ndGg7ZT5kO2QrKylmYihhLGJb
ZF0sYyk7cmV0dXJuIGN9ZnVuY3Rpb24gdWIoYSxiLGMsZCxlKXtmb3IodmFyIGYs
Zz1bXSxoPTAsaT1hLmxlbmd0aCxqPW51bGwhPWI7aT5oO2grKykoZj1hW2hdKSYm
KCFjfHxjKGYsZCxlKSkmJihnLnB1c2goZiksaiYmYi5wdXNoKGgpKTtyZXR1cm4g
Z31mdW5jdGlvbiB2YihhLGIsYyxkLGUsZil7cmV0dXJuIGQmJiFkW3VdJiYoZD12
YihkKSksZSYmIWVbdV0mJihlPXZiKGUsZikpLGhiKGZ1bmN0aW9uKGYsZyxoLGkp
e3ZhciBqLGssbCxtPVtdLG49W10sbz1nLmxlbmd0aCxwPWZ8fHRiKGJ8fCIqIixo
Lm5vZGVUeXBlP1toXTpoLFtdKSxxPSFhfHwhZiYmYj9wOnViKHAsbSxhLGgsaSks
cj1jP2V8fChmP2E6b3x8ZCk/W106ZzpxO2lmKGMmJmMocSxyLGgsaSksZCl7aj11
YihyLG4pLGQoaixbXSxoLGkpLGs9ai5sZW5ndGg7d2hpbGUoay0tKShsPWpba10p
JiYocltuW2tdXT0hKHFbbltrXV09bCkpfWlmKGYpe2lmKGV8fGEpe2lmKGUpe2o9
W10saz1yLmxlbmd0aDt3aGlsZShrLS0pKGw9cltrXSkmJmoucHVzaChxW2tdPWwp
O2UobnVsbCxyPVtdLGosaSl9az1yLmxlbmd0aDt3aGlsZShrLS0pKGw9cltrXSkm
JihqPWU/Sy5jYWxsKGYsbCk6bVtrXSk+LTEmJihmW2pdPSEoZ1tqXT1sKSl9fWVs
c2Ugcj11YihyPT09Zz9yLnNwbGljZShvLHIubGVuZ3RoKTpyKSxlP2UobnVsbCxn
LHIsaSk6SS5hcHBseShnLHIpfSl9ZnVuY3Rpb24gd2IoYSl7Zm9yKHZhciBiLGMs
ZSxmPWEubGVuZ3RoLGc9ZC5yZWxhdGl2ZVthWzBdLnR5cGVdLGg9Z3x8ZC5yZWxh
dGl2ZVsiICJdLGk9Zz8xOjAsaz1yYihmdW5jdGlvbihhKXtyZXR1cm4gYT09PWJ9
LGgsITApLGw9cmIoZnVuY3Rpb24oYSl7cmV0dXJuIEsuY2FsbChiLGEpPi0xfSxo
LCEwKSxtPVtmdW5jdGlvbihhLGMsZCl7cmV0dXJuIWcmJihkfHxjIT09ail8fCgo
Yj1jKS5ub2RlVHlwZT9rKGEsYyxkKTpsKGEsYyxkKSl9XTtmPmk7aSsrKWlmKGM9
ZC5yZWxhdGl2ZVthW2ldLnR5cGVdKW09W3JiKHNiKG0pLGMpXTtlbHNle2lmKGM9
ZC5maWx0ZXJbYVtpXS50eXBlXS5hcHBseShudWxsLGFbaV0ubWF0Y2hlcyksY1t1
XSl7Zm9yKGU9KytpO2Y+ZTtlKyspaWYoZC5yZWxhdGl2ZVthW2VdLnR5cGVdKWJy
ZWFrO3JldHVybiB2YihpPjEmJnNiKG0pLGk+MSYmcWIoYS5zbGljZSgwLGktMSku
Y29uY2F0KHt2YWx1ZToiICI9PT1hW2ktMl0udHlwZT8iKiI6IiJ9KSkucmVwbGFj
ZShSLCIkMSIpLGMsZT5pJiZ3YihhLnNsaWNlKGksZSkpLGY+ZSYmd2IoYT1hLnNs
aWNlKGUpKSxmPmUmJnFiKGEpKX1tLnB1c2goYyl9cmV0dXJuIHNiKG0pfWZ1bmN0
aW9uIHhiKGEsYil7dmFyIGM9Yi5sZW5ndGg+MCxlPWEubGVuZ3RoPjAsZj1mdW5j
dGlvbihmLGcsaCxpLGspe3ZhciBsLG0sbyxwPTAscT0iMCIscj1mJiZbXSxzPVtd
LHQ9aix1PWZ8fGUmJmQuZmluZC5UQUcoIioiLGspLHY9dys9bnVsbD09dD8xOk1h
dGgucmFuZG9tKCl8fC4xLHg9dS5sZW5ndGg7Zm9yKGsmJihqPWchPT1uJiZnKTtx
IT09eCYmbnVsbCE9KGw9dVtxXSk7cSsrKXtpZihlJiZsKXttPTA7d2hpbGUobz1h
W20rK10paWYobyhsLGcsaCkpe2kucHVzaChsKTticmVha31rJiYodz12KX1jJiYo
KGw9IW8mJmwpJiZwLS0sZiYmci5wdXNoKGwpKX1pZihwKz1xLGMmJnEhPT1wKXtt
PTA7d2hpbGUobz1iW20rK10pbyhyLHMsZyxoKTtpZihmKXtpZihwPjApd2hpbGUo
cS0tKXJbcV18fHNbcV18fChzW3FdPUcuY2FsbChpKSk7cz11YihzKX1JLmFwcGx5
KGkscyksayYmIWYmJnMubGVuZ3RoPjAmJnArYi5sZW5ndGg+MSYmZmIudW5pcXVl
U29ydChpKX1yZXR1cm4gayYmKHc9dixqPXQpLHJ9O3JldHVybiBjP2hiKGYpOmZ9
cmV0dXJuIGg9ZmIuY29tcGlsZT1mdW5jdGlvbihhLGIpe3ZhciBjLGQ9W10sZT1b
XSxmPUFbYSsiICJdO2lmKCFmKXtifHwoYj1nKGEpKSxjPWIubGVuZ3RoO3doaWxl
KGMtLSlmPXdiKGJbY10pLGZbdV0/ZC5wdXNoKGYpOmUucHVzaChmKTtmPUEoYSx4
YihlLGQpKSxmLnNlbGVjdG9yPWF9cmV0dXJuIGZ9LGk9ZmIuc2VsZWN0PWZ1bmN0
aW9uKGEsYixlLGYpe3ZhciBpLGosayxsLG0sbj0iZnVuY3Rpb24iPT10eXBlb2Yg
YSYmYSxvPSFmJiZnKGE9bi5zZWxlY3Rvcnx8YSk7aWYoZT1lfHxbXSwxPT09by5s
ZW5ndGgpe2lmKGo9b1swXT1vWzBdLnNsaWNlKDApLGoubGVuZ3RoPjImJiJJRCI9
PT0oaz1qWzBdKS50eXBlJiZjLmdldEJ5SWQmJjk9PT1iLm5vZGVUeXBlJiZwJiZk
LnJlbGF0aXZlW2pbMV0udHlwZV0pe2lmKGI9KGQuZmluZC5JRChrLm1hdGNoZXNb
MF0ucmVwbGFjZShjYixkYiksYil8fFtdKVswXSwhYilyZXR1cm4gZTtuJiYoYj1i
LnBhcmVudE5vZGUpLGE9YS5zbGljZShqLnNoaWZ0KCkudmFsdWUubGVuZ3RoKX1p
PVgubmVlZHNDb250ZXh0LnRlc3QoYSk/MDpqLmxlbmd0aDt3aGlsZShpLS0pe2lm
KGs9altpXSxkLnJlbGF0aXZlW2w9ay50eXBlXSlicmVhaztpZigobT1kLmZpbmRb
bF0pJiYoZj1tKGsubWF0Y2hlc1swXS5yZXBsYWNlKGNiLGRiKSxhYi50ZXN0KGpb
MF0udHlwZSkmJm9iKGIucGFyZW50Tm9kZSl8fGIpKSl7aWYoai5zcGxpY2UoaSwx
KSxhPWYubGVuZ3RoJiZxYihqKSwhYSlyZXR1cm4gSS5hcHBseShlLGYpLGU7YnJl
YWt9fX1yZXR1cm4obnx8aChhLG8pKShmLGIsIXAsZSxhYi50ZXN0KGEpJiZvYihi
LnBhcmVudE5vZGUpfHxiKSxlfSxjLnNvcnRTdGFibGU9dS5zcGxpdCgiIikuc29y
dChCKS5qb2luKCIiKT09PXUsYy5kZXRlY3REdXBsaWNhdGVzPSEhbCxtKCksYy5z
b3J0RGV0YWNoZWQ9aWIoZnVuY3Rpb24oYSl7cmV0dXJuIDEmYS5jb21wYXJlRG9j
dW1lbnRQb3NpdGlvbihuLmNyZWF0ZUVsZW1lbnQoImRpdiIpKX0pLGliKGZ1bmN0
aW9uKGEpe3JldHVybiBhLmlubmVySFRNTD0iPGEgaHJlZj0nIyc+PC9hPiIsIiMi
PT09YS5maXJzdENoaWxkLmdldEF0dHJpYnV0ZSgiaHJlZiIpfSl8fGpiKCJ0eXBl
fGhyZWZ8aGVpZ2h0fHdpZHRoIixmdW5jdGlvbihhLGIsYyl7cmV0dXJuIGM/dm9p
ZCAwOmEuZ2V0QXR0cmlidXRlKGIsInR5cGUiPT09Yi50b0xvd2VyQ2FzZSgpPzE6
Mil9KSxjLmF0dHJpYnV0ZXMmJmliKGZ1bmN0aW9uKGEpe3JldHVybiBhLmlubmVy
SFRNTD0iPGlucHV0Lz4iLGEuZmlyc3RDaGlsZC5zZXRBdHRyaWJ1dGUoInZhbHVl
IiwiIiksIiI9PT1hLmZpcnN0Q2hpbGQuZ2V0QXR0cmlidXRlKCJ2YWx1ZSIpfSl8
fGpiKCJ2YWx1ZSIsZnVuY3Rpb24oYSxiLGMpe3JldHVybiBjfHwiaW5wdXQiIT09
YS5ub2RlTmFtZS50b0xvd2VyQ2FzZSgpP3ZvaWQgMDphLmRlZmF1bHRWYWx1ZX0p
LGliKGZ1bmN0aW9uKGEpe3JldHVybiBudWxsPT1hLmdldEF0dHJpYnV0ZSgiZGlz
YWJsZWQiKX0pfHxqYihMLGZ1bmN0aW9uKGEsYixjKXt2YXIgZDtyZXR1cm4gYz92
b2lkIDA6YVtiXT09PSEwP2IudG9Mb3dlckNhc2UoKTooZD1hLmdldEF0dHJpYnV0
ZU5vZGUoYikpJiZkLnNwZWNpZmllZD9kLnZhbHVlOm51bGx9KSxmYn0oYSk7bi5m
aW5kPXQsbi5leHByPXQuc2VsZWN0b3JzLG4uZXhwclsiOiJdPW4uZXhwci5wc2V1
ZG9zLG4udW5pcXVlPXQudW5pcXVlU29ydCxuLnRleHQ9dC5nZXRUZXh0LG4uaXNY
TUxEb2M9dC5pc1hNTCxuLmNvbnRhaW5zPXQuY29udGFpbnM7dmFyIHU9bi5leHBy
Lm1hdGNoLm5lZWRzQ29udGV4dCx2PS9ePChcdyspXHMqXC8/Pig/OjxcL1wxPnwp
JC8sdz0vXi5bXjojXFtcLixdKiQvO2Z1bmN0aW9uIHgoYSxiLGMpe2lmKG4uaXNG
dW5jdGlvbihiKSlyZXR1cm4gbi5ncmVwKGEsZnVuY3Rpb24oYSxkKXtyZXR1cm4h
IWIuY2FsbChhLGQsYSkhPT1jfSk7aWYoYi5ub2RlVHlwZSlyZXR1cm4gbi5ncmVw
KGEsZnVuY3Rpb24oYSl7cmV0dXJuIGE9PT1iIT09Y30pO2lmKCJzdHJpbmciPT10
eXBlb2YgYil7aWYody50ZXN0KGIpKXJldHVybiBuLmZpbHRlcihiLGEsYyk7Yj1u
LmZpbHRlcihiLGEpfXJldHVybiBuLmdyZXAoYSxmdW5jdGlvbihhKXtyZXR1cm4g
Zy5jYWxsKGIsYSk+PTAhPT1jfSl9bi5maWx0ZXI9ZnVuY3Rpb24oYSxiLGMpe3Zh
ciBkPWJbMF07cmV0dXJuIGMmJihhPSI6bm90KCIrYSsiKSIpLDE9PT1iLmxlbmd0
aCYmMT09PWQubm9kZVR5cGU/bi5maW5kLm1hdGNoZXNTZWxlY3RvcihkLGEpP1tk
XTpbXTpuLmZpbmQubWF0Y2hlcyhhLG4uZ3JlcChiLGZ1bmN0aW9uKGEpe3JldHVy
biAxPT09YS5ub2RlVHlwZX0pKX0sbi5mbi5leHRlbmQoe2ZpbmQ6ZnVuY3Rpb24o
YSl7dmFyIGIsYz10aGlzLmxlbmd0aCxkPVtdLGU9dGhpcztpZigic3RyaW5nIiE9
dHlwZW9mIGEpcmV0dXJuIHRoaXMucHVzaFN0YWNrKG4oYSkuZmlsdGVyKGZ1bmN0
aW9uKCl7Zm9yKGI9MDtjPmI7YisrKWlmKG4uY29udGFpbnMoZVtiXSx0aGlzKSly
ZXR1cm4hMH0pKTtmb3IoYj0wO2M+YjtiKyspbi5maW5kKGEsZVtiXSxkKTtyZXR1
cm4gZD10aGlzLnB1c2hTdGFjayhjPjE/bi51bmlxdWUoZCk6ZCksZC5zZWxlY3Rv
cj10aGlzLnNlbGVjdG9yP3RoaXMuc2VsZWN0b3IrIiAiK2E6YSxkfSxmaWx0ZXI6
ZnVuY3Rpb24oYSl7cmV0dXJuIHRoaXMucHVzaFN0YWNrKHgodGhpcyxhfHxbXSwh
MSkpfSxub3Q6ZnVuY3Rpb24oYSl7cmV0dXJuIHRoaXMucHVzaFN0YWNrKHgodGhp
cyxhfHxbXSwhMCkpfSxpczpmdW5jdGlvbihhKXtyZXR1cm4hIXgodGhpcywic3Ry
aW5nIj09dHlwZW9mIGEmJnUudGVzdChhKT9uKGEpOmF8fFtdLCExKS5sZW5ndGh9
fSk7dmFyIHksej0vXig/OlxzKig8W1x3XFddKz4pW14+XSp8IyhbXHctXSopKSQv
LEE9bi5mbi5pbml0PWZ1bmN0aW9uKGEsYil7dmFyIGMsZDtpZighYSlyZXR1cm4g
dGhpcztpZigic3RyaW5nIj09dHlwZW9mIGEpe2lmKGM9IjwiPT09YVswXSYmIj4i
PT09YVthLmxlbmd0aC0xXSYmYS5sZW5ndGg+PTM/W251bGwsYSxudWxsXTp6LmV4
ZWMoYSksIWN8fCFjWzFdJiZiKXJldHVybiFifHxiLmpxdWVyeT8oYnx8eSkuZmlu
ZChhKTp0aGlzLmNvbnN0cnVjdG9yKGIpLmZpbmQoYSk7aWYoY1sxXSl7aWYoYj1i
IGluc3RhbmNlb2Ygbj9iWzBdOmIsbi5tZXJnZSh0aGlzLG4ucGFyc2VIVE1MKGNb
MV0sYiYmYi5ub2RlVHlwZT9iLm93bmVyRG9jdW1lbnR8fGI6bCwhMCkpLHYudGVz
dChjWzFdKSYmbi5pc1BsYWluT2JqZWN0KGIpKWZvcihjIGluIGIpbi5pc0Z1bmN0
aW9uKHRoaXNbY10pP3RoaXNbY10oYltjXSk6dGhpcy5hdHRyKGMsYltjXSk7cmV0
dXJuIHRoaXN9cmV0dXJuIGQ9bC5nZXRFbGVtZW50QnlJZChjWzJdKSxkJiZkLnBh
cmVudE5vZGUmJih0aGlzLmxlbmd0aD0xLHRoaXNbMF09ZCksdGhpcy5jb250ZXh0
PWwsdGhpcy5zZWxlY3Rvcj1hLHRoaXN9cmV0dXJuIGEubm9kZVR5cGU/KHRoaXMu
Y29udGV4dD10aGlzWzBdPWEsdGhpcy5sZW5ndGg9MSx0aGlzKTpuLmlzRnVuY3Rp
b24oYSk/InVuZGVmaW5lZCIhPXR5cGVvZiB5LnJlYWR5P3kucmVhZHkoYSk6YShu
KToodm9pZCAwIT09YS5zZWxlY3RvciYmKHRoaXMuc2VsZWN0b3I9YS5zZWxlY3Rv
cix0aGlzLmNvbnRleHQ9YS5jb250ZXh0KSxuLm1ha2VBcnJheShhLHRoaXMpKX07
QS5wcm90b3R5cGU9bi5mbix5PW4obCk7dmFyIEI9L14oPzpwYXJlbnRzfHByZXYo
PzpVbnRpbHxBbGwpKS8sQz17Y2hpbGRyZW46ITAsY29udGVudHM6ITAsbmV4dDoh
MCxwcmV2OiEwfTtuLmV4dGVuZCh7ZGlyOmZ1bmN0aW9uKGEsYixjKXt2YXIgZD1b
XSxlPXZvaWQgMCE9PWM7d2hpbGUoKGE9YVtiXSkmJjkhPT1hLm5vZGVUeXBlKWlm
KDE9PT1hLm5vZGVUeXBlKXtpZihlJiZuKGEpLmlzKGMpKWJyZWFrO2QucHVzaChh
KX1yZXR1cm4gZH0sc2libGluZzpmdW5jdGlvbihhLGIpe2Zvcih2YXIgYz1bXTth
O2E9YS5uZXh0U2libGluZykxPT09YS5ub2RlVHlwZSYmYSE9PWImJmMucHVzaChh
KTtyZXR1cm4gY319KSxuLmZuLmV4dGVuZCh7aGFzOmZ1bmN0aW9uKGEpe3ZhciBi
PW4oYSx0aGlzKSxjPWIubGVuZ3RoO3JldHVybiB0aGlzLmZpbHRlcihmdW5jdGlv
bigpe2Zvcih2YXIgYT0wO2M+YTthKyspaWYobi5jb250YWlucyh0aGlzLGJbYV0p
KXJldHVybiEwfSl9LGNsb3Nlc3Q6ZnVuY3Rpb24oYSxiKXtmb3IodmFyIGMsZD0w
LGU9dGhpcy5sZW5ndGgsZj1bXSxnPXUudGVzdChhKXx8InN0cmluZyIhPXR5cGVv
ZiBhP24oYSxifHx0aGlzLmNvbnRleHQpOjA7ZT5kO2QrKylmb3IoYz10aGlzW2Rd
O2MmJmMhPT1iO2M9Yy5wYXJlbnROb2RlKWlmKGMubm9kZVR5cGU8MTEmJihnP2cu
aW5kZXgoYyk+LTE6MT09PWMubm9kZVR5cGUmJm4uZmluZC5tYXRjaGVzU2VsZWN0
b3IoYyxhKSkpe2YucHVzaChjKTticmVha31yZXR1cm4gdGhpcy5wdXNoU3RhY2so
Zi5sZW5ndGg+MT9uLnVuaXF1ZShmKTpmKX0saW5kZXg6ZnVuY3Rpb24oYSl7cmV0
dXJuIGE/InN0cmluZyI9PXR5cGVvZiBhP2cuY2FsbChuKGEpLHRoaXNbMF0pOmcu
Y2FsbCh0aGlzLGEuanF1ZXJ5P2FbMF06YSk6dGhpc1swXSYmdGhpc1swXS5wYXJl
bnROb2RlP3RoaXMuZmlyc3QoKS5wcmV2QWxsKCkubGVuZ3RoOi0xfSxhZGQ6ZnVu
Y3Rpb24oYSxiKXtyZXR1cm4gdGhpcy5wdXNoU3RhY2sobi51bmlxdWUobi5tZXJn
ZSh0aGlzLmdldCgpLG4oYSxiKSkpKX0sYWRkQmFjazpmdW5jdGlvbihhKXtyZXR1
cm4gdGhpcy5hZGQobnVsbD09YT90aGlzLnByZXZPYmplY3Q6dGhpcy5wcmV2T2Jq
ZWN0LmZpbHRlcihhKSl9fSk7ZnVuY3Rpb24gRChhLGIpe3doaWxlKChhPWFbYl0p
JiYxIT09YS5ub2RlVHlwZSk7cmV0dXJuIGF9bi5lYWNoKHtwYXJlbnQ6ZnVuY3Rp
b24oYSl7dmFyIGI9YS5wYXJlbnROb2RlO3JldHVybiBiJiYxMSE9PWIubm9kZVR5
cGU/YjpudWxsfSxwYXJlbnRzOmZ1bmN0aW9uKGEpe3JldHVybiBuLmRpcihhLCJw
YXJlbnROb2RlIil9LHBhcmVudHNVbnRpbDpmdW5jdGlvbihhLGIsYyl7cmV0dXJu
IG4uZGlyKGEsInBhcmVudE5vZGUiLGMpfSxuZXh0OmZ1bmN0aW9uKGEpe3JldHVy
biBEKGEsIm5leHRTaWJsaW5nIil9LHByZXY6ZnVuY3Rpb24oYSl7cmV0dXJuIEQo
YSwicHJldmlvdXNTaWJsaW5nIil9LG5leHRBbGw6ZnVuY3Rpb24oYSl7cmV0dXJu
IG4uZGlyKGEsIm5leHRTaWJsaW5nIil9LHByZXZBbGw6ZnVuY3Rpb24oYSl7cmV0
dXJuIG4uZGlyKGEsInByZXZpb3VzU2libGluZyIpfSxuZXh0VW50aWw6ZnVuY3Rp
b24oYSxiLGMpe3JldHVybiBuLmRpcihhLCJuZXh0U2libGluZyIsYyl9LHByZXZV
bnRpbDpmdW5jdGlvbihhLGIsYyl7cmV0dXJuIG4uZGlyKGEsInByZXZpb3VzU2li
bGluZyIsYyl9LHNpYmxpbmdzOmZ1bmN0aW9uKGEpe3JldHVybiBuLnNpYmxpbmco
KGEucGFyZW50Tm9kZXx8e30pLmZpcnN0Q2hpbGQsYSl9LGNoaWxkcmVuOmZ1bmN0
aW9uKGEpe3JldHVybiBuLnNpYmxpbmcoYS5maXJzdENoaWxkKX0sY29udGVudHM6
ZnVuY3Rpb24oYSl7cmV0dXJuIGEuY29udGVudERvY3VtZW50fHxuLm1lcmdlKFtd
LGEuY2hpbGROb2Rlcyl9fSxmdW5jdGlvbihhLGIpe24uZm5bYV09ZnVuY3Rpb24o
YyxkKXt2YXIgZT1uLm1hcCh0aGlzLGIsYyk7cmV0dXJuIlVudGlsIiE9PWEuc2xp
Y2UoLTUpJiYoZD1jKSxkJiYic3RyaW5nIj09dHlwZW9mIGQmJihlPW4uZmlsdGVy
KGQsZSkpLHRoaXMubGVuZ3RoPjEmJihDW2FdfHxuLnVuaXF1ZShlKSxCLnRlc3Qo
YSkmJmUucmV2ZXJzZSgpKSx0aGlzLnB1c2hTdGFjayhlKX19KTt2YXIgRT0vXFMr
L2csRj17fTtmdW5jdGlvbiBHKGEpe3ZhciBiPUZbYV09e307cmV0dXJuIG4uZWFj
aChhLm1hdGNoKEUpfHxbXSxmdW5jdGlvbihhLGMpe2JbY109ITB9KSxifW4uQ2Fs
bGJhY2tzPWZ1bmN0aW9uKGEpe2E9InN0cmluZyI9PXR5cGVvZiBhP0ZbYV18fEco
YSk6bi5leHRlbmQoe30sYSk7dmFyIGIsYyxkLGUsZixnLGg9W10saT0hYS5vbmNl
JiZbXSxqPWZ1bmN0aW9uKGwpe2ZvcihiPWEubWVtb3J5JiZsLGM9ITAsZz1lfHww
LGU9MCxmPWgubGVuZ3RoLGQ9ITA7aCYmZj5nO2crKylpZihoW2ddLmFwcGx5KGxb
MF0sbFsxXSk9PT0hMSYmYS5zdG9wT25GYWxzZSl7Yj0hMTticmVha31kPSExLGgm
JihpP2kubGVuZ3RoJiZqKGkuc2hpZnQoKSk6Yj9oPVtdOmsuZGlzYWJsZSgpKX0s
az17YWRkOmZ1bmN0aW9uKCl7aWYoaCl7dmFyIGM9aC5sZW5ndGg7IWZ1bmN0aW9u
IGcoYil7bi5lYWNoKGIsZnVuY3Rpb24oYixjKXt2YXIgZD1uLnR5cGUoYyk7ImZ1
bmN0aW9uIj09PWQ/YS51bmlxdWUmJmsuaGFzKGMpfHxoLnB1c2goYyk6YyYmYy5s
ZW5ndGgmJiJzdHJpbmciIT09ZCYmZyhjKX0pfShhcmd1bWVudHMpLGQ/Zj1oLmxl
bmd0aDpiJiYoZT1jLGooYikpfXJldHVybiB0aGlzfSxyZW1vdmU6ZnVuY3Rpb24o
KXtyZXR1cm4gaCYmbi5lYWNoKGFyZ3VtZW50cyxmdW5jdGlvbihhLGIpe3ZhciBj
O3doaWxlKChjPW4uaW5BcnJheShiLGgsYykpPi0xKWguc3BsaWNlKGMsMSksZCYm
KGY+PWMmJmYtLSxnPj1jJiZnLS0pfSksdGhpc30saGFzOmZ1bmN0aW9uKGEpe3Jl
dHVybiBhP24uaW5BcnJheShhLGgpPi0xOiEoIWh8fCFoLmxlbmd0aCl9LGVtcHR5
OmZ1bmN0aW9uKCl7cmV0dXJuIGg9W10sZj0wLHRoaXN9LGRpc2FibGU6ZnVuY3Rp
b24oKXtyZXR1cm4gaD1pPWI9dm9pZCAwLHRoaXN9LGRpc2FibGVkOmZ1bmN0aW9u
KCl7cmV0dXJuIWh9LGxvY2s6ZnVuY3Rpb24oKXtyZXR1cm4gaT12b2lkIDAsYnx8
ay5kaXNhYmxlKCksdGhpc30sbG9ja2VkOmZ1bmN0aW9uKCl7cmV0dXJuIWl9LGZp
cmVXaXRoOmZ1bmN0aW9uKGEsYil7cmV0dXJuIWh8fGMmJiFpfHwoYj1ifHxbXSxi
PVthLGIuc2xpY2U/Yi5zbGljZSgpOmJdLGQ/aS5wdXNoKGIpOmooYikpLHRoaXN9
LGZpcmU6ZnVuY3Rpb24oKXtyZXR1cm4gay5maXJlV2l0aCh0aGlzLGFyZ3VtZW50
cyksdGhpc30sZmlyZWQ6ZnVuY3Rpb24oKXtyZXR1cm4hIWN9fTtyZXR1cm4ga30s
bi5leHRlbmQoe0RlZmVycmVkOmZ1bmN0aW9uKGEpe3ZhciBiPVtbInJlc29sdmUi
LCJkb25lIixuLkNhbGxiYWNrcygib25jZSBtZW1vcnkiKSwicmVzb2x2ZWQiXSxb
InJlamVjdCIsImZhaWwiLG4uQ2FsbGJhY2tzKCJvbmNlIG1lbW9yeSIpLCJyZWpl
Y3RlZCJdLFsibm90aWZ5IiwicHJvZ3Jlc3MiLG4uQ2FsbGJhY2tzKCJtZW1vcnki
KV1dLGM9InBlbmRpbmciLGQ9e3N0YXRlOmZ1bmN0aW9uKCl7cmV0dXJuIGN9LGFs
d2F5czpmdW5jdGlvbigpe3JldHVybiBlLmRvbmUoYXJndW1lbnRzKS5mYWlsKGFy
Z3VtZW50cyksdGhpc30sdGhlbjpmdW5jdGlvbigpe3ZhciBhPWFyZ3VtZW50czty
ZXR1cm4gbi5EZWZlcnJlZChmdW5jdGlvbihjKXtuLmVhY2goYixmdW5jdGlvbihi
LGYpe3ZhciBnPW4uaXNGdW5jdGlvbihhW2JdKSYmYVtiXTtlW2ZbMV1dKGZ1bmN0
aW9uKCl7dmFyIGE9ZyYmZy5hcHBseSh0aGlzLGFyZ3VtZW50cyk7YSYmbi5pc0Z1
bmN0aW9uKGEucHJvbWlzZSk/YS5wcm9taXNlKCkuZG9uZShjLnJlc29sdmUpLmZh
aWwoYy5yZWplY3QpLnByb2dyZXNzKGMubm90aWZ5KTpjW2ZbMF0rIldpdGgiXSh0
aGlzPT09ZD9jLnByb21pc2UoKTp0aGlzLGc/W2FdOmFyZ3VtZW50cyl9KX0pLGE9
bnVsbH0pLnByb21pc2UoKX0scHJvbWlzZTpmdW5jdGlvbihhKXtyZXR1cm4gbnVs
bCE9YT9uLmV4dGVuZChhLGQpOmR9fSxlPXt9O3JldHVybiBkLnBpcGU9ZC50aGVu
LG4uZWFjaChiLGZ1bmN0aW9uKGEsZil7dmFyIGc9ZlsyXSxoPWZbM107ZFtmWzFd
XT1nLmFkZCxoJiZnLmFkZChmdW5jdGlvbigpe2M9aH0sYlsxXmFdWzJdLmRpc2Fi
bGUsYlsyXVsyXS5sb2NrKSxlW2ZbMF1dPWZ1bmN0aW9uKCl7cmV0dXJuIGVbZlsw
XSsiV2l0aCJdKHRoaXM9PT1lP2Q6dGhpcyxhcmd1bWVudHMpLHRoaXN9LGVbZlsw
XSsiV2l0aCJdPWcuZmlyZVdpdGh9KSxkLnByb21pc2UoZSksYSYmYS5jYWxsKGUs
ZSksZX0sd2hlbjpmdW5jdGlvbihhKXt2YXIgYj0wLGM9ZC5jYWxsKGFyZ3VtZW50
cyksZT1jLmxlbmd0aCxmPTEhPT1lfHxhJiZuLmlzRnVuY3Rpb24oYS5wcm9taXNl
KT9lOjAsZz0xPT09Zj9hOm4uRGVmZXJyZWQoKSxoPWZ1bmN0aW9uKGEsYixjKXty
ZXR1cm4gZnVuY3Rpb24oZSl7YlthXT10aGlzLGNbYV09YXJndW1lbnRzLmxlbmd0
aD4xP2QuY2FsbChhcmd1bWVudHMpOmUsYz09PWk/Zy5ub3RpZnlXaXRoKGIsYyk6
LS1mfHxnLnJlc29sdmVXaXRoKGIsYyl9fSxpLGosaztpZihlPjEpZm9yKGk9bmV3
IEFycmF5KGUpLGo9bmV3IEFycmF5KGUpLGs9bmV3IEFycmF5KGUpO2U+YjtiKysp
Y1tiXSYmbi5pc0Z1bmN0aW9uKGNbYl0ucHJvbWlzZSk/Y1tiXS5wcm9taXNlKCku
ZG9uZShoKGIsayxjKSkuZmFpbChnLnJlamVjdCkucHJvZ3Jlc3MoaChiLGosaSkp
Oi0tZjtyZXR1cm4gZnx8Zy5yZXNvbHZlV2l0aChrLGMpLGcucHJvbWlzZSgpfX0p
O3ZhciBIO24uZm4ucmVhZHk9ZnVuY3Rpb24oYSl7cmV0dXJuIG4ucmVhZHkucHJv
bWlzZSgpLmRvbmUoYSksdGhpc30sbi5leHRlbmQoe2lzUmVhZHk6ITEscmVhZHlX
YWl0OjEsaG9sZFJlYWR5OmZ1bmN0aW9uKGEpe2E/bi5yZWFkeVdhaXQrKzpuLnJl
YWR5KCEwKX0scmVhZHk6ZnVuY3Rpb24oYSl7KGE9PT0hMD8tLW4ucmVhZHlXYWl0
Om4uaXNSZWFkeSl8fChuLmlzUmVhZHk9ITAsYSE9PSEwJiYtLW4ucmVhZHlXYWl0
PjB8fChILnJlc29sdmVXaXRoKGwsW25dKSxuLmZuLnRyaWdnZXJIYW5kbGVyJiYo
bihsKS50cmlnZ2VySGFuZGxlcigicmVhZHkiKSxuKGwpLm9mZigicmVhZHkiKSkp
KX19KTtmdW5jdGlvbiBJKCl7bC5yZW1vdmVFdmVudExpc3RlbmVyKCJET01Db250
ZW50TG9hZGVkIixJLCExKSxhLnJlbW92ZUV2ZW50TGlzdGVuZXIoImxvYWQiLEks
ITEpLG4ucmVhZHkoKX1uLnJlYWR5LnByb21pc2U9ZnVuY3Rpb24oYil7cmV0dXJu
IEh8fChIPW4uRGVmZXJyZWQoKSwiY29tcGxldGUiPT09bC5yZWFkeVN0YXRlP3Nl
dFRpbWVvdXQobi5yZWFkeSk6KGwuYWRkRXZlbnRMaXN0ZW5lcigiRE9NQ29udGVu
dExvYWRlZCIsSSwhMSksYS5hZGRFdmVudExpc3RlbmVyKCJsb2FkIixJLCExKSkp
LEgucHJvbWlzZShiKX0sbi5yZWFkeS5wcm9taXNlKCk7dmFyIEo9bi5hY2Nlc3M9
ZnVuY3Rpb24oYSxiLGMsZCxlLGYsZyl7dmFyIGg9MCxpPWEubGVuZ3RoLGo9bnVs
bD09YztpZigib2JqZWN0Ij09PW4udHlwZShjKSl7ZT0hMDtmb3IoaCBpbiBjKW4u
YWNjZXNzKGEsYixoLGNbaF0sITAsZixnKX1lbHNlIGlmKHZvaWQgMCE9PWQmJihl
PSEwLG4uaXNGdW5jdGlvbihkKXx8KGc9ITApLGomJihnPyhiLmNhbGwoYSxkKSxi
PW51bGwpOihqPWIsYj1mdW5jdGlvbihhLGIsYyl7cmV0dXJuIGouY2FsbChuKGEp
LGMpfSkpLGIpKWZvcig7aT5oO2grKyliKGFbaF0sYyxnP2Q6ZC5jYWxsKGFbaF0s
aCxiKGFbaF0sYykpKTtyZXR1cm4gZT9hOmo/Yi5jYWxsKGEpOmk/YihhWzBdLGMp
OmZ9O24uYWNjZXB0RGF0YT1mdW5jdGlvbihhKXtyZXR1cm4gMT09PWEubm9kZVR5
cGV8fDk9PT1hLm5vZGVUeXBlfHwhK2Eubm9kZVR5cGV9O2Z1bmN0aW9uIEsoKXtP
YmplY3QuZGVmaW5lUHJvcGVydHkodGhpcy5jYWNoZT17fSwwLHtnZXQ6ZnVuY3Rp
b24oKXtyZXR1cm57fX19KSx0aGlzLmV4cGFuZG89bi5leHBhbmRvK01hdGgucmFu
ZG9tKCl9Sy51aWQ9MSxLLmFjY2VwdHM9bi5hY2NlcHREYXRhLEsucHJvdG90eXBl
PXtrZXk6ZnVuY3Rpb24oYSl7aWYoIUsuYWNjZXB0cyhhKSlyZXR1cm4gMDt2YXIg
Yj17fSxjPWFbdGhpcy5leHBhbmRvXTtpZighYyl7Yz1LLnVpZCsrO3RyeXtiW3Ro
aXMuZXhwYW5kb109e3ZhbHVlOmN9LE9iamVjdC5kZWZpbmVQcm9wZXJ0aWVzKGEs
Yil9Y2F0Y2goZCl7Ylt0aGlzLmV4cGFuZG9dPWMsbi5leHRlbmQoYSxiKX19cmV0
dXJuIHRoaXMuY2FjaGVbY118fCh0aGlzLmNhY2hlW2NdPXt9KSxjfSxzZXQ6ZnVu
Y3Rpb24oYSxiLGMpe3ZhciBkLGU9dGhpcy5rZXkoYSksZj10aGlzLmNhY2hlW2Vd
O2lmKCJzdHJpbmciPT10eXBlb2YgYilmW2JdPWM7ZWxzZSBpZihuLmlzRW1wdHlP
YmplY3QoZikpbi5leHRlbmQodGhpcy5jYWNoZVtlXSxiKTtlbHNlIGZvcihkIGlu
IGIpZltkXT1iW2RdO3JldHVybiBmfSxnZXQ6ZnVuY3Rpb24oYSxiKXt2YXIgYz10
aGlzLmNhY2hlW3RoaXMua2V5KGEpXTtyZXR1cm4gdm9pZCAwPT09Yj9jOmNbYl19
LGFjY2VzczpmdW5jdGlvbihhLGIsYyl7dmFyIGQ7cmV0dXJuIHZvaWQgMD09PWJ8
fGImJiJzdHJpbmciPT10eXBlb2YgYiYmdm9pZCAwPT09Yz8oZD10aGlzLmdldChh
LGIpLHZvaWQgMCE9PWQ/ZDp0aGlzLmdldChhLG4uY2FtZWxDYXNlKGIpKSk6KHRo
aXMuc2V0KGEsYixjKSx2b2lkIDAhPT1jP2M6Yil9LHJlbW92ZTpmdW5jdGlvbihh
LGIpe3ZhciBjLGQsZSxmPXRoaXMua2V5KGEpLGc9dGhpcy5jYWNoZVtmXTtpZih2
b2lkIDA9PT1iKXRoaXMuY2FjaGVbZl09e307ZWxzZXtuLmlzQXJyYXkoYik/ZD1i
LmNvbmNhdChiLm1hcChuLmNhbWVsQ2FzZSkpOihlPW4uY2FtZWxDYXNlKGIpLGIg
aW4gZz9kPVtiLGVdOihkPWUsZD1kIGluIGc/W2RdOmQubWF0Y2goRSl8fFtdKSks
Yz1kLmxlbmd0aDt3aGlsZShjLS0pZGVsZXRlIGdbZFtjXV19fSxoYXNEYXRhOmZ1
bmN0aW9uKGEpe3JldHVybiFuLmlzRW1wdHlPYmplY3QodGhpcy5jYWNoZVthW3Ro
aXMuZXhwYW5kb11dfHx7fSl9LGRpc2NhcmQ6ZnVuY3Rpb24oYSl7YVt0aGlzLmV4
cGFuZG9dJiZkZWxldGUgdGhpcy5jYWNoZVthW3RoaXMuZXhwYW5kb11dfX07dmFy
IEw9bmV3IEssTT1uZXcgSyxOPS9eKD86XHtbXHdcV10qXH18XFtbXHdcV10qXF0p
JC8sTz0vKFtBLVpdKS9nO2Z1bmN0aW9uIFAoYSxiLGMpe3ZhciBkO2lmKHZvaWQg
MD09PWMmJjE9PT1hLm5vZGVUeXBlKWlmKGQ9ImRhdGEtIitiLnJlcGxhY2UoTywi
LSQxIikudG9Mb3dlckNhc2UoKSxjPWEuZ2V0QXR0cmlidXRlKGQpLCJzdHJpbmci
PT10eXBlb2YgYyl7dHJ5e2M9InRydWUiPT09Yz8hMDoiZmFsc2UiPT09Yz8hMToi
bnVsbCI9PT1jP251bGw6K2MrIiI9PT1jPytjOk4udGVzdChjKT9uLnBhcnNlSlNP
TihjKTpjfWNhdGNoKGUpe31NLnNldChhLGIsYyl9ZWxzZSBjPXZvaWQgMDtyZXR1
cm4gY31uLmV4dGVuZCh7aGFzRGF0YTpmdW5jdGlvbihhKXtyZXR1cm4gTS5oYXNE
YXRhKGEpfHxMLmhhc0RhdGEoYSl9LGRhdGE6ZnVuY3Rpb24oYSxiLGMpe3JldHVy
biBNLmFjY2VzcyhhLGIsYyl9LHJlbW92ZURhdGE6ZnVuY3Rpb24oYSxiKXtNLnJl
bW92ZShhLGIpCn0sX2RhdGE6ZnVuY3Rpb24oYSxiLGMpe3JldHVybiBMLmFjY2Vz
cyhhLGIsYyl9LF9yZW1vdmVEYXRhOmZ1bmN0aW9uKGEsYil7TC5yZW1vdmUoYSxi
KX19KSxuLmZuLmV4dGVuZCh7ZGF0YTpmdW5jdGlvbihhLGIpe3ZhciBjLGQsZSxm
PXRoaXNbMF0sZz1mJiZmLmF0dHJpYnV0ZXM7aWYodm9pZCAwPT09YSl7aWYodGhp
cy5sZW5ndGgmJihlPU0uZ2V0KGYpLDE9PT1mLm5vZGVUeXBlJiYhTC5nZXQoZiwi
aGFzRGF0YUF0dHJzIikpKXtjPWcubGVuZ3RoO3doaWxlKGMtLSlnW2NdJiYoZD1n
W2NdLm5hbWUsMD09PWQuaW5kZXhPZigiZGF0YS0iKSYmKGQ9bi5jYW1lbENhc2Uo
ZC5zbGljZSg1KSksUChmLGQsZVtkXSkpKTtMLnNldChmLCJoYXNEYXRhQXR0cnMi
LCEwKX1yZXR1cm4gZX1yZXR1cm4ib2JqZWN0Ij09dHlwZW9mIGE/dGhpcy5lYWNo
KGZ1bmN0aW9uKCl7TS5zZXQodGhpcyxhKX0pOkoodGhpcyxmdW5jdGlvbihiKXt2
YXIgYyxkPW4uY2FtZWxDYXNlKGEpO2lmKGYmJnZvaWQgMD09PWIpe2lmKGM9TS5n
ZXQoZixhKSx2b2lkIDAhPT1jKXJldHVybiBjO2lmKGM9TS5nZXQoZixkKSx2b2lk
IDAhPT1jKXJldHVybiBjO2lmKGM9UChmLGQsdm9pZCAwKSx2b2lkIDAhPT1jKXJl
dHVybiBjfWVsc2UgdGhpcy5lYWNoKGZ1bmN0aW9uKCl7dmFyIGM9TS5nZXQodGhp
cyxkKTtNLnNldCh0aGlzLGQsYiksLTEhPT1hLmluZGV4T2YoIi0iKSYmdm9pZCAw
IT09YyYmTS5zZXQodGhpcyxhLGIpfSl9LG51bGwsYixhcmd1bWVudHMubGVuZ3Ro
PjEsbnVsbCwhMCl9LHJlbW92ZURhdGE6ZnVuY3Rpb24oYSl7cmV0dXJuIHRoaXMu
ZWFjaChmdW5jdGlvbigpe00ucmVtb3ZlKHRoaXMsYSl9KX19KSxuLmV4dGVuZCh7
cXVldWU6ZnVuY3Rpb24oYSxiLGMpe3ZhciBkO3JldHVybiBhPyhiPShifHwiZngi
KSsicXVldWUiLGQ9TC5nZXQoYSxiKSxjJiYoIWR8fG4uaXNBcnJheShjKT9kPUwu
YWNjZXNzKGEsYixuLm1ha2VBcnJheShjKSk6ZC5wdXNoKGMpKSxkfHxbXSk6dm9p
ZCAwfSxkZXF1ZXVlOmZ1bmN0aW9uKGEsYil7Yj1ifHwiZngiO3ZhciBjPW4ucXVl
dWUoYSxiKSxkPWMubGVuZ3RoLGU9Yy5zaGlmdCgpLGY9bi5fcXVldWVIb29rcyhh
LGIpLGc9ZnVuY3Rpb24oKXtuLmRlcXVldWUoYSxiKX07ImlucHJvZ3Jlc3MiPT09
ZSYmKGU9Yy5zaGlmdCgpLGQtLSksZSYmKCJmeCI9PT1iJiZjLnVuc2hpZnQoImlu
cHJvZ3Jlc3MiKSxkZWxldGUgZi5zdG9wLGUuY2FsbChhLGcsZikpLCFkJiZmJiZm
LmVtcHR5LmZpcmUoKX0sX3F1ZXVlSG9va3M6ZnVuY3Rpb24oYSxiKXt2YXIgYz1i
KyJxdWV1ZUhvb2tzIjtyZXR1cm4gTC5nZXQoYSxjKXx8TC5hY2Nlc3MoYSxjLHtl
bXB0eTpuLkNhbGxiYWNrcygib25jZSBtZW1vcnkiKS5hZGQoZnVuY3Rpb24oKXtM
LnJlbW92ZShhLFtiKyJxdWV1ZSIsY10pfSl9KX19KSxuLmZuLmV4dGVuZCh7cXVl
dWU6ZnVuY3Rpb24oYSxiKXt2YXIgYz0yO3JldHVybiJzdHJpbmciIT10eXBlb2Yg
YSYmKGI9YSxhPSJmeCIsYy0tKSxhcmd1bWVudHMubGVuZ3RoPGM/bi5xdWV1ZSh0
aGlzWzBdLGEpOnZvaWQgMD09PWI/dGhpczp0aGlzLmVhY2goZnVuY3Rpb24oKXt2
YXIgYz1uLnF1ZXVlKHRoaXMsYSxiKTtuLl9xdWV1ZUhvb2tzKHRoaXMsYSksImZ4
Ij09PWEmJiJpbnByb2dyZXNzIiE9PWNbMF0mJm4uZGVxdWV1ZSh0aGlzLGEpfSl9
LGRlcXVldWU6ZnVuY3Rpb24oYSl7cmV0dXJuIHRoaXMuZWFjaChmdW5jdGlvbigp
e24uZGVxdWV1ZSh0aGlzLGEpfSl9LGNsZWFyUXVldWU6ZnVuY3Rpb24oYSl7cmV0
dXJuIHRoaXMucXVldWUoYXx8ImZ4IixbXSl9LHByb21pc2U6ZnVuY3Rpb24oYSxi
KXt2YXIgYyxkPTEsZT1uLkRlZmVycmVkKCksZj10aGlzLGc9dGhpcy5sZW5ndGgs
aD1mdW5jdGlvbigpey0tZHx8ZS5yZXNvbHZlV2l0aChmLFtmXSl9OyJzdHJpbmci
IT10eXBlb2YgYSYmKGI9YSxhPXZvaWQgMCksYT1hfHwiZngiO3doaWxlKGctLSlj
PUwuZ2V0KGZbZ10sYSsicXVldWVIb29rcyIpLGMmJmMuZW1wdHkmJihkKyssYy5l
bXB0eS5hZGQoaCkpO3JldHVybiBoKCksZS5wcm9taXNlKGIpfX0pO3ZhciBRPS9b
Ky1dPyg/OlxkKlwufClcZCsoPzpbZUVdWystXT9cZCt8KS8uc291cmNlLFI9WyJU
b3AiLCJSaWdodCIsIkJvdHRvbSIsIkxlZnQiXSxTPWZ1bmN0aW9uKGEsYil7cmV0
dXJuIGE9Ynx8YSwibm9uZSI9PT1uLmNzcyhhLCJkaXNwbGF5Iil8fCFuLmNvbnRh
aW5zKGEub3duZXJEb2N1bWVudCxhKX0sVD0vXig/OmNoZWNrYm94fHJhZGlvKSQv
aTshZnVuY3Rpb24oKXt2YXIgYT1sLmNyZWF0ZURvY3VtZW50RnJhZ21lbnQoKSxi
PWEuYXBwZW5kQ2hpbGQobC5jcmVhdGVFbGVtZW50KCJkaXYiKSksYz1sLmNyZWF0
ZUVsZW1lbnQoImlucHV0Iik7Yy5zZXRBdHRyaWJ1dGUoInR5cGUiLCJyYWRpbyIp
LGMuc2V0QXR0cmlidXRlKCJjaGVja2VkIiwiY2hlY2tlZCIpLGMuc2V0QXR0cmli
dXRlKCJuYW1lIiwidCIpLGIuYXBwZW5kQ2hpbGQoYyksay5jaGVja0Nsb25lPWIu
Y2xvbmVOb2RlKCEwKS5jbG9uZU5vZGUoITApLmxhc3RDaGlsZC5jaGVja2VkLGIu
aW5uZXJIVE1MPSI8dGV4dGFyZWE+eDwvdGV4dGFyZWE+IixrLm5vQ2xvbmVDaGVj
a2VkPSEhYi5jbG9uZU5vZGUoITApLmxhc3RDaGlsZC5kZWZhdWx0VmFsdWV9KCk7
dmFyIFU9InVuZGVmaW5lZCI7ay5mb2N1c2luQnViYmxlcz0ib25mb2N1c2luImlu
IGE7dmFyIFY9L15rZXkvLFc9L14oPzptb3VzZXxwb2ludGVyfGNvbnRleHRtZW51
KXxjbGljay8sWD0vXig/OmZvY3VzaW5mb2N1c3xmb2N1c291dGJsdXIpJC8sWT0v
XihbXi5dKikoPzpcLiguKyl8KSQvO2Z1bmN0aW9uIFooKXtyZXR1cm4hMH1mdW5j
dGlvbiAkKCl7cmV0dXJuITF9ZnVuY3Rpb24gXygpe3RyeXtyZXR1cm4gbC5hY3Rp
dmVFbGVtZW50fWNhdGNoKGEpe319bi5ldmVudD17Z2xvYmFsOnt9LGFkZDpmdW5j
dGlvbihhLGIsYyxkLGUpe3ZhciBmLGcsaCxpLGosayxsLG0sbyxwLHEscj1MLmdl
dChhKTtpZihyKXtjLmhhbmRsZXImJihmPWMsYz1mLmhhbmRsZXIsZT1mLnNlbGVj
dG9yKSxjLmd1aWR8fChjLmd1aWQ9bi5ndWlkKyspLChpPXIuZXZlbnRzKXx8KGk9
ci5ldmVudHM9e30pLChnPXIuaGFuZGxlKXx8KGc9ci5oYW5kbGU9ZnVuY3Rpb24o
Yil7cmV0dXJuIHR5cGVvZiBuIT09VSYmbi5ldmVudC50cmlnZ2VyZWQhPT1iLnR5
cGU/bi5ldmVudC5kaXNwYXRjaC5hcHBseShhLGFyZ3VtZW50cyk6dm9pZCAwfSks
Yj0oYnx8IiIpLm1hdGNoKEUpfHxbIiJdLGo9Yi5sZW5ndGg7d2hpbGUoai0tKWg9
WS5leGVjKGJbal0pfHxbXSxvPXE9aFsxXSxwPShoWzJdfHwiIikuc3BsaXQoIi4i
KS5zb3J0KCksbyYmKGw9bi5ldmVudC5zcGVjaWFsW29dfHx7fSxvPShlP2wuZGVs
ZWdhdGVUeXBlOmwuYmluZFR5cGUpfHxvLGw9bi5ldmVudC5zcGVjaWFsW29dfHx7
fSxrPW4uZXh0ZW5kKHt0eXBlOm8sb3JpZ1R5cGU6cSxkYXRhOmQsaGFuZGxlcjpj
LGd1aWQ6Yy5ndWlkLHNlbGVjdG9yOmUsbmVlZHNDb250ZXh0OmUmJm4uZXhwci5t
YXRjaC5uZWVkc0NvbnRleHQudGVzdChlKSxuYW1lc3BhY2U6cC5qb2luKCIuIil9
LGYpLChtPWlbb10pfHwobT1pW29dPVtdLG0uZGVsZWdhdGVDb3VudD0wLGwuc2V0
dXAmJmwuc2V0dXAuY2FsbChhLGQscCxnKSE9PSExfHxhLmFkZEV2ZW50TGlzdGVu
ZXImJmEuYWRkRXZlbnRMaXN0ZW5lcihvLGcsITEpKSxsLmFkZCYmKGwuYWRkLmNh
bGwoYSxrKSxrLmhhbmRsZXIuZ3VpZHx8KGsuaGFuZGxlci5ndWlkPWMuZ3VpZCkp
LGU/bS5zcGxpY2UobS5kZWxlZ2F0ZUNvdW50KyssMCxrKTptLnB1c2goayksbi5l
dmVudC5nbG9iYWxbb109ITApfX0scmVtb3ZlOmZ1bmN0aW9uKGEsYixjLGQsZSl7
dmFyIGYsZyxoLGksaixrLGwsbSxvLHAscSxyPUwuaGFzRGF0YShhKSYmTC5nZXQo
YSk7aWYociYmKGk9ci5ldmVudHMpKXtiPShifHwiIikubWF0Y2goRSl8fFsiIl0s
aj1iLmxlbmd0aDt3aGlsZShqLS0paWYoaD1ZLmV4ZWMoYltqXSl8fFtdLG89cT1o
WzFdLHA9KGhbMl18fCIiKS5zcGxpdCgiLiIpLnNvcnQoKSxvKXtsPW4uZXZlbnQu
c3BlY2lhbFtvXXx8e30sbz0oZD9sLmRlbGVnYXRlVHlwZTpsLmJpbmRUeXBlKXx8
byxtPWlbb118fFtdLGg9aFsyXSYmbmV3IFJlZ0V4cCgiKF58XFwuKSIrcC5qb2lu
KCJcXC4oPzouKlxcLnwpIikrIihcXC58JCkiKSxnPWY9bS5sZW5ndGg7d2hpbGUo
Zi0tKWs9bVtmXSwhZSYmcSE9PWsub3JpZ1R5cGV8fGMmJmMuZ3VpZCE9PWsuZ3Vp
ZHx8aCYmIWgudGVzdChrLm5hbWVzcGFjZSl8fGQmJmQhPT1rLnNlbGVjdG9yJiYo
IioqIiE9PWR8fCFrLnNlbGVjdG9yKXx8KG0uc3BsaWNlKGYsMSksay5zZWxlY3Rv
ciYmbS5kZWxlZ2F0ZUNvdW50LS0sbC5yZW1vdmUmJmwucmVtb3ZlLmNhbGwoYSxr
KSk7ZyYmIW0ubGVuZ3RoJiYobC50ZWFyZG93biYmbC50ZWFyZG93bi5jYWxsKGEs
cCxyLmhhbmRsZSkhPT0hMXx8bi5yZW1vdmVFdmVudChhLG8sci5oYW5kbGUpLGRl
bGV0ZSBpW29dKX1lbHNlIGZvcihvIGluIGkpbi5ldmVudC5yZW1vdmUoYSxvK2Jb
al0sYyxkLCEwKTtuLmlzRW1wdHlPYmplY3QoaSkmJihkZWxldGUgci5oYW5kbGUs
TC5yZW1vdmUoYSwiZXZlbnRzIikpfX0sdHJpZ2dlcjpmdW5jdGlvbihiLGMsZCxl
KXt2YXIgZixnLGgsaSxrLG0sbyxwPVtkfHxsXSxxPWouY2FsbChiLCJ0eXBlIik/
Yi50eXBlOmIscj1qLmNhbGwoYiwibmFtZXNwYWNlIik/Yi5uYW1lc3BhY2Uuc3Bs
aXQoIi4iKTpbXTtpZihnPWg9ZD1kfHxsLDMhPT1kLm5vZGVUeXBlJiY4IT09ZC5u
b2RlVHlwZSYmIVgudGVzdChxK24uZXZlbnQudHJpZ2dlcmVkKSYmKHEuaW5kZXhP
ZigiLiIpPj0wJiYocj1xLnNwbGl0KCIuIikscT1yLnNoaWZ0KCksci5zb3J0KCkp
LGs9cS5pbmRleE9mKCI6Iik8MCYmIm9uIitxLGI9YltuLmV4cGFuZG9dP2I6bmV3
IG4uRXZlbnQocSwib2JqZWN0Ij09dHlwZW9mIGImJmIpLGIuaXNUcmlnZ2VyPWU/
MjozLGIubmFtZXNwYWNlPXIuam9pbigiLiIpLGIubmFtZXNwYWNlX3JlPWIubmFt
ZXNwYWNlP25ldyBSZWdFeHAoIihefFxcLikiK3Iuam9pbigiXFwuKD86LipcXC58
KSIpKyIoXFwufCQpIik6bnVsbCxiLnJlc3VsdD12b2lkIDAsYi50YXJnZXR8fChi
LnRhcmdldD1kKSxjPW51bGw9PWM/W2JdOm4ubWFrZUFycmF5KGMsW2JdKSxvPW4u
ZXZlbnQuc3BlY2lhbFtxXXx8e30sZXx8IW8udHJpZ2dlcnx8by50cmlnZ2VyLmFw
cGx5KGQsYykhPT0hMSkpe2lmKCFlJiYhby5ub0J1YmJsZSYmIW4uaXNXaW5kb3co
ZCkpe2ZvcihpPW8uZGVsZWdhdGVUeXBlfHxxLFgudGVzdChpK3EpfHwoZz1nLnBh
cmVudE5vZGUpO2c7Zz1nLnBhcmVudE5vZGUpcC5wdXNoKGcpLGg9ZztoPT09KGQu
b3duZXJEb2N1bWVudHx8bCkmJnAucHVzaChoLmRlZmF1bHRWaWV3fHxoLnBhcmVu
dFdpbmRvd3x8YSl9Zj0wO3doaWxlKChnPXBbZisrXSkmJiFiLmlzUHJvcGFnYXRp
b25TdG9wcGVkKCkpYi50eXBlPWY+MT9pOm8uYmluZFR5cGV8fHEsbT0oTC5nZXQo
ZywiZXZlbnRzIil8fHt9KVtiLnR5cGVdJiZMLmdldChnLCJoYW5kbGUiKSxtJiZt
LmFwcGx5KGcsYyksbT1rJiZnW2tdLG0mJm0uYXBwbHkmJm4uYWNjZXB0RGF0YShn
KSYmKGIucmVzdWx0PW0uYXBwbHkoZyxjKSxiLnJlc3VsdD09PSExJiZiLnByZXZl
bnREZWZhdWx0KCkpO3JldHVybiBiLnR5cGU9cSxlfHxiLmlzRGVmYXVsdFByZXZl
bnRlZCgpfHxvLl9kZWZhdWx0JiZvLl9kZWZhdWx0LmFwcGx5KHAucG9wKCksYykh
PT0hMXx8IW4uYWNjZXB0RGF0YShkKXx8ayYmbi5pc0Z1bmN0aW9uKGRbcV0pJiYh
bi5pc1dpbmRvdyhkKSYmKGg9ZFtrXSxoJiYoZFtrXT1udWxsKSxuLmV2ZW50LnRy
aWdnZXJlZD1xLGRbcV0oKSxuLmV2ZW50LnRyaWdnZXJlZD12b2lkIDAsaCYmKGRb
a109aCkpLGIucmVzdWx0fX0sZGlzcGF0Y2g6ZnVuY3Rpb24oYSl7YT1uLmV2ZW50
LmZpeChhKTt2YXIgYixjLGUsZixnLGg9W10saT1kLmNhbGwoYXJndW1lbnRzKSxq
PShMLmdldCh0aGlzLCJldmVudHMiKXx8e30pW2EudHlwZV18fFtdLGs9bi5ldmVu
dC5zcGVjaWFsW2EudHlwZV18fHt9O2lmKGlbMF09YSxhLmRlbGVnYXRlVGFyZ2V0
PXRoaXMsIWsucHJlRGlzcGF0Y2h8fGsucHJlRGlzcGF0Y2guY2FsbCh0aGlzLGEp
IT09ITEpe2g9bi5ldmVudC5oYW5kbGVycy5jYWxsKHRoaXMsYSxqKSxiPTA7d2hp
bGUoKGY9aFtiKytdKSYmIWEuaXNQcm9wYWdhdGlvblN0b3BwZWQoKSl7YS5jdXJy
ZW50VGFyZ2V0PWYuZWxlbSxjPTA7d2hpbGUoKGc9Zi5oYW5kbGVyc1tjKytdKSYm
IWEuaXNJbW1lZGlhdGVQcm9wYWdhdGlvblN0b3BwZWQoKSkoIWEubmFtZXNwYWNl
X3JlfHxhLm5hbWVzcGFjZV9yZS50ZXN0KGcubmFtZXNwYWNlKSkmJihhLmhhbmRs
ZU9iaj1nLGEuZGF0YT1nLmRhdGEsZT0oKG4uZXZlbnQuc3BlY2lhbFtnLm9yaWdU
eXBlXXx8e30pLmhhbmRsZXx8Zy5oYW5kbGVyKS5hcHBseShmLmVsZW0saSksdm9p
ZCAwIT09ZSYmKGEucmVzdWx0PWUpPT09ITEmJihhLnByZXZlbnREZWZhdWx0KCks
YS5zdG9wUHJvcGFnYXRpb24oKSkpfXJldHVybiBrLnBvc3REaXNwYXRjaCYmay5w
b3N0RGlzcGF0Y2guY2FsbCh0aGlzLGEpLGEucmVzdWx0fX0saGFuZGxlcnM6ZnVu
Y3Rpb24oYSxiKXt2YXIgYyxkLGUsZixnPVtdLGg9Yi5kZWxlZ2F0ZUNvdW50LGk9
YS50YXJnZXQ7aWYoaCYmaS5ub2RlVHlwZSYmKCFhLmJ1dHRvbnx8ImNsaWNrIiE9
PWEudHlwZSkpZm9yKDtpIT09dGhpcztpPWkucGFyZW50Tm9kZXx8dGhpcylpZihp
LmRpc2FibGVkIT09ITB8fCJjbGljayIhPT1hLnR5cGUpe2ZvcihkPVtdLGM9MDto
PmM7YysrKWY9YltjXSxlPWYuc2VsZWN0b3IrIiAiLHZvaWQgMD09PWRbZV0mJihk
W2VdPWYubmVlZHNDb250ZXh0P24oZSx0aGlzKS5pbmRleChpKT49MDpuLmZpbmQo
ZSx0aGlzLG51bGwsW2ldKS5sZW5ndGgpLGRbZV0mJmQucHVzaChmKTtkLmxlbmd0
aCYmZy5wdXNoKHtlbGVtOmksaGFuZGxlcnM6ZH0pfXJldHVybiBoPGIubGVuZ3Ro
JiZnLnB1c2goe2VsZW06dGhpcyxoYW5kbGVyczpiLnNsaWNlKGgpfSksZ30scHJv
cHM6ImFsdEtleSBidWJibGVzIGNhbmNlbGFibGUgY3RybEtleSBjdXJyZW50VGFy
Z2V0IGV2ZW50UGhhc2UgbWV0YUtleSByZWxhdGVkVGFyZ2V0IHNoaWZ0S2V5IHRh
cmdldCB0aW1lU3RhbXAgdmlldyB3aGljaCIuc3BsaXQoIiAiKSxmaXhIb29rczp7
fSxrZXlIb29rczp7cHJvcHM6ImNoYXIgY2hhckNvZGUga2V5IGtleUNvZGUiLnNw
bGl0KCIgIiksZmlsdGVyOmZ1bmN0aW9uKGEsYil7cmV0dXJuIG51bGw9PWEud2hp
Y2gmJihhLndoaWNoPW51bGwhPWIuY2hhckNvZGU/Yi5jaGFyQ29kZTpiLmtleUNv
ZGUpLGF9fSxtb3VzZUhvb2tzOntwcm9wczoiYnV0dG9uIGJ1dHRvbnMgY2xpZW50
WCBjbGllbnRZIG9mZnNldFggb2Zmc2V0WSBwYWdlWCBwYWdlWSBzY3JlZW5YIHNj
cmVlblkgdG9FbGVtZW50Ii5zcGxpdCgiICIpLGZpbHRlcjpmdW5jdGlvbihhLGIp
e3ZhciBjLGQsZSxmPWIuYnV0dG9uO3JldHVybiBudWxsPT1hLnBhZ2VYJiZudWxs
IT1iLmNsaWVudFgmJihjPWEudGFyZ2V0Lm93bmVyRG9jdW1lbnR8fGwsZD1jLmRv
Y3VtZW50RWxlbWVudCxlPWMuYm9keSxhLnBhZ2VYPWIuY2xpZW50WCsoZCYmZC5z
Y3JvbGxMZWZ0fHxlJiZlLnNjcm9sbExlZnR8fDApLShkJiZkLmNsaWVudExlZnR8
fGUmJmUuY2xpZW50TGVmdHx8MCksYS5wYWdlWT1iLmNsaWVudFkrKGQmJmQuc2Ny
b2xsVG9wfHxlJiZlLnNjcm9sbFRvcHx8MCktKGQmJmQuY2xpZW50VG9wfHxlJiZl
LmNsaWVudFRvcHx8MCkpLGEud2hpY2h8fHZvaWQgMD09PWZ8fChhLndoaWNoPTEm
Zj8xOjImZj8zOjQmZj8yOjApLGF9fSxmaXg6ZnVuY3Rpb24oYSl7aWYoYVtuLmV4
cGFuZG9dKXJldHVybiBhO3ZhciBiLGMsZCxlPWEudHlwZSxmPWEsZz10aGlzLmZp
eEhvb2tzW2VdO2d8fCh0aGlzLmZpeEhvb2tzW2VdPWc9Vy50ZXN0KGUpP3RoaXMu
bW91c2VIb29rczpWLnRlc3QoZSk/dGhpcy5rZXlIb29rczp7fSksZD1nLnByb3Bz
P3RoaXMucHJvcHMuY29uY2F0KGcucHJvcHMpOnRoaXMucHJvcHMsYT1uZXcgbi5F
dmVudChmKSxiPWQubGVuZ3RoO3doaWxlKGItLSljPWRbYl0sYVtjXT1mW2NdO3Jl
dHVybiBhLnRhcmdldHx8KGEudGFyZ2V0PWwpLDM9PT1hLnRhcmdldC5ub2RlVHlw
ZSYmKGEudGFyZ2V0PWEudGFyZ2V0LnBhcmVudE5vZGUpLGcuZmlsdGVyP2cuZmls
dGVyKGEsZik6YX0sc3BlY2lhbDp7bG9hZDp7bm9CdWJibGU6ITB9LGZvY3VzOnt0
cmlnZ2VyOmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXMhPT1fKCkmJnRoaXMuZm9jdXM/
KHRoaXMuZm9jdXMoKSwhMSk6dm9pZCAwfSxkZWxlZ2F0ZVR5cGU6ImZvY3VzaW4i
fSxibHVyOnt0cmlnZ2VyOmZ1bmN0aW9uKCl7cmV0dXJuIHRoaXM9PT1fKCkmJnRo
aXMuYmx1cj8odGhpcy5ibHVyKCksITEpOnZvaWQgMH0sZGVsZWdhdGVUeXBlOiJm
b2N1c291dCJ9LGNsaWNrOnt0cmlnZ2VyOmZ1bmN0aW9uKCl7cmV0dXJuImNoZWNr
Ym94Ij09PXRoaXMudHlwZSYmdGhpcy5jbGljayYmbi5ub2RlTmFtZSh0aGlzLCJp
bnB1dCIpPyh0aGlzLmNsaWNrKCksITEpOnZvaWQgMH0sX2RlZmF1bHQ6ZnVuY3Rp
b24oYSl7cmV0dXJuIG4ubm9kZU5hbWUoYS50YXJnZXQsImEiKX19LGJlZm9yZXVu
bG9hZDp7cG9zdERpc3BhdGNoOmZ1bmN0aW9uKGEpe3ZvaWQgMCE9PWEucmVzdWx0
JiZhLm9yaWdpbmFsRXZlbnQmJihhLm9yaWdpbmFsRXZlbnQucmV0dXJuVmFsdWU9
YS5yZXN1bHQpfX19LHNpbXVsYXRlOmZ1bmN0aW9uKGEsYixjLGQpe3ZhciBlPW4u
ZXh0ZW5kKG5ldyBuLkV2ZW50LGMse3R5cGU6YSxpc1NpbXVsYXRlZDohMCxvcmln
aW5hbEV2ZW50Ont9fSk7ZD9uLmV2ZW50LnRyaWdnZXIoZSxudWxsLGIpOm4uZXZl
bnQuZGlzcGF0Y2guY2FsbChiLGUpLGUuaXNEZWZhdWx0UHJldmVudGVkKCkmJmMu
cHJldmVudERlZmF1bHQoKX19LG4ucmVtb3ZlRXZlbnQ9ZnVuY3Rpb24oYSxiLGMp
e2EucmVtb3ZlRXZlbnRMaXN0ZW5lciYmYS5yZW1vdmVFdmVudExpc3RlbmVyKGIs
YywhMSl9LG4uRXZlbnQ9ZnVuY3Rpb24oYSxiKXtyZXR1cm4gdGhpcyBpbnN0YW5j
ZW9mIG4uRXZlbnQ/KGEmJmEudHlwZT8odGhpcy5vcmlnaW5hbEV2ZW50PWEsdGhp
cy50eXBlPWEudHlwZSx0aGlzLmlzRGVmYXVsdFByZXZlbnRlZD1hLmRlZmF1bHRQ
cmV2ZW50ZWR8fHZvaWQgMD09PWEuZGVmYXVsdFByZXZlbnRlZCYmYS5yZXR1cm5W
YWx1ZT09PSExP1o6JCk6dGhpcy50eXBlPWEsYiYmbi5leHRlbmQodGhpcyxiKSx0
aGlzLnRpbWVTdGFtcD1hJiZhLnRpbWVTdGFtcHx8bi5ub3coKSx2b2lkKHRoaXNb
bi5leHBhbmRvXT0hMCkpOm5ldyBuLkV2ZW50KGEsYil9LG4uRXZlbnQucHJvdG90
eXBlPXtpc0RlZmF1bHRQcmV2ZW50ZWQ6JCxpc1Byb3BhZ2F0aW9uU3RvcHBlZDok
LGlzSW1tZWRpYXRlUHJvcGFnYXRpb25TdG9wcGVkOiQscHJldmVudERlZmF1bHQ6
ZnVuY3Rpb24oKXt2YXIgYT10aGlzLm9yaWdpbmFsRXZlbnQ7dGhpcy5pc0RlZmF1
bHRQcmV2ZW50ZWQ9WixhJiZhLnByZXZlbnREZWZhdWx0JiZhLnByZXZlbnREZWZh
dWx0KCl9LHN0b3BQcm9wYWdhdGlvbjpmdW5jdGlvbigpe3ZhciBhPXRoaXMub3Jp
Z2luYWxFdmVudDt0aGlzLmlzUHJvcGFnYXRpb25TdG9wcGVkPVosYSYmYS5zdG9w
UHJvcGFnYXRpb24mJmEuc3RvcFByb3BhZ2F0aW9uKCl9LHN0b3BJbW1lZGlhdGVQ
cm9wYWdhdGlvbjpmdW5jdGlvbigpe3ZhciBhPXRoaXMub3JpZ2luYWxFdmVudDt0
aGlzLmlzSW1tZWRpYXRlUHJvcGFnYXRpb25TdG9wcGVkPVosYSYmYS5zdG9wSW1t
ZWRpYXRlUHJvcGFnYXRpb24mJmEuc3RvcEltbWVkaWF0ZVByb3BhZ2F0aW9uKCks
dGhpcy5zdG9wUHJvcGFnYXRpb24oKX19LG4uZWFjaCh7bW91c2VlbnRlcjoibW91
c2VvdmVyIixtb3VzZWxlYXZlOiJtb3VzZW91dCIscG9pbnRlcmVudGVyOiJwb2lu
dGVyb3ZlciIscG9pbnRlcmxlYXZlOiJwb2ludGVyb3V0In0sZnVuY3Rpb24oYSxi
KXtuLmV2ZW50LnNwZWNpYWxbYV09e2RlbGVnYXRlVHlwZTpiLGJpbmRUeXBlOmIs
aGFuZGxlOmZ1bmN0aW9uKGEpe3ZhciBjLGQ9dGhpcyxlPWEucmVsYXRlZFRhcmdl
dCxmPWEuaGFuZGxlT2JqO3JldHVybighZXx8ZSE9PWQmJiFuLmNvbnRhaW5zKGQs
ZSkpJiYoYS50eXBlPWYub3JpZ1R5cGUsYz1mLmhhbmRsZXIuYXBwbHkodGhpcyxh
cmd1bWVudHMpLGEudHlwZT1iKSxjfX19KSxrLmZvY3VzaW5CdWJibGVzfHxuLmVh
Y2goe2ZvY3VzOiJmb2N1c2luIixibHVyOiJmb2N1c291dCJ9LGZ1bmN0aW9uKGEs
Yil7dmFyIGM9ZnVuY3Rpb24oYSl7bi5ldmVudC5zaW11bGF0ZShiLGEudGFyZ2V0
LG4uZXZlbnQuZml4KGEpLCEwKX07bi5ldmVudC5zcGVjaWFsW2JdPXtzZXR1cDpm
dW5jdGlvbigpe3ZhciBkPXRoaXMub3duZXJEb2N1bWVudHx8dGhpcyxlPUwuYWNj
ZXNzKGQsYik7ZXx8ZC5hZGRFdmVudExpc3RlbmVyKGEsYywhMCksTC5hY2Nlc3Mo
ZCxiLChlfHwwKSsxKX0sdGVhcmRvd246ZnVuY3Rpb24oKXt2YXIgZD10aGlzLm93
bmVyRG9jdW1lbnR8fHRoaXMsZT1MLmFjY2VzcyhkLGIpLTE7ZT9MLmFjY2Vzcyhk
LGIsZSk6KGQucmVtb3ZlRXZlbnRMaXN0ZW5lcihhLGMsITApLEwucmVtb3ZlKGQs
YikpfX19KSxuLmZuLmV4dGVuZCh7b246ZnVuY3Rpb24oYSxiLGMsZCxlKXt2YXIg
ZixnO2lmKCJvYmplY3QiPT10eXBlb2YgYSl7InN0cmluZyIhPXR5cGVvZiBiJiYo
Yz1jfHxiLGI9dm9pZCAwKTtmb3IoZyBpbiBhKXRoaXMub24oZyxiLGMsYVtnXSxl
KTtyZXR1cm4gdGhpc31pZihudWxsPT1jJiZudWxsPT1kPyhkPWIsYz1iPXZvaWQg
MCk6bnVsbD09ZCYmKCJzdHJpbmciPT10eXBlb2YgYj8oZD1jLGM9dm9pZCAwKToo
ZD1jLGM9YixiPXZvaWQgMCkpLGQ9PT0hMSlkPSQ7ZWxzZSBpZighZClyZXR1cm4g
dGhpcztyZXR1cm4gMT09PWUmJihmPWQsZD1mdW5jdGlvbihhKXtyZXR1cm4gbigp
Lm9mZihhKSxmLmFwcGx5KHRoaXMsYXJndW1lbnRzKX0sZC5ndWlkPWYuZ3VpZHx8
KGYuZ3VpZD1uLmd1aWQrKykpLHRoaXMuZWFjaChmdW5jdGlvbigpe24uZXZlbnQu
YWRkKHRoaXMsYSxkLGMsYil9KX0sb25lOmZ1bmN0aW9uKGEsYixjLGQpe3JldHVy
biB0aGlzLm9uKGEsYixjLGQsMSl9LG9mZjpmdW5jdGlvbihhLGIsYyl7dmFyIGQs
ZTtpZihhJiZhLnByZXZlbnREZWZhdWx0JiZhLmhhbmRsZU9iailyZXR1cm4gZD1h
LmhhbmRsZU9iaixuKGEuZGVsZWdhdGVUYXJnZXQpLm9mZihkLm5hbWVzcGFjZT9k
Lm9yaWdUeXBlKyIuIitkLm5hbWVzcGFjZTpkLm9yaWdUeXBlLGQuc2VsZWN0b3Is
ZC5oYW5kbGVyKSx0aGlzO2lmKCJvYmplY3QiPT10eXBlb2YgYSl7Zm9yKGUgaW4g
YSl0aGlzLm9mZihlLGIsYVtlXSk7cmV0dXJuIHRoaXN9cmV0dXJuKGI9PT0hMXx8
ImZ1bmN0aW9uIj09dHlwZW9mIGIpJiYoYz1iLGI9dm9pZCAwKSxjPT09ITEmJihj
PSQpLHRoaXMuZWFjaChmdW5jdGlvbigpe24uZXZlbnQucmVtb3ZlKHRoaXMsYSxj
LGIpfSl9LHRyaWdnZXI6ZnVuY3Rpb24oYSxiKXtyZXR1cm4gdGhpcy5lYWNoKGZ1
bmN0aW9uKCl7bi5ldmVudC50cmlnZ2VyKGEsYix0aGlzKX0pfSx0cmlnZ2VySGFu
ZGxlcjpmdW5jdGlvbihhLGIpe3ZhciBjPXRoaXNbMF07cmV0dXJuIGM/bi5ldmVu
dC50cmlnZ2VyKGEsYixjLCEwKTp2b2lkIDB9fSk7dmFyIGFiPS88KD8hYXJlYXxi
cnxjb2x8ZW1iZWR8aHJ8aW1nfGlucHV0fGxpbmt8bWV0YXxwYXJhbSkoKFtcdzpd
KylbXj5dKilcLz4vZ2ksYmI9LzwoW1x3Ol0rKS8sY2I9Lzx8JiM/XHcrOy8sZGI9
LzwoPzpzY3JpcHR8c3R5bGV8bGluaykvaSxlYj0vY2hlY2tlZFxzKig/OltePV18
PVxzKi5jaGVja2VkLikvaSxmYj0vXiR8XC8oPzpqYXZhfGVjbWEpc2NyaXB0L2ks
Z2I9L150cnVlXC8oLiopLyxoYj0vXlxzKjwhKD86XFtDREFUQVxbfC0tKXwoPzpc
XVxdfC0tKT5ccyokL2csaWI9e29wdGlvbjpbMSwiPHNlbGVjdCBtdWx0aXBsZT0n
bXVsdGlwbGUnPiIsIjwvc2VsZWN0PiJdLHRoZWFkOlsxLCI8dGFibGU+IiwiPC90
YWJsZT4iXSxjb2w6WzIsIjx0YWJsZT48Y29sZ3JvdXA+IiwiPC9jb2xncm91cD48
L3RhYmxlPiJdLHRyOlsyLCI8dGFibGU+PHRib2R5PiIsIjwvdGJvZHk+PC90YWJs
ZT4iXSx0ZDpbMywiPHRhYmxlPjx0Ym9keT48dHI+IiwiPC90cj48L3Rib2R5Pjwv
dGFibGU+Il0sX2RlZmF1bHQ6WzAsIiIsIiJdfTtpYi5vcHRncm91cD1pYi5vcHRp
b24saWIudGJvZHk9aWIudGZvb3Q9aWIuY29sZ3JvdXA9aWIuY2FwdGlvbj1pYi50
aGVhZCxpYi50aD1pYi50ZDtmdW5jdGlvbiBqYihhLGIpe3JldHVybiBuLm5vZGVO
YW1lKGEsInRhYmxlIikmJm4ubm9kZU5hbWUoMTEhPT1iLm5vZGVUeXBlP2I6Yi5m
aXJzdENoaWxkLCJ0ciIpP2EuZ2V0RWxlbWVudHNCeVRhZ05hbWUoInRib2R5Iilb
MF18fGEuYXBwZW5kQ2hpbGQoYS5vd25lckRvY3VtZW50LmNyZWF0ZUVsZW1lbnQo
InRib2R5IikpOmF9ZnVuY3Rpb24ga2IoYSl7cmV0dXJuIGEudHlwZT0obnVsbCE9
PWEuZ2V0QXR0cmlidXRlKCJ0eXBlIikpKyIvIithLnR5cGUsYX1mdW5jdGlvbiBs
YihhKXt2YXIgYj1nYi5leGVjKGEudHlwZSk7cmV0dXJuIGI/YS50eXBlPWJbMV06
YS5yZW1vdmVBdHRyaWJ1dGUoInR5cGUiKSxhfWZ1bmN0aW9uIG1iKGEsYil7Zm9y
KHZhciBjPTAsZD1hLmxlbmd0aDtkPmM7YysrKUwuc2V0KGFbY10sImdsb2JhbEV2
YWwiLCFifHxMLmdldChiW2NdLCJnbG9iYWxFdmFsIikpfWZ1bmN0aW9uIG5iKGEs
Yil7dmFyIGMsZCxlLGYsZyxoLGksajtpZigxPT09Yi5ub2RlVHlwZSl7aWYoTC5o
YXNEYXRhKGEpJiYoZj1MLmFjY2VzcyhhKSxnPUwuc2V0KGIsZiksaj1mLmV2ZW50
cykpe2RlbGV0ZSBnLmhhbmRsZSxnLmV2ZW50cz17fTtmb3IoZSBpbiBqKWZvcihj
PTAsZD1qW2VdLmxlbmd0aDtkPmM7YysrKW4uZXZlbnQuYWRkKGIsZSxqW2VdW2Nd
KX1NLmhhc0RhdGEoYSkmJihoPU0uYWNjZXNzKGEpLGk9bi5leHRlbmQoe30saCks
TS5zZXQoYixpKSl9fWZ1bmN0aW9uIG9iKGEsYil7dmFyIGM9YS5nZXRFbGVtZW50
c0J5VGFnTmFtZT9hLmdldEVsZW1lbnRzQnlUYWdOYW1lKGJ8fCIqIik6YS5xdWVy
eVNlbGVjdG9yQWxsP2EucXVlcnlTZWxlY3RvckFsbChifHwiKiIpOltdO3JldHVy
biB2b2lkIDA9PT1ifHxiJiZuLm5vZGVOYW1lKGEsYik/bi5tZXJnZShbYV0sYyk6
Y31mdW5jdGlvbiBwYihhLGIpe3ZhciBjPWIubm9kZU5hbWUudG9Mb3dlckNhc2Uo
KTsiaW5wdXQiPT09YyYmVC50ZXN0KGEudHlwZSk/Yi5jaGVja2VkPWEuY2hlY2tl
ZDooImlucHV0Ij09PWN8fCJ0ZXh0YXJlYSI9PT1jKSYmKGIuZGVmYXVsdFZhbHVl
PWEuZGVmYXVsdFZhbHVlKX1uLmV4dGVuZCh7Y2xvbmU6ZnVuY3Rpb24oYSxiLGMp
e3ZhciBkLGUsZixnLGg9YS5jbG9uZU5vZGUoITApLGk9bi5jb250YWlucyhhLm93
bmVyRG9jdW1lbnQsYSk7aWYoIShrLm5vQ2xvbmVDaGVja2VkfHwxIT09YS5ub2Rl
VHlwZSYmMTEhPT1hLm5vZGVUeXBlfHxuLmlzWE1MRG9jKGEpKSlmb3IoZz1vYiho
KSxmPW9iKGEpLGQ9MCxlPWYubGVuZ3RoO2U+ZDtkKyspcGIoZltkXSxnW2RdKTtp
ZihiKWlmKGMpZm9yKGY9Znx8b2IoYSksZz1nfHxvYihoKSxkPTAsZT1mLmxlbmd0
aDtlPmQ7ZCsrKW5iKGZbZF0sZ1tkXSk7ZWxzZSBuYihhLGgpO3JldHVybiBnPW9i
KGgsInNjcmlwdCIpLGcubGVuZ3RoPjAmJm1iKGcsIWkmJm9iKGEsInNjcmlwdCIp
KSxofSxidWlsZEZyYWdtZW50OmZ1bmN0aW9uKGEsYixjLGQpe2Zvcih2YXIgZSxm
LGcsaCxpLGosaz1iLmNyZWF0ZURvY3VtZW50RnJhZ21lbnQoKSxsPVtdLG09MCxv
PWEubGVuZ3RoO28+bTttKyspaWYoZT1hW21dLGV8fDA9PT1lKWlmKCJvYmplY3Qi
PT09bi50eXBlKGUpKW4ubWVyZ2UobCxlLm5vZGVUeXBlP1tlXTplKTtlbHNlIGlm
KGNiLnRlc3QoZSkpe2Y9Znx8ay5hcHBlbmRDaGlsZChiLmNyZWF0ZUVsZW1lbnQo
ImRpdiIpKSxnPShiYi5leGVjKGUpfHxbIiIsIiJdKVsxXS50b0xvd2VyQ2FzZSgp
LGg9aWJbZ118fGliLl9kZWZhdWx0LGYuaW5uZXJIVE1MPWhbMV0rZS5yZXBsYWNl
KGFiLCI8JDE+PC8kMj4iKStoWzJdLGo9aFswXTt3aGlsZShqLS0pZj1mLmxhc3RD
aGlsZDtuLm1lcmdlKGwsZi5jaGlsZE5vZGVzKSxmPWsuZmlyc3RDaGlsZCxmLnRl
eHRDb250ZW50PSIifWVsc2UgbC5wdXNoKGIuY3JlYXRlVGV4dE5vZGUoZSkpO2su
dGV4dENvbnRlbnQ9IiIsbT0wO3doaWxlKGU9bFttKytdKWlmKCghZHx8LTE9PT1u
LmluQXJyYXkoZSxkKSkmJihpPW4uY29udGFpbnMoZS5vd25lckRvY3VtZW50LGUp
LGY9b2Ioay5hcHBlbmRDaGlsZChlKSwic2NyaXB0IiksaSYmbWIoZiksYykpe2o9
MDt3aGlsZShlPWZbaisrXSlmYi50ZXN0KGUudHlwZXx8IiIpJiZjLnB1c2goZSl9
cmV0dXJuIGt9LGNsZWFuRGF0YTpmdW5jdGlvbihhKXtmb3IodmFyIGIsYyxkLGUs
Zj1uLmV2ZW50LnNwZWNpYWwsZz0wO3ZvaWQgMCE9PShjPWFbZ10pO2crKyl7aWYo
bi5hY2NlcHREYXRhKGMpJiYoZT1jW0wuZXhwYW5kb10sZSYmKGI9TC5jYWNoZVtl
XSkpKXtpZihiLmV2ZW50cylmb3IoZCBpbiBiLmV2ZW50cylmW2RdP24uZXZlbnQu
cmVtb3ZlKGMsZCk6bi5yZW1vdmVFdmVudChjLGQsYi5oYW5kbGUpO0wuY2FjaGVb
ZV0mJmRlbGV0ZSBMLmNhY2hlW2VdfWRlbGV0ZSBNLmNhY2hlW2NbTS5leHBhbmRv
XV19fX0pLG4uZm4uZXh0ZW5kKHt0ZXh0OmZ1bmN0aW9uKGEpe3JldHVybiBKKHRo
aXMsZnVuY3Rpb24oYSl7cmV0dXJuIHZvaWQgMD09PWE/bi50ZXh0KHRoaXMpOnRo
aXMuZW1wdHkoKS5lYWNoKGZ1bmN0aW9uKCl7KDE9PT10aGlzLm5vZGVUeXBlfHwx
MT09PXRoaXMubm9kZVR5cGV8fDk9PT10aGlzLm5vZGVUeXBlKSYmKHRoaXMudGV4
dENvbnRlbnQ9YSl9KX0sbnVsbCxhLGFyZ3VtZW50cy5sZW5ndGgpfSxhcHBlbmQ6
ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5kb21NYW5pcChhcmd1bWVudHMsZnVuY3Rp
b24oYSl7aWYoMT09PXRoaXMubm9kZVR5cGV8fDExPT09dGhpcy5ub2RlVHlwZXx8
OT09PXRoaXMubm9kZVR5cGUpe3ZhciBiPWpiKHRoaXMsYSk7Yi5hcHBlbmRDaGls
ZChhKX19KX0scHJlcGVuZDpmdW5jdGlvbigpe3JldHVybiB0aGlzLmRvbU1hbmlw
KGFyZ3VtZW50cyxmdW5jdGlvbihhKXtpZigxPT09dGhpcy5ub2RlVHlwZXx8MTE9
PT10aGlzLm5vZGVUeXBlfHw5PT09dGhpcy5ub2RlVHlwZSl7dmFyIGI9amIodGhp
cyxhKTtiLmluc2VydEJlZm9yZShhLGIuZmlyc3RDaGlsZCl9fSl9LGJlZm9yZTpm
dW5jdGlvbigpe3JldHVybiB0aGlzLmRvbU1hbmlwKGFyZ3VtZW50cyxmdW5jdGlv
bihhKXt0aGlzLnBhcmVudE5vZGUmJnRoaXMucGFyZW50Tm9kZS5pbnNlcnRCZWZv
cmUoYSx0aGlzKX0pfSxhZnRlcjpmdW5jdGlvbigpe3JldHVybiB0aGlzLmRvbU1h
bmlwKGFyZ3VtZW50cyxmdW5jdGlvbihhKXt0aGlzLnBhcmVudE5vZGUmJnRoaXMu
cGFyZW50Tm9kZS5pbnNlcnRCZWZvcmUoYSx0aGlzLm5leHRTaWJsaW5nKX0pfSxy
ZW1vdmU6ZnVuY3Rpb24oYSxiKXtmb3IodmFyIGMsZD1hP24uZmlsdGVyKGEsdGhp
cyk6dGhpcyxlPTA7bnVsbCE9KGM9ZFtlXSk7ZSsrKWJ8fDEhPT1jLm5vZGVUeXBl
fHxuLmNsZWFuRGF0YShvYihjKSksYy5wYXJlbnROb2RlJiYoYiYmbi5jb250YWlu
cyhjLm93bmVyRG9jdW1lbnQsYykmJm1iKG9iKGMsInNjcmlwdCIpKSxjLnBhcmVu
dE5vZGUucmVtb3ZlQ2hpbGQoYykpO3JldHVybiB0aGlzfSxlbXB0eTpmdW5jdGlv
bigpe2Zvcih2YXIgYSxiPTA7bnVsbCE9KGE9dGhpc1tiXSk7YisrKTE9PT1hLm5v
ZGVUeXBlJiYobi5jbGVhbkRhdGEob2IoYSwhMSkpLGEudGV4dENvbnRlbnQ9IiIp
O3JldHVybiB0aGlzfSxjbG9uZTpmdW5jdGlvbihhLGIpe3JldHVybiBhPW51bGw9
PWE/ITE6YSxiPW51bGw9PWI/YTpiLHRoaXMubWFwKGZ1bmN0aW9uKCl7cmV0dXJu
IG4uY2xvbmUodGhpcyxhLGIpfSl9LGh0bWw6ZnVuY3Rpb24oYSl7cmV0dXJuIEoo
dGhpcyxmdW5jdGlvbihhKXt2YXIgYj10aGlzWzBdfHx7fSxjPTAsZD10aGlzLmxl
bmd0aDtpZih2b2lkIDA9PT1hJiYxPT09Yi5ub2RlVHlwZSlyZXR1cm4gYi5pbm5l
ckhUTUw7aWYoInN0cmluZyI9PXR5cGVvZiBhJiYhZGIudGVzdChhKSYmIWliWyhi
Yi5leGVjKGEpfHxbIiIsIiJdKVsxXS50b0xvd2VyQ2FzZSgpXSl7YT1hLnJlcGxh
Y2UoYWIsIjwkMT48LyQyPiIpO3RyeXtmb3IoO2Q+YztjKyspYj10aGlzW2NdfHx7
fSwxPT09Yi5ub2RlVHlwZSYmKG4uY2xlYW5EYXRhKG9iKGIsITEpKSxiLmlubmVy
SFRNTD1hKTtiPTB9Y2F0Y2goZSl7fX1iJiZ0aGlzLmVtcHR5KCkuYXBwZW5kKGEp
fSxudWxsLGEsYXJndW1lbnRzLmxlbmd0aCl9LHJlcGxhY2VXaXRoOmZ1bmN0aW9u
KCl7dmFyIGE9YXJndW1lbnRzWzBdO3JldHVybiB0aGlzLmRvbU1hbmlwKGFyZ3Vt
ZW50cyxmdW5jdGlvbihiKXthPXRoaXMucGFyZW50Tm9kZSxuLmNsZWFuRGF0YShv
Yih0aGlzKSksYSYmYS5yZXBsYWNlQ2hpbGQoYix0aGlzKX0pLGEmJihhLmxlbmd0
aHx8YS5ub2RlVHlwZSk/dGhpczp0aGlzLnJlbW92ZSgpfSxkZXRhY2g6ZnVuY3Rp
b24oYSl7cmV0dXJuIHRoaXMucmVtb3ZlKGEsITApfSxkb21NYW5pcDpmdW5jdGlv
bihhLGIpe2E9ZS5hcHBseShbXSxhKTt2YXIgYyxkLGYsZyxoLGksaj0wLGw9dGhp
cy5sZW5ndGgsbT10aGlzLG89bC0xLHA9YVswXSxxPW4uaXNGdW5jdGlvbihwKTtp
ZihxfHxsPjEmJiJzdHJpbmciPT10eXBlb2YgcCYmIWsuY2hlY2tDbG9uZSYmZWIu
dGVzdChwKSlyZXR1cm4gdGhpcy5lYWNoKGZ1bmN0aW9uKGMpe3ZhciBkPW0uZXEo
Yyk7cSYmKGFbMF09cC5jYWxsKHRoaXMsYyxkLmh0bWwoKSkpLGQuZG9tTWFuaXAo
YSxiKX0pO2lmKGwmJihjPW4uYnVpbGRGcmFnbWVudChhLHRoaXNbMF0ub3duZXJE
b2N1bWVudCwhMSx0aGlzKSxkPWMuZmlyc3RDaGlsZCwxPT09Yy5jaGlsZE5vZGVz
Lmxlbmd0aCYmKGM9ZCksZCkpe2ZvcihmPW4ubWFwKG9iKGMsInNjcmlwdCIpLGti
KSxnPWYubGVuZ3RoO2w+ajtqKyspaD1jLGohPT1vJiYoaD1uLmNsb25lKGgsITAs
ITApLGcmJm4ubWVyZ2UoZixvYihoLCJzY3JpcHQiKSkpLGIuY2FsbCh0aGlzW2pd
LGgsaik7aWYoZylmb3IoaT1mW2YubGVuZ3RoLTFdLm93bmVyRG9jdW1lbnQsbi5t
YXAoZixsYiksaj0wO2c+ajtqKyspaD1mW2pdLGZiLnRlc3QoaC50eXBlfHwiIikm
JiFMLmFjY2VzcyhoLCJnbG9iYWxFdmFsIikmJm4uY29udGFpbnMoaSxoKSYmKGgu
c3JjP24uX2V2YWxVcmwmJm4uX2V2YWxVcmwoaC5zcmMpOm4uZ2xvYmFsRXZhbCho
LnRleHRDb250ZW50LnJlcGxhY2UoaGIsIiIpKSl9cmV0dXJuIHRoaXN9fSksbi5l
YWNoKHthcHBlbmRUbzoiYXBwZW5kIixwcmVwZW5kVG86InByZXBlbmQiLGluc2Vy
dEJlZm9yZToiYmVmb3JlIixpbnNlcnRBZnRlcjoiYWZ0ZXIiLHJlcGxhY2VBbGw6
InJlcGxhY2VXaXRoIn0sZnVuY3Rpb24oYSxiKXtuLmZuW2FdPWZ1bmN0aW9uKGEp
e2Zvcih2YXIgYyxkPVtdLGU9bihhKSxnPWUubGVuZ3RoLTEsaD0wO2c+PWg7aCsr
KWM9aD09PWc/dGhpczp0aGlzLmNsb25lKCEwKSxuKGVbaF0pW2JdKGMpLGYuYXBw
bHkoZCxjLmdldCgpKTtyZXR1cm4gdGhpcy5wdXNoU3RhY2soZCl9fSk7dmFyIHFi
LHJiPXt9O2Z1bmN0aW9uIHNiKGIsYyl7dmFyIGQsZT1uKGMuY3JlYXRlRWxlbWVu
dChiKSkuYXBwZW5kVG8oYy5ib2R5KSxmPWEuZ2V0RGVmYXVsdENvbXB1dGVkU3R5
bGUmJihkPWEuZ2V0RGVmYXVsdENvbXB1dGVkU3R5bGUoZVswXSkpP2QuZGlzcGxh
eTpuLmNzcyhlWzBdLCJkaXNwbGF5Iik7cmV0dXJuIGUuZGV0YWNoKCksZn1mdW5j
dGlvbiB0YihhKXt2YXIgYj1sLGM9cmJbYV07cmV0dXJuIGN8fChjPXNiKGEsYiks
Im5vbmUiIT09YyYmY3x8KHFiPShxYnx8bigiPGlmcmFtZSBmcmFtZWJvcmRlcj0n
MCcgd2lkdGg9JzAnIGhlaWdodD0nMCcvPiIpKS5hcHBlbmRUbyhiLmRvY3VtZW50
RWxlbWVudCksYj1xYlswXS5jb250ZW50RG9jdW1lbnQsYi53cml0ZSgpLGIuY2xv
c2UoKSxjPXNiKGEsYikscWIuZGV0YWNoKCkpLHJiW2FdPWMpLGN9dmFyIHViPS9e
bWFyZ2luLyx2Yj1uZXcgUmVnRXhwKCJeKCIrUSsiKSg/IXB4KVthLXolXSskIiwi
aSIpLHdiPWZ1bmN0aW9uKGEpe3JldHVybiBhLm93bmVyRG9jdW1lbnQuZGVmYXVs
dFZpZXcuZ2V0Q29tcHV0ZWRTdHlsZShhLG51bGwpfTtmdW5jdGlvbiB4YihhLGIs
Yyl7dmFyIGQsZSxmLGcsaD1hLnN0eWxlO3JldHVybiBjPWN8fHdiKGEpLGMmJihn
PWMuZ2V0UHJvcGVydHlWYWx1ZShiKXx8Y1tiXSksYyYmKCIiIT09Z3x8bi5jb250
YWlucyhhLm93bmVyRG9jdW1lbnQsYSl8fChnPW4uc3R5bGUoYSxiKSksdmIudGVz
dChnKSYmdWIudGVzdChiKSYmKGQ9aC53aWR0aCxlPWgubWluV2lkdGgsZj1oLm1h
eFdpZHRoLGgubWluV2lkdGg9aC5tYXhXaWR0aD1oLndpZHRoPWcsZz1jLndpZHRo
LGgud2lkdGg9ZCxoLm1pbldpZHRoPWUsaC5tYXhXaWR0aD1mKSksdm9pZCAwIT09
Zz9nKyIiOmd9ZnVuY3Rpb24geWIoYSxiKXtyZXR1cm57Z2V0OmZ1bmN0aW9uKCl7
cmV0dXJuIGEoKT92b2lkIGRlbGV0ZSB0aGlzLmdldDoodGhpcy5nZXQ9YikuYXBw
bHkodGhpcyxhcmd1bWVudHMpfX19IWZ1bmN0aW9uKCl7dmFyIGIsYyxkPWwuZG9j
dW1lbnRFbGVtZW50LGU9bC5jcmVhdGVFbGVtZW50KCJkaXYiKSxmPWwuY3JlYXRl
RWxlbWVudCgiZGl2Iik7aWYoZi5zdHlsZSl7Zi5zdHlsZS5iYWNrZ3JvdW5kQ2xp
cD0iY29udGVudC1ib3giLGYuY2xvbmVOb2RlKCEwKS5zdHlsZS5iYWNrZ3JvdW5k
Q2xpcD0iIixrLmNsZWFyQ2xvbmVTdHlsZT0iY29udGVudC1ib3giPT09Zi5zdHls
ZS5iYWNrZ3JvdW5kQ2xpcCxlLnN0eWxlLmNzc1RleHQ9ImJvcmRlcjowO3dpZHRo
OjA7aGVpZ2h0OjA7dG9wOjA7bGVmdDotOTk5OXB4O21hcmdpbi10b3A6MXB4O3Bv
c2l0aW9uOmFic29sdXRlIixlLmFwcGVuZENoaWxkKGYpO2Z1bmN0aW9uIGcoKXtm
LnN0eWxlLmNzc1RleHQ9Ii13ZWJraXQtYm94LXNpemluZzpib3JkZXItYm94Oy1t
b3otYm94LXNpemluZzpib3JkZXItYm94O2JveC1zaXppbmc6Ym9yZGVyLWJveDtk
aXNwbGF5OmJsb2NrO21hcmdpbi10b3A6MSU7dG9wOjElO2JvcmRlcjoxcHg7cGFk
ZGluZzoxcHg7d2lkdGg6NHB4O3Bvc2l0aW9uOmFic29sdXRlIixmLmlubmVySFRN
TD0iIixkLmFwcGVuZENoaWxkKGUpO3ZhciBnPWEuZ2V0Q29tcHV0ZWRTdHlsZShm
LG51bGwpO2I9IjElIiE9PWcudG9wLGM9IjRweCI9PT1nLndpZHRoLGQucmVtb3Zl
Q2hpbGQoZSl9YS5nZXRDb21wdXRlZFN0eWxlJiZuLmV4dGVuZChrLHtwaXhlbFBv
c2l0aW9uOmZ1bmN0aW9uKCl7cmV0dXJuIGcoKSxifSxib3hTaXppbmdSZWxpYWJs
ZTpmdW5jdGlvbigpe3JldHVybiBudWxsPT1jJiZnKCksY30scmVsaWFibGVNYXJn
aW5SaWdodDpmdW5jdGlvbigpe3ZhciBiLGM9Zi5hcHBlbmRDaGlsZChsLmNyZWF0
ZUVsZW1lbnQoImRpdiIpKTtyZXR1cm4gYy5zdHlsZS5jc3NUZXh0PWYuc3R5bGUu
Y3NzVGV4dD0iLXdlYmtpdC1ib3gtc2l6aW5nOmNvbnRlbnQtYm94Oy1tb3otYm94
LXNpemluZzpjb250ZW50LWJveDtib3gtc2l6aW5nOmNvbnRlbnQtYm94O2Rpc3Bs
YXk6YmxvY2s7bWFyZ2luOjA7Ym9yZGVyOjA7cGFkZGluZzowIixjLnN0eWxlLm1h
cmdpblJpZ2h0PWMuc3R5bGUud2lkdGg9IjAiLGYuc3R5bGUud2lkdGg9IjFweCIs
ZC5hcHBlbmRDaGlsZChlKSxiPSFwYXJzZUZsb2F0KGEuZ2V0Q29tcHV0ZWRTdHls
ZShjLG51bGwpLm1hcmdpblJpZ2h0KSxkLnJlbW92ZUNoaWxkKGUpLGJ9fSl9fSgp
LG4uc3dhcD1mdW5jdGlvbihhLGIsYyxkKXt2YXIgZSxmLGc9e307Zm9yKGYgaW4g
YilnW2ZdPWEuc3R5bGVbZl0sYS5zdHlsZVtmXT1iW2ZdO2U9Yy5hcHBseShhLGR8
fFtdKTtmb3IoZiBpbiBiKWEuc3R5bGVbZl09Z1tmXTtyZXR1cm4gZX07dmFyIHpi
PS9eKG5vbmV8dGFibGUoPyEtY1tlYV0pLispLyxBYj1uZXcgUmVnRXhwKCJeKCIr
USsiKSguKikkIiwiaSIpLEJiPW5ldyBSZWdFeHAoIl4oWystXSk9KCIrUSsiKSIs
ImkiKSxDYj17cG9zaXRpb246ImFic29sdXRlIix2aXNpYmlsaXR5OiJoaWRkZW4i
LGRpc3BsYXk6ImJsb2NrIn0sRGI9e2xldHRlclNwYWNpbmc6IjAiLGZvbnRXZWln
aHQ6IjQwMCJ9LEViPVsiV2Via2l0IiwiTyIsIk1veiIsIm1zIl07ZnVuY3Rpb24g
RmIoYSxiKXtpZihiIGluIGEpcmV0dXJuIGI7dmFyIGM9YlswXS50b1VwcGVyQ2Fz
ZSgpK2Iuc2xpY2UoMSksZD1iLGU9RWIubGVuZ3RoO3doaWxlKGUtLSlpZihiPUVi
W2VdK2MsYiBpbiBhKXJldHVybiBiO3JldHVybiBkfWZ1bmN0aW9uIEdiKGEsYixj
KXt2YXIgZD1BYi5leGVjKGIpO3JldHVybiBkP01hdGgubWF4KDAsZFsxXS0oY3x8
MCkpKyhkWzJdfHwicHgiKTpifWZ1bmN0aW9uIEhiKGEsYixjLGQsZSl7Zm9yKHZh
ciBmPWM9PT0oZD8iYm9yZGVyIjoiY29udGVudCIpPzQ6IndpZHRoIj09PWI/MTow
LGc9MDs0PmY7Zis9MikibWFyZ2luIj09PWMmJihnKz1uLmNzcyhhLGMrUltmXSwh
MCxlKSksZD8oImNvbnRlbnQiPT09YyYmKGctPW4uY3NzKGEsInBhZGRpbmciK1Jb
Zl0sITAsZSkpLCJtYXJnaW4iIT09YyYmKGctPW4uY3NzKGEsImJvcmRlciIrUltm
XSsiV2lkdGgiLCEwLGUpKSk6KGcrPW4uY3NzKGEsInBhZGRpbmciK1JbZl0sITAs
ZSksInBhZGRpbmciIT09YyYmKGcrPW4uY3NzKGEsImJvcmRlciIrUltmXSsiV2lk
dGgiLCEwLGUpKSk7cmV0dXJuIGd9ZnVuY3Rpb24gSWIoYSxiLGMpe3ZhciBkPSEw
LGU9IndpZHRoIj09PWI/YS5vZmZzZXRXaWR0aDphLm9mZnNldEhlaWdodCxmPXdi
KGEpLGc9ImJvcmRlci1ib3giPT09bi5jc3MoYSwiYm94U2l6aW5nIiwhMSxmKTtp
ZigwPj1lfHxudWxsPT1lKXtpZihlPXhiKGEsYixmKSwoMD5lfHxudWxsPT1lKSYm
KGU9YS5zdHlsZVtiXSksdmIudGVzdChlKSlyZXR1cm4gZTtkPWcmJihrLmJveFNp
emluZ1JlbGlhYmxlKCl8fGU9PT1hLnN0eWxlW2JdKSxlPXBhcnNlRmxvYXQoZSl8
fDB9cmV0dXJuIGUrSGIoYSxiLGN8fChnPyJib3JkZXIiOiJjb250ZW50IiksZCxm
KSsicHgifWZ1bmN0aW9uIEpiKGEsYil7Zm9yKHZhciBjLGQsZSxmPVtdLGc9MCxo
PWEubGVuZ3RoO2g+ZztnKyspZD1hW2ddLGQuc3R5bGUmJihmW2ddPUwuZ2V0KGQs
Im9sZGRpc3BsYXkiKSxjPWQuc3R5bGUuZGlzcGxheSxiPyhmW2ddfHwibm9uZSIh
PT1jfHwoZC5zdHlsZS5kaXNwbGF5PSIiKSwiIj09PWQuc3R5bGUuZGlzcGxheSYm
UyhkKSYmKGZbZ109TC5hY2Nlc3MoZCwib2xkZGlzcGxheSIsdGIoZC5ub2RlTmFt
ZSkpKSk6KGU9UyhkKSwibm9uZSI9PT1jJiZlfHxMLnNldChkLCJvbGRkaXNwbGF5
IixlP2M6bi5jc3MoZCwiZGlzcGxheSIpKSkpO2ZvcihnPTA7aD5nO2crKylkPWFb
Z10sZC5zdHlsZSYmKGImJiJub25lIiE9PWQuc3R5bGUuZGlzcGxheSYmIiIhPT1k
LnN0eWxlLmRpc3BsYXl8fChkLnN0eWxlLmRpc3BsYXk9Yj9mW2ddfHwiIjoibm9u
ZSIpKTtyZXR1cm4gYX1uLmV4dGVuZCh7Y3NzSG9va3M6e29wYWNpdHk6e2dldDpm
dW5jdGlvbihhLGIpe2lmKGIpe3ZhciBjPXhiKGEsIm9wYWNpdHkiKTtyZXR1cm4i
Ij09PWM/IjEiOmN9fX19LGNzc051bWJlcjp7Y29sdW1uQ291bnQ6ITAsZmlsbE9w
YWNpdHk6ITAsZmxleEdyb3c6ITAsZmxleFNocmluazohMCxmb250V2VpZ2h0OiEw
LGxpbmVIZWlnaHQ6ITAsb3BhY2l0eTohMCxvcmRlcjohMCxvcnBoYW5zOiEwLHdp
ZG93czohMCx6SW5kZXg6ITAsem9vbTohMH0sY3NzUHJvcHM6eyJmbG9hdCI6ImNz
c0Zsb2F0In0sc3R5bGU6ZnVuY3Rpb24oYSxiLGMsZCl7aWYoYSYmMyE9PWEubm9k
ZVR5cGUmJjghPT1hLm5vZGVUeXBlJiZhLnN0eWxlKXt2YXIgZSxmLGcsaD1uLmNh
bWVsQ2FzZShiKSxpPWEuc3R5bGU7cmV0dXJuIGI9bi5jc3NQcm9wc1toXXx8KG4u
Y3NzUHJvcHNbaF09RmIoaSxoKSksZz1uLmNzc0hvb2tzW2JdfHxuLmNzc0hvb2tz
W2hdLHZvaWQgMD09PWM/ZyYmImdldCJpbiBnJiZ2b2lkIDAhPT0oZT1nLmdldChh
LCExLGQpKT9lOmlbYl06KGY9dHlwZW9mIGMsInN0cmluZyI9PT1mJiYoZT1CYi5l
eGVjKGMpKSYmKGM9KGVbMV0rMSkqZVsyXStwYXJzZUZsb2F0KG4uY3NzKGEsYikp
LGY9Im51bWJlciIpLG51bGwhPWMmJmM9PT1jJiYoIm51bWJlciIhPT1mfHxuLmNz
c051bWJlcltoXXx8KGMrPSJweCIpLGsuY2xlYXJDbG9uZVN0eWxlfHwiIiE9PWN8
fDAhPT1iLmluZGV4T2YoImJhY2tncm91bmQiKXx8KGlbYl09ImluaGVyaXQiKSxn
JiYic2V0ImluIGcmJnZvaWQgMD09PShjPWcuc2V0KGEsYyxkKSl8fChpW2JdPWMp
KSx2b2lkIDApfX0sY3NzOmZ1bmN0aW9uKGEsYixjLGQpe3ZhciBlLGYsZyxoPW4u
Y2FtZWxDYXNlKGIpO3JldHVybiBiPW4uY3NzUHJvcHNbaF18fChuLmNzc1Byb3Bz
W2hdPUZiKGEuc3R5bGUsaCkpLGc9bi5jc3NIb29rc1tiXXx8bi5jc3NIb29rc1to
XSxnJiYiZ2V0ImluIGcmJihlPWcuZ2V0KGEsITAsYykpLHZvaWQgMD09PWUmJihl
PXhiKGEsYixkKSksIm5vcm1hbCI9PT1lJiZiIGluIERiJiYoZT1EYltiXSksIiI9
PT1jfHxjPyhmPXBhcnNlRmxvYXQoZSksYz09PSEwfHxuLmlzTnVtZXJpYyhmKT9m
fHwwOmUpOmV9fSksbi5lYWNoKFsiaGVpZ2h0Iiwid2lkdGgiXSxmdW5jdGlvbihh
LGIpe24uY3NzSG9va3NbYl09e2dldDpmdW5jdGlvbihhLGMsZCl7cmV0dXJuIGM/
emIudGVzdChuLmNzcyhhLCJkaXNwbGF5IikpJiYwPT09YS5vZmZzZXRXaWR0aD9u
LnN3YXAoYSxDYixmdW5jdGlvbigpe3JldHVybiBJYihhLGIsZCl9KTpJYihhLGIs
ZCk6dm9pZCAwfSxzZXQ6ZnVuY3Rpb24oYSxjLGQpe3ZhciBlPWQmJndiKGEpO3Jl
dHVybiBHYihhLGMsZD9IYihhLGIsZCwiYm9yZGVyLWJveCI9PT1uLmNzcyhhLCJi
b3hTaXppbmciLCExLGUpLGUpOjApfX19KSxuLmNzc0hvb2tzLm1hcmdpblJpZ2h0
PXliKGsucmVsaWFibGVNYXJnaW5SaWdodCxmdW5jdGlvbihhLGIpe3JldHVybiBi
P24uc3dhcChhLHtkaXNwbGF5OiJpbmxpbmUtYmxvY2sifSx4YixbYSwibWFyZ2lu
UmlnaHQiXSk6dm9pZCAwfSksbi5lYWNoKHttYXJnaW46IiIscGFkZGluZzoiIixi
b3JkZXI6IldpZHRoIn0sZnVuY3Rpb24oYSxiKXtuLmNzc0hvb2tzW2ErYl09e2V4
cGFuZDpmdW5jdGlvbihjKXtmb3IodmFyIGQ9MCxlPXt9LGY9InN0cmluZyI9PXR5
cGVvZiBjP2Muc3BsaXQoIiAiKTpbY107ND5kO2QrKyllW2ErUltkXStiXT1mW2Rd
fHxmW2QtMl18fGZbMF07cmV0dXJuIGV9fSx1Yi50ZXN0KGEpfHwobi5jc3NIb29r
c1thK2JdLnNldD1HYil9KSxuLmZuLmV4dGVuZCh7Y3NzOmZ1bmN0aW9uKGEsYil7
cmV0dXJuIEoodGhpcyxmdW5jdGlvbihhLGIsYyl7dmFyIGQsZSxmPXt9LGc9MDtp
ZihuLmlzQXJyYXkoYikpe2ZvcihkPXdiKGEpLGU9Yi5sZW5ndGg7ZT5nO2crKylm
W2JbZ11dPW4uY3NzKGEsYltnXSwhMSxkKTtyZXR1cm4gZn1yZXR1cm4gdm9pZCAw
IT09Yz9uLnN0eWxlKGEsYixjKTpuLmNzcyhhLGIpfSxhLGIsYXJndW1lbnRzLmxl
bmd0aD4xKX0sc2hvdzpmdW5jdGlvbigpe3JldHVybiBKYih0aGlzLCEwKX0saGlk
ZTpmdW5jdGlvbigpe3JldHVybiBKYih0aGlzKX0sdG9nZ2xlOmZ1bmN0aW9uKGEp
e3JldHVybiJib29sZWFuIj09dHlwZW9mIGE/YT90aGlzLnNob3coKTp0aGlzLmhp
ZGUoKTp0aGlzLmVhY2goZnVuY3Rpb24oKXtTKHRoaXMpP24odGhpcykuc2hvdygp
Om4odGhpcykuaGlkZSgpfSl9fSk7ZnVuY3Rpb24gS2IoYSxiLGMsZCxlKXtyZXR1
cm4gbmV3IEtiLnByb3RvdHlwZS5pbml0KGEsYixjLGQsZSl9bi5Ud2Vlbj1LYixL
Yi5wcm90b3R5cGU9e2NvbnN0cnVjdG9yOktiLGluaXQ6ZnVuY3Rpb24oYSxiLGMs
ZCxlLGYpe3RoaXMuZWxlbT1hLHRoaXMucHJvcD1jLHRoaXMuZWFzaW5nPWV8fCJz
d2luZyIsdGhpcy5vcHRpb25zPWIsdGhpcy5zdGFydD10aGlzLm5vdz10aGlzLmN1
cigpLHRoaXMuZW5kPWQsdGhpcy51bml0PWZ8fChuLmNzc051bWJlcltjXT8iIjoi
cHgiKX0sY3VyOmZ1bmN0aW9uKCl7dmFyIGE9S2IucHJvcEhvb2tzW3RoaXMucHJv
cF07cmV0dXJuIGEmJmEuZ2V0P2EuZ2V0KHRoaXMpOktiLnByb3BIb29rcy5fZGVm
YXVsdC5nZXQodGhpcyl9LHJ1bjpmdW5jdGlvbihhKXt2YXIgYixjPUtiLnByb3BI
b29rc1t0aGlzLnByb3BdO3JldHVybiB0aGlzLnBvcz1iPXRoaXMub3B0aW9ucy5k
dXJhdGlvbj9uLmVhc2luZ1t0aGlzLmVhc2luZ10oYSx0aGlzLm9wdGlvbnMuZHVy
YXRpb24qYSwwLDEsdGhpcy5vcHRpb25zLmR1cmF0aW9uKTphLHRoaXMubm93PSh0
aGlzLmVuZC10aGlzLnN0YXJ0KSpiK3RoaXMuc3RhcnQsdGhpcy5vcHRpb25zLnN0
ZXAmJnRoaXMub3B0aW9ucy5zdGVwLmNhbGwodGhpcy5lbGVtLHRoaXMubm93LHRo
aXMpLGMmJmMuc2V0P2Muc2V0KHRoaXMpOktiLnByb3BIb29rcy5fZGVmYXVsdC5z
ZXQodGhpcyksdGhpc319LEtiLnByb3RvdHlwZS5pbml0LnByb3RvdHlwZT1LYi5w
cm90b3R5cGUsS2IucHJvcEhvb2tzPXtfZGVmYXVsdDp7Z2V0OmZ1bmN0aW9uKGEp
e3ZhciBiO3JldHVybiBudWxsPT1hLmVsZW1bYS5wcm9wXXx8YS5lbGVtLnN0eWxl
JiZudWxsIT1hLmVsZW0uc3R5bGVbYS5wcm9wXT8oYj1uLmNzcyhhLmVsZW0sYS5w
cm9wLCIiKSxiJiYiYXV0byIhPT1iP2I6MCk6YS5lbGVtW2EucHJvcF19LHNldDpm
dW5jdGlvbihhKXtuLmZ4LnN0ZXBbYS5wcm9wXT9uLmZ4LnN0ZXBbYS5wcm9wXShh
KTphLmVsZW0uc3R5bGUmJihudWxsIT1hLmVsZW0uc3R5bGVbbi5jc3NQcm9wc1th
LnByb3BdXXx8bi5jc3NIb29rc1thLnByb3BdKT9uLnN0eWxlKGEuZWxlbSxhLnBy
b3AsYS5ub3crYS51bml0KTphLmVsZW1bYS5wcm9wXT1hLm5vd319fSxLYi5wcm9w
SG9va3Muc2Nyb2xsVG9wPUtiLnByb3BIb29rcy5zY3JvbGxMZWZ0PXtzZXQ6ZnVu
Y3Rpb24oYSl7YS5lbGVtLm5vZGVUeXBlJiZhLmVsZW0ucGFyZW50Tm9kZSYmKGEu
ZWxlbVthLnByb3BdPWEubm93KX19LG4uZWFzaW5nPXtsaW5lYXI6ZnVuY3Rpb24o
YSl7cmV0dXJuIGF9LHN3aW5nOmZ1bmN0aW9uKGEpe3JldHVybi41LU1hdGguY29z
KGEqTWF0aC5QSSkvMn19LG4uZng9S2IucHJvdG90eXBlLmluaXQsbi5meC5zdGVw
PXt9O3ZhciBMYixNYixOYj0vXig/OnRvZ2dsZXxzaG93fGhpZGUpJC8sT2I9bmV3
IFJlZ0V4cCgiXig/OihbKy1dKT18KSgiK1ErIikoW2EteiVdKikkIiwiaSIpLFBi
PS9xdWV1ZUhvb2tzJC8sUWI9W1ZiXSxSYj17IioiOltmdW5jdGlvbihhLGIpe3Zh
ciBjPXRoaXMuY3JlYXRlVHdlZW4oYSxiKSxkPWMuY3VyKCksZT1PYi5leGVjKGIp
LGY9ZSYmZVszXXx8KG4uY3NzTnVtYmVyW2FdPyIiOiJweCIpLGc9KG4uY3NzTnVt
YmVyW2FdfHwicHgiIT09ZiYmK2QpJiZPYi5leGVjKG4uY3NzKGMuZWxlbSxhKSks
aD0xLGk9MjA7aWYoZyYmZ1szXSE9PWYpe2Y9Znx8Z1szXSxlPWV8fFtdLGc9K2R8
fDE7ZG8gaD1ofHwiLjUiLGcvPWgsbi5zdHlsZShjLmVsZW0sYSxnK2YpO3doaWxl
KGghPT0oaD1jLmN1cigpL2QpJiYxIT09aCYmLS1pKX1yZXR1cm4gZSYmKGc9Yy5z
dGFydD0rZ3x8K2R8fDAsYy51bml0PWYsYy5lbmQ9ZVsxXT9nKyhlWzFdKzEpKmVb
Ml06K2VbMl0pLGN9XX07ZnVuY3Rpb24gU2IoKXtyZXR1cm4gc2V0VGltZW91dChm
dW5jdGlvbigpe0xiPXZvaWQgMH0pLExiPW4ubm93KCl9ZnVuY3Rpb24gVGIoYSxi
KXt2YXIgYyxkPTAsZT17aGVpZ2h0OmF9O2ZvcihiPWI/MTowOzQ+ZDtkKz0yLWIp
Yz1SW2RdLGVbIm1hcmdpbiIrY109ZVsicGFkZGluZyIrY109YTtyZXR1cm4gYiYm
KGUub3BhY2l0eT1lLndpZHRoPWEpLGV9ZnVuY3Rpb24gVWIoYSxiLGMpe2Zvcih2
YXIgZCxlPShSYltiXXx8W10pLmNvbmNhdChSYlsiKiJdKSxmPTAsZz1lLmxlbmd0
aDtnPmY7ZisrKWlmKGQ9ZVtmXS5jYWxsKGMsYixhKSlyZXR1cm4gZH1mdW5jdGlv
biBWYihhLGIsYyl7dmFyIGQsZSxmLGcsaCxpLGosayxsPXRoaXMsbT17fSxvPWEu
c3R5bGUscD1hLm5vZGVUeXBlJiZTKGEpLHE9TC5nZXQoYSwiZnhzaG93Iik7Yy5x
dWV1ZXx8KGg9bi5fcXVldWVIb29rcyhhLCJmeCIpLG51bGw9PWgudW5xdWV1ZWQm
JihoLnVucXVldWVkPTAsaT1oLmVtcHR5LmZpcmUsaC5lbXB0eS5maXJlPWZ1bmN0
aW9uKCl7aC51bnF1ZXVlZHx8aSgpfSksaC51bnF1ZXVlZCsrLGwuYWx3YXlzKGZ1
bmN0aW9uKCl7bC5hbHdheXMoZnVuY3Rpb24oKXtoLnVucXVldWVkLS0sbi5xdWV1
ZShhLCJmeCIpLmxlbmd0aHx8aC5lbXB0eS5maXJlKCl9KX0pKSwxPT09YS5ub2Rl
VHlwZSYmKCJoZWlnaHQiaW4gYnx8IndpZHRoImluIGIpJiYoYy5vdmVyZmxvdz1b
by5vdmVyZmxvdyxvLm92ZXJmbG93WCxvLm92ZXJmbG93WV0saj1uLmNzcyhhLCJk
aXNwbGF5Iiksaz0ibm9uZSI9PT1qP0wuZ2V0KGEsIm9sZGRpc3BsYXkiKXx8dGIo
YS5ub2RlTmFtZSk6aiwiaW5saW5lIj09PWsmJiJub25lIj09PW4uY3NzKGEsImZs
b2F0IikmJihvLmRpc3BsYXk9ImlubGluZS1ibG9jayIpKSxjLm92ZXJmbG93JiYo
by5vdmVyZmxvdz0iaGlkZGVuIixsLmFsd2F5cyhmdW5jdGlvbigpe28ub3ZlcmZs
b3c9Yy5vdmVyZmxvd1swXSxvLm92ZXJmbG93WD1jLm92ZXJmbG93WzFdLG8ub3Zl
cmZsb3dZPWMub3ZlcmZsb3dbMl19KSk7Zm9yKGQgaW4gYilpZihlPWJbZF0sTmIu
ZXhlYyhlKSl7aWYoZGVsZXRlIGJbZF0sZj1mfHwidG9nZ2xlIj09PWUsZT09PShw
PyJoaWRlIjoic2hvdyIpKXtpZigic2hvdyIhPT1lfHwhcXx8dm9pZCAwPT09cVtk
XSljb250aW51ZTtwPSEwfW1bZF09cSYmcVtkXXx8bi5zdHlsZShhLGQpfWVsc2Ug
aj12b2lkIDA7aWYobi5pc0VtcHR5T2JqZWN0KG0pKSJpbmxpbmUiPT09KCJub25l
Ij09PWo/dGIoYS5ub2RlTmFtZSk6aikmJihvLmRpc3BsYXk9aik7ZWxzZXtxPyJo
aWRkZW4iaW4gcSYmKHA9cS5oaWRkZW4pOnE9TC5hY2Nlc3MoYSwiZnhzaG93Iix7
fSksZiYmKHEuaGlkZGVuPSFwKSxwP24oYSkuc2hvdygpOmwuZG9uZShmdW5jdGlv
bigpe24oYSkuaGlkZSgpfSksbC5kb25lKGZ1bmN0aW9uKCl7dmFyIGI7TC5yZW1v
dmUoYSwiZnhzaG93Iik7Zm9yKGIgaW4gbSluLnN0eWxlKGEsYixtW2JdKX0pO2Zv
cihkIGluIG0pZz1VYihwP3FbZF06MCxkLGwpLGQgaW4gcXx8KHFbZF09Zy5zdGFy
dCxwJiYoZy5lbmQ9Zy5zdGFydCxnLnN0YXJ0PSJ3aWR0aCI9PT1kfHwiaGVpZ2h0
Ij09PWQ/MTowKSl9fWZ1bmN0aW9uIFdiKGEsYil7dmFyIGMsZCxlLGYsZztmb3Io
YyBpbiBhKWlmKGQ9bi5jYW1lbENhc2UoYyksZT1iW2RdLGY9YVtjXSxuLmlzQXJy
YXkoZikmJihlPWZbMV0sZj1hW2NdPWZbMF0pLGMhPT1kJiYoYVtkXT1mLGRlbGV0
ZSBhW2NdKSxnPW4uY3NzSG9va3NbZF0sZyYmImV4cGFuZCJpbiBnKXtmPWcuZXhw
YW5kKGYpLGRlbGV0ZSBhW2RdO2ZvcihjIGluIGYpYyBpbiBhfHwoYVtjXT1mW2Nd
LGJbY109ZSl9ZWxzZSBiW2RdPWV9ZnVuY3Rpb24gWGIoYSxiLGMpe3ZhciBkLGUs
Zj0wLGc9UWIubGVuZ3RoLGg9bi5EZWZlcnJlZCgpLmFsd2F5cyhmdW5jdGlvbigp
e2RlbGV0ZSBpLmVsZW19KSxpPWZ1bmN0aW9uKCl7aWYoZSlyZXR1cm4hMTtmb3Io
dmFyIGI9TGJ8fFNiKCksYz1NYXRoLm1heCgwLGouc3RhcnRUaW1lK2ouZHVyYXRp
b24tYiksZD1jL2ouZHVyYXRpb258fDAsZj0xLWQsZz0wLGk9ai50d2VlbnMubGVu
Z3RoO2k+ZztnKyspai50d2VlbnNbZ10ucnVuKGYpO3JldHVybiBoLm5vdGlmeVdp
dGgoYSxbaixmLGNdKSwxPmYmJmk/YzooaC5yZXNvbHZlV2l0aChhLFtqXSksITEp
fSxqPWgucHJvbWlzZSh7ZWxlbTphLHByb3BzOm4uZXh0ZW5kKHt9LGIpLG9wdHM6
bi5leHRlbmQoITAse3NwZWNpYWxFYXNpbmc6e319LGMpLG9yaWdpbmFsUHJvcGVy
dGllczpiLG9yaWdpbmFsT3B0aW9uczpjLHN0YXJ0VGltZTpMYnx8U2IoKSxkdXJh
dGlvbjpjLmR1cmF0aW9uLHR3ZWVuczpbXSxjcmVhdGVUd2VlbjpmdW5jdGlvbihi
LGMpe3ZhciBkPW4uVHdlZW4oYSxqLm9wdHMsYixjLGoub3B0cy5zcGVjaWFsRWFz
aW5nW2JdfHxqLm9wdHMuZWFzaW5nKTtyZXR1cm4gai50d2VlbnMucHVzaChkKSxk
fSxzdG9wOmZ1bmN0aW9uKGIpe3ZhciBjPTAsZD1iP2oudHdlZW5zLmxlbmd0aDow
O2lmKGUpcmV0dXJuIHRoaXM7Zm9yKGU9ITA7ZD5jO2MrKylqLnR3ZWVuc1tjXS5y
dW4oMSk7cmV0dXJuIGI/aC5yZXNvbHZlV2l0aChhLFtqLGJdKTpoLnJlamVjdFdp
dGgoYSxbaixiXSksdGhpc319KSxrPWoucHJvcHM7Zm9yKFdiKGssai5vcHRzLnNw
ZWNpYWxFYXNpbmcpO2c+ZjtmKyspaWYoZD1RYltmXS5jYWxsKGosYSxrLGoub3B0
cykpcmV0dXJuIGQ7cmV0dXJuIG4ubWFwKGssVWIsaiksbi5pc0Z1bmN0aW9uKGou
b3B0cy5zdGFydCkmJmoub3B0cy5zdGFydC5jYWxsKGEsaiksbi5meC50aW1lcihu
LmV4dGVuZChpLHtlbGVtOmEsYW5pbTpqLHF1ZXVlOmoub3B0cy5xdWV1ZX0pKSxq
LnByb2dyZXNzKGoub3B0cy5wcm9ncmVzcykuZG9uZShqLm9wdHMuZG9uZSxqLm9w
dHMuY29tcGxldGUpLmZhaWwoai5vcHRzLmZhaWwpLmFsd2F5cyhqLm9wdHMuYWx3
YXlzKX1uLkFuaW1hdGlvbj1uLmV4dGVuZChYYix7dHdlZW5lcjpmdW5jdGlvbihh
LGIpe24uaXNGdW5jdGlvbihhKT8oYj1hLGE9WyIqIl0pOmE9YS5zcGxpdCgiICIp
O2Zvcih2YXIgYyxkPTAsZT1hLmxlbmd0aDtlPmQ7ZCsrKWM9YVtkXSxSYltjXT1S
YltjXXx8W10sUmJbY10udW5zaGlmdChiKX0scHJlZmlsdGVyOmZ1bmN0aW9uKGEs
Yil7Yj9RYi51bnNoaWZ0KGEpOlFiLnB1c2goYSl9fSksbi5zcGVlZD1mdW5jdGlv
bihhLGIsYyl7dmFyIGQ9YSYmIm9iamVjdCI9PXR5cGVvZiBhP24uZXh0ZW5kKHt9
LGEpOntjb21wbGV0ZTpjfHwhYyYmYnx8bi5pc0Z1bmN0aW9uKGEpJiZhLGR1cmF0
aW9uOmEsZWFzaW5nOmMmJmJ8fGImJiFuLmlzRnVuY3Rpb24oYikmJmJ9O3JldHVy
biBkLmR1cmF0aW9uPW4uZngub2ZmPzA6Im51bWJlciI9PXR5cGVvZiBkLmR1cmF0
aW9uP2QuZHVyYXRpb246ZC5kdXJhdGlvbiBpbiBuLmZ4LnNwZWVkcz9uLmZ4LnNw
ZWVkc1tkLmR1cmF0aW9uXTpuLmZ4LnNwZWVkcy5fZGVmYXVsdCwobnVsbD09ZC5x
dWV1ZXx8ZC5xdWV1ZT09PSEwKSYmKGQucXVldWU9ImZ4IiksZC5vbGQ9ZC5jb21w
bGV0ZSxkLmNvbXBsZXRlPWZ1bmN0aW9uKCl7bi5pc0Z1bmN0aW9uKGQub2xkKSYm
ZC5vbGQuY2FsbCh0aGlzKSxkLnF1ZXVlJiZuLmRlcXVldWUodGhpcyxkLnF1ZXVl
KX0sZH0sbi5mbi5leHRlbmQoe2ZhZGVUbzpmdW5jdGlvbihhLGIsYyxkKXtyZXR1
cm4gdGhpcy5maWx0ZXIoUykuY3NzKCJvcGFjaXR5IiwwKS5zaG93KCkuZW5kKCku
YW5pbWF0ZSh7b3BhY2l0eTpifSxhLGMsZCl9LGFuaW1hdGU6ZnVuY3Rpb24oYSxi
LGMsZCl7dmFyIGU9bi5pc0VtcHR5T2JqZWN0KGEpLGY9bi5zcGVlZChiLGMsZCks
Zz1mdW5jdGlvbigpe3ZhciBiPVhiKHRoaXMsbi5leHRlbmQoe30sYSksZik7KGV8
fEwuZ2V0KHRoaXMsImZpbmlzaCIpKSYmYi5zdG9wKCEwKX07cmV0dXJuIGcuZmlu
aXNoPWcsZXx8Zi5xdWV1ZT09PSExP3RoaXMuZWFjaChnKTp0aGlzLnF1ZXVlKGYu
cXVldWUsZyl9LHN0b3A6ZnVuY3Rpb24oYSxiLGMpe3ZhciBkPWZ1bmN0aW9uKGEp
e3ZhciBiPWEuc3RvcDtkZWxldGUgYS5zdG9wLGIoYyl9O3JldHVybiJzdHJpbmci
IT10eXBlb2YgYSYmKGM9YixiPWEsYT12b2lkIDApLGImJmEhPT0hMSYmdGhpcy5x
dWV1ZShhfHwiZngiLFtdKSx0aGlzLmVhY2goZnVuY3Rpb24oKXt2YXIgYj0hMCxl
PW51bGwhPWEmJmErInF1ZXVlSG9va3MiLGY9bi50aW1lcnMsZz1MLmdldCh0aGlz
KTtpZihlKWdbZV0mJmdbZV0uc3RvcCYmZChnW2VdKTtlbHNlIGZvcihlIGluIGcp
Z1tlXSYmZ1tlXS5zdG9wJiZQYi50ZXN0KGUpJiZkKGdbZV0pO2ZvcihlPWYubGVu
Z3RoO2UtLTspZltlXS5lbGVtIT09dGhpc3x8bnVsbCE9YSYmZltlXS5xdWV1ZSE9
PWF8fChmW2VdLmFuaW0uc3RvcChjKSxiPSExLGYuc3BsaWNlKGUsMSkpOyhifHwh
YykmJm4uZGVxdWV1ZSh0aGlzLGEpfSl9LGZpbmlzaDpmdW5jdGlvbihhKXtyZXR1
cm4gYSE9PSExJiYoYT1hfHwiZngiKSx0aGlzLmVhY2goZnVuY3Rpb24oKXt2YXIg
YixjPUwuZ2V0KHRoaXMpLGQ9Y1thKyJxdWV1ZSJdLGU9Y1thKyJxdWV1ZUhvb2tz
Il0sZj1uLnRpbWVycyxnPWQ/ZC5sZW5ndGg6MDtmb3IoYy5maW5pc2g9ITAsbi5x
dWV1ZSh0aGlzLGEsW10pLGUmJmUuc3RvcCYmZS5zdG9wLmNhbGwodGhpcywhMCks
Yj1mLmxlbmd0aDtiLS07KWZbYl0uZWxlbT09PXRoaXMmJmZbYl0ucXVldWU9PT1h
JiYoZltiXS5hbmltLnN0b3AoITApLGYuc3BsaWNlKGIsMSkpO2ZvcihiPTA7Zz5i
O2IrKylkW2JdJiZkW2JdLmZpbmlzaCYmZFtiXS5maW5pc2guY2FsbCh0aGlzKTtk
ZWxldGUgYy5maW5pc2h9KX19KSxuLmVhY2goWyJ0b2dnbGUiLCJzaG93IiwiaGlk
ZSJdLGZ1bmN0aW9uKGEsYil7dmFyIGM9bi5mbltiXTtuLmZuW2JdPWZ1bmN0aW9u
KGEsZCxlKXtyZXR1cm4gbnVsbD09YXx8ImJvb2xlYW4iPT10eXBlb2YgYT9jLmFw
cGx5KHRoaXMsYXJndW1lbnRzKTp0aGlzLmFuaW1hdGUoVGIoYiwhMCksYSxkLGUp
fX0pLG4uZWFjaCh7c2xpZGVEb3duOlRiKCJzaG93Iiksc2xpZGVVcDpUYigiaGlk
ZSIpLHNsaWRlVG9nZ2xlOlRiKCJ0b2dnbGUiKSxmYWRlSW46e29wYWNpdHk6InNo
b3cifSxmYWRlT3V0OntvcGFjaXR5OiJoaWRlIn0sZmFkZVRvZ2dsZTp7b3BhY2l0
eToidG9nZ2xlIn19LGZ1bmN0aW9uKGEsYil7bi5mblthXT1mdW5jdGlvbihhLGMs
ZCl7cmV0dXJuIHRoaXMuYW5pbWF0ZShiLGEsYyxkKX19KSxuLnRpbWVycz1bXSxu
LmZ4LnRpY2s9ZnVuY3Rpb24oKXt2YXIgYSxiPTAsYz1uLnRpbWVycztmb3IoTGI9
bi5ub3coKTtiPGMubGVuZ3RoO2IrKylhPWNbYl0sYSgpfHxjW2JdIT09YXx8Yy5z
cGxpY2UoYi0tLDEpO2MubGVuZ3RofHxuLmZ4LnN0b3AoKSxMYj12b2lkIDB9LG4u
ZngudGltZXI9ZnVuY3Rpb24oYSl7bi50aW1lcnMucHVzaChhKSxhKCk/bi5meC5z
dGFydCgpOm4udGltZXJzLnBvcCgpfSxuLmZ4LmludGVydmFsPTEzLG4uZnguc3Rh
cnQ9ZnVuY3Rpb24oKXtNYnx8KE1iPXNldEludGVydmFsKG4uZngudGljayxuLmZ4
LmludGVydmFsKSl9LG4uZnguc3RvcD1mdW5jdGlvbigpe2NsZWFySW50ZXJ2YWwo
TWIpLE1iPW51bGx9LG4uZnguc3BlZWRzPXtzbG93OjYwMCxmYXN0OjIwMCxfZGVm
YXVsdDo0MDB9LG4uZm4uZGVsYXk9ZnVuY3Rpb24oYSxiKXtyZXR1cm4gYT1uLmZ4
P24uZnguc3BlZWRzW2FdfHxhOmEsYj1ifHwiZngiLHRoaXMucXVldWUoYixmdW5j
dGlvbihiLGMpe3ZhciBkPXNldFRpbWVvdXQoYixhKTtjLnN0b3A9ZnVuY3Rpb24o
KXtjbGVhclRpbWVvdXQoZCl9fSl9LGZ1bmN0aW9uKCl7dmFyIGE9bC5jcmVhdGVF
bGVtZW50KCJpbnB1dCIpLGI9bC5jcmVhdGVFbGVtZW50KCJzZWxlY3QiKSxjPWIu
YXBwZW5kQ2hpbGQobC5jcmVhdGVFbGVtZW50KCJvcHRpb24iKSk7YS50eXBlPSJj
aGVja2JveCIsay5jaGVja09uPSIiIT09YS52YWx1ZSxrLm9wdFNlbGVjdGVkPWMu
c2VsZWN0ZWQsYi5kaXNhYmxlZD0hMCxrLm9wdERpc2FibGVkPSFjLmRpc2FibGVk
LGE9bC5jcmVhdGVFbGVtZW50KCJpbnB1dCIpLGEudmFsdWU9InQiLGEudHlwZT0i
cmFkaW8iLGsucmFkaW9WYWx1ZT0idCI9PT1hLnZhbHVlfSgpO3ZhciBZYixaYiwk
Yj1uLmV4cHIuYXR0ckhhbmRsZTtuLmZuLmV4dGVuZCh7YXR0cjpmdW5jdGlvbihh
LGIpe3JldHVybiBKKHRoaXMsbi5hdHRyLGEsYixhcmd1bWVudHMubGVuZ3RoPjEp
fSxyZW1vdmVBdHRyOmZ1bmN0aW9uKGEpe3JldHVybiB0aGlzLmVhY2goZnVuY3Rp
b24oKXtuLnJlbW92ZUF0dHIodGhpcyxhKX0pfX0pLG4uZXh0ZW5kKHthdHRyOmZ1
bmN0aW9uKGEsYixjKXt2YXIgZCxlLGY9YS5ub2RlVHlwZTtpZihhJiYzIT09ZiYm
OCE9PWYmJjIhPT1mKXJldHVybiB0eXBlb2YgYS5nZXRBdHRyaWJ1dGU9PT1VP24u
cHJvcChhLGIsYyk6KDE9PT1mJiZuLmlzWE1MRG9jKGEpfHwoYj1iLnRvTG93ZXJD
YXNlKCksZD1uLmF0dHJIb29rc1tiXXx8KG4uZXhwci5tYXRjaC5ib29sLnRlc3Qo
Yik/WmI6WWIpKSx2b2lkIDA9PT1jP2QmJiJnZXQiaW4gZCYmbnVsbCE9PShlPWQu
Z2V0KGEsYikpP2U6KGU9bi5maW5kLmF0dHIoYSxiKSxudWxsPT1lP3ZvaWQgMDpl
KTpudWxsIT09Yz9kJiYic2V0ImluIGQmJnZvaWQgMCE9PShlPWQuc2V0KGEsYyxi
KSk/ZTooYS5zZXRBdHRyaWJ1dGUoYixjKyIiKSxjKTp2b2lkIG4ucmVtb3ZlQXR0
cihhLGIpKQp9LHJlbW92ZUF0dHI6ZnVuY3Rpb24oYSxiKXt2YXIgYyxkLGU9MCxm
PWImJmIubWF0Y2goRSk7aWYoZiYmMT09PWEubm9kZVR5cGUpd2hpbGUoYz1mW2Ur
K10pZD1uLnByb3BGaXhbY118fGMsbi5leHByLm1hdGNoLmJvb2wudGVzdChjKSYm
KGFbZF09ITEpLGEucmVtb3ZlQXR0cmlidXRlKGMpfSxhdHRySG9va3M6e3R5cGU6
e3NldDpmdW5jdGlvbihhLGIpe2lmKCFrLnJhZGlvVmFsdWUmJiJyYWRpbyI9PT1i
JiZuLm5vZGVOYW1lKGEsImlucHV0Iikpe3ZhciBjPWEudmFsdWU7cmV0dXJuIGEu
c2V0QXR0cmlidXRlKCJ0eXBlIixiKSxjJiYoYS52YWx1ZT1jKSxifX19fX0pLFpi
PXtzZXQ6ZnVuY3Rpb24oYSxiLGMpe3JldHVybiBiPT09ITE/bi5yZW1vdmVBdHRy
KGEsYyk6YS5zZXRBdHRyaWJ1dGUoYyxjKSxjfX0sbi5lYWNoKG4uZXhwci5tYXRj
aC5ib29sLnNvdXJjZS5tYXRjaCgvXHcrL2cpLGZ1bmN0aW9uKGEsYil7dmFyIGM9
JGJbYl18fG4uZmluZC5hdHRyOyRiW2JdPWZ1bmN0aW9uKGEsYixkKXt2YXIgZSxm
O3JldHVybiBkfHwoZj0kYltiXSwkYltiXT1lLGU9bnVsbCE9YyhhLGIsZCk/Yi50
b0xvd2VyQ2FzZSgpOm51bGwsJGJbYl09ZiksZX19KTt2YXIgX2I9L14oPzppbnB1
dHxzZWxlY3R8dGV4dGFyZWF8YnV0dG9uKSQvaTtuLmZuLmV4dGVuZCh7cHJvcDpm
dW5jdGlvbihhLGIpe3JldHVybiBKKHRoaXMsbi5wcm9wLGEsYixhcmd1bWVudHMu
bGVuZ3RoPjEpfSxyZW1vdmVQcm9wOmZ1bmN0aW9uKGEpe3JldHVybiB0aGlzLmVh
Y2goZnVuY3Rpb24oKXtkZWxldGUgdGhpc1tuLnByb3BGaXhbYV18fGFdfSl9fSks
bi5leHRlbmQoe3Byb3BGaXg6eyJmb3IiOiJodG1sRm9yIiwiY2xhc3MiOiJjbGFz
c05hbWUifSxwcm9wOmZ1bmN0aW9uKGEsYixjKXt2YXIgZCxlLGYsZz1hLm5vZGVU
eXBlO2lmKGEmJjMhPT1nJiY4IT09ZyYmMiE9PWcpcmV0dXJuIGY9MSE9PWd8fCFu
LmlzWE1MRG9jKGEpLGYmJihiPW4ucHJvcEZpeFtiXXx8YixlPW4ucHJvcEhvb2tz
W2JdKSx2b2lkIDAhPT1jP2UmJiJzZXQiaW4gZSYmdm9pZCAwIT09KGQ9ZS5zZXQo
YSxjLGIpKT9kOmFbYl09YzplJiYiZ2V0ImluIGUmJm51bGwhPT0oZD1lLmdldChh
LGIpKT9kOmFbYl19LHByb3BIb29rczp7dGFiSW5kZXg6e2dldDpmdW5jdGlvbihh
KXtyZXR1cm4gYS5oYXNBdHRyaWJ1dGUoInRhYmluZGV4Iil8fF9iLnRlc3QoYS5u
b2RlTmFtZSl8fGEuaHJlZj9hLnRhYkluZGV4Oi0xfX19fSksay5vcHRTZWxlY3Rl
ZHx8KG4ucHJvcEhvb2tzLnNlbGVjdGVkPXtnZXQ6ZnVuY3Rpb24oYSl7dmFyIGI9
YS5wYXJlbnROb2RlO3JldHVybiBiJiZiLnBhcmVudE5vZGUmJmIucGFyZW50Tm9k
ZS5zZWxlY3RlZEluZGV4LG51bGx9fSksbi5lYWNoKFsidGFiSW5kZXgiLCJyZWFk
T25seSIsIm1heExlbmd0aCIsImNlbGxTcGFjaW5nIiwiY2VsbFBhZGRpbmciLCJy
b3dTcGFuIiwiY29sU3BhbiIsInVzZU1hcCIsImZyYW1lQm9yZGVyIiwiY29udGVu
dEVkaXRhYmxlIl0sZnVuY3Rpb24oKXtuLnByb3BGaXhbdGhpcy50b0xvd2VyQ2Fz
ZSgpXT10aGlzfSk7dmFyIGFjPS9bXHRcclxuXGZdL2c7bi5mbi5leHRlbmQoe2Fk
ZENsYXNzOmZ1bmN0aW9uKGEpe3ZhciBiLGMsZCxlLGYsZyxoPSJzdHJpbmciPT10
eXBlb2YgYSYmYSxpPTAsaj10aGlzLmxlbmd0aDtpZihuLmlzRnVuY3Rpb24oYSkp
cmV0dXJuIHRoaXMuZWFjaChmdW5jdGlvbihiKXtuKHRoaXMpLmFkZENsYXNzKGEu
Y2FsbCh0aGlzLGIsdGhpcy5jbGFzc05hbWUpKX0pO2lmKGgpZm9yKGI9KGF8fCIi
KS5tYXRjaChFKXx8W107aj5pO2krKylpZihjPXRoaXNbaV0sZD0xPT09Yy5ub2Rl
VHlwZSYmKGMuY2xhc3NOYW1lPygiICIrYy5jbGFzc05hbWUrIiAiKS5yZXBsYWNl
KGFjLCIgIik6IiAiKSl7Zj0wO3doaWxlKGU9YltmKytdKWQuaW5kZXhPZigiICIr
ZSsiICIpPDAmJihkKz1lKyIgIik7Zz1uLnRyaW0oZCksYy5jbGFzc05hbWUhPT1n
JiYoYy5jbGFzc05hbWU9Zyl9cmV0dXJuIHRoaXN9LHJlbW92ZUNsYXNzOmZ1bmN0
aW9uKGEpe3ZhciBiLGMsZCxlLGYsZyxoPTA9PT1hcmd1bWVudHMubGVuZ3RofHwi
c3RyaW5nIj09dHlwZW9mIGEmJmEsaT0wLGo9dGhpcy5sZW5ndGg7aWYobi5pc0Z1
bmN0aW9uKGEpKXJldHVybiB0aGlzLmVhY2goZnVuY3Rpb24oYil7bih0aGlzKS5y
ZW1vdmVDbGFzcyhhLmNhbGwodGhpcyxiLHRoaXMuY2xhc3NOYW1lKSl9KTtpZiho
KWZvcihiPShhfHwiIikubWF0Y2goRSl8fFtdO2o+aTtpKyspaWYoYz10aGlzW2ld
LGQ9MT09PWMubm9kZVR5cGUmJihjLmNsYXNzTmFtZT8oIiAiK2MuY2xhc3NOYW1l
KyIgIikucmVwbGFjZShhYywiICIpOiIiKSl7Zj0wO3doaWxlKGU9YltmKytdKXdo
aWxlKGQuaW5kZXhPZigiICIrZSsiICIpPj0wKWQ9ZC5yZXBsYWNlKCIgIitlKyIg
IiwiICIpO2c9YT9uLnRyaW0oZCk6IiIsYy5jbGFzc05hbWUhPT1nJiYoYy5jbGFz
c05hbWU9Zyl9cmV0dXJuIHRoaXN9LHRvZ2dsZUNsYXNzOmZ1bmN0aW9uKGEsYil7
dmFyIGM9dHlwZW9mIGE7cmV0dXJuImJvb2xlYW4iPT10eXBlb2YgYiYmInN0cmlu
ZyI9PT1jP2I/dGhpcy5hZGRDbGFzcyhhKTp0aGlzLnJlbW92ZUNsYXNzKGEpOnRo
aXMuZWFjaChuLmlzRnVuY3Rpb24oYSk/ZnVuY3Rpb24oYyl7bih0aGlzKS50b2dn
bGVDbGFzcyhhLmNhbGwodGhpcyxjLHRoaXMuY2xhc3NOYW1lLGIpLGIpfTpmdW5j
dGlvbigpe2lmKCJzdHJpbmciPT09Yyl7dmFyIGIsZD0wLGU9bih0aGlzKSxmPWEu
bWF0Y2goRSl8fFtdO3doaWxlKGI9ZltkKytdKWUuaGFzQ2xhc3MoYik/ZS5yZW1v
dmVDbGFzcyhiKTplLmFkZENsYXNzKGIpfWVsc2UoYz09PVV8fCJib29sZWFuIj09
PWMpJiYodGhpcy5jbGFzc05hbWUmJkwuc2V0KHRoaXMsIl9fY2xhc3NOYW1lX18i
LHRoaXMuY2xhc3NOYW1lKSx0aGlzLmNsYXNzTmFtZT10aGlzLmNsYXNzTmFtZXx8
YT09PSExPyIiOkwuZ2V0KHRoaXMsIl9fY2xhc3NOYW1lX18iKXx8IiIpfSl9LGhh
c0NsYXNzOmZ1bmN0aW9uKGEpe2Zvcih2YXIgYj0iICIrYSsiICIsYz0wLGQ9dGhp
cy5sZW5ndGg7ZD5jO2MrKylpZigxPT09dGhpc1tjXS5ub2RlVHlwZSYmKCIgIit0
aGlzW2NdLmNsYXNzTmFtZSsiICIpLnJlcGxhY2UoYWMsIiAiKS5pbmRleE9mKGIp
Pj0wKXJldHVybiEwO3JldHVybiExfX0pO3ZhciBiYz0vXHIvZztuLmZuLmV4dGVu
ZCh7dmFsOmZ1bmN0aW9uKGEpe3ZhciBiLGMsZCxlPXRoaXNbMF07e2lmKGFyZ3Vt
ZW50cy5sZW5ndGgpcmV0dXJuIGQ9bi5pc0Z1bmN0aW9uKGEpLHRoaXMuZWFjaChm
dW5jdGlvbihjKXt2YXIgZTsxPT09dGhpcy5ub2RlVHlwZSYmKGU9ZD9hLmNhbGwo
dGhpcyxjLG4odGhpcykudmFsKCkpOmEsbnVsbD09ZT9lPSIiOiJudW1iZXIiPT10
eXBlb2YgZT9lKz0iIjpuLmlzQXJyYXkoZSkmJihlPW4ubWFwKGUsZnVuY3Rpb24o
YSl7cmV0dXJuIG51bGw9PWE/IiI6YSsiIn0pKSxiPW4udmFsSG9va3NbdGhpcy50
eXBlXXx8bi52YWxIb29rc1t0aGlzLm5vZGVOYW1lLnRvTG93ZXJDYXNlKCldLGIm
JiJzZXQiaW4gYiYmdm9pZCAwIT09Yi5zZXQodGhpcyxlLCJ2YWx1ZSIpfHwodGhp
cy52YWx1ZT1lKSl9KTtpZihlKXJldHVybiBiPW4udmFsSG9va3NbZS50eXBlXXx8
bi52YWxIb29rc1tlLm5vZGVOYW1lLnRvTG93ZXJDYXNlKCldLGImJiJnZXQiaW4g
YiYmdm9pZCAwIT09KGM9Yi5nZXQoZSwidmFsdWUiKSk/YzooYz1lLnZhbHVlLCJz
dHJpbmciPT10eXBlb2YgYz9jLnJlcGxhY2UoYmMsIiIpOm51bGw9PWM/IiI6Yyl9
fX0pLG4uZXh0ZW5kKHt2YWxIb29rczp7b3B0aW9uOntnZXQ6ZnVuY3Rpb24oYSl7
dmFyIGI9bi5maW5kLmF0dHIoYSwidmFsdWUiKTtyZXR1cm4gbnVsbCE9Yj9iOm4u
dHJpbShuLnRleHQoYSkpfX0sc2VsZWN0OntnZXQ6ZnVuY3Rpb24oYSl7Zm9yKHZh
ciBiLGMsZD1hLm9wdGlvbnMsZT1hLnNlbGVjdGVkSW5kZXgsZj0ic2VsZWN0LW9u
ZSI9PT1hLnR5cGV8fDA+ZSxnPWY/bnVsbDpbXSxoPWY/ZSsxOmQubGVuZ3RoLGk9
MD5lP2g6Zj9lOjA7aD5pO2krKylpZihjPWRbaV0sISghYy5zZWxlY3RlZCYmaSE9
PWV8fChrLm9wdERpc2FibGVkP2MuZGlzYWJsZWQ6bnVsbCE9PWMuZ2V0QXR0cmli
dXRlKCJkaXNhYmxlZCIpKXx8Yy5wYXJlbnROb2RlLmRpc2FibGVkJiZuLm5vZGVO
YW1lKGMucGFyZW50Tm9kZSwib3B0Z3JvdXAiKSkpe2lmKGI9bihjKS52YWwoKSxm
KXJldHVybiBiO2cucHVzaChiKX1yZXR1cm4gZ30sc2V0OmZ1bmN0aW9uKGEsYil7
dmFyIGMsZCxlPWEub3B0aW9ucyxmPW4ubWFrZUFycmF5KGIpLGc9ZS5sZW5ndGg7
d2hpbGUoZy0tKWQ9ZVtnXSwoZC5zZWxlY3RlZD1uLmluQXJyYXkoZC52YWx1ZSxm
KT49MCkmJihjPSEwKTtyZXR1cm4gY3x8KGEuc2VsZWN0ZWRJbmRleD0tMSksZn19
fX0pLG4uZWFjaChbInJhZGlvIiwiY2hlY2tib3giXSxmdW5jdGlvbigpe24udmFs
SG9va3NbdGhpc109e3NldDpmdW5jdGlvbihhLGIpe3JldHVybiBuLmlzQXJyYXko
Yik/YS5jaGVja2VkPW4uaW5BcnJheShuKGEpLnZhbCgpLGIpPj0wOnZvaWQgMH19
LGsuY2hlY2tPbnx8KG4udmFsSG9va3NbdGhpc10uZ2V0PWZ1bmN0aW9uKGEpe3Jl
dHVybiBudWxsPT09YS5nZXRBdHRyaWJ1dGUoInZhbHVlIik/Im9uIjphLnZhbHVl
fSl9KSxuLmVhY2goImJsdXIgZm9jdXMgZm9jdXNpbiBmb2N1c291dCBsb2FkIHJl
c2l6ZSBzY3JvbGwgdW5sb2FkIGNsaWNrIGRibGNsaWNrIG1vdXNlZG93biBtb3Vz
ZXVwIG1vdXNlbW92ZSBtb3VzZW92ZXIgbW91c2VvdXQgbW91c2VlbnRlciBtb3Vz
ZWxlYXZlIGNoYW5nZSBzZWxlY3Qgc3VibWl0IGtleWRvd24ga2V5cHJlc3Mga2V5
dXAgZXJyb3IgY29udGV4dG1lbnUiLnNwbGl0KCIgIiksZnVuY3Rpb24oYSxiKXtu
LmZuW2JdPWZ1bmN0aW9uKGEsYyl7cmV0dXJuIGFyZ3VtZW50cy5sZW5ndGg+MD90
aGlzLm9uKGIsbnVsbCxhLGMpOnRoaXMudHJpZ2dlcihiKX19KSxuLmZuLmV4dGVu
ZCh7aG92ZXI6ZnVuY3Rpb24oYSxiKXtyZXR1cm4gdGhpcy5tb3VzZWVudGVyKGEp
Lm1vdXNlbGVhdmUoYnx8YSl9LGJpbmQ6ZnVuY3Rpb24oYSxiLGMpe3JldHVybiB0
aGlzLm9uKGEsbnVsbCxiLGMpfSx1bmJpbmQ6ZnVuY3Rpb24oYSxiKXtyZXR1cm4g
dGhpcy5vZmYoYSxudWxsLGIpfSxkZWxlZ2F0ZTpmdW5jdGlvbihhLGIsYyxkKXty
ZXR1cm4gdGhpcy5vbihiLGEsYyxkKX0sdW5kZWxlZ2F0ZTpmdW5jdGlvbihhLGIs
Yyl7cmV0dXJuIDE9PT1hcmd1bWVudHMubGVuZ3RoP3RoaXMub2ZmKGEsIioqIik6
dGhpcy5vZmYoYixhfHwiKioiLGMpfX0pO3ZhciBjYz1uLm5vdygpLGRjPS9cPy87
bi5wYXJzZUpTT049ZnVuY3Rpb24oYSl7cmV0dXJuIEpTT04ucGFyc2UoYSsiIil9
LG4ucGFyc2VYTUw9ZnVuY3Rpb24oYSl7dmFyIGIsYztpZighYXx8InN0cmluZyIh
PXR5cGVvZiBhKXJldHVybiBudWxsO3RyeXtjPW5ldyBET01QYXJzZXIsYj1jLnBh
cnNlRnJvbVN0cmluZyhhLCJ0ZXh0L3htbCIpfWNhdGNoKGQpe2I9dm9pZCAwfXJl
dHVybighYnx8Yi5nZXRFbGVtZW50c0J5VGFnTmFtZSgicGFyc2VyZXJyb3IiKS5s
ZW5ndGgpJiZuLmVycm9yKCJJbnZhbGlkIFhNTDogIithKSxifTt2YXIgZWMsZmMs
Z2M9LyMuKiQvLGhjPS8oWz8mXSlfPVteJl0qLyxpYz0vXiguKj8pOlsgXHRdKihb
XlxyXG5dKikkL2dtLGpjPS9eKD86YWJvdXR8YXBwfGFwcC1zdG9yYWdlfC4rLWV4
dGVuc2lvbnxmaWxlfHJlc3x3aWRnZXQpOiQvLGtjPS9eKD86R0VUfEhFQUQpJC8s
bGM9L15cL1wvLyxtYz0vXihbXHcuKy1dKzopKD86XC9cLyg/OlteXC8/I10qQHwp
KFteXC8/IzpdKikoPzo6KFxkKyl8KXwpLyxuYz17fSxvYz17fSxwYz0iKi8iLmNv
bmNhdCgiKiIpO3RyeXtmYz1sb2NhdGlvbi5ocmVmfWNhdGNoKHFjKXtmYz1sLmNy
ZWF0ZUVsZW1lbnQoImEiKSxmYy5ocmVmPSIiLGZjPWZjLmhyZWZ9ZWM9bWMuZXhl
YyhmYy50b0xvd2VyQ2FzZSgpKXx8W107ZnVuY3Rpb24gcmMoYSl7cmV0dXJuIGZ1
bmN0aW9uKGIsYyl7InN0cmluZyIhPXR5cGVvZiBiJiYoYz1iLGI9IioiKTt2YXIg
ZCxlPTAsZj1iLnRvTG93ZXJDYXNlKCkubWF0Y2goRSl8fFtdO2lmKG4uaXNGdW5j
dGlvbihjKSl3aGlsZShkPWZbZSsrXSkiKyI9PT1kWzBdPyhkPWQuc2xpY2UoMSl8
fCIqIiwoYVtkXT1hW2RdfHxbXSkudW5zaGlmdChjKSk6KGFbZF09YVtkXXx8W10p
LnB1c2goYyl9fWZ1bmN0aW9uIHNjKGEsYixjLGQpe3ZhciBlPXt9LGY9YT09PW9j
O2Z1bmN0aW9uIGcoaCl7dmFyIGk7cmV0dXJuIGVbaF09ITAsbi5lYWNoKGFbaF18
fFtdLGZ1bmN0aW9uKGEsaCl7dmFyIGo9aChiLGMsZCk7cmV0dXJuInN0cmluZyIh
PXR5cGVvZiBqfHxmfHxlW2pdP2Y/IShpPWopOnZvaWQgMDooYi5kYXRhVHlwZXMu
dW5zaGlmdChqKSxnKGopLCExKX0pLGl9cmV0dXJuIGcoYi5kYXRhVHlwZXNbMF0p
fHwhZVsiKiJdJiZnKCIqIil9ZnVuY3Rpb24gdGMoYSxiKXt2YXIgYyxkLGU9bi5h
amF4U2V0dGluZ3MuZmxhdE9wdGlvbnN8fHt9O2ZvcihjIGluIGIpdm9pZCAwIT09
YltjXSYmKChlW2NdP2E6ZHx8KGQ9e30pKVtjXT1iW2NdKTtyZXR1cm4gZCYmbi5l
eHRlbmQoITAsYSxkKSxhfWZ1bmN0aW9uIHVjKGEsYixjKXt2YXIgZCxlLGYsZyxo
PWEuY29udGVudHMsaT1hLmRhdGFUeXBlczt3aGlsZSgiKiI9PT1pWzBdKWkuc2hp
ZnQoKSx2b2lkIDA9PT1kJiYoZD1hLm1pbWVUeXBlfHxiLmdldFJlc3BvbnNlSGVh
ZGVyKCJDb250ZW50LVR5cGUiKSk7aWYoZClmb3IoZSBpbiBoKWlmKGhbZV0mJmhb
ZV0udGVzdChkKSl7aS51bnNoaWZ0KGUpO2JyZWFrfWlmKGlbMF1pbiBjKWY9aVsw
XTtlbHNle2ZvcihlIGluIGMpe2lmKCFpWzBdfHxhLmNvbnZlcnRlcnNbZSsiICIr
aVswXV0pe2Y9ZTticmVha31nfHwoZz1lKX1mPWZ8fGd9cmV0dXJuIGY/KGYhPT1p
WzBdJiZpLnVuc2hpZnQoZiksY1tmXSk6dm9pZCAwfWZ1bmN0aW9uIHZjKGEsYixj
LGQpe3ZhciBlLGYsZyxoLGksaj17fSxrPWEuZGF0YVR5cGVzLnNsaWNlKCk7aWYo
a1sxXSlmb3IoZyBpbiBhLmNvbnZlcnRlcnMpaltnLnRvTG93ZXJDYXNlKCldPWEu
Y29udmVydGVyc1tnXTtmPWsuc2hpZnQoKTt3aGlsZShmKWlmKGEucmVzcG9uc2VG
aWVsZHNbZl0mJihjW2EucmVzcG9uc2VGaWVsZHNbZl1dPWIpLCFpJiZkJiZhLmRh
dGFGaWx0ZXImJihiPWEuZGF0YUZpbHRlcihiLGEuZGF0YVR5cGUpKSxpPWYsZj1r
LnNoaWZ0KCkpaWYoIioiPT09ZilmPWk7ZWxzZSBpZigiKiIhPT1pJiZpIT09Zil7
aWYoZz1qW2krIiAiK2ZdfHxqWyIqICIrZl0sIWcpZm9yKGUgaW4gailpZihoPWUu
c3BsaXQoIiAiKSxoWzFdPT09ZiYmKGc9altpKyIgIitoWzBdXXx8alsiKiAiK2hb
MF1dKSl7Zz09PSEwP2c9altlXTpqW2VdIT09ITAmJihmPWhbMF0say51bnNoaWZ0
KGhbMV0pKTticmVha31pZihnIT09ITApaWYoZyYmYVsidGhyb3dzIl0pYj1nKGIp
O2Vsc2UgdHJ5e2I9ZyhiKX1jYXRjaChsKXtyZXR1cm57c3RhdGU6InBhcnNlcmVy
cm9yIixlcnJvcjpnP2w6Ik5vIGNvbnZlcnNpb24gZnJvbSAiK2krIiB0byAiK2Z9
fX1yZXR1cm57c3RhdGU6InN1Y2Nlc3MiLGRhdGE6Yn19bi5leHRlbmQoe2FjdGl2
ZTowLGxhc3RNb2RpZmllZDp7fSxldGFnOnt9LGFqYXhTZXR0aW5nczp7dXJsOmZj
LHR5cGU6IkdFVCIsaXNMb2NhbDpqYy50ZXN0KGVjWzFdKSxnbG9iYWw6ITAscHJv
Y2Vzc0RhdGE6ITAsYXN5bmM6ITAsY29udGVudFR5cGU6ImFwcGxpY2F0aW9uL3gt
d3d3LWZvcm0tdXJsZW5jb2RlZDsgY2hhcnNldD1VVEYtOCIsYWNjZXB0czp7Iioi
OnBjLHRleHQ6InRleHQvcGxhaW4iLGh0bWw6InRleHQvaHRtbCIseG1sOiJhcHBs
aWNhdGlvbi94bWwsIHRleHQveG1sIixqc29uOiJhcHBsaWNhdGlvbi9qc29uLCB0
ZXh0L2phdmFzY3JpcHQifSxjb250ZW50czp7eG1sOi94bWwvLGh0bWw6L2h0bWwv
LGpzb246L2pzb24vfSxyZXNwb25zZUZpZWxkczp7eG1sOiJyZXNwb25zZVhNTCIs
dGV4dDoicmVzcG9uc2VUZXh0Iixqc29uOiJyZXNwb25zZUpTT04ifSxjb252ZXJ0
ZXJzOnsiKiB0ZXh0IjpTdHJpbmcsInRleHQgaHRtbCI6ITAsInRleHQganNvbiI6
bi5wYXJzZUpTT04sInRleHQgeG1sIjpuLnBhcnNlWE1MfSxmbGF0T3B0aW9uczp7
dXJsOiEwLGNvbnRleHQ6ITB9fSxhamF4U2V0dXA6ZnVuY3Rpb24oYSxiKXtyZXR1
cm4gYj90Yyh0YyhhLG4uYWpheFNldHRpbmdzKSxiKTp0YyhuLmFqYXhTZXR0aW5n
cyxhKX0sYWpheFByZWZpbHRlcjpyYyhuYyksYWpheFRyYW5zcG9ydDpyYyhvYyks
YWpheDpmdW5jdGlvbihhLGIpeyJvYmplY3QiPT10eXBlb2YgYSYmKGI9YSxhPXZv
aWQgMCksYj1ifHx7fTt2YXIgYyxkLGUsZixnLGgsaSxqLGs9bi5hamF4U2V0dXAo
e30sYiksbD1rLmNvbnRleHR8fGssbT1rLmNvbnRleHQmJihsLm5vZGVUeXBlfHxs
LmpxdWVyeSk/bihsKTpuLmV2ZW50LG89bi5EZWZlcnJlZCgpLHA9bi5DYWxsYmFj
a3MoIm9uY2UgbWVtb3J5IikscT1rLnN0YXR1c0NvZGV8fHt9LHI9e30scz17fSx0
PTAsdT0iY2FuY2VsZWQiLHY9e3JlYWR5U3RhdGU6MCxnZXRSZXNwb25zZUhlYWRl
cjpmdW5jdGlvbihhKXt2YXIgYjtpZigyPT09dCl7aWYoIWYpe2Y9e307d2hpbGUo
Yj1pYy5leGVjKGUpKWZbYlsxXS50b0xvd2VyQ2FzZSgpXT1iWzJdfWI9ZlthLnRv
TG93ZXJDYXNlKCldfXJldHVybiBudWxsPT1iP251bGw6Yn0sZ2V0QWxsUmVzcG9u
c2VIZWFkZXJzOmZ1bmN0aW9uKCl7cmV0dXJuIDI9PT10P2U6bnVsbH0sc2V0UmVx
dWVzdEhlYWRlcjpmdW5jdGlvbihhLGIpe3ZhciBjPWEudG9Mb3dlckNhc2UoKTty
ZXR1cm4gdHx8KGE9c1tjXT1zW2NdfHxhLHJbYV09YiksdGhpc30sb3ZlcnJpZGVN
aW1lVHlwZTpmdW5jdGlvbihhKXtyZXR1cm4gdHx8KGsubWltZVR5cGU9YSksdGhp
c30sc3RhdHVzQ29kZTpmdW5jdGlvbihhKXt2YXIgYjtpZihhKWlmKDI+dClmb3Io
YiBpbiBhKXFbYl09W3FbYl0sYVtiXV07ZWxzZSB2LmFsd2F5cyhhW3Yuc3RhdHVz
XSk7cmV0dXJuIHRoaXN9LGFib3J0OmZ1bmN0aW9uKGEpe3ZhciBiPWF8fHU7cmV0
dXJuIGMmJmMuYWJvcnQoYikseCgwLGIpLHRoaXN9fTtpZihvLnByb21pc2Uodiku
Y29tcGxldGU9cC5hZGQsdi5zdWNjZXNzPXYuZG9uZSx2LmVycm9yPXYuZmFpbCxr
LnVybD0oKGF8fGsudXJsfHxmYykrIiIpLnJlcGxhY2UoZ2MsIiIpLnJlcGxhY2Uo
bGMsZWNbMV0rIi8vIiksay50eXBlPWIubWV0aG9kfHxiLnR5cGV8fGsubWV0aG9k
fHxrLnR5cGUsay5kYXRhVHlwZXM9bi50cmltKGsuZGF0YVR5cGV8fCIqIikudG9M
b3dlckNhc2UoKS5tYXRjaChFKXx8WyIiXSxudWxsPT1rLmNyb3NzRG9tYWluJiYo
aD1tYy5leGVjKGsudXJsLnRvTG93ZXJDYXNlKCkpLGsuY3Jvc3NEb21haW49ISgh
aHx8aFsxXT09PWVjWzFdJiZoWzJdPT09ZWNbMl0mJihoWzNdfHwoImh0dHA6Ij09
PWhbMV0/IjgwIjoiNDQzIikpPT09KGVjWzNdfHwoImh0dHA6Ij09PWVjWzFdPyI4
MCI6IjQ0MyIpKSkpLGsuZGF0YSYmay5wcm9jZXNzRGF0YSYmInN0cmluZyIhPXR5
cGVvZiBrLmRhdGEmJihrLmRhdGE9bi5wYXJhbShrLmRhdGEsay50cmFkaXRpb25h
bCkpLHNjKG5jLGssYix2KSwyPT09dClyZXR1cm4gdjtpPWsuZ2xvYmFsLGkmJjA9
PT1uLmFjdGl2ZSsrJiZuLmV2ZW50LnRyaWdnZXIoImFqYXhTdGFydCIpLGsudHlw
ZT1rLnR5cGUudG9VcHBlckNhc2UoKSxrLmhhc0NvbnRlbnQ9IWtjLnRlc3Qoay50
eXBlKSxkPWsudXJsLGsuaGFzQ29udGVudHx8KGsuZGF0YSYmKGQ9ay51cmwrPShk
Yy50ZXN0KGQpPyImIjoiPyIpK2suZGF0YSxkZWxldGUgay5kYXRhKSxrLmNhY2hl
PT09ITEmJihrLnVybD1oYy50ZXN0KGQpP2QucmVwbGFjZShoYywiJDFfPSIrY2Mr
Kyk6ZCsoZGMudGVzdChkKT8iJiI6Ij8iKSsiXz0iK2NjKyspKSxrLmlmTW9kaWZp
ZWQmJihuLmxhc3RNb2RpZmllZFtkXSYmdi5zZXRSZXF1ZXN0SGVhZGVyKCJJZi1N
b2RpZmllZC1TaW5jZSIsbi5sYXN0TW9kaWZpZWRbZF0pLG4uZXRhZ1tkXSYmdi5z
ZXRSZXF1ZXN0SGVhZGVyKCJJZi1Ob25lLU1hdGNoIixuLmV0YWdbZF0pKSwoay5k
YXRhJiZrLmhhc0NvbnRlbnQmJmsuY29udGVudFR5cGUhPT0hMXx8Yi5jb250ZW50
VHlwZSkmJnYuc2V0UmVxdWVzdEhlYWRlcigiQ29udGVudC1UeXBlIixrLmNvbnRl
bnRUeXBlKSx2LnNldFJlcXVlc3RIZWFkZXIoIkFjY2VwdCIsay5kYXRhVHlwZXNb
MF0mJmsuYWNjZXB0c1trLmRhdGFUeXBlc1swXV0/ay5hY2NlcHRzW2suZGF0YVR5
cGVzWzBdXSsoIioiIT09ay5kYXRhVHlwZXNbMF0/IiwgIitwYysiOyBxPTAuMDEi
OiIiKTprLmFjY2VwdHNbIioiXSk7Zm9yKGogaW4gay5oZWFkZXJzKXYuc2V0UmVx
dWVzdEhlYWRlcihqLGsuaGVhZGVyc1tqXSk7aWYoay5iZWZvcmVTZW5kJiYoay5i
ZWZvcmVTZW5kLmNhbGwobCx2LGspPT09ITF8fDI9PT10KSlyZXR1cm4gdi5hYm9y
dCgpO3U9ImFib3J0Ijtmb3IoaiBpbntzdWNjZXNzOjEsZXJyb3I6MSxjb21wbGV0
ZToxfSl2W2pdKGtbal0pO2lmKGM9c2Mob2MsayxiLHYpKXt2LnJlYWR5U3RhdGU9
MSxpJiZtLnRyaWdnZXIoImFqYXhTZW5kIixbdixrXSksay5hc3luYyYmay50aW1l
b3V0PjAmJihnPXNldFRpbWVvdXQoZnVuY3Rpb24oKXt2LmFib3J0KCJ0aW1lb3V0
Iil9LGsudGltZW91dCkpO3RyeXt0PTEsYy5zZW5kKHIseCl9Y2F0Y2godyl7aWYo
ISgyPnQpKXRocm93IHc7eCgtMSx3KX19ZWxzZSB4KC0xLCJObyBUcmFuc3BvcnQi
KTtmdW5jdGlvbiB4KGEsYixmLGgpe3ZhciBqLHIscyx1LHcseD1iOzIhPT10JiYo
dD0yLGcmJmNsZWFyVGltZW91dChnKSxjPXZvaWQgMCxlPWh8fCIiLHYucmVhZHlT
dGF0ZT1hPjA/NDowLGo9YT49MjAwJiYzMDA+YXx8MzA0PT09YSxmJiYodT11Yyhr
LHYsZikpLHU9dmMoayx1LHYsaiksaj8oay5pZk1vZGlmaWVkJiYodz12LmdldFJl
c3BvbnNlSGVhZGVyKCJMYXN0LU1vZGlmaWVkIiksdyYmKG4ubGFzdE1vZGlmaWVk
W2RdPXcpLHc9di5nZXRSZXNwb25zZUhlYWRlcigiZXRhZyIpLHcmJihuLmV0YWdb
ZF09dykpLDIwND09PWF8fCJIRUFEIj09PWsudHlwZT94PSJub2NvbnRlbnQiOjMw
ND09PWE/eD0ibm90bW9kaWZpZWQiOih4PXUuc3RhdGUscj11LmRhdGEscz11LmVy
cm9yLGo9IXMpKToocz14LChhfHwheCkmJih4PSJlcnJvciIsMD5hJiYoYT0wKSkp
LHYuc3RhdHVzPWEsdi5zdGF0dXNUZXh0PShifHx4KSsiIixqP28ucmVzb2x2ZVdp
dGgobCxbcix4LHZdKTpvLnJlamVjdFdpdGgobCxbdix4LHNdKSx2LnN0YXR1c0Nv
ZGUocSkscT12b2lkIDAsaSYmbS50cmlnZ2VyKGo/ImFqYXhTdWNjZXNzIjoiYWph
eEVycm9yIixbdixrLGo/cjpzXSkscC5maXJlV2l0aChsLFt2LHhdKSxpJiYobS50
cmlnZ2VyKCJhamF4Q29tcGxldGUiLFt2LGtdKSwtLW4uYWN0aXZlfHxuLmV2ZW50
LnRyaWdnZXIoImFqYXhTdG9wIikpKX1yZXR1cm4gdn0sZ2V0SlNPTjpmdW5jdGlv
bihhLGIsYyl7cmV0dXJuIG4uZ2V0KGEsYixjLCJqc29uIil9LGdldFNjcmlwdDpm
dW5jdGlvbihhLGIpe3JldHVybiBuLmdldChhLHZvaWQgMCxiLCJzY3JpcHQiKX19
KSxuLmVhY2goWyJnZXQiLCJwb3N0Il0sZnVuY3Rpb24oYSxiKXtuW2JdPWZ1bmN0
aW9uKGEsYyxkLGUpe3JldHVybiBuLmlzRnVuY3Rpb24oYykmJihlPWV8fGQsZD1j
LGM9dm9pZCAwKSxuLmFqYXgoe3VybDphLHR5cGU6YixkYXRhVHlwZTplLGRhdGE6
YyxzdWNjZXNzOmR9KX19KSxuLmVhY2goWyJhamF4U3RhcnQiLCJhamF4U3RvcCIs
ImFqYXhDb21wbGV0ZSIsImFqYXhFcnJvciIsImFqYXhTdWNjZXNzIiwiYWpheFNl
bmQiXSxmdW5jdGlvbihhLGIpe24uZm5bYl09ZnVuY3Rpb24oYSl7cmV0dXJuIHRo
aXMub24oYixhKX19KSxuLl9ldmFsVXJsPWZ1bmN0aW9uKGEpe3JldHVybiBuLmFq
YXgoe3VybDphLHR5cGU6IkdFVCIsZGF0YVR5cGU6InNjcmlwdCIsYXN5bmM6ITEs
Z2xvYmFsOiExLCJ0aHJvd3MiOiEwfSl9LG4uZm4uZXh0ZW5kKHt3cmFwQWxsOmZ1
bmN0aW9uKGEpe3ZhciBiO3JldHVybiBuLmlzRnVuY3Rpb24oYSk/dGhpcy5lYWNo
KGZ1bmN0aW9uKGIpe24odGhpcykud3JhcEFsbChhLmNhbGwodGhpcyxiKSl9KToo
dGhpc1swXSYmKGI9bihhLHRoaXNbMF0ub3duZXJEb2N1bWVudCkuZXEoMCkuY2xv
bmUoITApLHRoaXNbMF0ucGFyZW50Tm9kZSYmYi5pbnNlcnRCZWZvcmUodGhpc1sw
XSksYi5tYXAoZnVuY3Rpb24oKXt2YXIgYT10aGlzO3doaWxlKGEuZmlyc3RFbGVt
ZW50Q2hpbGQpYT1hLmZpcnN0RWxlbWVudENoaWxkO3JldHVybiBhfSkuYXBwZW5k
KHRoaXMpKSx0aGlzKX0sd3JhcElubmVyOmZ1bmN0aW9uKGEpe3JldHVybiB0aGlz
LmVhY2gobi5pc0Z1bmN0aW9uKGEpP2Z1bmN0aW9uKGIpe24odGhpcykud3JhcElu
bmVyKGEuY2FsbCh0aGlzLGIpKX06ZnVuY3Rpb24oKXt2YXIgYj1uKHRoaXMpLGM9
Yi5jb250ZW50cygpO2MubGVuZ3RoP2Mud3JhcEFsbChhKTpiLmFwcGVuZChhKX0p
fSx3cmFwOmZ1bmN0aW9uKGEpe3ZhciBiPW4uaXNGdW5jdGlvbihhKTtyZXR1cm4g
dGhpcy5lYWNoKGZ1bmN0aW9uKGMpe24odGhpcykud3JhcEFsbChiP2EuY2FsbCh0
aGlzLGMpOmEpfSl9LHVud3JhcDpmdW5jdGlvbigpe3JldHVybiB0aGlzLnBhcmVu
dCgpLmVhY2goZnVuY3Rpb24oKXtuLm5vZGVOYW1lKHRoaXMsImJvZHkiKXx8bih0
aGlzKS5yZXBsYWNlV2l0aCh0aGlzLmNoaWxkTm9kZXMpfSkuZW5kKCl9fSksbi5l
eHByLmZpbHRlcnMuaGlkZGVuPWZ1bmN0aW9uKGEpe3JldHVybiBhLm9mZnNldFdp
ZHRoPD0wJiZhLm9mZnNldEhlaWdodDw9MH0sbi5leHByLmZpbHRlcnMudmlzaWJs
ZT1mdW5jdGlvbihhKXtyZXR1cm4hbi5leHByLmZpbHRlcnMuaGlkZGVuKGEpfTt2
YXIgd2M9LyUyMC9nLHhjPS9cW1xdJC8seWM9L1xyP1xuL2csemM9L14oPzpzdWJt
aXR8YnV0dG9ufGltYWdlfHJlc2V0fGZpbGUpJC9pLEFjPS9eKD86aW5wdXR8c2Vs
ZWN0fHRleHRhcmVhfGtleWdlbikvaTtmdW5jdGlvbiBCYyhhLGIsYyxkKXt2YXIg
ZTtpZihuLmlzQXJyYXkoYikpbi5lYWNoKGIsZnVuY3Rpb24oYixlKXtjfHx4Yy50
ZXN0KGEpP2QoYSxlKTpCYyhhKyJbIisoIm9iamVjdCI9PXR5cGVvZiBlP2I6IiIp
KyJdIixlLGMsZCl9KTtlbHNlIGlmKGN8fCJvYmplY3QiIT09bi50eXBlKGIpKWQo
YSxiKTtlbHNlIGZvcihlIGluIGIpQmMoYSsiWyIrZSsiXSIsYltlXSxjLGQpfW4u
cGFyYW09ZnVuY3Rpb24oYSxiKXt2YXIgYyxkPVtdLGU9ZnVuY3Rpb24oYSxiKXti
PW4uaXNGdW5jdGlvbihiKT9iKCk6bnVsbD09Yj8iIjpiLGRbZC5sZW5ndGhdPWVu
Y29kZVVSSUNvbXBvbmVudChhKSsiPSIrZW5jb2RlVVJJQ29tcG9uZW50KGIpfTtp
Zih2b2lkIDA9PT1iJiYoYj1uLmFqYXhTZXR0aW5ncyYmbi5hamF4U2V0dGluZ3Mu
dHJhZGl0aW9uYWwpLG4uaXNBcnJheShhKXx8YS5qcXVlcnkmJiFuLmlzUGxhaW5P
YmplY3QoYSkpbi5lYWNoKGEsZnVuY3Rpb24oKXtlKHRoaXMubmFtZSx0aGlzLnZh
bHVlKX0pO2Vsc2UgZm9yKGMgaW4gYSlCYyhjLGFbY10sYixlKTtyZXR1cm4gZC5q
b2luKCImIikucmVwbGFjZSh3YywiKyIpfSxuLmZuLmV4dGVuZCh7c2VyaWFsaXpl
OmZ1bmN0aW9uKCl7cmV0dXJuIG4ucGFyYW0odGhpcy5zZXJpYWxpemVBcnJheSgp
KX0sc2VyaWFsaXplQXJyYXk6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5tYXAoZnVu
Y3Rpb24oKXt2YXIgYT1uLnByb3AodGhpcywiZWxlbWVudHMiKTtyZXR1cm4gYT9u
Lm1ha2VBcnJheShhKTp0aGlzfSkuZmlsdGVyKGZ1bmN0aW9uKCl7dmFyIGE9dGhp
cy50eXBlO3JldHVybiB0aGlzLm5hbWUmJiFuKHRoaXMpLmlzKCI6ZGlzYWJsZWQi
KSYmQWMudGVzdCh0aGlzLm5vZGVOYW1lKSYmIXpjLnRlc3QoYSkmJih0aGlzLmNo
ZWNrZWR8fCFULnRlc3QoYSkpfSkubWFwKGZ1bmN0aW9uKGEsYil7dmFyIGM9bih0
aGlzKS52YWwoKTtyZXR1cm4gbnVsbD09Yz9udWxsOm4uaXNBcnJheShjKT9uLm1h
cChjLGZ1bmN0aW9uKGEpe3JldHVybntuYW1lOmIubmFtZSx2YWx1ZTphLnJlcGxh
Y2UoeWMsIlxyXG4iKX19KTp7bmFtZTpiLm5hbWUsdmFsdWU6Yy5yZXBsYWNlKHlj
LCJcclxuIil9fSkuZ2V0KCl9fSksbi5hamF4U2V0dGluZ3MueGhyPWZ1bmN0aW9u
KCl7dHJ5e3JldHVybiBuZXcgWE1MSHR0cFJlcXVlc3R9Y2F0Y2goYSl7fX07dmFy
IENjPTAsRGM9e30sRWM9ezA6MjAwLDEyMjM6MjA0fSxGYz1uLmFqYXhTZXR0aW5n
cy54aHIoKTthLkFjdGl2ZVhPYmplY3QmJm4oYSkub24oInVubG9hZCIsZnVuY3Rp
b24oKXtmb3IodmFyIGEgaW4gRGMpRGNbYV0oKX0pLGsuY29ycz0hIUZjJiYid2l0
aENyZWRlbnRpYWxzImluIEZjLGsuYWpheD1GYz0hIUZjLG4uYWpheFRyYW5zcG9y
dChmdW5jdGlvbihhKXt2YXIgYjtyZXR1cm4gay5jb3JzfHxGYyYmIWEuY3Jvc3NE
b21haW4/e3NlbmQ6ZnVuY3Rpb24oYyxkKXt2YXIgZSxmPWEueGhyKCksZz0rK0Nj
O2lmKGYub3BlbihhLnR5cGUsYS51cmwsYS5hc3luYyxhLnVzZXJuYW1lLGEucGFz
c3dvcmQpLGEueGhyRmllbGRzKWZvcihlIGluIGEueGhyRmllbGRzKWZbZV09YS54
aHJGaWVsZHNbZV07YS5taW1lVHlwZSYmZi5vdmVycmlkZU1pbWVUeXBlJiZmLm92
ZXJyaWRlTWltZVR5cGUoYS5taW1lVHlwZSksYS5jcm9zc0RvbWFpbnx8Y1siWC1S
ZXF1ZXN0ZWQtV2l0aCJdfHwoY1siWC1SZXF1ZXN0ZWQtV2l0aCJdPSJYTUxIdHRw
UmVxdWVzdCIpO2ZvcihlIGluIGMpZi5zZXRSZXF1ZXN0SGVhZGVyKGUsY1tlXSk7
Yj1mdW5jdGlvbihhKXtyZXR1cm4gZnVuY3Rpb24oKXtiJiYoZGVsZXRlIERjW2dd
LGI9Zi5vbmxvYWQ9Zi5vbmVycm9yPW51bGwsImFib3J0Ij09PWE/Zi5hYm9ydCgp
OiJlcnJvciI9PT1hP2QoZi5zdGF0dXMsZi5zdGF0dXNUZXh0KTpkKEVjW2Yuc3Rh
dHVzXXx8Zi5zdGF0dXMsZi5zdGF0dXNUZXh0LCJzdHJpbmciPT10eXBlb2YgZi5y
ZXNwb25zZVRleHQ/e3RleHQ6Zi5yZXNwb25zZVRleHR9OnZvaWQgMCxmLmdldEFs
bFJlc3BvbnNlSGVhZGVycygpKSl9fSxmLm9ubG9hZD1iKCksZi5vbmVycm9yPWIo
ImVycm9yIiksYj1EY1tnXT1iKCJhYm9ydCIpO3RyeXtmLnNlbmQoYS5oYXNDb250
ZW50JiZhLmRhdGF8fG51bGwpfWNhdGNoKGgpe2lmKGIpdGhyb3cgaH19LGFib3J0
OmZ1bmN0aW9uKCl7YiYmYigpfX06dm9pZCAwfSksbi5hamF4U2V0dXAoe2FjY2Vw
dHM6e3NjcmlwdDoidGV4dC9qYXZhc2NyaXB0LCBhcHBsaWNhdGlvbi9qYXZhc2Ny
aXB0LCBhcHBsaWNhdGlvbi9lY21hc2NyaXB0LCBhcHBsaWNhdGlvbi94LWVjbWFz
Y3JpcHQifSxjb250ZW50czp7c2NyaXB0Oi8oPzpqYXZhfGVjbWEpc2NyaXB0L30s
Y29udmVydGVyczp7InRleHQgc2NyaXB0IjpmdW5jdGlvbihhKXtyZXR1cm4gbi5n
bG9iYWxFdmFsKGEpLGF9fX0pLG4uYWpheFByZWZpbHRlcigic2NyaXB0IixmdW5j
dGlvbihhKXt2b2lkIDA9PT1hLmNhY2hlJiYoYS5jYWNoZT0hMSksYS5jcm9zc0Rv
bWFpbiYmKGEudHlwZT0iR0VUIil9KSxuLmFqYXhUcmFuc3BvcnQoInNjcmlwdCIs
ZnVuY3Rpb24oYSl7aWYoYS5jcm9zc0RvbWFpbil7dmFyIGIsYztyZXR1cm57c2Vu
ZDpmdW5jdGlvbihkLGUpe2I9bigiPHNjcmlwdD4iKS5wcm9wKHthc3luYzohMCxj
aGFyc2V0OmEuc2NyaXB0Q2hhcnNldCxzcmM6YS51cmx9KS5vbigibG9hZCBlcnJv
ciIsYz1mdW5jdGlvbihhKXtiLnJlbW92ZSgpLGM9bnVsbCxhJiZlKCJlcnJvciI9
PT1hLnR5cGU/NDA0OjIwMCxhLnR5cGUpfSksbC5oZWFkLmFwcGVuZENoaWxkKGJb
MF0pfSxhYm9ydDpmdW5jdGlvbigpe2MmJmMoKX19fX0pO3ZhciBHYz1bXSxIYz0v
KD0pXD8oPz0mfCQpfFw/XD8vO24uYWpheFNldHVwKHtqc29ucDoiY2FsbGJhY2si
LGpzb25wQ2FsbGJhY2s6ZnVuY3Rpb24oKXt2YXIgYT1HYy5wb3AoKXx8bi5leHBh
bmRvKyJfIitjYysrO3JldHVybiB0aGlzW2FdPSEwLGF9fSksbi5hamF4UHJlZmls
dGVyKCJqc29uIGpzb25wIixmdW5jdGlvbihiLGMsZCl7dmFyIGUsZixnLGg9Yi5q
c29ucCE9PSExJiYoSGMudGVzdChiLnVybCk/InVybCI6InN0cmluZyI9PXR5cGVv
ZiBiLmRhdGEmJiEoYi5jb250ZW50VHlwZXx8IiIpLmluZGV4T2YoImFwcGxpY2F0
aW9uL3gtd3d3LWZvcm0tdXJsZW5jb2RlZCIpJiZIYy50ZXN0KGIuZGF0YSkmJiJk
YXRhIik7cmV0dXJuIGh8fCJqc29ucCI9PT1iLmRhdGFUeXBlc1swXT8oZT1iLmpz
b25wQ2FsbGJhY2s9bi5pc0Z1bmN0aW9uKGIuanNvbnBDYWxsYmFjayk/Yi5qc29u
cENhbGxiYWNrKCk6Yi5qc29ucENhbGxiYWNrLGg/YltoXT1iW2hdLnJlcGxhY2Uo
SGMsIiQxIitlKTpiLmpzb25wIT09ITEmJihiLnVybCs9KGRjLnRlc3QoYi51cmwp
PyImIjoiPyIpK2IuanNvbnArIj0iK2UpLGIuY29udmVydGVyc1sic2NyaXB0IGpz
b24iXT1mdW5jdGlvbigpe3JldHVybiBnfHxuLmVycm9yKGUrIiB3YXMgbm90IGNh
bGxlZCIpLGdbMF19LGIuZGF0YVR5cGVzWzBdPSJqc29uIixmPWFbZV0sYVtlXT1m
dW5jdGlvbigpe2c9YXJndW1lbnRzfSxkLmFsd2F5cyhmdW5jdGlvbigpe2FbZV09
ZixiW2VdJiYoYi5qc29ucENhbGxiYWNrPWMuanNvbnBDYWxsYmFjayxHYy5wdXNo
KGUpKSxnJiZuLmlzRnVuY3Rpb24oZikmJmYoZ1swXSksZz1mPXZvaWQgMH0pLCJz
Y3JpcHQiKTp2b2lkIDB9KSxuLnBhcnNlSFRNTD1mdW5jdGlvbihhLGIsYyl7aWYo
IWF8fCJzdHJpbmciIT10eXBlb2YgYSlyZXR1cm4gbnVsbDsiYm9vbGVhbiI9PXR5
cGVvZiBiJiYoYz1iLGI9ITEpLGI9Ynx8bDt2YXIgZD12LmV4ZWMoYSksZT0hYyYm
W107cmV0dXJuIGQ/W2IuY3JlYXRlRWxlbWVudChkWzFdKV06KGQ9bi5idWlsZEZy
YWdtZW50KFthXSxiLGUpLGUmJmUubGVuZ3RoJiZuKGUpLnJlbW92ZSgpLG4ubWVy
Z2UoW10sZC5jaGlsZE5vZGVzKSl9O3ZhciBJYz1uLmZuLmxvYWQ7bi5mbi5sb2Fk
PWZ1bmN0aW9uKGEsYixjKXtpZigic3RyaW5nIiE9dHlwZW9mIGEmJkljKXJldHVy
biBJYy5hcHBseSh0aGlzLGFyZ3VtZW50cyk7dmFyIGQsZSxmLGc9dGhpcyxoPWEu
aW5kZXhPZigiICIpO3JldHVybiBoPj0wJiYoZD1uLnRyaW0oYS5zbGljZShoKSks
YT1hLnNsaWNlKDAsaCkpLG4uaXNGdW5jdGlvbihiKT8oYz1iLGI9dm9pZCAwKTpi
JiYib2JqZWN0Ij09dHlwZW9mIGImJihlPSJQT1NUIiksZy5sZW5ndGg+MCYmbi5h
amF4KHt1cmw6YSx0eXBlOmUsZGF0YVR5cGU6Imh0bWwiLGRhdGE6Yn0pLmRvbmUo
ZnVuY3Rpb24oYSl7Zj1hcmd1bWVudHMsZy5odG1sKGQ/bigiPGRpdj4iKS5hcHBl
bmQobi5wYXJzZUhUTUwoYSkpLmZpbmQoZCk6YSl9KS5jb21wbGV0ZShjJiZmdW5j
dGlvbihhLGIpe2cuZWFjaChjLGZ8fFthLnJlc3BvbnNlVGV4dCxiLGFdKX0pLHRo
aXN9LG4uZXhwci5maWx0ZXJzLmFuaW1hdGVkPWZ1bmN0aW9uKGEpe3JldHVybiBu
LmdyZXAobi50aW1lcnMsZnVuY3Rpb24oYil7cmV0dXJuIGE9PT1iLmVsZW19KS5s
ZW5ndGh9O3ZhciBKYz1hLmRvY3VtZW50LmRvY3VtZW50RWxlbWVudDtmdW5jdGlv
biBLYyhhKXtyZXR1cm4gbi5pc1dpbmRvdyhhKT9hOjk9PT1hLm5vZGVUeXBlJiZh
LmRlZmF1bHRWaWV3fW4ub2Zmc2V0PXtzZXRPZmZzZXQ6ZnVuY3Rpb24oYSxiLGMp
e3ZhciBkLGUsZixnLGgsaSxqLGs9bi5jc3MoYSwicG9zaXRpb24iKSxsPW4oYSks
bT17fTsic3RhdGljIj09PWsmJihhLnN0eWxlLnBvc2l0aW9uPSJyZWxhdGl2ZSIp
LGg9bC5vZmZzZXQoKSxmPW4uY3NzKGEsInRvcCIpLGk9bi5jc3MoYSwibGVmdCIp
LGo9KCJhYnNvbHV0ZSI9PT1rfHwiZml4ZWQiPT09aykmJihmK2kpLmluZGV4T2Yo
ImF1dG8iKT4tMSxqPyhkPWwucG9zaXRpb24oKSxnPWQudG9wLGU9ZC5sZWZ0KToo
Zz1wYXJzZUZsb2F0KGYpfHwwLGU9cGFyc2VGbG9hdChpKXx8MCksbi5pc0Z1bmN0
aW9uKGIpJiYoYj1iLmNhbGwoYSxjLGgpKSxudWxsIT1iLnRvcCYmKG0udG9wPWIu
dG9wLWgudG9wK2cpLG51bGwhPWIubGVmdCYmKG0ubGVmdD1iLmxlZnQtaC5sZWZ0
K2UpLCJ1c2luZyJpbiBiP2IudXNpbmcuY2FsbChhLG0pOmwuY3NzKG0pfX0sbi5m
bi5leHRlbmQoe29mZnNldDpmdW5jdGlvbihhKXtpZihhcmd1bWVudHMubGVuZ3Ro
KXJldHVybiB2b2lkIDA9PT1hP3RoaXM6dGhpcy5lYWNoKGZ1bmN0aW9uKGIpe24u
b2Zmc2V0LnNldE9mZnNldCh0aGlzLGEsYil9KTt2YXIgYixjLGQ9dGhpc1swXSxl
PXt0b3A6MCxsZWZ0OjB9LGY9ZCYmZC5vd25lckRvY3VtZW50O2lmKGYpcmV0dXJu
IGI9Zi5kb2N1bWVudEVsZW1lbnQsbi5jb250YWlucyhiLGQpPyh0eXBlb2YgZC5n
ZXRCb3VuZGluZ0NsaWVudFJlY3QhPT1VJiYoZT1kLmdldEJvdW5kaW5nQ2xpZW50
UmVjdCgpKSxjPUtjKGYpLHt0b3A6ZS50b3ArYy5wYWdlWU9mZnNldC1iLmNsaWVu
dFRvcCxsZWZ0OmUubGVmdCtjLnBhZ2VYT2Zmc2V0LWIuY2xpZW50TGVmdH0pOmV9
LHBvc2l0aW9uOmZ1bmN0aW9uKCl7aWYodGhpc1swXSl7dmFyIGEsYixjPXRoaXNb
MF0sZD17dG9wOjAsbGVmdDowfTtyZXR1cm4iZml4ZWQiPT09bi5jc3MoYywicG9z
aXRpb24iKT9iPWMuZ2V0Qm91bmRpbmdDbGllbnRSZWN0KCk6KGE9dGhpcy5vZmZz
ZXRQYXJlbnQoKSxiPXRoaXMub2Zmc2V0KCksbi5ub2RlTmFtZShhWzBdLCJodG1s
Iil8fChkPWEub2Zmc2V0KCkpLGQudG9wKz1uLmNzcyhhWzBdLCJib3JkZXJUb3BX
aWR0aCIsITApLGQubGVmdCs9bi5jc3MoYVswXSwiYm9yZGVyTGVmdFdpZHRoIiwh
MCkpLHt0b3A6Yi50b3AtZC50b3Atbi5jc3MoYywibWFyZ2luVG9wIiwhMCksbGVm
dDpiLmxlZnQtZC5sZWZ0LW4uY3NzKGMsIm1hcmdpbkxlZnQiLCEwKX19fSxvZmZz
ZXRQYXJlbnQ6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5tYXAoZnVuY3Rpb24oKXt2
YXIgYT10aGlzLm9mZnNldFBhcmVudHx8SmM7d2hpbGUoYSYmIW4ubm9kZU5hbWUo
YSwiaHRtbCIpJiYic3RhdGljIj09PW4uY3NzKGEsInBvc2l0aW9uIikpYT1hLm9m
ZnNldFBhcmVudDtyZXR1cm4gYXx8SmN9KX19KSxuLmVhY2goe3Njcm9sbExlZnQ6
InBhZ2VYT2Zmc2V0IixzY3JvbGxUb3A6InBhZ2VZT2Zmc2V0In0sZnVuY3Rpb24o
YixjKXt2YXIgZD0icGFnZVlPZmZzZXQiPT09YztuLmZuW2JdPWZ1bmN0aW9uKGUp
e3JldHVybiBKKHRoaXMsZnVuY3Rpb24oYixlLGYpe3ZhciBnPUtjKGIpO3JldHVy
biB2b2lkIDA9PT1mP2c/Z1tjXTpiW2VdOnZvaWQoZz9nLnNjcm9sbFRvKGQ/YS5w
YWdlWE9mZnNldDpmLGQ/ZjphLnBhZ2VZT2Zmc2V0KTpiW2VdPWYpfSxiLGUsYXJn
dW1lbnRzLmxlbmd0aCxudWxsKX19KSxuLmVhY2goWyJ0b3AiLCJsZWZ0Il0sZnVu
Y3Rpb24oYSxiKXtuLmNzc0hvb2tzW2JdPXliKGsucGl4ZWxQb3NpdGlvbixmdW5j
dGlvbihhLGMpe3JldHVybiBjPyhjPXhiKGEsYiksdmIudGVzdChjKT9uKGEpLnBv
c2l0aW9uKClbYl0rInB4IjpjKTp2b2lkIDB9KX0pLG4uZWFjaCh7SGVpZ2h0OiJo
ZWlnaHQiLFdpZHRoOiJ3aWR0aCJ9LGZ1bmN0aW9uKGEsYil7bi5lYWNoKHtwYWRk
aW5nOiJpbm5lciIrYSxjb250ZW50OmIsIiI6Im91dGVyIithfSxmdW5jdGlvbihj
LGQpe24uZm5bZF09ZnVuY3Rpb24oZCxlKXt2YXIgZj1hcmd1bWVudHMubGVuZ3Ro
JiYoY3x8ImJvb2xlYW4iIT10eXBlb2YgZCksZz1jfHwoZD09PSEwfHxlPT09ITA/
Im1hcmdpbiI6ImJvcmRlciIpO3JldHVybiBKKHRoaXMsZnVuY3Rpb24oYixjLGQp
e3ZhciBlO3JldHVybiBuLmlzV2luZG93KGIpP2IuZG9jdW1lbnQuZG9jdW1lbnRF
bGVtZW50WyJjbGllbnQiK2FdOjk9PT1iLm5vZGVUeXBlPyhlPWIuZG9jdW1lbnRF
bGVtZW50LE1hdGgubWF4KGIuYm9keVsic2Nyb2xsIithXSxlWyJzY3JvbGwiK2Fd
LGIuYm9keVsib2Zmc2V0IithXSxlWyJvZmZzZXQiK2FdLGVbImNsaWVudCIrYV0p
KTp2b2lkIDA9PT1kP24uY3NzKGIsYyxnKTpuLnN0eWxlKGIsYyxkLGcpfSxiLGY/
ZDp2b2lkIDAsZixudWxsKX19KX0pLG4uZm4uc2l6ZT1mdW5jdGlvbigpe3JldHVy
biB0aGlzLmxlbmd0aH0sbi5mbi5hbmRTZWxmPW4uZm4uYWRkQmFjaywiZnVuY3Rp
b24iPT10eXBlb2YgZGVmaW5lJiZkZWZpbmUuYW1kJiZkZWZpbmUoImpxdWVyeSIs
W10sZnVuY3Rpb24oKXtyZXR1cm4gbn0pO3ZhciBMYz1hLmpRdWVyeSxNYz1hLiQ7
cmV0dXJuIG4ubm9Db25mbGljdD1mdW5jdGlvbihiKXtyZXR1cm4gYS4kPT09biYm
KGEuJD1NYyksYiYmYS5qUXVlcnk9PT1uJiYoYS5qUXVlcnk9TGMpLG59LHR5cGVv
ZiBiPT09VSYmKGEualF1ZXJ5PWEuJD1uKSxufSk7Ci8vIyBzb3VyY2VNYXBwaW5n
VVJMPWpxdWVyeS5taW4ubWFwPC9zY3JpcHQ+DQogICAgICA8IS0tIGQzLnNsaWRl
ci5qcyBsaWJyYXJ5IC0tPg0KICAgICAgPHNjcmlwdD4vKg0KICAgIEQzLmpzIFNs
aWRlcg0KICAgIEluc3BpcmVkIGJ5IGpRdWVyeSBVSSBTbGlkZXINCiAgICBDb3B5
cmlnaHQgKGMpIDIwMTMsIEJqb3JuIFNhbmR2aWsgLSBodHRwOi8vYmxvZy50aGVt
YXRpY21hcHBpbmcub3JnDQogICAgQlNEIGxpY2Vuc2U6IGh0dHA6Ly9vcGVuc291
cmNlLm9yZy9saWNlbnNlcy9CU0QtMy1DbGF1c2UNCiovDQooZnVuY3Rpb24gKHJv
b3QsIGZhY3RvcnkpIHsNCiAgaWYgKHR5cGVvZiBkZWZpbmUgPT09ICdmdW5jdGlv
bicgJiYgZGVmaW5lLmFtZCkgew0KICAgIC8vIEFNRC4gUmVnaXN0ZXIgYXMgYW4g
YW5vbnltb3VzIG1vZHVsZS4NCiAgICBkZWZpbmUoWydkMyddLCBmYWN0b3J5KTsN
CiAgfSBlbHNlIGlmICh0eXBlb2YgZXhwb3J0cyA9PT0gJ29iamVjdCcpIHsNCiAg
ICBpZiAocHJvY2Vzcy5icm93c2VyKSB7DQogICAgICAvLyBCcm93c2VyaWZ5LiBJ
bXBvcnQgY3NzIHRvbyB1c2luZyBjc3NpZnkuDQogICAgICByZXF1aXJlKCcuL2Qz
LnNsaWRlci5jc3MnKTsNCiAgICB9DQogICAgLy8gTm9kZS4gRG9lcyBub3Qgd29y
ayB3aXRoIHN0cmljdCBDb21tb25KUywgYnV0DQogICAgLy8gb25seSBDb21tb25K
Uy1saWtlIGVudmlyb25tZW50cyB0aGF0IHN1cHBvcnQgbW9kdWxlLmV4cG9ydHMs
DQogICAgLy8gbGlrZSBOb2RlLg0KICAgIG1vZHVsZS5leHBvcnRzID0gZmFjdG9y
eShyZXF1aXJlKCdkMycpKTsNCiAgfSBlbHNlIHsNCiAgICAvLyBCcm93c2VyIGds
b2JhbHMgKHJvb3QgaXMgd2luZG93KQ0KICAgIHJvb3QuZDMuc2xpZGVyID0gZmFj
dG9yeShyb290LmQzKTsNCiAgfQ0KfSh0aGlzLCBmdW5jdGlvbiAoZDMpIHsNCnJl
dHVybiBmdW5jdGlvbiBtb2R1bGUoKSB7DQogICJ1c2Ugc3RyaWN0IjsNCg0KICAv
LyBQdWJsaWMgdmFyaWFibGVzIHdpZHRoIGRlZmF1bHQgc2V0dGluZ3MNCiAgdmFy
IG1pbiA9IDAsDQogICAgICBtYXggPSAxMDAsDQogICAgICBzdGVwID0gMC4wMSwN
CiAgICAgIGFuaW1hdGUgPSB0cnVlLA0KICAgICAgb3JpZW50YXRpb24gPSAiaG9y
aXpvbnRhbCIsDQogICAgICBheGlzID0gZmFsc2UsDQogICAgICBtYXJnaW4gPSA1
MCwNCiAgICAgIHZhbHVlLA0KICAgICAgYWN0aXZlID0gMSwNCiAgICAgIHNjYWxl
LA0KICAgICAgaW50ZXJ2YWw9MTsNCg0KICAvLyBQcml2YXRlIHZhcmlhYmxlcw0K
ICB2YXIgYXhpc1NjYWxlLA0KICAgICAgZGlzcGF0Y2ggPSBkMy5kaXNwYXRjaCgi
c2xpZGUiLCAic2xpZGVlbmQiKSwNCiAgICAgIGZvcm1hdFBlcmNlbnQgPSBkMy5m
b3JtYXQoIi4yJSIpLA0KICAgICAgdGlja0Zvcm1hdCA9IGQzLmZvcm1hdCgiLjAi
KSwNCiAgICAgIGhhbmRsZTEsDQogICAgICBoYW5kbGUyID0gbnVsbCwNCiAgICAg
IHNsaWRlckxlbmd0aDsNCg0KICBmdW5jdGlvbiBzbGlkZXIoc2VsZWN0aW9uKSB7
DQogICAgc2VsZWN0aW9uLmVhY2goZnVuY3Rpb24oKSB7DQoNCiAgICAgIC8vIENy
ZWF0ZSBzY2FsZSBpZiBub3QgZGVmaW5lZCBieSB1c2VyDQogICAgICBpZiAoIXNj
YWxlKSB7DQogICAgICAgIHNjYWxlID0gZDMuc2NhbGUubGluZWFyKCkuZG9tYWlu
KFttaW4sIG1heF0pOw0KICAgICAgfQ0KDQogICAgICAvLyBTdGFydCB2YWx1ZQ0K
ICAgICAgdmFsdWUgPSB2YWx1ZSB8fCBzY2FsZS5kb21haW4oKVswXTsNCg0KICAg
ICAgLy8gRElWIGNvbnRhaW5lcg0KICAgICAgdmFyIGRpdiA9IGQzLnNlbGVjdCh0
aGlzKS5jbGFzc2VkKCJkMy1zbGlkZXIgZDMtc2xpZGVyLSIgKyBvcmllbnRhdGlv
biwgdHJ1ZSk7DQogICAgICANCiAgICAgIHZhciBkcmFnID0gZDMuYmVoYXZpb3Iu
ZHJhZygpOw0KICAgICAgZHJhZy5vbignZHJhZ2VuZCcsIGZ1bmN0aW9uICgpIHsN
CiAgICAgICAgZGlzcGF0Y2guc2xpZGVlbmQoZDMuZXZlbnQsIHZhbHVlKTsNCiAg
ICAgIH0pDQoNCiAgICAgIC8vIFNsaWRlciBoYW5kbGUNCiAgICAgIC8vaWYgcmFu
Z2Ugc2xpZGVyLCBjcmVhdGUgdHdvDQogICAgICB2YXIgZGl2UmFuZ2U7DQoNCiAg
ICAgIGlmICggdmFsdWUubGVuZ3RoID09IDIgKSB7DQogICAgICAgIGhhbmRsZTEg
PSBkaXYuYXBwZW5kKCJhIikNCiAgICAgICAgICAuY2xhc3NlZCgiZDMtc2xpZGVy
LWhhbmRsZSIsIHRydWUpDQogICAgICAgICAgLmF0dHIoInhsaW5rOmhyZWYiLCAi
IyIpDQogICAgICAgICAgLmF0dHIoJ2lkJywgImhhbmRsZS1vbmUiKQ0KICAgICAg
ICAgIC5vbigiY2xpY2siLCBzdG9wUHJvcGFnYXRpb24pDQogICAgICAgICAgLmNh
bGwoZHJhZyk7DQogICAgICAgIGhhbmRsZTIgPSBkaXYuYXBwZW5kKCJhIikNCiAg
ICAgICAgICAuY2xhc3NlZCgiZDMtc2xpZGVyLWhhbmRsZSIsIHRydWUpDQogICAg
ICAgICAgLmF0dHIoJ2lkJywgImhhbmRsZS10d28iKQ0KICAgICAgICAgIC5hdHRy
KCJ4bGluazpocmVmIiwgIiMiKQ0KICAgICAgICAgIC5vbigiY2xpY2siLCBzdG9w
UHJvcGFnYXRpb24pDQogICAgICAgICAgLmNhbGwoZHJhZyk7DQogICAgICB9IGVs
c2Ugew0KICAgICAgICBoYW5kbGUxID0gZGl2LmFwcGVuZCgiYSIpDQogICAgICAg
ICAgLmNsYXNzZWQoImQzLXNsaWRlci1oYW5kbGUiLCB0cnVlKQ0KICAgICAgICAg
IC5hdHRyKCJ4bGluazpocmVmIiwgIiMiKQ0KICAgICAgICAgIC5hdHRyKCdpZCcs
ICJoYW5kbGUtb25lIikNCiAgICAgICAgICAub24oImNsaWNrIiwgc3RvcFByb3Bh
Z2F0aW9uKQ0KICAgICAgICAgIC5jYWxsKGRyYWcpOw0KICAgICAgfQ0KICAgICAg
Ly8gSG9yaXpvbnRhbCBzbGlkZXINCiAgICAgIGlmIChvcmllbnRhdGlvbiA9PT0g
Imhvcml6b250YWwiKSB7DQoNCiAgICAgICAgZGl2Lm9uKCJjbGljayIsIG9uQ2xp
Y2tIb3Jpem9udGFsKTsNCiAgICAgICAgDQogICAgICAgIGlmICggdmFsdWUubGVu
Z3RoID09IDIgKSB7DQogICAgICAgICAgZGl2UmFuZ2UgPSBkMy5zZWxlY3QodGhp
cykuYXBwZW5kKCdkaXYnKS5jbGFzc2VkKCJkMy1zbGlkZXItcmFuZ2UiLCB0cnVl
KTsNCg0KICAgICAgICAgIGhhbmRsZTEuc3R5bGUoImxlZnQiLCBmb3JtYXRQZXJj
ZW50KHNjYWxlKHZhbHVlWyAwIF0pKSk7DQogICAgICAgICAgZGl2UmFuZ2Uuc3R5
bGUoImxlZnQiLCBmb3JtYXRQZXJjZW50KHNjYWxlKHZhbHVlWyAwIF0pKSk7DQog
ICAgICAgICAgZHJhZy5vbigiZHJhZyIsIG9uRHJhZ0hvcml6b250YWwpOw0KDQog
ICAgICAgICAgdmFyIHdpZHRoID0gMTAwIC0gcGFyc2VGbG9hdChmb3JtYXRQZXJj
ZW50KHNjYWxlKHZhbHVlWyAxIF0pKSk7DQogICAgICAgICAgaGFuZGxlMi5zdHls
ZSgibGVmdCIsIGZvcm1hdFBlcmNlbnQoc2NhbGUodmFsdWVbIDEgXSkpKTsNCiAg
ICAgICAgICBkaXZSYW5nZS5zdHlsZSgicmlnaHQiLCB3aWR0aCsiJSIpOw0KICAg
ICAgICAgIGRyYWcub24oImRyYWciLCBvbkRyYWdIb3Jpem9udGFsKTsNCg0KICAg
ICAgICB9IGVsc2Ugew0KICAgICAgICAgIGhhbmRsZTEuc3R5bGUoImxlZnQiLCBm
b3JtYXRQZXJjZW50KHNjYWxlKHZhbHVlKSkpOw0KICAgICAgICAgIGRyYWcub24o
ImRyYWciLCBvbkRyYWdIb3Jpem9udGFsKTsNCiAgICAgICAgfQ0KICAgICAgICAN
Cg0KICAgICAgICBzbGlkZXJMZW5ndGggPSBwYXJzZUludChkaXYuc3R5bGUoIndp
ZHRoIiksIDEwKTsNCiAgICAgICAgaGFuZGxlMS5zdHlsZSgnd2lkdGgnLCAoKHNj
YWxlKGludGVydmFsK21pbikqc2xpZGVyTGVuZ3RoKSkrJ3B4JykNCg0KICAgICAg
fSBlbHNlIHsgLy8gVmVydGljYWwNCg0KICAgICAgICBkaXYub24oImNsaWNrIiwg
b25DbGlja1ZlcnRpY2FsKTsNCiAgICAgICAgZHJhZy5vbigiZHJhZyIsIG9uRHJh
Z1ZlcnRpY2FsKTsNCiAgICAgICAgaWYgKCB2YWx1ZS5sZW5ndGggPT0gMiApIHsN
CiAgICAgICAgICBkaXZSYW5nZSA9IGQzLnNlbGVjdCh0aGlzKS5hcHBlbmQoJ2Rp
dicpLmNsYXNzZWQoImQzLXNsaWRlci1yYW5nZS12ZXJ0aWNhbCIsIHRydWUpOw0K
DQogICAgICAgICAgaGFuZGxlMS5zdHlsZSgiYm90dG9tIiwgZm9ybWF0UGVyY2Vu
dChzY2FsZSh2YWx1ZVsgMCBdKSkpOw0KICAgICAgICAgIGRpdlJhbmdlLnN0eWxl
KCJib3R0b20iLCBmb3JtYXRQZXJjZW50KHNjYWxlKHZhbHVlWyAwIF0pKSk7DQog
ICAgICAgICAgZHJhZy5vbigiZHJhZyIsIG9uRHJhZ1ZlcnRpY2FsKTsNCg0KICAg
ICAgICAgIHZhciB0b3AgPSAxMDAgLSBwYXJzZUZsb2F0KGZvcm1hdFBlcmNlbnQo
c2NhbGUodmFsdWVbIDEgXSkpKTsNCiAgICAgICAgICBoYW5kbGUyLnN0eWxlKCJi
b3R0b20iLCBmb3JtYXRQZXJjZW50KHNjYWxlKHZhbHVlWyAxIF0pKSk7DQogICAg
ICAgICAgZGl2UmFuZ2Uuc3R5bGUoInRvcCIsIHRvcCsiJSIpOw0KICAgICAgICAg
IGRyYWcub24oImRyYWciLCBvbkRyYWdWZXJ0aWNhbCk7DQoNCiAgICAgICAgfSBl
bHNlIHsNCiAgICAgICAgICBoYW5kbGUxLnN0eWxlKCJib3R0b20iLCBmb3JtYXRQ
ZXJjZW50KHNjYWxlKHZhbHVlKSkpOw0KICAgICAgICAgIGRyYWcub24oImRyYWci
LCBvbkRyYWdWZXJ0aWNhbCk7DQogICAgICAgIH0NCiAgICAgICAgDQogICAgICAg
IHNsaWRlckxlbmd0aCA9IHBhcnNlSW50KGRpdi5zdHlsZSgiaGVpZ2h0IiksIDEw
KTsNCg0KICAgICAgfQ0KICAgICAgDQogICAgICBpZiAoYXhpcykgew0KICAgICAg
ICBjcmVhdGVBeGlzKGRpdik7DQogICAgICB9DQoNCg0KICAgICAgZnVuY3Rpb24g
Y3JlYXRlQXhpcyhkb20pIHsNCg0KICAgICAgICAvLyBDcmVhdGUgYXhpcyBpZiBu
b3QgZGVmaW5lZCBieSB1c2VyDQogICAgICAgIGlmICh0eXBlb2YgYXhpcyA9PT0g
ImJvb2xlYW4iKSB7DQoNCiAgICAgICAgICBheGlzID0gZDMuc3ZnLmF4aXMoKQ0K
ICAgICAgICAgICAgICAudGlja3MoTWF0aC5yb3VuZChzbGlkZXJMZW5ndGggLyAx
MDApKQ0KICAgICAgICAgICAgICAudGlja0Zvcm1hdCh0aWNrRm9ybWF0KQ0KICAg
ICAgICAgICAgICAub3JpZW50KChvcmllbnRhdGlvbiA9PT0gImhvcml6b250YWwi
KSA/ICJib3R0b20iIDogICJyaWdodCIpOw0KDQogICAgICAgIH0NCg0KICAgICAg
ICAvLyBDb3B5IHNsaWRlciBzY2FsZSB0byBtb3ZlIGZyb20gcGVyY2VudGFnZXMg
dG8gcGl4ZWxzDQogICAgICAgIGF4aXNTY2FsZSA9IHNjYWxlLmNvcHkoKS5yYW5n
ZShbMCwgc2xpZGVyTGVuZ3RoXSk7DQogICAgICAgICAgYXhpcy5zY2FsZShheGlz
U2NhbGUpOw0KDQogICAgICAgICAgLy8gQ3JlYXRlIFNWRyBheGlzIGNvbnRhaW5l
cg0KICAgICAgICB2YXIgc3ZnID0gZG9tLmFwcGVuZCgic3ZnIikNCiAgICAgICAg
ICAgIC5jbGFzc2VkKCJkMy1zbGlkZXItYXhpcyBkMy1zbGlkZXItYXhpcy0iICsg
YXhpcy5vcmllbnQoKSwgdHJ1ZSkNCiAgICAgICAgICAgIC5vbigiY2xpY2siLCBz
dG9wUHJvcGFnYXRpb24pOw0KDQogICAgICAgIHZhciBnID0gc3ZnLmFwcGVuZCgi
ZyIpOw0KDQogICAgICAgIC8vIEhvcml6b250YWwgYXhpcw0KICAgICAgICBpZiAo
b3JpZW50YXRpb24gPT09ICJob3Jpem9udGFsIikgew0KDQogICAgICAgICAgc3Zn
LnN0eWxlKCJtYXJnaW4tbGVmdCIsIC1tYXJnaW4gKyAicHgiKTsNCg0KICAgICAg
ICAgIHN2Zy5hdHRyKHsNCiAgICAgICAgICAgIHdpZHRoOiBzbGlkZXJMZW5ndGgg
KyBtYXJnaW4gKiAyLA0KICAgICAgICAgICAgaGVpZ2h0OiBtYXJnaW4NCiAgICAg
ICAgICB9KTsNCg0KICAgICAgICAgIGlmIChheGlzLm9yaWVudCgpID09PSAidG9w
Iikgew0KICAgICAgICAgICAgc3ZnLnN0eWxlKCJ0b3AiLCAtbWFyZ2luICsgInB4
Iik7DQogICAgICAgICAgICBnLmF0dHIoInRyYW5zZm9ybSIsICJ0cmFuc2xhdGUo
IiArIG1hcmdpbiArICIsIiArIG1hcmdpbiArICIpIik7DQogICAgICAgICAgfSBl
bHNlIHsgLy8gYm90dG9tDQogICAgICAgICAgICBnLmF0dHIoInRyYW5zZm9ybSIs
ICJ0cmFuc2xhdGUoIiArIG1hcmdpbiArICIsMCkiKTsNCiAgICAgICAgICB9DQoN
CiAgICAgICAgfSBlbHNlIHsgLy8gVmVydGljYWwNCg0KICAgICAgICAgIHN2Zy5z
dHlsZSgidG9wIiwgLW1hcmdpbiArICJweCIpOw0KDQogICAgICAgICAgc3ZnLmF0
dHIoew0KICAgICAgICAgICAgd2lkdGg6IG1hcmdpbiwNCiAgICAgICAgICAgIGhl
aWdodDogc2xpZGVyTGVuZ3RoICsgbWFyZ2luICogMg0KICAgICAgICAgIH0pOw0K
DQogICAgICAgICAgaWYgKGF4aXMub3JpZW50KCkgPT09ICJsZWZ0Iikgew0KICAg
ICAgICAgICAgc3ZnLnN0eWxlKCJsZWZ0IiwgLW1hcmdpbiArICJweCIpOw0KICAg
ICAgICAgICAgZy5hdHRyKCJ0cmFuc2Zvcm0iLCAidHJhbnNsYXRlKCIgKyBtYXJn
aW4gKyAiLCIgKyBtYXJnaW4gKyAiKSIpOw0KICAgICAgICAgIH0gZWxzZSB7IC8v
IHJpZ2h0ICAgICAgICAgIA0KICAgICAgICAgICAgZy5hdHRyKCJ0cmFuc2Zvcm0i
LCAidHJhbnNsYXRlKCIgKyAwICsgIiwiICsgbWFyZ2luICsgIikiKTsNCiAgICAg
ICAgICB9DQoNCiAgICAgICAgfQ0KDQogICAgICAgIGcuY2FsbChheGlzKTsNCg0K
ICAgICAgfQ0KDQogICAgICBmdW5jdGlvbiBvbkNsaWNrSG9yaXpvbnRhbCgpIHsN
CiAgICAgICAgaWYgKCF2YWx1ZS5sZW5ndGgpIHsNCiAgICAgICAgICB2YXIgcG9z
ID0gTWF0aC5tYXgoMCwgTWF0aC5taW4oc2xpZGVyTGVuZ3RoLCBkMy5ldmVudC5v
ZmZzZXRYIHx8IGQzLmV2ZW50LmxheWVyWCkpOw0KICAgICAgICAgIG1vdmVIYW5k
bGUoc3RlcFZhbHVlKHNjYWxlLmludmVydChwb3MgLyBzbGlkZXJMZW5ndGgpKSk7
DQogICAgICAgIH0NCiAgICAgIH0NCg0KICAgICAgZnVuY3Rpb24gb25DbGlja1Zl
cnRpY2FsKCkgew0KICAgICAgICBpZiAoIXZhbHVlLmxlbmd0aCkgew0KICAgICAg
ICAgIHZhciBwb3MgPSBzbGlkZXJMZW5ndGggLSBNYXRoLm1heCgwLCBNYXRoLm1p
bihzbGlkZXJMZW5ndGgsIGQzLmV2ZW50Lm9mZnNldFkgfHwgZDMuZXZlbnQubGF5
ZXJZKSk7DQogICAgICAgICAgbW92ZUhhbmRsZShzdGVwVmFsdWUoc2NhbGUuaW52
ZXJ0KHBvcyAvIHNsaWRlckxlbmd0aCkpKTsNCiAgICAgICAgfQ0KICAgICAgfQ0K
DQogICAgICBmdW5jdGlvbiBvbkRyYWdIb3Jpem9udGFsKCkgew0KICAgICAgICBp
ZiAoIGQzLmV2ZW50LnNvdXJjZUV2ZW50LnRhcmdldC5pZCA9PT0gImhhbmRsZS1v
bmUiKSB7DQogICAgICAgICAgYWN0aXZlID0gMTsNCiAgICAgICAgfSBlbHNlIGlm
ICggZDMuZXZlbnQuc291cmNlRXZlbnQudGFyZ2V0LmlkID09ICJoYW5kbGUtdHdv
IiApIHsNCiAgICAgICAgICBhY3RpdmUgPSAyOw0KICAgICAgICB9DQogICAgICAg
IHZhciBwb3MgPSBNYXRoLm1heCgwLCBNYXRoLm1pbihzbGlkZXJMZW5ndGgsIGQz
LmV2ZW50LngpKTsNCiAgICAgICAgbW92ZUhhbmRsZShzdGVwVmFsdWUoc2NhbGUu
aW52ZXJ0KHBvcyAvIHNsaWRlckxlbmd0aCkpKTsNCiAgICAgIH0NCg0KICAgICAg
ZnVuY3Rpb24gb25EcmFnVmVydGljYWwoKSB7DQogICAgICAgIGlmICggZDMuZXZl
bnQuc291cmNlRXZlbnQudGFyZ2V0LmlkID09PSAiaGFuZGxlLW9uZSIpIHsNCiAg
ICAgICAgICBhY3RpdmUgPSAxOw0KICAgICAgICB9IGVsc2UgaWYgKCBkMy5ldmVu
dC5zb3VyY2VFdmVudC50YXJnZXQuaWQgPT0gImhhbmRsZS10d28iICkgew0KICAg
ICAgICAgIGFjdGl2ZSA9IDI7DQogICAgICAgIH0NCiAgICAgICAgdmFyIHBvcyA9
IHNsaWRlckxlbmd0aCAtIE1hdGgubWF4KDAsIE1hdGgubWluKHNsaWRlckxlbmd0
aCwgZDMuZXZlbnQueSkpDQogICAgICAgIG1vdmVIYW5kbGUoc3RlcFZhbHVlKHNj
YWxlLmludmVydChwb3MgLyBzbGlkZXJMZW5ndGgpKSk7DQogICAgICB9DQoNCiAg
ICAgIGZ1bmN0aW9uIHN0b3BQcm9wYWdhdGlvbigpIHsNCiAgICAgICAgZDMuZXZl
bnQuc3RvcFByb3BhZ2F0aW9uKCk7DQogICAgICB9DQoNCiAgICB9KTsNCg0KICB9
DQoNCiAgLy8gTW92ZSBzbGlkZXIgaGFuZGxlIG9uIGNsaWNrL2RyYWcNCiAgZnVu
Y3Rpb24gbW92ZUhhbmRsZShuZXdWYWx1ZSkgeyANCiAgICBpZiAoc2xpZGVyLm1h
eCgpLXNsaWRlci5pbnRlcnZhbCgpIDwgbmV3VmFsdWUpIHsgDQogICAgICBuZXdW
YWx1ZSA9IHNsaWRlci5tYXgoKSAtIHNsaWRlci5pbnRlcnZhbCgpOw0KICAgIH0N
Cg0KICAgIHZhciBjdXJyZW50VmFsdWUgPSB2YWx1ZS5sZW5ndGggPyB2YWx1ZVth
Y3RpdmUgLSAxXTogdmFsdWU7DQoNCiAgICBpZiAoY3VycmVudFZhbHVlICE9PSBu
ZXdWYWx1ZSkgew0KICAgICAgdmFyIG9sZFBvcyA9IGZvcm1hdFBlcmNlbnQoc2Nh
bGUoc3RlcFZhbHVlKGN1cnJlbnRWYWx1ZSkpKSwNCiAgICAgICAgICBuZXdQb3Mg
PSBmb3JtYXRQZXJjZW50KHNjYWxlKHN0ZXBWYWx1ZShuZXdWYWx1ZSkpKSwNCiAg
ICAgICAgICBwb3NpdGlvbiA9IChvcmllbnRhdGlvbiA9PT0gImhvcml6b250YWwi
KSA/ICJsZWZ0IiA6ICJib3R0b20iOw0KDQogICAgICBpZiAoIHZhbHVlLmxlbmd0
aCA9PT0gMikgew0KICAgICAgICB2YWx1ZVsgYWN0aXZlIC0gMSBdID0gbmV3VmFs
dWU7DQogICAgICAgIGlmIChkMy5ldmVudCkgew0KICAgICAgICAgIGRpc3BhdGNo
LnNsaWRlKGQzLmV2ZW50LCB2YWx1ZSApOw0KICAgICAgICB9Ow0KICAgICAgfSBl
bHNlIHsNCiAgICAgICAgaWYgKGQzLmV2ZW50KSB7DQogICAgICAgICAgZGlzcGF0
Y2guc2xpZGUoZDMuZXZlbnQuc291cmNlRXZlbnQgfHwgZDMuZXZlbnQsIHZhbHVl
ID0gbmV3VmFsdWUpOw0KICAgICAgICB9Ow0KICAgICAgfQ0KDQogICAgICBpZiAo
IHZhbHVlWyAwIF0gPj0gdmFsdWVbIDEgXSApIHJldHVybjsNCiAgICAgIGlmICgg
YWN0aXZlID09PSAxICkgew0KICAgICAgICANCiAgICAgICAgaWYgKHZhbHVlLmxl
bmd0aCA9PT0gMikgew0KICAgICAgICAgIChwb3NpdGlvbiA9PT0gImxlZnQiKSA/
IGRpdlJhbmdlLnN0eWxlKCJsZWZ0IiwgbmV3UG9zKSA6IGRpdlJhbmdlLnN0eWxl
KCJib3R0b20iLCBuZXdQb3MpOw0KICAgICAgICB9DQoNCiAgICAgICAgaWYgKGFu
aW1hdGUpIHsNCiAgICAgICAgICBoYW5kbGUxLnRyYW5zaXRpb24oKQ0KICAgICAg
ICAgICAgICAuc3R5bGVUd2Vlbihwb3NpdGlvbiwgZnVuY3Rpb24oKSB7IHJldHVy
biBkMy5pbnRlcnBvbGF0ZShvbGRQb3MsIG5ld1Bvcyk7IH0pDQogICAgICAgICAg
ICAgIC5kdXJhdGlvbigodHlwZW9mIGFuaW1hdGUgPT09ICJudW1iZXIiKSA/IGFu
aW1hdGUgOiAyNTApOw0KICAgICAgICB9IGVsc2Ugew0KICAgICAgICAgIGhhbmRs
ZTEuc3R5bGUocG9zaXRpb24sIG5ld1Bvcyk7DQogICAgICAgIH0NCiAgICAgIH0g
ZWxzZSB7DQogICAgICAgIA0KICAgICAgICB2YXIgd2lkdGggPSAxMDAgLSBwYXJz
ZUZsb2F0KG5ld1Bvcyk7DQogICAgICAgIHZhciB0b3AgPSAxMDAgLSBwYXJzZUZs
b2F0KG5ld1Bvcyk7DQoNCiAgICAgICAgKHBvc2l0aW9uID09PSAibGVmdCIpID8g
ZGl2UmFuZ2Uuc3R5bGUoInJpZ2h0Iiwgd2lkdGggKyAiJSIpIDogZGl2UmFuZ2Uu
c3R5bGUoInRvcCIsIHRvcCArICIlIik7DQogICAgICAgIA0KICAgICAgICBpZiAo
YW5pbWF0ZSkgew0KICAgICAgICAgIGhhbmRsZTIudHJhbnNpdGlvbigpDQogICAg
ICAgICAgICAgIC5zdHlsZVR3ZWVuKHBvc2l0aW9uLCBmdW5jdGlvbigpIHsgcmV0
dXJuIGQzLmludGVycG9sYXRlKG9sZFBvcywgbmV3UG9zKTsgfSkNCiAgICAgICAg
ICAgICAgLmR1cmF0aW9uKCh0eXBlb2YgYW5pbWF0ZSA9PT0gIm51bWJlciIpID8g
YW5pbWF0ZSA6IDI1MCk7DQogICAgICAgIH0gZWxzZSB7DQogICAgICAgICAgaGFu
ZGxlMi5zdHlsZShwb3NpdGlvbiwgbmV3UG9zKTsNCiAgICAgICAgfQ0KICAgICAg
fQ0KICAgIH0NCiAgfQ0KDQogIC8vIENhbGN1bGF0ZSBuZWFyZXN0IHN0ZXAgdmFs
dWUNCiAgZnVuY3Rpb24gc3RlcFZhbHVlKHZhbCkgew0KDQogICAgaWYgKHZhbCA9
PT0gc2NhbGUuZG9tYWluKClbMF0gfHwgdmFsID09PSBzY2FsZS5kb21haW4oKVsx
XSkgew0KICAgICAgcmV0dXJuIHZhbDsNCiAgICB9DQoNCiAgICB2YXIgdmFsTW9k
U3RlcCA9ICh2YWwgLSBzY2FsZS5kb21haW4oKVswXSkgJSBzdGVwLA0KICAgICAg
ICBhbGlnblZhbHVlID0gdmFsIC0gdmFsTW9kU3RlcDsNCg0KICAgIGlmIChNYXRo
LmFicyh2YWxNb2RTdGVwKSAqIDIgPj0gc3RlcCkgew0KICAgICAgYWxpZ25WYWx1
ZSArPSAodmFsTW9kU3RlcCA+IDApID8gc3RlcCA6IC1zdGVwOw0KICAgIH0NCg0K
ICAgIHJldHVybiBhbGlnblZhbHVlOw0KDQogIH0NCg0KICAvLyBHZXR0ZXIvc2V0
dGVyIGZ1bmN0aW9ucw0KICBzbGlkZXIubWluID0gZnVuY3Rpb24oXykgew0KICAg
IGlmICghYXJndW1lbnRzLmxlbmd0aCkgcmV0dXJuIG1pbjsNCiAgICBtaW4gPSBf
Ow0KICAgIHJldHVybiBzbGlkZXI7DQogIH07DQoNCiAgc2xpZGVyLm1heCA9IGZ1
bmN0aW9uKF8pIHsNCiAgICBpZiAoIWFyZ3VtZW50cy5sZW5ndGgpIHJldHVybiBt
YXg7DQogICAgbWF4ID0gXzsNCiAgICByZXR1cm4gc2xpZGVyOw0KICB9Ow0KDQog
IHNsaWRlci5zdGVwID0gZnVuY3Rpb24oXykgew0KICAgIGlmICghYXJndW1lbnRz
Lmxlbmd0aCkgcmV0dXJuIHN0ZXA7DQogICAgc3RlcCA9IF87DQogICAgcmV0dXJu
IHNsaWRlcjsNCiAgfTsNCg0KICBzbGlkZXIuYW5pbWF0ZSA9IGZ1bmN0aW9uKF8p
IHsNCiAgICBpZiAoIWFyZ3VtZW50cy5sZW5ndGgpIHJldHVybiBhbmltYXRlOw0K
ICAgIGFuaW1hdGUgPSBfOw0KICAgIHJldHVybiBzbGlkZXI7DQogIH07DQoNCiAg
c2xpZGVyLm9yaWVudGF0aW9uID0gZnVuY3Rpb24oXykgew0KICAgIGlmICghYXJn
dW1lbnRzLmxlbmd0aCkgcmV0dXJuIG9yaWVudGF0aW9uOw0KICAgIG9yaWVudGF0
aW9uID0gXzsNCiAgICByZXR1cm4gc2xpZGVyOw0KICB9Ow0KDQogIHNsaWRlci5h
eGlzID0gZnVuY3Rpb24oXykgew0KICAgIGlmICghYXJndW1lbnRzLmxlbmd0aCkg
cmV0dXJuIGF4aXM7DQogICAgYXhpcyA9IF87DQogICAgcmV0dXJuIHNsaWRlcjsN
CiAgfTsNCg0KICBzbGlkZXIubWFyZ2luID0gZnVuY3Rpb24oXykgew0KICAgIGlm
ICghYXJndW1lbnRzLmxlbmd0aCkgcmV0dXJuIG1hcmdpbjsNCiAgICBtYXJnaW4g
PSBfOw0KICAgIHJldHVybiBzbGlkZXI7DQogIH07DQoNCiAgc2xpZGVyLnZhbHVl
ID0gZnVuY3Rpb24oXykgew0KICAgIGlmICghYXJndW1lbnRzLmxlbmd0aCkgcmV0
dXJuIHZhbHVlOw0KICAgIGlmICh2YWx1ZSAhPT0gdW5kZWZpbmVkKSB7DQogICAg
ICBtb3ZlSGFuZGxlKHN0ZXBWYWx1ZShfKSk7DQogICAgfTsNCiAgICB2YWx1ZSA9
IF87DQogICAgcmV0dXJuIHNsaWRlcjsNCiAgfTsNCg0KICBzbGlkZXIuc2NhbGUg
PSBmdW5jdGlvbihfKSB7DQogICAgaWYgKCFhcmd1bWVudHMubGVuZ3RoKSByZXR1
cm4gc2NhbGU7DQogICAgc2NhbGUgPSBfOw0KICAgIHJldHVybiBzbGlkZXI7DQog
IH07DQoNCiAgc2xpZGVyLmludGVydmFsID0gZnVuY3Rpb24oXykgew0KICAgIGlm
ICghYXJndW1lbnRzLmxlbmd0aCkgcmV0dXJuIGludGVydmFsOw0KICAgIGludGVy
dmFsID0gXzsNCiAgICByZXR1cm4gc2xpZGVyOw0KICB9Ow0KDQoNCiAgZDMucmVi
aW5kKHNsaWRlciwgZGlzcGF0Y2gsICJvbiIpOw0KDQogIHJldHVybiBzbGlkZXI7
DQoNCn0NCn0pKTsNCjwvc2NyaXB0Pg0KICAgICAgPCEtLSBuZHR2LWQzIGpzIGNv
ZGUgLS0+DQogICAgICA8c2NyaXB0Pi8qKgpuZHR2LWQzIGlzIGEgZDMtYmFzZWQg
SFRNTDUgbmV0d29yayBhbmltYXRpb24gcGxheWVyIGZvciB0aGUgbmR0diBwYWNr
YWdlIChodHRwOi8vY3Jhbi5yLXByb2plY3Qub3JnL3dlYi9wYWNrYWdlcy9uZHR2
L2luZGV4Lmh0bWwpCgpUaGUgbmR0di1kMyBsaWJyYXJ5IHdhcyBjcmVhdGVkIGJ5
IEdyZWcgTWljaGFsZWMgYW5kIFNreWUgQmVuZGVyLWRlTW9sbCBmb3IgdGhlIHN0
YXRuZXQgcHJvamVjdCBodHRwOi8vc3RhdG5ldC5vcmcgZnVuZGVkIGJ5IE5JQ0hE
IGdyYW50IFIwMUhEMDY4Mzk1LgoKVGhpcyBzb2Z0d2FyZSBpcyBkaXN0cmlidXRl
ZCB1bmRlciB0aGUgR1BMLTMgbGljZW5zZSAoaHR0cDovL2Nob29zZWFsaWNlbnNl
LmNvbS9saWNlbnNlcy9ncGwtMy4wLykuICBJdCBpcyBmcmVlLCBvcGVuIHNvdXJj
ZSwgYW5kIGhhcyB0aGUgYXR0cmlidXRpb24gcmVxdWlyZW1lbnRzIChHUEwgU2Vj
dGlvbiA3KSBhdCBodHRwOi8vc3RhdG5ldC5vcmcvYXR0cmlidXRpb246CgphLiB5
b3UgYWdyZWUgdG8gcmV0YWluIGluIG5kdHYtZDMgYW5kIGFueSBtb2RpZmljYXRp
b25zIHRvIG5kdHYtZDMgdGhlIGNvcHlyaWdodCwgYXV0aG9yIGF0dHJpYnV0aW9u
IGFuZCBVUkwgaW5mb3JtYXRpb24gYXMgcHJvdmlkZWQgYXQgYSBodHRwOi8vc3Rh
dG5ldHByb2plY3Qub3JnL2F0dHJpYnV0aW9uLgoKYi4geW91IGFncmVlIHRoYXQg
bmR0di1kMyBhbmQgYW55IG1vZGlmaWNhdGlvbnMgdG8gbmR0di1kMyB3aWxsLCB3
aGVuIHVzZWQsIGRpc3BsYXkgdGhlIGF0dHJpYnV0aW9uOgoKICAgIEJhc2VkIG9u
ICdzdGF0bmV0JyBwcm9qZWN0IHNvZnR3YXJlIChodHRwOi8vc3RhdG5ldHByb2pl
Y3Qub3JnKS4gRm9yIGxpY2Vuc2UgYW5kIGNpdGF0aW9uIGluZm9ybWF0aW9uIHNl
ZSBodHRwOi8vc3RhdG5ldHByb2plY3Qub3JnL2F0dHJpYnV0aW9uCgpDb3B5cmln
aHQgMjAxNCBTdGF0bmV0IENvbW1vbnMgaHR0cDovL3N0YXRuZXQub3JnCgpUbyBj
aXRlIHRoaXMgcHJvamVjdCwgcGxlYXNlIHVzZToKCkdyZWcgTWljaGFsZWMsIFNr
eWUgQmVuZGVyLWRlTW9sbCwgTWFydGluYSBNb3JyaXMgKDIwMTQpICduZHR2LWQz
OiBhbiBIVE1MNSBuZXR3b3JrIGFuaW1hdGlvbiBwbGF5ZXIgZm9yIHRoZSBuZHR2
IHBhY2thZ2UnIFRoZSBzdGF0bmV0IHByb2plY3QuIGh0dHA6Ly9zdGF0bmV0Lm9y
ZwpAbW9kdWxlCiovCgoKKGZ1bmN0aW9uIChyb290LCBmYWN0b3J5KSB7CiAgLyoq
IEBjbGFzcyAqLwogIHJvb3QubmR0dl9kMyA9IGZhY3RvcnkoKTsKfSh0aGlzLCBm
dW5jdGlvbigpIHsKICAidXNlIHN0cmljdCI7CiAKICAvKioKICAqIFB1YmxpYyBv
cHRpb25zIHRvIGNvbnRyb2wgdmlzdWFsaXphdGlvbiBmdW5jdGlvbmFsaXR5CiAg
KiBAY29uc3RhbnQge29iamVjdH0KICAqIEBnbG9iYWwKICAqIEBkZWZhdWx0CiAg
Ki8KICB2YXIgZGVmYXVsdF9vcHRpb25zID0gewogICAgYW5pbWF0aW9uRHVyYXRp
b246IDgwMCwgICAgICAgLy9EdXJhdGlvbiBvZiBlYWNoIHN0ZXAgYW5pbWF0aW9u
IGR1cmluZyBwbGF5IG9yIHN0ZXAgYWN0aW9ucywgaW4gbWlsbGlzZWNvbmRzCiAg
ICBlbnRlckV4aXRBbmltYXRpb25GYWN0b3I6IDAsICAvL1BlcmNlbnRhZ2UgKDAt
MSkgb2YgdG90YWwgc3RlcCBhbmltYXRpb24gdGltZSB0aGF0IGVudGVyL2V4aXQg
YW5pbWF0aW9ucyBzaG91bGQgdGFrZQogICAgbGFiZWxPZmZzZXQ6IHsgICAgICAg
ICAgICAgICAgLy9waXhlbCBvZmZzZXQgb2YgbGFiZWxzCiAgICAgIHg6IDEyLAog
ICAgICB5OiAwCiAgICB9LAogICAgYmFzZUZvbnRTaXplOiAnMTQnLCAgICAgICAg
ICAgLy9Gb250IHNpemUsIGluIHBpeGVscywgZm9yIGxhYmVscyB3aXRoIGNleCB2
YWx1ZSBvZiAxCiAgICBub2RlU2l6ZUZhY3RvcjogMC4wMSwgICAgICAgICAvL1Bl
cmNlbnRhZ2UgKDAtMSkgb2Ygdmlld3BvcnQgc2l6ZSB0aGF0IGEgbm9kZSBvZiBz
aXplIDEgd2lsbCBiZSByZW5kZXJlZCBhdAogICAgZGF0YUNob29zZXI6IGZhbHNl
LCAgICAgICAgICAgLy9zaG93IGEgc2VsZWN0IGJveCBmb3IgY2hvb3NpbmcgZGlm
ZmVyZW50IGdyYXBocz8KICAgIGRhdGFDaG9vc2VyRGlyOiAnZGF0YS8nLCAgICAg
IC8vd2ViIHBhdGggdG8gZGlyIGNvbnRhaW5pbmcgZGF0YSBqc29uIGZpbGVzCiAg
ICBwbGF5Q29udHJvbHM6IHRydWUsICAgICAgICAgICAvL3Nob3cgdGhlIHBsYXll
ciBjb250cm9scwogICAgc2xpZGVyOiB0cnVlLCAgICAgICAgICAgICAgICAgLy9z
aG93IHRoZSBzbGlkZXIgY29udHJvbAogICAgbWVudTogdHJ1ZSwgICAgICAgICAg
ICAgICAgICAgLy9zaG93IGEgbWVudSBpbiB1cHBlci1yaWdodAogICAgYW5pbWF0
ZU9uTG9hZDogZmFsc2UsICAgICAgICAgLy9wbGF5IHRoZSBncmFwaCBhbmltYXRp
b24gb24gcGFnZSBsb2FkCiAgICBtYXJnaW46IHsgICAgICAgICAgICAgICAgICAg
ICAvL2dyYXBoIHJlbmRlciBhcmVhIG1hcmdpbnMKICAgICAgeDogMjAsCiAgICAg
IHk6IDEwCiAgICB9LAogICAgZ3JhcGhEYXRhOiBudWxsLCAgICAgICAgICAgICAg
Ly9ncmFwaCBkYXRhLCBlaXRoZXIgYXMgSlNPTiBvYmplY3Qgb3IgVVJMIHRvIGpz
b24gZmlsZQogICAgZGVidWdGcmFtZUluZm86IGZhbHNlLCAgICAgICAgLy9TaG93
IHRoZSBzbGljZSBpbmZvIGluIGNvcm5lcgogICAgZHVyYXRpb25Db250cm9sOiB0
cnVlLCAgLy9TaG93IGEgY29udHJvbCB0byBjaGFuZ2UgZHVyYXRpb24gc3BlZWQK
ICB9OwoKICAvKioKICAqIFN1cHBvcnRlZCBORFRWIG5ldHdvcmsgcHJvcGVydGll
cyBhbmQgdGhlaXIgZGVmYXVsdCB2YWx1ZXMKICAqIEBjb25zdGFudCB7b2JqZWN0
fQogICogQGdsb2JhbAogICogQGRlZmF1bHQKICAqLwogIHZhciBuZHR2UHJvcGVy
dGllcyA9IHsKICAgIGdyYXBoOiB7CiAgICAgIHhsYWI6IG51bGwsICAgICAgICAg
ICAgICAgICAgICAgLy8gbGFiZWwgY2FwdGlvbiBiZWxvdyB0aGUgcmVuZGVyLCBv
biB0aGUgeGF4aXMKICAgICAgbWFpbjogbnVsbCwgICAgICAgICAgICAgICAgICAg
ICAvLyBtYWluIGhlYWRsaW5lIGFib3ZlIHRoZSByZW5kZXIKICAgICAgZGlzcGxh
eWxhYmVsczogZmFsc2UgLCAgICAgICAgICAvLyBzaG91bGQgdmVydGV4IGxhYmVs
cyBiZSBkaXNwbGF5ZWQKICAgICAgYmc6ICcjZmZmJywgICAgICAgICAgICAgICAg
ICAgICAvLyBiYWNrZ3JvdW5kIGNvbG9yCiAgICAgIHVzZWFycm93czogdHJ1ZSwg
ICAgICAgICAgICAgICAgLy8gc2hvdWxkIGFycm93cyBiZSBkcmF3biBvbiBlZGdl
cz8KICAgICAgeGxpbTogbnVsbCwgICAgICAgICAgICAgICAgICAgICAvLyByYW5n
ZSBvZiB4IHZhbHVlcyAgICAgICAgICAgICAgICAgICAgIAogICAgICB5bGltOiBu
dWxsLCAgICAgICAgICAgICAgICAgICAgIC8vIHJhbmdlIG9mIHkgdmFsdWVzICAK
ICAgICAgZWRnZU9mZnNldDogMCwgICAgICAgICAgICAgICAgICAvLyBvZmZzZXQg
b2YgZWRnZSBsZW5ndGggdG8gdmVydGV4IGJvcmRlcnMgIAogICAgICB0b29sdGlw
T2Zmc2V0OiAwLCAgICAgICAgICAgICAgIC8vIG9mZnNldCBvZiB0b29sdGlwcyB0
byB2ZXJ0ZXggYm9yZGVycyAKICAgIH0sIAogICAgbm9kZTogewogICAgICBjb29y
ZDogbnVsbCwgICAgICAgICAgICAgICAgICAgIC8vIGNvb3JkaW5hdGVzIGZvciBu
b2RlcwogICAgICBsYWJlbDogbnVsbCwgICAgICAgICAgICAgICAgICAgIC8vIGxh
YmVscyBmb3IgdmVydGljZXMKICAgICAgJ2xhYmVsLmNvbCc6ICcjMDAwJywgICAg
ICAgICAgICAvLyBjb2xvciBvZiBub2RlIGxhYmVsCiAgICAgICdsYWJlbC5jZXgn
OiAxLCAgICAgICAgICAgICAgICAgLy8gbGFiZWwgZm9udCBzaXplIHNjYWxlIGZh
Y3RvcgogICAgICAndmVydGV4LmNleCc6IDEsICAgICAgICAgICAgICAgIC8vIHZl
cnRleCAobm9kZSkgZXhwYW5zaW9uIHNjYWxlIGZhY3RvcgogICAgICAndmVydGV4
LmNvbCc6ICcjRjAwJywgICAgICAgICAgIC8vIG5vZGUgZmlsbCBjb2xvcgogICAg
ICAndmVydGV4LnNpZGVzJzogNTAsICAgICAgICAgICAgIC8vIG51bWJlciBvZiBz
aWRlcyBmb3IgdmVydGV4IHBvbHlnb24gKHNoYXBlKQogICAgICAndmVydGV4LnJv
dCc6IDAsICAgICAgICAgICAgICAgIC8vIHJvdGF0aW9uIGZvciB2ZXJ0ZXggcG9s
eWdvbgogICAgICAndmVydGV4LnRvb2x0aXAnOiAnJywgICAgICAgICAgIC8vIHZl
cnRleCB0b29sdGlwIHZhbHVlCiAgICAgICd2ZXJ0ZXguYm9yZGVyJzogJyMwMDAn
LCAgICAgICAgLy8gY29sb3Igb2YgdmVydGV4IGJvcmRlciBzdHJva2UKICAgICAg
J3ZlcnRleC5sd2QnOiAxLCAgICAgICAgICAgICAgICAvLyB3aWR0aCBvZiB2ZXJ0
ZXggYm9yZGVyIHN0cm9rZQogICAgICAndmVydGV4LmNzcy5jbGFzcyc6IG51bGws
ICAgICAgIC8vIGNzcyBjbGFzcyBuYW1lIGFwcGxpZWQgdG8gbm9kZQogICAgICAn
dmVydGV4LmxhYmVsLmNzcy5jbGFzcyc6IG51bGwsIC8vIGNzcyBjbGFzcyBuYW1l
IGFwcGxpZWQgdG8gbm9kZSBsYWJlbAogICAgICAndmVydGV4LmNzcy5zdHlsZSc6
IG51bGwsICAgICAgIC8vIGNzcyBpbmxpbmUtc3R5bGUgYXBwbGllZCB0byBub2Rl
IChVTklNUExJTUVOVEVEKQogICAgICAndmVydGV4LmxhYmVsLmNzcy5zdHlsZSc6
IG51bGwsIC8vIGNzcyBpbmxpbmUgc3R5bGUgYXBwbGllZCB0byBub2RlIGxhYmVs
IChVTklNUExFTUVOVEVEKQogICAgICAnaW1hZ2UnOiBudWxsLCAgICAgICAgICAg
ICAgICAgIC8vIGJhY2tncm91bmQgaW1hZ2UgZm9yIHZlcnRleAogICAgfSwKICAg
IGVkZ2U6IHsKICAgICAgJ2VkZ2UubHdkJzogMSwgICAgICAgICAgICAgICAgICAv
LyB3aWR0aCBvZiBlZGdlIHN0cm9rZQogICAgICAnZWRnZS5jb2wnOiAnIzAwMCcs
ICAgICAgICAgICAgIC8vIGVkZ2Ugc3Ryb2tlIGNvbG9yCiAgICAgICdlZGdlLnRv
b2x0aXAnOiBudWxsLCAgICAgICAgICAgLy8gZWRnZSB0b29sdGlwIHZhbHVlCiAg
ICAgICdlZGdlLmNzcy5jbGFzcyc6IG51bGwsICAgICAgICAgLy8gY3NzIGNsYXNz
IG5hbWUgYXBwbGllZCB0byBlZGdlCiAgICAgICdlZGdlLmxhYmVsLmNzcy5jbGFz
cyc6IG51bGwsICAgLy8gY3NzIGNsYXNzIG5hbWUgYXBwbGllZCB0byBlZGdlIGxh
YmVsCiAgICAgICdlZGdlLmNzcy5zdHlsZSc6IG51bGwsICAgICAgICAgLy8gY3Nz
IGlubGluZS1zdHlsZSBhcHBsaWVkIHRvIGVkZ2UgKFVOSU1QTElNRU5URUQpCiAg
ICAgICdlZGdlLmxhYmVsLmNzcy5zdHlsZSc6IG51bGwsICAgLy8gY3NzIGlubGlu
ZSBzdHlsZSBhcHBsaWVkIHRvIGVkZ2UgbGFiZWwgKFVOSU1QTEVNRU5URUQpCiAg
ICB9CiAgfQogIAoKICAvKioKICAqIEluaXRpYWxpemUgYSBuZXcgbmR0di1kMyBp
bnN0YW5jZQogICogQGNvbnN0cnVjdHMgbmR0dl9kMwogICogQHBhcmFtIHtvYmpl
Y3R9IC0gQW4gb2JqZWN0IG9mIGRlZmF1bHQgb3B0aW9ucyBvdmVycmlkZXMKICAq
IEBwYXJhbSB7c3RyaW5nfEhUTUxFbGVtZW50fSAtIEEgQ1NTIHNlbGVjdG9yIHN0
cmluZyBvciBET00gZWxlbWVudCByZWZlcmVuY2Ugc3BlY2lmeWluZyB0aGUgdGFy
Z2V0IGRvbSBlbGVtZW50IHRoZSBuZXR3b3JrIHNob3VsZCBiZSBpbml0aWFsaXpl
ZCB0bwogICovCiAgdmFyIG4zID0gZnVuY3Rpb24ob3B0aW9ucywgdGFyZ2V0KSB7
CiAgICB2YXIgbjMgPSB0aGlzOwogICAgCiAgICB2YXIgZ2xvYmFscyA9IHsKICAg
ICAgc3ZnOiBudWxsLAogICAgICB4U2NhbGU6IG51bGwsCiAgICAgIHlTY2FsZTog
bnVsbCwKICAgICAgbWluVGltZTogbnVsbCwKICAgICAgaW50ZXJ2YWw6IG51bGws
CiAgICAgIG1heFRpbWU6IG51bGwsCiAgICAgIGFuaW1hdGU6IG51bGwsCiAgICAg
IGJhc2VOb2RlU2l6ZTogbnVsbCwKICAgICAgY3VyclRpbWU6IDAsCiAgICAgIGdy
YXBoOiBudWxsLAogICAgICB0aW1lSW5kZXg6bnVsbCwKICAgICAgZG9tVGFyZ2V0
Om51bGwsCiAgICAgIHNsaWRlcjpudWxsLAogICAgICBub2RlQ29vcmRzOiB7fSwK
ICAgICAgb3B0aW9uczoge30KICAgIAp9CiAgICAvL2luaXRpYWxpemUgY2xhc3Mg
Z2xvYmFscwogICAgJC5leHRlbmQodHJ1ZSwgbjMsIGdsb2JhbHMpOwoKICAgIC8v
cmVwbGFjZSBkZWZhdWx0cyB3aXRoIHVzZXItc3BlY2lmaWVkIG9wdGlvbnMKICAg
ICQuZXh0ZW5kKHRydWUsIG4zLm9wdGlvbnMsIGRlZmF1bHRfb3B0aW9ucyk7CiAg
ICAkLmV4dGVuZCh0cnVlLCBuMy5vcHRpb25zLCBvcHRpb25zKTsKCiAgICBpZiAo
IXRhcmdldCkgewogICAgICB0YXJnZXQgPSBkMy5zZWxlY3QoJ2JvZHknKS5hcHBl
bmQoJ2RpdicpLnN0eWxlKHt3aWR0aDogJzEwMCUnLCBoZWlnaHQ6ICcxMDAlJ30p
Lm5vZGUoKTsKICAgICAgZDMuc2VsZWN0QWxsKCdodG1sLCBib2R5JykuY2xhc3Nl
ZCh7J25kdHYtZnVsbHNjcmVlbic6IHRydWV9KQogICAgfQogICAgbjMuZG9tVGFy
Z2V0ID0gZDMuc2VsZWN0KHRhcmdldCk7CiAgICBuMy5kb21UYXJnZXQuY2xhc3Nl
ZCh7J25kdHYtZDMtY29udGFpbmVyJzogdHJ1ZX0pOwogICAgbjMuU1ZHU2V0dXAo
bjMuZG9tVGFyZ2V0KTsKICAgIGlmIChuMy5vcHRpb25zLnBsYXlDb250cm9scyB8
fCBuMy5vcHRpb25zLnNsaWRlcikgewogICAgICBuMy5kb21UYXJnZXQuYXBwZW5k
KCdkaXYnKS5hdHRyKCdjbGFzcycsICdjb250cm9scycpOwogICAgfQogICAgaWYg
KG4zLm9wdGlvbnMuZGF0YUNob29zZXIpIHsgbjMuY3JlYXRlRGF0YUNob29zZXIo
KTsgfQogICAgaWYgKG4zLm9wdGlvbnMucGxheUNvbnRyb2xzKSB7IG4zLmNyZWF0
ZVBsYXlDb250cm9scygpOyB9CiAgICBpZiAobjMub3B0aW9ucy5zbGlkZXIpIHsg
bjMuY3JlYXRlU2xpZGVyQ29udHJvbCgpOyB9CiAgICBpZiAobjMub3B0aW9ucy5t
ZW51KSB7IG4zLmNyZWF0ZU1lbnUoKTsgfQoKCiAgICBuMy50b29sdGlwID0gbjMu
ZG9tVGFyZ2V0LnNlbGVjdCgnLmdyYXBoJykuYXBwZW5kKCdkaXYnKS5hdHRyKCdj
bGFzcycsICd0b29sdGlwJyk7CiAgICBuMy5mcmFtZUluZm9EaXYgPSBuMy5kb21U
YXJnZXQuc2VsZWN0KCcuZ3JhcGgnKS5hcHBlbmQoJ2RpdicpLmF0dHIoJ2NsYXNz
JywgJ2ZyYW1lSW5mbycpCiAgICBpZiAobjMub3B0aW9ucy5kZWJ1Z0ZyYW1lSW5m
bykgeyBuMy5mcmFtZUluZm9EaXYuc3R5bGUoJ2Rpc3BsYXknLCAnYmxvY2snKTsg
fQogICAgaWYobjMub3B0aW9ucy5ncmFwaERhdGEpIHsgbjMubG9hZERhdGEobjMu
b3B0aW9ucy5ncmFwaERhdGEpOyB9CiAgfQoKICAvKioKICAqIEluaXRpYWxpemUg
dGhlIFNWRyBlbGVtZW50IGFuZCByZWxhdGVkIERPTSBlbGVtZW50cyBhbmQgbGlz
dGVuZXJzCiAgKiBAcGFyYW0ge0QzU2VsZWN0aW9ufSAtIERPTSBlbGVtZW50IHRv
IGluc2VydCBzdmcgaW50bwogICovCiAgbjMucHJvdG90eXBlLlNWR1NldHVwID0g
ZnVuY3Rpb24oZG9tVGFyZ2V0KSB7CiAgICB2YXIgbjMgPSB0aGlzOwoKICAgICQo
ZG9tVGFyZ2V0KS5yZXNpemUoZnVuY3Rpb24obikgeyAKICAgICAgbjMucmVzaXpl
R3JhcGgobik7CiAgICB9KTsKICAgICQod2luZG93KS5yZXNpemUoZnVuY3Rpb24o
bikgeyAKICAgICAgbjMucmVzaXplR3JhcGgobik7CiAgICB9KTsKIAogICAgZG9t
VGFyZ2V0CiAgICAgIC5hcHBlbmQoJ2RpdicpLmF0dHIoJ2NsYXNzJywgJ2dyYXBo
JykKICAgICAgLmFwcGVuZCgic3ZnOnN2ZyIpCiAgICAgIC5hcHBlbmQoImRlZnMi
KQoKICAgIHZhciBzdmcgPSBkb21UYXJnZXQuc2VsZWN0KCdzdmcnKQogICAgICAu
YXBwZW5kKCdnJykKCiAgICB2YXIgZHJhZ0V2ZW50OwogICAgdmFyIHJlY3QgPSBz
dmcuYXBwZW5kKCJyZWN0IikKICAgICAgLmF0dHIoJ2NsYXNzJywgJ2JhY2tncm91
bmQnKQogICAgICAuc3R5bGUoImZpbGwiLCAibm9uZSIpCiAgICAgIC5zdHlsZSgi
cG9pbnRlci1ldmVudHMiLCAiYWxsIikKICAgICAgLm9uKCdtb3VzZWRvd24nLCBm
dW5jdGlvbigpIHsgCiAgICAgICAgZHJhZ0V2ZW50ID0gZDMuZXZlbnQ7CiAgICAg
IH0pCiAgICAgIC5vbignbW91c2V1cCcsIGZ1bmN0aW9uKCkgeyAKICAgICAgICBp
ZiAoTWF0aC5hYnMoZHJhZ0V2ZW50LnBhZ2VYIC0gZDMuZXZlbnQucGFnZVgpIDwg
NSAmJiBNYXRoLmFicyhkcmFnRXZlbnQucGFnZVkgLSBkMy5ldmVudC5wYWdlWSkg
PCA1KSB7CiAgICAgICAgICBuMy5oaWRlVG9vbHRpcCgpOwogICAgICAgICAgbjMu
dW5TZWxlY3ROZXR3b3JrKCk7CiAgICAgICAgfQogICAgICB9KQoKICAgIG4zLmNv
bnRhaW5lciA9IHN2Zy5hcHBlbmQoImciKQogICAgICAuYXR0cignY2xhc3MnLCAn
Y29udGFpbmVyJykKICAgIG4zLmNvbnRhaW5lci5hcHBlbmQoJ2cnKS5hdHRyKCdj
bGFzcycsICdlZGdlcycpOwogICAgbjMuY29udGFpbmVyLmFwcGVuZCgnZycpLmF0
dHIoJ2NsYXNzJywgJ25vZGVzJyk7CiAgICBuMy5jb250YWluZXIuYXBwZW5kKCdn
JykuYXR0cignY2xhc3MnLCAnbGFiZWxzJyk7CiAgICBuMy5jb250YWluZXIuYXBw
ZW5kKCdyZWN0JykuYXR0cignY2xhc3MnLCAnc2NyZWVuJyk7CiAgICBuMy5jb250
YWluZXIuYXBwZW5kKCdnJykuYXR0cignY2xhc3MnLCAnZWRnZXNfc2VsZWN0ZWQn
KTsKICAgIG4zLmNvbnRhaW5lci5hcHBlbmQoJ2cnKS5hdHRyKCdjbGFzcycsICdu
b2Rlc19zZWxlY3RlZCcpOwogICAgbjMuY29udGFpbmVyLmFwcGVuZCgnZycpLmF0
dHIoJ2NsYXNzJywgJ2xhYmVsc19zZWxlY3RlZCcpOwoKICAgIHN2Zy5hcHBlbmQo
J2cnKS5hdHRyKCdjbGFzcycsICdtYWluJykuYXBwZW5kKCd0ZXh0Jyk7CiAgICBz
dmcuYXBwZW5kKCdnJykuYXR0cignY2xhc3MnLCAneGxhYicpLmFwcGVuZCgndGV4
dCcpOwoKICAgIHN2Zy5vbignbW91c2Vkb3duJywgZnVuY3Rpb24oKSB7CiAgICAg
IHN2Zy5jbGFzc2VkKHsnZHJhZ2dpbmcnOiB0cnVlfSkKICAgIH0pCiAgICBzdmcu
b24oJ21vdXNldXAnLCBmdW5jdGlvbigpIHsKICAgICAgc3ZnLmNsYXNzZWQoeydk
cmFnZ2luZyc6IGZhbHNlfSkKICAgIH0pCgogICAgbjMuem9vbSA9IGQzLmJlaGF2
aW9yLnpvb20oKQogICAgICAuc2NhbGVFeHRlbnQoWy41LCAxMF0pCiAgICAgIC5v
bigiem9vbSIsIGZ1bmN0aW9uIHpvb21lZCgpIHsKICAgICAgICBuMy5jb250YWlu
ZXIuYXR0cigidHJhbnNmb3JtIiwgInRyYW5zbGF0ZSgiICsgZDMuZXZlbnQudHJh
bnNsYXRlICsgIilzY2FsZSgiICsgZDMuZXZlbnQuc2NhbGUgKyAiKSIpOwogICAg
ICAgIG4zLmN0bSA9IG4zLmNvbnRhaW5lci5ub2RlKCkuZ2V0U2NyZWVuQ1RNKCk7
CiAgICAgICAgbjMubW92ZVRvb2x0aXAoKTsKICAgICAgfSkKICAgIHN2Zy5jYWxs
KG4zLnpvb20pCiAgfQoKICAvKiogc2V0cyBwb3NpdGlvbmluZyBvbiBzdmcgZWxl
bWVudHMgYmFzZWQgb24gY3VycmVudCBET00gY29udGFpbmVyIHNpemUgYW5kIHNl
dHMgZGF0YSBzY2FsaW5nIGZhY3RvcnMgYWNjb3JkaW5nbHkgKi8KICBuMy5wcm90
b3R5cGUudXBkYXRlRGltZW5zaW9ucyA9IGZ1bmN0aW9uKCkgewogICAgdmFyIG4z
ID0gdGhpczsKICAgIHZhciBkaXZfd2lkdGggPSBuMy5kb21UYXJnZXQubm9kZSgp
Lm9mZnNldFdpZHRoCiAgICB2YXIgZGl2X2hlaWdodCA9IG4zLmRvbVRhcmdldC5u
b2RlKCkub2Zmc2V0SGVpZ2h0IC0gJChuMy5kb21UYXJnZXQuc2VsZWN0KCcuY29u
dHJvbHMnKS5ub2RlKCkpLm91dGVySGVpZ2h0KHRydWUpOwoKICAgIHZhciB4bGFi
ID0gbjMudGltZUluZGV4W24zLmN1cnJUaW1lXS5yZW5kZXJEYXRhLmdyYXBoLnhs
YWI7CiAgICB2YXIgbWFpbiA9IG4zLnRpbWVJbmRleFtuMy5jdXJyVGltZV0ucmVu
ZGVyRGF0YS5ncmFwaC5tYWluOwogICAgdmFyIHhsYWJTaXplID0gcGFyc2VGbG9h
dCh3aW5kb3cuZ2V0Q29tcHV0ZWRTdHlsZShuMy5kb21UYXJnZXQuc2VsZWN0KCcu
eGxhYicpLm5vZGUoKSwgbnVsbCkuZ2V0UHJvcGVydHlWYWx1ZSgnZm9udC1zaXpl
JykpOwogICAgdmFyIG1haW5TaXplID0gcGFyc2VGbG9hdCh3aW5kb3cuZ2V0Q29t
cHV0ZWRTdHlsZShuMy5kb21UYXJnZXQuc2VsZWN0KCcubWFpbicpLm5vZGUoKSwg
bnVsbCkuZ2V0UHJvcGVydHlWYWx1ZSgnZm9udC1zaXplJykpOwogICAgdmFyIG1h
aW5NYXJnaW4gPSAwOwogICAgdmFyIHhsYWJNYXJnaW4gPSAwOwoKICAgIGlmICh4
bGFiKSB7CiAgICAgIHhsYWJNYXJnaW4gPSB4bGFiU2l6ZSooeGxhYi5sZW5ndGgr
MSkqMS4yOwogICAgfSAKICAgIGlmIChtYWluKSB7CiAgICAgIG1haW5NYXJnaW4g
PSBtYWluU2l6ZSoobWFpbi5sZW5ndGgrMSkqMS4yOwogICAgfSAKICAgIAogICAg
dmFyIG1hcmdpbiA9IHsKICAgICAgeDogbjMub3B0aW9ucy5tYXJnaW4ueCwKICAg
ICAgeTogbjMub3B0aW9ucy5tYXJnaW4ueQogICAgfQogICAgaWYgKGRpdl93aWR0
aCA+IGRpdl9oZWlnaHQpIHsgCiAgICAgIG1hcmdpbi54ID0gKGRpdl93aWR0aCAt
IGRpdl9oZWlnaHQpLzIKICAgIH0gZWxzZSB7CiAgICAgIG1hcmdpbi55ID0gKGRp
dl9oZWlnaHQgLSBkaXZfd2lkdGgpLzIKICAgIH0KCiAgICB2YXIgd2lkdGggPSBk
aXZfd2lkdGggLSAobWFyZ2luLngqMik7CiAgICB2YXIgaGVpZ2h0ID0gZGl2X2hl
aWdodCAtIChtYXJnaW4ueSoyKTsKCiAgICBuMy5kb21UYXJnZXQuc2VsZWN0QWxs
KCcuZ3JhcGg+c3ZnLCAuYmFja2dyb3VuZCwgLnNjcmVlbicpCiAgICAgIC5hdHRy
KHsKICAgICAgICB3aWR0aDogd2lkdGggKyBtYXJnaW4ueCAqIDIsCiAgICAgICAg
aGVpZ2h0OiBoZWlnaHQgKyBtYXJnaW4ueSAqIDIKICAgICAgfSkKICAgIAogICAg
Ly9yZXNldCBoZWlnaHQgaW5jbHVkaW5nIG1haW4gJiB4bGFiIGZvciBncmFwaCBj
b250YWluZXIgdHJhbnNsYXRpb24KICAgIGhlaWdodCA9IGhlaWdodCAtIG1haW5N
YXJnaW4gLSB4bGFiTWFyZ2luOwoKICAgIG4zLmNvbnRhaW5lci5hdHRyKCJ0cmFu
c2Zvcm0iLCAidHJhbnNsYXRlKCIgKyBtYXJnaW4ueCArICIsIiArIChtYXJnaW4u
eSttYWluTWFyZ2luKSArICIpIik7CgogICAgdmFyIGNlbnRlciA9IG1hcmdpbi54
ICsgd2lkdGgvMjsKICAgIG4zLmRvbVRhcmdldC5zZWxlY3QoJy54bGFiJykuYXR0
cigndHJhbnNmb3JtJywgInRyYW5zbGF0ZSgiK2NlbnRlcisiLCIrKGRpdl9oZWln
aHQtbWFyZ2luLnkpKyIpIikKICAgIG4zLmRvbVRhcmdldC5zZWxlY3QoJy5tYWlu
JykuYXR0cigndHJhbnNmb3JtJywgInRyYW5zbGF0ZSgiK2NlbnRlcisiLCIrKG1h
cmdpbi55K21haW5TaXplKSsiKSIpCgogICAgdmFyIHBpeGVsU3BhY2UgPSBoZWln
aHQgPiB3aWR0aCA/IHdpZHRoIDogaGVpZ2h0OwogICAgbjMuYmFzZU5vZGVTaXpl
ID0gcGl4ZWxTcGFjZSAqIG4zLm9wdGlvbnMubm9kZVNpemVGYWN0b3I7CgogICAg
Ly9zZXQgdGhlIFggYW5kIFkgc2NhbGVzCiAgICBuMy54U2NhbGUgPSBkMy5zY2Fs
ZS5saW5lYXIoKQogICAgICAuZG9tYWluKFtuMy50aW1lSW5kZXhbMF0ucmVuZGVy
RGF0YS5ncmFwaC54bGltWzBdLG4zLnRpbWVJbmRleFswXS5yZW5kZXJEYXRhLmdy
YXBoLnhsaW1bMV1dKQogICAgICAucmFuZ2UoWzAsIHBpeGVsU3BhY2VdKTsKCiAg
ICBuMy55U2NhbGUgPSBkMy5zY2FsZS5saW5lYXIoKQogICAgICAuZG9tYWluKFtu
My50aW1lSW5kZXhbMF0ucmVuZGVyRGF0YS5ncmFwaC55bGltWzBdLG4zLnRpbWVJ
bmRleFswXS5yZW5kZXJEYXRhLmdyYXBoLnlsaW1bMV1dKQogICAgICAucmFuZ2Uo
W3BpeGVsU3BhY2UsIDBdKTsKCiAgICAvL3Jlc2V0IHpvb20gdHJhbnNsYXRlIGJh
c2VkIG9uIG1hcmdpbnMKICAgIG4zLnpvb20udHJhbnNsYXRlKFttYXJnaW4ueCwg
bWFyZ2luLnkrbWFpbk1hcmdpbl0pCgogICAgLy9DYWNoZSBoZWlnaHQgYW5kIG9m
ZnNldCB0byB1c2UgZm9yIHRvb2x0aXAgbW92ZW1lbnQKICAgIG4zLmhlaWdodCA9
IG4zLmRvbVRhcmdldC5zZWxlY3QoJy5ncmFwaCcpLm5vZGUoKS5vZmZzZXRIZWln
aHQKICAgIG4zLm9mZnNldCA9ICQobjMuZG9tVGFyZ2V0LnNlbGVjdCgnLmdyYXBo
Jykubm9kZSgpKS5vZmZzZXQoKTsKICAgIG4zLmN0bSA9IG4zLmNvbnRhaW5lci5u
b2RlKCkuZ2V0U2NyZWVuQ1RNKCk7CiAgfQogIAogIC8qKiBjcmVhdGVzIHRoZSBv
cHRpb25hbCBkYXRhQ2hvb3NlciBlbGVtZW50IHRvIGJlIHVzZWQgZm9yIHNsZWN0
aW5nIGFtb25nIG11bHRpcGxlIEpTT04gZmlsZXMgZm9yIGRlYnVnZ2luZyAqLwog
IG4zLnByb3RvdHlwZS5jcmVhdGVEYXRhQ2hvb3NlciA9IGZ1bmN0aW9uKCkgewog
ICAgdmFyIG4zID0gdGhpczsKCiAgICB2YXIgZGl2ID0gbjMuZG9tVGFyZ2V0LmFw
cGVuZCgnZGl2JykuYXR0cignY2xhc3MnLCAnZGF0YV9jaG9vc2VyX2NvbnRhaW5l
cicpCiAgICBkaXYuYXBwZW5kKCdzZWxlY3QnKS5hdHRyKCdjbGFzcycsICdkYXRh
X2Nob29zZXInKQogICAgZGl2LmFwcGVuZCgnYScpLmF0dHIoeydjbGFzcyc6ICd2
aWRlb19saW5rJywgJ3RhcmdldCc6ICdfYmxhbmsnfSkuaHRtbCgnVmlkZW8nKTsK
CiAgICB2YXIgc2V0VmlkTGluayA9IGZ1bmN0aW9uKHVybCkgewogICAgICBkaXYu
c2VsZWN0KCcudmlkZW9fbGluaycpLmF0dHIoJ2hyZWYnLCB1cmwucmVwbGFjZSgn
Lmpzb24nLCAnLm1wNCcpKQogICAgfQogICAgJC5nZXQobjMub3B0aW9ucy5kYXRh
Q2hvb3NlckRpciwgZnVuY3Rpb24oZGF0YSl7CiAgICAgIGRpdi5zZWxlY3QoJy5k
YXRhX2Nob29zZXInKS5vbignY2hhbmdlJywgZnVuY3Rpb24oKSB7CiAgICAgICAg
dmFyIHVybCA9ICQodGhpcykudmFsKCk7CiAgICAgICAgbjMubG9hZERhdGEodXJs
KTsKICAgICAgICBzZXRWaWRMaW5rKHVybCkKICAgICAgfSkKICAgICAgdmFyIG1h
dGNoZXMgPSBkYXRhLm1hdGNoKC88dGQ+PGEgaHJlZj0iW14iXSoiL2cpOwogICAg
ICAkLmVhY2gobWF0Y2hlcywgZnVuY3Rpb24oaSwgbSkgewogICAgICAgIHZhciB1
cmwgPSBtLm1hdGNoKC9ocmVmPSIoW14iXSopIi8pWzFdOwogICAgICAgIGlmICh1
cmwubWF0Y2goLy5qc29uJC8pKSB7CiAgICAgICAgICBkaXYuc2VsZWN0KCcuZGF0
YV9jaG9vc2VyJykuYXBwZW5kKCdvcHRpb24nKS5hdHRyKCd2YWx1ZScsIG4zLm9w
dGlvbnMuZGF0YUNob29zZXJEaXIrdXJsKS5odG1sKHVybCk7CiAgICAgICAgfQog
ICAgICAgIGlmIChpID09IDEpIHsKICAgICAgICAgIHNldFZpZExpbmsodXJsKTsK
ICAgICAgICB9CiAgICAgIH0pCiAgICB9KQogIH0KICAKICAvKiogY3JlYXRlcyB0
aGUgb3B0aW9uYWwgbWVudSBlbGVtZW50IHRvIGJlIHVzZWQgZm9yIGNvbnRyb2xs
aW5nIHNldHRpbmdzIGFuZCBkaXNwbGF5aW5nICdhYm91dCcgbGluayAqLwogIG4z
LnByb3RvdHlwZS5jcmVhdGVNZW51ID0gZnVuY3Rpb24oKSB7CiAgICB2YXIgbjMg
PSB0aGlzOwogICAgaWYgKGQzLnNlbGVjdCgnI25kdHYtc3ZnLW1lbnUtaWNvbnMn
KS5lbXB0eSgpKSB7CiAgICAgICQoJ2JvZHknKS5wcmVwZW5kKAogICAgICAnPHN2
ZyBpZD0ibmR0di1zdmctbWVudS1pY29ucyIgZGlzcGxheT0ibm9uZSIgdmVyc2lv
bj0iMS4xIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5z
OnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTc2
IiBoZWlnaHQ9IjMyIiB2aWV3Qm94PSIwIDAgMTc2IDMyIj4nKwogICAgICAnICA8
ZGVmcz4nKwogICAgICAnICAgIDxnIGlkPSJpY29uLWxpc3QiPjxwYXRoIGQ9Ik0y
NS42IDE0LjRoLTE5LjJjLTAuODgzIDAtMS42IDAuNzE3LTEuNiAxLjZzMC43MTcg
MS42IDEuNiAxLjZoMTkuMmMwLjg4NSAwIDEuNi0wLjcxNyAxLjYtMS42cy0wLjcx
NS0xLjYtMS42LTEuNnpNNi40IDExLjJoMTkuMmMwLjg4NSAwIDEuNi0wLjcxNyAx
LjYtMS42cy0wLjcxNS0xLjYtMS42LTEuNmgtMTkuMmMtMC44ODMgMC0xLjYgMC43
MTctMS42IDEuNnMwLjcxNyAxLjYgMS42IDEuNnpNMjUuNiAyMC44aC0xOS4yYy0w
Ljg4MyAwLTEuNiAwLjcxNS0xLjYgMS42czAuNzE3IDEuNiAxLjYgMS42aDE5LjJj
MC44ODUgMCAxLjYtMC43MTUgMS42LTEuNnMtMC43MTUtMS42LTEuNi0xLjZ6Ij48
L3BhdGg+PC9nPicrCiAgICAgICcgIDwvZGVmcz4nKwogICAgICAnPC9zdmc+Jyk7
CiAgICB9CiAgICB2YXIgZGl2ID0gbjMuZG9tVGFyZ2V0LnNlbGVjdCgnLmdyYXBo
JykuYXBwZW5kKCdkaXYnKS5hdHRyKCdjbGFzcycsICduZHR2LW1lbnUtY29udGFp
bmVyJyk7CiAgICBkaXYuaHRtbCgKICAgICAgIjxkaXYgY2xhc3M9J25kdHYtbWVu
dS1pY29uJz4iKwogICAgICAiIDxzdmcgY2xhc3M9J2ljb24gbWVudS1jb250cm9s
JyB2aWV3Qm94PScwIDAgMzIgMzInPjx1c2UgeGxpbms6aHJlZj0nI2ljb24tbGlz
dCc+PC91c2U+PC9zdmc+IisKICAgICAgIjwvZGl2PiIrCiAgICAgICI8ZGl2IGNs
YXNzPSduZHR2LW1lbnUnPjwvZGl2PiIKICAgICkKICAgIHZhciBtZW51ID0gbjMu
ZG9tVGFyZ2V0LnNlbGVjdCgnLm5kdHYtbWVudScpOwoKICAgIGlmIChuMy5vcHRp
b25zLmR1cmF0aW9uQ29udHJvbCkgeyAKICAgICAgdmFyIGR1cmF0aW9uQ29udHJv
bCA9IG1lbnUuYXBwZW5kKCdkaXYnKS5hdHRyKCdjbGFzcycsICdtZW51LWl0ZW0g
ZHVyYXRpb25Db250cm9sQ29udGFpbmVyJyk7CiAgICAgIGR1cmF0aW9uQ29udHJv
bC5hcHBlbmQoJ3NwYW4nKS5hdHRyKCdjbGFzcycsICdtZW51LWxhYmVsJykuaHRt
bCgnQW5pbWF0aW9uIER1cmF0aW9uJyk7CiAgICAgIHZhciBkdXJhdGlvblNsaWRl
ciA9IGQzLnNsaWRlcigpLm1pbigwKS5tYXgoOCkuYXhpcyhuZXcgZDMuc3ZnLmF4
aXMoKS50aWNrcyg1KSkudmFsdWUobjMub3B0aW9ucy5hbmltYXRpb25EdXJhdGlv
bi8xMDAwKTsKICAgICAgZHVyYXRpb25Db250cm9sLmFwcGVuZCgnZGl2JykuYXR0
cignY2xhc3MnLCAnZHVyYXRpb25Db250cm9sJykuY2FsbChkdXJhdGlvblNsaWRl
cikKICAgICAgZHVyYXRpb25TbGlkZXIub24oJ3NsaWRlJywgZnVuY3Rpb24oZXZ0
LCB2YWx1ZSl7CiAgICAgICAgbjMub3B0aW9ucy5hbmltYXRpb25EdXJhdGlvbiA9
IHZhbHVlKjEwMDA7CiAgICAgICAgaWYobjMub3B0aW9ucy5zbGlkZXIpIHsKICAg
ICAgICAgIG4zLnNsaWRlci5hbmltYXRlKG4zLm9wdGlvbnMuYW5pbWF0aW9uRHVy
YXRpb24pCiAgICAgICAgfQogICAgICB9KQogICAgfQoKICAgIG1lbnUuYXBwZW5k
KCJkaXYiKS5hdHRyKCdjbGFzcycsICdtZW51LWl0ZW0nKS5odG1sKCI8YSBocmVm
PSdodHRwczovL2dpdGh1Yi5jb20vbWljaGFsZ20vbmR0di1kMy9ibG9iL21hc3Rl
ci9SRUFETUUubWQnIHRhcmdldD0nX2JsYW5rJz5BYm91dCBORFRWLUQzPC9hPjwv
ZGl2PiIpOwogICAgbjMuZG9tVGFyZ2V0LnNlbGVjdCgnLm5kdHYtbWVudS1pY29u
Jykub24oJ2NsaWNrJywgZnVuY3Rpb24oKSB7CiAgICAgICQobWVudS5ub2RlKCkp
LmZhZGVUb2dnbGUoMjAwKTsKICAgICAgJCh0aGlzKS50b2dnbGVDbGFzcygnbWVu
dS1hY3RpdmUnKQogICAgfSkKICB9CgogIC8qKiBjcmVhdGVzIHRoZSBvcHRpb25h
bCBwbGF5IGNvbnRyb2xzIGRpdiB1c2luZyBzdmcgaWNvbnMgYW5kIGRlZmluZXMg
dGhlIGF0dGFjaGVkIGV2ZW50cyAqLwogIG4zLnByb3RvdHlwZS5jcmVhdGVQbGF5
Q29udHJvbHMgPSBmdW5jdGlvbigpIHsKICAgIHZhciBuMyA9IHRoaXM7CgogICAg
Ly9kZWZpbmUgU1ZHIGljb25zIHRvIGJlIHVzZWQgaW4gdGhlIHBsYXkgY29udHJv
bGxlcgogICAgaWYgKGQzLnNlbGVjdCgnI25kdHYtc3ZnLWljb25zJykuZW1wdHko
KSkgewogICAgICAkKCdib2R5JykucHJlcGVuZCgKICAgICAgJzxzdmcgaWQ9Im5k
dHYtc3ZnLWljb25zIiBkaXNwbGF5PSJub25lIiB2ZXJzaW9uPSIxLjEiIHhtbG5z
PSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6
Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHdpZHRoPSIxNzYiIGhlaWdodD0iMzIi
IHZpZXdCb3g9IjAgMCAxNzYgMzIiPicrCiAgICAgICcgIDxkZWZzPicrCiAgICAg
ICcgICAgPGcgaWQ9Imljb24tcGxheSI+PHBhdGggY2xhc3M9InBhdGgxIiBkPSJN
MjYuNzE3IDE1LjE3OWwtMTMuNjk4LTguNDg2Yy0wLjk5OC0wLjY1NC0xLjgxNC0w
LjE3MS0xLjgxNCAxLjA3MnYxNi40NzRjMCAxLjI0MyAwLjgxOCAxLjcyNSAxLjgx
NCAxLjA3MGwxMy42OTktOC40ODZjMCAwIDAuNDg2LTAuMzQyIDAuNDg2LTAuODIy
LTAuMDAyLTAuNDc4LTAuNDg4LTAuODIxLTAuNDg4LTAuODIxeiI+PC9wYXRoPjwv
Zz4nKwogICAgICAnICAgIDxnIGlkPSJpY29uLXBhdXNlIj48cGF0aCBjbGFzcz0i
cGF0aDEiIGQ9Ik0yMS42IDQuOGMtMS41OSAwLTIuODggMC40OS0yLjg4IDIuMDgw
djE4LjI0YzAgMS41OSAxLjI5IDIuMDgwIDIuODggMi4wODBzMi44OC0wLjQ5IDIu
ODgtMi4wODB2LTE4LjI0YzAtMS41OS0xLjI5LTIuMDgwLTIuODgtMi4wODB6TTEw
LjQgNC44Yy0xLjU5IDAtMi44OCAwLjQ5LTIuODggMi4wODB2MTguMjRjMCAxLjU5
IDEuMjkgMi4wODAgMi44OCAyLjA4MHMyLjg4LTAuNDkgMi44OC0yLjA4MHYtMTgu
MjRjMC0xLjU5LTEuMjktMi4wODAtMi44OC0yLjA4MHoiPjwvcGF0aD48L2c+JysK
ICAgICAgJyAgICA8ZyBpZD0iaWNvbi1maXJzdCI+PHBhdGggY2xhc3M9InBhdGgx
IiBkPSJNMTEuOTc2IDE2YzAgMC40MTMgMC40MTkgMC43MDcgMC40MTkgMC43MDds
MTEuNjQgNy4zMWMwLjg2MiAwLjU2NSAxLjU2NSAwLjE0OSAxLjU2NS0wLjkydi0x
NC4xOTVjMC0xLjA3MC0wLjcwMi0xLjQ4Ni0xLjU2NS0wLjkyMmwtMTEuNjQgNy4z
MTJjMCAwLjAwMi0wLjQxOSAwLjI5NC0wLjQxOSAwLjcwN3pNNi40IDguNTcxdjE0
Ljg1OGMwIDEuNDIxIDAuOTc5IDEuODU2IDIuNCAxLjg1NnMyLjQtMC40MzUgMi40
LTEuODU0di0xNC44NTljMC0xLjQyMi0wLjk3OS0xLjg1OC0yLjQtMS44NThzLTIu
NCAwLjQzNy0yLjQgMS44NTh6Ij48L3BhdGg+PC9nPicrCiAgICAgICcgIDwvZGVm
cz4nKwogICAgICAnPC9zdmc+Jyk7CiAgICB9CiAgICB2YXIgZGl2ID0gbjMuZG9t
VGFyZ2V0LnNlbGVjdCgnLmNvbnRyb2xzJykuYXBwZW5kKCdkaXYnKS5hdHRyKCdj
bGFzcycsICdwbGF5LWNvbnRyb2wtY29udGFpbmVyJyk7CiAgICBkaXYuaHRtbCgK
ICAgICAgIjxkaXYgY2xhc3M9J3N0ZXAtYmFjay1jb250cm9sJz48c3ZnIGNsYXNz
PSdpY29uJyB3aWR0aD0nMzInIGhlaWdodD0nMzInIHZpZXdCb3g9JzAgMCAzMiAz
Mic+PHVzZSB4bGluazpocmVmPScjaWNvbi1maXJzdCc+PC91c2U+PC9zdmc+PC9k
aXY+IisKICAgICAgIjxkaXYgY2xhc3M9J3BsYXktYmFjay1jb250cm9sJz48c3Zn
IGNsYXNzPSdpY29uJyB3aWR0aD0nMzInIGhlaWdodD0nMzInIHZpZXdCb3g9JzAg
MCAzMiAzMic+PGcgdHJhbnNmb3JtPSdyb3RhdGUoMTgwLCAxNiwgMTYpJz48dXNl
IHhsaW5rOmhyZWY9JyNpY29uLXBsYXknPjwvdXNlPjwvZz48L3N2Zz48L2Rpdj4i
KwogICAgICAiPGRpdiBjbGFzcz0ncGF1c2UtY29udHJvbCc+PHN2ZyBjbGFzcz0n
aWNvbicgd2lkdGg9JzMyJyBoZWlnaHQ9JzMyJyB2aWV3Qm94PScwIDAgMzIgMzIn
Pjx1c2UgeGxpbms6aHJlZj0nI2ljb24tcGF1c2UnPjwvdXNlPjwvc3ZnPjwvZGl2
PiIrCiAgICAgICI8ZGl2IGNsYXNzPSdwbGF5LWZvcndhcmQtY29udHJvbCc+PHN2
ZyBjbGFzcz0naWNvbicgd2lkdGg9JzMyJyBoZWlnaHQ9JzMyJyB2aWV3Qm94PScw
IDAgMzIgMzInPjx1c2UgeGxpbms6aHJlZj0nI2ljb24tcGxheSc+PC91c2U+PC9z
dmc+PC9kaXY+IisKICAgICAgIjxkaXYgY2xhc3M9J3N0ZXAtZm9yd2FyZC1jb250
cm9sJz48c3ZnIGNsYXNzPSdpY29uJyB3aWR0aD0nMzInIGhlaWdodD0nMzInIHZp
ZXdCb3g9JzAgMCAzMiAzMic+PGcgdHJhbnNmb3JtPSdyb3RhdGUoMTgwLCAxNiwg
MTYpJz48dXNlIHhsaW5rOmhyZWY9JyNpY29uLWZpcnN0Jz48L3VzZT48L2c+PC9z
dmc+PC9kaXY+IgogICAgKTsKCiAgICBkaXYuc2VsZWN0KCcuc3RlcC1iYWNrLWNv
bnRyb2wnKS5vbignY2xpY2snLCBmdW5jdGlvbigpIHsgbjMuc3RlcEFuaW1hdGlv
bigxKTsgfSk7CiAgICBkaXYuc2VsZWN0KCcucGxheS1iYWNrLWNvbnRyb2wnKS5v
bignY2xpY2snLCBmdW5jdGlvbigpIHsgbjMucGxheUFuaW1hdGlvbigxKTsgfSk7
CiAgICBkaXYuc2VsZWN0KCcucGF1c2UtY29udHJvbCcpLm9uKCdjbGljaycsIGZ1
bmN0aW9uKCkgeyBuMy5lbmRBbmltYXRpb24oKTsgfSk7CiAgICBkaXYuc2VsZWN0
KCcucGxheS1mb3J3YXJkLWNvbnRyb2wnKS5vbignY2xpY2snLCBmdW5jdGlvbigp
IHsgbjMucGxheUFuaW1hdGlvbigpOyB9KTsKICAgIGRpdi5zZWxlY3QoJy5zdGVw
LWZvcndhcmQtY29udHJvbCcpLm9uKCdjbGljaycsIGZ1bmN0aW9uKCkgeyBuMy5z
dGVwQW5pbWF0aW9uKCk7IH0pOwogIH0KICAKICAvKiogY3JlYXRlcyB0aGUgdGlt
ZSBzbGlkZXIgY29udHJvbHMgYW5kIGRlZmluZXMgYXR0YWNoZWQgZXZlbnRzICov
CiAgbjMucHJvdG90eXBlLmNyZWF0ZVNsaWRlckNvbnRyb2wgPSBmdW5jdGlvbigp
IHsKICAgIHZhciBuMyA9IHRoaXM7CiAgICBuMy5kb21UYXJnZXQuc2VsZWN0KCcu
Y29udHJvbHMnKS5hcHBlbmQoJ2RpdicpLmF0dHIoJ2NsYXNzJywgJ3NsaWRlci1j
b250cm9sLWNvbnRhaW5lcicpLmFwcGVuZCgnZGl2JykuYXR0cignY2xhc3MnLCAn
c2xpZGVyJyk7CiAgfQogCiAgLyoqIGxvYWQgYW5kIHByb2Nlc3MgdGhlIEpTT04g
Zm9ybWF0dGVkIGRhdGEKICAqIEBwYXJhbSB7dXJsfEpTT059IC0gZWl0aGVyIGEg
TkRUVi1nZW5lcmF0ZWQgSlNPTiBvYmplY3QsIG9yIGEgVVJMIHBhdGggdG8gZmls
ZSBjb250YWluaW5nIEpTT04gZGF0YQogICovCiAgbjMucHJvdG90eXBlLmxvYWRE
YXRhID0gZnVuY3Rpb24oZ3JhcGhEYXRhKSB7CiAgICB2YXIgbjMgPSB0aGlzOwog
ICAgbjMuZW5kQW5pbWF0aW9uKCk7CiAgICBuMy5jdXJyVGltZSA9IDA7CiAgICBu
My5zZWxlY3RlZE5ldHdvcmsgPSBudWxsOwogICAgbjMuc2VsZWN0ZWQgPSBudWxs
OwogICAgCiAgICB2YXIgcHJvY2Vzc0RhdGEgPSBmdW5jdGlvbihkYXRhKSB7CiAg
ICAgIGNvbnNvbGUudGltZSgnbG9hZERhdGEnKTsKICAgICAgbjMuZ3JhcGggPSBk
YXRhLm5ldHdvcms7CiAgICAgIG4zLnRpbWVJbmRleCA9IGRhdGEucmVuZGVyOwog
ICAgICBpZiAobjMub3B0aW9ucy5kYXRhQ2hvb3NlciAmJiAhICQuaXNQbGFpbk9i
amVjdChncmFwaERhdGEpKSB7CiAgICAgICAgJChuMy5kb21UYXJnZXQuc2VsZWN0
KCcuZGF0YV9jaG9vc2VyJykubm9kZSgpKS52YWwoZ3JhcGhEYXRhKTsKICAgICAg
ICBuMy5kb21UYXJnZXQuc2VsZWN0KCcudmlkZW9fbGluaycpLmF0dHIoJ2hyZWYn
LCBncmFwaERhdGEucmVwbGFjZSgnLmpzb24nLCAnLm1wNCcpKQogICAgICB9CiAg
ICAgIG4zLmNvbnRhaW5lci5zZWxlY3RBbGwoJy5lZGdlLCAubGFiZWwsIC5ub2Rl
JykucmVtb3ZlKCk7CgogICAgICBuMy5ub2RlQ29vcmRzID0ge307CgogICAgICAk
LmVhY2gobjMuZ3JhcGgudmFsLCBmdW5jdGlvbihpLCBuKSB7CiAgICAgICAgaWYg
KCEgJC5pc0VtcHR5T2JqZWN0KG4pKSB7CiAgICAgICAgICBuLmlkID0gaSsxOwog
ICAgICAgIH0KICAgICAgICBuMy5ub2RlQ29vcmRzW24uaWRdID0gewogICAgICAg
ICAgY29vcmQ6IFswLDBdLAogICAgICAgICAgYWN0aXZlOiBmYWxzZSwKICAgICAg
ICAgIHNpemU6IDAsCiAgICAgICAgICBwb2ludHM6IFtdCiAgICAgICAgfQogICAg
ICB9KQogICAgICAkLmVhY2gobjMuZ3JhcGgubWVsLCBmdW5jdGlvbihpLCBlKSB7
CiAgICAgICAgaWYgKCEgJC5pc0VtcHR5T2JqZWN0KGUpKSB7CiAgICAgICAgICBl
LmlkID0gaSsxOwogICAgICAgIH0KICAgICAgfSkKICAgICAgdmFyIHNsaWNlSW5m
byA9IG4zLmdyYXBoLmdhbFsnc2xpY2UucGFyJ107CiAgICAgIAogICAgICBuMy5t
aW5UaW1lID0gc2xpY2VJbmZvLnN0YXJ0WzBdOwogICAgICBuMy5tYXhUaW1lID0g
c2xpY2VJbmZvLmVuZFswXTsKICAgICAgbjMuaW50ZXJ2YWwgPSBzbGljZUluZm8u
aW50ZXJ2YWxbMF07CiAgICAgIHZhciB2YWxJbmRleCA9IHt9OwogICAgICAkLmVh
Y2gobjMudGltZUluZGV4LCBmdW5jdGlvbihpLCB0KXsKICAgICAgICB2YWxJbmRl
eFt0LnN0YXJ0XSA9IGk7CiAgICAgICAgdC5yZW5kZXJEYXRhID0gbjMuZ2VuZXJh
dGVTbGljZVJlbmRlckRhdGEoaSk7CiAgICAgICAgZGVsZXRlIHQuZGF0YTsgLy9y
ZW1vdmUgcmVkdW5kYW50IGRhdGEgdGhhdCB3ZSd2ZSBzdG9yZWQgaW4gcmVuZGVy
RGF0YQogICAgICB9KSAgICAgIAoKICAgICAgbjMudXBkYXRlRGltZW5zaW9ucygp
OwoKICAgICAgaWYobjMub3B0aW9ucy5zbGlkZXIpIHsKICAgICAgICB2YXIgc2xp
ZGVyRGl2ID0gbjMuZG9tVGFyZ2V0LnNlbGVjdCgnLnNsaWRlcicpOwoKICAgICAg
ICBzbGlkZXJEaXYuaHRtbCgnJyk7CgogICAgICAgIG4zLnNsaWRlciA9IGQzLnNs
aWRlcigpLmF4aXModHJ1ZSkuc3RlcChuMy5pbnRlcnZhbCk7CiAgICAgICAgbjMu
c2xpZGVyLm1hcmdpbigzNSkKICAgICAgICBuMy5zbGlkZXIubWluKG4zLm1pblRp
bWUpCiAgICAgICAgbjMuc2xpZGVyLm1heChuMy5tYXhUaW1lK3NsaWNlSW5mb1sn
YWdncmVnYXRlLmR1ciddWzBdKQogICAgICAgIG4zLnNsaWRlci5hbmltYXRlKG4z
Lm9wdGlvbnMuYW5pbWF0aW9uRHVyYXRpb24pCiAgICAgICAgbjMuc2xpZGVyLnZh
bHVlKG4zLm1pblRpbWUpCiAgICAgICAgbjMuc2xpZGVyLmludGVydmFsKHNsaWNl
SW5mb1snYWdncmVnYXRlLmR1ciddWzBdKQogICAgICAgIG4zLnNsaWRlci5vbign
c2xpZGUnLCBmdW5jdGlvbihleHQsIHZhbHVlKSB7CiAgICAgICAgICAvL0NoZWNr
IHRvIHNlZSBpZiBldmVudCBvcmlnaW5hdGVkIGZyb20gc2xpZGVyIGNvbnRyb2wg
b3IgZWxzZXdoZXJlCiAgICAgICAgICB2YXIgZXZlbnQgPSBkMy5ldmVudDsKICAg
ICAgICAgIGlmIChldmVudC50eXBlID09ICdkcmFnJyB8fCBkMy5zZWxlY3QoZXZl
bnQuY3VycmVudFRhcmdldCkuY2xhc3NlZCgnZDMtc2xpZGVyJykpIHsKICAgICAg
ICAgICAgbjMuZW5kQW5pbWF0aW9uKCk7CiAgICAgICAgICAgIG4zLmFuaW1hdGVH
cmFwaChuMy5jdXJyVGltZSwgdmFsSW5kZXhbdmFsdWVdLCB0cnVlKTsKICAgICAg
ICAgIH0KICAgICAgICB9KQogICAgICAgIAogICAgICAgIG4zLnNsaWRlci5vbign
c2xpZGVlbmQnLCBmdW5jdGlvbigpIHsKICAgICAgICAgIG4zLnNsaWRlci5hbmlt
YXRlKG4zLm9wdGlvbnMuYW5pbWF0aW9uRHVyYXRpb24pOwogICAgICAgIH0pCiAg
ICAgICAgc2xpZGVyRGl2Lm9uKCdtb3VzZWRvd24nLCBmdW5jdGlvbihlKSB7IAog
ICAgICAgICAgbjMuc2xpZGVyLmFuaW1hdGUoMCk7CiAgICAgICAgfSkKCiAgICAg
ICAgc2xpZGVyRGl2LmNhbGwobjMuc2xpZGVyKTsKICAgICAgfQogICAgICBjb25z
b2xlLnRpbWVFbmQoJ2xvYWREYXRhJyk7CiAgICAgIGlmIChuMy5vcHRpb25zLmFu
aW1hdGVPbkxvYWQpIHsKICAgICAgICBuMy5wbGF5QW5pbWF0aW9uKCk7CiAgICAg
IH0gZWxzZSB7CiAgICAgICAgbjMudXBkYXRlR3JhcGgobjMub3B0aW9ucy5hbmlt
YXRpb25EdXJhdGlvbik7CiAgICAgIH0KICAgIH07CgogICAgaWYoJC5pc1BsYWlu
T2JqZWN0KGdyYXBoRGF0YSkpIHsKICAgICAgcHJvY2Vzc0RhdGEoZ3JhcGhEYXRh
KQogICAgfSBlbHNlIHsKICAgICAgJC5nZXRKU09OKGdyYXBoRGF0YSwgZnVuY3Rp
b24oZGF0YSkgewogICAgICAgIHByb2Nlc3NEYXRhKGRhdGEpOwogICAgICB9KTsK
ICAgIH0KICB9CgogIHZhciBjb2xvciA9IGQzLnNjYWxlLmNhdGVnb3J5MjAoKTsK
CiAgLyoqIEZvciBhIGdpdmVuIHRpbWUgc2xpY2UsIHByb2Nlc3MgdGltZUluZGV4
IGRhdGEgYW5kIGdlbmVyYXRlIHJlbmRlciBkYXRhLCBmaWxsaW5nIGluIGRlZmF1
bHRzIGFzIG5lY2Vzc2FyeQogICogQHBhcmFtIHtpbnRlZ2VyfSAtIHRoZSB0aW1l
IGluZGV4IHRvIHByb2Nlc3MKICAqIEBwcml2YXRlCiAgKi8KICBuMy5wcm90b3R5
cGUuZ2VuZXJhdGVTbGljZVJlbmRlckRhdGEgPSBmdW5jdGlvbih0aW1lKSB7CiAg
ICB2YXIgbjMgPSB0aGlzOwoKICAgIHZhciBzbGljZVJlbmRlckRhdGEgPSB7CiAg
ICAgIGdyYXBoOiB7fSwKICAgICAgbm9kZToge30sCiAgICAgIGVkZ2U6IHt9CiAg
ICB9OwoKICAgIHZhciBkYXRhID0gbjMudGltZUluZGV4W3RpbWVdLmRhdGE7Cgog
ICAgJC5lYWNoKFsnZ3JhcGgnLCAnbm9kZScsICdlZGdlJ10sIGZ1bmN0aW9uKGks
IHR5cGUpIHsKICAgICAgdmFyIHNvdXJjZUxpc3QgPSBbXTsKICAgICAgaWYgKHR5
cGUgPT0gJ2dyYXBoJykgewogICAgICAgIHNvdXJjZUxpc3QgPSBbbjMuZ3JhcGgu
Z2FsXTsKICAgICAgfSBlbHNlIGlmICh0eXBlID09ICdub2RlJykgewogICAgICAg
IHNvdXJjZUxpc3QgPSBuMy5ncmFwaC52YWw7CiAgICAgIH0gZWxzZSB7CiAgICAg
ICAgc291cmNlTGlzdCA9IG4zLmdyYXBoLm1lbDsKICAgICAgfQoKICAgICAgJC5l
YWNoKHNvdXJjZUxpc3QsIGZ1bmN0aW9uKGksIGl0ZW0pIHsKICAgICAgICB2YXIg
aWQgPTA7CiAgICAgICAgdmFyIHByb3BlcnR5SW5kZXg7CiAgICAgICAgdmFyIGl0
ZW1Qcm9wZXJ0aWVzID0ge307CgogICAgICAgIGlmICh0eXBlICE9ICdncmFwaCcp
IHsKICAgICAgICAgIGlkID0gaXRlbS5pZDsKICAgICAgICAgIHByb3BlcnR5SW5k
ZXggPSBkYXRhLmFjdGl2ZVt0eXBlKydzJ11baWRdOwogICAgICAgIH0KCiAgICAg
ICAgaWYgKHR5cGUgPT0gJ2dyYXBoJyB8fCBwcm9wZXJ0eUluZGV4ICE9PSB1bmRl
ZmluZWQpIHsKICAgICAgICAgIGl0ZW1Qcm9wZXJ0aWVzLmlkID0gaWQ7CiAgICAg
ICAgICAkLmVhY2gobmR0dlByb3BlcnRpZXNbdHlwZV0sIGZ1bmN0aW9uKHByb3Bl
cnR5LCBkZWYpIHsKICAgICAgICAgICAgdmFyIGxvb2t1cCA9IHByb3BlcnR5SW5k
ZXg7CiAgICAgICAgICAgIHZhciB2YWx1ZSA9IGRlZjsKCiAgICAgICAgICAgIC8v
SWYgdGhlIHByb3BlcnR5IGxpc3QgaGFzIG9ubHkgb25lIHZhbHVlLCB3ZSBhcHBs
eSBpdCB0byBhbGwgaXRlbXMKICAgICAgICAgICAgaWYodHlwZW9mIGRhdGFbcHJv
cGVydHldICE9PSAndW5kZWZpbmVkJyAmJiBkYXRhW3Byb3BlcnR5XS5sZW5ndGgg
PT0gMSkgewogICAgICAgICAgICAgIGxvb2t1cCA9IDA7CiAgICAgICAgICAgIH0K
ICAgICAgICAgICAgaWYgKHR5cGVvZiBkYXRhW3Byb3BlcnR5XSAhPT0gJ3VuZGVm
aW5lZCcgJiYgZGF0YVtwcm9wZXJ0eV1bbG9va3VwXSAhPT0gdW5kZWZpbmVkKSB7
CiAgICAgICAgICAgICAgdmFsdWUgPSBkYXRhW3Byb3BlcnR5XVtsb29rdXBdOwog
ICAgICAgICAgICAgIGlmICh2YWx1ZSAmJiAocHJvcGVydHkgPT0gJ21haW4nIHx8
IHByb3BlcnR5ID09ICd4bGFiJykpIHsKICAgICAgICAgICAgICAgIHZhbHVlID0g
dmFsdWUuc3BsaXQoJ1xuJyk7CiAgICAgICAgICAgICAgfSBlbHNlIGlmICghIHZh
bHVlICYmIHByb3BlcnR5ID09ICdjb29yZCcpIHsKICAgICAgICAgICAgICAgIGNv
bnNvbGUubG9nKCdtaXNzaW5nIGNvb3JkaW5hdGVzIGZvciBub2RlICcraWQrICcg
YXQgdGltZSAnK3RpbWUrJyAoJytuMy50aW1lSW5kZXhbdGltZV0uc3RhcnQrJy0n
K24zLnRpbWVJbmRleFt0aW1lXS5lbmQrJyknKTsKICAgICAgICAgICAgICB9CiAg
ICAgICAgICAgIH0gZWxzZSBpZiAodHlwZSA9PSAnZ3JhcGgnICYmIHR5cGVvZiBk
YXRhW3Byb3BlcnR5XSAhPT0gJ3VuZGVmaW5lZCcpIHsgLy9ncmFwaCBwcm9wZXJ0
aWVzIGdldCBhcHBsaWVkIGRpcmVjdGx5CiAgICAgICAgICAgICAgdmFsdWUgPSBk
YXRhW3Byb3BlcnR5XTsKICAgICAgICAgICAgfSBlbHNlIGlmIChwcm9wZXJ0eSA9
PSAnbGFiZWwnICYmIHNsaWNlUmVuZGVyRGF0YS5ncmFwaC5kaXNwbGF5bGFiZWxz
KSB7CiAgICAgICAgICAgICAgdmFsdWUgPSBpZDsKICAgICAgICAgICAgfQogICAg
ICAgICAgICBpZiAodmFsdWUgJiYgJC50eXBlKHZhbHVlKSA9PT0gJ3N0cmluZycg
JiYgdmFsdWUubWF0Y2goJ3JnYmEnKSkgewogICAgICAgICAgICAgIHZhciByZ2Jh
ID0gdmFsdWUubWF0Y2goL15yZ2JhXCgoLiopLCA/KC4qKVwpJC8pOwogICAgICAg
ICAgICAgIGlmIChyZ2JhKSB7CiAgICAgICAgICAgICAgICB2YWx1ZSA9ICJyZ2Io
IityZ2JhWzFdKyIpIjsKICAgICAgICAgICAgICAgIHZhciBvcGFjaXR5UHJvcCA9
IHByb3BlcnR5KyIuc3Ryb2tlLW9wYWNpdHkiOwogICAgICAgICAgICAgICAgaWYg
KHByb3BlcnR5ID09ICd2ZXJ0ZXguY29sJyB8fCBwcm9wZXJ0eSA9PSAnYmcnIHx8
IHByb3BlcnR5ID09ICdsYWJlbC5jb2wnKSB7CiAgICAgICAgICAgICAgICAgIG9w
YWNpdHlQcm9wID0gcHJvcGVydHkrIi5maWxsLW9wYWNpdHkiOwogICAgICAgICAg
ICAgICAgfQogICAgICAgICAgICAgICAgaXRlbVByb3BlcnRpZXNbb3BhY2l0eVBy
b3BdID0gcmdiYVsyXTsKICAgICAgICAgICAgICB9CiAgICAgICAgICAgIH0KICAg
ICAgICAgICAgLy8gaWYgKHByb3BlcnR5ID09ICdlZGdlLmx3ZCcpIHsKICAgICAg
ICAgICAgICAvLyB2YWx1ZSA9IGlkKjAuMjsKICAgICAgICAgICAgLy8gfQogICAg
ICAgICAgICAvLyBpZiAocHJvcGVydHkgPT0gJ2VkZ2UuY29sJyB8fCBwcm9wZXJ0
eSA9PSAndmVydGV4LmNvbCcgfHwgcHJvcGVydHkgPT0gJ2xhYmVsLmNvbCcpIHsK
ICAgICAgICAgICAgLy8gICB2YWx1ZSA9IHR5cGUgPT0gJ2VkZ2UnID8gY29sb3Io
dGltZSkgOiBjb2xvcih0aW1lKzEpOwogICAgICAgICAgICAvLyB9CiAgICAgICAg
ICAgIGl0ZW1Qcm9wZXJ0aWVzW3Byb3BlcnR5XSA9IHZhbHVlOwogICAgICAgICAg
fSkKCiAgICAgICAgICBpZiAodHlwZSA9PSAnZWRnZScpIHsKICAgICAgICAgICAg
JC5lYWNoKFsnaW5sJywgJ291dGwnXSwgZnVuY3Rpb24oaSwgZGlyZWN0aW9uKSB7
CiAgICAgICAgICAgICAgdmFyIG90aGVyZGlyID0gZGlyZWN0aW9uID09ICdpbmwn
ID8gJ291dGwnIDogJ2lubCc7CiAgICAgICAgICAgICAgaXRlbVByb3BlcnRpZXNb
ZGlyZWN0aW9uXSA9IHsKICAgICAgICAgICAgICAgIGlkOiBpdGVtW2RpcmVjdGlv
bl1bMF0KICAgICAgICAgICAgICB9CiAgICAgICAgICAgICAgc2xpY2VSZW5kZXJE
YXRhLm5vZGVbaXRlbVtkaXJlY3Rpb25dWzBdXS5saW5rc1tpdGVtW290aGVyZGly
XVswXV0gPSBpZDsKICAgICAgICAgICAgfSk7ICAgICAgCiAgICAgICAgICB9IGVs
c2UgaWYgKHR5cGUgPT0gJ25vZGUnKSB7CiAgICAgICAgICAgIGl0ZW1Qcm9wZXJ0
aWVzLmxpbmtzID0ge307CiAgICAgICAgICB9CiAgICAgICAgICBpZiAodHlwZSA9
PSAnZ3JhcGgnKSB7CiAgICAgICAgICAgIHNsaWNlUmVuZGVyRGF0YVt0eXBlXSA9
IGl0ZW1Qcm9wZXJ0aWVzOwogICAgICAgICAgfSBlbHNlIHsKICAgICAgICAgICAg
c2xpY2VSZW5kZXJEYXRhW3R5cGVdW2lkXSA9IGl0ZW1Qcm9wZXJ0aWVzOwogICAg
ICAgICAgfQogICAgICAgIH0gZWxzZSBpZiAodHlwZSA9PSAnbm9kZScpIHsKICAg
ICAgICAgIG4zLm5vZGVDb29yZHNbaWRdLmFjdGl2ZSA9IGZhbHNlOwogICAgICAg
IH0KICAgICAgfSkKICAgIH0pCiAgICByZXR1cm4gc2xpY2VSZW5kZXJEYXRhOwog
IH0KCiAgLyoqIHVwZGF0ZXMgcmVuZGVyZGF0YSBub2RlIGNvb3JkaW5hdGVzIGJh
c2VkIG9uIGN1cnJlbnQgc3RhdGUgb2YgZ3JhcGgsIGFuZCB1cGRhdGVzIG5vZGUg
c3RhdGUgdHJhY2tlcgogICogQHBhcmFtIHtpbnRlZ2VyfSAtIHRoZSB0aW1lIGlu
ZGV4IHRvIHByb2Nlc3MKICAqIEBwcml2YXRlCiAgKi8KICBuMy5wcm90b3R5cGUu
dXBkYXRlU2xpY2VSZW5kZXJEYXRhID0gZnVuY3Rpb24odGltZSkgewogICAgdmFy
IG4zID0gdGhpczsKCiAgICB2YXIgcHJldk5vZGVDb29yZHMgPSAkLmV4dGVuZCh7
fSwgbjMubm9kZUNvb3Jkcyk7CiAgICB2YXIgZGF0YSA9IG4zLnRpbWVJbmRleFt0
aW1lXS5yZW5kZXJEYXRhOwogICAgCiAgICAkLmVhY2gobjMubm9kZUNvb3Jkcywg
ZnVuY3Rpb24oaWQsIG5vZGVDb29yZCkgewogICAgICB2YXIgbm9kZSA9IGRhdGEu
bm9kZVtpZF07CiAgICAgIGlmIChub2RlKSB7CiAgICAgICAgaWYgKCFub2RlLmNv
b3JkKSB7CiAgICAgICAgICBub2RlLnJlbmRlckNvb3JkID0gbm9kZUNvb3JkLmNv
b3JkOwogICAgICAgIH0gZWxzZSB7CiAgICAgICAgICBub2RlLnJlbmRlckNvb3Jk
ID0gbm9kZS5jb29yZDsKICAgICAgICB9CiAgICAgICAgbjMubm9kZUNvb3Jkc1tp
ZF0gPSB7CiAgICAgICAgICBjb29yZDogbm9kZS5yZW5kZXJDb29yZCwKICAgICAg
ICAgIGFjdGl2ZTogdHJ1ZSwKICAgICAgICAgIHNpemU6IG5vZGVbJ3ZlcnRleC5j
ZXgnXSwKICAgICAgICAgIHByZXZQb2ludHM6IHByZXZOb2RlQ29vcmRzW2lkXS5w
b2ludHMsCiAgICAgICAgICAvL0ZJWE1FIC0gdGhpcyBjb3VsZCBicmVhayBpZiBh
IG5vZGUgaXMgZXZlciBjZW50ZXJlZCBhdCAwLDAKICAgICAgICAgIHByZXZDb29y
ZDogcHJldk5vZGVDb29yZHNbaWRdLmNvb3JkWzBdICYmIHByZXZOb2RlQ29vcmRz
W2lkXS5jb29yZFsxXSA/IHByZXZOb2RlQ29vcmRzW2lkXS5jb29yZCA6IG51bGwK
ICAgICAgICB9CiAgICAgIH0gZWxzZSB7CiAgICAgICAgbjMubm9kZUNvb3Jkc1tp
ZF0uYWN0aXZlID0gZmFsc2U7CiAgICAgIH0KICAgIH0pOwogICAgJC5lYWNoKGRh
dGEuZWRnZSwgZnVuY3Rpb24oaWQsIGVkZ2UpewogICAgICAkLmVhY2goWydpbmwn
LCAnb3V0bCddLCBmdW5jdGlvbihpLCBkaXJlY3Rpb24pewogICAgICAgIHZhciBu
b2RlaWQgPSBlZGdlW2RpcmVjdGlvbl0uaWQ7CiAgICAgICAgdmFyIHByZXZDb29y
ZCA9IHByZXZOb2RlQ29vcmRzW25vZGVpZF07CiAgICAgICAgdmFyIGNvb3JkcyA9
IG4zLm5vZGVDb29yZHNbbm9kZWlkXSB8fCBwcmV2Q29vcmQ7CiAgICAgICAgZWRn
ZVtkaXJlY3Rpb25dID0gewogICAgICAgICAgaWQ6IG5vZGVpZCwKICAgICAgICAg
IGNvb3JkczogY29vcmRzLAogICAgICAgICAgcHJldlBvaW50czpwcmV2Q29vcmQu
cG9pbnRzLAogICAgICAgICAgLy9JZiB0aGUgbm9kZSBpcyBuZXdseSBhY3RpdmUs
IHVzZSB0aGUgY3VycmVudCBjb29yZGluYXRlcyBmb3IgdGhlIHN0YXJ0IHZhbHVl
cwogICAgICAgICAgc3RhcnRDb29yZHM6ICEgcHJldkNvb3JkLmFjdGl2ZSA/IGNv
b3JkcyA6IHByZXZDb29yZAogICAgICAgIH0KICAgICAgfSkKICAgIH0pCiAgICBy
ZXR1cm4gZGF0YTsKICB9CgogIC8qKiBjcmVhdGUgYSBwYXRoIGRlc2NyaXB0aW9u
cyBvbiB0aGUgZ2l2ZW4gZGF0YSBzZWxlY3Rpb24KICAqIEBwYXJhbSB7b2JqZWN0
fSAtIHRoZSBEMyBkYXRhIHNlbGVjdGlvbgogICogQHBhcmFtIHtORFRWX0QzfQog
ICogQHBhcmFtIHtib29sZWFufSAtIElmIHRydWUsIHBvc2l0aW9ucyBlbmQgb2Yg
bGluZSBvZmZzZXQgb2Ygbm9kZSByYWRpdXMgdG8gYWNjb21vZGF0ZSBhcnJvd2hl
YWQgCiAgKiBAcGFyYW0ge2Jvb2xlYW59IC0gSWYgdHJ1ZSwgZHJhd3MgcGF0aCB1
c2luZyBjdXJyZW50IG5vZGUgcG9zaXRpb25zIChiZWZvcmUgYW5pbWF0aW9uIGJl
Z2lucykKICAqIEBwcml2YXRlCiAgKi8KICBuMy5wcm90b3R5cGUuZHJhd0VkZ2Ug
PSBmdW5jdGlvbihzZWxlY3Rpb24sIG4zLCB1c2VhcnJvd3MsIHN0YXJ0KSB7CiAg
ICBzZWxlY3Rpb24uYXR0cih7CiAgICAgIGQ6IGZ1bmN0aW9uKGQpIHsKICAgICAg
ICB2YXIgdHlwZSA9IHN0YXJ0ID8gJ3N0YXJ0Q29vcmRzJyA6ICdjb29yZHMnOwoK
ICAgICAgICB2YXIgc3RhcnROb2RlID0gZC5vdXRsW3R5cGVdOwogICAgICAgIHZh
ciBlbmROb2RlID0gZC5pbmxbdHlwZV07CiAgICAgICAgdmFyIHN0YXJ0Q29vcmRz
ID0gW24zLnhTY2FsZShzdGFydE5vZGUuY29vcmRbMF0pLCBuMy55U2NhbGUoc3Rh
cnROb2RlLmNvb3JkWzFdKV07CiAgICAgICAgdmFyIGVuZENvb3JkcyA9IFtuMy54
U2NhbGUoZW5kTm9kZS5jb29yZFswXSksIG4zLnlTY2FsZShlbmROb2RlLmNvb3Jk
WzFdKV07CgogICAgICAgIGlmKHVzZWFycm93cyB8fCBuZHR2UHJvcGVydGllcy5n
cmFwaC5lZGdlT2Zmc2V0KSB7CiAgICAgICAgICB2YXIgYXJyb3dPZmZzZXQgPSB1
c2VhcnJvd3MgPyBzY2FsZUFycm93aGVhZHMoZCkqZFsnZWRnZS5sd2QnXSA6IDA7
CiAgICAgICAgICBzdGFydENvb3JkcyA9IGZpbmROb2RlSW50ZXJzZWN0aW9uKG4z
LCBkLm91dGwuaWQsIGVuZENvb3JkcywgbmR0dlByb3BlcnRpZXMuZ3JhcGguZWRn
ZU9mZnNldCwgc3RhcnQpCiAgICAgICAgICBlbmRDb29yZHMgPSBmaW5kTm9kZUlu
dGVyc2VjdGlvbihuMywgZC5pbmwuaWQsIHN0YXJ0Q29vcmRzLCBhcnJvd09mZnNl
dCtuZHR2UHJvcGVydGllcy5ncmFwaC5lZGdlT2Zmc2V0LCBzdGFydCkKICAgICAg
ICB9CiAgICAgICAgZFsnY3VycmVudENvb3JkcyddID0gW3N0YXJ0Q29vcmRzLCBl
bmRDb29yZHNdOwogICAgICAgIHJldHVybiAnTSAnK3N0YXJ0Q29vcmRzWzBdLnRv
Rml4ZWQoMSkrJyAnK3N0YXJ0Q29vcmRzWzFdLnRvRml4ZWQoMSkrJyBMICcrZW5k
Q29vcmRzWzBdLnRvRml4ZWQoMSkrJyAnK2VuZENvb3Jkc1sxXS50b0ZpeGVkKDEp
OyAgICAgCiAgICAgIH0KICAgIH0pCiAgfQoKICAvKiogaW5pdGlhbGl6ZXMgYSBE
MyBsaW5lIGRyYXdpbmcgZnVuY3Rpb24KICAqIEBwcml2YXRlICovCiAgdmFyIGRy
YXdMaW5lID0gZnVuY3Rpb24oKSB7CiAgICByZXR1cm4gZDMuc3ZnLmxpbmUoKQog
ICAgICAueChmdW5jdGlvbihkKXtyZXR1cm4gZFswXTt9KQogICAgICAueShmdW5j
dGlvbihkKXtyZXR1cm4gZFsxXTt9KQogIH0KICAKICAvKiogb2Zmc2V0IGEgbGlu
ZSBiZXR3ZWVuIHR3byBnaXZlbiBwb2ludHMgYnkgdGhlIHNwZWNpZmllZCBhbW91
bnQuIHJldHVybnMgdGhlIG5ldyB0YXJnZXQgcG9pbnQKICAqIEBwYXJhbSB7cG9p
bnR9IC0gdGhlIG9yaWdpbiBwb2ludAogICogQHBhcmFtIHtwb2ludH0gLSB0aGUg
dGFyZ2V0IHBvaW50CiAgKiBAcGFyYW0ge251bWJlcn0gLSB0aGUgb2Zmc2V0IGFt
b3VudCAoYXMgbGVuZ3RoIG9mIGxpbmUpCiAgKiBAcHJpdmF0ZSAqLwogIHZhciBv
ZmZzZXRMaW5lID0gZnVuY3Rpb24ocG9pbnRBLCBwb2ludEIsIG9mZnNldCkgewog
ICAgcG9pbnRBID0gcG9pbnRBLm1hcChwYXJzZUZsb2F0KTsKICAgIHBvaW50QiA9
IHBvaW50Qi5tYXAocGFyc2VGbG9hdCk7CgogICAgdmFyIHhsZW4gPSBwb2ludEJb
MF0gLSBwb2ludEFbMF07CiAgICB2YXIgeWxlbiA9IHBvaW50QlsxXSAtIHBvaW50
QVsxXTsKCiAgICAvLyBEZXRlcm1pbmUgaHlwb3RlbnVzZSBsZW5ndGgKICAgIHZh
ciBobGVuID0gTWF0aC5zcXJ0KE1hdGgucG93KHhsZW4sMikgKyBNYXRoLnBvdyh5
bGVuLDIpKTsKCiAgICAvLyBEZXRlcm1pbmUgdGhlIHJhdGlvIGJldHdlZW4gdGhl
IHNob3J0ZW5lZCB2YWx1ZSBhbmQgdGhlIGZ1bGwgaHlwb3RlbnVzZS4KICAgIHZh
ciByYXRpbyA9IChobGVuIC0gb2Zmc2V0KSAvIGhsZW47CgogICAgLy9JZiB0aGUg
cmF0aW8gaXMgaW52YWxpZCwganVzdCB1c2UgdGhlIG9yaWdpbmFsIGNvb3JkaW5h
dGVzCiAgICBpZiAoJC5pc051bWVyaWMocmF0aW8pKSB7IAogICAgICByZXR1cm4g
WwogICAgICAgIHBvaW50QVswXSArICh4bGVuICogcmF0aW8pLAogICAgICAgIHBv
aW50QVsxXSArICh5bGVuICogcmF0aW8pCiAgICAgIF07CiAgICB9IGVsc2Ugewog
ICAgICByZXR1cm4gcG9pbnRCOwogICAgfQogIH0KCiAgdmFyIHNjYWxlQXJyb3do
ZWFkcyA9IGZ1bmN0aW9uKGQpIHsKICAgIHJldHVybiA2LTIuNSpNYXRoLmF0YW4o
ZFsnZWRnZS5sd2QnXSouNikKICB9CiAgLyoqIGZpbmQgdGhlIHBvaW50IGF0IHdo
aWNoIGEgbGluZSBkcmF3biBmcm9tIGEgcG9pbnQgdG8gdGhlIGNlbnRlciBvZiBh
IG5vZGUgaW50ZXJzZWN0cyB3aXRoIHRoZSBub2RlcyBib3JkZXIuCiAgKiByZXR1
cm5zIHRoZSBjb29yZGluYXRlLCBvcHRpb25hbGx5IGFwcGx5aW5nIGFuIG9mZnNl
dAogICogQHBhcmFtIHtuM30gbjMgLSB0aGUgbjMgb2JqZWN0CiAgKiBAcGFyYW0g
e2lkfSBpZCAtIHRoZSBub2RlIGlkCiAgKiBAcGFyYW0ge3BvaW50fSBwb2ludCAt
IHRoZSBwb2ludCBvZiBvcmlnaW4gZm9yIHRoZSBsaW5lCiAgKiBAcGFyYW0geyBu
dW1iZXJ9IG9mZnNldCAtIHRoZSBvZmZzZXQgYW1vdW50IHRvIGFwcGx5CiAgKiBA
cGFyYW0ge2Jvb2xlYW59IHVzZVByZXYgLSBzaG91bGQgd2UgdXNlIHRoZSBjdXJy
ZW50IHNsaWNlIGNvb3JkaW5hdGVzLCBvciB0aG9zZSBvZiB0aGUgcHJldmlvdXMg
c3RhdGU/CiAgKiBAcHJpdmF0ZSAqLwogIHZhciBmaW5kTm9kZUludGVyc2VjdGlv
biA9IGZ1bmN0aW9uKG4zLCBpZCwgcG9pbnQsIG9mZnNldCwgdXNlUHJldikgewog
ICAgb2Zmc2V0ID0gb2Zmc2V0IHx8IDA7CiAgICB2YXIgbm9kZURhdGEgPSBuMy5u
b2RlQ29vcmRzW2lkXTsKICAgIHZhciBjZW50ZXIgPSB1c2VQcmV2ICYmIG5vZGVE
YXRhLnByZXZDb29yZCA/IG5vZGVEYXRhLnByZXZDb29yZC5zbGljZSgpIDogbm9k
ZURhdGEuY29vcmQuc2xpY2UoKTsKICAgIHZhciBpbnRlcnNlY3Rpb24gPSBbCiAg
ICAgIG4zLnhTY2FsZShjZW50ZXJbMF0pLAogICAgICBuMy55U2NhbGUoY2VudGVy
WzFdKQogICAgXTsKCiAgICBpZiAobm9kZURhdGEucG9pbnRzKSB7CiAgICAgIHZh
ciBwb2ludHMgPSB1c2VQcmV2ICYmIG5vZGVEYXRhLnByZXZQb2ludHMubGVuZ3Ro
ID8gbm9kZURhdGEucHJldlBvaW50cyA6IG5vZGVEYXRhLnBvaW50czsKICAgICAg
aW50ZXJzZWN0aW9uID0gZmluZFBvbHlnb25JbnRlcnNlY3Rpb24ocG9pbnRzLCBp
bnRlcnNlY3Rpb24sIHBvaW50KTsKICAgIH0gZWxzZSB7CiAgICAgIG9mZnNldCAr
PSBub2RlRGF0YS5zaXplICogbjMuYmFzZU5vZGVTaXplOwogICAgfQogICAgcmV0
dXJuIG9mZnNldExpbmUocG9pbnQsIGludGVyc2VjdGlvbiwgb2Zmc2V0KTsKICB9
CiAKICAvKiogZmluZCB0aGUgcG9pbnQgb2YgaW50ZXNlY3Rpb24gb2YgdHdvIGxp
bmVzIC0gYWRhcHRlZCBmcm9tIGh0dHA6Ly9wYXVsYm91cmtlLm5ldC9nZW9tZXRy
eS9wb2ludGxpbmVwbGFuZS8gCiAgKiBAcHJpdmF0ZSAqLwogIHZhciBmaW5kTGlu
ZUludGVyc2VjdGlvbiA9IGZ1bmN0aW9uKGEsIGIsIGMsIGQpIHsKICAgIHZhciBk
ZW5vbWluYXRvciA9IChkWzFdIC0gY1sxXSkgKiAoYlswXSAtIGFbMF0pIC0gKGRb
MF0gLSBjWzBdKSAqIChiWzFdIC0gYVsxXSk7CiAgICB2YXIgcHJlY2lzaW9uID0g
MTAwMDAwOwoKICAgIHZhciBvbmUgPSAoIChkWzBdIC0gY1swXSkgKiAoYVsxXSAt
IGNbMV0pIC0gKGRbMV0gLSBjWzFdKSAqIChhWzBdIC0gY1swXSkgKSAvIGRlbm9t
aW5hdG9yOwogICAgdmFyIHR3byA9ICggKGJbMF0gLSBhWzBdKSAqIChhWzFdIC0g
Y1sxXSkgLSAoYlsxXSAtIGFbMV0pICogKGFbMF0gLSBjWzBdKSApIC8gZGVub21p
bmF0b3I7CiAgICBvbmUgPSBNYXRoLnJvdW5kKG9uZSAqIHByZWNpc2lvbikgLyBw
cmVjaXNpb247CiAgICB0d28gPSBNYXRoLnJvdW5kKHR3byAqIHByZWNpc2lvbikg
LyBwcmVjaXNpb247CgogICAgaWYgKG9uZSA8PSAxICYmIG9uZSA+PSAwICYmIHR3
byA8PSAxICYmIHR3byA+PSAwKSB7CiAgICAgIHZhciB4ID0gYVswXSArIG9uZSAq
IChiWzBdIC0gYVswXSk7CiAgICAgIHZhciB5ID0gYVsxXSArIG9uZSAqIChiWzFd
IC0gYVsxXSk7IAogICAgICByZXR1cm4gW3gseV07CiAgICB9CiAgfQoKICAvKiog
ZmluZCB0aGUgcG9pbnQgYXQgd2hpY2ggYSBsaW5lIGludGVyc2VjdCBhIHBvbHln
b24KICAqIEBwcml2YXRlICovCiAgdmFyIGZpbmRQb2x5Z29uSW50ZXJzZWN0aW9u
ID0gZnVuY3Rpb24ocG9pbnRzLCBjZW50ZXJBLCBjZW50ZXJCKSB7CiAgICB2YXIg
aW50ZXJzZWN0aW9uID0gW107CiAgICBmb3IodmFyIGkgPSAwOyBpPHBvaW50cy5s
ZW5ndGgtMTsgaSsrKSB7CiAgICAgIHZhciByZXMgPSBmaW5kTGluZUludGVyc2Vj
dGlvbihwb2ludHNbaV0sIHBvaW50c1tpKzFdLCBjZW50ZXJBLCBjZW50ZXJCKTsK
ICAgICAgaWYgKHJlcykgewogICAgICAgIGludGVyc2VjdGlvbiA9IHJlczsKICAg
ICAgICBicmVhazsKICAgICAgfQogICAgfQogICAgaWYgKCEgaW50ZXJzZWN0aW9u
Lmxlbmd0aCkgewogICAgICBpbnRlcnNlY3Rpb24gPSBmaW5kTGluZUludGVyc2Vj
dGlvbihwb2ludHNbcG9pbnRzLmxlbmd0aC0xXSwgcG9pbnRzWzBdLCBjZW50ZXJB
LCBjZW50ZXJCKTsKICAgIH0KICAgIGlmICghIGludGVyc2VjdGlvbikgewogICAg
ICBjb25zb2xlLmxvZygidW5hYmxlIHRvIGZpbmQgaW50ZXJzZWN0aW9uISB1c2lu
ZyB0YXJnZXQgcG9pbnQiLCBwb2ludHMsIGNlbnRlckEsIGNlbnRlckIpOwoKICAg
ICAgLy9kZWJ1Z2dpbmcgY29kZQogICAgICAvLyB2YXIgY29sb3IgPSBkMy5zY2Fs
ZS5jYXRlZ29yeTEwKCkuZG9tYWluKGQzLnJhbmdlKDAsMTApKShncmFwaDIuY3Vy
clRpbWUpOwogICAgICAvLyBkMy5zZWxlY3QoJy5jb250YWluZXInKS5hcHBlbmQo
J3BhdGgnKS5hdHRyKCdkJywgIk0gIitjZW50ZXJBWzBdICsgJyAnK2NlbnRlckFb
MV0gKyAnIEwgJysgY2VudGVyQlswXSArICcgJytjZW50ZXJCWzFdKS5zdHlsZSh7
J3N0cm9rZSc6IGNvbG9yfSkKICAgICAgLy8gZm9yKGkgPSAwOyBpPHBvaW50cy5s
ZW5ndGgtMTsgaSsrKSB7CiAgICAgIC8vICAgZDMuc2VsZWN0KCcuY29udGFpbmVy
JykuYXBwZW5kKCdwYXRoJykuYXR0cignZCcsICJNICIrcG9pbnRzW2ldWzBdICsg
JyAnK3BvaW50c1tpXVsxXSArICcgTCAnKyBwb2ludHNbaSsxXVswXSArICcgJytw
b2ludHNbaSsxXVsxXSkuc3R5bGUoeydzdHJva2UnOiBjb2xvcn0pCiAgICAgIC8v
IH0KICAgICAgLy8gZDMuc2VsZWN0KCcuY29udGFpbmVyJykuYXBwZW5kKCdwYXRo
JykuYXR0cignZCcsICJNICIrcG9pbnRzW3BvaW50cy5sZW5ndGgtMV1bMF0gKyAn
ICcrcG9pbnRzW3BvaW50cy5sZW5ndGgtMV1bMV0gKyAnIEwgJysgcG9pbnRzWzBd
WzBdICsgJyAnK3BvaW50c1swXVsxXSkuc3R5bGUoeydzdHJva2UnOiBjb2xvcn0p
CiAgICAgIHJldHVybiBjZW50ZXJCOyAvL0ZJWE1FIC0gdGhpcyBzaG91bGQgYmUg
ZW5hYmxlZCBmb3IgcHJvZHVjdGlvbgoKICAgIH0KICAgIHJldHVybiBpbnRlcnNl
Y3Rpb247CiAgfQoKICAvKiogY3JlYXRlcyBhIHBvbHlnb24tc2hhcGVkIHBhdGgg
YXR0cmlidXRlIGZvciBnaXZlbiBub2RlIHNlbGVjdGlvbgogICogQHBhcmFtIHtE
M3NlbGVjdGlvbn0KICAqIEBwYXJhbSB7TkRUVl9EM30KICAqIEBwcml2YXRlCiAg
Ki8KICBuMy5wcm90b3R5cGUuZHJhd1BvbHlnb25Ob2RlID0gZnVuY3Rpb24oc2Vs
ZWN0aW9uLCBuMyl7CiAgICAvLyBjb25zb2xlLnByb2ZpbGUoJ3BvbHltYXRoJykK
ICAgIC8vIGNvbnNvbGUubG9nKHJlbmRlckRhdGEubikKICAgIHNlbGVjdGlvbi5h
dHRyKHsKICAgICAgcG9pbnRzOiBmdW5jdGlvbihkLCBpKSB7IAogICAgICAgIHZh
ciBzaWRlcyA9IGRbJ3ZlcnRleC5zaWRlcyddOwogICAgICAgIHZhciBzaXplID0g
ZFsndmVydGV4LmNleCddICogbjMuYmFzZU5vZGVTaXplOwogICAgICAgIHZhciBj
b29yZHMgPSBkLnJlbmRlckNvb3JkOwogICAgICAgIHZhciByb3RhdGlvbiA9IGRb
J3ZlcnRleC5yb3QnXTsKICAgICAgICB2YXIgY2VudGVyWCA9IG4zLnhTY2FsZShj
b29yZHNbMF0pOwogICAgICAgIHZhciBjZW50ZXJZID0gbjMueVNjYWxlKGNvb3Jk
c1sxXSk7CgogICAgICAgIHZhciByb3QgPSByb3RhdGlvbiAqIDIgKiBNYXRoLlBJ
LzM2MAogICAgICAgIHZhciBiYXNlID0gMS9zaWRlcyAqIDIgKiBNYXRoLlBJOwog
ICAgICAgIG4zLm5vZGVDb29yZHNbZC5pZF0ucG9pbnRzID0gW107CgogICAgICAg
IGZvciAodmFyIGkgPSAxOyBpIDw9IHNpZGVzOyBpKyspIHsKICAgICAgICAgICAg
dmFyIGFuZyA9IGkgKiBiYXNlICsgcm90OwogICAgICAgICAgICB2YXIgeCA9IGNl
bnRlclggKyBzaXplICogTWF0aC5jb3MoYW5nKTsKICAgICAgICAgICAgdmFyIHkg
PSBjZW50ZXJZICsgc2l6ZSAqIE1hdGguc2luKGFuZyk7CiAgICAgICAgICAgIG4z
Lm5vZGVDb29yZHNbZC5pZF0ucG9pbnRzLnB1c2goW3gsIHldKTsKICAgICAgICB9
CiAgICAgICAgcmV0dXJuIG4zLm5vZGVDb29yZHNbZC5pZF0ucG9pbnRzLm1hcChm
dW5jdGlvbihwKSB7IHJldHVybiBwWzBdLnRvRml4ZWQoMSkrJywnK3BbMV0udG9G
aXhlZCgxKTsgfSkuam9pbignICcpOwogICAgICB9LAogICAgfSkKICAgIC8vIGNv
bnNvbGUucHJvZmlsZUVuZCgncG9seW1hdGgnKQogIH0KCiAgLyoqIGNyZWF0ZXMg
Y2lyY2xlIGF0dHJpYnV0ZXMgZm9yIGdpdmVuIG5vZGUgc2VsZWN0aW9uCiAgKiBA
cGFyYW0ge0Qzc2VsZWN0aW9ufQogICogQHBhcmFtIHtORFRWX0QzfQogICogQHBy
aXZhdGUKICAqLwogIG4zLnByb3RvdHlwZS5kcmF3Q2lyY2xlTm9kZSA9IGZ1bmN0
aW9uKHNlbGVjdGlvbiwgbjMpewogICAgc2VsZWN0aW9uLmF0dHIoewogICAgICBj
eDogZnVuY3Rpb24oZCwgaSkgeyByZXR1cm4gbjMueFNjYWxlKGQucmVuZGVyQ29v
cmRbMF0pLnRvRml4ZWQoMSk7IH0sCiAgICAgIGN5OiBmdW5jdGlvbihkLCBpKSB7
IHJldHVybiBuMy55U2NhbGUoZC5yZW5kZXJDb29yZFsxXSkudG9GaXhlZCgxKTsg
fSwKICAgICAgcjogZnVuY3Rpb24oZCwgaSkgeyByZXR1cm4gKGRbJ3ZlcnRleC5j
ZXgnXSAqIG4zLmJhc2VOb2RlU2l6ZSkudG9GaXhlZCgxKTsgfSwKICAgIH0pCiAg
fQoKICAvKiogcG9zaXRpb25zIHRoZSBub2RlIGxhYmVscwogICogQHBhcmFtIHtE
M3NlbGVjdGlvbn0KICAqIEBwYXJhbSB7TkRUVl9EM30KICAqIEBwcml2YXRlCiAg
Ki8KICBuMy5wcm90b3R5cGUuZHJhd05vZGVMYWJlbCA9IGZ1bmN0aW9uKHNlbGVj
dGlvbiwgbjMpewogICAgc2VsZWN0aW9uLmF0dHIoewogICAgICB4OiBmdW5jdGlv
bihkLCBpKSB7IHJldHVybiAobjMueFNjYWxlKGQucmVuZGVyQ29vcmRbMF0pK24z
Lm9wdGlvbnMubGFiZWxPZmZzZXQueCkudG9GaXhlZCgxKTsgfSwKICAgICAgeTog
ZnVuY3Rpb24oZCwgaSkgeyByZXR1cm4gKG4zLnlTY2FsZShkLnJlbmRlckNvb3Jk
WzFdKStuMy5vcHRpb25zLmxhYmVsT2Zmc2V0LnkpLnRvRml4ZWQoMSk7IH0sCiAg
ICB9KQogIH0KCiAgLyoqIGhpZ2hsaWdodHMgdGhlIGN1cnJlbnRseSBzZWxlY3Rl
ZCBuZXR3b3JrICovCiAgbjMucHJvdG90eXBlLnVwZGF0ZVNlbGVjdGVkTmV0d29y
ayA9IGZ1bmN0aW9uKCkgewogICAgdmFyIG4zID0gdGhpczsKICAgIHZhciBub2Rl
ID0gbjMuc2VsZWN0ZWROZXR3b3JrOwogICAgJC5lYWNoKFsnbm9kZV9ncm91cCcs
ICdlZGdlJywgJ2xhYmVsJ10sIGZ1bmN0aW9uKGksIGNsYXNzbmFtZSkgewogICAg
ICB2YXIgc2VsZWN0aW9uID0gbjMuY29udGFpbmVyLnNlbGVjdEFsbCgnLicrY2xh
c3NuYW1lKQogICAgICB2YXIgdHlwZSA9IGNsYXNzbmFtZSA9PSAnbm9kZV9ncm91
cCcgPyAnbm9kZScgOiBjbGFzc25hbWU7CiAgICAgIHZhciB1bnNlbGVjdGVkVGFy
Z2V0Q2xhc3MgPSAnLicrdHlwZSsncyc7CiAgICAgIHZhciBzZWxlY3RlZFRhcmdl
dENsYXNzID0gJy4nK3R5cGUrJ3Nfc2VsZWN0ZWQnOwogICAgICB2YXIgdW5zZWxl
Y3RlZFRhcmdldCA9IG4zLmNvbnRhaW5lci5zZWxlY3QodW5zZWxlY3RlZFRhcmdl
dENsYXNzKS5ub2RlKCk7CiAgICAgIHZhciBzZWxlY3RlZFRhcmdldCA9IG4zLmNv
bnRhaW5lci5zZWxlY3Qoc2VsZWN0ZWRUYXJnZXRDbGFzcykubm9kZSgpOwoKICAg
ICAgc2VsZWN0aW9uLmVhY2goZnVuY3Rpb24oZCl7CiAgICAgICAgdmFyIHRhcmdl
dCA9IHVuc2VsZWN0ZWRUYXJnZXQ7CiAgICAgICAgdmFyIHRhcmdldENsYXNzID0g
dW5zZWxlY3RlZFRhcmdldENsYXNzOwogICAgICAgIGlmICh0eXBlID09ICdlZGdl
JykgewogICAgICAgICAgaWYgKG5vZGUgJiYgKGQuaW5sLmlkID09IG5vZGUuaWQg
fHwgZC5vdXRsLmlkID09IG5vZGUuaWQpKSB7CiAgICAgICAgICAgIHRhcmdldCA9
IHNlbGVjdGVkVGFyZ2V0OwogICAgICAgICAgICB0YXJnZXRDbGFzcyA9IHNlbGVj
dGVkVGFyZ2V0Q2xhc3M7CiAgICAgICAgICB9CiAgICAgICAgfSBlbHNlIHsKICAg
ICAgICAgIGlmIChub2RlICYmIChub2RlLmxpbmtzW2QuaWRdICE9PSB1bmRlZmlu
ZWQgfHwgZC5pZCA9PSBub2RlLmlkKSkgewogICAgICAgICAgICB0YXJnZXQgPSBz
ZWxlY3RlZFRhcmdldDsKICAgICAgICAgICAgdGFyZ2V0Q2xhc3MgPSBzZWxlY3Rl
ZFRhcmdldENsYXNzOwogICAgICAgICAgfQogICAgICAgIH0KICAgICAgICAvLyBj
b25zb2xlLmxvZyh0YXJnZXRDbGFzcyk7CiAgICAgICAgaWYgKCEgJCh0aGlzKS5w
YXJlbnQodGFyZ2V0Q2xhc3MpLmxlbmd0aCkgewogICAgICAgICAgLy8gY29uc29s
ZS5sb2cobjMuY3VyclRpbWUpCiAgICAgICAgICAkKHRhcmdldCkuYXBwZW5kKHRo
aXMpOwogICAgICAgIH0KICAgICAgfSkgICAgCiAgICB9KSAgICAKICB9CgogIC8q
KiB1bmhpZ2hsaWdodHMgdGhlIGN1cnJlbnRseSBzZWxlY3RlZCBuZXR3b3JrICov
CiAgbjMucHJvdG90eXBlLnVuU2VsZWN0TmV0d29yayA9IGZ1bmN0aW9uKCkgewog
ICAgdmFyIG4zID0gdGhpczsKICAgIG4zLnNlbGVjdGVkTmV0d29yayA9IG51bGw7
CiAgICBuMy5jb250YWluZXIuc2VsZWN0KCcuc2NyZWVuJykuY2xhc3NlZCh7J25l
dHdvcmstc2VsZWN0ZWQnOiBmYWxzZX0pOwogICAgbjMudXBkYXRlU2VsZWN0ZWRO
ZXR3b3JrKCk7CiAgfQoKICAvKiogcmVuZGVyIHRoZSBncmFwaCB0byByZWZsZWN0
IHRoZSBzdGF0ZSBhdCBjdXJyVGltZSwgdHJhbnNpdGlvbmluZyBlbGVtZW50cyBv
dmVyIGEgZ2l2ZW4gZHVyYXRpb24KICAqIEBwYXJhbSB7bWlsbGlzZWNvbmRzfSAt
IHRoZSBhbW91bnQgb2YgdGltZSB0aGUgdHJhbnNpdGlvbiBhbmltYXRpb24gc2hv
dWxkIHRha2UKICAqLwogIG4zLnByb3RvdHlwZS51cGRhdGVHcmFwaCA9IGZ1bmN0
aW9uKGR1cmF0aW9uKSB7CiAgICB2YXIgbjMgPSB0aGlzOwogICAgLy8gY29uc29s
ZS5wcm9maWxlKCd1cGRhdGUgJytuMy5jdXJyVGltZSk7CgogICAgdmFyIHJlbmRl
ckRhdGEgPSBuMy51cGRhdGVTbGljZVJlbmRlckRhdGEobjMuY3VyclRpbWUpOwog
ICAgbjMuZnJhbWVJbmZvRGl2Lmh0bWwobjMuY3VyclRpbWUrJzogJytuMy50aW1l
SW5kZXhbbjMuY3VyclRpbWVdLnN0YXJ0ICsgJy0nK24zLnRpbWVJbmRleFtuMy5j
dXJyVGltZV0uZW5kKQoKICAgIHZhciBlbnRlckV4aXREdXJhdGlvbiA9IGR1cmF0
aW9uICogbjMub3B0aW9ucy5lbnRlckV4aXRBbmltYXRpb25GYWN0b3I7CiAgICB2
YXIgdXBkYXRlRHVyYXRpb24gPSBkdXJhdGlvbiAqICgxLW4zLm9wdGlvbnMuZW50
ZXJFeGl0QW5pbWF0aW9uRmFjdG9yKTsKCiAgICAkLmVhY2goWydtYWluJywgJ3hs
YWInXSwgZnVuY3Rpb24oaSwgdHlwZSl7CiAgICAgIHZhciB0ZXh0ID0gcmVuZGVy
RGF0YS5ncmFwaFt0eXBlXTsKICAgICAgdmFyIHRhcmdldCA9IG4zLmRvbVRhcmdl
dC5zZWxlY3QoJy4nK3R5cGUrJyB0ZXh0Jyk7CiAgICAgIHRhcmdldC5zZWxlY3RB
bGwoJyonKS5yZW1vdmUoKTsKCiAgICAgIGlmICh0ZXh0KSB7CiAgICAgICAgJC5l
YWNoKHRleHQsIGZ1bmN0aW9uKGksIHQpewogICAgICAgICAgdGFyZ2V0LmFwcGVu
ZCgndHNwYW4nKS5hdHRyKHsKICAgICAgICAgICAgJ2R5JzogKGkgPyAnMS4yZW0n
IDogMCksCiAgICAgICAgICAgICd4JzogMCwKICAgICAgICAgIH0pLnRleHQodCk7
CiAgICAgICAgfSkKICAgICAgfQogICAgfSk7CgogICAgbjMuZG9tVGFyZ2V0LnNl
bGVjdEFsbCgnLmJhY2tncm91bmQsIC5zY3JlZW4nKS50cmFuc2l0aW9uKCkKICAg
ICAgLnN0eWxlKHtmaWxsOiByZW5kZXJEYXRhLmdyYXBoWydiZyddLCAnZmlsbC1v
cGFjaXR5JzogcmVuZGVyRGF0YS5ncmFwaFsnYmcuZmlsbC1vcGFjaXR5J119KTsK
CiAgICB2YXIgc2hvd0luZm8gPSBmdW5jdGlvbihkKSB7CiAgICAgIGlmKCEgbjMu
c2VsZWN0ZWQgfHwgbjMuc2VsZWN0ZWQuaWQgIT09IGQuaWQpIHsKICAgICAgICBu
My5zZWxlY3RlZCA9IGQ7CiAgICAgICAgbjMubW92ZVRvb2x0aXAoKTsKICAgICAg
fSBlbHNlIHsKICAgICAgICBuMy5oaWRlVG9vbHRpcCgpOwogICAgICB9CiAgICB9
CgogICAgLy91cGRhdGUgc2VsZWN0ZWQgaXRlbQogICAgaWYgKG4zLnNlbGVjdGVk
KSB7CiAgICAgIG4zLnNlbGVjdGVkID0gbjMuc2VsZWN0ZWQuaW5sID8gcmVuZGVy
RGF0YS5lZGdlW24zLnNlbGVjdGVkLmlkXSA6IHJlbmRlckRhdGEubm9kZVtuMy5z
ZWxlY3RlZC5pZF07CiAgICAgIGlmICghIG4zLnNlbGVjdGVkKSB7CiAgICAgICAg
bjMuaGlkZVRvb2x0aXAoKTsKICAgICAgfSAKICAgIH0KICAgIGlmIChuMy5zZWxl
Y3RlZE5ldHdvcmspIHsKICAgICAgbjMuc2VsZWN0ZWROZXR3b3JrID0gcmVuZGVy
RGF0YS5ub2RlW24zLnNlbGVjdGVkTmV0d29yay5pZF07CiAgICB9CgogICAgLyoq
IGFwcGx5IHN0eWxlcyBhbmQgYXRycmlidXRlcyB0byBub2RlcwogICAgKiBAcHJp
dmF0ZQogICAgKi8KICAgIHZhciBzdHlsZU5vZGVzID0gZnVuY3Rpb24oc2VsZWN0
aW9uKSB7CiAgICAgIHNlbGVjdGlvbi5zdHlsZSh7CiAgICAgICAgLy8gJ2ZpbGwn
OiBmdW5jdGlvbihkLCBpKSB7cmV0dXJuIGRbJ3ZlcnRleC5jb2wnXTsgfSwKICAg
ICAgICBmaWxsOiBmdW5jdGlvbihkKSB7CiAgICAgICAgICBpZiAoZC5pbWFnZSkg
eyAKICAgICAgICAgICAgcmV0dXJuICd1cmwoI2ltYWdlXycrcGFyc2VJbnQoZC5p
ZCkrJyknOyAKICAgICAgICAgIH0gZWxzZSB7CiAgICAgICAgICAgIHJldHVybiBk
Wyd2ZXJ0ZXguY29sJ107CiAgICAgICAgICB9CiAgICAgICAgfSwKCiAgICAgICAg
J2ZpbGwtb3BhY2l0eSc6IGZ1bmN0aW9uKGQsIGkpIHtyZXR1cm4gZFsndmVydGV4
LmNvbC5maWxsLW9wYWNpdHknXTsgfSwKICAgICAgICAnc3Ryb2tlLXdpZHRoJzog
ZnVuY3Rpb24oZCkge3JldHVybiBkWyd2ZXJ0ZXgubHdkJ107IH0sCiAgICAgICAg
J3N0cm9rZSc6IGZ1bmN0aW9uKGQpIHtyZXR1cm4gZFsndmVydGV4LmJvcmRlcidd
OyB9LAogICAgICAgICdzdHJva2Utb3BhY2l0eSc6IGZ1bmN0aW9uKGQpIHtyZXR1
cm4gZFsndmVydGV4LmJvcmRlci5zdHJva2Utb3BhY2l0eSddOyB9LAogICAgICB9
KQogICAgICBzZWxlY3Rpb24uZmlsdGVyKCdjaXJjbGUnKS5jYWxsKG4zLmRyYXdD
aXJjbGVOb2RlLCBuMykKICAgICAgc2VsZWN0aW9uLmZpbHRlcigncG9seWdvbicp
LmNhbGwobjMuZHJhd1BvbHlnb25Ob2RlLCBuMykKICAgIH0KCiAgICAvKiogc2V0
IGF0dHJpYnV0ZXMgJiB0cmFuc2l0aW9ucyB0byBiZSBhcHBsaWVkIHRvIG5ldyBu
b2RlcwogICAgKiBAcHJpdmF0ZQogICAgKi8KICAgIHZhciBjcmVhdGVOb2RlcyA9
IGZ1bmN0aW9uKHNlbGVjdGlvbikgewogICAgICBzZWxlY3Rpb24KICAgICAgICAu
YXR0cih7CiAgICAgICAgICBjbGFzczogZnVuY3Rpb24oZCkgeyByZXR1cm4gJ25v
ZGUgbm9kZV8nK2QuaWQrJyAnKyhkWyd2ZXJ0ZXguY3NzLmNsYXNzJ10gfHwgJycp
OyB9LAogICAgICAgICAgb3BhY2l0eTogMCwKICAgICAgICAgIC8vIGZpbGw6IGZ1
bmN0aW9uKGQpIHsgcmV0dXJuICd1cmwoI2ltYWdlXycrZC5pZCsnKSc7IH0KICAg
ICAgICB9KQogICAgICAgIC5jYWxsKHN0eWxlTm9kZXMpCiAgICAgICAgLm9uKCdj
bGljaycsIHNob3dJbmZvKQogICAgICAgIC5vbignZGJsY2xpY2snLCBmdW5jdGlv
bihkKSB7CiAgICAgICAgICBpZiAoISBuMy5zZWxlY3RlZE5ldHdvcmsgfHwgZC5p
ZCAhPSBuMy5zZWxlY3RlZE5ldHdvcmsuaWQpIHsKICAgICAgICAgICAgbjMuc2Vs
ZWN0ZWROZXR3b3JrID0gZDsKICAgICAgICAgIH0gZWxzZSB7CiAgICAgICAgICAg
IG4zLnNlbGVjdGVkTmV0d29yayA9IG51bGw7CiAgICAgICAgICB9CiAgICAgICAg
ICBuMy51cGRhdGVTZWxlY3RlZE5ldHdvcmsoKQogICAgICAgICAgbjMuY29udGFp
bmVyLnNlbGVjdCgnLnNjcmVlbicpLmNsYXNzZWQoeyduZXR3b3JrLXNlbGVjdGVk
JzogbjMuc2VsZWN0ZWROZXR3b3JrfSkKICAgICAgICAgIG4zLnNlbGVjdGVkID0g
bjMuc2VsZWN0ZWROZXR3b3JrOwogICAgICAgICAgbjMubW92ZVRvb2x0aXAoKTsK
ICAgICAgICAgIGQzLmV2ZW50LnN0b3BQcm9wYWdhdGlvbigpOwogICAgICAgIH0p
CiAgICAgICAgLm9uKCdtb3VzZW92ZXInLCBmdW5jdGlvbihkKSB7CiAgICAgICAg
ICBkMy5zZWxlY3QodGhpcykKICAgICAgICAgICAgLnRyYW5zaXRpb24oKQogICAg
ICAgICAgICAuZHVyYXRpb24oMjAwKQogICAgICAgICAgICAuc3R5bGUoJ3N0cm9r
ZS13aWR0aCcsIHBhcnNlRmxvYXQoZFsndmVydGV4Lmx3ZCddKSs1KTsKICAgICAg
ICB9KQogICAgICAgIC5vbignbW91c2VvdXQnLCBmdW5jdGlvbihkKSB7CiAgICAg
ICAgICBkMy5zZWxlY3QodGhpcykKICAgICAgICAgICAgLnRyYW5zaXRpb24oKQog
ICAgICAgICAgICAuZHVyYXRpb24oMjAwKQogICAgICAgICAgICAuc3R5bGUoJ3N0
cm9rZS13aWR0aCcsIGRbJ3ZlcnRleC5sd2QnXSk7CiAgICAgICAgfSkKICAgICAg
ICAudHJhbnNpdGlvbigpCiAgICAgICAgLmR1cmF0aW9uKGVudGVyRXhpdER1cmF0
aW9uKQogICAgICAgIC5hdHRyKCdvcGFjaXR5JywgMSkKCiAgICB9CgogICAgdmFy
IGltYWdlcyA9IG4zLmRvbVRhcmdldC5zZWxlY3QoJ2RlZnMnKS5zZWxlY3RBbGwo
Jy5ub2RlLWltYWdlJykuZGF0YShkMy52YWx1ZXMocmVuZGVyRGF0YS5ub2RlKS5m
aWx0ZXIoZnVuY3Rpb24oZCkgeyByZXR1cm4gZC5pbWFnZTsgfSksIGZ1bmN0aW9u
KG4pIHsgcmV0dXJuIG4uaWQ7IH0pCiAgICAgIGltYWdlcy5lbnRlcigpCiAgICAg
ICAgLmFwcGVuZCgncGF0dGVybicpLmF0dHIoewogICAgICAgIGlkOiBmdW5jdGlv
bihkKSB7IHJldHVybiAnaW1hZ2VfJytkLmlkOyB9LAogICAgICAgIGNsYXNzOiAn
bm9kZS1pbWFnZScsCiAgICAgICAgdmlld0JveDoiMCAwIDEwMCAxMDAiLAogICAg
ICAgIC8vIHByZXNlcnZlQXNwZWN0UmF0aW86Im5vbmUiLCAKICAgICAgICAvLyAn
cGF0dGVyblVuaXRzJzogJ3VzZXJTcGFjZU9uVXNlJywKICAgICAgICB3aWR0aDog
MTAsCiAgICAgICAgaGVpZ2h0OjEwCiAgICAgIH0pLmFwcGVuZCgnaW1hZ2UnKS5h
dHRyKHsKICAgICAgICAneGxpbms6aHJlZic6IGZ1bmN0aW9uKGQpIHsgcmV0dXJu
IGQuaW1hZ2U7IH0sCiAgICAgICAgd2lkdGg6IDEwLAogICAgICAgIGhlaWdodDox
MCAgICAgICAgCiAgICAgIH0pCgogICAgICBpbWFnZXMuc2VsZWN0KCdpbWFnZScp
LnRyYW5zaXRpb24oKQogICAgICAgIC5kZWxheShlbnRlckV4aXREdXJhdGlvbikK
ICAgICAgICAuZHVyYXRpb24odXBkYXRlRHVyYXRpb24pCiAgICAgICAgLmF0dHIo
ewogICAgICAgICAgJ3hsaW5rOmhyZWYnOiBmdW5jdGlvbihkKSB7IHJldHVybiBk
LmltYWdlOyB9CiAgICAgICAgfSk7CgogICAgICBpbWFnZXMuZXhpdCgpCiAgICAg
ICAgLnRyYW5zaXRpb24oKQogICAgICAgIC5kdXJhdGlvbihlbnRlckV4aXREdXJh
dGlvbikKICAgICAgICAucmVtb3ZlKCk7IAoKICAgIHZhciBub2RlcyA9IG4zLmNv
bnRhaW5lci5zZWxlY3RBbGwoJy5ub2RlJykuZGF0YShkMy52YWx1ZXMocmVuZGVy
RGF0YS5ub2RlKSwgZnVuY3Rpb24oZSkgeyByZXR1cm4gZS5pZDsgfSkKCiAgICAg
IHZhciBub2RlX2dyb3VwcyA9IG5vZGVzLmVudGVyKCkuYXBwZW5kKCdnJykuY2xh
c3NlZCh7J25vZGVfZ3JvdXAnIDogdHJ1ZX0pOwogICAgICBub2RlX2dyb3Vwcy5m
aWx0ZXIoZnVuY3Rpb24oZCkgeyByZXR1cm4gZFsndmVydGV4LnNpZGVzJ10gIT0g
NTA7IH0pLmFwcGVuZCgncG9seWdvbicpLmNhbGwoY3JlYXRlTm9kZXMpOwogICAg
ICBub2RlX2dyb3Vwcy5maWx0ZXIoZnVuY3Rpb24oZCkgeyByZXR1cm4gZFsndmVy
dGV4LnNpZGVzJ10gPT0gNTA7IH0pLmFwcGVuZCgnY2lyY2xlJykuY2FsbChjcmVh
dGVOb2Rlcyk7CgogICAgICBpZiAoIWVudGVyRXhpdER1cmF0aW9uKSB7bm9kZXMu
YXR0cih7b3BhY2l0eTogMX0pOyB9CgogICAgICBub2Rlcy5maWx0ZXIoJy5ub2Rl
JykudHJhbnNpdGlvbigpCiAgICAgICAgLmRlbGF5KGVudGVyRXhpdER1cmF0aW9u
KQogICAgICAgIC5kdXJhdGlvbih1cGRhdGVEdXJhdGlvbikKICAgICAgICAuYXR0
cignb3BhY2l0eScsIDEpCiAgICAgICAgLmNhbGwoc3R5bGVOb2RlcykKCiAgICAg
IG5vZGVzLmV4aXQoKQogICAgICAgIC50cmFuc2l0aW9uKCkKICAgICAgICAuZHVy
YXRpb24oZW50ZXJFeGl0RHVyYXRpb24pCiAgICAgICAgLmF0dHIoJ29wYWNpdHkn
LCAwKQogICAgICAgIC5yZW1vdmUoKTsgCgogICAgaWYgKHJlbmRlckRhdGEuZ3Jh
cGgudXNlYXJyb3dzKSB7CiAgICAgIHZhciBtYXJrZXJzID0gbjMuZG9tVGFyZ2V0
LnNlbGVjdCgnZGVmcycpLnNlbGVjdEFsbCgnLmFycm93aGVhZCcpLmRhdGEoZDMu
dmFsdWVzKHJlbmRlckRhdGEuZWRnZSksIGZ1bmN0aW9uKGUpIHsgcmV0dXJuIGUu
aWR9KQogICAgICAgIG1hcmtlcnMuZW50ZXIoKS5hcHBlbmQoJ21hcmtlcicpLmF0
dHIoewogICAgICAgICAgaWQ6IGZ1bmN0aW9uKGQpIHsgcmV0dXJuICdhcnJvd2hl
YWRfJytkLmlkOyB9LAogICAgICAgICAgY2xhc3M6ICdhcnJvd2hlYWQnLAogICAg
ICAgICAgdmlld0JveDogIjAgLTEgMiAyIiwKICAgICAgICAgIG1hcmtlcldpZHRo
OiBzY2FsZUFycm93aGVhZHMsCiAgICAgICAgICBtYXJrZXJIZWlnaHQ6IHNjYWxl
QXJyb3doZWFkcywKICAgICAgICAgIG9yaWVudDogImF1dG8iLAogICAgICAgIH0p
LmFwcGVuZCgic3ZnOnBhdGgiKQogICAgICAgICAgLmF0dHIoewogICAgICAgICAg
ICBkOiAiTTAsLTFMMiwwTDAsMSIsCiAgICAgICAgICAgIGZpbGw6ICdncmVlbicK
ICAgICAgICAgIH0pOwoKICAgICAgICBtYXJrZXJzLnNlbGVjdCgncGF0aCcpLnRy
YW5zaXRpb24oKQogICAgICAgICAgLmRlbGF5KGVudGVyRXhpdER1cmF0aW9uKQog
ICAgICAgICAgLmR1cmF0aW9uKHVwZGF0ZUR1cmF0aW9uKQogICAgICAgICAgLmF0
dHIoewogICAgICAgICAgICBmaWxsOiBmdW5jdGlvbihkKSB7IHJldHVybiBkWydl
ZGdlLmNvbCddOyB9LAogICAgICAgICAgICAnZmlsbC1vcGFjaXR5JzogZnVuY3Rp
b24oZCkgeyByZXR1cm4gZFsnZWRnZS5jb2wuc3Ryb2tlLW9wYWNpdHknXTsgfQog
ICAgICAgICAgfSkKCiAgICAgICAgbWFya2Vycy5leGl0KCkuc2VsZWN0QWxsKCdw
YXRoJykKICAgICAgICAgIC5hdHRyKHsKICAgICAgICAgICAgZmlsbDogJ3JlZCcK
ICAgICAgICAgIH0pCiAgICAgICAgCiAgICAgICAgbWFya2Vycy5leGl0KCkudHJh
bnNpdGlvbigpCiAgICAgICAgICAuZGVsYXkoZHVyYXRpb24pCiAgICAgICAgICAu
ZHVyYXRpb24oMCkKICAgICAgICAgIC5yZW1vdmUoKQogICAgfQoKICAgIHZhciBs
aW5lcyA9IG4zLmNvbnRhaW5lci5zZWxlY3RBbGwoJy5lZGdlJykuZGF0YShkMy52
YWx1ZXMocmVuZGVyRGF0YS5lZGdlKSwgZnVuY3Rpb24oZSkgeyByZXR1cm4gZS5p
ZH0pCiAgICAgIGxpbmVzLmVudGVyKCkuYXBwZW5kKCdwYXRoJykKICAgICAgICAu
YXR0cih7CiAgICAgICAgICBjbGFzczogZnVuY3Rpb24oZCkgeyByZXR1cm4gJ2Vk
Z2UgZWRnZV8nK2QuaWQrJyAnKyhkWydlZGdlLmNzcy5jbGFzcyddIHx8ICcnKTsg
fSwgICAgIAogICAgICAgICAgb3BhY2l0eTogMCwKICAgICAgICAgICJtYXJrZXIt
ZW5kIjogZnVuY3Rpb24oZCkgeyBpZihyZW5kZXJEYXRhLmdyYXBoLnVzZWFycm93
cykgeyByZXR1cm4gInVybCgjYXJyb3doZWFkXyIrZC5pZCsiKSI7IH19CiAgICAg
ICAgfSkKICAgICAgICAuc3R5bGUoewogICAgICAgICAgJ3N0cm9rZSc6ICdncmVl
bicsCiAgICAgICAgICAnc3Ryb2tlLXdpZHRoJzogZnVuY3Rpb24oZCkgeyByZXR1
cm4gZFsnZWRnZS5sd2QnXTsgfQogICAgICAgIH0pCiAgICAgICAgLmNhbGwobjMu
ZHJhd0VkZ2UsIG4zLCByZW5kZXJEYXRhLmdyYXBoLnVzZWFycm93cywgMSkKICAg
ICAgICAub24oJ2NsaWNrJywgc2hvd0luZm8pCiAgICAgICAgLm9uKCdtb3VzZW92
ZXInLCBmdW5jdGlvbihkKSB7CiAgICAgICAgICB2YXIgZWRnZSA9IGQzLnNlbGVj
dCh0aGlzKTsKICAgICAgICAgIHZhciBsaW5lID0gJ00gJytkLmN1cnJlbnRDb29y
ZHNbMF1bMF0udG9GaXhlZCgxKSsnICcrZC5jdXJyZW50Q29vcmRzWzBdWzFdLnRv
Rml4ZWQoMSkrJyBMICcrZC5jdXJyZW50Q29vcmRzWzFdWzBdLnRvRml4ZWQoMSkr
JyAnK2QuY3VycmVudENvb3Jkc1sxXVsxXS50b0ZpeGVkKDEpOwogICAgICAgICAg
aWYgKHJlbmRlckRhdGEuZ3JhcGgudXNlYXJyb3dzKSB7CiAgICAgICAgICAgIHZh
ciBvZmZzZXRQb2ludHMgPSBvZmZzZXRMaW5lKFtkLmN1cnJlbnRDb29yZHNbMF1b
MF0sIGQuY3VycmVudENvb3Jkc1swXVsxXV0sIFtkLmN1cnJlbnRDb29yZHNbMV1b
MF0sIGQuY3VycmVudENvb3Jkc1sxXVsxXV0sIChzY2FsZUFycm93aGVhZHMoZCkq
NSkpOwogICAgICAgICAgICBsaW5lID0gJ00gJytkLmN1cnJlbnRDb29yZHNbMF1b
MF0udG9GaXhlZCgxKSsnICcrZC5jdXJyZW50Q29vcmRzWzBdWzFdLnRvRml4ZWQo
MSkrJyBMICcrb2Zmc2V0UG9pbnRzWzBdLnRvRml4ZWQoMSkrJyAnK29mZnNldFBv
aW50c1sxXS50b0ZpeGVkKDEpOyAgICAgCiAgICAgICAgICB9CiAgICAgICAgICBl
ZGdlLnRyYW5zaXRpb24oKQogICAgICAgICAgICAuZHVyYXRpb24oMjAwKQogICAg
ICAgICAgICAuc3R5bGUoJ3N0cm9rZS13aWR0aCcsIHBhcnNlRmxvYXQoZFsnZWRn
ZS5sd2QnXSkrNSkKICAgICAgICAgICAgLmF0dHIoJ2QnLCBsaW5lKTsKICAgICAg
ICB9KQogICAgICAgIC5vbignbW91c2VvdXQnLCBmdW5jdGlvbihkKSB7CiAgICAg
ICAgICBkMy5zZWxlY3QodGhpcykudHJhbnNpdGlvbigpCiAgICAgICAgICAgIC5k
dXJhdGlvbigyMDApCiAgICAgICAgICAgIC5zdHlsZSgnc3Ryb2tlLXdpZHRoJywg
ZFsnZWRnZS5sd2QnXSkKICAgICAgICAgICAgLmNhbGwobjMuZHJhd0VkZ2UsIG4z
LCByZW5kZXJEYXRhLmdyYXBoLnVzZWFycm93cywgZmFsc2UpCiAgICAgICAgfSkK
ICAgICAgICAudHJhbnNpdGlvbigpCiAgICAgICAgLmR1cmF0aW9uKGVudGVyRXhp
dER1cmF0aW9uKQogICAgICAgIC5hdHRyKHtvcGFjaXR5OiAxfSkKCiAgICAgIGlm
ICghZW50ZXJFeGl0RHVyYXRpb24pIHtsaW5lcy5hdHRyKHtvcGFjaXR5OiAxfSk7
IH0KICAgICAgCiAgICAgIGxpbmVzLnRyYW5zaXRpb24oKQogICAgICAgIC5kZWxh
eShlbnRlckV4aXREdXJhdGlvbikKICAgICAgICAuZHVyYXRpb24odXBkYXRlRHVy
YXRpb24pCiAgICAgICAgLnN0eWxlKHsKICAgICAgICAgICdzdHJva2UnOiBmdW5j
dGlvbihkKSB7IHJldHVybiBkWydlZGdlLmNvbCddfSwKICAgICAgICAgICdzdHJv
a2Utb3BhY2l0eSc6IGZ1bmN0aW9uKGQpIHsgcmV0dXJuIGRbJ2VkZ2UuY29sLnN0
cm9rZS1vcGFjaXR5J107IH0sCiAgICAgICAgICAnc3Ryb2tlLXdpZHRoJzogZnVu
Y3Rpb24oZCkgeyByZXR1cm4gZFsnZWRnZS5sd2QnXTsgfSwKICAgICAgICB9KQog
ICAgICAgIC5jYWxsKG4zLmRyYXdFZGdlLCBuMywgcmVuZGVyRGF0YS5ncmFwaC51
c2VhcnJvd3MpCiAgICAgICAgLmF0dHIoe29wYWNpdHk6IDF9KQoKICAgICAgbGlu
ZXMuZXhpdCgpCiAgICAgICAgLnN0eWxlKCdzdHJva2UnLCAncmVkJykKICAgICAg
ICAudHJhbnNpdGlvbigpCiAgICAgICAgLmR1cmF0aW9uKGVudGVyRXhpdER1cmF0
aW9uKQogICAgICAgIC5hdHRyKCdvcGFjaXR5JywgMCkgICAgICAgICAgCiAgICAg
ICAgLnJlbW92ZSgpOwoKICAgIHZhciBsYWJlbHMgPSBuMy5jb250YWluZXIuc2Vs
ZWN0KCcubGFiZWxzJykuc2VsZWN0QWxsKCd0ZXh0JykuZGF0YShkMy52YWx1ZXMo
cmVuZGVyRGF0YS5ub2RlKSwgZnVuY3Rpb24oZSkgeyByZXR1cm4gZS5pZH0pOwog
ICAgICBsYWJlbHMuZW50ZXIoKS5hcHBlbmQoJ3RleHQnKS5maWx0ZXIoZnVuY3Rp
b24oZCkgeyByZXR1cm4gcmVuZGVyRGF0YS5ncmFwaC5kaXNwbGF5bGFiZWxzOyB9
KQogICAgICAgIC5hdHRyKHsKICAgICAgICAgIGNsYXNzOiBmdW5jdGlvbihkKSB7
IHJldHVybiAnbGFiZWwgbGFiZWxfJytkLmlkKyAnICcrIChkWyd2ZXJ0ZXgubGFi
ZWwuY3NzLmNsYXNzJ10gfHwgJycpOyB9LAogICAgICAgICAgb3BhY2l0eTogMAog
ICAgICAgIH0pCiAgICAgICAgLmNhbGwobjMuZHJhd05vZGVMYWJlbCwgbjMpCiAg
ICAgICAgLnRleHQoZnVuY3Rpb24oZCwgaSkgeyByZXR1cm4gZC5sYWJlbDsgfSkK
ICAgICAgICAuc3R5bGUoewogICAgICAgICAgJ2ZpbGwnOiBmdW5jdGlvbihkKSB7
cmV0dXJuIGRbJ2xhYmVsLmNvbCddOyB9LAogICAgICAgICAgJ2ZpbGwtb3BhY2l0
eSc6IGZ1bmN0aW9uKGQpIHtyZXR1cm4gZFsnbGFiZWwuY29sLmZpbGwtb3BhY2l0
eSddOyB9LAogICAgICAgICAgJ2ZvbnQtc2l6ZSc6IGZ1bmN0aW9uKGQpIHsgcmV0
dXJuIG4zLm9wdGlvbnMuYmFzZUZvbnRTaXplICogZFsnbGFiZWwuY2V4J10rJ3B4
Jzt9CiAgICAgICAgfSkKICAgICAgICAudHJhbnNpdGlvbigpCiAgICAgICAgLmRl
bGF5KDApCiAgICAgICAgLmR1cmF0aW9uKGVudGVyRXhpdER1cmF0aW9uKQogICAg
ICAgIC5hdHRyKHtvcGFjaXR5OiAxfSkKCiAgICAgIGlmICghZW50ZXJFeGl0RHVy
YXRpb24pIHtsYWJlbHMuYXR0cih7b3BhY2l0eTogMX0pOyB9CgogICAgICBsYWJl
bHMudHJhbnNpdGlvbigpLmZpbHRlcihmdW5jdGlvbihkKSB7IHJldHVybiByZW5k
ZXJEYXRhLmdyYXBoLmRpc3BsYXlsYWJlbHM7IH0pCiAgICAgICAgLmRlbGF5KGVu
dGVyRXhpdER1cmF0aW9uKQogICAgICAgIC5kdXJhdGlvbih1cGRhdGVEdXJhdGlv
bikKICAgICAgICAuY2FsbChuMy5kcmF3Tm9kZUxhYmVsLCBuMykKICAgICAgICAv
Ly5hdHRyKHtvcGFjaXR5OiAxfSkKICAgICAgICAudGV4dChmdW5jdGlvbihkLCBp
KSB7IHJldHVybiBkLmxhYmVsOyB9KQogICAgICAgIC5zdHlsZSh7CiAgICAgICAg
ICAnZmlsbCc6IGZ1bmN0aW9uKGQpIHtyZXR1cm4gZFsnbGFiZWwuY29sJ107IH0s
CiAgICAgICAgICAnZmlsbC1vcGFjaXR5JzogZnVuY3Rpb24oZCkge3JldHVybiBk
WydsYWJlbC5jb2wuZmlsbC1vcGFjaXR5J107IH0sCiAgICAgICAgfSkKCiAgICAg
IGxhYmVscy5leGl0KCkKICAgICAgICAudHJhbnNpdGlvbigpCiAgICAgICAgLmR1
cmF0aW9uKGVudGVyRXhpdER1cmF0aW9uKQogICAgICAgIC5hdHRyKCdvcGFjaXR5
JywgMCkKICAgICAgICAucmVtb3ZlKCk7CgogICAgICBuMy51cGRhdGVTZWxlY3Rl
ZE5ldHdvcmsoKTsKICAKICAgICAgdmFyIHN0YXJ0ID0gRGF0ZS5ub3coKTsKICAg
ICAgZDMudGltZXIoZnVuY3Rpb24oKSB7CiAgICAgICAgaWYgKG4zLnNlbGVjdGVk
ICE9PSB1bmRlZmluZWQpIHsKICAgICAgICAgIG4zLm1vdmVUb29sdGlwKCk7CiAg
ICAgICAgfQogICAgICAgIHJldHVybiBEYXRlLm5vdygpID49IHN0YXJ0ICtkdXJh
dGlvbjsgCiAgICAgIH0pCiAgICAgIC8vIGNvbnNvbGUucHJvZmlsZUVuZCgndXBk
YXRlICcrbjMuY3VyclRpbWUpOwogIH0KCiAgLyoqIHJlc2l6ZXMgZ3JhcGggYW5k
IG90aGVyIGRpc3BsYXkgZWxlbWVudHMgdG8gZmlsbCB0aGUgdGFyZ2V0IHZpZXdw
b3J0ICovCiAgbjMucHJvdG90eXBlLnJlc2l6ZUdyYXBoID0gZnVuY3Rpb24oKSB7
CiAgICB2YXIgbjMgPSB0aGlzOwogICAgbjMudXBkYXRlRGltZW5zaW9ucygpOwoK
CiAgICBuMy5jb250YWluZXIuc2VsZWN0QWxsKCdjaXJjbGUubm9kZScpLmNhbGwo
bjMuZHJhd0NpcmNsZU5vZGUsIG4zKQogICAgbjMuY29udGFpbmVyLnNlbGVjdEFs
bCgncG9seWdvbi5ub2RlJykuY2FsbChuMy5kcmF3UG9seWdvbk5vZGUsIG4zKQoK
ICAgIG4zLmNvbnRhaW5lci5zZWxlY3RBbGwoJy5lZGdlJykuY2FsbChuMy5kcmF3
RWRnZSwgbjMsIG4zLnRpbWVJbmRleFtuMy5jdXJyVGltZV0ucmVuZGVyRGF0YS5n
cmFwaC51c2VhcnJvd3MpCgogICAgbjMuY29udGFpbmVyLnNlbGVjdCgnLmxhYmVs
cycpLnNlbGVjdEFsbCgndGV4dCcpLmNhbGwobjMuZHJhd05vZGVMYWJlbCwgbjMp
CgogICAgbjMubW92ZVRvb2x0aXAoKTsKICAgIC8vcmVkcmF3IHRoZSBzbGlkZXIg
Y29udHJvbCAKICAgIGlmIChuMy5vcHRpb25zLnNsaWRlcikgewogICAgICB2YXIg
c2xpZGVyRGl2ID0gbjMuZG9tVGFyZ2V0LnNlbGVjdCgnLnNsaWRlcicpOwogICAg
ICBzbGlkZXJEaXYuaHRtbCgnJyk7CiAgICAgIHNsaWRlckRpdi5jYWxsKG4zLnNs
aWRlcik7CiAgICB9IAogIH0KICAKICAvKiogZ3JhcGggYW5pbWF0aW9uIGNvbnRy
b2xsZXIKICAqIEBwYXJhbSB7aW50ZWdlcn0gLSByZW5kZXIgdGhlIGdyYXBoIHRv
IHRoZSBzdGF0ZSBhdCB0aGlzIHRpbWVzbGljZSBpbmRleAogICogQHBhcmFtIHtp
bnRlZ2VyfSAtIGZ1bmN0aW9uIHdpbGwgcmVjdXJzaXZlbHkgY2FsbCBpdHNlbGYg
dW50aWwgdGltZSBlcXVhbHMgdGhpcyB2YWx1ZQogICogQHBhcmFtIHtib29sZWFu
fSAtIHNob3VsZCB0aGUgZ3JhcGggdXBkYXRlIGltbWVkaWF0ZWx5LCBvciBhbmlt
YXRlPwogICovCiAgbjMucHJvdG90eXBlLmFuaW1hdGVHcmFwaCA9IGZ1bmN0aW9u
KHRpbWUsIGVuZFRpbWUsIGltbWVkaWF0ZSkgewogICAgdmFyIG4zID0gdGhpczsK
ICAgIGlmICh0aW1lID4gbjMudGltZUluZGV4Lmxlbmd0aCAtMSB8fCB0aW1lIDwg
MCkgeyByZXR1cm47IH0KCiAgICB2YXIgZHVyYXRpb24gPSBpbW1lZGlhdGUgPyAw
IDogbjMub3B0aW9ucy5hbmltYXRpb25EdXJhdGlvbjsKICAgIGVuZFRpbWUgPSBl
bmRUaW1lID09PSB1bmRlZmluZWQgPyBuMy50aW1lSW5kZXgubGVuZ3RoIC0xIDog
ZW5kVGltZTsKICAgIHZhciBuZXh0VGltZTsKICAgIGlmICh0aW1lID09IGVuZFRp
bWUpIHsKICAgICAgbmV4dFRpbWUgPSB0aW1lOwogICAgfSBlbHNlIGlmIChlbmRU
aW1lID4gdGltZSkgewogICAgICBuZXh0VGltZSA9IHRpbWUgKzE7CiAgICB9IGVs
c2UgewogICAgICBuZXh0VGltZSA9IHRpbWUgLTE7CiAgICB9CgogICAgbjMuY3Vy
clRpbWUgPSB0aW1lID09IG4zLmN1cnJUaW1lID8gbmV4dFRpbWUgOiB0aW1lOwog
ICAgLy9jb25zb2xlLmxvZyhuMy5jdXJyVGltZSArICcgJyt0aW1lKycgJytlbmRU
aW1lKyAnICcrbmV4dFRpbWUrICcgJytuMy5wcmV2VGltZSkKICAgIGlmKCEgaW1t
ZWRpYXRlICYmIG4zLm9wdGlvbnMuc2xpZGVyKSB7CiAgICAgIG4zLnNsaWRlci52
YWx1ZShuMy50aW1lSW5kZXhbbjMuY3VyclRpbWVdLnN0YXJ0WzBdKTsKICAgIH0K
ICAgIG4zLnVwZGF0ZUdyYXBoKGR1cmF0aW9uKTsKICAgIGlmIChuMy5jdXJyVGlt
ZSAhPSBlbmRUaW1lKSB7CiAgICAgIG4zLmFuaW1hdGUgPSBzZXRUaW1lb3V0KGZ1
bmN0aW9uKCl7CiAgICAgICAgbjMuYW5pbWF0ZUdyYXBoKG5leHRUaW1lLCBlbmRU
aW1lLCBpbW1lZGlhdGUpOwogICAgICB9LCBkdXJhdGlvbikKICAgIH0KICB9Cgog
IC8qKiByZWRyYXcgdGhlIGluZm8gcG9wb3ZlciAvL0ZJWE1FIC0gbmVlZHMgcmVu
YW1lZCAqLwogIG4zLnByb3RvdHlwZS5tb3ZlVG9vbHRpcCA9IGZ1bmN0aW9uKCkg
ewogICAgLy8gY29uc29sZS5wcm9maWxlKCdtb3ZlVG9vbHRpcCcpOwogICAgdmFy
IG4zID0gdGhpczsKICAgIGlmIChuMy5zZWxlY3RlZCkgewogICAgICB2YXIgaXRl
bSA9IG4zLnNlbGVjdGVkOwogICAgICB2YXIgdHlwZSA9IGl0ZW0uaW5sID8gJ2Vk
Z2UnIDogJ25vZGUnOwogICAgICB2YXIgbm9kZURPTSA9IG4zLmNvbnRhaW5lci5z
ZWxlY3QoJy4nK3R5cGUrJ18nK2l0ZW0uaWQpLm5vZGUoKTsKICAgICAgaWYgKCFu
b2RlRE9NKSB7CiAgICAgICAgbjMuaGlkZVRvb2x0aXAoKTsKICAgICAgfSBlbHNl
IHsKICAgICAgICB2YXIgY29vcmRzID0gbjMuY29udmVydENvb3JkcyhpdGVtKTsK
ICAgICAgICB2YXIgcHJvcGVydHkgPSAndmVydGV4LnRvb2x0aXAnOwogICAgICAg
IGlmICh0eXBlID09ICdlZGdlJykgewogICAgICAgICAgcHJvcGVydHkgPSAnZWRn
ZS50b29sdGlwJzsKICAgICAgICB9CiAgICAgICAgdmFyIGh0bWwgPSBuMy5zZWxl
Y3RlZFtwcm9wZXJ0eV0gfHwgdHlwZSsiIGlkOiAiK24zLnNlbGVjdGVkLmlkOwog
ICAgICAgIG4zLnRvb2x0aXAuc3R5bGUoewogICAgICAgICAgZGlzcGxheTogJ2Js
b2NrJywKICAgICAgICAgIGJvdHRvbTogY29vcmRzWzFdKydweCcsCiAgICAgICAg
ICBsZWZ0OiBjb29yZHNbMF0rJ3B4JywKICAgICAgICB9KS5odG1sKGh0bWwpCiAg
ICAgIH0KICAgIH0gZWxzZSB7CiAgICAgIG4zLmhpZGVUb29sdGlwKCk7CiAgICB9
CiAgICAvLyBjb25zb2xlLnByb2ZpbGVFbmQoJ21vdmVUb29sdGlwJyk7CiAgfQoK
ICAvKiogZ2V0IGNlbnRlciBwb2ludCBvZiBlZGdlIG9yIG5vZGUsIGluIERPTSBw
aXhlbHMgKi8KICBuMy5wcm90b3R5cGUuY29udmVydENvb3JkcyA9IGZ1bmN0aW9u
KGl0ZW0pIHsKICAgIHZhciBuMyA9IHRoaXM7CiAgICB2YXIgdHlwZSA9IGl0ZW0u
aW5sID8gJ2VkZ2UnIDogJ25vZGUnOwogICAgdmFyIG5vZGVET00gPSBuMy5jb250
YWluZXIuc2VsZWN0KCcuJyt0eXBlKydfJytpdGVtLmlkKS5ub2RlKCk7CiAgICB2
YXIgY3RtID0gbjMuY3RtOwogICAgdmFyIHgsIHk7CiAgICB2YXIgYmJveCA9IG5v
ZGVET00uZ2V0QkJveCgpOwogICAgdmFyIGNlbnRlciA9IHsKICAgICAgeDogYmJv
eC54ICsgYmJveC53aWR0aC8yLAogICAgICB5OiBiYm94LnkgKyBiYm94LmhlaWdo
dC8yCiAgICB9CiAgICBpZiAodHlwZSA9PSAnbm9kZScpIHsKICAgICAgaWYgKG5v
ZGVET00udGFnTmFtZSA9PSAncG9seWdvbicpIHsKICAgICAgICB2YXIgcG9pbnRz
ID0gJCgncG9seWdvbi5ub2RlXycraXRlbS5pZCkuYXR0cigncG9pbnRzJykuc3Bs
aXQoJyAnKS5tYXAoZnVuY3Rpb24ocCkgeyAKICAgICAgICAgIHJldHVybiBwLnNw
bGl0KCcsJykubWFwKHBhcnNlRmxvYXQpOwogICAgICAgIH0pCiAgICAgICAgdmFy
IHBvaW50ID0gZmluZFBvbHlnb25JbnRlcnNlY3Rpb24ocG9pbnRzLCBbYmJveC54
ICsgYmJveC53aWR0aCwgYmJveC55XSwgW2NlbnRlci54LCBjZW50ZXIueV0pCiAg
ICAgICAgeCA9IHBvaW50WzBdOwogICAgICAgIHkgPSBwb2ludFsxXTsKICAgICAg
fSBlbHNlIHsKICAgICAgICB2YXIgc2l6ZSA9IHBhcnNlRmxvYXQobm9kZURPTS5n
ZXRBdHRyaWJ1dGUoJ3InKSk7CiAgICAgICAgdmFyIGFuZ2xlID0gLU1hdGguUEkv
NDsKICAgICAgICB4ID0gY2VudGVyLnggKyBzaXplICogTWF0aC5jb3MoYW5nbGUp
OwogICAgICAgIHkgPSBjZW50ZXIueSArIHNpemUgKiBNYXRoLnNpbihhbmdsZSk7
CiAgICAgIH0KICAgIH0gZWxzZSB7CiAgICAgIHggPSBjZW50ZXIueDsKICAgICAg
eSA9IGNlbnRlci55OwogICAgfQogICAgLy8gdmFyIG9mZnNldENvb3JkcyA9IG9m
ZnNldExpbmUoIFtjZW50ZXIueCwgY2VudGVyLnldLFt4LCB5XSxuZHR2UHJvcGVy
dGllcy5ncmFwaC50b29sdGlwT2Zmc2V0KQogICAgLy8geCA9IG9mZnNldENvb3Jk
c1swXTsKICAgIC8vIHkgPSBvZmZzZXRDb29yZHNbMV07CgogICAgdmFyIGxlZnQg
PSAoeCpjdG0uYSkgKyBjdG0uZSAtIG4zLm9mZnNldC5sZWZ0IC0gJCh3aW5kb3cp
LnNjcm9sbExlZnQoKSArMTsKICAgIHZhciBib3R0b20gPSBuMy5oZWlnaHQgLSh5
KmN0bS5kKS1jdG0uZiArIG4zLm9mZnNldC50b3AgLSAkKHdpbmRvdykuc2Nyb2xs
VG9wKCkgKzE7CiAgICByZXR1cm4gW2xlZnQsIGJvdHRvbV07CiAgfQoKICAvKiog
aGlkZSB0aGUgdG9vbHRpcCBhbmQgdW5zZXQgdGhlIHNlbGVjdGVkIGdsb2JhbCAq
LwogIG4zLnByb3RvdHlwZS5oaWRlVG9vbHRpcCA9IGZ1bmN0aW9uKCkgewogICAg
dmFyIG4zID0gdGhpczsKICAgIG4zLnNlbGVjdGVkID0gbnVsbDsKICAgIG4zLnRv
b2x0aXAuc3R5bGUoJ2Rpc3BsYXknLCAnbm9uZScpOwogIH0KCiAgLyoqIHN0b3Ag
dGhlIGN1cnJlbnQgYW5pbWF0aW9uIGN5Y2xlCiAgKiBAcGFyYW0ge2Jvb2xlYW59
IC0gaWYgdHJ1ZSwgaW1tZWRpYXRlIGhhbHQgYWxsIGFjdGl2ZSB0cmFuc2l0aW9u
cyAob3RoZXJ3aXNlLCBsZXQgYW5pbWF0aW9uIGNvbnRpbnVlIHRvIG5leHQgdGlt
ZSBzbGljZSkKICAqLwogIG4zLnByb3RvdHlwZS5lbmRBbmltYXRpb24gPSBmdW5j
dGlvbihub0hhbHQpewogICAgdmFyIG4zID0gdGhpczsKICAgIGNsZWFyVGltZW91
dChuMy5hbmltYXRlKTsKICAgIGlmICghIG5vSGFsdCkgewogICAgICBuMy5kb21U
YXJnZXQuc2VsZWN0QWxsKCcubm9kZSwgLmVkZ2UsIC5sYWJlbCwgLmQzLXNsaWRl
ci1oYW5kbGUnKS50cmFuc2l0aW9uKCkuZHVyYXRpb24oMCkKICAgIH0KICB9Cgog
IC8qKiBzdGVwIHRoZSBhbmltYXRpb24gYnkgb25lIHRpbWUgc2xpY2UKICAqIEBw
YXJhbSB7Ym9vbGVhbn0gLSBpZiB0cnVlLCBnbyB0byBwcmV2aW91cyB0aW1lIHNs
aWNlLCBlbHNlIGdvIGZvcndhcmQKICAqLwogIG4zLnByb3RvdHlwZS5zdGVwQW5p
bWF0aW9uID0gZnVuY3Rpb24ocmV2ZXJzZSkgewogICAgdmFyIG4zID0gdGhpczsK
CiAgICBuMy5lbmRBbmltYXRpb24oMSk7CiAgICBpZiAocmV2ZXJzZSkgewogICAg
ICBuMy5hbmltYXRlR3JhcGgobjMuY3VyclRpbWUtMSwgbjMuY3VyclRpbWUtMSk7
IAogICAgfSBlbHNlIHsKICAgICAgbjMuYW5pbWF0ZUdyYXBoKG4zLmN1cnJUaW1l
KzEsIG4zLmN1cnJUaW1lKzEpOyAKICAgIH0KICB9CgogIC8qKiBhbmltYXRlIHRo
ZSBncmFwaCBvdmVyIGFsbCB0aW1lIHNsaWNlcywgc3RhcnRpbmcgYXQgY3VycmVu
dCBzbGljZQogICogQHBhcmFtIHtib29sZWFufSAtIGlmIHRydWUsIGFuaW1hdGUg
c2xpY2VzIGJhY2t3YXJkcyB1bnRpbCBiZWdpbm5pbmcgb2YgdGltZSBpbmRleCwg
b3RoZXIgcGxheSB1bnRpbCBlbmQKICAqLwogIG4zLnByb3RvdHlwZS5wbGF5QW5p
bWF0aW9uID0gZnVuY3Rpb24ocmV2ZXJzZSkgewogICAgdmFyIG4zID0gdGhpczsK
CiAgICBuMy5lbmRBbmltYXRpb24oMSk7CiAgICBpZiAocmV2ZXJzZSkgeyAKICAg
ICAgbjMuYW5pbWF0ZUdyYXBoKG4zLmN1cnJUaW1lLTEsIDApOyAKICAgIH0gZWxz
ZSB7CiAgICAgIG4zLmFuaW1hdGVHcmFwaChuMy5jdXJyVGltZSsxKTsgCiAgICB9
CiAgfQogIHJldHVybiBuMzsKfSkpOwo8L3NjcmlwdD48L2hlYWQ+DQogIDxib2R5
PjxzY3JpcHQ+DQogICAgLy9JTklUIEdSQVBIIERBVEEgSEVSRQ0KICB2YXIgZ3Jh
cGhEYXRhID0geyJyZW5kZXIiOlt7InN0YXJ0IjpbMF0sImVuZCI6WzFdLCJkYXRh
Ijp7ImFjdGl2ZSI6eyJub2RlcyI6eyIxIjpbMF0sIjIiOlsxXSwiMyI6WzJdLCI0
IjpbM10sIjUiOls0XSwiNiI6WzVdLCI3IjpbNl0sIjgiOls3XSwiOSI6WzhdLCIx
MCI6WzldLCIxMSI6WzEwXSwiMTIiOlsxMV0sIjEzIjpbMTJdLCIxNCI6WzEzXSwi
MTUiOlsxNF0sIjE2IjpbMTVdLCIxNyI6WzE2XSwiMTgiOlsxN10sIjE5IjpbMThd
LCIyMCI6WzE5XSwiMjEiOlsyMF0sIjIyIjpbMjFdLCIyMyI6WzIyXSwiMjQiOlsy
M10sIjI1IjpbMjRdLCIyNiI6WzI1XSwiMjciOlsyNl0sIjI4IjpbMjddLCIyOSI6
WzI4XSwiMzAiOlsyOV0sIjMxIjpbMzBdLCIzMiI6WzMxXSwiMzMiOlszMl0sIjM0
IjpbMzNdLCIzNSI6WzM0XSwiMzYiOlszNV0sIjM3IjpbMzZdLCIzOCI6WzM3XSwi
MzkiOlszOF0sIjQwIjpbMzldLCI0MSI6WzQwXSwiNDIiOls0MV0sIjQzIjpbNDJd
LCI0NCI6WzQzXSwiNDUiOls0NF0sIjQ2IjpbNDVdLCI0NyI6WzQ2XSwiNDgiOls0
N10sIjQ5IjpbNDhdLCI1MCI6WzQ5XSwiNTEiOls1MF0sIjUyIjpbNTFdLCI1MyI6
WzUyXSwiNTQiOls1M10sIjU1IjpbNTRdLCI1NiI6WzU1XSwiNTciOls1Nl0sIjU4
IjpbNTddLCI1OSI6WzU4XSwiNjAiOls1OV0sIjYxIjpbNjBdLCI2MiI6WzYxXSwi
NjMiOls2Ml0sIjY0IjpbNjNdLCI2NSI6WzY0XSwiNjYiOls2NV0sIjY3IjpbNjZd
LCI2OCI6WzY3XSwiNjkiOls2OF0sIjcwIjpbNjldLCI3MSI6WzcwXSwiNzIiOls3
MV0sIjczIjpbNzJdLCI3NCI6WzczXSwiNzUiOls3NF0sIjc2IjpbNzVdLCI3NyI6
Wzc2XSwiNzgiOls3N10sIjc5IjpbNzhdLCI4MCI6Wzc5XX0sImVkZ2VzIjp7IjEi
OlswXSwiMiI6WzFdLCIzIjpbMl0sIjQiOlszXSwiNSI6WzRdLCI2IjpbNV0sIjci
Ols2XSwiOCI6WzddLCI5IjpbOF0sIjEwIjpbOV0sIjExIjpbMTBdLCIxMiI6WzEx
XSwiMTMiOlsxMl0sIjE0IjpbMTNdLCIxNSI6WzE0XSwiMTYiOlsxNV0sIjE3Ijpb
MTZdLCIxOCI6WzE3XSwiMTkiOlsxOF0sIjIwIjpbMTldLCIyMSI6WzIwXSwiMjIi
OlsyMV0sIjIzIjpbMjJdLCIyNCI6WzIzXSwiMjUiOlsyNF0sIjI2IjpbMjVdLCIy
NyI6WzI2XSwiMjgiOlsyN10sIjI5IjpbMjhdLCIzMCI6WzI5XSwiMzEiOlszMF0s
IjMyIjpbMzFdLCIzMyI6WzMyXSwiMzQiOlszM10sIjM1IjpbMzRdLCIzNiI6WzM1
XSwiMzciOlszNl0sIjM4IjpbMzddLCIzOSI6WzM4XSwiNDAiOlszOV0sIjQxIjpb
NDBdLCI0MiI6WzQxXSwiNDMiOls0Ml0sIjQ0IjpbNDNdLCI0NSI6WzQ0XSwiNDYi
Ols0NV0sIjQ3IjpbNDZdLCI0OCI6WzQ3XSwiNDkiOls0OF0sIjUwIjpbNDldLCI1
MSI6WzUwXSwiNTIiOls1MV0sIjUzIjpbNTJdLCI1NCI6WzUzXSwiNTUiOls1NF0s
IjU2IjpbNTVdLCI1NyI6WzU2XSwiNTgiOls1N10sIjU5IjpbNThdLCI2MCI6WzU5
XSwiNjEiOls2MF0sIjYyIjpbNjFdLCI2MyI6WzYyXSwiNjQiOls2M10sIjY1Ijpb
NjRdLCI2NiI6WzY1XSwiNjciOls2Nl0sIjY4IjpbNjddLCI2OSI6WzY4XSwiNzAi
Ols2OV0sIjcxIjpbNzBdLCI3MiI6WzcxXSwiNzMiOls3Ml0sIjc0IjpbNzNdLCI3
NSI6Wzc0XSwiNzYiOls3NV0sIjc3IjpbNzZdLCI3OCI6Wzc3XSwiNzkiOls3OF0s
IjgwIjpbNzldLCI4MSI6WzgwXSwiODIiOls4MV0sIjgzIjpbODJdLCI4NCI6Wzgz
XSwiODUiOls4NF0sIjg2IjpbODVdLCI4NyI6Wzg2XSwiODgiOls4N10sIjg5Ijpb
ODhdLCI5MCI6Wzg5XSwiOTEiOls5MF0sIjkyIjpbOTFdLCI5MyI6WzkyXSwiOTQi
Ols5M10sIjk1IjpbOTRdLCI5NiI6Wzk1XSwiOTciOls5Nl0sIjk4IjpbOTddLCI5
OSI6Wzk4XSwiMTAwIjpbOTldfX0sImNvb3JkIjpbWy0xLjEyNDQsMi44NzM5XSxb
LTAuNjU2NCwtNS4xMzNdLFszLjY0OTYsMy44MzExXSxbNC4yOTIzLC0yLjk3NV0s
Wy0yLjYyMDEsLTMuNDE5XSxbLTIuMTg0NywtNC44Mjg0XSxbMy4xNjk0LC00LjE3
OTldLFsyLjE4MTEsNC43NDMxXSxbLTIuMDQ4MiwxLjgzODVdLFswLjIzMzYsMy4y
NzExXSxbLTEuNjkxLC0yLjcxNDNdLFstNC4xOTk2LC0xLjUzMjFdLFswLjU2MjEs
NS4yMzkzXSxbLTIuNDg4LDMuMzIzOV0sWy0zLjg4MzcsLTEuMjk5NV0sWy00LjIw
NzYsLTAuMzE3OV0sWzIuNDczNSwtNC42MzQ2XSxbMC45MDk2LC0wLjg1NTddLFst
Mi44ODMsLTQuNDg2Ml0sWy01LjAxNjgsLTAuOTI1M10sWy0xLjQyMDgsLTUuMTQx
Nl0sWzEuNzQ2LC00Ljg5NDNdLFsxLjAxNiwtNS4xOTZdLFstMy42MTU4LDIuODU5
MV0sWy0wLjI3NzgsNS4zMzc2XSxbMi43MzA0LDMuNDQ5OV0sWy0xLjk5NzMsNC45
ODM1XSxbLTQuNTg4NywtMS41MzddLFstMy4zOTI4LDQuMTE5N10sWy00LjY5NTcs
Mi43ODQzXSxbMS4yMjI4LC0zLjY5NTFdLFstMy43NjUsLTAuOTIzMV0sWzAuNDY5
NSwtMC44MjI2XSxbMy43Nzk2LC0zLjYyOTNdLFstNC4zOTc4LC0wLjkyMjVdLFs1
LjIxNDYsMC4xOTAzXSxbMS40MTk5LDUuMTQyMV0sWzMuMDk3MSwyLjM4MDFdLFst
NC44OTcxLC0xLjI5ODNdLFstNC45MTE2LC0wLjU0ODldLFszLjgxNzcsLTEuODA2
OF0sWy00LjA0NzksLTMuNTYxXSxbNC4xNDIyLDAuNDcwOF0sWy0wLjQwMjUsLTMu
NDkwM10sWzEuMzc1NSwtMC4xNzAzXSxbMi4zMDAxLC0zLjQ5NDVdLFs0LjE2Mjgs
LTAuNjg2NV0sWy0xLjE0MzgsNS4xOTc4XSxbNS4xMjEsMS4wMTg2XSxbLTIuNzI4
Myw0LjU2NjldLFswLjExNTUsLTUuMzM3Nl0sWzMuMTM5NCwtMi43NDU2XSxbMS4x
NTMxLDQuMjE3OV0sWy00LjA4NzgsMy41ODM1XSxbMC4zNTUxLC00LjM3NThdLFst
My44ODg1LC0wLjU0OF0sWy0zLjQ5NjMsLTMuOTk3NV0sWzAuMTU0MiwtMC41MTc0
XSxbLTAuMjk1NSw0LjM0MzddLFstMS41MjU3LDQuMDkyNF0sWzQuMTA3MSwzLjE2
MzZdLFswLjczNDksLTAuMjM1M10sWy01LjIxNDYsMS42OTZdLFswLjM1NjMsMC4y
ODddLFs0LjU1NTEsMi41Mzk5XSxbNS4xOTk0LC0wLjYzODNdLFsyLjkxMzMsNC40
MTQ5XSxbLTMuMTY4NiwxLjg0ODVdLFswLjEwOCwtMC4wNzU2XSxbNS4wMDk1LC0x
LjQ2OF0sWzQuNzMzMSwtMi4yNjM4XSxbLTQuNjAxLC0wLjMxMzNdLFs0LjkxMDEs
MS44MDFdLFstNC41NDA1LDIuMDE5Ml0sWzAuNzc5LDAuNDA3MV0sWzEuNjY5NSwz
LjEyNl0sWy0xLjQyNzcsLTQuMDE5M10sWzEuMjY3NSwtMC41OTY5XSxbMy44MzE5
LDEuNTYyXSxbMS4xODMyLDAuMjI4MV1dLCJ2ZXJ0ZXguY29sIjpbInJnYmEoMTE3
LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSww
Ljc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwy
LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4
NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2Jh
KDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwx
NzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3
LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEo
MjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5
NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDEx
MiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5
MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIx
Nyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMx
MzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3
LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDEx
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywx
NTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMx
MzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2Jh
KDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSww
Ljc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgy
MTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDEx
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1
LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5
NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDEx
MiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEs
NDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2Jh
KDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgx
MTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgs
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIl0sImJn
IjpbInJnYmEoMjU1LDI1NSwyNTUsMSkiXSwibGFiZWwiOlsiMTExIFJhbmNoIiwi
QWRvYmUgSGlsbCIsIkFydGlmYWN0IEhpbGwiLCJBc2ggVGVycmFjZSIsIkFaIEJC
OjM6MjIiLCJBWiBDQzoxOjMiLCJBWiBDQzoyOjE4NSIsIkFaIENDOjI6MzMoQkxN
KSIsIkJhamFkYSBTaXRlIiwiQmF5bGVzcyBSdWluIiwiQmlnIEJlbGwiLCJCcmFk
eSBXYXNoIiwiQnV6YW4iLCJDYWN0dXMgRm9yZXN0IiwiQ2FzYSBCdWVuYSIsIkNh
c2EgR3JhbmRlIiwiQ2x1ZmYgUmFuY2giLCJDb250aW5lbnRhbCBTaXRlIiwiQ3Jl
c2NlbnQgU2l0ZSIsIkNyaXNtb24iLCJDdXJ0aXMiLCJDdXJ0aXMgUnVpbihCdWVu
YSBWaXN0YSkiLCJEYXZpcyBSYW5jaCBTaXRlIiwiRHVkbGV5dmlsbGUgTW91bmQi
LCJFYXJ2ZW4gRmxhdCBTaXRlIiwiRWwgUG9sdm9yb24iLCJFbGxpb3R0IFNpdGUi
LCJFc2NhbGFudGUiLCJGaXNjaGVyIHNpdGUiLCJGbGllZ2VyIiwiRm9ydCBHcmFu
dCBQdWVibG8iLCJHZXJtYW5uIFNpdGUiLCJHaWJib24gIFNwcmluZ3MiLCJHb2F0
IEhpbGwiLCJHcmFuZCBDYW5hbCIsIkhhYnkgUmFuY2giLCJIaWdoIE1lc2EiLCJK
b3NlIFNvbGFzIFJ1aW4iLCJKdW5reWFyZCBTaXRlIiwiTGFzIENhbm9wYXMiLCJM
YXMgQ29saW5hcyIsIkxhcyBGb3NhcyIsIkxlYXZlcnRvbiIsIkxvcyBHdWFuYWNv
cyIsIkxvcyBNb3J0ZXJvcyIsIkxvcyBNdWVydG9zIiwiTG9zdCBNb3VuZCIsIk1h
cmlqaWxkYSBSdWluIiwiTWVzYSBHcmFuZGUiLCJNZXRob2Rpc3QgQ2h1cmNoIiwi
TXVycGh5IFNpdGUtLVNhZmZvcmQgRWFzdCIsIk93ZW5zLUNvbHZpbiIsIlBpcGVy
IFNwcmluZ3MiLCJQdWVibG8gQmxhbmNvIiwiUHVlYmxvIGRlbCBNb250ZSIsIlB1
ZWJsbyBHcmFuZGUiLCJQdWVibG8gU2FsYWRvIiwiUmFiaWQgUnVpbiIsIlJhdHRs
ZXNuYWtlIE1lc2EiLCJSZWV2ZSBSdWluIiwiUmljaGFyZHNvbiBPcmNoYXJkIiwi
UmlsbGl0byBGYW4iLCJSaW5jb24gQ2FueW9uIiwiU2FuIFhhdmllciBCcmlkZ2Ui
LCJTZWNvbmQgQ2FueW9uIENvbXBvdW5kIiwiU2hhcm9uIFNpdGUiLCJTcGVhciBS
YW5jaCIsIlN3aW5nbGUncyBTYW1wbGUiLCJUYW5xdWUgVmVyZGUgVmlsbGFnZSIs
IlRyZXMgUHVlYmxvcyIsIlR3aW4gSGF3a3MiLCJWZXJlcyIsIldlYmIgUnVpbiIs
IldlcyBKZXJuaWdhbiBTaXRlIiwiV2hpcHRhaWwgUnVpbiIsIldoaXRtZXIiLCJX
cmlnaHQiLCJZdW1hIFdhc2gtLVR1Y3NvbiIsIll1bWEgV2FzaCBTaXRlIiwiWmFu
YXJkZWxsaSBTaXRlIl0sInhsYWIiOlsidD0wLTEiXSwiaml0dGVyIjpbZmFsc2Vd
LCJ1c2VhcnJvd3MiOltmYWxzZV0sInhsaW0iOlstNS43OTUyLDUuNzk1Ml0sInls
aW0iOlstNS4zMzc2LDUuMzM3Nl19fSx7InN0YXJ0IjpbMV0sImVuZCI6WzJdLCJk
YXRhIjp7ImFjdGl2ZSI6eyJub2RlcyI6eyIxIjpbMF0sIjIiOlsxXSwiMyI6WzJd
LCI0IjpbM10sIjUiOls0XSwiNiI6WzVdLCI3IjpbNl0sIjgiOls3XSwiOSI6Wzhd
LCIxMCI6WzldLCIxMSI6WzEwXSwiMTIiOlsxMV0sIjEzIjpbMTJdLCIxNCI6WzEz
XSwiMTUiOlsxNF0sIjE2IjpbMTVdLCIxNyI6WzE2XSwiMTgiOlsxN10sIjE5Ijpb
MThdLCIyMCI6WzE5XSwiMjEiOlsyMF0sIjIyIjpbMjFdLCIyMyI6WzIyXSwiMjQi
OlsyM10sIjI1IjpbMjRdLCIyNiI6WzI1XSwiMjciOlsyNl0sIjI4IjpbMjddLCIy
OSI6WzI4XSwiMzAiOlsyOV0sIjMxIjpbMzBdLCIzMiI6WzMxXSwiMzMiOlszMl0s
IjM0IjpbMzNdLCIzNSI6WzM0XSwiMzYiOlszNV0sIjM3IjpbMzZdLCIzOCI6WzM3
XSwiMzkiOlszOF0sIjQwIjpbMzldLCI0MSI6WzQwXSwiNDIiOls0MV0sIjQzIjpb
NDJdLCI0NCI6WzQzXSwiNDUiOls0NF0sIjQ2IjpbNDVdLCI0NyI6WzQ2XSwiNDgi
Ols0N10sIjQ5IjpbNDhdLCI1MCI6WzQ5XSwiNTEiOls1MF0sIjUyIjpbNTFdLCI1
MyI6WzUyXSwiNTQiOls1M10sIjU1IjpbNTRdLCI1NiI6WzU1XSwiNTciOls1Nl0s
IjU4IjpbNTddLCI1OSI6WzU4XSwiNjAiOls1OV0sIjYxIjpbNjBdLCI2MiI6WzYx
XSwiNjMiOls2Ml0sIjY0IjpbNjNdLCI2NSI6WzY0XSwiNjYiOls2NV0sIjY3Ijpb
NjZdLCI2OCI6WzY3XSwiNjkiOls2OF0sIjcwIjpbNjldLCI3MSI6WzcwXSwiNzIi
Ols3MV0sIjczIjpbNzJdLCI3NCI6WzczXSwiNzUiOls3NF0sIjc2IjpbNzVdLCI3
NyI6Wzc2XSwiNzgiOls3N10sIjc5IjpbNzhdLCI4MCI6Wzc5XX0sImVkZ2VzIjp7
IjEiOlswXSwiMiI6WzFdLCIzIjpbMl0sIjQiOlszXSwiNSI6WzRdLCI2IjpbNV0s
IjgiOls2XSwiOSI6WzddLCIxMCI6WzhdLCIxMSI6WzldLCIxMiI6WzEwXSwiMTMi
OlsxMV0sIjE0IjpbMTJdLCIxNSI6WzEzXSwiMTciOlsxNF0sIjE4IjpbMTVdLCIx
OSI6WzE2XSwiMjAiOlsxN10sIjIxIjpbMThdLCIyMiI6WzE5XSwiMjMiOlsyMF0s
IjI1IjpbMjFdLCIyNiI6WzIyXSwiMjciOlsyM10sIjI4IjpbMjRdLCIyOSI6WzI1
XSwiMzAiOlsyNl0sIjMxIjpbMjddLCIzMiI6WzI4XSwiMzMiOlsyOV0sIjM0Ijpb
MzBdLCIzNSI6WzMxXSwiMzYiOlszMl0sIjM3IjpbMzNdLCIzOCI6WzM0XSwiMzki
OlszNV0sIjQxIjpbMzZdLCI0MiI6WzM3XSwiNDMiOlszOF0sIjQ0IjpbMzldLCI0
NSI6WzQwXSwiNDciOls0MV0sIjQ4IjpbNDJdLCI0OSI6WzQzXSwiNTAiOls0NF0s
IjUyIjpbNDVdLCI1MyI6WzQ2XSwiNTQiOls0N10sIjU1IjpbNDhdLCI1NiI6WzQ5
XSwiNTciOls1MF0sIjU4IjpbNTFdLCI1OSI6WzUyXSwiNjAiOls1M10sIjYxIjpb
NTRdLCI2MiI6WzU1XSwiNjQiOls1Nl0sIjY1IjpbNTddLCI2NiI6WzU4XSwiNzAi
Ols1OV0sIjcxIjpbNjBdLCI3MiI6WzYxXSwiNzMiOls2Ml0sIjc0IjpbNjNdLCI3
NSI6WzY0XSwiNzYiOls2NV0sIjc3IjpbNjZdLCI3OCI6WzY3XSwiNzkiOls2OF0s
IjgwIjpbNjldLCI4MSI6WzcwXSwiODIiOls3MV0sIjgzIjpbNzJdLCI4NCI6Wzcz
XSwiODUiOls3NF0sIjg2IjpbNzVdLCI4NyI6Wzc2XSwiODgiOls3N10sIjg5Ijpb
NzhdLCI5MCI6Wzc5XSwiOTEiOls4MF0sIjkyIjpbODFdLCI5MyI6WzgyXSwiOTQi
Ols4M10sIjk1IjpbODRdLCI5NiI6Wzg1XSwiOTciOls4Nl0sIjk4IjpbODddLCI5
OSI6Wzg4XSwiMTAwIjpbODldLCIxMDEiOls5MF0sIjEwMiI6WzkxXSwiMTAzIjpb
OTJdLCIxMDQiOls5M10sIjEwNSI6Wzk0XSwiMTA2IjpbOTVdLCIxMDciOls5Nl0s
IjEwOCI6Wzk3XX19LCJjb29yZCI6W1stMi4yMzksMS4yODc4XSxbNS4yMjMsMC40
MTg4XSxbMy4zMjMzLDQuMDA1N10sWzQuMzg5OSwtMC45NV0sWzAuMjI4Miw1LjMy
MzRdLFstMS44MzUxLC0zLjkzNjFdLFs0LjU0NDYsLTIuNDgwOV0sWzMuOTU4OSwz
LjM5OTZdLFstMS41NzMzLDIuNzg0Nl0sWy0wLjMyMDUsMy40NzMyXSxbLTEuODU5
NSwtMS41OTk5XSxbLTQuMzQ1NywtMS4wNDU1XSxbLTEuMzAzMywtNS4xNjAxXSxb
LTUuMDkyLDIuMDE1N10sWy00LjAzNTIsLTAuNzQwN10sWy00LjI0MjIsMC4wNTRd
LFs0LjkxNDgsMS44NDg0XSxbMS4xODAxLC0wLjMyMDldLFsyLjYxNiw0LjUyMjFd
LFstNS4yMjMsLTAuNDAzN10sWy0wLjUwNDUsLTUuMjkwOF0sWzEuODE4OCwtMy45
NzUxXSxbLTIuODExNiwtNC41NTMxXSxbLTMuNTIyMSwzLjczODhdLFstMi4yMzA1
LDQuODUwM10sWy0zLjkxMzUsMi4zMTgyXSxbLTMuMDkxNiw0LjUwMDddLFstNC43
NzYyLC0xLjA4MzZdLFs0LjczMjEsMC45OTg4XSxbNC4zMTcxLC0wLjA1NzJdLFsy
LjEwODMsLTIuOTQ4M10sWy0zLjk5MzEsLTAuMzA0N10sWzAuNzQxMywtMC4yODgz
XSxbNC4xMDQ3LC0zLjI0MzVdLFstNC42MDYsLTAuNDY0XSxbMy45NzE5LDEuNzE2
XSxbMS4yNDk0LC01LjE1NTNdLFswLjUyNDIsLTMuMTU1N10sWy00LjA3OTYsLTMu
NjU0NF0sWy01LjA1MTIsLTAuMDA3XSxbLTMuNDI4NywtNC4xMjg5XSxbLTMuMzc3
MywtMy4wNDQ4XSxbLTAuNjA5Niw1LjIwMTJdLFswLjMzMTgsLTUuMzIzNF0sWzEu
NjUxNCwwLjM1NzZdLFszLjIwNzcsLTIuODQwN10sWzUuMjIwNSwtMC42MDg2XSxb
NC45MzE2LC0xLjcwMjNdLFstMC42MDY2LC00LjE0MzNdLFstMi44MTYxLDIuMzkw
NF0sWzAuNjE1LC00LjM1ODVdLFszLjY2NTUsLTEuODQ5XSxbMi4xMDQ3LC00Ljg2
OF0sWy0yLjA5OSwtNC45Njg0XSxbNC40NDI5LDIuNjQ2Ml0sWy01LjExNzksLTAu
ODI0NV0sWy0yLjI4NzMsLTIuODI3Ml0sWzAuNDI1OCwwLjAxOTFdLFswLjMxNDMs
NC4zODMxXSxbLTEuMzE1Miw0LjE1MDddLFsxLjExOTMsNS4xMzI2XSxbMS4wMDk3
LDAuMjk4Ml0sWy00LjczNDIsMi43MjEzXSxbMC42MzI2LDAuODIzXSxbMS4yOTks
My43ODc2XSxbLTEuNDI0OSw1LjE1NjldLFszLjQ4MzEsLTMuODY2M10sWy0yLjYy
OTEsMy43NDkxXSxbMC4zODM3LDAuNDU4NF0sWy00LjYwNjgsLTIuOTcxOF0sWzIu
MzcyMywzLjUwMjldLFstNC42NjAyLDAuMTczNl0sWzIuODUwMywtNC4zNjIxXSxb
LTQuMjQyNSwzLjQzNDhdLFsxLjA1ODksMC45Mzk1XSxbMS44NzE5LDQuODQyOV0s
Wy0wLjg4NDIsLTIuNzgwOF0sWzEuNTQsLTAuMDY2Nl0sWzMuMjk2MSwyLjc2NjVd
LFsxLjQ2MDEsMC43NTU5XV0sInZlcnRleC5jb2wiOlsicmdiYSgxMTcsMTEyLDE3
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5
NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIx
Nyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkw
MTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNyki
LCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDEx
MiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2Jh
KDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDEx
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEs
MTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcs
OTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5
MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMx
MzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3
LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSww
Ljc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4
NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2Jh
KDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIs
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDEx
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1
OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5
MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEz
NykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgx
MTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTks
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1
LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5
NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwy
LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1
OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5
MDE5NjA3ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEz
NykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3
LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMx
MzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIz
MSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSww
Ljc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgx
MTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgs
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1
LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIs
MTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNykiXSwiYmciOlsicmdi
YSgyNTUsMjU1LDI1NSwxKSJdLCJsYWJlbCI6WyIxMTEgUmFuY2giLCJBZG9iZSBI
aWxsIiwiQXJ0aWZhY3QgSGlsbCIsIkFzaCBUZXJyYWNlIiwiQVogQkI6MzoyMiIs
IkFaIENDOjE6MyIsIkFaIENDOjI6MTg1IiwiQVogQ0M6MjozMyhCTE0pIiwiQmFq
YWRhIFNpdGUiLCJCYXlsZXNzIFJ1aW4iLCJCaWcgQmVsbCIsIkJyYWR5IFdhc2gi
LCJCdXphbiIsIkNhY3R1cyBGb3Jlc3QiLCJDYXNhIEJ1ZW5hIiwiQ2FzYSBHcmFu
ZGUiLCJDbHVmZiBSYW5jaCIsIkNvbnRpbmVudGFsIFNpdGUiLCJDcmVzY2VudCBT
aXRlIiwiQ3Jpc21vbiIsIkN1cnRpcyIsIkN1cnRpcyBSdWluKEJ1ZW5hIFZpc3Rh
KSIsIkRhdmlzIFJhbmNoIFNpdGUiLCJEdWRsZXl2aWxsZSBNb3VuZCIsIkVhcnZl
biBGbGF0IFNpdGUiLCJFbCBQb2x2b3JvbiIsIkVsbGlvdHQgU2l0ZSIsIkVzY2Fs
YW50ZSIsIkZpc2NoZXIgc2l0ZSIsIkZsaWVnZXIiLCJGb3J0IEdyYW50IFB1ZWJs
byIsIkdlcm1hbm4gU2l0ZSIsIkdpYmJvbiAgU3ByaW5ncyIsIkdvYXQgSGlsbCIs
IkdyYW5kIENhbmFsIiwiSGFieSBSYW5jaCIsIkhpZ2ggTWVzYSIsIkpvc2UgU29s
YXMgUnVpbiIsIkp1bmt5YXJkIFNpdGUiLCJMYXMgQ2Fub3BhcyIsIkxhcyBDb2xp
bmFzIiwiTGFzIEZvc2FzIiwiTGVhdmVydG9uIiwiTG9zIEd1YW5hY29zIiwiTG9z
IE1vcnRlcm9zIiwiTG9zIE11ZXJ0b3MiLCJMb3N0IE1vdW5kIiwiTWFyaWppbGRh
IFJ1aW4iLCJNZXNhIEdyYW5kZSIsIk1ldGhvZGlzdCBDaHVyY2giLCJNdXJwaHkg
U2l0ZS0tU2FmZm9yZCBFYXN0IiwiT3dlbnMtQ29sdmluIiwiUGlwZXIgU3ByaW5n
cyIsIlB1ZWJsbyBCbGFuY28iLCJQdWVibG8gZGVsIE1vbnRlIiwiUHVlYmxvIEdy
YW5kZSIsIlB1ZWJsbyBTYWxhZG8iLCJSYWJpZCBSdWluIiwiUmF0dGxlc25ha2Ug
TWVzYSIsIlJlZXZlIFJ1aW4iLCJSaWNoYXJkc29uIE9yY2hhcmQiLCJSaWxsaXRv
IEZhbiIsIlJpbmNvbiBDYW55b24iLCJTYW4gWGF2aWVyIEJyaWRnZSIsIlNlY29u
ZCBDYW55b24gQ29tcG91bmQiLCJTaGFyb24gU2l0ZSIsIlNwZWFyIFJhbmNoIiwi
U3dpbmdsZSdzIFNhbXBsZSIsIlRhbnF1ZSBWZXJkZSBWaWxsYWdlIiwiVHJlcyBQ
dWVibG9zIiwiVHdpbiBIYXdrcyIsIlZlcmVzIiwiV2ViYiBSdWluIiwiV2VzIEpl
cm5pZ2FuIFNpdGUiLCJXaGlwdGFpbCBSdWluIiwiV2hpdG1lciIsIldyaWdodCIs
Ill1bWEgV2FzaC0tVHVjc29uIiwiWXVtYSBXYXNoIFNpdGUiLCJaYW5hcmRlbGxp
IFNpdGUiXSwieGxhYiI6WyJ0PTEtMiJdLCJqaXR0ZXIiOltmYWxzZV0sInVzZWFy
cm93cyI6W2ZhbHNlXSwieGxpbSI6Wy01Ljc5NTIsNS43OTUyXSwieWxpbSI6Wy01
LjMzNzYsNS4zMzc2XX19LHsic3RhcnQiOlsyXSwiZW5kIjpbM10sImRhdGEiOnsi
YWN0aXZlIjp7Im5vZGVzIjp7IjEiOlswXSwiMiI6WzFdLCIzIjpbMl0sIjQiOlsz
XSwiNSI6WzRdLCI2IjpbNV0sIjciOls2XSwiOCI6WzddLCI5IjpbOF0sIjEwIjpb
OV0sIjExIjpbMTBdLCIxMiI6WzExXSwiMTMiOlsxMl0sIjE0IjpbMTNdLCIxNSI6
WzE0XSwiMTYiOlsxNV0sIjE3IjpbMTZdLCIxOCI6WzE3XSwiMTkiOlsxOF0sIjIw
IjpbMTldLCIyMSI6WzIwXSwiMjIiOlsyMV0sIjIzIjpbMjJdLCIyNCI6WzIzXSwi
MjUiOlsyNF0sIjI2IjpbMjVdLCIyNyI6WzI2XSwiMjgiOlsyN10sIjI5IjpbMjhd
LCIzMCI6WzI5XSwiMzEiOlszMF0sIjMyIjpbMzFdLCIzMyI6WzMyXSwiMzQiOlsz
M10sIjM1IjpbMzRdLCIzNiI6WzM1XSwiMzciOlszNl0sIjM4IjpbMzddLCIzOSI6
WzM4XSwiNDAiOlszOV0sIjQxIjpbNDBdLCI0MiI6WzQxXSwiNDMiOls0Ml0sIjQ0
IjpbNDNdLCI0NSI6WzQ0XSwiNDYiOls0NV0sIjQ3IjpbNDZdLCI0OCI6WzQ3XSwi
NDkiOls0OF0sIjUwIjpbNDldLCI1MSI6WzUwXSwiNTIiOls1MV0sIjUzIjpbNTJd
LCI1NCI6WzUzXSwiNTUiOls1NF0sIjU2IjpbNTVdLCI1NyI6WzU2XSwiNTgiOls1
N10sIjU5IjpbNThdLCI2MCI6WzU5XSwiNjEiOls2MF0sIjYyIjpbNjFdLCI2MyI6
WzYyXSwiNjQiOls2M10sIjY1IjpbNjRdLCI2NiI6WzY1XSwiNjciOls2Nl0sIjY4
IjpbNjddLCI2OSI6WzY4XSwiNzAiOls2OV0sIjcxIjpbNzBdLCI3MiI6WzcxXSwi
NzMiOls3Ml0sIjc0IjpbNzNdLCI3NSI6Wzc0XSwiNzYiOls3NV0sIjc3IjpbNzZd
LCI3OCI6Wzc3XSwiNzkiOls3OF0sIjgwIjpbNzldfSwiZWRnZXMiOnsiNCI6WzBd
LCIxMSI6WzFdLCIxMiI6WzJdLCIxMyI6WzNdLCIxNCI6WzRdLCIxNSI6WzVdLCIx
OCI6WzZdLCIxOSI6WzddLCIyMCI6WzhdLCIyMiI6WzldLCIyMyI6WzEwXSwiMjYi
OlsxMV0sIjI3IjpbMTJdLCIyOSI6WzEzXSwiMzIiOlsxNF0sIjMzIjpbMTVdLCIz
NCI6WzE2XSwiMzUiOlsxN10sIjM2IjpbMThdLCIzNyI6WzE5XSwiMzgiOlsyMF0s
IjM5IjpbMjFdLCI0MiI6WzIyXSwiNDMiOlsyM10sIjQ0IjpbMjRdLCI0NSI6WzI1
XSwiNDgiOlsyNl0sIjUwIjpbMjddLCI1MyI6WzI4XSwiNTQiOlsyOV0sIjY1Ijpb
MzBdLCI2NiI6WzMxXSwiNzQiOlszMl0sIjc1IjpbMzNdLCI3NiI6WzM0XSwiNzci
OlszNV0sIjc4IjpbMzZdLCI3OSI6WzM3XSwiOTEiOlszOF0sIjkyIjpbMzldLCI5
MyI6WzQwXSwiOTQiOls0MV0sIjk1IjpbNDJdLCI5NiI6WzQzXSwiOTciOls0NF0s
Ijk4IjpbNDVdLCI5OSI6WzQ2XSwiMTAwIjpbNDddLCIxMDEiOls0OF0sIjEwMiI6
WzQ5XSwiMTA0IjpbNTBdLCIxMDUiOls1MV0sIjEwNiI6WzUyXSwiMTA3IjpbNTNd
LCIxMDkiOls1NF0sIjExMCI6WzU1XSwiMTExIjpbNTZdLCIxMTIiOls1N10sIjEx
MyI6WzU4XSwiMTE0IjpbNTldLCIxMTUiOls2MF0sIjExNiI6WzYxXSwiMTE3Ijpb
NjJdLCIxMTgiOls2M10sIjExOSI6WzY0XSwiMTIwIjpbNjVdLCIxMjEiOls2Nl0s
IjEyMiI6WzY3XSwiMTIzIjpbNjhdLCIxMjQiOls2OV0sIjEyNSI6WzcwXSwiMTI2
IjpbNzFdLCIxMjciOls3Ml0sIjEyOCI6WzczXSwiMTI5IjpbNzRdLCIxMzAiOls3
NV0sIjEzMSI6Wzc2XSwiMTMyIjpbNzddLCIxMzMiOls3OF0sIjEzNCI6Wzc5XSwi
MTM1IjpbODBdLCIxMzYiOls4MV0sIjEzNyI6WzgyXSwiMTM4IjpbODNdLCIxMzki
Ols4NF0sIjE0MCI6Wzg1XSwiMTQxIjpbODZdLCIxNDIiOls4N10sIjE0MyI6Wzg4
XSwiMTQ0IjpbODldLCIxNDUiOls5MF0sIjE0NiI6WzkxXSwiMTQ3IjpbOTJdLCIx
NDgiOls5M10sIjE0OSI6Wzk0XSwiMTUwIjpbOTVdLCIxNTEiOls5Nl0sIjE1MiI6
Wzk3XSwiMTUzIjpbOThdLCIxNTQiOls5OV0sIjE1NSI6WzEwMF0sIjE1NiI6WzEw
MV0sIjE1NyI6WzEwMl0sIjE1OCI6WzEwM10sIjE1OSI6WzEwNF0sIjE2MCI6WzEw
NV0sIjE2MSI6WzEwNl0sIjE2MiI6WzEwN10sIjE2MyI6WzEwOF0sIjE2NCI6WzEw
OV0sIjE2NSI6WzExMF0sIjE2NiI6WzExMV0sIjE2NyI6WzExMl0sIjE2OCI6WzEx
M10sIjE2OSI6WzExNF0sIjE3MCI6WzExNV0sIjE3MSI6WzExNl0sIjE3MiI6WzEx
N10sIjE3MyI6WzExOF0sIjE3NCI6WzExOV0sIjE3NSI6WzEyMF0sIjE3NiI6WzEy
MV0sIjE3NyI6WzEyMl0sIjE3OCI6WzEyM10sIjE3OSI6WzEyNF0sIjE4MCI6WzEy
NV0sIjE4MSI6WzEyNl0sIjE4MiI6WzEyN10sIjE4MyI6WzEyOF0sIjE4NCI6WzEy
OV0sIjE4NSI6WzEzMF0sIjE4NiI6WzEzMV0sIjE4NyI6WzEzMl0sIjE4OCI6WzEz
M10sIjE4OSI6WzEzNF0sIjE5MCI6WzEzNV0sIjE5MSI6WzEzNl0sIjE5MiI6WzEz
N10sIjE5MyI6WzEzOF0sIjE5NCI6WzEzOV0sIjE5NSI6WzE0MF0sIjE5NiI6WzE0
MV0sIjE5NyI6WzE0Ml0sIjE5OCI6WzE0M10sIjE5OSI6WzE0NF0sIjIwMCI6WzE0
NV0sIjIwMSI6WzE0Nl0sIjIwMiI6WzE0N10sIjIwMyI6WzE0OF0sIjIwNCI6WzE0
OV0sIjIwNSI6WzE1MF0sIjIwNiI6WzE1MV0sIjIwNyI6WzE1Ml0sIjIwOCI6WzE1
M10sIjIwOSI6WzE1NF0sIjIxMCI6WzE1NV0sIjIxMSI6WzE1Nl0sIjIxMiI6WzE1
N119fSwiY29vcmQiOltbNC40Mzk4LDIuNDg4M10sWzUuMDQwMiwxLjE0NzFdLFst
MS44MTEsMy43OTA4XSxbLTAuNTU0MywyLjQyODhdLFstMC4yNDc0LDQuODY0OV0s
Wy0zLjg3NTYsLTMuNzQ5Ml0sWzIuODc1MiwtMy4xNjE1XSxbMi44Nzc0LC00LjIx
MV0sWy0yLjMxMSw0LjU2MjldLFstMC4xNjM3LC0xLjMxMjRdLFswLjYwOTUsLTEu
NzU1Nl0sWy00LjQ3NDEsLTIuMDIwNV0sWzEuODYxNiw1LjE3MjJdLFs1LjA0ODEs
LTAuMzU0M10sWy0zLjkyMDMsLTAuNDA3OV0sWy00LjYzODMsMC41NjYzXSxbNC44
ODA2LDEuOTA1MV0sWzMuMDg4NSwwLjgyN10sWy0xLjMzOTMsNC45MTcxXSxbLTQu
NzYxOCwtMC41Nzk2XSxbLTEuNzgyMSw0LjA3ODRdLFswLjc1NDQsLTQuMjgyXSxb
LTIuMzc2MSwzLjk0NjRdLFstMi44MTM0LDQuNDI0Nl0sWzEuNTI0LC0zLjM3ODld
LFstMC4wMDYzLC0zLjQ5NTJdLFstMi41MDE0LDMuMjk3N10sWy00Ljc1OTIsLTEu
MTc5MV0sWzMuNjI1MywtMy42MzE1XSxbLTAuODQ3OCwzLjMwODRdLFstMS4xMDkx
LC01LjE0MjNdLFstNC4wMzY2LC0wLjA2NDZdLFstMy4yMzEyLC00LjEwNTZdLFs0
LjE3NDEsLTIuODYxNl0sWy01LjA0OTgsLTAuMTQ4MV0sWzMuMTMwMSwzLjE0MDZd
LFswLjYwNTQsNC44ODhdLFstMS4zNzI1LC0yLjQ5NDFdLFstMS4xODgyLC00LjA3
NjNdLFszLjY4OTksMy44Nzc5XSxbLTIuNDIxMywtMy40MzQ5XSxbLTUuMTQxMywt
MC40OTk3XSxbLTEuMjQ2MSwzLjAwOTFdLFstMC4zMTE4LC01LjAxODhdLFsyLjc0
OSwxLjIxMzFdLFstNS4wNjc4LDAuMzg0Ml0sWzUuMTQxMywwLjM4OTJdLFstMC43
MzM1LDQuMDEwNF0sWzEuNDY1NiwtNC45MzU3XSxbNC4yMjM4LDMuMTkxN10sWzIu
MTU1NSwtNC41MDk3XSxbNS4wNDcsLTEuMTQ5NV0sWy0yLjgzNDEsMy41XSxbLTEu
ODI5MiwtNC43MDQyXSxbLTQuNTQ3NCwtMC4wODE2XSxbLTQuMzA1NywtMC41Mjgx
XSxbLTMuODM2NiwtMi42MDkyXSxbLTEuMDEwMiwtMS42NzM0XSxbLTAuOTY3OSw0
LjY2MjRdLFstMS4zMDE3LDQuMDcxNV0sWzIuMzc0Nyw0LjcwOTNdLFstMi42MTYy
LC00LjU5NTldLFsyLjA0NjksLTIuMTg0Ml0sWzIuMDgxNCwwLjI2MjldLFstMC44
NjI4LC0wLjc3MDddLFswLjQ5MTgsLTUuMTcyMl0sWzMuNTMyNywtMi4yMzddLFst
Mi44OTM3LDMuOTExNF0sWzMuMDExNSwwLjMxNzldLFstNC4xNiwtMS4xMjVdLFs0
LjcwMzcsLTIuMTE5NF0sWy00LjE3MDQsMC40NzAxXSxbMi4xNzQsMy42MDQ1XSxb
LTUuMDIyMywyLjY4NzFdLFsyLjIzNTIsMS4xNzgxXSxbNC4xODAyLC0xLjNdLFst
MS45NDIxLDMuMTAwNF0sWzIuNTYwOCwwLjA2NjhdLFszLjA0NDcsNC40MDYxXSxb
MS45MzE1LDAuNzU3Ml1dLCJ2ZXJ0ZXguY29sIjpbInJnYmEoMTE3LDExMiwxNzks
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcs
OTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5
NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIs
MTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkw
MTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEz
NykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgx
MTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTks
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEz
OCwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1
LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3
KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywx
NTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgx
MTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTks
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgs
MTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3
LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwy
LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiww
Ljc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3
ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgs
MTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywx
MTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkw
MTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3
KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEs
NDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEz
NykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3
LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwy
LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4
NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIl0sImJnIjpbInJnYmEo
MjU1LDI1NSwyNTUsMSkiXSwibGFiZWwiOlsiMTExIFJhbmNoIiwiQWRvYmUgSGls
bCIsIkFydGlmYWN0IEhpbGwiLCJBc2ggVGVycmFjZSIsIkFaIEJCOjM6MjIiLCJB
WiBDQzoxOjMiLCJBWiBDQzoyOjE4NSIsIkFaIENDOjI6MzMoQkxNKSIsIkJhamFk
YSBTaXRlIiwiQmF5bGVzcyBSdWluIiwiQmlnIEJlbGwiLCJCcmFkeSBXYXNoIiwi
QnV6YW4iLCJDYWN0dXMgRm9yZXN0IiwiQ2FzYSBCdWVuYSIsIkNhc2EgR3JhbmRl
IiwiQ2x1ZmYgUmFuY2giLCJDb250aW5lbnRhbCBTaXRlIiwiQ3Jlc2NlbnQgU2l0
ZSIsIkNyaXNtb24iLCJDdXJ0aXMiLCJDdXJ0aXMgUnVpbihCdWVuYSBWaXN0YSki
LCJEYXZpcyBSYW5jaCBTaXRlIiwiRHVkbGV5dmlsbGUgTW91bmQiLCJFYXJ2ZW4g
RmxhdCBTaXRlIiwiRWwgUG9sdm9yb24iLCJFbGxpb3R0IFNpdGUiLCJFc2NhbGFu
dGUiLCJGaXNjaGVyIHNpdGUiLCJGbGllZ2VyIiwiRm9ydCBHcmFudCBQdWVibG8i
LCJHZXJtYW5uIFNpdGUiLCJHaWJib24gIFNwcmluZ3MiLCJHb2F0IEhpbGwiLCJH
cmFuZCBDYW5hbCIsIkhhYnkgUmFuY2giLCJIaWdoIE1lc2EiLCJKb3NlIFNvbGFz
IFJ1aW4iLCJKdW5reWFyZCBTaXRlIiwiTGFzIENhbm9wYXMiLCJMYXMgQ29saW5h
cyIsIkxhcyBGb3NhcyIsIkxlYXZlcnRvbiIsIkxvcyBHdWFuYWNvcyIsIkxvcyBN
b3J0ZXJvcyIsIkxvcyBNdWVydG9zIiwiTG9zdCBNb3VuZCIsIk1hcmlqaWxkYSBS
dWluIiwiTWVzYSBHcmFuZGUiLCJNZXRob2Rpc3QgQ2h1cmNoIiwiTXVycGh5IFNp
dGUtLVNhZmZvcmQgRWFzdCIsIk93ZW5zLUNvbHZpbiIsIlBpcGVyIFNwcmluZ3Mi
LCJQdWVibG8gQmxhbmNvIiwiUHVlYmxvIGRlbCBNb250ZSIsIlB1ZWJsbyBHcmFu
ZGUiLCJQdWVibG8gU2FsYWRvIiwiUmFiaWQgUnVpbiIsIlJhdHRsZXNuYWtlIE1l
c2EiLCJSZWV2ZSBSdWluIiwiUmljaGFyZHNvbiBPcmNoYXJkIiwiUmlsbGl0byBG
YW4iLCJSaW5jb24gQ2FueW9uIiwiU2FuIFhhdmllciBCcmlkZ2UiLCJTZWNvbmQg
Q2FueW9uIENvbXBvdW5kIiwiU2hhcm9uIFNpdGUiLCJTcGVhciBSYW5jaCIsIlN3
aW5nbGUncyBTYW1wbGUiLCJUYW5xdWUgVmVyZGUgVmlsbGFnZSIsIlRyZXMgUHVl
YmxvcyIsIlR3aW4gSGF3a3MiLCJWZXJlcyIsIldlYmIgUnVpbiIsIldlcyBKZXJu
aWdhbiBTaXRlIiwiV2hpcHRhaWwgUnVpbiIsIldoaXRtZXIiLCJXcmlnaHQiLCJZ
dW1hIFdhc2gtLVR1Y3NvbiIsIll1bWEgV2FzaCBTaXRlIiwiWmFuYXJkZWxsaSBT
aXRlIl0sInhsYWIiOlsidD0yLTMiXSwiaml0dGVyIjpbZmFsc2VdLCJ1c2VhcnJv
d3MiOltmYWxzZV0sInhsaW0iOlstNS43OTUyLDUuNzk1Ml0sInlsaW0iOlstNS4z
Mzc2LDUuMzM3Nl19fSx7InN0YXJ0IjpbM10sImVuZCI6WzRdLCJkYXRhIjp7ImFj
dGl2ZSI6eyJub2RlcyI6eyIxIjpbMF0sIjIiOlsxXSwiMyI6WzJdLCI0IjpbM10s
IjUiOls0XSwiNiI6WzVdLCI3IjpbNl0sIjgiOls3XSwiOSI6WzhdLCIxMCI6Wzld
LCIxMSI6WzEwXSwiMTIiOlsxMV0sIjEzIjpbMTJdLCIxNCI6WzEzXSwiMTUiOlsx
NF0sIjE2IjpbMTVdLCIxNyI6WzE2XSwiMTgiOlsxN10sIjE5IjpbMThdLCIyMCI6
WzE5XSwiMjEiOlsyMF0sIjIyIjpbMjFdLCIyMyI6WzIyXSwiMjQiOlsyM10sIjI1
IjpbMjRdLCIyNiI6WzI1XSwiMjciOlsyNl0sIjI4IjpbMjddLCIyOSI6WzI4XSwi
MzAiOlsyOV0sIjMxIjpbMzBdLCIzMiI6WzMxXSwiMzMiOlszMl0sIjM0IjpbMzNd
LCIzNSI6WzM0XSwiMzYiOlszNV0sIjM3IjpbMzZdLCIzOCI6WzM3XSwiMzkiOlsz
OF0sIjQwIjpbMzldLCI0MSI6WzQwXSwiNDIiOls0MV0sIjQzIjpbNDJdLCI0NCI6
WzQzXSwiNDUiOls0NF0sIjQ2IjpbNDVdLCI0NyI6WzQ2XSwiNDgiOls0N10sIjQ5
IjpbNDhdLCI1MCI6WzQ5XSwiNTEiOls1MF0sIjUyIjpbNTFdLCI1MyI6WzUyXSwi
NTQiOls1M10sIjU1IjpbNTRdLCI1NiI6WzU1XSwiNTciOls1Nl0sIjU4IjpbNTdd
LCI1OSI6WzU4XSwiNjAiOls1OV0sIjYxIjpbNjBdLCI2MiI6WzYxXSwiNjMiOls2
Ml0sIjY0IjpbNjNdLCI2NSI6WzY0XSwiNjYiOls2NV0sIjY3IjpbNjZdLCI2OCI6
WzY3XSwiNjkiOls2OF0sIjcwIjpbNjldLCI3MSI6WzcwXSwiNzIiOls3MV0sIjcz
IjpbNzJdLCI3NCI6WzczXSwiNzUiOls3NF0sIjc2IjpbNzVdLCI3NyI6Wzc2XSwi
NzgiOls3N10sIjc5IjpbNzhdLCI4MCI6Wzc5XX0sImVkZ2VzIjp7IjEiOlswXSwi
MiI6WzFdLCIzIjpbMl0sIjQiOlszXSwiNSI6WzRdLCI2IjpbNV0sIjciOls2XSwi
OSI6WzddLCIxMSI6WzhdLCIxMiI6WzldLCIxMyI6WzEwXSwiMTQiOlsxMV0sIjE1
IjpbMTJdLCIxNiI6WzEzXSwiMTgiOlsxNF0sIjIwIjpbMTVdLCIyMSI6WzE2XSwi
MjIiOlsxN10sIjIzIjpbMThdLCIyNCI6WzE5XSwiMjYiOlsyMF0sIjM3IjpbMjFd
LCIzOCI6WzIyXSwiMzkiOlsyM10sIjQwIjpbMjRdLCI0MiI6WzI1XSwiNDQiOlsy
Nl0sIjQ1IjpbMjddLCI0NiI6WzI4XSwiNDgiOlsyOV0sIjUwIjpbMzBdLCI1MSI6
WzMxXSwiNTMiOlszMl0sIjYzIjpbMzNdLCI2NSI6WzM0XSwiNjgiOlszNV0sIjEw
MSI6WzM2XSwiMTAyIjpbMzddLCIxMDQiOlszOF0sIjEwNSI6WzM5XSwiMTA2Ijpb
NDBdLCIxMDgiOls0MV0sIjEwOSI6WzQyXSwiMTEwIjpbNDNdLCIxMTEiOls0NF0s
IjExMiI6WzQ1XSwiMTEzIjpbNDZdLCIxMTQiOls0N10sIjExNiI6WzQ4XSwiMTE4
IjpbNDldLCIxMTkiOls1MF0sIjEyMCI6WzUxXSwiMTIxIjpbNTJdLCIxMjIiOls1
M10sIjEyNSI6WzU0XSwiMTI2IjpbNTVdLCIxMjciOls1Nl0sIjEyOCI6WzU3XSwi
MTI5IjpbNThdLCIxMzAiOls1OV0sIjEzMSI6WzYwXSwiMTMyIjpbNjFdLCIxMzMi
Ols2Ml0sIjEzNiI6WzYzXSwiMTM3IjpbNjRdLCIxMzgiOls2NV0sIjEzOSI6WzY2
XSwiMTQxIjpbNjddLCIxNDIiOls2OF0sIjE0MyI6WzY5XSwiMTQ1IjpbNzBdLCIx
NDYiOls3MV0sIjE0OSI6WzcyXSwiMTUwIjpbNzNdLCIxNTIiOls3NF0sIjE1MyI6
Wzc1XSwiMTU0IjpbNzZdLCIxNTUiOls3N10sIjE1NiI6Wzc4XSwiMTU3IjpbNzld
LCIxNTgiOls4MF0sIjE2MCI6WzgxXSwiMTYxIjpbODJdLCIxNjIiOls4M10sIjE2
MyI6Wzg0XSwiMTY0IjpbODVdLCIxNjUiOls4Nl0sIjE2NiI6Wzg3XSwiMTY3Ijpb
ODhdLCIxNjgiOls4OV0sIjE2OSI6WzkwXSwiMTcwIjpbOTFdLCIxNzEiOls5Ml0s
IjE3MiI6WzkzXSwiMTczIjpbOTRdLCIxNzQiOls5NV0sIjE3NSI6Wzk2XSwiMTc2
IjpbOTddLCIxNzciOls5OF0sIjE3OSI6Wzk5XSwiMTgwIjpbMTAwXSwiMTgxIjpb
MTAxXSwiMTgzIjpbMTAyXSwiMTg0IjpbMTAzXSwiMTg1IjpbMTA0XSwiMTg3Ijpb
MTA1XSwiMTg4IjpbMTA2XSwiMTkwIjpbMTA3XSwiMTkyIjpbMTA4XSwiMTkzIjpb
MTA5XSwiMTk0IjpbMTEwXSwiMTk3IjpbMTExXSwiMjAzIjpbMTEyXSwiMjA0Ijpb
MTEzXSwiMjA1IjpbMTE0XSwiMjA2IjpbMTE1XSwiMjA4IjpbMTE2XSwiMjEwIjpb
MTE3XSwiMjExIjpbMTE4XSwiMjEyIjpbMTE5XSwiMjEzIjpbMTIwXSwiMjE0Ijpb
MTIxXSwiMjE1IjpbMTIyXSwiMjE2IjpbMTIzXSwiMjE3IjpbMTI0XSwiMjE4Ijpb
MTI1XSwiMjE5IjpbMTI2XSwiMjIwIjpbMTI3XSwiMjIxIjpbMTI4XSwiMjIyIjpb
MTI5XSwiMjIzIjpbMTMwXSwiMjI0IjpbMTMxXSwiMjI1IjpbMTMyXSwiMjI2Ijpb
MTMzXSwiMjI3IjpbMTM0XSwiMjI4IjpbMTM1XSwiMjI5IjpbMTM2XSwiMjMwIjpb
MTM3XSwiMjMxIjpbMTM4XSwiMjMyIjpbMTM5XSwiMjMzIjpbMTQwXSwiMjM0Ijpb
MTQxXSwiMjM1IjpbMTQyXSwiMjM2IjpbMTQzXSwiMjM3IjpbMTQ0XSwiMjM4Ijpb
MTQ1XSwiMjM5IjpbMTQ2XSwiMjQwIjpbMTQ3XSwiMjQxIjpbMTQ4XSwiMjQyIjpb
MTQ5XSwiMjQzIjpbMTUwXSwiMjQ0IjpbMTUxXSwiMjQ1IjpbMTUyXSwiMjQ2Ijpb
MTUzXSwiMjQ3IjpbMTU0XSwiMjQ4IjpbMTU1XSwiMjQ5IjpbMTU2XSwiMjUwIjpb
MTU3XSwiMjUxIjpbMTU4XSwiMjUyIjpbMTU5XSwiMjUzIjpbMTYwXSwiMjU0Ijpb
MTYxXSwiMjU1IjpbMTYyXSwiMjU2IjpbMTYzXSwiMjU3IjpbMTY0XSwiMjU4Ijpb
MTY1XSwiMjU5IjpbMTY2XSwiMjYwIjpbMTY3XSwiMjYxIjpbMTY4XSwiMjYyIjpb
MTY5XSwiMjYzIjpbMTcwXSwiMjY0IjpbMTcxXSwiMjY1IjpbMTcyXSwiMjY2Ijpb
MTczXSwiMjY3IjpbMTc0XSwiMjY4IjpbMTc1XSwiMjY5IjpbMTc2XSwiMjcwIjpb
MTc3XSwiMjcxIjpbMTc4XSwiMjcyIjpbMTc5XSwiMjczIjpbMTgwXSwiMjc0Ijpb
MTgxXSwiMjc1IjpbMTgyXSwiMjc2IjpbMTgzXSwiMjc3IjpbMTg0XSwiMjc4Ijpb
MTg1XSwiMjc5IjpbMTg2XSwiMjgwIjpbMTg3XSwiMjgxIjpbMTg4XSwiMjgyIjpb
MTg5XSwiMjgzIjpbMTkwXSwiMjg0IjpbMTkxXSwiMjg1IjpbMTkyXSwiMjg2Ijpb
MTkzXSwiMjg3IjpbMTk0XSwiMjg4IjpbMTk1XSwiMjg5IjpbMTk2XSwiMjkwIjpb
MTk3XSwiMjkxIjpbMTk4XSwiMjkyIjpbMTk5XSwiMjkzIjpbMjAwXSwiMjk0Ijpb
MjAxXSwiMjk1IjpbMjAyXSwiMjk2IjpbMjAzXSwiMjk3IjpbMjA0XSwiMjk4Ijpb
MjA1XSwiMjk5IjpbMjA2XSwiMzAwIjpbMjA3XSwiMzAxIjpbMjA4XSwiMzAyIjpb
MjA5XSwiMzAzIjpbMjEwXSwiMzA0IjpbMjExXSwiMzA1IjpbMjEyXSwiMzA2Ijpb
MjEzXSwiMzA3IjpbMjE0XSwiMzA4IjpbMjE1XSwiMzA5IjpbMjE2XSwiMzEwIjpb
MjE3XSwiMzExIjpbMjE4XSwiMzEyIjpbMjE5XSwiMzEzIjpbMjIwXSwiMzE0Ijpb
MjIxXSwiMzE1IjpbMjIyXSwiMzE2IjpbMjIzXSwiMzE3IjpbMjI0XSwiMzE4Ijpb
MjI1XSwiMzE5IjpbMjI2XSwiMzIwIjpbMjI3XSwiMzIxIjpbMjI4XSwiMzIyIjpb
MjI5XSwiMzIzIjpbMjMwXSwiMzI0IjpbMjMxXSwiMzI1IjpbMjMyXSwiMzI2Ijpb
MjMzXSwiMzI3IjpbMjM0XSwiMzI4IjpbMjM1XSwiMzI5IjpbMjM2XSwiMzMwIjpb
MjM3XSwiMzMxIjpbMjM4XSwiMzMyIjpbMjM5XSwiMzMzIjpbMjQwXSwiMzM0Ijpb
MjQxXSwiMzM1IjpbMjQyXSwiMzM2IjpbMjQzXSwiMzM3IjpbMjQ0XSwiMzM4Ijpb
MjQ1XSwiMzM5IjpbMjQ2XSwiMzQwIjpbMjQ3XSwiMzQxIjpbMjQ4XSwiMzQyIjpb
MjQ5XSwiMzQzIjpbMjUwXSwiMzQ0IjpbMjUxXSwiMzQ1IjpbMjUyXSwiMzQ2Ijpb
MjUzXSwiMzQ3IjpbMjU0XSwiMzQ4IjpbMjU1XSwiMzQ5IjpbMjU2XSwiMzUwIjpb
MjU3XSwiMzUxIjpbMjU4XSwiMzUyIjpbMjU5XSwiMzUzIjpbMjYwXSwiMzU0Ijpb
MjYxXSwiMzU1IjpbMjYyXSwiMzU2IjpbMjYzXSwiMzU3IjpbMjY0XSwiMzU4Ijpb
MjY1XSwiMzU5IjpbMjY2XSwiMzYwIjpbMjY3XSwiMzYxIjpbMjY4XSwiMzYyIjpb
MjY5XSwiMzYzIjpbMjcwXSwiMzY0IjpbMjcxXSwiMzY1IjpbMjcyXSwiMzY2Ijpb
MjczXSwiMzY3IjpbMjc0XSwiMzY4IjpbMjc1XSwiMzY5IjpbMjc2XSwiMzcwIjpb
Mjc3XSwiMzcxIjpbMjc4XSwiMzcyIjpbMjc5XSwiMzczIjpbMjgwXSwiMzc0Ijpb
MjgxXSwiMzc1IjpbMjgyXSwiMzc2IjpbMjgzXSwiMzc3IjpbMjg0XSwiMzc4Ijpb
Mjg1XSwiMzc5IjpbMjg2XSwiMzgwIjpbMjg3XSwiMzgxIjpbMjg4XSwiMzgyIjpb
Mjg5XSwiMzgzIjpbMjkwXSwiMzg0IjpbMjkxXSwiMzg1IjpbMjkyXSwiMzg2Ijpb
MjkzXSwiMzg3IjpbMjk0XSwiMzg4IjpbMjk1XSwiMzg5IjpbMjk2XSwiMzkwIjpb
Mjk3XSwiMzkxIjpbMjk4XSwiMzkyIjpbMjk5XSwiMzkzIjpbMzAwXSwiMzk0Ijpb
MzAxXSwiMzk1IjpbMzAyXSwiMzk2IjpbMzAzXSwiMzk3IjpbMzA0XSwiMzk4Ijpb
MzA1XSwiMzk5IjpbMzA2XSwiNDAwIjpbMzA3XSwiNDAxIjpbMzA4XSwiNDAyIjpb
MzA5XSwiNDAzIjpbMzEwXSwiNDA0IjpbMzExXSwiNDA1IjpbMzEyXSwiNDA2Ijpb
MzEzXSwiNDA3IjpbMzE0XSwiNDA4IjpbMzE1XSwiNDA5IjpbMzE2XSwiNDEwIjpb
MzE3XSwiNDExIjpbMzE4XSwiNDEyIjpbMzE5XSwiNDEzIjpbMzIwXSwiNDE0Ijpb
MzIxXSwiNDE1IjpbMzIyXSwiNDE2IjpbMzIzXSwiNDE3IjpbMzI0XSwiNDE4Ijpb
MzI1XSwiNDE5IjpbMzI2XSwiNDIwIjpbMzI3XSwiNDIxIjpbMzI4XSwiNDIyIjpb
MzI5XSwiNDIzIjpbMzMwXSwiNDI0IjpbMzMxXSwiNDI1IjpbMzMyXSwiNDI2Ijpb
MzMzXSwiNDI3IjpbMzM0XSwiNDI4IjpbMzM1XSwiNDI5IjpbMzM2XSwiNDMwIjpb
MzM3XSwiNDMxIjpbMzM4XSwiNDMyIjpbMzM5XSwiNDMzIjpbMzQwXSwiNDM0Ijpb
MzQxXSwiNDM1IjpbMzQyXSwiNDM2IjpbMzQzXSwiNDM3IjpbMzQ0XSwiNDM4Ijpb
MzQ1XSwiNDM5IjpbMzQ2XSwiNDQwIjpbMzQ3XSwiNDQxIjpbMzQ4XSwiNDQyIjpb
MzQ5XSwiNDQzIjpbMzUwXSwiNDQ0IjpbMzUxXSwiNDQ1IjpbMzUyXSwiNDQ2Ijpb
MzUzXSwiNDQ3IjpbMzU0XSwiNDQ4IjpbMzU1XSwiNDQ5IjpbMzU2XSwiNDUwIjpb
MzU3XSwiNDUxIjpbMzU4XSwiNDUyIjpbMzU5XSwiNDUzIjpbMzYwXSwiNDU0Ijpb
MzYxXSwiNDU1IjpbMzYyXSwiNDU2IjpbMzYzXSwiNDU3IjpbMzY0XSwiNDU4Ijpb
MzY1XSwiNDU5IjpbMzY2XSwiNDYwIjpbMzY3XSwiNDYxIjpbMzY4XSwiNDYyIjpb
MzY5XSwiNDYzIjpbMzcwXSwiNDY0IjpbMzcxXSwiNDY1IjpbMzcyXSwiNDY2Ijpb
MzczXSwiNDY3IjpbMzc0XSwiNDY4IjpbMzc1XSwiNDY5IjpbMzc2XSwiNDcwIjpb
Mzc3XSwiNDcxIjpbMzc4XSwiNDcyIjpbMzc5XSwiNDczIjpbMzgwXSwiNDc0Ijpb
MzgxXSwiNDc1IjpbMzgyXSwiNDc2IjpbMzgzXSwiNDc3IjpbMzg0XSwiNDc4Ijpb
Mzg1XSwiNDc5IjpbMzg2XSwiNDgwIjpbMzg3XSwiNDgxIjpbMzg4XSwiNDgyIjpb
Mzg5XSwiNDgzIjpbMzkwXSwiNDg0IjpbMzkxXSwiNDg1IjpbMzkyXSwiNDg2Ijpb
MzkzXSwiNDg3IjpbMzk0XSwiNDg4IjpbMzk1XSwiNDg5IjpbMzk2XSwiNDkwIjpb
Mzk3XSwiNDkxIjpbMzk4XSwiNDkyIjpbMzk5XSwiNDkzIjpbNDAwXSwiNDk0Ijpb
NDAxXSwiNDk1IjpbNDAyXSwiNDk2IjpbNDAzXSwiNDk3IjpbNDA0XSwiNDk4Ijpb
NDA1XSwiNDk5IjpbNDA2XSwiNTAwIjpbNDA3XSwiNTAxIjpbNDA4XSwiNTAyIjpb
NDA5XSwiNTAzIjpbNDEwXSwiNTA0IjpbNDExXSwiNTA1IjpbNDEyXSwiNTA2Ijpb
NDEzXSwiNTA3IjpbNDE0XSwiNTA4IjpbNDE1XSwiNTA5IjpbNDE2XSwiNTEwIjpb
NDE3XSwiNTExIjpbNDE4XSwiNTEyIjpbNDE5XSwiNTEzIjpbNDIwXSwiNTE0Ijpb
NDIxXSwiNTE1IjpbNDIyXSwiNTE2IjpbNDIzXSwiNTE3IjpbNDI0XSwiNTE4Ijpb
NDI1XSwiNTE5IjpbNDI2XSwiNTIwIjpbNDI3XSwiNTIxIjpbNDI4XSwiNTIyIjpb
NDI5XSwiNTIzIjpbNDMwXSwiNTI0IjpbNDMxXSwiNTI1IjpbNDMyXSwiNTI2Ijpb
NDMzXSwiNTI3IjpbNDM0XSwiNTI4IjpbNDM1XSwiNTI5IjpbNDM2XSwiNTMwIjpb
NDM3XSwiNTMxIjpbNDM4XSwiNTMyIjpbNDM5XSwiNTMzIjpbNDQwXSwiNTM0Ijpb
NDQxXSwiNTM1IjpbNDQyXSwiNTM2IjpbNDQzXSwiNTM3IjpbNDQ0XSwiNTM4Ijpb
NDQ1XSwiNTM5IjpbNDQ2XSwiNTQwIjpbNDQ3XSwiNTQxIjpbNDQ4XSwiNTQyIjpb
NDQ5XSwiNTQzIjpbNDUwXSwiNTQ0IjpbNDUxXSwiNTQ1IjpbNDUyXSwiNTQ2Ijpb
NDUzXSwiNTQ3IjpbNDU0XSwiNTQ4IjpbNDU1XSwiNTQ5IjpbNDU2XSwiNTUwIjpb
NDU3XSwiNTUxIjpbNDU4XSwiNTUyIjpbNDU5XSwiNTUzIjpbNDYwXSwiNTU0Ijpb
NDYxXSwiNTU1IjpbNDYyXSwiNTU2IjpbNDYzXSwiNTU3IjpbNDY0XSwiNTU4Ijpb
NDY1XSwiNTU5IjpbNDY2XSwiNTYwIjpbNDY3XSwiNTYxIjpbNDY4XSwiNTYyIjpb
NDY5XSwiNTYzIjpbNDcwXSwiNTY0IjpbNDcxXSwiNTY1IjpbNDcyXSwiNTY2Ijpb
NDczXSwiNTY3IjpbNDc0XSwiNTY4IjpbNDc1XSwiNTY5IjpbNDc2XSwiNTcwIjpb
NDc3XSwiNTcxIjpbNDc4XSwiNTcyIjpbNDc5XSwiNTczIjpbNDgwXSwiNTc0Ijpb
NDgxXSwiNTc1IjpbNDgyXSwiNTc2IjpbNDgzXSwiNTc3IjpbNDg0XSwiNTc4Ijpb
NDg1XSwiNTc5IjpbNDg2XSwiNTgwIjpbNDg3XSwiNTgxIjpbNDg4XSwiNTgyIjpb
NDg5XSwiNTgzIjpbNDkwXSwiNTg0IjpbNDkxXSwiNTg1IjpbNDkyXSwiNTg2Ijpb
NDkzXSwiNTg3IjpbNDk0XSwiNTg4IjpbNDk1XSwiNTg5IjpbNDk2XSwiNTkwIjpb
NDk3XSwiNTkxIjpbNDk4XSwiNTkyIjpbNDk5XSwiNTkzIjpbNTAwXSwiNTk0Ijpb
NTAxXSwiNTk1IjpbNTAyXSwiNTk2IjpbNTAzXSwiNTk3IjpbNTA0XSwiNTk4Ijpb
NTA1XSwiNTk5IjpbNTA2XSwiNjAwIjpbNTA3XSwiNjAxIjpbNTA4XSwiNjAyIjpb
NTA5XSwiNjAzIjpbNTEwXSwiNjA0IjpbNTExXSwiNjA1IjpbNTEyXSwiNjA2Ijpb
NTEzXSwiNjA3IjpbNTE0XSwiNjA4IjpbNTE1XSwiNjA5IjpbNTE2XSwiNjEwIjpb
NTE3XSwiNjExIjpbNTE4XSwiNjEyIjpbNTE5XSwiNjEzIjpbNTIwXSwiNjE0Ijpb
NTIxXSwiNjE1IjpbNTIyXSwiNjE2IjpbNTIzXSwiNjE3IjpbNTI0XSwiNjE4Ijpb
NTI1XSwiNjE5IjpbNTI2XSwiNjIwIjpbNTI3XSwiNjIxIjpbNTI4XSwiNjIyIjpb
NTI5XSwiNjIzIjpbNTMwXSwiNjI0IjpbNTMxXSwiNjI1IjpbNTMyXSwiNjI2Ijpb
NTMzXSwiNjI3IjpbNTM0XSwiNjI4IjpbNTM1XSwiNjI5IjpbNTM2XSwiNjMwIjpb
NTM3XSwiNjMxIjpbNTM4XSwiNjMyIjpbNTM5XSwiNjMzIjpbNTQwXSwiNjM0Ijpb
NTQxXSwiNjM1IjpbNTQyXSwiNjM2IjpbNTQzXSwiNjM3IjpbNTQ0XSwiNjM4Ijpb
NTQ1XSwiNjM5IjpbNTQ2XSwiNjQwIjpbNTQ3XSwiNjQxIjpbNTQ4XSwiNjQyIjpb
NTQ5XSwiNjQzIjpbNTUwXSwiNjQ0IjpbNTUxXSwiNjQ1IjpbNTUyXSwiNjQ2Ijpb
NTUzXSwiNjQ3IjpbNTU0XSwiNjQ4IjpbNTU1XSwiNjQ5IjpbNTU2XSwiNjUwIjpb
NTU3XSwiNjUxIjpbNTU4XSwiNjUyIjpbNTU5XSwiNjUzIjpbNTYwXSwiNjU0Ijpb
NTYxXSwiNjU1IjpbNTYyXSwiNjU2IjpbNTYzXSwiNjU3IjpbNTY0XSwiNjU4Ijpb
NTY1XSwiNjU5IjpbNTY2XSwiNjYwIjpbNTY3XSwiNjYxIjpbNTY4XSwiNjYyIjpb
NTY5XSwiNjYzIjpbNTcwXSwiNjY0IjpbNTcxXSwiNjY1IjpbNTcyXSwiNjY2Ijpb
NTczXSwiNjY3IjpbNTc0XSwiNjY4IjpbNTc1XSwiNjY5IjpbNTc2XSwiNjcwIjpb
NTc3XSwiNjcxIjpbNTc4XSwiNjcyIjpbNTc5XSwiNjczIjpbNTgwXSwiNjc0Ijpb
NTgxXSwiNjc1IjpbNTgyXSwiNjc2IjpbNTgzXSwiNjc3IjpbNTg0XSwiNjc4Ijpb
NTg1XSwiNjc5IjpbNTg2XSwiNjgwIjpbNTg3XSwiNjgxIjpbNTg4XSwiNjgyIjpb
NTg5XSwiNjgzIjpbNTkwXSwiNjg0IjpbNTkxXSwiNjg1IjpbNTkyXSwiNjg2Ijpb
NTkzXSwiNjg3IjpbNTk0XSwiNjg4IjpbNTk1XSwiNjg5IjpbNTk2XSwiNjkwIjpb
NTk3XSwiNjkxIjpbNTk4XSwiNjkyIjpbNTk5XSwiNjkzIjpbNjAwXSwiNjk0Ijpb
NjAxXSwiNjk1IjpbNjAyXSwiNjk2IjpbNjAzXSwiNjk3IjpbNjA0XSwiNjk4Ijpb
NjA1XSwiNjk5IjpbNjA2XSwiNzAwIjpbNjA3XSwiNzAxIjpbNjA4XSwiNzAyIjpb
NjA5XSwiNzAzIjpbNjEwXSwiNzA0IjpbNjExXSwiNzA1IjpbNjEyXSwiNzA2Ijpb
NjEzXSwiNzA3IjpbNjE0XSwiNzA4IjpbNjE1XSwiNzA5IjpbNjE2XSwiNzEwIjpb
NjE3XSwiNzExIjpbNjE4XSwiNzEyIjpbNjE5XSwiNzEzIjpbNjIwXSwiNzE0Ijpb
NjIxXSwiNzE1IjpbNjIyXSwiNzE2IjpbNjIzXSwiNzE3IjpbNjI0XSwiNzE4Ijpb
NjI1XSwiNzE5IjpbNjI2XSwiNzIwIjpbNjI3XSwiNzIxIjpbNjI4XSwiNzIyIjpb
NjI5XSwiNzIzIjpbNjMwXSwiNzI0IjpbNjMxXSwiNzI1IjpbNjMyXSwiNzI2Ijpb
NjMzXSwiNzI3IjpbNjM0XSwiNzI4IjpbNjM1XSwiNzI5IjpbNjM2XSwiNzMwIjpb
NjM3XSwiNzMxIjpbNjM4XSwiNzMyIjpbNjM5XSwiNzMzIjpbNjQwXSwiNzM0Ijpb
NjQxXSwiNzM1IjpbNjQyXSwiNzM2IjpbNjQzXSwiNzM3IjpbNjQ0XSwiNzM4Ijpb
NjQ1XSwiNzM5IjpbNjQ2XSwiNzQwIjpbNjQ3XSwiNzQxIjpbNjQ4XSwiNzQyIjpb
NjQ5XSwiNzQzIjpbNjUwXSwiNzQ0IjpbNjUxXSwiNzQ1IjpbNjUyXSwiNzQ2Ijpb
NjUzXSwiNzQ3IjpbNjU0XSwiNzQ4IjpbNjU1XSwiNzQ5IjpbNjU2XSwiNzUwIjpb
NjU3XSwiNzUxIjpbNjU4XSwiNzUyIjpbNjU5XSwiNzUzIjpbNjYwXSwiNzU0Ijpb
NjYxXSwiNzU1IjpbNjYyXSwiNzU2IjpbNjYzXSwiNzU3IjpbNjY0XSwiNzU4Ijpb
NjY1XSwiNzU5IjpbNjY2XSwiNzYwIjpbNjY3XSwiNzYxIjpbNjY4XSwiNzYyIjpb
NjY5XSwiNzYzIjpbNjcwXSwiNzY0IjpbNjcxXSwiNzY1IjpbNjcyXSwiNzY2Ijpb
NjczXSwiNzY3IjpbNjc0XSwiNzY4IjpbNjc1XSwiNzY5IjpbNjc2XSwiNzcwIjpb
Njc3XSwiNzcxIjpbNjc4XSwiNzcyIjpbNjc5XSwiNzczIjpbNjgwXSwiNzc0Ijpb
NjgxXSwiNzc1IjpbNjgyXSwiNzc2IjpbNjgzXSwiNzc3IjpbNjg0XSwiNzc4Ijpb
Njg1XSwiNzc5IjpbNjg2XSwiNzgwIjpbNjg3XSwiNzgxIjpbNjg4XSwiNzgyIjpb
Njg5XSwiNzgzIjpbNjkwXSwiNzg0IjpbNjkxXSwiNzg1IjpbNjkyXSwiNzg2Ijpb
NjkzXSwiNzg3IjpbNjk0XSwiNzg4IjpbNjk1XSwiNzg5IjpbNjk2XSwiNzkwIjpb
Njk3XSwiNzkxIjpbNjk4XSwiNzkyIjpbNjk5XSwiNzkzIjpbNzAwXSwiNzk0Ijpb
NzAxXSwiNzk1IjpbNzAyXSwiNzk2IjpbNzAzXSwiNzk3IjpbNzA0XSwiNzk4Ijpb
NzA1XSwiNzk5IjpbNzA2XSwiODAwIjpbNzA3XSwiODAxIjpbNzA4XSwiODAyIjpb
NzA5XSwiODAzIjpbNzEwXSwiODA0IjpbNzExXX19LCJjb29yZCI6W1s1LjU1MTks
LTAuNzU4Ml0sWzAuMzQxOCwtMy43Nzk0XSxbLTAuMTIwNiwtMy4yMTQ1XSxbLTAu
NTcyLC0yLjMzNV0sWy0wLjAyNjIsLTIuNDE0OV0sWzMuMTIyNSwtMy4xMTA5XSxb
NS43OTUyLDAuMDU3NV0sWy01Ljc5NTIsLTEuODk2OF0sWzAuMjU2OSwtNC4wMjEz
XSxbMC4yMTY4LC0zLjM0MDFdLFswLjE3NTgsLTEuODg3Ml0sWy0wLjE1OCwtMy41
MDQ0XSxbNC40ODMsMi4xNTRdLFstMC43OTYsLTMuODcyNF0sWzAuMTY0OSwtMy44
NjA5XSxbLTAuNjg0LC0yLjkzNTVdLFs1LjA4OTYsMC45NzVdLFstMC40Njg4LDQu
MjkzNl0sWy0wLjMwODksLTMuMDM2OV0sWy0wLjQzODMsLTQuMzAyNl0sWzAuMjQ4
NSwtMy4xNTEyXSxbMS45MDE4LC0yLjEyOTFdLFstMC42Mjc3LC0zLjUzOTFdLFst
MC40NDcyLC0zLjMxMTRdLFswLjQxMjYsNC4zMDM3XSxbLTAuMDQ0NiwzLjQ2MjRd
LFstMS4wMTM3LC0zLjE5NjZdLFstMS4xMDI3LC0zLjY3NzRdLFs1LjAyNzEsMC4w
MTUyXSxbLTAuMDU0LC0yLjk0NjVdLFsxLjMyMTQsLTMuMTEwMV0sWy0wLjMwNywt
NC4wNTkxXSxbMi45ODExLDMuNDk5M10sWy0xLjA4ODQsMy4zNTQzXSxbLTAuOTQ1
MiwtNC4wMzZdLFstMC42MjA5LC0zLjEwODZdLFsyLjE3NDUsLTIuODg2M10sWy0x
LjA2MjEsLTMuMzkxMl0sWy0wLjg4NDMsLTMuNTA1Ml0sWy01LjMyODUsMC4yNzc5
XSxbLTQuNTg3MiwyLjAzMTddLFswLjEwMzQsLTQuMTU0NV0sWzAuNTUyOSwtMi4y
NjkyXSxbLTAuMjQzMiwtNC4zMDM3XSxbMC45OTYyLDMuMzYyN10sWzMuNzc3Niwy
LjAyOV0sWy0wLjExOTQsLTAuODkwNV0sWy0zLjgzODcsMi44NTg3XSxbMS4yODUz
LDQuMTgwMV0sWy0yLjE4MDcsMy44OTM1XSxbLTMuMDE3NCwzLjQ2ODVdLFstMy44
NjI4LDIuMDI4N10sWy0wLjg3NzMsLTMuMDMzOF0sWy0wLjc4NjUsLTQuMTYxNF0s
Wy0wLjU1NCwtMy45OTJdLFstMC4wNTk4LC00LjI0OTNdLFstMC42MjA0LC00LjI1
NDRdLFsyLjAwMjMsMy4wOTYyXSxbMS4wNTAzLC0yLjU4NTJdLFswLjM2MTksLTMu
NDI2Ml0sWy01LjE0OSwwLjkxMjFdLFstMS4wMzcsLTMuODUxMV0sWzIuMTI4Nywz
LjkxNzldLFszLjc5NDUsMi44ODMxXSxbMC4xMjc2LC0zLjA0NjhdLFs1LjcyNTgs
LTIuNjA2NF0sWy01LjU2NTQsLTAuMzUxXSxbLTAuMzY4MiwtMy42MzMzXSxbMi45
NDE0LDIuNjc2XSxbLTAuMDc5NiwtMy45NTY2XSxbLTEuMzM4NSw0LjE3NDFdLFs1
Ljc1NzgsLTEuNjU5XSxbMC4wNzM0LC0zLjU3MThdLFstNS43MTgxLC0xLjAzNzZd
LFstMy4wNTMxLDIuNjM1Ml0sWy0wLjgxNDQsLTMuMjY3NF0sWy0wLjM4NjUsLTIu
ODY3OF0sWzQuNjMwNywxLjQyMDldLFstNC42NDg2LDEuMzEzM10sWy0yLjA5ODMs
My4wNjY4XV0sInZlcnRleC5jb2wiOlsicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3
KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgx
MTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMx
MzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIx
Nyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIs
MTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3
LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5
MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3
LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTks
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIs
MTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5
NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcs
MTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2Jh
KDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywx
MTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkw
MTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3
KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcs
OTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5
NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcs
MTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2Jh
KDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzks
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0
MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgs
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3
OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywx
NTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkw
MTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNyki
LCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcs
OTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5
MDE5NjA3ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEz
NykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjMx
LDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNykiXSwiYmciOlsicmdiYSgyNTUsMjU1
LDI1NSwxKSJdLCJsYWJlbCI6WyIxMTEgUmFuY2giLCJBZG9iZSBIaWxsIiwiQXJ0
aWZhY3QgSGlsbCIsIkFzaCBUZXJyYWNlIiwiQVogQkI6MzoyMiIsIkFaIENDOjE6
MyIsIkFaIENDOjI6MTg1IiwiQVogQ0M6MjozMyhCTE0pIiwiQmFqYWRhIFNpdGUi
LCJCYXlsZXNzIFJ1aW4iLCJCaWcgQmVsbCIsIkJyYWR5IFdhc2giLCJCdXphbiIs
IkNhY3R1cyBGb3Jlc3QiLCJDYXNhIEJ1ZW5hIiwiQ2FzYSBHcmFuZGUiLCJDbHVm
ZiBSYW5jaCIsIkNvbnRpbmVudGFsIFNpdGUiLCJDcmVzY2VudCBTaXRlIiwiQ3Jp
c21vbiIsIkN1cnRpcyIsIkN1cnRpcyBSdWluKEJ1ZW5hIFZpc3RhKSIsIkRhdmlz
IFJhbmNoIFNpdGUiLCJEdWRsZXl2aWxsZSBNb3VuZCIsIkVhcnZlbiBGbGF0IFNp
dGUiLCJFbCBQb2x2b3JvbiIsIkVsbGlvdHQgU2l0ZSIsIkVzY2FsYW50ZSIsIkZp
c2NoZXIgc2l0ZSIsIkZsaWVnZXIiLCJGb3J0IEdyYW50IFB1ZWJsbyIsIkdlcm1h
bm4gU2l0ZSIsIkdpYmJvbiAgU3ByaW5ncyIsIkdvYXQgSGlsbCIsIkdyYW5kIENh
bmFsIiwiSGFieSBSYW5jaCIsIkhpZ2ggTWVzYSIsIkpvc2UgU29sYXMgUnVpbiIs
Ikp1bmt5YXJkIFNpdGUiLCJMYXMgQ2Fub3BhcyIsIkxhcyBDb2xpbmFzIiwiTGFz
IEZvc2FzIiwiTGVhdmVydG9uIiwiTG9zIEd1YW5hY29zIiwiTG9zIE1vcnRlcm9z
IiwiTG9zIE11ZXJ0b3MiLCJMb3N0IE1vdW5kIiwiTWFyaWppbGRhIFJ1aW4iLCJN
ZXNhIEdyYW5kZSIsIk1ldGhvZGlzdCBDaHVyY2giLCJNdXJwaHkgU2l0ZS0tU2Fm
Zm9yZCBFYXN0IiwiT3dlbnMtQ29sdmluIiwiUGlwZXIgU3ByaW5ncyIsIlB1ZWJs
byBCbGFuY28iLCJQdWVibG8gZGVsIE1vbnRlIiwiUHVlYmxvIEdyYW5kZSIsIlB1
ZWJsbyBTYWxhZG8iLCJSYWJpZCBSdWluIiwiUmF0dGxlc25ha2UgTWVzYSIsIlJl
ZXZlIFJ1aW4iLCJSaWNoYXJkc29uIE9yY2hhcmQiLCJSaWxsaXRvIEZhbiIsIlJp
bmNvbiBDYW55b24iLCJTYW4gWGF2aWVyIEJyaWRnZSIsIlNlY29uZCBDYW55b24g
Q29tcG91bmQiLCJTaGFyb24gU2l0ZSIsIlNwZWFyIFJhbmNoIiwiU3dpbmdsZSdz
IFNhbXBsZSIsIlRhbnF1ZSBWZXJkZSBWaWxsYWdlIiwiVHJlcyBQdWVibG9zIiwi
VHdpbiBIYXdrcyIsIlZlcmVzIiwiV2ViYiBSdWluIiwiV2VzIEplcm5pZ2FuIFNp
dGUiLCJXaGlwdGFpbCBSdWluIiwiV2hpdG1lciIsIldyaWdodCIsIll1bWEgV2Fz
aC0tVHVjc29uIiwiWXVtYSBXYXNoIFNpdGUiLCJaYW5hcmRlbGxpIFNpdGUiXSwi
eGxhYiI6WyJ0PTMtNCJdLCJqaXR0ZXIiOltmYWxzZV0sInVzZWFycm93cyI6W2Zh
bHNlXSwieGxpbSI6Wy01Ljc5NTIsNS43OTUyXSwieWxpbSI6Wy01LjMzNzYsNS4z
Mzc2XX19LHsic3RhcnQiOls0XSwiZW5kIjpbNV0sImRhdGEiOnsiYWN0aXZlIjp7
Im5vZGVzIjp7IjEiOlswXSwiMiI6WzFdLCIzIjpbMl0sIjQiOlszXSwiNSI6WzRd
LCI2IjpbNV0sIjciOls2XSwiOCI6WzddLCI5IjpbOF0sIjEwIjpbOV0sIjExIjpb
MTBdLCIxMiI6WzExXSwiMTMiOlsxMl0sIjE0IjpbMTNdLCIxNSI6WzE0XSwiMTYi
OlsxNV0sIjE3IjpbMTZdLCIxOCI6WzE3XSwiMTkiOlsxOF0sIjIwIjpbMTldLCIy
MSI6WzIwXSwiMjIiOlsyMV0sIjIzIjpbMjJdLCIyNCI6WzIzXSwiMjUiOlsyNF0s
IjI2IjpbMjVdLCIyNyI6WzI2XSwiMjgiOlsyN10sIjI5IjpbMjhdLCIzMCI6WzI5
XSwiMzEiOlszMF0sIjMyIjpbMzFdLCIzMyI6WzMyXSwiMzQiOlszM10sIjM1Ijpb
MzRdLCIzNiI6WzM1XSwiMzciOlszNl0sIjM4IjpbMzddLCIzOSI6WzM4XSwiNDAi
OlszOV0sIjQxIjpbNDBdLCI0MiI6WzQxXSwiNDMiOls0Ml0sIjQ0IjpbNDNdLCI0
NSI6WzQ0XSwiNDYiOls0NV0sIjQ3IjpbNDZdLCI0OCI6WzQ3XSwiNDkiOls0OF0s
IjUwIjpbNDldLCI1MSI6WzUwXSwiNTIiOls1MV0sIjUzIjpbNTJdLCI1NCI6WzUz
XSwiNTUiOls1NF0sIjU2IjpbNTVdLCI1NyI6WzU2XSwiNTgiOls1N10sIjU5Ijpb
NThdLCI2MCI6WzU5XSwiNjEiOls2MF0sIjYyIjpbNjFdLCI2MyI6WzYyXSwiNjQi
Ols2M10sIjY1IjpbNjRdLCI2NiI6WzY1XSwiNjciOls2Nl0sIjY4IjpbNjddLCI2
OSI6WzY4XSwiNzAiOls2OV0sIjcxIjpbNzBdLCI3MiI6WzcxXSwiNzMiOls3Ml0s
Ijc0IjpbNzNdLCI3NSI6Wzc0XSwiNzYiOls3NV0sIjc3IjpbNzZdLCI3OCI6Wzc3
XSwiNzkiOls3OF0sIjgwIjpbNzldfSwiZWRnZXMiOnsiMSI6WzBdLCIyIjpbMV0s
IjMiOlsyXSwiNCI6WzNdLCI1IjpbNF0sIjYiOls1XSwiNyI6WzZdLCI5IjpbN10s
IjExIjpbOF0sIjEyIjpbOV0sIjEzIjpbMTBdLCIxNCI6WzExXSwiMTUiOlsxMl0s
IjE2IjpbMTNdLCIxOCI6WzE0XSwiMjAiOlsxNV0sIjIxIjpbMTZdLCIyMiI6WzE3
XSwiMjMiOlsxOF0sIjI0IjpbMTldLCIyNiI6WzIwXSwiMzciOlsyMV0sIjM4Ijpb
MjJdLCIzOSI6WzIzXSwiNDAiOlsyNF0sIjQyIjpbMjVdLCI0NCI6WzI2XSwiNDUi
OlsyN10sIjQ2IjpbMjhdLCI0OCI6WzI5XSwiNTAiOlszMF0sIjUxIjpbMzFdLCI1
MyI6WzMyXSwiNjMiOlszM10sIjY1IjpbMzRdLCI2OCI6WzM1XSwiMTAxIjpbMzZd
LCIxMDIiOlszN10sIjEwNCI6WzM4XSwiMTA1IjpbMzldLCIxMDYiOls0MF0sIjEw
OCI6WzQxXSwiMTA5IjpbNDJdLCIxMTAiOls0M10sIjExMSI6WzQ0XSwiMTEyIjpb
NDVdLCIxMTMiOls0Nl0sIjExNCI6WzQ3XSwiMTE2IjpbNDhdLCIxMTciOls0OV0s
IjExOCI6WzUwXSwiMTE5IjpbNTFdLCIxMjAiOls1Ml0sIjEyMSI6WzUzXSwiMTIy
IjpbNTRdLCIxMjUiOls1NV0sIjEyNiI6WzU2XSwiMTI3IjpbNTddLCIxMjgiOls1
OF0sIjEyOSI6WzU5XSwiMTMwIjpbNjBdLCIxMzEiOls2MV0sIjEzMiI6WzYyXSwi
MTMzIjpbNjNdLCIxMzYiOls2NF0sIjEzNyI6WzY1XSwiMTM4IjpbNjZdLCIxMzki
Ols2N10sIjE0MSI6WzY4XSwiMTQyIjpbNjldLCIxNDMiOls3MF0sIjE0NSI6Wzcx
XSwiMTQ2IjpbNzJdLCIxNDgiOls3M10sIjE0OSI6Wzc0XSwiMTUwIjpbNzVdLCIx
NTIiOls3Nl0sIjE1MyI6Wzc3XSwiMTU0IjpbNzhdLCIxNTUiOls3OV0sIjE1NiI6
WzgwXSwiMTU3IjpbODFdLCIxNTgiOls4Ml0sIjE2MCI6WzgzXSwiMTYxIjpbODRd
LCIxNjIiOls4NV0sIjE2MyI6Wzg2XSwiMTY0IjpbODddLCIxNjUiOls4OF0sIjE2
NiI6Wzg5XSwiMTY3IjpbOTBdLCIxNjgiOls5MV0sIjE2OSI6WzkyXSwiMTcwIjpb
OTNdLCIxNzEiOls5NF0sIjE3MiI6Wzk1XSwiMTczIjpbOTZdLCIxNzQiOls5N10s
IjE3NSI6Wzk4XSwiMTc2IjpbOTldLCIxNzciOlsxMDBdLCIxNzkiOlsxMDFdLCIx
ODAiOlsxMDJdLCIxODEiOlsxMDNdLCIxODMiOlsxMDRdLCIxODQiOlsxMDVdLCIx
ODUiOlsxMDZdLCIxODciOlsxMDddLCIxODgiOlsxMDhdLCIxOTAiOlsxMDldLCIx
OTIiOlsxMTBdLCIxOTMiOlsxMTFdLCIxOTQiOlsxMTJdLCIxOTciOlsxMTNdLCIy
MDMiOlsxMTRdLCIyMDQiOlsxMTVdLCIyMDUiOlsxMTZdLCIyMDYiOlsxMTddLCIy
MDgiOlsxMThdLCIyMTAiOlsxMTldLCIyMTEiOlsxMjBdLCIyMTIiOlsxMjFdLCIy
MTMiOlsxMjJdLCIyMTQiOlsxMjNdLCIyMTUiOlsxMjRdLCIyMTYiOlsxMjVdLCIy
MTciOlsxMjZdLCIyMTgiOlsxMjddLCIyMTkiOlsxMjhdLCIyMjAiOlsxMjldLCIy
MjEiOlsxMzBdLCIyMjIiOlsxMzFdLCIyMjMiOlsxMzJdLCIyMjQiOlsxMzNdLCIy
MjUiOlsxMzRdLCIyMjYiOlsxMzVdLCIyMjciOlsxMzZdLCIyMjgiOlsxMzddLCIy
MjkiOlsxMzhdLCIyMzAiOlsxMzldLCIyMzEiOlsxNDBdLCIyMzIiOlsxNDFdLCIy
MzMiOlsxNDJdLCIyMzQiOlsxNDNdLCIyMzUiOlsxNDRdLCIyMzYiOlsxNDVdLCIy
MzciOlsxNDZdLCIyMzgiOlsxNDddLCIyMzkiOlsxNDhdLCIyNDAiOlsxNDldLCIy
NDEiOlsxNTBdLCIyNDIiOlsxNTFdLCIyNDMiOlsxNTJdLCIyNDUiOlsxNTNdLCIy
NDYiOlsxNTRdLCIyNDciOlsxNTVdLCIyNDgiOlsxNTZdLCIyNDkiOlsxNTddLCIy
NTAiOlsxNThdLCIyNTEiOlsxNTldLCIyNTIiOlsxNjBdLCIyNTMiOlsxNjFdLCIy
NTQiOlsxNjJdLCIyNTUiOlsxNjNdLCIyNTYiOlsxNjRdLCIyNTciOlsxNjVdLCIy
NTgiOlsxNjZdLCIyNTkiOlsxNjddLCIyNjAiOlsxNjhdLCIyNjEiOlsxNjldLCIy
NjIiOlsxNzBdLCIyNjMiOlsxNzFdLCIyNjQiOlsxNzJdLCIyNjUiOlsxNzNdLCIy
NjYiOlsxNzRdLCIyNjciOlsxNzVdLCIyNjgiOlsxNzZdLCIyNjkiOlsxNzddLCIy
NzAiOlsxNzhdLCIyNzEiOlsxNzldLCIyNzIiOlsxODBdLCIyNzMiOlsxODFdLCIy
NzQiOlsxODJdLCIyNzUiOlsxODNdLCIyNzYiOlsxODRdLCIyNzciOlsxODVdLCIy
NzgiOlsxODZdLCIyODIiOlsxODddLCIyODUiOlsxODhdLCIyODYiOlsxODldLCIy
ODgiOlsxOTBdLCIyODkiOlsxOTFdLCIyOTEiOlsxOTJdLCIyOTQiOlsxOTNdLCIy
OTUiOlsxOTRdLCIyOTYiOlsxOTVdLCIyOTciOlsxOTZdLCIyOTgiOlsxOTddLCIy
OTkiOlsxOThdLCIzMDAiOlsxOTldLCIzMDEiOlsyMDBdLCIzMDMiOlsyMDFdLCIz
MDQiOlsyMDJdLCIzMDUiOlsyMDNdLCIzMDYiOlsyMDRdLCIzMDciOlsyMDVdLCIz
MDgiOlsyMDZdLCIzMDkiOlsyMDddLCIzMTAiOlsyMDhdLCIzMTEiOlsyMDldLCIz
MTIiOlsyMTBdLCIzMTMiOlsyMTFdLCIzMTUiOlsyMTJdLCIzMTYiOlsyMTNdLCIz
MTgiOlsyMTRdLCIzMTkiOlsyMTVdLCIzMjAiOlsyMTZdLCIzMjEiOlsyMTddLCIz
MjIiOlsyMThdLCIzMjMiOlsyMTldLCIzMjQiOlsyMjBdLCIzMjUiOlsyMjFdLCIz
MjYiOlsyMjJdLCIzMjciOlsyMjNdLCIzMjgiOlsyMjRdLCIzMjkiOlsyMjVdLCIz
MzAiOlsyMjZdLCIzMzEiOlsyMjddLCIzMzIiOlsyMjhdLCIzMzMiOlsyMjldLCIz
MzQiOlsyMzBdLCIzMzUiOlsyMzFdLCIzMzYiOlsyMzJdLCIzMzciOlsyMzNdLCIz
MzgiOlsyMzRdLCIzMzkiOlsyMzVdLCIzNDEiOlsyMzZdLCIzNDIiOlsyMzddLCIz
NDMiOlsyMzhdLCIzNDQiOlsyMzldLCIzNDUiOlsyNDBdLCIzNDYiOlsyNDFdLCIz
NDciOlsyNDJdLCIzNDgiOlsyNDNdLCIzNDkiOlsyNDRdLCIzNTAiOlsyNDVdLCIz
NTEiOlsyNDZdLCIzNTIiOlsyNDddLCIzNTMiOlsyNDhdLCIzNTQiOlsyNDldLCIz
NTUiOlsyNTBdLCIzNTYiOlsyNTFdLCIzNTciOlsyNTJdLCIzNTgiOlsyNTNdLCIz
NTkiOlsyNTRdLCIzNjAiOlsyNTVdLCIzNjEiOlsyNTZdLCIzNjIiOlsyNTddLCIz
NjMiOlsyNThdLCIzNjQiOlsyNTldLCIzNjUiOlsyNjBdLCIzNjYiOlsyNjFdLCIz
NjciOlsyNjJdLCIzNjgiOlsyNjNdLCIzNjkiOlsyNjRdLCIzNzAiOlsyNjVdLCIz
NzEiOlsyNjZdLCIzNzIiOlsyNjddLCIzNzMiOlsyNjhdLCIzNzQiOlsyNjldLCIz
NzUiOlsyNzBdLCIzNzYiOlsyNzFdLCIzNzciOlsyNzJdLCIzNzgiOlsyNzNdLCIz
NzkiOlsyNzRdLCIzODAiOlsyNzVdLCIzODEiOlsyNzZdLCIzODIiOlsyNzddLCIz
ODMiOlsyNzhdLCIzODQiOlsyNzldLCIzODUiOlsyODBdLCIzODYiOlsyODFdLCIz
ODciOlsyODJdLCIzODgiOlsyODNdLCIzODkiOlsyODRdLCIzOTAiOlsyODVdLCIz
OTEiOlsyODZdLCIzOTIiOlsyODddLCIzOTMiOlsyODhdLCIzOTQiOlsyODldLCIz
OTUiOlsyOTBdLCIzOTYiOlsyOTFdLCIzOTciOlsyOTJdLCIzOTgiOlsyOTNdLCIz
OTkiOlsyOTRdLCI0MDAiOlsyOTVdLCI0MDEiOlsyOTZdLCI0MDIiOlsyOTddLCI0
MDMiOlsyOThdLCI0MDQiOlsyOTldLCI0MDUiOlszMDBdLCI0MDYiOlszMDFdLCI0
MDciOlszMDJdLCI0MDgiOlszMDNdLCI0MDkiOlszMDRdLCI0MTAiOlszMDVdLCI0
MTEiOlszMDZdLCI0MTIiOlszMDddLCI0MTMiOlszMDhdLCI0MTQiOlszMDldLCI0
MTUiOlszMTBdLCI0MTYiOlszMTFdLCI0MTciOlszMTJdLCI0MTgiOlszMTNdLCI0
MTkiOlszMTRdLCI0MjAiOlszMTVdLCI0MjEiOlszMTZdLCI0MjIiOlszMTddLCI0
MjMiOlszMThdLCI0MjQiOlszMTldLCI0MjUiOlszMjBdLCI0MjYiOlszMjFdLCI0
MjciOlszMjJdLCI0MjgiOlszMjNdLCI0MjkiOlszMjRdLCI0MzEiOlszMjVdLCI0
MzIiOlszMjZdLCI0MzMiOlszMjddLCI0MzQiOlszMjhdLCI0MzUiOlszMjldLCI0
MzYiOlszMzBdLCI0MzciOlszMzFdLCI0MzgiOlszMzJdLCI0MzkiOlszMzNdLCI0
NDAiOlszMzRdLCI0NDEiOlszMzVdLCI0NDIiOlszMzZdLCI0NDMiOlszMzddLCI0
NDQiOlszMzhdLCI0NDUiOlszMzldLCI0NDYiOlszNDBdLCI0NDciOlszNDFdLCI0
NDgiOlszNDJdLCI0NDkiOlszNDNdLCI0NTEiOlszNDRdLCI0NTIiOlszNDVdLCI0
NTMiOlszNDZdLCI0NTQiOlszNDddLCI0NTUiOlszNDhdLCI0NTYiOlszNDldLCI0
NTciOlszNTBdLCI0NTgiOlszNTFdLCI0NTkiOlszNTJdLCI0NjAiOlszNTNdLCI0
NjEiOlszNTRdLCI0NjIiOlszNTVdLCI0NjMiOlszNTZdLCI0NjQiOlszNTddLCI0
NjUiOlszNThdLCI0NjYiOlszNTldLCI0NjciOlszNjBdLCI0NjgiOlszNjFdLCI0
NjkiOlszNjJdLCI0NzAiOlszNjNdLCI0NzEiOlszNjRdLCI0NzIiOlszNjVdLCI0
NzMiOlszNjZdLCI0NzQiOlszNjddLCI0NzUiOlszNjhdLCI0NzYiOlszNjldLCI0
NzciOlszNzBdLCI0NzgiOlszNzFdLCI0NzkiOlszNzJdLCI0ODAiOlszNzNdLCI0
ODEiOlszNzRdLCI0ODIiOlszNzVdLCI0ODMiOlszNzZdLCI0ODQiOlszNzddLCI0
ODUiOlszNzhdLCI0ODYiOlszNzldLCI0ODgiOlszODBdLCI0ODkiOlszODFdLCI0
OTAiOlszODJdLCI0OTEiOlszODNdLCI0OTIiOlszODRdLCI0OTMiOlszODVdLCI0
OTQiOlszODZdLCI0OTUiOlszODddLCI0OTYiOlszODhdLCI0OTciOlszODldLCI0
OTgiOlszOTBdLCI0OTkiOlszOTFdLCI1MDAiOlszOTJdLCI1MDEiOlszOTNdLCI1
MDIiOlszOTRdLCI1MDMiOlszOTVdLCI1MDQiOlszOTZdLCI1MDUiOlszOTddLCI1
MDYiOlszOThdLCI1MDciOlszOTldLCI1MDgiOls0MDBdLCI1MDkiOls0MDFdLCI1
MTAiOls0MDJdLCI1MTEiOls0MDNdLCI1MTIiOls0MDRdLCI1MTMiOls0MDVdLCI1
MTUiOls0MDZdLCI1MTYiOls0MDddLCI1MTciOls0MDhdLCI1MTgiOls0MDldLCI1
MTkiOls0MTBdLCI1MjAiOls0MTFdLCI1MjEiOls0MTJdLCI1MjIiOls0MTNdLCI1
MjMiOls0MTRdLCI1MjQiOls0MTVdLCI1MjUiOls0MTZdLCI1MjYiOls0MTddLCI1
MjciOls0MThdLCI1MjgiOls0MTldLCI1MjkiOls0MjBdLCI1MzAiOls0MjFdLCI1
MzEiOls0MjJdLCI1MzIiOls0MjNdLCI1MzMiOls0MjRdLCI1MzQiOls0MjVdLCI1
MzUiOls0MjZdLCI1MzYiOls0MjddLCI1MzciOls0MjhdLCI1MzgiOls0MjldLCI1
MzkiOls0MzBdLCI1NDAiOls0MzFdLCI1NDEiOls0MzJdLCI1NDIiOls0MzNdLCI1
NDMiOls0MzRdLCI1NDQiOls0MzVdLCI1NDUiOls0MzZdLCI1NDYiOls0MzddLCI1
NDciOls0MzhdLCI1NDgiOls0MzldLCI1NDkiOls0NDBdLCI1NTAiOls0NDFdLCI1
NTEiOls0NDJdLCI1NTIiOls0NDNdLCI1NTMiOls0NDRdLCI1NTQiOls0NDVdLCI1
NTUiOls0NDZdLCI1NTYiOls0NDddLCI1NTciOls0NDhdLCI1NTgiOls0NDldLCI1
NTkiOls0NTBdLCI1NjAiOls0NTFdLCI1NjEiOls0NTJdLCI1NjIiOls0NTNdLCI1
NjMiOls0NTRdLCI1NjQiOls0NTVdLCI1NjUiOls0NTZdLCI1NjYiOls0NTddLCI1
NjciOls0NThdLCI1NjgiOls0NTldLCI1NjkiOls0NjBdLCI1NzAiOls0NjFdLCI1
NzEiOls0NjJdLCI1NzIiOls0NjNdLCI1NzMiOls0NjRdLCI1NzQiOls0NjVdLCI1
NzUiOls0NjZdLCI1NzYiOls0NjddLCI1NzciOls0NjhdLCI1NzgiOls0NjldLCI1
NzkiOls0NzBdLCI1ODAiOls0NzFdLCI1ODEiOls0NzJdLCI1ODIiOls0NzNdLCI1
ODMiOls0NzRdLCI1ODQiOls0NzVdLCI1ODUiOls0NzZdLCI1ODYiOls0NzddLCI1
ODciOls0NzhdLCI1ODgiOls0NzldLCI1ODkiOls0ODBdLCI1OTAiOls0ODFdLCI1
OTEiOls0ODJdLCI1OTIiOls0ODNdLCI1OTMiOls0ODRdLCI1OTQiOls0ODVdLCI1
OTUiOls0ODZdLCI1OTYiOls0ODddLCI1OTciOls0ODhdLCI1OTgiOls0ODldLCI1
OTkiOls0OTBdLCI2MDAiOls0OTFdLCI2MDEiOls0OTJdLCI2MDIiOls0OTNdLCI2
MDMiOls0OTRdLCI2MDQiOls0OTVdLCI2MDUiOls0OTZdLCI2MDciOls0OTddLCI2
MDgiOls0OThdLCI2MDkiOls0OTldLCI2MTAiOls1MDBdLCI2MTEiOls1MDFdLCI2
MTIiOls1MDJdLCI2MTMiOls1MDNdLCI2MTQiOls1MDRdLCI2MTUiOls1MDVdLCI2
MTYiOls1MDZdLCI2MTciOls1MDddLCI2MTgiOls1MDhdLCI2MTkiOls1MDldLCI2
MjAiOls1MTBdLCI2MjEiOls1MTFdLCI2MjIiOls1MTJdLCI2MjMiOls1MTNdLCI2
MjQiOls1MTRdLCI2MjUiOls1MTVdLCI2MjYiOls1MTZdLCI2MjciOls1MTddLCI2
MjgiOls1MThdLCI2MjkiOls1MTldLCI2MzAiOls1MjBdLCI2MzEiOls1MjFdLCI2
MzIiOls1MjJdLCI2MzMiOls1MjNdLCI2MzQiOls1MjRdLCI2MzUiOls1MjVdLCI2
MzYiOls1MjZdLCI2MzciOls1MjddLCI2MzgiOls1MjhdLCI2MzkiOls1MjldLCI2
NDAiOls1MzBdLCI2NDEiOls1MzFdLCI2NDMiOls1MzJdLCI2NDQiOls1MzNdLCI2
NDUiOls1MzRdLCI2NDYiOls1MzVdLCI2NDciOls1MzZdLCI2NDgiOls1MzddLCI2
NDkiOls1MzhdLCI2NTAiOls1MzldLCI2NTEiOls1NDBdLCI2NTIiOls1NDFdLCI2
NTMiOls1NDJdLCI2NTQiOls1NDNdLCI2NTYiOls1NDRdLCI2NTciOls1NDVdLCI2
NTgiOls1NDZdLCI2NTkiOls1NDddLCI2NjAiOls1NDhdLCI2NjEiOls1NDldLCI2
NjIiOls1NTBdLCI2NjMiOls1NTFdLCI2NjQiOls1NTJdLCI2NjUiOls1NTNdLCI2
NjYiOls1NTRdLCI2NjciOls1NTVdLCI2NjgiOls1NTZdLCI2NjkiOls1NTddLCI2
NzAiOls1NThdLCI2NzEiOls1NTldLCI2NzIiOls1NjBdLCI2NzMiOls1NjFdLCI2
NzQiOls1NjJdLCI2NzUiOls1NjNdLCI2NzYiOls1NjRdLCI2NzciOls1NjVdLCI2
NzgiOls1NjZdLCI2NzkiOls1NjddLCI2ODAiOls1NjhdLCI2ODEiOls1NjldLCI2
ODIiOls1NzBdLCI2ODMiOls1NzFdLCI2ODQiOls1NzJdLCI2ODUiOls1NzNdLCI2
ODYiOls1NzRdLCI2ODciOls1NzVdLCI2ODgiOls1NzZdLCI2ODkiOls1NzddLCI2
OTAiOls1NzhdLCI2OTEiOls1NzldLCI2OTIiOls1ODBdLCI2OTMiOls1ODFdLCI2
OTQiOls1ODJdLCI2OTUiOls1ODNdLCI2OTYiOls1ODRdLCI2OTciOls1ODVdLCI2
OTgiOls1ODZdLCI2OTkiOls1ODddLCI3MDAiOls1ODhdLCI3MDEiOls1ODldLCI3
MDIiOls1OTBdLCI3MDMiOls1OTFdLCI3MDQiOls1OTJdLCI3MDUiOls1OTNdLCI3
MDYiOls1OTRdLCI3MDciOls1OTVdLCI3MDgiOls1OTZdLCI3MDkiOls1OTddLCI3
MTAiOls1OThdLCI3MTEiOls1OTldLCI3MTMiOls2MDBdLCI3MTQiOls2MDFdLCI3
MTUiOls2MDJdLCI3MTYiOls2MDNdLCI3MTciOls2MDRdLCI3MTgiOls2MDVdLCI3
MTkiOls2MDZdLCI3MjAiOls2MDddLCI3MjEiOls2MDhdLCI3MjIiOls2MDldLCI3
MjMiOls2MTBdLCI3MjQiOls2MTFdLCI3MjUiOls2MTJdLCI3MjYiOls2MTNdLCI3
MjgiOls2MTRdLCI3MjkiOls2MTVdLCI3MzAiOls2MTZdLCI3MzEiOls2MTddLCI3
MzIiOls2MThdLCI3MzMiOls2MTldLCI3MzQiOls2MjBdLCI3MzUiOls2MjFdLCI3
MzYiOls2MjJdLCI3MzciOls2MjNdLCI3MzgiOls2MjRdLCI3MzkiOls2MjVdLCI3
NDAiOls2MjZdLCI3NDEiOls2MjddLCI3NDIiOls2MjhdLCI3NDMiOls2MjldLCI3
NDQiOls2MzBdLCI3NDUiOls2MzFdLCI3NDYiOls2MzJdLCI3NDciOls2MzNdLCI3
NDkiOls2MzRdLCI3NTAiOls2MzVdLCI3NTEiOls2MzZdLCI3NTIiOls2MzddLCI3
NTMiOls2MzhdLCI3NTQiOls2MzldLCI3NTUiOls2NDBdLCI3NTYiOls2NDFdLCI3
NTgiOls2NDJdLCI3NTkiOls2NDNdLCI3NjAiOls2NDRdLCI3NjEiOls2NDVdLCI3
NjIiOls2NDZdLCI3NjMiOls2NDddLCI3NjQiOls2NDhdLCI3NjYiOls2NDldLCI3
NjciOls2NTBdLCI3NjgiOls2NTFdLCI3NjkiOls2NTJdLCI3NzAiOls2NTNdLCI3
NzEiOls2NTRdLCI3NzMiOls2NTVdLCI3NzQiOls2NTZdLCI3NzUiOls2NTddLCI3
NzYiOls2NThdLCI3NzciOls2NTldLCI3NzgiOls2NjBdLCI3NzkiOls2NjFdLCI3
ODAiOls2NjJdLCI3ODEiOls2NjNdLCI3ODIiOls2NjRdLCI3ODMiOls2NjVdLCI3
ODQiOls2NjZdLCI3ODYiOls2NjddLCI3ODciOls2NjhdLCI3ODgiOls2NjldLCI3
ODkiOls2NzBdLCI3OTAiOls2NzFdLCI3OTEiOls2NzJdLCI3OTMiOls2NzNdLCI3
OTQiOls2NzRdLCI3OTUiOls2NzVdLCI3OTYiOls2NzZdLCI3OTciOls2NzddLCI3
OTgiOls2NzhdLCI3OTkiOls2NzldLCI4MDAiOls2ODBdLCI4MDEiOls2ODFdLCI4
MDIiOls2ODJdLCI4MDMiOls2ODNdLCI4MDQiOls2ODRdLCI4MDUiOls2ODVdLCI4
MDYiOls2ODZdLCI4MDciOls2ODddLCI4MDgiOls2ODhdLCI4MDkiOls2ODldLCI4
MTAiOls2OTBdLCI4MTEiOls2OTFdfX0sImNvb3JkIjpbWzUuNDAzOSwtMC4xODI0
XSxbLTAuOTg1MiwtMy41NjkzXSxbMC4wNTEsLTMuMTM1XSxbLTAuMTI3LC0yLjE1
MTRdLFswLjM1NTksLTIuNDc4NF0sWzQuODU1OCwxLjQ2OTZdLFs1LjU0MDEsMC42
NzI1XSxbLTUuNzI1NCwtMi4yNDFdLFswLjM0OTUsLTQuMDczMl0sWzAuMzQ0Miwt
My4yOTg1XSxbMC4xNTQsLTEuODM4N10sWy0wLjU0MTIsLTMuMzI0Ml0sWzQuMTk0
MiwyLjUyNTFdLFstMC43NTkzLC0zLjYxNzldLFswLjE5OTksLTQuMTQyNV0sWzAu
MTA5OCwtMy44MDI4XSxbNC4yNTc5LDEuNzc3MV0sWy0wLjc4Miw0LjI5MTVdLFsw
LjQ4NTgsLTMuNzg0NF0sWy0wLjQ2MTksLTQuMzEyMl0sWzAuMjQzNCwtMy4xMDQ4
XSxbMS44NjI3LC0yLjIzNjJdLFswLjM2NjIsLTMuNzA1MV0sWy0wLjgwNTMsLTMu
MjUxNl0sWzAuMDkyMyw0LjMzOTVdLFstMC40OTgsMy40MjQ1XSxbLTAuMzQ0OSwt
My4wNTU3XSxbLTAuOTYxNCwtMy4zNzY4XSxbNC43NzExLDAuNDk2NF0sWy0wLjA2
ODgsLTIuOTQyNF0sWzEuNDc1MiwtMy4yMTQ2XSxbLTAuMjgyNiwtNC4zMjU2XSxb
Mi42NTM5LDMuNjk4Nl0sWy0xLjQ5NDksMy4yNDI5XSxbLTAuODg3MywtMy45NDI3
XSxbMC4wMTQyLC0zLjQxOTldLFsyLjEyMDUsLTIuODk1M10sWy0wLjcwNDcsLTMu
MDkyMV0sWy0wLjE4MTQsLTMuMjQwNV0sWy01LjM5MjYsLTAuMDU3MV0sWy00Ljc5
MywxLjczNjRdLFswLjA5ODYsLTQuMjgyM10sWzAuNzIzNywtMi4xNzhdLFstMC40
MzMxLC00LjA0NF0sWzAuNTE2NCwzLjQzNjldLFszLjQwNjYsMi4zMTIzXSxbLTAu
MzYwMSwtMC45MTk3XSxbLTQuMTA2NSwyLjYxOF0sWzAuOTc2OSw0LjI1MzVdLFst
Mi40Nzk5LDMuNzddLFstMy4yOTI1LDMuMzA3Ml0sWy00LjA2NzIsMS43NTg0XSxb
MC4yMzY4LC0zLjQ3OTRdLFstMC43ODIyLC00LjA5NjVdLFstMC42MjEsLTMuODUw
N10sWy0wLjA5MjQsLTQuMzM5NV0sWy0wLjYzODIsLTQuMjE0XSxbMS41MzE2LDMu
MjI0Nl0sWzAuOTcyLC0yLjc0NzldLFswLjUwMTksLTMuNDk2OF0sWy01LjI3NTQs
MC41OTczXSxbLTAuOTYzMiwtMy43NzIxXSxbMS44NDM2LDQuMDM1M10sWzMuNDQw
OSwzLjE3NDldLFswLjU0MSwtMi42OTM3XSxbNS43MjU0LC0xLjY5MDNdLFstNS41
OTg0LC0wLjY5NDJdLFstMC4zNTY0LC0zLjUzNzddLFsyLjQ5OTYsMi44Nzc1XSxb
LTAuMTE2MywtNC4xMDA2XSxbLTEuNjQzNyw0LjEwMzldLFs1LjY2MjUsLTAuOTE3
XSxbLTAuMTk3MSwtMy44MDMyXSxbLTUuNjk1NSwtMS4zNzVdLFstMy4zMzQsMi40
MTUxXSxbLTAuNTgyLC0yLjk3NDhdLFstMC4yNDk1LC0yLjg3NzhdLFs1LjY5Njcs
LTIuNTc2M10sWy00Ljc3NiwwLjk5ODNdLFstMi40NTE0LDIuODg4N11dLCJ2ZXJ0
ZXguY29sIjpbInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywx
MTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEz
NykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3
LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3
KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgx
MTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTks
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4
LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5
NjA3ODQzMTM3KSIsInJnYmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNyki
LCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4
LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5
MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcp
IiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDEx
NywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIs
MC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwx
MzgsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYw
Nzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwi
cmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIs
MTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkw
MTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3
KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3
LDE1OCwxMTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4
NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjMxLDQxLDEzOCwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwx
MTksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5
NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJy
Z2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4
LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDI3LDE1OCwxMTksMC43NDkw
MTk2MDc4NDMxMzcpIiwicmdiYSgyNywxNTgsMTE5LDAuNzQ5MDE5NjA3ODQzMTM3
KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIz
MSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcsOTUsMiwwLjc0
OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIsMTc5LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgy
MzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43
NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQz
MTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4NDMxMzcpIiwicmdi
YSgyMTcsOTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAu
NzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMTE3LDExMiwxNzksMC43NDkwMTk2MDc4
NDMxMzcpIiwicmdiYSgyMzEsNDEsMTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJn
YmEoMjcsMTU4LDExOSwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDExNywxMTIs
MTc5LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjcsMTU4LDExOSwwLjc0OTAx
OTYwNzg0MzEzNykiLCJyZ2JhKDIxNyw5NSwyLDAuNzQ5MDE5NjA3ODQzMTM3KSIs
InJnYmEoMjE3LDk1LDIsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMzEsNDEs
MTM4LDAuNzQ5MDE5NjA3ODQzMTM3KSIsInJnYmEoMjE3LDk1LDIsMC43NDkwMTk2
MDc4NDMxMzcpIiwicmdiYSgxMTcsMTEyLDE3OSwwLjc0OTAxOTYwNzg0MzEzNyki
LCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkwMTk2MDc4NDMxMzcpIiwicmdiYSgyMTcs
OTUsMiwwLjc0OTAxOTYwNzg0MzEzNykiLCJyZ2JhKDIzMSw0MSwxMzgsMC43NDkw
MTk2MDc4NDMxMzcpIl0sImJnIjpbInJnYmEoMjU1LDI1NSwyNTUsMSkiXSwibGFi
ZWwiOlsiMTExIFJhbmNoIiwiQWRvYmUgSGlsbCIsIkFydGlmYWN0IEhpbGwiLCJB
c2ggVGVycmFjZSIsIkFaIEJCOjM6MjIiLCJBWiBDQzoxOjMiLCJBWiBDQzoyOjE4
NSIsIkFaIENDOjI6MzMoQkxNKSIsIkJhamFkYSBTaXRlIiwiQmF5bGVzcyBSdWlu
IiwiQmlnIEJlbGwiLCJCcmFkeSBXYXNoIiwiQnV6YW4iLCJDYWN0dXMgRm9yZXN0
IiwiQ2FzYSBCdWVuYSIsIkNhc2EgR3JhbmRlIiwiQ2x1ZmYgUmFuY2giLCJDb250
aW5lbnRhbCBTaXRlIiwiQ3Jlc2NlbnQgU2l0ZSIsIkNyaXNtb24iLCJDdXJ0aXMi
LCJDdXJ0aXMgUnVpbihCdWVuYSBWaXN0YSkiLCJEYXZpcyBSYW5jaCBTaXRlIiwi
RHVkbGV5dmlsbGUgTW91bmQiLCJFYXJ2ZW4gRmxhdCBTaXRlIiwiRWwgUG9sdm9y
b24iLCJFbGxpb3R0IFNpdGUiLCJFc2NhbGFudGUiLCJGaXNjaGVyIHNpdGUiLCJG
bGllZ2VyIiwiRm9ydCBHcmFudCBQdWVibG8iLCJHZXJtYW5uIFNpdGUiLCJHaWJi
b24gIFNwcmluZ3MiLCJHb2F0IEhpbGwiLCJHcmFuZCBDYW5hbCIsIkhhYnkgUmFu
Y2giLCJIaWdoIE1lc2EiLCJKb3NlIFNvbGFzIFJ1aW4iLCJKdW5reWFyZCBTaXRl
IiwiTGFzIENhbm9wYXMiLCJMYXMgQ29saW5hcyIsIkxhcyBGb3NhcyIsIkxlYXZl
cnRvbiIsIkxvcyBHdWFuYWNvcyIsIkxvcyBNb3J0ZXJvcyIsIkxvcyBNdWVydG9z
IiwiTG9zdCBNb3VuZCIsIk1hcmlqaWxkYSBSdWluIiwiTWVzYSBHcmFuZGUiLCJN
ZXRob2Rpc3QgQ2h1cmNoIiwiTXVycGh5IFNpdGUtLVNhZmZvcmQgRWFzdCIsIk93
ZW5zLUNvbHZpbiIsIlBpcGVyIFNwcmluZ3MiLCJQdWVibG8gQmxhbmNvIiwiUHVl
YmxvIGRlbCBNb250ZSIsIlB1ZWJsbyBHcmFuZGUiLCJQdWVibG8gU2FsYWRvIiwi
UmFiaWQgUnVpbiIsIlJhdHRsZXNuYWtlIE1lc2EiLCJSZWV2ZSBSdWluIiwiUmlj
aGFyZHNvbiBPcmNoYXJkIiwiUmlsbGl0byBGYW4iLCJSaW5jb24gQ2FueW9uIiwi
U2FuIFhhdmllciBCcmlkZ2UiLCJTZWNvbmQgQ2FueW9uIENvbXBvdW5kIiwiU2hh
cm9uIFNpdGUiLCJTcGVhciBSYW5jaCIsIlN3aW5nbGUncyBTYW1wbGUiLCJUYW5x
dWUgVmVyZGUgVmlsbGFnZSIsIlRyZXMgUHVlYmxvcyIsIlR3aW4gSGF3a3MiLCJW
ZXJlcyIsIldlYmIgUnVpbiIsIldlcyBKZXJuaWdhbiBTaXRlIiwiV2hpcHRhaWwg
UnVpbiIsIldoaXRtZXIiLCJXcmlnaHQiLCJZdW1hIFdhc2gtLVR1Y3NvbiIsIll1
bWEgV2FzaCBTaXRlIiwiWmFuYXJkZWxsaSBTaXRlIl0sInhsYWIiOlsidD00LTUi
XSwiaml0dGVyIjpbZmFsc2VdLCJ1c2VhcnJvd3MiOltmYWxzZV0sInhsaW0iOlst
NS43OTUyLDUuNzk1Ml0sInlsaW0iOlstNS4zMzc2LDUuMzM3Nl19fV0sIm5ldHdv
cmsiOnsibWVsIjpbeyJpbmwiOlsxNV0sIm91dGwiOlsxMl0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl0sWzMsNV1dfX0seyJpbmwiOlsxNl0sIm91
dGwiOlsxMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl0sWzMs
NV1dfX0seyJpbmwiOlsyMF0sIm91dGwiOlsxMl0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzAsMl0sWzMsNV1dfX0seyJpbmwiOlsyOF0sIm91dGwiOlsx
Ml0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwi
OlszMl0sIm91dGwiOlsxMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzAsMl0sWzMsNV1dfX0seyJpbmwiOlszNV0sIm91dGwiOlsxMl0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl0sWzMsNV1dfX0seyJpbmwiOlszOV0s
Im91dGwiOlsxMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV0s
WzMsNV1dfX0seyJpbmwiOls0MF0sIm91dGwiOlsxMl0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls1Nl0sIm91dGwiOlsxMl0s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl0sWzMsNV1dfX0seyJp
bmwiOls3Ml0sIm91dGwiOlsxMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzAsMl1dfX0seyJpbmwiOlsxNl0sIm91dGwiOlsxNV0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlsyMF0sIm91dGwiOlsx
NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwi
OlsyOF0sIm91dGwiOlsxNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzAsNV1dfX0seyJpbmwiOlszMl0sIm91dGwiOlsxNV0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlszNV0sIm91dGwiOlsxNV0s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlsz
OV0sIm91dGwiOlsxNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAs
MV0sWzMsNV1dfX0seyJpbmwiOls0MF0sIm91dGwiOlsxNV0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls1Nl0sIm91dGwiOlsx
NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwi
Ols3Ml0sIm91dGwiOlsxNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzAsM11dfX0seyJpbmwiOlsyMF0sIm91dGwiOlsxNl0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlsyOF0sIm91dGwiOlsxNl0s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl0sWzMsNV1dfX0seyJp
bmwiOlszMl0sIm91dGwiOlsxNl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzAsNV1dfX0seyJpbmwiOlszNV0sIm91dGwiOlsxNl0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlszOV0sIm91dGwiOlsx
Nl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV0sWzMsNV1dfX0s
eyJpbmwiOls0MF0sIm91dGwiOlsxNl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzAsMl1dfX0seyJpbmwiOls1Nl0sIm91dGwiOlsxNl0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOls3Ml0sIm91dGwi
OlsxNl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJp
bmwiOlszM10sIm91dGwiOlsxOF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzAsMl1dfX0seyJpbmwiOls0NV0sIm91dGwiOlsxOF0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls1OF0sIm91dGwiOlsx
OF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwi
Ols2Ml0sIm91dGwiOlsxOF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzAsMl1dfX0seyJpbmwiOls2NF0sIm91dGwiOlsxOF0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls2OV0sIm91dGwiOlsxOF0s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls3
NV0sIm91dGwiOlsxOF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAs
M11dfX0seyJpbmwiOls3OF0sIm91dGwiOlsxOF0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls4MF0sIm91dGwiOlsxOF0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOlsyOF0s
Im91dGwiOlsyMF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1d
fX0seyJpbmwiOlszMl0sIm91dGwiOlsyMF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlszNV0sIm91dGwiOlsyMF0sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlszOV0sIm91
dGwiOlsyMF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV0sWzMs
NV1dfX0seyJpbmwiOls0MF0sIm91dGwiOlsyMF0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls1Nl0sIm91dGwiOlsyMF0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOls3Ml0s
Im91dGwiOlsyMF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11d
fX0seyJpbmwiOlszMl0sIm91dGwiOlsyOF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlszNV0sIm91dGwiOlsyOF0sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlszOV0sIm91
dGwiOlsyOF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV0sWzMs
NV1dfX0seyJpbmwiOls0MF0sIm91dGwiOlsyOF0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls1Nl0sIm91dGwiOlsyOF0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOls3Ml0s
Im91dGwiOlsyOF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1d
fX0seyJpbmwiOlszNV0sIm91dGwiOlszMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOlszOV0sIm91dGwiOlszMl0sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV0sWzMsNV1dfX0seyJpbmwiOls0
MF0sIm91dGwiOlszMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAs
Ml1dfX0seyJpbmwiOls1Nl0sIm91dGwiOlszMl0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzAsNV1dfX0seyJpbmwiOls3Ml0sIm91dGwiOlszMl0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls0NV0s
Im91dGwiOlszM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1d
fX0seyJpbmwiOls1OF0sIm91dGwiOlszM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls2Ml0sIm91dGwiOlszM10sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls2NF0sIm91
dGwiOlszM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0s
eyJpbmwiOls2OV0sIm91dGwiOlszM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzAsMl1dfX0seyJpbmwiOls3NV0sIm91dGwiOlszM10sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls3OF0sIm91dGwi
OlszM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJp
bmwiOls4MF0sIm91dGwiOlszM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzAsMl1dfX0seyJpbmwiOlszOV0sIm91dGwiOlszNV0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV0sWzMsNV1dfX0seyJpbmwiOls0MF0sIm91
dGwiOlszNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0s
eyJpbmwiOls1Nl0sIm91dGwiOlszNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzAsNV1dfX0seyJpbmwiOls3Ml0sIm91dGwiOlszNV0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls0MF0sIm91dGwi
OlszOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV1dfX0seyJp
bmwiOls1Nl0sIm91dGwiOlszOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzAsMV0sWzMsNV1dfX0seyJpbmwiOls3Ml0sIm91dGwiOlszOV0sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMV1dfX0seyJpbmwiOls1Nl0sIm91
dGwiOls0MF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0s
eyJpbmwiOls3Ml0sIm91dGwiOls0MF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzAsMl1dfX0seyJpbmwiOls1OF0sIm91dGwiOls0NV0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls2Ml0sIm91dGwi
Ols0NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJp
bmwiOls2NF0sIm91dGwiOls0NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzAsM11dfX0seyJpbmwiOls2OV0sIm91dGwiOls0NV0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls3NV0sIm91dGwiOls0
NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwi
Ols3OF0sIm91dGwiOls0NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzAsM11dfX0seyJpbmwiOls4MF0sIm91dGwiOls0NV0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls3Ml0sIm91dGwiOls1Nl0s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls2
Ml0sIm91dGwiOls1OF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAs
Ml1dfX0seyJpbmwiOls2NF0sIm91dGwiOls1OF0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls2OV0sIm91dGwiOls1OF0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls3NV0s
Im91dGwiOls1OF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1d
fX0seyJpbmwiOls3OF0sIm91dGwiOls1OF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls4MF0sIm91dGwiOls1OF0sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls2NF0sIm91
dGwiOls2Ml0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0s
eyJpbmwiOls2OV0sIm91dGwiOls2Ml0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzAsMl1dfX0seyJpbmwiOls3NV0sIm91dGwiOls2Ml0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJpbmwiOls3OF0sIm91dGwi
Ols2Ml0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsMl1dfX0seyJp
bmwiOls4MF0sIm91dGwiOls2Ml0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzAsMl1dfX0seyJpbmwiOls2OV0sIm91dGwiOls2NF0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls3NV0sIm91dGwiOls2
NF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwi
Ols3OF0sIm91dGwiOls2NF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzAsM11dfX0seyJpbmwiOls4MF0sIm91dGwiOls2NF0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls3NV0sIm91dGwiOls2OV0s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls3
OF0sIm91dGwiOls2OV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAs
M11dfX0seyJpbmwiOls4MF0sIm91dGwiOls2OV0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls3OF0sIm91dGwiOls3NV0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOls4MF0s
Im91dGwiOls3NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzAsM11d
fX0seyJpbmwiOls4MF0sIm91dGwiOls3OF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzAsM11dfX0seyJpbmwiOlsxOV0sIm91dGwiOlszXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMSw1XV19fSx7ImlubCI6WzMwXSwib3V0
bCI6WzRdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1sxLDVdXX19LHsi
aW5sIjpbMjldLCJvdXRsIjpbMTddLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2
ZSI6W1sxLDJdXX19LHsiaW5sIjpbMjddLCJvdXRsIjpbMjRdLCJhdGwiOnsibmEi
OltmYWxzZV0sImFjdGl2ZSI6W1sxLDVdXX19LHsiaW5sIjpbNjhdLCJvdXRsIjpb
MjRdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1sxLDVdXX19LHsiaW5s
IjpbNjhdLCJvdXRsIjpbMjddLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6
W1sxLDVdXX19LHsiaW5sIjpbNjddLCJvdXRsIjpbMzRdLCJhdGwiOnsibmEiOltm
YWxzZV0sImFjdGl2ZSI6W1sxLDNdXX19LHsiaW5sIjpbNzBdLCJvdXRsIjpbMzld
LCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1sxLDJdLFszLDVdXX19LHsi
aW5sIjpbOV0sIm91dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMiw1XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltm
YWxzZV0sImFjdGl2ZSI6W1syLDVdXX19LHsiaW5sIjpbMjNdLCJvdXRsIjpbM10s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzIsNV1dfX0seyJpbmwiOlsy
NF0sIm91dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1
XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltmYWxzZV0s
ImFjdGl2ZSI6W1syLDVdXX19LHsiaW5sIjpbNDNdLCJvdXRsIjpbM10sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzIsNV1dfX0seyJpbmwiOls0OF0sIm91
dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7
ImlubCI6WzUzXSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2
ZSI6W1syLDVdXX19LHsiaW5sIjpbNTldLCJvdXRsIjpbM10sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzIsM10sWzQsNV1dfX0seyJpbmwiOls2MF0sIm91
dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7
ImlubCI6WzY4XSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2
ZSI6W1syLDVdXX19LHsiaW5sIjpbNzddLCJvdXRsIjpbM10sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzIsNV1dfX0seyJpbmwiOls0M10sIm91dGwiOls0
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6
WzE5XSwib3V0bCI6WzVdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1sy
LDVdXX19LHsiaW5sIjpbMzddLCJvdXRsIjpbNV0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzIsM11dfX0seyJpbmwiOls0OF0sIm91dGwiOls1XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU5XSwi
b3V0bCI6WzVdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1syLDVdXX19
LHsiaW5sIjpbMjldLCJvdXRsIjpbN10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzIsNV1dfX0seyJpbmwiOlsyMV0sIm91dGwiOls5XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzIzXSwib3V0bCI6
WzldLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1syLDVdXX19LHsiaW5s
IjpbMjRdLCJvdXRsIjpbOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzIsNV1dfX0seyJpbmwiOlsyN10sIm91dGwiOls5XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzldLCJh
dGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1syLDVdXX19LHsiaW5sIjpbNjBd
LCJvdXRsIjpbOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzIsNV1d
fX0seyJpbmwiOls2OF0sIm91dGwiOls5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzExXSwib3V0bCI6WzEwXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU4XSwib3V0
bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7
ImlubCI6WzY1XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMiw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6
WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7Imlu
bCI6WzQyXSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMiw1XV19fSx7ImlubCI6WzQ2XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzE1
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6
WzcwXSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Miw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzQ2XSwib3V0bCI6WzE2XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU1
XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1
XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzQ4XSwib3V0bCI6WzE5XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU5XSwi
b3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXSxb
NCw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzIwXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzQ2
XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwz
XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzIwXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzIzXSwi
b3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19
fSx7ImlubCI6WzI0XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzIxXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzMwXSwib3V0
bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7
ImlubCI6WzQzXSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMiw1XV19fSx7ImlubCI6WzQ4XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzUzXSwib3V0bCI6
WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7Imlu
bCI6WzU5XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMiw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzIx
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6
Wzc3XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Miw1XV19fSx7ImlubCI6WzI0XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzIzXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzUz
XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1
XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzIzXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6Wzc3XSwi
b3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19
fSx7ImlubCI6WzUzXSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzI3XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6Wzc3XSwib3V0
bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7
ImlubCI6WzQyXSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMiw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6
WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7Imlu
bCI6WzQzXSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMiw1XV19fSx7ImlubCI6WzQ4XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzMw
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6
Wzc3XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Miw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzQ2XSwib3V0bCI6WzMyXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU1
XSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1
XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzM1XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzQ2XSwi
b3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19
fSx7ImlubCI6WzU1XSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzM1XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzU4XSwib3V0
bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7
ImlubCI6WzU0XSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMiw1XV19fSx7ImlubCI6WzQ2XSwib3V0bCI6WzQyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU1XSwib3V0bCI6
WzQyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7Imlu
bCI6WzU2XSwib3V0bCI6WzQyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMiw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzQyXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzcyXSwib3V0bCI6WzQy
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6
WzQ4XSwib3V0bCI6WzQzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
MiwzXV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzQzXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzQ2XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU2
XSwib3V0bCI6WzQ2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwz
XV19fSx7ImlubCI6WzcyXSwib3V0bCI6WzQ2XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzU5XSwib3V0bCI6WzQ4XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzYwXSwi
b3V0bCI6WzQ4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19
fSx7ImlubCI6WzY4XSwib3V0bCI6WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMiw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzUzXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6WzU2XSwib3V0
bCI6WzU1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7
ImlubCI6WzcwXSwib3V0bCI6WzU1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMiw1XV19fSx7ImlubCI6WzcyXSwib3V0bCI6WzU1XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMiwzXV19fSx7ImlubCI6WzcwXSwib3V0bCI6
WzU2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7Imlu
bCI6WzY1XSwib3V0bCI6WzU4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMiwzXV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzU5XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzYw
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMiw1XV19fSx7ImlubCI6
Wzc3XSwib3V0bCI6WzY4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Miw1XV19fSx7ImlubCI6WzddLCJvdXRsIjpbMV0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlsyOV0sIm91dGwiOlsxXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzNdLCJv
dXRsIjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0s
eyJpbmwiOls5XSwib3V0bCI6WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2
ZSI6W1szLDVdXX19LHsiaW5sIjpbMTBdLCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlsxMl0sIm91dGwiOlsy
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzE0XSwib3V0bCI6WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1sz
LDVdXX19LHsiaW5sIjpbMTVdLCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlsxNl0sIm91dGwiOlsyXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE5XSwi
b3V0bCI6WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19
LHsiaW5sIjpbMjBdLCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzMsNV1dfX0seyJpbmwiOlsyMV0sIm91dGwiOlsyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIzXSwib3V0bCI6
WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5s
IjpbMjRdLCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzMsNV1dfX0seyJpbmwiOlsyN10sIm91dGwiOlsyXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI4XSwib3V0bCI6WzJdLCJh
dGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzBd
LCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1d
fX0seyJpbmwiOlszMl0sIm91dGwiOlsyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM1XSwib3V0bCI6WzJdLCJhdGwiOnsi
bmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzZdLCJvdXRs
IjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJp
bmwiOlszOF0sIm91dGwiOlsyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzM5XSwib3V0bCI6WzJdLCJhdGwiOnsibmEiOltm
YWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbNDJdLCJvdXRsIjpbMl0s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls0
NF0sIm91dGwiOlsyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzJdLCJhdGwiOnsibmEiOltmYWxzZV0s
ImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbNTRdLCJvdXRsIjpbMl0sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls1NV0sIm91
dGwiOlsyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzU2XSwib3V0bCI6WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2
ZSI6W1szLDVdXX19LHsiaW5sIjpbNTddLCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls2MF0sIm91dGwiOlsy
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzYyXSwib3V0bCI6WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1sz
LDVdXX19LHsiaW5sIjpbNjVdLCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzMsNF1dfX0seyJpbmwiOls2OF0sIm91dGwiOlsyXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwi
b3V0bCI6WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19
LHsiaW5sIjpbNzNdLCJvdXRsIjpbMl0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzMsNV1dfX0seyJpbmwiOls3Nl0sIm91dGwiOlsyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6
WzJdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5s
IjpbNF0sIm91dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzVdLCJvdXRsIjpbM10sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlsxMF0sIm91dGwiOlszXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzEyXSwi
b3V0bCI6WzNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19
LHsiaW5sIjpbMTRdLCJvdXRsIjpbM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0
aXZlIjpbWzMsNV1dfX0seyJpbmwiOlsxNV0sIm91dGwiOlszXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE2XSwib3V0bCI6
WzNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5s
IjpbMjBdLCJvdXRsIjpbM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpb
WzMsNV1dfX0seyJpbmwiOlsyOF0sIm91dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzNdLCJh
dGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzJd
LCJvdXRsIjpbM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1d
fX0seyJpbmwiOlszNV0sIm91dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzNdLCJhdGwiOnsi
bmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzhdLCJvdXRs
IjpbM10sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJp
bmwiOlszOV0sIm91dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltm
YWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbNDRdLCJvdXRsIjpbM10s
ImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls1
NF0sIm91dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltmYWxzZV0s
ImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbNTZdLCJvdXRsIjpbM10sImF0bCI6
eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls1N10sIm91
dGwiOlszXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzYyXSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2
ZSI6W1szLDVdXX19LHsiaW5sIjpbNjVdLCJvdXRsIjpbM10sImF0bCI6eyJuYSI6
W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls3MF0sIm91dGwiOlsz
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzczXSwib3V0bCI6WzNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1sz
LDVdXX19LHsiaW5sIjpbNzZdLCJvdXRsIjpbM10sImF0bCI6eyJuYSI6W2ZhbHNl
XSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls1XSwib3V0bCI6WzRdLCJhdGwi
OnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMTBdLCJv
dXRsIjpbNF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0s
eyJpbmwiOlsxMV0sIm91dGwiOls0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzEyXSwib3V0bCI6WzRdLCJhdGwiOnsibmEi
OltmYWxzZV0sImFjdGl2ZSI6W1szLDRdXX19LHsiaW5sIjpbMTZdLCJvdXRsIjpb
NF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNF1dfX0seyJpbmwi
OlsxOV0sIm91dGwiOls0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw0XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzRdLCJhdGwiOnsibmEiOltmYWxz
ZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMjNdLCJvdXRsIjpbNF0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNF1dfX0seyJpbmwiOlsyNF0s
Im91dGwiOls0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19
fSx7ImlubCI6WzI3XSwib3V0bCI6WzRdLCJhdGwiOnsibmEiOltmYWxzZV0sImFj
dGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzZdLCJvdXRsIjpbNF0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlszOF0sIm91dGwi
Ols0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7Imlu
bCI6WzM5XSwib3V0bCI6WzRdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6
W1szLDVdXX19LHsiaW5sIjpbNTNdLCJvdXRsIjpbNF0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls2MF0sIm91dGwiOls0XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY1
XSwib3V0bCI6WzRdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVd
XX19LHsiaW5sIjpbNjhdLCJvdXRsIjpbNF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzMsNF1dfX0seyJpbmwiOls3M10sIm91dGwiOls0XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6Wzc2XSwib3V0
bCI6WzRdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsi
aW5sIjpbNzddLCJvdXRsIjpbNF0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzMsNV1dfX0seyJpbmwiOlsxMF0sIm91dGwiOls1XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzExXSwib3V0bCI6WzVd
LCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpb
MTJdLCJvdXRsIjpbNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMs
NV1dfX0seyJpbmwiOlsxNl0sIm91dGwiOls1XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzVdLCJhdGwi
OnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMjNdLCJv
dXRsIjpbNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0s
eyJpbmwiOlsyNF0sIm91dGwiOls1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw0XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzVdLCJhdGwiOnsibmEi
OltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzBdLCJvdXRsIjpb
NV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwi
OlszMV0sIm91dGwiOls1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzVdLCJhdGwiOnsibmEiOltmYWxz
ZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzhdLCJvdXRsIjpbNV0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlszOV0s
Im91dGwiOls1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzQzXSwib3V0bCI6WzVdLCJhdGwiOnsibmEiOltmYWxzZV0sImFj
dGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbNTNdLCJvdXRsIjpbNV0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls2MF0sIm91dGwi
Ols1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzY1XSwib3V0bCI6WzVdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6
W1szLDVdXX19LHsiaW5sIjpbNjhdLCJvdXRsIjpbNV0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls3M10sIm91dGwiOls1XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6Wzc2
XSwib3V0bCI6WzVdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVd
XX19LHsiaW5sIjpbNzddLCJvdXRsIjpbNV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlszN10sIm91dGwiOls2XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6Wzc0XSwib3V0
bCI6WzhdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsi
aW5sIjpbMTBdLCJvdXRsIjpbOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzMsNV1dfX0seyJpbmwiOlsxMl0sIm91dGwiOls5XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE0XSwib3V0bCI6Wzld
LCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpb
MTVdLCJvdXRsIjpbOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMs
NV1dfX0seyJpbmwiOlsxNl0sIm91dGwiOls5XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE5XSwib3V0bCI6WzldLCJhdGwi
OnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMjBdLCJv
dXRsIjpbOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0s
eyJpbmwiOlsyOF0sIm91dGwiOls5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzldLCJhdGwiOnsibmEi
OltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzJdLCJvdXRsIjpb
OV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwi
OlszNV0sIm91dGwiOls5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzldLCJhdGwiOnsibmEiOltmYWxz
ZV0sImFjdGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbMzhdLCJvdXRsIjpbOV0sImF0
bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOlszOV0s
Im91dGwiOls5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzQyXSwib3V0bCI6WzldLCJhdGwiOnsibmEiOltmYWxzZV0sImFj
dGl2ZSI6W1szLDVdXX19LHsiaW5sIjpbNDRdLCJvdXRsIjpbOV0sImF0bCI6eyJu
YSI6W2ZhbHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls1NF0sIm91dGwi
Ols5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzU1XSwib3V0bCI6WzldLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6
W1szLDVdXX19LHsiaW5sIjpbNTZdLCJvdXRsIjpbOV0sImF0bCI6eyJuYSI6W2Zh
bHNlXSwiYWN0aXZlIjpbWzMsNV1dfX0seyJpbmwiOls1N10sIm91dGwiOls5XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYy
XSwib3V0bCI6WzldLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVd
XX19LHsiaW5sIjpbNjVdLCJvdXRsIjpbOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwi
YWN0aXZlIjpbWzMsNF1dfX0seyJpbmwiOls3MF0sIm91dGwiOls5XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0
bCI6WzldLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1szLDVdXX19LHsi
aW5sIjpbNzZdLCJvdXRsIjpbOV0sImF0bCI6eyJuYSI6W2ZhbHNlXSwiYWN0aXZl
IjpbWzMsNV1dfX0seyJpbmwiOls3N10sIm91dGwiOls5XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzEyXSwib3V0bCI6WzEw
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzE0XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzE1XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE2XSwib3V0bCI6WzEwXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE5
XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzIwXSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzEwXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIzXSwi
b3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzI0XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzEwXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI4XSwib3V0
bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzMwXSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzMyXSwib3V0bCI6WzEwXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM1XSwib3V0bCI6
WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzM2XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM5XSwib3V0bCI6WzEw
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzQyXSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzQzXSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzEwXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUz
XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzEwXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2XSwi
b3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzU3XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU5XSwib3V0bCI6WzEwXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0
bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzYyXSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6
WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzczXSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzEwXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzEw
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzIxXSwib3V0bCI6WzExXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzExXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQzXSwib3V0bCI6WzExXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ3
XSwib3V0bCI6WzExXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU5XSwib3V0bCI6WzExXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzExXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwi
b3V0bCI6WzExXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzE0XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE5XSwib3V0bCI6WzEyXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIxXSwib3V0
bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzIzXSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzI0XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI3XSwib3V0bCI6
WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzMwXSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzEy
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzQyXSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzEyXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU0
XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzEyXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwi
b3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzY1XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzEyXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0
bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6Wzc2XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzEyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzE1XSwib3V0bCI6
WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzE2XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzE5XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIwXSwib3V0bCI6WzE0
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzIxXSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzIzXSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI0XSwib3V0bCI6WzE0XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI3
XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzI4XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzE0XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMyXSwi
b3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzM1XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzE0XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM4XSwib3V0
bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzM5XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzE0XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6
WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzUzXSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzE0
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU2XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzE0XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYy
XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzE0XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwi
b3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzczXSwib3V0bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzE0XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0
bCI6WzE0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzE5XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzE1XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIzXSwib3V0bCI6
WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzI0XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzE1
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzM2XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzE1XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUz
XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzE1XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwi
b3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzYyXSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzE1XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0
bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzczXSwib3V0bCI6WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzE1XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6
WzE1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzE5XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIzXSwib3V0bCI6WzE2
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzI0XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzE2XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2
XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzE2XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUzXSwi
b3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzU0XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzE2XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0
bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzYyXSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY4XSwib3V0bCI6
WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzcwXSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzE2
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
Wzc3XSwib3V0bCI6WzE2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzIwXSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIzXSwib3V0bCI6WzE5XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI0
XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI4XSwib3V0bCI6WzE5XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwi
b3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzMyXSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM1XSwib3V0bCI6WzE5XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0
bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzM4XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzM5XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6
WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzQzXSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw0XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzE5
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU0XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2XSwib3V0bCI6WzE5XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3
XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzE5XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY4XSwi
b3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzcwXSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzE5XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0
bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6Wzc3XSwib3V0bCI6WzE5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzIxXSwib3V0bCI6WzIwXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzIzXSwib3V0bCI6
WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzI0XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzI3XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzIw
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzM2XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzIwXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUz
XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzIwXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwi
b3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzYyXSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzIwXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0
bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzczXSwib3V0bCI6WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzIwXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6
WzIwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzI4XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzMxXSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMyXSwib3V0bCI6WzIx
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzM1XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzIxXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM5
XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzIxXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU0XSwi
b3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzU1XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2XSwib3V0bCI6WzIxXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0
bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzYyXSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6
WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzczXSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzIxXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMxXSwib3V0bCI6WzIy
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzM3XSwib3V0bCI6WzIyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU5XSwib3V0bCI6WzIyXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI4XSwib3V0bCI6WzIzXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMw
XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzMyXSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM1XSwib3V0bCI6WzIzXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwi
b3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzM4XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM5XSwib3V0bCI6WzIzXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0
bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzQ0XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6
WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzU2XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzIz
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzY1XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzIzXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2
XSwib3V0bCI6WzIzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzI4XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzI0XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMyXSwi
b3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzM1XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzI0XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM4XSwib3V0
bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzM5XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzI0XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6
WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzU0XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2XSwib3V0bCI6WzI0
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU3XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzI0XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1
XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzI0XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwi
b3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6Wzc3XSwib3V0bCI6WzI0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzI4XSwib3V0bCI6WzI3XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0
bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzMyXSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzM1XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6
WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzM4XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzM5XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzI3
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzQ0XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzI3XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2
XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzI3XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwi
b3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzY1XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzI3XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0
bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6Wzc2XSwib3V0bCI6WzI3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzMwXSwib3V0bCI6WzI4XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6
WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzM4XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzI4
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU0XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzI4XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYy
XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzI4XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwi
b3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6Wzc2XSwib3V0bCI6WzI4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzI4XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzMyXSwib3V0
bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzM1XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzM2XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6
WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzM5XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzMw
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzUzXSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzMwXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2
XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU5XSwib3V0bCI6WzMwXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwi
b3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzY1XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzMwXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0
bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzczXSwib3V0bCI6WzMwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzMwXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM3XSwib3V0bCI6
WzMxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzU5XSwib3V0bCI6WzMxXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzMxXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzMx
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzM2XSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzMyXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUz
XSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzMyXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwi
b3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzYyXSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzMyXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0
bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzczXSwib3V0bCI6WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzMyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6
WzMyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzM2XSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzM4XSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzM1
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzUzXSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzM1XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYw
XSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzM1XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwi
b3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzczXSwib3V0bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzM1XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0
bCI6WzM1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzM4XSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzM5XSwib3V0bCI6WzM2XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6
WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzQ0XSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU0XSwib3V0bCI6WzM2
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU1XSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU2XSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzM2XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYw
XSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzM2XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY4XSwi
b3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzcwXSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzM2XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0
bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6Wzc3XSwib3V0bCI6WzM2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzU5XSwib3V0bCI6WzM3XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzM5XSwib3V0bCI6
WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzQyXSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzM4
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU0XSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2XSwib3V0bCI6WzM4XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3
XSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzM4XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwi
b3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzY4XSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzM4XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0
bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6Wzc2XSwib3V0bCI6WzM4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzM4XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQyXSwib3V0bCI6
WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzQ0XSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzUzXSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzM5
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU3XSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzM5XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1
XSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzM5XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwi
b3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6Wzc3XSwib3V0bCI6WzM5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzQ0XSwib3V0bCI6WzQyXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUzXSwib3V0
bCI6WzQyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzU0XSwib3V0bCI6WzQyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzQyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6
WzQyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzYyXSwib3V0bCI6WzQyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzQyXSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzQy
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzczXSwib3V0bCI6WzQyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzQyXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzQyXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU5
XSwib3V0bCI6WzQzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzQzXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzQzXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzUzXSwi
b3V0bCI6WzQ0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzU0XSwib3V0bCI6WzQ0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzQ0XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2XSwib3V0
bCI6WzQ0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzU3XSwib3V0bCI6WzQ0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzQ0XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6
WzQ0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzY1XSwib3V0bCI6WzQ0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzQ0XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzQ0
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzczXSwib3V0bCI6WzQ0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzQ0XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzQ0XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU0
XSwib3V0bCI6WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzU1XSwib3V0bCI6WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU2XSwib3V0bCI6WzUzXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwi
b3V0bCI6WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzYwXSwib3V0bCI6WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzUzXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0
bCI6WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzcwXSwib3V0bCI6WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzUzXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6
WzUzXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzU1XSwib3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzU2XSwib3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzU0
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzYwXSwib3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzU0XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4
XSwib3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzU0XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwi
b3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6Wzc3XSwib3V0bCI6WzU0XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzU3XSwib3V0bCI6WzU1XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0
bCI6WzU1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzYyXSwib3V0bCI6WzU1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzU1XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0bCI6
WzU1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzczXSwib3V0bCI6WzU1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzU1XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzU1
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzU3XSwib3V0bCI6WzU2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzU2XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzU2XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1
XSwib3V0bCI6WzU2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw0
XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzU2XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzU2XSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwi
b3V0bCI6WzU2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6Wzc3XSwib3V0bCI6WzU2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYwXSwib3V0bCI6WzU3XSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzYyXSwib3V0
bCI6WzU3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzY1XSwib3V0bCI6WzU3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzU3XSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6
WzU3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzczXSwib3V0bCI6WzU3XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzU3XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzU3
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
WzY1XSwib3V0bCI6WzU5XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6WzYyXSwib3V0bCI6WzYwXSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY1XSwib3V0bCI6WzYwXSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzY4
XSwib3V0bCI6WzYwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzYwXSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzYwXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwi
b3V0bCI6WzYwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6WzY1XSwib3V0bCI6WzYyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzY4XSwib3V0bCI6WzYyXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0
bCI6WzYyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6WzczXSwib3V0bCI6WzYyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzYyXSwiYXRsIjp7Im5h
IjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6
WzYyXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7Imlu
bCI6WzY4XSwib3V0bCI6WzY1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUi
OltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzY1XSwiYXRsIjp7Im5hIjpb
ZmFsc2VdLCJhY3RpdmUiOltbMyw0XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzY1
XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6
Wzc2XSwib3V0bCI6WzY1XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltb
Myw1XV19fSx7ImlubCI6Wzc3XSwib3V0bCI6WzY1XSwiYXRsIjp7Im5hIjpbZmFs
c2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzcwXSwib3V0bCI6WzY4XSwi
YXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzcz
XSwib3V0bCI6WzY4XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1
XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzY4XSwiYXRsIjp7Im5hIjpbZmFsc2Vd
LCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6WzczXSwib3V0bCI6WzcwXSwiYXRs
Ijp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwi
b3V0bCI6WzcwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19
fSx7ImlubCI6Wzc3XSwib3V0bCI6WzcwXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJh
Y3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc2XSwib3V0bCI6WzczXSwiYXRsIjp7
Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7ImlubCI6Wzc3XSwib3V0
bCI6WzczXSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3RpdmUiOltbMyw1XV19fSx7
ImlubCI6Wzc3XSwib3V0bCI6Wzc2XSwiYXRsIjp7Im5hIjpbZmFsc2VdLCJhY3Rp
dmUiOltbMyw1XV19fSx7ImlubCI6WzU5XSwib3V0bCI6WzRdLCJhdGwiOnsibmEi
OltmYWxzZV0sImFjdGl2ZSI6W1s0LDVdXX19LHsiaW5sIjpbNTldLCJvdXRsIjpb
MjNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1s0LDVdXX19LHsiaW5s
IjpbNTldLCJvdXRsIjpbMjddLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6
W1s0LDVdXX19LHsiaW5sIjpbNTldLCJvdXRsIjpbMzZdLCJhdGwiOnsibmEiOltm
YWxzZV0sImFjdGl2ZSI6W1s0LDVdXX19LHsiaW5sIjpbNTldLCJvdXRsIjpbMzld
LCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1s0LDVdXX19LHsiaW5sIjpb
NTldLCJvdXRsIjpbNTNdLCJhdGwiOnsibmEiOltmYWxzZV0sImFjdGl2ZSI6W1s0
LDVdXX19LHsiaW5sIjpbNzddLCJvdXRsIjpbNTldLCJhdGwiOnsibmEiOltmYWxz
ZV0sImFjdGl2ZSI6W1s0LDVdXX19XSwiZ2FsIjp7Im4iOls4MF0sIm1uZXh0Ijpb
ODEyXSwiZGlyZWN0ZWQiOltmYWxzZV0sImh5cGVyIjpbZmFsc2VdLCJsb29wcyI6
W2ZhbHNlXSwibXVsdGlwbGUiOltmYWxzZV0sImJpcGFydGl0ZSI6W2ZhbHNlXSwi
bmV0Lm9icy5wZXJpb2QiOnsib2JzZXJ2YXRpb25zIjpbWzAsNV1dLCJtb2RlIjpb
ImRpc2NyZXRlIl0sInRpbWUuaW5jcmVtZW50IjpbMV0sInRpbWUudW5pdCI6WyJz
dGVwIl19LCJzbGljZS5wYXIiOnsic3RhcnQiOlswXSwiZW5kIjpbNV0sImludGVy
dmFsIjpbMV0sImFnZ3JlZ2F0ZS5kdXIiOlsxXSwicnVsZSI6WyJsYXRlc3QiXX19
LCJ2YWwiOlt7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiMTExIFJhbmNo
Il0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVz
IjpbIkFkb2JlIEhpbGwiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2Vd
LCJ2ZXJ0ZXgubmFtZXMiOlsiQXJ0aWZhY3QgSGlsbCJdLCJhY3RpdmUiOltbMCw1
XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJBc2ggVGVycmFjZSJd
LCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6
WyJBWiBCQjozOjIyIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwi
dmVydGV4Lm5hbWVzIjpbIkFaIENDOjE6MyJdLCJhY3RpdmUiOltbMCw1XV19LHsi
bmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJBWiBDQzoyOjE4NSJdLCJhY3Rp
dmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJBWiBD
QzoyOjMzKEJMTSkiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2
ZXJ0ZXgubmFtZXMiOlsiQmFqYWRhIFNpdGUiXSwiYWN0aXZlIjpbWzAsNV1dfSx7
Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiQmF5bGVzcyBSdWluIl0sImFj
dGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIkJp
ZyBCZWxsIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4
Lm5hbWVzIjpbIkJyYWR5IFdhc2giXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpb
ZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiQnV6YW4iXSwiYWN0aXZlIjpbWzAsNV1d
fSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiQ2FjdHVzIEZvcmVzdCJd
LCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6
WyJDYXNhIEJ1ZW5hIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwi
dmVydGV4Lm5hbWVzIjpbIkNhc2EgR3JhbmRlIl0sImFjdGl2ZSI6W1swLDVdXX0s
eyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIkNsdWZmIFJhbmNoIl0sImFj
dGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIkNv
bnRpbmVudGFsIFNpdGUiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2Vd
LCJ2ZXJ0ZXgubmFtZXMiOlsiQ3Jlc2NlbnQgU2l0ZSJdLCJhY3RpdmUiOltbMCw1
XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJDcmlzbW9uIl0sImFj
dGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIkN1
cnRpcyJdLCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5u
YW1lcyI6WyJDdXJ0aXMgUnVpbihCdWVuYSBWaXN0YSkiXSwiYWN0aXZlIjpbWzAs
NV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiRGF2aXMgUmFuY2gg
U2l0ZSJdLCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5u
YW1lcyI6WyJEdWRsZXl2aWxsZSBNb3VuZCJdLCJhY3RpdmUiOltbMCw1XV19LHsi
bmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJFYXJ2ZW4gRmxhdCBTaXRlIl0s
ImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpb
IkVsIFBvbHZvcm9uIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwi
dmVydGV4Lm5hbWVzIjpbIkVsbGlvdHQgU2l0ZSJdLCJhY3RpdmUiOltbMCw1XV19
LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJFc2NhbGFudGUiXSwiYWN0
aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiRmlz
Y2hlciBzaXRlIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVy
dGV4Lm5hbWVzIjpbIkZsaWVnZXIiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpb
ZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiRm9ydCBHcmFudCBQdWVibG8iXSwiYWN0
aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiR2Vy
bWFubiBTaXRlIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVy
dGV4Lm5hbWVzIjpbIkdpYmJvbiAgU3ByaW5ncyJdLCJhY3RpdmUiOltbMCw1XV19
LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJHb2F0IEhpbGwiXSwiYWN0
aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiR3Jh
bmQgQ2FuYWwiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0
ZXgubmFtZXMiOlsiSGFieSBSYW5jaCJdLCJhY3RpdmUiOltbMCw1XV19LHsibmEi
OltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJIaWdoIE1lc2EiXSwiYWN0aXZlIjpb
WzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiSm9zZSBTb2xh
cyBSdWluIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4
Lm5hbWVzIjpbIkp1bmt5YXJkIFNpdGUiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5h
IjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiTGFzIENhbm9wYXMiXSwiYWN0aXZl
IjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiTGFzIENv
bGluYXMiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgu
bmFtZXMiOlsiTGFzIEZvc2FzIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2Zh
bHNlXSwidmVydGV4Lm5hbWVzIjpbIkxlYXZlcnRvbiJdLCJhY3RpdmUiOltbMCw1
XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJMb3MgR3VhbmFjb3Mi
XSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMi
OlsiTG9zIE1vcnRlcm9zIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNl
XSwidmVydGV4Lm5hbWVzIjpbIkxvcyBNdWVydG9zIl0sImFjdGl2ZSI6W1swLDVd
XX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIkxvc3QgTW91bmQiXSwi
YWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsi
TWFyaWppbGRhIFJ1aW4iXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2Vd
LCJ2ZXJ0ZXgubmFtZXMiOlsiTWVzYSBHcmFuZGUiXSwiYWN0aXZlIjpbWzAsNV1d
fSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiTWV0aG9kaXN0IENodXJj
aCJdLCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1l
cyI6WyJNdXJwaHkgU2l0ZS0tU2FmZm9yZCBFYXN0Il0sImFjdGl2ZSI6W1swLDVd
XX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIk93ZW5zLUNvbHZpbiJd
LCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6
WyJQaXBlciBTcHJpbmdzIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNl
XSwidmVydGV4Lm5hbWVzIjpbIlB1ZWJsbyBCbGFuY28iXSwiYWN0aXZlIjpbWzAs
NV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiUHVlYmxvIGRlbCBN
b250ZSJdLCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5u
YW1lcyI6WyJQdWVibG8gR3JhbmRlIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6
W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIlB1ZWJsbyBTYWxhZG8iXSwiYWN0aXZl
IjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiUmFiaWQg
UnVpbiJdLCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5u
YW1lcyI6WyJSYXR0bGVzbmFrZSBNZXNhIl0sImFjdGl2ZSI6W1swLDVdXX0seyJu
YSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIlJlZXZlIFJ1aW4iXSwiYWN0aXZl
IjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiUmljaGFy
ZHNvbiBPcmNoYXJkIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwi
dmVydGV4Lm5hbWVzIjpbIlJpbGxpdG8gRmFuIl0sImFjdGl2ZSI6W1swLDVdXX0s
eyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIlJpbmNvbiBDYW55b24iXSwi
YWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsi
U2FuIFhhdmllciBCcmlkZ2UiXSwiYWN0aXZlIjpbWzAsNV1dfSx7Im5hIjpbZmFs
c2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiU2Vjb25kIENhbnlvbiBDb21wb3VuZCJdLCJh
Y3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJT
aGFyb24gU2l0ZSJdLCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZl
cnRleC5uYW1lcyI6WyJTcGVhciBSYW5jaCJdLCJhY3RpdmUiOltbMCw1XV19LHsi
bmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJTd2luZ2xlJ3MgU2FtcGxlIl0s
ImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpb
IlRhbnF1ZSBWZXJkZSBWaWxsYWdlIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6
W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIlRyZXMgUHVlYmxvcyJdLCJhY3RpdmUi
OltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJUd2luIEhh
d2tzIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5h
bWVzIjpbIlZlcmVzIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwi
dmVydGV4Lm5hbWVzIjpbIldlYmIgUnVpbiJdLCJhY3RpdmUiOltbMCw1XV19LHsi
bmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6WyJXZXMgSmVybmlnYW4gU2l0ZSJd
LCJhY3RpdmUiOltbMCw1XV19LHsibmEiOltmYWxzZV0sInZlcnRleC5uYW1lcyI6
WyJXaGlwdGFpbCBSdWluIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNl
XSwidmVydGV4Lm5hbWVzIjpbIldoaXRtZXIiXSwiYWN0aXZlIjpbWzAsNV1dfSx7
Im5hIjpbZmFsc2VdLCJ2ZXJ0ZXgubmFtZXMiOlsiV3JpZ2h0Il0sImFjdGl2ZSI6
W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIll1bWEgV2Fz
aC0tVHVjc29uIl0sImFjdGl2ZSI6W1swLDVdXX0seyJuYSI6W2ZhbHNlXSwidmVy
dGV4Lm5hbWVzIjpbIll1bWEgV2FzaCBTaXRlIl0sImFjdGl2ZSI6W1swLDVdXX0s
eyJuYSI6W2ZhbHNlXSwidmVydGV4Lm5hbWVzIjpbIlphbmFyZGVsbGkgU2l0ZSJd
LCJhY3RpdmUiOltbMCw1XV19XSwiaWVsIjpbW10sW10sWzIxNV0sWzI1MF0sWzI1
MSwyNzZdLFtdLFsyMTNdLFtdLFsxMDksMjE2XSxbMjE3LDI1MiwyNzcsMjk2LDMx
OV0sWzEzNCwyNzgsMjk3XSxbMjE4LDI1MywyNzksMjk4LDMyMCwzNDVdLFtdLFsy
MTksMjU0LDMyMSwzNDYsMzg1XSxbMSwyMjAsMjU1LDMyMiwzNDcsNDA2XSxbMiwx
MSwyMjEsMjU2LDI4MCwyOTksMzIzLDM0OCw0MDddLFtdLFtdLFsxMDEsMTIyLDIy
MiwyODEsMzI0LDM0OSwzODYsNDA4LDQzNiw0NTVdLFszLDEyLDIwLDIyMywyNTcs
MzI1LDM1MCw0MDksNDc1XSxbMTEwLDEyNywxNDYsMjI0LDI4MiwzMDAsMzUxLDM3
OCwzODcsNDEwLDQzNyw0NTYsNTAxXSxbXSxbMTExLDEyOCwxNTQsMjI1LDI4Mywz
MDEsMzUyLDM4OCw0MTEsNDM4LDQ1Nyw0NzYsNTAyXSxbMTEyLDEyOSwxNTUsMTY1
LDIyNiwyODQsMzAyLDM1MywzODksNDEyLDQzOSw0NTgsNDc3LDUwM10sW10sW10s
WzEwNCwxMTMsMTMwLDE1NiwxNjYsMjI3LDI4NSwzMDMsMzU0LDM5MCw0MTMsNDQw
LDQ1OSw0NzgsNTA0XSxbNCwxMywyMSwzNywyMjgsMjU4LDMyNiwzNTUsNDE0LDQ3
OSw1MTksNTQwLDU1OCw1NzhdLFsxMDMsMTI2LDIxNF0sWzEwMiwxNTcsMjI5LDI1
OSwzMDQsMzI3LDM1NiwzNzksMzkxLDQxNSw0NDEsNDYwLDQ4MCw1MDUsNTQxLDU1
OSw1NzksNTk3XSxbMzA1LDUyMCw1MzddLFs1LDE0LDIyLDM4LDQ0LDIzMCwyNjAs
MzI4LDM1Nyw0MTYsNDgxLDUyMSw1NDIsNTYwLDU4MCw2MTFdLFsyOF0sW10sWzYs
MTUsMjMsMzksNDUsNTAsMjMxLDI2MSwzMjksMzU4LDQxNyw0ODIsNTIyLDU0Myw1
NjEsNTgxLDYxMl0sWzIzMiwyNjIsMjg2LDMwNiwzMzAsMzU5LDM5Miw0MTgsNDQy
LDQ2MSw0ODMsNTA2LDUyMyw1NDQsNTYyLDU4Miw1OTgsNjEzLDYzNCw2NDddLFsx
MjMsMzE3LDUzOCw2MzBdLFsyMzMsMjYzLDI4NywzMDcsMzMxLDM2MCwzOTMsNDE5
LDQ0Myw0NjIsNDg0LDUwNyw1MjQsNTQ1LDU2Myw1ODMsNTk5LDYxNCw2MzUsNjQ4
LDY2MF0sWzcsMTYsMjQsNDAsNDYsNTEsNjMsMjM0LDI2NCwyODgsMzA4LDMzMiwz
NjEsNDIwLDQ4NSw1MjUsNTQ2LDU2NCw1ODQsNjE1LDY2MSw2NzhdLFs4LDE3LDI1
LDQxLDQ3LDUyLDY0LDY3XSxbXSxbMTM5LDE0MywxNTAsMTc0LDE4MSwxODUsMjM1
LDI2NSwzMzMsMzYyLDM5NCw0MjEsNDg2LDUyNiw1NDcsNTY1LDU4NSw2MTYsNjYy
LDY3OSw2OTRdLFsxMTQsMTIxLDE1OCwxNzcsMzA5LDM2MywzODAsNDg3XSxbMjM2
LDI2NiwzMzQsMzY0LDM5NSw0MjIsNDQ0LDQ2Myw0ODgsNTA4LDUyNyw1NDgsNTY2
LDU4Niw2MDAsNjE3LDYzNiw2NDksNjYzLDY4MCw2OTUsNzA2XSxbMjksNTVdLFsx
NDAsMTQ0LDE1MSwxODIsMTg2LDE5MV0sWzM4MV0sWzExNSwxMjQsMTQ3LDE1OSwx
NzgsMTk2XSxbXSxbXSxbXSxbXSxbMTE2LDEzMSwxNjAsMTY3LDE3MSwxNzIsMjM3
LDI4OSwzMTAsMzY1LDM5Niw0MjMsNDQ1LDQ2NCw0ODksNTA5LDYwMSw2MTgsNjM3
LDY1MCw2NjQsNjgxLDY5Niw3MDcsNzIwXSxbMTkwLDIzOCwyNjcsMzM1LDM2Niwz
OTcsNDI0LDQ0Niw0NjUsNDkwLDUxMCw1MjgsNTQ5LDU2Nyw1ODcsNjAyLDYxOSw2
MzgsNjUxLDY2NSw2ODIsNzA4LDcyMSw3MzNdLFsxNDEsMTQ1LDE1MiwxNzUsMTgz
LDE4NywxOTIsMTk4LDIzOSwyNjgsMzM2LDM2NywzOTgsNDI1LDQ5MSw1MjksNTUw
LDU2OCw1ODgsNjIwLDY2Niw2ODMsNjk3LDcyMiw3MzQsNzQzXSxbOSwxOCwyNiw0
Miw0OCw1Myw2NSw2OCw3MCwxOTMsMTk5LDIwNSwyNDAsMjY5LDMzNywzNjgsNDI2
LDQ5Miw1MzAsNTUxLDU2OSw1ODksNjIxLDY2Nyw2ODQsNzIzLDczNSw3NDRdLFsx
MzcsMjQxLDI3MCwzMzgsMzY5LDQyNyw0NDcsNDY2LDQ5Myw1MTEsNTMxLDU1Miw1
NzAsNTkwLDYwMyw2MjIsNjM5LDY1Miw2NjgsNjg1LDY5OCw3MDksNzI0LDczNiw3
NDUsNzU0LDc2Ml0sWzMwLDU2LDcyLDEzNSwxODldLFsxMTcsMTI1LDE0OCwxNjEs
MjAxLDM3MCwzODIsNTM5LDYyMyw2MzEsNjc3LDcxNyw4MDUsODA2LDgwNyw4MDgs
ODA5LDgxMF0sWzExOCwxMzIsMTQ5LDE2MiwxNjgsMTc5LDIwMiwyNDIsMjkwLDMx
MSwzNzEsMzk5LDQyOCw0NDgsNDY3LDUxMiw1NzEsNTkxLDYwNCw2MzIsNjQwLDY1
Myw2NjksNjg2LDY5OSw3MTAsNzE4LDcyNSw3MzcsNzQ2LDc1NSw3NjMsNzcwLDIx
MF0sW10sWzMxLDU3LDczLDgwLDI0MywyNzEsMzM5LDM3Miw0MDAsNDI5LDQ0OSw0
NjgsNDk0LDUxMyw1MzIsNTUzLDU3Miw1OTIsNjA1LDYyNCw2NDEsNjU0LDY3MCw2
ODcsNzAwLDcxMSw3MjYsNzM4LDc0Nyw3NTYsNzY0LDc3MSw3NzldLFtdLFszMiw1
OCw3NCw4MSw4Nl0sWzEzNiwyMDksMjQ0LDI3MiwyOTEsMzEyLDM0MCwzODMsNDAx
LDQzMCw0NTAsNDY5LDQ5NSw1MTQsNTMzLDU1NCw1NzMsNTkzLDYwNiw2MjUsNjMz
LDY0Miw2NTUsNjcxLDY4OCw3MDEsNzEyLDcxOSw3MjcsNzM5LDc0OCw3NTcsNzY1
LDc3Miw3NzgsNzgwLDc4NV0sW10sWzEwN10sWzEwNSwxMDYsMTE5LDEzMywxNjMs
MTY5LDIwMywyNDUsMjkyLDMxMywzNzMsNDAyLDQzMSw0NTEsNDcwLDQ5Niw1MTUs
NjA3LDYyNiw2NDMsNjU2LDY3Miw2ODksNzAyLDcxMyw3MjgsNzQ5LDc1OCw3NjYs
NzczLDc4MSw3ODYsNzkxXSxbMzMsNTksNzUsODIsODcsOTFdLFsxMDgsMTM4LDE0
MiwxNTMsMTc2LDE4NCwxODgsMTk0LDIwNiwyMDgsMjQ2LDI3MywzNDEsMzc0LDQz
Miw0NzEsNDk3LDUzNCw1NTUsNTc0LDU5NCw2MjcsNjczLDY5MCw3MjksNzQwLDc1
MCw3NzQsNzgyLDc4Nyw3OTIsNzk2XSxbXSxbMTAsMTksMjcsNDMsNDksNTQsNjYs
NjksNzEsNzksMTk1LDIwMCwyMDddLFsyNDcsMjc0LDI5MywzMTQsMzQyLDM3NSw0
MDMsNDMzLDQ1Miw0NzIsNDk4LDUxNiw1MzUsNTU2LDU3NSw1OTUsNjA4LDYyOCw2
NDQsNjU3LDY3NCw2OTEsNzAzLDcxNCw3MzAsNzQxLDc1MSw3NTksNzY3LDc3NSw3
ODMsNzg4LDc5Myw3OTcsNzk5XSxbMzE4XSxbMzQsNjAsNzYsODMsODgsOTIsOTVd
LFsyNDgsMjc1LDI5NCwzMTUsMzQzLDM3Niw0MDQsNDM0LDQ1Myw0NzMsNDk5LDUx
Nyw1MzYsNTU3LDU3Niw1OTYsNjA5LDYyOSw2NDUsNjU4LDY3NSw2OTIsNzA0LDcx
NSw3MzEsNzQyLDc1Miw3NjAsNzY4LDc3Niw3ODQsNzg5LDc5NCw3OTgsODAwLDgw
Ml0sWzEyMCwxNjQsMTcwLDE3MywxODAsMTk3LDIwNCwyMTEsMjEyLDI0OSwyOTUs
MzE2LDM0NCwzNzcsMzg0LDQwNSw0MzUsNDU0LDQ3NCw1MDAsNTE4LDU3Nyw2MTAs
NjQ2LDY1OSw2NzYsNjkzLDcwNSw3MTYsNzMyLDc1Myw3NjEsNzY5LDc3Nyw3OTAs
Nzk1LDgwMSw4MDMsODA0LDgxMV0sWzM1LDYxLDc3LDg0LDg5LDkzLDk2LDk4XSxb
XSxbMzYsNjIsNzgsODUsOTAsOTQsOTcsOTksMTAwXV0sIm9lbCI6W1syMTQsMjEz
XSxbMjQ5LDI0OCwyNDcsMjQ2LDI0NSwyNDQsMjQzLDI0MiwyNDEsMjQwLDIzOSwy
MzgsMjM3LDIzNiwyMzUsMjM0LDIzMywyMzIsMjMxLDIzMCwyMjksMjI4LDIyNywy
MjYsMjI1LDIyNCwyMjMsMjIyLDIyMSwyMjAsMjE5LDIxOCwyMTcsMjE2LDIxNV0s
WzI3NSwyNzQsMjczLDI3MiwyNzEsMjcwLDI2OSwyNjgsMjY3LDI2NiwyNjUsMjY0
LDI2MywyNjIsMjYxLDI2MCwyNTksMjU4LDI1NywyNTYsMjU1LDI1NCwyNTMsMjUy
LDI1MSwyNTAsMTIwLDExOSwxMTgsMTE3LDExNiwxMTUsMTE0LDExMywxMTIsMTEx
LDExMCwxMDksMTAxXSxbODA1LDI5NSwyOTQsMjkzLDI5MiwyOTEsMjkwLDI4OSwy
ODgsMjg3LDI4NiwyODUsMjg0LDI4MywyODIsMjgxLDI4MCwyNzksMjc4LDI3Nywy
NzYsMTIxLDEwMl0sWzMxNiwzMTUsMzE0LDMxMywzMTIsMzExLDMxMCwzMDksMzA4
LDMwNywzMDYsMzA1LDMwNCwzMDMsMzAyLDMwMSwzMDAsMjk5LDI5OCwyOTcsMjk2
LDEyNSwxMjQsMTIzLDEyMl0sWzMxN10sWzEyNl0sWzMxOF0sWzM0NCwzNDMsMzQy
LDM0MSwzNDAsMzM5LDMzOCwzMzcsMzM2LDMzNSwzMzQsMzMzLDMzMiwzMzEsMzMw
LDMyOSwzMjgsMzI3LDMyNiwzMjUsMzI0LDMyMywzMjIsMzIxLDMyMCwzMTksMTMz
LDEzMiwxMzEsMTMwLDEyOSwxMjgsMTI3XSxbMzc3LDM3NiwzNzUsMzc0LDM3Mywz
NzIsMzcxLDM3MCwzNjksMzY4LDM2NywzNjYsMzY1LDM2NCwzNjMsMzYyLDM2MSwz
NjAsMzU5LDM1OCwzNTcsMzU2LDM1NSwzNTQsMzUzLDM1MiwzNTEsMzUwLDM0OSwz
NDgsMzQ3LDM0NiwzNDUsMTM2LDEzNSwxMzRdLFszODQsMzgzLDM4MiwzODEsMzgw
LDM3OSwzNzhdLFs0MDUsNDA0LDQwMyw0MDIsNDAxLDQwMCwzOTksMzk4LDM5Nywz
OTYsMzk1LDM5NCwzOTMsMzkyLDM5MSwzOTAsMzg5LDM4OCwzODcsMzg2LDM4NSwx
MzgsMTM3LDEwLDksOCw3LDYsNSw0LDMsMiwxXSxbXSxbNDM1LDQzNCw0MzMsNDMy
LDQzMSw0MzAsNDI5LDQyOCw0MjcsNDI2LDQyNSw0MjQsNDIzLDQyMiw0MjEsNDIw
LDQxOSw0MTgsNDE3LDQxNiw0MTUsNDE0LDQxMyw0MTIsNDExLDQxMCw0MDksNDA4
LDQwNyw0MDZdLFs0NTQsNDUzLDQ1Miw0NTEsNDUwLDQ0OSw0NDgsNDQ3LDQ0Niw0
NDUsNDQ0LDQ0Myw0NDIsNDQxLDQ0MCw0MzksNDM4LDQzNyw0MzYsMTQyLDE0MSwx
NDAsMTM5LDE5LDE4LDE3LDE2LDE1LDE0LDEzLDEyLDExXSxbNDc0LDQ3Myw0NzIs
NDcxLDQ3MCw0NjksNDY4LDQ2Nyw0NjYsNDY1LDQ2NCw0NjMsNDYyLDQ2MSw0NjAs
NDU5LDQ1OCw0NTcsNDU2LDQ1NSwxNDUsMTQ0LDE0MywyNywyNiwyNSwyNCwyMywy
MiwyMSwyMF0sWzEwM10sWzM2LDM1LDM0LDMzLDMyLDMxLDMwLDI5LDI4XSxbNTAw
LDQ5OSw0OTgsNDk3LDQ5Niw0OTUsNDk0LDQ5Myw0OTIsNDkxLDQ5MCw0ODksNDg4
LDQ4Nyw0ODYsNDg1LDQ4NCw0ODMsNDgyLDQ4MSw0ODAsNDc5LDQ3OCw0NzcsNDc2
LDQ3NSwxNDksMTQ4LDE0NywxNDZdLFs1MTgsNTE3LDUxNiw1MTUsNTE0LDUxMyw1
MTIsNTExLDUxMCw1MDksNTA4LDUwNyw1MDYsNTA1LDUwNCw1MDMsNTAyLDUwMSwx
NTMsMTUyLDE1MSwxNTAsNDMsNDIsNDEsNDAsMzksMzgsMzddLFs1MzYsNTM1LDUz
NCw1MzMsNTMyLDUzMSw1MzAsNTI5LDUyOCw1MjcsNTI2LDUyNSw1MjQsNTIzLDUy
Miw1MjEsNTIwLDUxOSwxNjQsMTYzLDE2MiwxNjEsMTYwLDE1OSwxNTgsMTU3LDE1
NiwxNTUsMTU0XSxbNTM5LDUzOCw1MzddLFs4MDYsNTU3LDU1Niw1NTUsNTU0LDU1
Myw1NTIsNTUxLDU1MCw1NDksNTQ4LDU0Nyw1NDYsNTQ1LDU0NCw1NDMsNTQyLDU0
MSw1NDAsMTcwLDE2OSwxNjgsMTY3LDE2NiwxNjVdLFs1NzcsNTc2LDU3NSw1NzQs
NTczLDU3Miw1NzEsNTcwLDU2OSw1NjgsNTY3LDU2Niw1NjUsNTY0LDU2Myw1NjIs
NTYxLDU2MCw1NTksNTU4LDE3MSwxMDUsMTA0XSxbXSxbXSxbODA3LDU5Niw1OTUs
NTk0LDU5Myw1OTIsNTkxLDU5MCw1ODksNTg4LDU4Nyw1ODYsNTg1LDU4NCw1ODMs
NTgyLDU4MSw1ODAsNTc5LDU3OCwxNzMsMTcyLDEwNl0sWzYxMCw2MDksNjA4LDYw
Nyw2MDYsNjA1LDYwNCw2MDMsNjAyLDYwMSw2MDAsNTk5LDU5OCw1OTcsMTc2LDE3
NSwxNzQsNDksNDgsNDcsNDYsNDUsNDRdLFtdLFs2MjksNjI4LDYyNyw2MjYsNjI1
LDYyNCw2MjMsNjIyLDYyMSw2MjAsNjE5LDYxOCw2MTcsNjE2LDYxNSw2MTQsNjEz
LDYxMiw2MTEsMTgwLDE3OSwxNzgsMTc3XSxbNjMzLDYzMiw2MzEsNjMwXSxbNjQ2
LDY0NSw2NDQsNjQzLDY0Miw2NDEsNjQwLDYzOSw2MzgsNjM3LDYzNiw2MzUsNjM0
LDE4NCwxODMsMTgyLDE4MSw1NCw1Myw1Miw1MSw1MF0sWzYyLDYxLDYwLDU5LDU4
LDU3LDU2LDU1XSxbMTA3XSxbNjU5LDY1OCw2NTcsNjU2LDY1NSw2NTQsNjUzLDY1
Miw2NTEsNjUwLDY0OSw2NDgsNjQ3LDE4OCwxODcsMTg2LDE4NSw2Niw2NSw2NCw2
M10sWzgwOCw2NzYsNjc1LDY3NCw2NzMsNjcyLDY3MSw2NzAsNjY5LDY2OCw2Njcs
NjY2LDY2NSw2NjQsNjYzLDY2Miw2NjEsNjYwXSxbNjc3XSxbNjkzLDY5Miw2OTEs
NjkwLDY4OSw2ODgsNjg3LDY4Niw2ODUsNjg0LDY4Myw2ODIsNjgxLDY4MCw2Nzks
Njc4LDE4OV0sWzEwOCw4MDksNzA1LDcwNCw3MDMsNzAyLDcwMSw3MDAsNjk5LDY5
OCw2OTcsNjk2LDY5NSw2OTQsMTkwLDY5LDY4LDY3XSxbNzEsNzBdLFtdLFs3MTYs
NzE1LDcxNCw3MTMsNzEyLDcxMSw3MTAsNzA5LDcwOCw3MDcsNzA2LDE5NSwxOTQs
MTkzLDE5MiwxOTFdLFs3MTksNzE4LDcxNywxOTcsMTk2XSxbNzMyLDczMSw3MzAs
NzI5LDcyOCw3MjcsNzI2LDcyNSw3MjQsNzIzLDcyMiw3MjEsNzIwXSxbNzgsNzcs
NzYsNzUsNzQsNzMsNzJdLFsyMDAsMTk5LDE5OF0sW10sWzIwMiwyMDFdLFtdLFtd
LFtdLFtdLFs4MTAsNzQyLDc0MSw3NDAsNzM5LDczOCw3MzcsNzM2LDczNSw3MzQs
NzMzLDIwNCwyMDNdLFs3NTMsNzUyLDc1MSw3NTAsNzQ5LDc0OCw3NDcsNzQ2LDc0
NSw3NDQsNzQzXSxbNzYxLDc2MCw3NTksNzU4LDc1Nyw3NTYsNzU1LDc1NCwyMDcs
MjA2LDIwNV0sWzc2OSw3NjgsNzY3LDc2Niw3NjUsNzY0LDc2Myw3NjIsMjA4LDc5
XSxbNzc3LDc3Niw3NzUsNzc0LDc3Myw3NzIsNzcxLDc3MF0sWzIwOSw4NSw4NCw4
Myw4Miw4MSw4MF0sWzgxMSw3NzgsMjEwXSxbNzg0LDc4Myw3ODIsNzgxLDc4MCw3
NzksMjExXSxbXSxbNzkwLDc4OSw3ODgsNzg3LDc4Niw3ODUsOTAsODksODgsODcs
ODZdLFtdLFs5NCw5Myw5Miw5MV0sWzc5NSw3OTQsNzkzLDc5Miw3OTFdLFtdLFtd
LFsyMTIsNzk4LDc5Nyw3OTZdLFs5Nyw5Niw5NV0sWzgwMSw4MDAsNzk5XSxbXSxb
XSxbODAzLDgwMl0sW10sWzk5LDk4XSxbODA0XSxbXSxbMTAwXSxbXSxbXV19fTsN
CiAgdmFyIG9wdGlvbnMgPSB7Im5kdHYudmVyc2lvbiI6IjAuMTMuMywyMDIyLTEx
LTIwIn07DQogIC8vRU5EIEdSQVBIIERBVEEgSU5JVA0KICANCiAgLy9JbnNlcnQg
aW5pdCBKUyBIZXJlDQogICQoZnVuY3Rpb24oKSB7DQogICAgb3B0aW9ucy5ncmFw
aERhdGEgPSBncmFwaERhdGE7DQogICAgdmFyIGdyYXBoID0gbmV3IG5kdHZfZDMo
b3B0aW9ucyk7ICAgICAgICANCiAgfSkNCiAgPC9zY3JpcHQ+PC9ib2R5Pg0KPC9o
dG1sPg==

"></iframe>

### Figure 6.25: Interactive Networks {- #Figure_6_25}

Fig. 6.25. An example of a dynamic network visual created in R. Notice how the nodes and edges are responding to the movement of the edge under the cursor and the drop down menu that allows selection of nodes by group.

For this example we closely follow an example provided on the [Static and dynamic network visualization with R](https://kateto.net/network-visualization) workshop documents online but using the [Cibola technological similarity data](data/Figure6_25.Rdata) instead.


```r
library(visNetwork)
library(networkD3)
library(igraph)

load("data/Figure6_25.Rdata") # Contains an igraph graph object

# Use igraph to make the graph and find membership
clust <- cluster_louvain(graph)
members <- membership(clust)

# Convert to object suitable for networkD3
graph_d3 <- igraph_to_networkD3(graph, group = members)

# Modify interactive network to allow highlighting by groups, etc.
links <- graph_d3$links
colnames(links) <- c("from", "to")
links[, 1] <- links[, 1] + 1
links[, 2] <- links[, 2] + 1
nodes <- graph_d3$nodes
colnames(nodes)[1] <- "id"

# Create node and link objects in d3 format
vis_nodes <- nodes
vis_links <- links

# Set visualization options
vis_nodes$shape  <- "dot"
vis_nodes$shadow <- TRUE # Nodes will drop shadow
vis_nodes$borderWidth <- 2 # Node border width
vis_nodes$color.background <- c("slategrey", "tomato", "gold",
                                "purple")[nodes$group]
vis_nodes$color.border <- "black"
vis_nodes$color.highlight.background <- "orange"
vis_nodes$color.highlight.border <- "darkred"

# Create network in d3 format
visnet <- visNetwork(vis_nodes, vis_links)

# View network with visualization options active
visOptions(visnet, highlightNearest = TRUE, selectedBy = "group")
```

```{=html}
<div class="visNetwork html-widget html-fill-item" id="htmlwidget-7a2280d66564672caf04" style="width:672px;height:672px;"></div>
<script type="application/json" data-for="htmlwidget-7a2280d66564672caf04">{"x":{"nodes":{"id":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"],"group":[1,2,2,1,2,1,1,3,2,3,1,1,1,2,2,1,2,2,2,3,2,1,2,2,1,2,1,1,1,4,2],"shape":["dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot"],"shadow":[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],"borderWidth":[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],"color.background":["slategrey","tomato","tomato","slategrey","tomato","slategrey","slategrey","gold","tomato","gold","slategrey","slategrey","slategrey","tomato","tomato","slategrey","tomato","tomato","tomato","gold","tomato","slategrey","tomato","tomato","slategrey","tomato","slategrey","slategrey","slategrey","purple","tomato"],"color.border":["black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black"],"color.highlight.background":["orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange","orange"],"color.highlight.border":["darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred","darkred"],"label":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"]},"edges":{"from":[1,2,1,4,4,6,7,8,5,9,5,7,4,8,1,4,6,6,10,11,1,8,4,12,6,10,11,4,1,7,10,9,5,2,14,9,5,10,11,1,8,6,7,12,13,10,5,14,15,9,2,9,15,14,10,5,17,8,2,3,5,9,15,14,18,17,8,14,10,9,5,15,18,19,9,2,17,12,6,1,16,10,8,13,4,11,5,21,9,19,18,10,2,8,15,17,14,19,21,23,17,18,10,15,9,5,2,6,11,12,1,13,22,4,21,10,14,24,19,5,2,8,9,23,17,18,15,10,12,16,1,7,6,22,11,25,4,13,10,11,12,27,6,13,7,22,25,1,16,4,8,28,11,1,13,16,27,19,15,14,24,9,23,26,17,5,20],"to":[4,5,6,6,8,8,8,9,9,10,10,10,10,10,11,11,11,12,12,12,12,12,12,13,13,13,13,13,13,13,14,14,14,15,15,15,15,16,16,16,16,16,16,16,16,17,17,17,17,17,17,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,20,20,20,20,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,26,26,26,26,26,26,26,26,26,26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,28,28,28,28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,31,31,31,31,31,31,31,31,31,31]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":["1","2","3","4"],"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":true,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false,"variable":"group","main":"Select by group","values":[1,2,3,4]},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script>
```

### Figure 6.26: SWSN Example 1{- #Figure_6_26}

Fig. 6.26. Networks by time for the SWSN project area (from Mills et al. 2013).

The figure for the original plot in Mills et al. 2013 was produced in R and then compiled and modified using Adobe Illustrator. First a regional color scheme was defined and then each time period was plotted using this color scheme. In Illustrator components were arranged in rough geographic positions and isolates were placed at the margin. Click the link for more info on the [Southwest Social Networks Project](#SWSN.)

The following chunk of code reproduces Figure 6.26 for one time period (AD1300-1350). [Download these data](data/Figure6_26.Rdata) to follow along.


```r
library(statnet)
library(ggraph)

load("data/Figure6_26.Rdata")

# Create sna network object
net <-
  network(event2dichot(sim, method = "absolute", thresh = 0.75),
          directed = FALSE)

# define color scheme. colors listed in order based on the
# factor attr$Macro
mycols <- c("#000738", "#ffa1a1", "#ad71d8", "#016d1b", "#00ff30",
            "#92d8ff", "#ffffff", "#adadad", "#846b00", "#ff0000",
            "#5273dd", "#946a43", "#a00000", "#f97c00", "#00ffec",
            "#ffff3e", "#824444", "#00ba89", "#00ba89", "#0303ff")

# Plot network
set.seed(235)
ggraph(net, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(fill = as.factor(attr$Macro), size = evcent(net)),
                  shape = 21,
                  show.legend = FALSE) +
  scale_size(range = c(1.5, 3)) +
  scale_fill_manual(values = mycols) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/unnamed-chunk-21-1.png" width="672" />

### Figure 6.27: SWSN Example 2{- #Figure_6_27}

Fig. 6.27. An explicit geographic map network of the SWSN project area through time (Mills et al. 2013).

The original version of this figure was produced in ArcGIS using data prepared in R. Here we show how these same network maps with edges color coded by geographic length can be produced in R. We provide code to prepare a map for one time period (AD1300-1350). [Use these data](data/Figure6_27.Rdata) to follow along. Note that this figure will differ slightly from the one in the book and in the original Mills et al. 2013 publication as site locations have been jittered. In this example we use geographic coordinates to calculate distance. See the [spatial networks](#SpatialNetworks) section for more details.

<div class="rmdtip">
<p>Note for short edges to be visible on top of long edges here we must
first sort the order of edges bu length in the original edge list before
converting it into a igraph network object. In the three lines beginning
with <code># Order edges so shorest will plot last</code>, we use the
<code>order</code> function and set <code>decreasing = TRUE</code> so
that edges will be listed from longest to shortest. The order of the
edge list is the order that edges will be plotted.</p>
</div>




```r
library(statnet)
library(igraph)
library(intergraph)
library(geosphere)
library(ggmap)
library(sf)
library(tidyverse)
library(ggraph)

# Load in network and map data
load("data/Figure6_27.Rdata")

# prepare network object
net <- network(event2dichot(sim, method = "absolute", thresh = 0.75),
               directed = FALSE)
r_net <- asIgraph(net)

# convert coordinates to lat/long and covert to sf object
locations_sf <- st_as_sf(attr,
                         coords = c("EASTING", "NORTHING"),
                         crs = 26912)
z <- st_transform(locations_sf, crs = 4326)
coord1 <- do.call(rbind, st_geometry(z)) %>%
  tibble::as_tibble() %>%
  setNames(c("lon", "lat"))

# output coordinates in data frame
xy <- as.data.frame(coord1)
colnames(xy) <- c("x", "y")

# Create edge list with xy coordinates for each source and target
edgelist2 <- get.edgelist(r_net)
edges2 <- data.frame(xy[edgelist2[, 1], ], xy[edgelist2[, 2], ])
colnames(edges2) <- c("X1", "Y1", "X2", "Y2")

# Determine the geographic distances of edges using the distm
# function in the geosphere package
dist_meas <- NULL
for (i in seq_len(nrow(edges2))) {
  temp <- as.matrix(edges2[i, ])
  dist_meas[i] <- distm(temp[1, 1:2], temp[1, 3:4])
}

# Order edges so shortest will plot last
net_dat <- as.data.frame(cbind(edges2, dist_meas))
net_dat <- net_dat[order(net_dat$dist_meas, decreasing = TRUE), ]

# Create bins in distance measurement
net_dat <- net_dat %>%
  mutate(DistBins = cut(dist_meas,
                        breaks = c(-Inf, 25000, 100000, 250000, Inf)))

# Plot network map
ggmap(base2, darken = 0.5) +
  geom_segment(
    data = net_dat,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2,
      col = DistBins
    ),
    size = 0.15,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("white", "skyblue", "dodgerblue",
                                "darkblue")) +
  theme_graph()
```

<img src="05-visualization_files/figure-html/Fig6_27-1.png" width="672" />

