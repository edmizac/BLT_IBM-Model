; ==== Black lion tamarin model ============
; Eduardo Zanette & Ronald Bialozyt
; Two travel modes: long-distance (target selected when energy > lvl 2 and travels to this direction up to when energy < lvl 1) and short-distance (when energy < lvl 1)
; Random angle at each step (random-angle? switch)
; Phenology values (specific energy and species-time values on and off by switcher phenology-on? Stil need to add phenology cycles though)
; ------------------------------------------------

extensions [ gis r palette pathdir] ; using the GIS extension of NetLogo

;; BREEDS ;;
turtles-own [
  x_UTM y_UTM
  X_coords Y_coords ; X and Y list of coordinates (x_UTM and y_UTM) for home range calculation with the r extension in the end of each run (days = n_days)
  Name ; monkey who number for home range calculation with the r extension in case there's more than one group
  KDE_values ; output of amt package in calc-homerange

]

; trees
breed [feeding-trees feeding-tree]
feeding-trees-own [ species id-tree visitations ] ; feeding trees have species and id code and a visited counter

breed [sleeping-trees sleeping-tree]
sleeping-trees-own [ species id-tree visitations ]

breed [resting-trees resting-tree]
resting-trees-own [ species id-tree ]

breed [legend-trees legend-tree] ; to set up a legend with the color of trees

breed [seeds seed]
seeds-own [ id-seed species mother-tree disp-day ]

breed [monkeys monkey]
monkeys-own [
  energy          ; energy the tamarin has left
;  status         ; what is the desire ===== DO WE REALLY NEED THIS? ==============
  action          ; what was the last action
  action-time     ; how long you do the same action again (other than frugivory)
  frugivory-time  ; how long you consume the same species (= feeding bout)
  going-sleeping? ; if timestep > 108 and tamarins are going to sl
  behavior        ; as in activity budget data tables
  steps-moved     ; number of steps taken
  dist-traveled   ; distance traveled this time step
  DPL             ; daily path length
  DPL_d           ; list with values of DPL for the DPL plot
  travel_mode     ; if it is short or long distance travel
  tree_target     ; target tree (short distance, but = long distance in this travel mode)
  tree_target_mem ; target tree memory for when tamarins are avoiding the matrix (avoid-matrix and avoid-patch-set)
  tree_target_dist ; target tree distance
  ld_tree_target  ; long distance target tree
  tree_target_species ; species of the target tree independent of travel mode
  patch_avoid_matrix ; patch to walk to when straight-line-to-target? = FALSE
  front_patches   ; patches in-cone or in-radius
  candidate_patches ; patches to go when avoiding the matrix
  straight-line-to-target? ; if there is matrix in front of the tamarin in straight line
  travelmodelist  ; list to make travel mode histogram
  tree_current    ; old_tree

  tree_pot_list   ; list of all feeding trees in homerange for that tamarin
  tree_ate_list   ; list of trees the tamarins did eat
  tree_mem_list   ; list of timesteps since the tamarin feeded on that tree
  tree_add_list   ; helper list to increase the mem list

  seed_ate_list   ; list of trees they fed on ([who] of tree_current)
  seed_mem_list   ; list of timesteps since the tamarin ate the seed
  seed_add_list   ; helper list to increase the mem list by 1 each time step
;  x_UTM
;  y_UTM

]

breed [blobs blob]
blobs-own [patch_before]

patches-own [
  habitat
]

;; GLOBALS ;;
globals [
  patch-scale
  behaviorsequence

  timestep ; step counter during one day
  day  ; present day in the simulation
  ;scale ; to fit the data to the simulation area
  meanxcoord ; translating the geo coordinates to world coordinates
  meanycoord ; translating the geo coordinates to world coordinates
  ;step_forget ; amount of timesteps until the tamarin forgets to be in that tree
  midday ; the time of the middle of the day (important for resting)

  ;; patch sets:
  forest_set
  matrix_set
  border_patches


  ;; INPUT ;;
  ;gis;
  bb-gis      ; raster (.asc) file for defining patch size (10 x 10 m)
  bb-gis-shp  ; shapefile for drawing the fragment and defining habitat/non-habitat patches
  guarei-dataset
  shape-type
  property-names
  feature-list
  vertex-lists
  tree-file ; filename with the tree location and type
  sleep-file ; filename with the location of all sleeping sites


  ;param values;
;  gut_transit_time ; amount of timesteps until the tamarin defecates (time the seed takes to go throught all the digestive system)
;  travel_speed ; global speed for travel
;  species_time ; how long the tamarin feeds on the tree species
  energy_species ; value of energy they get from feeding of each species

  ;; OUTPUT ;;
  local-path  ; path for the model to run in different CPUs
  output-locations ; base filename for the monkey locations
  output-seeds-locations ; base filename for the seed locations
  output-rest-locations ; base filename for data from the simulated resting trees
  output-sleep-locations ; base filename for data from the simulated sleeping trees
  output-trees-locations ; base filename to check the geo coordinates for feeding trees
]




;--------------------------------------------------------------------------------
; SETTING UP
;--------------------------------------------------------------------------------
to setup
  clear-all

  if USER = "Ronald"
  [ set local-path "/home/rbialozyt/BLT_IBM-Model/" ]
  if USER = "Eduardo"
  [ set local-path "D:/Data/Documentos/github/BLT_IBM-Model" ]
  if USER = "LEEC"
  [set local-path "D:/EDUARDO_LAP"]
  if USER = "Others"
  [ set local-path "~/" ]


  setup-patches
  setup-gis
  get-patch-scale
  setup-trees
  setup-monkeys

  output-files

  create-legend

  set day 1
  set midday 58
  set timestep 0
  set gut_transit_time round (gut_transit_time)
;  set travel_speed travel_speed

  reset-ticks
end

; PATCHES
to setup-patches
;  ask patches [set pcolor yellow + 4]
end

; GIS
to setup-gis
  set-patch-size 3

  if study_area = "Guareí" [
    ; load .prj and .asc (raster 10 x 10 m)
    gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei-poligono.prj" ; WGS_1984_UTM_Zone_22S
    set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei-poligono2_reproj.asc" ; fragment/study area raster (reprojected***)

    ; load the poligon (.shp) to determine forest and matrix patches
    set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp" ; fragment/study area polygon
  ]


  if study_area = "Suzano" [
    ; load .prj and .asc (raster 10 x 10 m)
    gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.prj" ; WGS_1984_UTM_Zone_22S
    set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_rasterized_reproj.asc" ; fragment/study area raster (reprojected***)

    ; load the poligon (.shp) to determine forest and matrix patches
    set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp" ; fragment/study area polygon
  ]


  if study_area = "Taquara" [ ;;
    set-patch-size floor (0.8 * patch-size) ; Taquara large raster is too big for the world

    ; load .prj and .asc (raster 10 x 10 m)
    gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only4_rec_rasterized_reproj.prj" ; WGS_1984_UTM_Zone_22S
    set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only4_rec_rasterized_reproj.asc" ; fragment/study area raster (reprojected***)

    ; load the poligon (.shp) to determine forest and matrix patches
    set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp" ; fragment/study area polygon
  ]


  if study_area = "Santa Maria" [
    set-patch-size floor (1 * patch-size) ; Santa Maria large raster results in a large world

    ; load .prj and .asc (raster 10 x 10 m)
    gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_recortado_rasterized_reproj.prj" ; WGS_1984_UTM_Zone_22S
    set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_recortado_rasterized_reproj.asc" ; fragment/study area raster (reprojected***)

    ; load the poligon (.shp) to determine forest and matrix patches
    set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp" ; fragment/study area polygon
  ]


  ; make each raster cell = patch in NetLogo
  let width floor (gis:width-of bb-gis / 2)
  let height floor (gis:height-of bb-gis / 2)
  resize-world (-1 * width ) width (-1 * height ) height

  gis:set-world-envelope gis:envelope-of bb-gis
  gis:apply-raster bb-gis habitat

  gis:set-drawing-color black
  gis:draw bb-gis-shp 1

  ;; define habitat and border patches based on .shp (ref: PatchSize.nlogo model in Agent-Based Modelling and Geographical Information Systems: A Practical Primer)
  set forest_set patches gis:intersecting bb-gis-shp
  ask forest_set [
    set pcolor lime + 3
    set habitat "forest"
  ]

  set matrix_set patches with [habitat != "forest"]
  ask matrix_set [
    set pcolor yellow + 4
    set habitat "matrix"
  ]

  set border_patches patches with [ habitat = "forest" AND count neighbors with [habitat = "matrix"] >= 1]
  ask border_patches [
    ;    set pcolor red
    ;    set border? TRUE
    set habitat "border"
  ]

;  testing if it has worked:
;  ask one-of monkeys [ print any? patches with [ habitat = "" ] ]


end

to get-patch-scale
  create-blobs 1

  ask one-of blobs [
    move-to patch 0 0

    set x_UTM (item 0 gis:envelope-of self)
    set y_UTM (item 0 gis:envelope-of self)
    set patch_before patch-here

    move-to patch 1 0

    let x_UTM2 (item 0 gis:envelope-of self)
    let y_UTM2 (item 0 gis:envelope-of self)

    set patch-scale ( x_UTM2 - x_UTM )
    die
  ]
end

; TREES INPUT
to setup-trees


  ;; INPUT SLEEPING TREES OF ALL STUDY PERIOD INDEPENDENT OF MONTH:

  let id-tree-slp 0
  if ( sleeping-trees-scenario = "empirical" AND all-slp-trees? = TRUE )  [
    if ( study_area = "Guareí")  [ set sleep-file word ( local-path) "/Data/Resource-Trees/guarei_trees_unique_slp.shp" ]
    if ( study_area = "Santa Maria")  [ set sleep-file word ( local-path) "/Data/Resource-Trees/sma_trees_unique_slp.shp" ]
    if ( study_area = "Suzano")  [ set sleep-file word ( local-path) "/Data/Resource-Trees/suz_trees_unique_slp.shp" ]
    if ( study_area = "Taquara")  [ set sleep-file word ( local-path) "/Data/Resource-Trees/taq_trees_unique_slp.shp" ]


    let sleep-gis gis:load-dataset sleep-file ; defined by tree-scenario chooser
    foreach gis:feature-list-of sleep-gis [ vector-feature ->
      let location-slp gis:location-of (first (first (gis:vertex-lists-of vector-feature)))
      set id-tree-slp gis:property-value vector-feature "behavior"

      create-sleeping-trees 1 [
        set size 3
        set shape "tree"
        set color magenta
        setxy item 0 location-slp item 1 location-slp
        set id-tree-slp gis:property-value vector-feature "id"
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
  ]]]


  ;;;;;;; load tree-file according to tree-scenario chooser (.SHP) ;;;;;;;

  ; Guareí
  if ( study_area = "Guareí" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/guarei_trees_unique_all.shp" ]
  if ( study_area = "Guareí" AND feeding-trees-scenario = "May" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/guarei_trees_unique_May.shp" ]
  if ( study_area = "Guareí" AND feeding-trees-scenario = "Jun" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/guarei_trees_unique_Jun.shp" ]
  if ( study_area = "Guareí" AND feeding-trees-scenario = "Jul" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/guarei_trees_unique_Jul.shp" ]
  if ( study_area = "Guareí" AND feeding-trees-scenario = "Aug" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/guarei_trees_unique_Aug.shp" ]

  ; Santa Maria
  if ( study_area = "Santa Maria" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/sma_trees_unique_all.shp" ]
  if ( study_area = "Santa Maria" AND feeding-trees-scenario = "Mar" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/sma_trees_unique_Mar.shp" ]
  if ( study_area = "Santa Maria" AND feeding-trees-scenario = "Apr" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/sma_trees_unique_Apr.shp" ]
  if ( study_area = "Santa Maria" AND feeding-trees-scenario = "May" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/sma_trees_unique_May.shp" ]

  ; Taquara
  if ( study_area = "Taquara" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_all.shp" ]
  if ( study_area = "Taquara" AND feeding-trees-scenario = "Jan" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_Jan.shp" ]
;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Feb" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_Feb.shp" ]
;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Apr" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_Apr.shp" ]
;  if ( study_area = "Taquara" AND feeding-trees-scenario = "May" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_May.shp" ]
;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Jul" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_Jul.shp" ]
;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Sep" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_Sep.shp" ]
;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Dec" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/taq_trees_unique_Dec.shp" ]

  ; Suzano
  if ( study_area = "Suzano" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/suz_trees_unique_all.shp" ]
  if ( study_area = "Suzano" AND feeding-trees-scenario = "Feb" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/suz_trees_unique_Feb.shp" ]
  if ( study_area = "Suzano" AND feeding-trees-scenario = "Apr" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/suz_trees_unique_Apr.shp" ]
  if ( study_area = "Suzano" AND feeding-trees-scenario = "Sep" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/suz_trees_unique_Sep.shp" ]
  if ( study_area = "Suzano" AND feeding-trees-scenario = "Dec" )   [ set tree-file word ( local-path) "/Data/Movement/Resource-Trees/suz_trees_unique_Dec.shp" ]


  let number 0
  let xcoord 0
  let ycoord 0
  let tree-type 0 ; phenology types

;  procedure to check if the file exists (not working):
;  let direct_list sort pathdir:list word (local-path) "/Data/Movement/Resource-Trees/"
;  let local-path "D:/Data/Movement/Documentos/github/BLT_IBM-Model" let direct_list sort pathdir:list word (local-path) "/Data/Movement/Resource-Trees/"     ( foreach [direct_list] [ i -> read-from-string ( item i direct_list) ] )
;  ifelse file-exists? ( map [ i -> read-from-string direct_list ] ) [


  ifelse tree-file != 0 [
  let trees-gis gis:load-dataset tree-file ; defined by tree-scenario chooser
  foreach gis:feature-list-of trees-gis [ vector-feature ->
    let location gis:location-of (first (first (gis:vertex-lists-of vector-feature)))
    set tree-type gis:property-value vector-feature "behavior"

    if ( feeding-trees? = TRUE AND tree-type = "Frugivory" ) [
      create-feeding-trees 1 [
        set size 3
        set shape "tree"
        set color green
        set visitations 0
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "id"
        if species = "" [ set species "NA" ]
        if id-tree = "" [ set id-tree "NA" ]
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
    ]];


    if ( sleeping-trees? = TRUE AND all-slp-trees? = FALSE AND tree-type = "Sleeping site"  ) [
      create-sleeping-trees 1 [
        set size 3
        set shape "tree"
        set color magenta
        set visitations 0
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "id"
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
    ]] ;
  ]



  ][
    print "NO TREES FOR THIS MONTH! CHOOSE ANOTHER MONTH!"
  ]

;  ;; DOES NOT SEEM TO BE NEEDED (LOOK AT BORDER TREES IN SUZANO)
;;   do this for tamarins not to avoid approaching trees close to border (avoid-matrix)
;  ask feeding-trees [
;    ask patches in-radius travel_speed with [habitat = "matrix" ] [
;      set habitat "border"
;      set pcolor brown
;    ]
;  ]
;
;
;  ask sleeping-trees [
;   ask patches in-radius travel_speed with [habitat = "matrix" ] [
;      set habitat "border"
;      set pcolor brown
;    ]
;  ]

end


; TAMARINS
to setup-monkeys

  create-monkeys 1
  ask monkeys [

    ; for the behaviorsequence plot
    set behaviorsequence []

    ; for home range calculation with the r extension
    set Name word "BLT_" ( [who] of self )
    set X_coords ( list x_UTM )
    set Y_coords ( list y_UTM )

    ;;    set shape "banana"
;    set shape "mlp-2"
;    set color black
    set size 1.5

    if sleeping-trees-scenario = "empirical" AND sleeping-trees? = FALSE [
      setxy random xcor random ycor
      set tree_current -1
    ]

    if sleeping-trees-scenario = "empirical" AND sleeping-trees? = TRUE [
      let start one-of sleeping-trees
      setxy [xcor] of start [ycor] of start
      set tree_current start
    ]

    set x_UTM (item 0 gis:envelope-of self)
    set y_UTM (item 2 gis:envelope-of self)

    set travel_mode "short_distance"
    set tree_target -1
    set ld_tree_target -1
    set patch_avoid_matrix patch-ahead step_len_travel
    set straight-line-to-target? TRUE ; this should be checked (travel procedure)

    set steps-moved 0
    set action-time 0
    set action "travel"
    set behavior ""
    set going-sleeping? FALSE
    set energy start-energy

    ; create empty lists
    set tree_ate_list []
    set tree_mem_list []
    set tree_add_list []
    set tree_pot_list []
    set seed_ate_list []
    set seed_mem_list []
    set seed_add_list []
    set travelmodelist []

    set DPL_d []

    ; fill the potential feeding tree list
    let let_pot_list []
    ask feeding-trees [
      set let_pot_list lput who let_pot_list
    ]
    set tree_pot_list let_pot_list

    ifelse show-energy? [
      set label round energy
      set label-color black
    ]
    [set label ""]
    ifelse show-path? [
      set pen-mode "down" ; alternative: https://groups.google.com/g/netlogo-users/c/qEf4yS0AAeQ
      set pen-size 1
      set color gray
;;      set Color blue
    ][ set pen-mode "up" ]
  ] ; end ask monkeys

  if step-model-param? = TRUE [
    ;; Parameterizing BLT velocity with empirical data:
    ;option 1 (= mean velocities, Zanette et al 2021 ATBC) (I believe this underestimates tamarin velocities as our model only uses travelling speed
    ;  if study_area = "Guareí" [ set travel_speed ( 15.27 / 10 ) ]   ; 15.27 m / 10 m ( BLT mean velocity / patch resolution)
    ;  if study_area = "Santa Maria" [ set travel_speed ( 18.4 / 10 ) ]
    ;  if study_area = "Taquara" [ set travel_speed ( 23.24 / 10 ) ]
    ;  if study_area = "Guareí" [ set travel_speed ( 8.93 / 10 )  ]

;    ;option 2 (= 3 steps before feeding on fruits, Zanette et al 2021 ATBC)
;    if study_area = "Guareí" [ set travel_speed ( 20 / 10 ) ]   ; 15.27 m / 10 m ( BLT mean velocity / patch resolution)
;    if study_area = "Santa Maria" [ set travel_speed ( 20.2 / 10 ) ]
;    if study_area = "Taquara" [ set travel_speed ( 33.4 / 10 ) ]
;    if study_area = "Suzano" [ set travel_speed ( 13.7 / 10 )  ]
;  ]

    ;option 3 (= Data/Parameter-table.csv)
; travel
if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set step_len_forage ( 23.43 / 10 ) ]   ; 15.27 m / 10 m ( BLT mean velocity / patch resolution)
if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set step_len_forage ( 25.44 / 10 ) ]
if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set step_len_forage ( 25.20 / 10 ) ]
if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set step_len_forage ( 25.30 / 10 ) ]

if study_area = "Santa Maria" AND feeding-trees-scenario = "Mar"[ set step_len_forage ( 32.37 / 10 ) ]
if study_area = "Santa Maria" AND feeding-trees-scenario = "Apr"[ set step_len_forage ( 35.97 / 10 ) ]

if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set step_len_forage ( 17.94 / 10 )  ]
if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set step_len_forage ( 17.49 / 10 )  ]

if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set step_len_forage ( 39.31 / 10 ) ]

; foraging
if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set step_len_forage ( 14.06 / 10 ) ]  ; ( BLT mean velocity / patch resolution)
if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set step_len_forage ( 12.14 / 10 ) ]
if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set step_len_forage ( 12.93 / 10 ) ]
if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set step_len_forage ( 13.87 / 10 ) ]

if study_area = "Santa Maria" AND feeding-trees-scenario = "Mar"[ set step_len_forage ( 16.95 / 10 ) ]
if study_area = "Santa Maria" AND feeding-trees-scenario = "Apr"[ set step_len_forage ( 21.3 / 10 ) ]

if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set step_len_forage ( 7.51 / 10 )  ]
if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set step_len_forage ( 8.83 / 10 )  ]

if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set step_len_forage ( 30.89 / 10 ) ]

  ]

end

; LEGEND
to create-legend

  create-legend-trees 1 [ ; tamarin
    set size 4
    ;;      set shape "banana"
    set color black
    setxy min-pxcor + 4 max-pycor - 5
  ]
  create-legend-trees 1 [ ; feeding
    set size 4
    set shape "tree"
    set color green
    setxy min-pxcor + 4 max-pycor - 10
  ]
  create-legend-trees 1 [ ; sleeping
    set size 4
    set shape "tree"
    set color magenta
    setxy min-pxcor + 4 max-pycor - 15
  ]
  create-legend-trees 1 [ ; seeds
    set size 3
    set shape "circle"
    set color black
    setxy min-pxcor + 4 max-pycor - 20
  ]
  create-legend-trees 1 [ ; short distance target tree
    set size 4
    set shape "tree"
    set color red
    setxy min-pxcor + 2 min-pycor + 5
  ]
  create-legend-trees 1 [ ; long distance target tree
    set size 4
    set shape "tree"
    set color blue
    setxy min-pxcor + 2 min-pycor + 10
  ]

end

to output-files
  if output-files? = TRUE [
    ;; OUTPUT FILE NAMES
    set output-locations word ( runtime ) "locations_monkey.txt"               ;; word (date-and-time "_" "e-" start-energy) for adding the day and start-energy to the filename
    set output-seeds-locations word ( runtime ) "locations_seeds.txt"
    set output-trees-locations word ( runtime ) "locations_trees.txt"
    set output-rest-locations word ( runtime ) "locations_rest.txt"
    set output-sleep-locations word ( runtime ) "locations_sleep.txt"

    ;; CHECK MILLES ET AL 2020 FOR NOT NEEDING TO DELETE FILES
    if ( file-exists? output-locations ) [ file-delete output-locations ]
    if ( file-exists? output-seeds-locations ) [ file-delete output-seeds-locations ]
    if ( file-exists? output-trees-locations ) [ file-delete output-trees-locations ]
    if ( file-exists? output-rest-locations ) [ file-delete output-rest-locations ]
    if ( file-exists? output-sleep-locations ) [ file-delete output-sleep-locations ]

    ;; DEFINE OUTPUT FILE HEADERS
    file-open output-locations
    ;  file-print (word "ticks" "," "day" "," "timestep" "," "x" "," "y" "," "energy" "," "action" "," "behavior) ;; FOR CSV
    file-print (word " " "ticks" " " "day" " " "timestep" " " "x" " " "y" " " "energy" " " "action" " " "behavior") ;; FOR TXT
    file-close

    file-open output-seeds-locations
    ;  file-print ("id-seed" "," ) ;; FOR CSV
    ;  file-print (word " " "id-seed" " " "x" " " "y" " " "species" " " "mother-tree") ;; FOR TXT
    file-close

    file-open output-trees-locations
    ;  file-print ("x" "," ) ;; FOR CSV
    file-print (word " " "x" " " "y" " " "species" " " "id-tree") ;; FOR TXT
    file-close

    file-open output-rest-locations
    ;  file-print ("x" "," ) ;; FOR CSV
    file-print (word " " "x" " " "y" " " "species" " " "id-tree") ;; FOR TXT
    file-close

    file-open output-sleep-locations
    ;  file-print ("x" "," ) ;; FOR CSV
    file-print (word " " "x" " " "y" " " "species" " " "id-tree") ;; FOR TXT
    file-close
  ]

end



;--------------------------------------------------------------------------------------------
; Activities commands
;--------------------------------------------------------------------------------------------
to go

  if ticks > 10000 [stop]

  if all? monkeys [action = "sleeping"] [
    set day day + 1
    set timestep 0
    ask monkeys [ set action "" ]
    if day > no_days [
      output-print "run-days click finished"
      output-print "calculating home range with r extension"
      calc-homerange
      output-print "home range calculation with r extension finished"
      stop
    ]
  ]

  move-monkeys

;  ask monkeys [ set behaviorsequence lput behavior behaviorsequence ]

  set timestep timestep + 1
  tick
  if output-files? = TRUE [
    write-to-file ;; WRITE-FILE IS CALLED AGAIN IN next_day() AND step()
  ]

  ; create a gif (adapted from Milles et al 2020)
  if export-png = TRUE [
    let file-id random -1
    let world-name (word runtime no_days "_" "e-" start-energy "_" file-id "timestep" ticks "_world.png") ; date-and-time
    export-view world-name
    ask monkeys [
      if behavior = "sleeping" [
        set world-name (word runtime no_days "_" "e-" start-energy "_" file-id "_interface.png") ; date-and-time
        export-interface world-name
      ]
    ]
  ]

  if not any? monkeys [ stop ]

;  if simulation-time-end = TRUE [
;    output-print "AHOY"
;    print "AHOY"
;  ]


end


to step ; FOR DEBUG PURPOSES ONLY

  if all? monkeys [action = "sleeping"] [
    ask monkeys [
      set action-time 0
      type timestep type " - Energy: " type energy type " "
      type tree_target  type " " show action
      ]
    set day day + 1
    set timestep 0
    ask monkeys [ set action "travel" ]
    stop
  ]

  repeat 1 [ move-monkeys ]
  set timestep timestep + 1
  tick
  if output-files? = TRUE [ write-to-file ]

  ;; DEBUGGING
  if print-step? = TRUE [
    ask monkeys [
      type "---- STEP ---- " print timestep
      type " ---- MODE: " print travel_mode
      type "tree_target: " type tree_target type " "
      type "ld_tree_target: " type ld_tree_target type " "
      type "tree_current: " type tree_current type " "
      type "behavior: " type behavior type " "
      type "action: " print action
;      type "tree_pot_list: " print length tree_pot_list print tree_pot_list
      type "tree_ate_list: " print length tree_ate_list print tree_ate_list
;      type "tree_mem_list: " print length tree_mem_list print tree_mem_list
      ;    type "tree_add_list " print length tree_add_list print tree_add_list
      type "action-time: " print action-time
      type "energy: " print energy
      type "x: " print x_UTM
      type "y: " print y_UTM
      if tree_target != -1 [
        type "distance: " print distance tree_target
        type "target_species: " print tree_target_species
      ]
      type "DPL_d: " print DPL_d
      type "DPL (m): " print DPL * 10
      type "dist-traveled: " print dist-traveled
;      ifelse travel_mode = "short_distance" [
;        ; print distance tree_target
;      ][
;        print distance ld_tree_target
;      ]
      print " ----------------step end------------------------"
    ]
  ]

end


;-------------------------------------------------------------
to run_days


  repeat no_days [ next_day ]

  if output-files? = TRUE [
    write-seeds
    write-rest
    write-sleep
    write-trees
  ]
end


;-------------------------------------------------------------
to next_day

  ;; DEBUGGING
  output-type "===== Day: "
  output-type day
  output-print " ====="
;  output-type "*simulation-time* "
;  output-type ticks
;  output-print " ----"


  ask monkeys [
;    output-type "action-time "
;    output-print action-time
    output-type "energy "
    output-print energy
  ]


  loop [
    if all? monkeys [action = "sleeping"] [
      ask monkeys [
        if path-color-by-day? = TRUE [
          let color-days palette:scale-gradient palette:scheme-colors "Divergent" "Spectral" 10 no_days 0 9 ;; Spectral
;          let color-days palette:scale-gradient [[255 0 0] [0 255 0] [0 0 255]] no_days 0 9               ;; RGB
;          let color-days palette:set-alpha random 100              ;; manually choosen
          set color one-of color-days
        ]
        set action-time 0
        set frugivory-time 0
        type timestep type " - Energy: " type energy type " "
        type tree_target  type " " show action
      ]
      set day day + 1
      set timestep 0
      ask monkeys [ set action "travel" ]
      stop
    ]
    go

  ]


  ; export the landscape as a .png if neccessary  (= Milles et al 2020)
  if day = no_days AND export-png = TRUE [
   let file-id random -1
   let world-name (word runtime no_days "_" "e-" start-energy "_" file-id "_world.png") ; date-and-time
   export-view world-name
;   export-interface world-name
  ]

end



;-------------------------------------------------------------------------------

to move-monkeys

  ask monkeys
  [
    set dist-traveled 0

    ifelse show-energy? [
      set label round energy
      set label-color black
    ]
    [ set label "" ]


    if energy < 1 [ die ]

    set x_UTM (item 0 gis:envelope-of self)
    set y_UTM (item 2 gis:envelope-of self)

    ; for home range calculation with r extension:
    set X_coords lput x_UTM X_coords
    set Y_coords lput y_UTM Y_coords


    ; Avoid matrix (for other implementations, check v1.1_matrix-avoidance branch):
;    avoid-matrix



  ;; BLT ROUTINE

    if timestep = 0 [
      set energy start-energy ; we want the tamarins to flutuate between level 1 and 2; only take this out if you calibrate the energy values
      set tree_current -1
      set DPL 0 ; set daily path length to 0 every day
      set going-sleeping? FALSE
      remove_trees_surrounding ; to avoid feeding in the closest tree
    ]

    if timestep = 1 [
      morning-defecation
    ]

    if timestep >= simulation-time [
      if timestep = simulation-time [
        set tree_target -1
        set going-sleeping? TRUE
      ] ; force monkey select a sleeping site
      sleeping
      if output-print? = TRUE [
        output-day-stats
      ]

    ]


    ; Trying to make monkeys go to sleep only if energy > energy_level 1:
;    if timestep >= simulation-time AND energy > energy_level_1 [
;      set tree_target -1
;      sleeping
;
;      if output-print? = TRUE [
;        output-day-stats
;      ]
;    ]


    if timestep < simulation-time [ ; energy levels: energy_level_1 = 80 and energy_level_2 = 150
      ; modulate memory_list
      enhance_memory_list

      ; keep track of distance to the target
      if tree_target != -1 [ set tree_target_dist distance tree_target ]

      ifelse energy < energy_level_1 [ ; energy < level 1
        set travel_mode "short_distance"
        if ld_tree_target = tree_target [
          set tree_target -1 ; remove tree_target when coming from "long_distance"
          set ld_tree_target -1  ; if this is not here it will make the tamarin lose the target very close to the tree when coming from long distance bc of the condition ld_tree_target = tree_target (Ronald debugged on the 14th of July 2022)
        ]
        frugivory
      ][ ; energy > level 1
;        ifelse (action = "feeding" or action = "travel") [              ;; v1.0 version
        ifelse (travel_mode = "short_distance" ) [                       ;; v1.1 version (long and short-distance travel)
          ifelse energy > energy_level_2 [ ; energy > level 2 ==> other activities
            if tree_current = -1 [
             set travel_mode "long_distance"
            ]
            ifelse (timestep > (midday - 10) and timestep < (midday + 10)) [
              resting
            ][
              random-action
            ]
          ][ ; energy_level_1 < energy < energy_level_2
            frugivory
          ] ;; energy > level 2 ==> other activities
        ][ ; travel_mode = "long_distance"


          ifelse random (2 * duration) < action-time [ ; action time for other than feeding
            ; set ld_tree_target -1  ;; commented out by Ronnald on 14th of July ;; has to be set to -1 otherwise tamarins will come back in the next ld travel cycle
;            print " *********** 2"
            frugivory
          ][
;            print " *********** 1"
            set action-time action-time + 1
            last-action-again
          ]

        ]
      ] ;; energy > level 1
      forget_trees
      defecation

      set DPL DPL + dist-traveled

    ] ; end of daily routine
] ; end ask monkeys
end



;--------------------------------------------------------------------------------
; matrix avoidance
;--------------------------------------------------------------------------------
to avoid-patch-set

  ; test:
  ; print count patches with  [ any? neighbors with [ habitat = "border"] ]

  ; make patch_avoid_matrix a new one every step if tamarins are close to it (it can be the same)
  if patch_avoid_matrix = nobody OR patch_avoid_matrix = 0 OR distance patch_avoid_matrix < (2 * step_len_travel ) [
    set patch_avoid_matrix nobody
  ]

  ; make line straight TRUE (= initial setup)
  set straight-line-to-target? TRUE

  if tree_target != -1 [
    set tree_target_mem tree_target
    ;    if ( count patches with [ any? neighbors with [ habitat = "border"] ] > 0 OR count patches with [ any? neighbors with [ habitat = "matrix" ] ]  > 0 ) [
    ;  while [ [habitat] of patch-ahead (2 * travel_speed) != "forest" AND [habitat] of patch-ahead (3 * travel_speed) != "forest" ] [
;    if ( [habitat] of patch-ahead (2 * travel_speed) = "matrix" AND [habitat] of patch-ahead (3 * travel_speed) = "matrix" ) [


      ; define patch-set in front of tamarins (in Taquara 2 * 2.4 = 4.8 is their velocity when going to sleep, so add one more for reaching more than the maximum tamarins can move)
      set front_patches patches in-cone (2 * step_len_travel + 1) ( 60 ) ; with [ habitat = "matrix" ] ; cone
      ;set front_patches patches with [habitat = "forest"] in-radius 5 ; in radius

      ;paint them
      if ( any? front_patches with [habitat = "matrix"] ) [
      ask front_patches with [ habitat = "matrix" ] [ set pcolor yellow - 2]
;        ask patch-ahead (2 * travel_speed) [ set pcolor yellow ]
;        ask patch-ahead (3 * travel_speed) [ set pcolor cyan ]

      ; set this for agents not to run the travel procedure and walk again
;      print "SETTING FALSE"
      set straight-line-to-target? FALSE
    ]

      ;paint them magenta:
;      ask front_patches with [habitat = "matrix" ] [ set pcolor magenta + 3 ]

      ; make tamarins "come back" from the matrix
    if ( all? patches in-radius 3 [habitat = "matrix"] ) [
      avoid-full-matrix
    ]

      ; ; make tamarins avoid entering matrix
    if ( any? front_patches with [habitat = "matrix"] ) [
      avoid-some-matrix
    ]


  ]


end


to avoid-full-matrix

  print "I'm in the middle of the sugarcane!"
  ;    set color "red"

  ; choose closest patch within radius that is not matrix
  let patch-radius patches in-radius 3 ;( 2 * travel_speed )
  set patch_avoid_matrix min-one-of patch-radius with [ habitat != "matrix" ] [distance myself]
  ; if there's no patch in such radius, then choose the closest one that is not matrix independently of radius
  if patch_avoid_matrix = nobody OR [habitat] of patch_avoid_matrix = "matrix" [
    set patch_avoid_matrix min-one-of patches with [ habitat != "matrix" ] [distance myself]
  ]
  ; go to the choosen patch of forest:
  ;    move-to patch_avoid_matrix

  ; set this for agents not to run the travel procedure and walk again
  ;    print "SETTING FALSE"
  ;    set straight-line-to-target? FALSE

end


to avoid-some-matrix
;  if ( any? front_patches [habitat = "matrix"] ) [
    print "there's some matrix in front of me!"
;    set color orange

    let d tree_target_mem ; d is a local variable and tree_target_mem is a monkey variable; the patch_avoid_matrix can't be set because it is within the patch context and patche can't access monkey variables

    set candidate_patches patch-set patches with [habitat != "matrix"] in-radius 5 ; or in-cone ( 2 * travel_speed ) 90
                                                                                   ; if there are no candidate patches, go to the closest non-matrix patch regardless of distance to target
    ifelse candidate_patches = nobody OR candidate_patches = 0 [

      ;    set patch_avoid_matrix min-one-of patches with [ habitat != "matrix" ] [distance myself]
      set patch_avoid_matrix one-of ( ( (candidate_patches ) with [ habitat != "matrix"] ) with-min [distance d] )

    ][
      ask candidate_patches [ set pcolor green + 2 ]
      ;    type "CANDIDATE PATCHES: " print candidate_patches
      ;    type "distance of patches to monkey target: " ask candidate_patches [ print distance d ]

      ;  set patch_avoid_matrix min-one-of candidate_patches [distance monkey_target]
      set patch_avoid_matrix one-of ( ( (candidate_patches ) with [ habitat != "matrix"] ) with-min [distance d] )        ; from https://stackoverflow.com/questions/70036380/netlogo-creating-variable-from-distance-to-specific-patch
                                                                                                                          ;      type "MONKEY TARGET : " print tree_target_mem
                                                                                                                          ;    type "patch_avoid_matrix :" print patch_avoid_matrix
    ]

    ; it might be the case that there's no patch_avoid_matrix. In this case, choose the closest non-matrix patch regardless of distance to target
    if patch_avoid_matrix = nobody OR [habitat] of patch_avoid_matrix = "matrix" OR patch_avoid_matrix = 0 [
;      print "no patches"
      set patch_avoid_matrix min-one-of patches with [ habitat = "forest" ] [distance myself]
    ]
    ;      print distance tree_target_mem

    ask patch_avoid_matrix [ set pcolor red ]
;    type "patch_avoid_matrix :" print patch_avoid_matrix
    ;      pen-up
    ;  move-to patch_avoid_matrix
    ;      pen-down

    ; set this for agents not to run the travel procedure and walk again
    ;  print "SETTING FALSE"
    ;  set straight-line-to-target? FALSE
    ;  set color grey
;  ]
end

;--------------------------------------------------------------------------------
; the whole loop for frugivory
;--------------------------------------------------------------------------------
to frugivory

  print "frugivory"  ; debugging

  avoid-patch-set ; bump on the territory borders


  if travel_mode = "short_distance" [   ;; short distance frugivory
    set travelmodelist lput 1 travelmodelist ; to the travel mode histogram
    ifelse on-feeding-tree? [
      ifelse random (2 * species_time ) > frugivory-time [
;        print "I'm feeding!" ; for debugging
        feeding
      ][
        set tree_current -1
;        print "time over -- New feeding tree" ; for debugging
        to-feeding-tree
      ]
    ][
;      print "New feeding tree" ; for debugging
      to-feeding-tree
    ]
  ]

  if travel_mode = "long_distance" [    ;; long distance frugivory
    set travelmodelist lput 2 travelmodelist ; to the travel mode histogram
    ifelse on-feeding-tree? [
      ifelse random (2 * species_time ) > frugivory-time [
;        print "I'm feeding!" ; for debugging
        feeding
      ][
        set tree_current -1
;        print "time over -- New feeding tree" ; for debugging
        to-feeding-tree
      ]
    ][
;      print "New long distance feeding tree" ; for debugging
      to-feeding-tree
    ]
  ]

end


;----------------------------------------

to-report on-feeding-tree?


  if travel_mode = "short_distance" [   ;; short distance frugivory
    ifelse action = "travel" OR action = "forage" AND tree_target != -1 [
;      type "on-feeding-tree? reporter distance to target : "
      print distance tree_target ; for debugging
      ifelse distance tree_target < 0.8 * step_len_travel [

;        print "HERE ***************"

        set tree_current tree_target

        set dist-traveled dist-traveled + distance tree_target
        move-to tree_target

        ; make UTM of tamarins match UTM of trees (like empirical data collection):
        set x_UTM [ x_UTM ] of tree_current
        set y_UTM [ y_UTM ] of tree_current
        ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
        set xcor [xcor] of tree_current + 0.01
        set ycor [ycor] of tree_current + 0.01

        ask tree_target [ set visitations visitations + 1 ]
        set tree_target -1
        ifelse feedingbout-on?
        [ set species_time [ species_time ] of tree_current ]
;        [ set species_time duration ] ;; duration = 2 is the most common value over all species, but as there's a random variation on the 'random (2 * species_time), I'll leave it as the same as duration
        [ set species_time 2 ]
  ;      type "tree_current: " print tree_current
  ;      type "tree_target: " print tree_target
;        print "on-feeding-tree? TRUE" ; for debugging
        print "ON tree"
        report true

      ][
;        print "on-feeding-tree? FALSE" ; for debugging
  ;      print tree_target
        print "NOT on tree"
        report false
      ]
    ][
;      print "Action != travel OR tree_target = -1" ; for debugging
      ifelse action = "feeding" [
        print "ON tree"
        report true
      ][
        print "NOT on tree"
        report false
      ]
    ]
  ]


  if travel_mode = "long_distance" [    ;; long distance frugivory

    ifelse action = "travel" OR action = "forage" AND ld_tree_target != -1 [
;      type "on-feeding-tree? reporter distance to target : "
      print distance ld_tree_target ; for debugging
      ifelse distance ld_tree_target < 0.8 * step_len_travel [
        print "distance to ld_tree_target is < 80%"

        set tree_current ld_tree_target

        set dist-traveled dist-traveled + distance ld_tree_target
        move-to ld_tree_target

        set x_UTM [ x_UTM ] of tree_current
        set y_UTM [ y_UTM ] of tree_current
        ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
        set xcor [xcor] of tree_current + 0.01
        set ycor [ycor] of tree_current + 0.01

        ask ld_tree_target [ set visitations visitations + 1 ]

        set tree_target -1
        set ld_tree_target -1
;        set tree_target ld_tree_target ;; IMPORTANT FOR NOT HAAVING TO CHANGE ALL THE FEEDING PROCESS
        ifelse feedingbout-on?
        [ set species_time [ species_time ] of tree_current ]
;        [ set species_time duration ] ;; duration = 2 is the most common value over all species, but as there's a random variation on the 'random (2 * species_time), I'll leave it as the same as duration
        [ set species_time 2 ]
        print "ON tree"
        report true
      ][
        print "NOT on tree"
        report false
      ]
    ][
;      print "Action != travel OR tree_target = -1" ; for debugging
      ifelse action = "feeding" [
        print "ON tree"
        report true
      ][
        print "NOT on tree"
        report false
      ]
    ]
  ]
end

;----------------------------------------

to feeding
  print "feeding"    ; debugging
  if travel_mode = "long_distance" [ print "FEEDING IS HAPPENING" ]

  set action "feeding"
  set behavior "frugivory"

  set behaviorsequence lput 1 behaviorsequence ;; activity budget

  set energy energy + energy-from-fruits + energy_species
  set frugivory-time frugivory-time + 1

  ; change mem list
  if( length tree_ate_list = 0 ) [
    ;; THIS REMOVES THE TREE THAT THE TAMARINS JUST ATE FROM THE POT LIST, THUS IT SHOULD STAY IN THE FEEDING COMAMND
    set tree_pot_list remove-item ( position [who] of tree_current tree_pot_list ) tree_pot_list
    set tree_ate_list lput [who] of tree_current tree_ate_list
    set tree_mem_list lput 0 tree_mem_list
    set tree_add_list lput 1 tree_add_list
    ; remove_trees_surrounding
  ]

  if tree_current = -1 [ print "========== tree_current = -1 ============" ]

  ifelse( member? [who] of tree_current tree_ate_list) [
    set tree_mem_list replace-item (length tree_mem_list - 1) tree_mem_list 0
    set tree_add_list replace-item (length tree_add_list - 1) tree_add_list 1
  ][
    ;; THIS REMOVES THE TREE THAT THE TAMARINS JUST ATE FROM THE POT LIST, THUS IT SHOULD STAY IN THE FEEDING COMAMND
    set tree_pot_list remove-item ( position [who] of tree_current tree_pot_list ) tree_pot_list
    set tree_ate_list lput [who] of tree_current tree_ate_list
    set tree_mem_list lput 0 tree_mem_list
    set tree_add_list lput 1 tree_add_list
    ;; HERE REMOVE ADDITIONAL TREES within the visual range of the tamarins (25m ?)
    remove_trees_surrounding
  ]

  ; consume seeds:
  set seed_ate_list lput [who] of tree_current seed_ate_list
  set seed_mem_list lput 0 seed_mem_list
  set seed_add_list lput 1 seed_add_list
end

;-----------------------------------------
to remove_trees_surrounding
  ;; HERE REMOVE ADDITIONAL TREES within the visual range of the tamarins (1 patch = 10 m)
  let trees_remove filter [ s -> member? s [ who ] of feeding-trees with [ distance myself < visual] ] tree_pot_list

;  ;; testing agentset selected by visual:
;  ; Option 1) (does not work properly)  (trees_remove is just the who number and require more code):
;  let trees_remove_agents feeding-trees with [ distance myself < visual]
;  type "+++++++TREES SURROUNDING: " print trees_remove_agents
;  type "======= trees_remove: " print trees_remove
;
;  foreach trees_remove [ x -> ask patch-here [ ask neighbors [ set pcolor red] ] ]
;
;   Option 2) (does not work) asking the agentset:
;  ask trees_remove_agents [
;    ask patch-here [ set pcolor red]
;  ]

  let l length trees_remove
;  print l ; for debugging
  while [ l > 0 ]
  [
    set l l - 1
    set tree_pot_list remove-item ( position (item l trees_remove) tree_pot_list ) tree_pot_list
    set tree_ate_list lput (item l trees_remove) tree_ate_list
    set tree_mem_list lput 0 tree_mem_list
    set tree_add_list lput 1 tree_add_list
  ]
end

to enhance_memory_list
    ;; make pot_list increase again if it is too small (otherwise will return an error) -> revisitation to trees is more common when primates are in small fragments (less trees availble) (Boyle et al 2009);
    ;; don't make prop_trees_to_reset_memory bigger than 8 otherwise the potential list will get very very small (high chances to return an error)
  let n_trees round ( count feeding-trees  / prop_trees_to_reset_memory ) - 2 ; don't know what should be the number exactly. The smaller it is, more the tamarins will travel around to find other trees in the pot_list while avoiding returning to visited trees ;
  if ( length tree_pot_list <= n_trees ) [
    let tree_bucket sublist tree_ate_list ( 0 ) ( n_trees )
;    print tree_bucket

    ; enhance potential list
    ( foreach tree_bucket [ x -> set tree_pot_list lput x tree_pot_list ] )

    ; reduce mem_list and add_list
    set tree_mem_list sublist tree_mem_list ( n_trees ) ( length tree_mem_list)
    set tree_add_list sublist tree_add_list ( n_trees ) ( length tree_add_list)
    set tree_ate_list sublist tree_ate_list ( n_trees ) ( length tree_ate_list)
  ]

end


;-----------------------------------------

to to-feeding-tree

  print "TO-FEEDING-TREE"


  if travel_mode = "short_distance" [
    if tree_target = -1 [
      set frugivory-time 0
      search-feeding-tree
    ]

    ;    if on-feeding-tree? = FALSE [ set heading towards tree_target ] ; to avoid the same point (x,y) error
    if straight-line-to-target? = TRUE [ ; otherwise it might enter the matrix
      set heading towards tree_target
    ]


    ifelse ( action = "travel" OR action = "forage" ) [

      ifelse ( random-float 1 < p-foraging-while-traveling ) [

        if tree_target != -1 AND distance tree_target > 0.8 * step_len_travel [ ; otherwise it migh forage for 3 sequential steps while arriving in the feeding tree
          forage
          ;; RANDOM movement while foraging:
          if step-model-param? = TRUE  AND distance tree_target > 0.8 [
            rt ( random max_rel_ang_forage_75q ) - ( random max_rel_ang_forage_75q )  ; feeding is less directed than travel
          ]
          travel
          set action "travel"
          set behavior "travel"
          set behaviorsequence lput 3 behaviorsequence
        ]

      ][

        if step-model-param? = TRUE  AND distance tree_target > 0.8 [
          rt ( random max_rel_ang_travel_75q ) - ( random max_rel_ang_travel_75q)  ; travel is more directed than foraging, so we don't divide the max-random-angle
        ]
        travel
        set action "travel"
        set behavior "travel"
        set behaviorsequence lput 3 behaviorsequence

      ]
    ][
      if step-model-param? = TRUE  AND distance tree_target > 0.8 [
        rt ( random max_rel_ang_travel_75q ) - ( random max_rel_ang_travel_75q )  ; travel is more directed than foraging, so we don't divide the max-random-angle
      ]
      travel
      set action "travel"
      set behavior "travel"
      set behaviorsequence lput 3 behaviorsequence

    ]
  ]



  if travel_mode = "long_distance" [
    if ld_tree_target = -1 [
      set frugivory-time 0
      search-feeding-tree
    ]

    ;    if on-feeding-tree? = FALSE [ set heading towards ld_tree_target ] ; to avoid the same point (x,y) error
    if straight-line-to-target? = TRUE [ ; otherwise it might enter the matrix
      face ld_tree_target
    ]


    ifelse ( action = "travel" OR action = "forage" ) [

      ifelse ( random-float 1 < p-foraging-while-traveling ) [

        if ld_tree_target != -1 AND distance ld_tree_target > 0.8 * step_len_travel [ ; otherwise it migh forage for 3 sequential steps while arriving in the feeding tree
          forage
          ;; RANDOM movement while foraging:
          if step-model-param? = TRUE  AND distance ld_tree_target > 0.8 [
            rt ( random max_rel_ang_forage_75q ) - ( random max_rel_ang_forage_75q )  ; feeding is less directed than travel
          ]
          if step-model-param? = TRUE  AND distance ld_tree_target > 0.8 [
            rt ( random max_rel_ang_travel_75q ) - ( random max_rel_ang_travel_75q )  ; travel is more directed than foraging, so we don't divide the max-random-angle
          ]
          travel
          set action "travel"
          set behavior "travel"
          set behaviorsequence lput 3 behaviorsequence
        ]

      ][

        if step-model-param? = TRUE  AND distance ld_tree_target > 0.8 [
          rt ( random max_rel_ang_travel_75q ) - ( random max_rel_ang_travel_75q )  ; travel is more directed than foraging, so we don't divide the max-random-angle
        ]
        travel
        set action "travel"
        set behavior "travel"
        set behaviorsequence lput 3 behaviorsequence

      ]
    ][
      travel
      set action "travel"
      set behavior "travel"
      set behaviorsequence lput 3 behaviorsequence

    ]
  ]


  ; ========================================= ;
  ;; this procedure independs of travel mode:
;  travel
;
;  set behaviorsequence lput 3 behaviorsequence

;  set color grey ; in case the tamarin foraged, it became magenta

  ; ========================================= ;
  ;; this procedure independs of travel mode:


end

;----------------------------------------

to search-feeding-tree


;  ask feeding-trees with [color = red OR color = blue] [ set color green ]  ; make last target (short or long distance) green again

  if travel_mode = "short_distance" [
    let let_pot_list tree_pot_list

    set tree_target min-one-of feeding-trees with [member? who let_pot_list] [distance myself] ;; CLOSEST TREE
    ask tree_target [ set color red ]    ; make close tree target red
    set tree_target_species [ species ] of tree_target
;    print tree_target   ; debugging
  ]

  if travel_mode = "long_distance" [
    let let_pot_list tree_pot_list
    set ld_tree_target one-of feeding-trees with [member? who let_pot_list] ;; RANDOM TREE
    set tree_target ld_tree_target ; VERY IMPORTANT FOR NOT HAVING TO CHANGE ALL THE FEEDING PROCEDURE
    ask tree_target [ set color blue ]    ; make long distance target blue

    set tree_target_species [ species ] of ld_tree_target
;    print ld_tree_target    ; debugging
  ]


  if feedingbout-on? [
    ;; TREE ENERGY VARIABLE WAS DERIVED BY ECKHARD AND MAYARA; SPECIES-TIME EMPIRICAL BASED ON FELIPE BUFALO DISSERTATION
    if tree_target_species = "annona" [
      set species_time 1
;      set energy_species 5
    ]
    if tree_target_species = "Celtis iguanaea" [
      set species_time 4
;      set energy_species 2
    ]
    if tree_target_species = "Cissus sulcicaulis" [
      set species_time 1
;      set energy_species 4
    ]
    if tree_target_species = "cordia" [ ; check if this species occurs in the input of trees
      set species_time 4
;      set energy_species 4
    ]
    if tree_target_species = "Dyospiros inconstans" [
      set species_time 1
;      set energy_species 3
    ]
    if tree_target_species = "ficus" [ ; check if this species occurs in the input of trees
      set species_time 4
;      set energy_species 2
    ]
    if tree_target_species = "Pereskia aculeata" [
      set species_time 2
;      set energy_species 5
    ]
    if tree_target_species = "Rhipsalis cereuscula" [
      set species_time 2
;      set energy_species 1
    ]
    if tree_target_species = "Syagrus romanzoffiana" [
      set species_time 2
;      set energy_species 3
    ]
    if tree_target_species = "rhamnidium" [ ; check if this species occurs in the input of trees
      set species_time 1
;      set energy_species 4
    ]
    if tree_target_species = "unknown" [  ; I don't know to which trees in Felipe dataset this one referes to so I didn't change the values
      set species_time 3
;      set energy_species 1
    ]
    if tree_target_species = "claussenii" [  ; This one either
      set species_time 3
;      set energy_species 1
    ]
    if tree_target_species = "Eugenia sp" [
      set species_time 3
;      set energy_species 3
    ]
    if tree_target_species = "sp_five" [     ; This one either
      set species_time 3
;      set energy_species 2
    ]
    if tree_target_species = "NA" [     ; This one either
      set species_time 2
      ;      set energy_species 2
    ]
  ]
end

to travel

  ;  avoid-patch-set ; bump on the territory borders
;  print "TRAVEL"

  ifelse straight-line-to-target? = FALSE AND patch_avoid_matrix != nobody [
;    print "straight line false"
    face patch_avoid_matrix ; ignores random-angle while going to-feeding-tree
    if (action = "travel" ) [ forward step_len_travel ]
    if (action = "forage" ) [ forward step_len_forage ]
    set dist-traveled step_len_travel
    set steps-moved steps-moved + 1
    set energy energy + ( energy-loss-traveling * step_len_travel )
;    print "travel 2"
  ][
    forward step_len_travel
    set dist-traveled step_len_travel
    set steps-moved steps-moved + 1
    if (action = "travel" ) [ forward step_len_travel ]
    if (action = "forage" ) [ forward step_len_forage ]
    set straight-line-to-target? TRUE
;    print "travel 3"
  ]


end

;---------------------------------------------------------------------------------------------
; Defecation commands
;---------------------------------------------------------------------------------------------
to defecation
  if timestep < simulation-time * 85 / 100 [ ; 10% of the simulation-time (check parameterization);   Mayara's model: 84 is for 7 hours after waking up (after 3pm)

    ; testing if the monkey defecates the seeds AND put the seeds to the seeds' agent list
    if member? gut_transit_time seed_mem_list [                            ; if the timestep since consumed (seed_mem_list) is equal to gut_transit_time ...
      let loc_index position gut_transit_time seed_mem_list                ;
      let loc_who item loc_index seed_ate_list                             ; take the who number of the consumed seed based on the seed_mem_lit and save it in an index
      set seed_ate_list remove-item 0 seed_ate_list                        ; remove the first seed from the seed_ate_list
      set seed_add_list remove-item 0 seed_add_list                        ; do the same for the helper list (seed_add_list)
      set seed_mem_list remove gut_transit_time seed_mem_list              ; remove the gut_transit_time item from the seed_mem_list
      hatch-seeds n_seeds_hatched [ ; change to hatch more seeds! <<<
        setxy xcor ycor
        set mother-tree [id-tree] of feeding-trees with [ who = loc_who ]
        set species [species] of feeding-trees with [ who = loc_who ]
        set id-seed who
        set disp-day "same day"
        set label ""
        set shape "plant"
        set size 0.45
        set color 1
      ]
    ]
  ]

  set seed_mem_list (map + seed_add_list seed_mem_list)

end

;----------------------------------------------------

to morning-defecation

  foreach seed_ate_list [
    x ->  hatch-seeds n_seeds_hatched [ ; change to hatch more seeds! <<<
      setxy xcor ycor
      set mother-tree [id-tree] of feeding-trees with [ who = x ]
      set species [species] of feeding-trees with [ who = x ]
      set id-seed who
      set disp-day "next day"
      set label ""
      set shape "plant"
      set size 1
      set color 1
    ]
  ]
  set seed_ate_list []
  set seed_mem_list []
  set seed_add_list []
end

;---------------------------------------------------------------------------------------------
; Resting commands
;---------------------------------------------------------------------------------------------
to resting
  print "resting"   ; debugging
  set action "resting"
  set behavior "resting"

  set tree_current -1

  set behaviorsequence lput 4 behaviorsequence

  set steps-moved steps-moved + 1
  set energy energy + energy-loss-resting

end

;---------------------------------------------------------------------------------------------
; Sleeping commands
;---------------------------------------------------------------------------------------------
to sleeping

  ifelse tree_target = -1 [

    if sleeping-trees-scenario = "empirical" [ search-sleeping-defined ]  ; when using field trees ; WITH THIS IT DOES NOT           ;; EMPIRICAL
;    if sleeping-trees-scenario = "simulated" [ search-sleeping-tree ]     ; when simulating trees  ; ONLY WORKS WITH THIS PROCEDURE ;; SIMULATED (procedure droped on July 20th 2022)
  ][

;    avoid-patch-set
;
;    if straight-line-to-target? = TRUE [ ; otherwise it might enter the matrix
;      set heading towards tree_target
;    ]
;
;    set straight-line-to-target? TRUE


    if distance tree_target < 2 * step_len_travel * 0.8 [ ; travel speed basically doubles when tamrarins are going to the sleeping site

      set dist-traveled dist-traveled + distance tree_target
      move-to tree_target
      set x_UTM [ x_UTM ] of tree_target
      set y_UTM [ y_UTM ] of tree_target
      ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
      ; use set timestep 108 to test this
      set xcor [xcor] of tree_target + 0.01
      set ycor [ycor] of tree_target + 0.01

      ask tree_target [ set visitations visitations + 1 ]

      set tree_current tree_target
      set tree_target -1
      set action "sleeping"
      set behavior "sleeping"
      set action-time 0
      set frugivory-time 0
;       trees in the tree_ate_list[] had to get back to the tree_pot_list[]
;       they forget about the trees they visited last day
;      while [length tree_ate_list > 0] [
;        set tree_pot_list lput first tree_ate_list tree_pot_list
;        set tree_ate_list remove (first tree_ate_list) tree_ate_list
;      ]
;      set tree_mem_list [] ;; COMMENT OUT THIS BECAUSE THE TAMARINS DON'T FORGET
;      set tree_add_list [] ;; COMMENT OUT THIS BECAUSE THE TAMARINS DON'T FORGET

      output-type "*simulation-time* "
      output-type ticks
      output-print " ----"

    ]
  ]

  if action != "sleeping" [

    avoid-patch-set

    ifelse straight-line-to-target? = TRUE [ ; otherwise it might enter the matrix
      face tree_target

    ;; RANDOM movement while traveling:
    if step-model-param? = TRUE  AND distance tree_target > 1.5 [
      rt ( random max_rel_ang_travel_75q / 2 ) - ( random max_rel_ang_travel_75q / 2 ) ; tamarins show more directed behavior when heading to sleeping sites, so here we divide by 3
      ]

    ;-------------------- = travel
    forward 2 * step_len_travel ; travel speed basically doubles when tamrarins are going to the sleeping site
    set dist-traveled ( 2 * step_len_travel )
    set steps-moved steps-moved + 1
    set energy energy + ( energy-loss-traveling * ( 2 * step_len_travel ) )
    set action "travel"
    set behavior "travel"
    ;--------------------

    ][
;      print "straight line false"
      face patch_avoid_matrix
      forward 2 * step_len_travel ; travel speed basically doubles when tamrarins are going to the sleeping site
      set dist-traveled ( 2 * step_len_travel )
      set steps-moved steps-moved + 1
      set energy energy + ( energy-loss-traveling * ( 2 * step_len_travel ) )
      set action "travel"
      set behavior "travel"
    ]





    set behaviorsequence lput 3 behaviorsequence

  ]

  if all? monkeys [action = "sleeping"] [
        output-print "AHOY"

  ]

end

;----- activate when NOT simulating sleeping trees (data from field) --------------
to search-sleeping-defined

  if empirical-trees-choice = "closest" [
    set tree_target min-one-of sleeping-trees [distance myself] ;; FOR CHOSSING THE CLOSEST SLEEPING SITE
  ]

  if empirical-trees-choice = "random" [
    set tree_target one-of sleeping-trees                        ;; FOR CHOSSING RANDOM SLEEPING SITE
  ]

end

;---------------------------------------------------------------------------------------------
; Commands for other activities
;---------------------------------------------------------------------------------------------
to forage
;  print "forage"   ; debugging
  set action "forage"
;  set color magenta
  set behavior "foraging"

  set behaviorsequence lput 2 behaviorsequence



;; movement (travel) is already called by the 'to-feeding-tree' and after the 'forage' procedure being called ('random-action' not anymore), so it has to be commented out from here
;  forward travel_speed
;  set dist-traveled travel_speed
;  set steps-moved steps-moved + 1
  set energy energy + energy-from-prey + energy-loss-foraging

end

;-------------------------------------------------------------
to random-action

;  print "random-action"    ; debugging

  set action-time 0
  set frugivory-time 0

  ifelse random-float 1 < 0.25 [
;    set color black
;    ask patch-here [ set pcolor orange ]
    frugivory ;; because travel, forage and frugivory are within the same loop
;    show "random-action foraging"
;    beep
;;    set color grey
  ][
    resting
  ]
end

;-------------------------------------------------------------
to last-action-again

  print "last-action-again"   ; debugging

;  if action = "forage" [
;    forage
;    ; the following lines were excluded from the foraging procedure to not conflict with the to-feeding-tree procedure (agents were doing two steps)
;
;    if travel_mode = "long_distance" AND distance ld_tree_target > travel_speed * 0.8 [
;      travel
;    ]
;
;    if travel_mode = "short_distance" AND distance tree_target > travel_speed * 0.8 [
;      travel
;;      set color grey ; in case the tamarin has foraged, it became magenta
;    ]
;  ]

  if action = "resting" [ resting ]
  if action = "travel" OR action = "forage" [ frugivory ]

end


;---------------------------------------------------------------------------------------------
; Extra commands
;---------------------------------------------------------------------------------------------
to mod_memory ; Modulate memory list
;  set tree_ate_list lput [who] of tree_current tree_ate_list
;  set tree_mem_list lput 0 tree_mem_list
;  set tree_add_list lput 1 tree_add_list
;  let let_pot_list tree_pot_list
end

to forget_trees

  ;; INCREASE MM


  ; testing if the monkey forgets a tree AND returns this one back to the available list
  while [ member? step_forget tree_mem_list ][                    ; suppose step_forget is 18:         if 18 is in the mem list
    let loc_index position step_forget tree_mem_list        ; set loc_index as the position where 18 is in the mem_list (starting on 0, not 1) -> should be always 0
    let loc_who item loc_index tree_ate_list                 ; set loc_who as the first turtle (0) of the ate_list (e.g. feeding-tree 23)
    set tree_ate_list remove-item loc_index tree_ate_list    ; remove first item (loc_index = 0) from ate_list
    set tree_add_list remove-item loc_index tree_add_list    ; remove first item (loc_index = 0) from add_list
    set tree_mem_list remove-item loc_index tree_mem_list      ; remove the item which corresponds to step_forget (18) from mem_list
    set tree_pot_list lput loc_who tree_pot_list             ; include loc_who (e.g. feeding-tree 23) again in the potential list
  ]


    ; increase the memory counter
  set tree_mem_list (map + tree_add_list tree_mem_list)     ;; THIS IS NECESSARY FOR ADDING + 1 TO THE MEMORY LIST (= STEP NUMBERS) , AND NOT BECOMING [0, 0, 0, 0 ... ]

;  set tree_pot_list remove-item ( position [who] of tree_current tree_pot_list ) tree_pot_list    ;; THIS REMOVES THE TREE THAT THE TAMARINS JUST ATE FROM THE POT LIST, THUS IT SHOULD STAY IN THE FEEDING COMAMND

end




;---------------------------------------------------------------------------------------------
; Output commands
;---------------------------------------------------------------------------------------------
to output-day-stats

  output-type "steps-moved "
  output-print steps-moved
  output-type "ticks "
  output-print ticks
  output-type "steps/ticks "
  output-print (steps-moved / ticks)

end

;---------------------------------------------------------------
to write-to-file
  file-open output-locations
  foreach sort monkeys [ ?1 ->
    ask ?1 [
      file-write ticks
      file-write day
      file-write timestep
      ; write back the true geographical coordinates
      file-write precision first gis:envelope-of self 2
      file-write precision  last gis:envelope-of self 2
      file-write precision energy 1
      file-write action
      file-write behavior
    ]
  ]
  file-print " "
  file-close
end

;-----------------------------------------------------------------
to write-seeds
  file-open output-seeds-locations
  foreach sort seeds [ ?1 ->
    ask ?1 [
      ;file-write ticks     ; AS THIS IS ONLY BEING CALLED IN run_days PROCEDURE, IT ONLY WRITES THE LAST TICK VALUE
      file-write day       ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      file-write timestep  ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      file-write id-seed
      file-write precision first gis:envelope-of self 2
      file-write precision  last gis:envelope-of self 2
      file-write species
      file-write mother-tree
    ] file-print " "
  ]
  file-print " "
  file-close
end

;---------------------------------------------------------------
to write-trees
  file-open output-trees-locations
  foreach sort feeding-trees [ ?1 ->
    ask ?1 [
      ;file-write ticks     ; AS THIS IS ONLY BEING CALLED IN run_days PROCEDURE, IT ONLY WRITES THE LAST TICK VALUE
      ;file-write day       ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      ;file-write timestep  ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      file-write precision first gis:envelope-of self 2
      file-write precision  last gis:envelope-of self 2
      file-write species
      file-write id-tree
    ] file-print " "
  ]
  file-print " "
  file-close
end

;---------------------------------------------------------------
to write-rest
  file-open output-rest-locations
  foreach sort resting-trees [ ?1 ->
    ask ?1 [
      ;file-write ticks     ; AS THIS IS ONLY BEING CALLED IN run_days PROCEDURE, IT ONLY WRITES THE LAST TICK VALUE
      ;file-write day       ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      ;file-write timestep  ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      file-write precision first gis:envelope-of self 2
      file-write precision  last gis:envelope-of self 2
      file-write species
      file-write id-tree
    ] file-print " "
  ]
  file-print " "
  file-close
end

;---------------------------------------------------------------
to write-sleep
  file-open output-sleep-locations
  foreach sort sleeping-trees [ ?1 ->
    ask ?1 [
      ;file-write ticks     ; AS THIS IS ONLY BEING CALLED IN run_days PROCEDURE, IT ONLY WRITES THE LAST TICK VALUE
      ;file-write day       ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      ;file-write timestep  ; IT DOES NOT MAKE SENSE BECAUSE THIS PROCEDURE OUTPUTS EVERYTHING IN THE LANDSCAPE, NOT THE SEEDS FROM EACH DAY
      file-write precision first gis:envelope-of self 2
      file-write precision  last gis:envelope-of self 2
      file-write species
      file-write id-tree
    ] file-print " "
  ]
  file-print " "
  file-close
end
;-----------------------------------------------------------------

to calc-homerange
  r:eval "library(adehabitat)"
  r:eval "library(dplyr)"
  r:eval "library(tidyr)"
  r:eval "library(amt)"

  ;; create an empty data.frame"
  r:eval "turtles <- data.frame()"

  ;; merge the Name, X- and Y-lists of all animals to one big data.frame
  ask monkeys
  [
    (r:putdataframe "turtle" "X" X_coords "Y" Y_coords)
    r:eval (word "turtle <- data.frame(turtle, Name = '" Name "')")
    r:eval "turtles <- rbind(turtles, turtle)"
  ]

  ;; split the data.frame into coordinates and factor variable
  r:eval "xy <- turtles[,c('X','Y')]"
  r:eval "id <- turtles$Name"

  ;; calculate homerange (mcp method)
;  r:eval "homerange <- mcp(xy, id)"

  ;; calculate homerange (amt package)
  r:eval "db <- cbind(xy, id)"
  ;  show r:get "db"
  r:eval "db_nest <- db %>%  make_track(.x=Y, .y=X, id = id) %>% nest(data = -c(id))"
  ;  show r:get "db_nest"
  ; calculate HR metrics for every list (=id = run) using map()
  r:eval "db_nest <- db_nest %>% mutate( KDE95 = amt::map(data, ~ hr_kde(., level = 0.95)), KDE50 = amt::map(data, ~ hr_kde(., level = 0.50)) )"
  r:eval "db_nest <- db_nest %>%  select(-data) %>% pivot_longer(KDE95:KDE50, names_to = 'KDE_value', values_to = 'hr')"
  r:eval "db_nest <- db_nest %>% mutate(hr_area = map(hr, hr_area)) %>%  unnest(cols = hr_area)"
;  r:eval "db_nest <- db_nest %>% filter(KDE_value == 'KDE95') %>% dplyr::select(-c(3, 4))"
  r:eval "db_nest <- db_nest %>% dplyr::select(-c('what', 'hr'))"
  r:eval "db_nest <- db_nest %>% mutate(area = area / 10000)"

  print "db_nest: "
  show r:get "db_nest"

  r:gc

  ; get hr values to agent variable
  ask monkeys [
   set KDE_values item 3 r:get "db_nest"
  ]


;  ; Merge HR to db and save
;  r:eval "db <- left_join(db, db_nest)"
;  r:eval "db <- db %>% mutate(hr_area_ha = area / 10000)"
;
;  ; drop columns
;  r:eval "db <- db %>%    select(-c(KDE_value, area))"
;  print "db: "
;  show r:get "db"


end
;-----------------------------------------------------------------
; end of commands ================================================
;-----------------------------------------------------------------





;-----------------------------------------------------------------
;; REPORTERS =====================================================
;-----------------------------------------------------------------

;to-report simulation-time-end
;  ifelse day = no_days AND all? monkeys [ behavior = "sleeping" ]
;  ;  ifelse not any? monkeys
;  [   report TRUE   ]
;  [   report FALSE  ]
;end


;;; --------------------------------- ;;;
;;; REPORTERS FOR THE ACTIVITY BUDGET ;;;
to-report freq [ i_ list_ ]
  report length filter [ ind -> ind = i_ ] list_
end

to-report freq_map [ list_ ]
  ; get length of input list
  let len length list_

  ; get unique values for the input list
  let uniques remove-duplicates list_

  ; get counts of each unique value
  let counts map [ i -> freq i list_ ] uniques

  ; report an xy pair for each unique value / proportion
  report ( map [ [ x y ] -> list x ( y / len ) ] uniques counts )
end

;;; --------------------------------- ;;;
;;;  REPORT FOR TRAVEL MODE HISTOGRAM ;;;
to-report travelmode
;  let travelmodelist []
  ask monkeys [

    ifelse travel_mode = "short_distance" [
    set travelmodelist lput 1 travelmodelist
    ][
    set travelmodelist lput 2 travelmodelist
    ]
]

  report travelmodelist
end


;;; ---------------------------------------------------------- ;;;
;;; MAKING TAMARINS ENTER THE LONG DISTANCE MODE FOR DEBUGGING ;;;
to test-long-distance
  ask monkeys [
    set energy energy_level_1 + 100
    set travel_mode "long_distance"
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
501
406
-1
-1
3.0
1
10
1
1
1
0
0
0
1
-80
80
-64
64
0
0
1
ticks
30.0

SLIDER
516
312
688
345
start-energy
start-energy
30
170
92.0
1
1
NIL
HORIZONTAL

BUTTON
603
12
675
48
SETUP
if any? monkeys [ stop-inspecting one-of monkeys ]\nsetup\ninspect one-of monkeys
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
44
44
194
62
Feeding tree
12
0.0
1

TEXTBOX
44
63
194
81
Sleeping tree
12
0.0
1

TEXTBOX
45
83
195
101
Defecated seeds
12
0.0
1

SWITCH
1382
514
1500
547
show-energy?
show-energy?
0
1
-1000

SWITCH
1382
557
1502
590
show-path?
show-path?
0
1
-1000

SLIDER
564
90
736
123
simulation-time
simulation-time
0
170
108.0
1
1
NIL
HORIZONTAL

SLIDER
516
348
688
381
energy-from-fruits
energy-from-fruits
0
30
4.0
1
1
NIL
HORIZONTAL

BUTTON
563
188
630
221
STEP
step
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
0

BUTTON
604
48
674
82
go
go
T
1
T
OBSERVER
NIL
C
NIL
NIL
1

BUTTON
676
49
759
82
Next Day
next_day
NIL
1
T
OBSERVER
NIL
N
NIL
NIL
1

BUTTON
676
12
760
47
Run Days
run_days
NIL
1
T
OBSERVER
NIL
R
NIL
NIL
1

INPUTBOX
539
12
601
82
no_days
8.0
1
0
Number

SLIDER
516
386
688
419
energy-from-prey
energy-from-prey
0
15
2.3
0.1
1
NIL
HORIZONTAL

SLIDER
516
423
689
456
energy-loss-traveling
energy-loss-traveling
-10
0
-3.0
0.1
1
NIL
HORIZONTAL

SLIDER
516
459
689
492
energy-loss-foraging
energy-loss-foraging
-10
0
-2.5
0.1
1
NIL
HORIZONTAL

SLIDER
516
497
689
530
energy-loss-resting
energy-loss-resting
-10
0
-2.5
0.1
1
NIL
HORIZONTAL

MONITOR
614
133
690
178
timestep
timestep
17
1
11

OUTPUT
450
615
674
728
11

TEXTBOX
46
25
196
43
Tamarin
12
0.0
1

INPUTBOX
4
612
411
698
runtime
./runtime/
1
0
String

CHOOSER
977
29
1127
74
feeding-trees-scenario
feeding-trees-scenario
"All months" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
5

CHOOSER
984
102
1130
147
sleeping-trees-scenario
sleeping-trees-scenario
"empirical" "simulated"
0

SWITCH
1382
473
1502
506
export-png
export-png
1
1
-1000

SLIDER
715
337
877
370
step_forget
step_forget
0
1000
30.0
1
1
NIL
HORIZONTAL

TEXTBOX
542
292
678
328
2. energy related
14
15.0
1

TEXTBOX
728
288
865
322
3. memory related
14
15.0
1

SLIDER
924
616
1075
649
gut_transit_time
gut_transit_time
0
100
13.0
1
1
NIL
HORIZONTAL

TEXTBOX
912
287
1069
321
4. movement related
14
15.0
1

TEXTBOX
753
495
893
531
5. feeding bout
14
15.0
1

SLIDER
535
538
671
571
energy_level_1
energy_level_1
0
200
99.0
1
1
NIL
HORIZONTAL

SLIDER
535
573
671
606
energy_level_2
energy_level_2
1
1000
143.0
1
1
NIL
HORIZONTAL

TEXTBOX
935
596
1062
632
6. seed dispersal
14
15.0
1

SLIDER
925
655
1074
688
n_seeds_hatched
n_seeds_hatched
0
100
1.0
1
1
NIL
HORIZONTAL

CHOOSER
984
144
1130
189
empirical-trees-choice
empirical-trees-choice
"closest" "random"
0

MONITOR
552
133
609
178
day
day
17
1
11

SWITCH
813
195
932
228
sleeping-trees?
sleeping-trees?
0
1
-1000

SWITCH
813
163
932
196
feeding-trees?
feeding-trees?
0
1
-1000

TEXTBOX
796
10
959
45
1. Resources scenario
14
15.0
1

SWITCH
1002
217
1128
250
all-slp-trees?
all-slp-trees?
1
1
-1000

TEXTBOX
1009
192
1114
220
make all trees from study period available:
9
0.0
1

BUTTON
653
225
729
258
108 steps
repeat 108 [ step ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
543
225
648
258
print-step?
print-step?
1
1
-1000

PLOT
1134
338
1357
532
Memory
tick
count memory lists
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ask monkeys [ plot length tree_pot_list ]"
"pen-1" 1.0 0 -2674135 true "" "ask monkeys [ plot length tree_mem_list ]"
"pen-2" 1.0 0 -955883 true "" ";ask monkeys [ plot length tree_ate_list ]"
"pen-3" 1.0 0 -13840069 true "" "ask monkeys [ plot (length tree_mem_list + length tree_pot_list) + 2 ]"
"pen-4" 1.0 0 -16777216 true "" "let n_trees round ( count feeding-trees  / prop_trees_to_reset_memory )\nplot n_trees"

TEXTBOX
973
83
1170
111
1.4 Choose sleeping site scenario
11
0.0
1

TEXTBOX
969
13
1148
42
1.3 Choose fruit trees scenario
11
0.0
1

BUTTON
650
187
727
220
50 steps
repeat 50 [ step ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
726
642
871
675
duration
duration
0
20
3.0
1
1
NIL
HORIZONTAL

MONITOR
696
132
754
177
Energy
[ round energy ] of monkeys
3
1
11

SLIDER
715
386
879
419
visual
visual
0
20
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
708
313
875
338
How many timesteps BLTs take to forget a tree:
10
0.0
1

TEXTBOX
710
372
877
392
exclude trees in radii from pot list:
10
0.0
1

SWITCH
1383
598
1548
631
path-color-by-day?
path-color-by-day?
1
1
-1000

TEXTBOX
32
423
104
446
OUTPUT:
16
15.0
1

SWITCH
0
499
118
532
output-files?
output-files?
1
1
-1000

CHOOSER
0
448
128
493
USER
USER
"Ronald" "Eduardo" "LEEC" "Others"
1

SWITCH
2
534
120
567
output-print?
output-print?
1
1
-1000

SLIDER
924
546
1068
579
p-foraging-while-traveling
p-foraging-while-traveling
0
1
0.7
0.05
1
NIL
HORIZONTAL

TEXTBOX
732
676
882
699
max timesteps repeating same behavior (other than feeding)
9
0.0
1

PLOT
1360
209
1533
333
action-time (red) / frugivory-time (blue)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "ask monkeys [ plot action-time ]"
"pen-1" 1.0 0 -13345367 true "" "ask monkeys [ plot frugivory-time ]"

PLOT
1360
14
1604
204
Activity budget
NIL
Proportion (%)
0.0
4.0
0.0
1.0
true
false
"set-histogram-num-bars 4" ""
PENS
"default" 1.0 1 -16777216 true "" "clear-plot\nforeach ( freq_map behaviorsequence ) [ x -> \nplotxy first x last x ]"
"pen-1" 1.0 0 -7500403 true "" "plot 0.5"

TEXTBOX
1414
187
1457
207
frugivory
8
0.0
1

TEXTBOX
1454
187
1492
208
foraging
8
0.0
1

TEXTBOX
1494
187
1521
207
travel
8
0.0
1

TEXTBOX
1524
185
1557
205
resting
8
0.0
1

PLOT
1148
210
1356
333
energy
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -1184463 true "" "ask monkeys [ plot energy ]"
"pen-1" 1.0 0 -16777216 true "" "plot energy_level_1"
"pen-2" 1.0 0 -955883 true "" "plot energy_level_2"

TEXTBOX
909
344
1083
371
add empirical step parameters. If so:
9
0.0
1

BUTTON
402
483
499
518
NIL
test-long-distance
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1135
538
1335
688
Travel mode frequency
NIL
NIL
0.0
4.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "ask monkeys [ histogram travelmodelist ]"

SWITCH
727
516
874
549
feedingbout-on?
feedingbout-on?
1
1
-1000

BUTTON
135
447
254
480
NIL
ride one-of monkeys
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
262
448
358
482
NIL
reset-perspective
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
360
449
499
483
NIL
inspect one-of monkeys
NIL
1
T
OBSERVER
NIL
I
NIL
NIL
1

BUTTON
135
484
397
517
NIL
ask monkeys [ show length tree_pot_list ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
715
428
879
461
prop_trees_to_reset_memory
prop_trees_to_reset_memory
2
8
2.0
1
1
NIL
HORIZONTAL

TEXTBOX
716
464
866
482
don't choose 1
9
0.0
1

TEXTBOX
728
552
878
574
energy and time spent feeding for each tree species. If not:
9
0.0
1

PLOT
1362
338
1535
458
duration (red) / species_time (blue)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "ask monkeys [ ifelse tree_current != -1 [ plot [ species_time ] of tree_current ] [ plot 0 ] ]"
"pen-1" 1.0 0 -2674135 true "" ";ask monkeys [ ifelse tree_current != -1 [ plot [ duration ] of tree_current ] [ plot 0 ] ]\n\nplot duration\n\n"

BUTTON
135
518
244
553
check tree species
ask one-of feeding-trees [ let specieslist [species] of feeding-trees set specieslist remove-duplicates specieslist print specieslist] 
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1147
69
1356
206
DPL (m)
NIL
NIL
0.0
0.0
0.0
5000.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true ";if [behavior] of monkeys = \"sleeping\" [ plot-pen-down ]\nask monkeys [ if behavior = \"sleeping\" [ plot-pen-down ] ]" ";ask monkeys [ plot DPL * patch-scale ]"
"pen-1" 1.0 2 -2674135 true "" "ask monkeys [ if action = \"sleeping\" [ plot DPL * patch-scale ] ]"
"pen-2" 1.0 0 -7500403 true "" ";ask monkeys [ if behavior = \"sleeping\" [ plot mean DPL_d ] ]\n;ask monkeys [ plot mean DPL_d ]"

TEXTBOX
34
360
201
380
Long distance target
11
0.0
1

TEXTBOX
33
382
200
402
Short distance target
11
0.0
1

TEXTBOX
800
32
967
52
1.1 choose fragment
11
0.0
1

CHOOSER
802
50
941
95
study_area
study_area
"Guareí" "Santa Maria" "Taquara" "Suzano"
0

BUTTON
247
520
370
554
count feeding-trees
print count feeding-trees
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
287
378
525
422
MODEL VERIFICATION:
18
15.0
1

TEXTBOX
1145
10
1325
75
Guareí = May, Jun, Jul, Aug\nSanta Maria = Mar, Apr\nTaquara = Jan\nSuzano = Sep, Dec (Feb and Apr for debugging avoid-matrix)\n
10
15.0
1

TEXTBOX
802
148
951
172
1.2 Create tree resources
11
0.0
1

MONITOR
826
94
912
139
patch size (m)
patch-scale
17
1
11

BUTTON
372
520
500
554
count visited trees
type \"unvisited trees: \" print count feeding-trees with [visitations = 0]\n\ntype \"visited trees: \" print count feeding-trees with [visitations > 0]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
136
556
243
590
NIL
clear-drawing
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
246
557
381
590
move-to farther slp tree
ask one-of monkeys [\nlet choice max-one-of sleeping-trees [xcor]\nmove-to choice\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
920
309
1063
343
step-model-param?
step-model-param?
0
1
-1000

SLIDER
907
395
1101
429
max_rel_ang_forage_75q
max_rel_ang_forage_75q
0
180
50.0
5
1
NIL
HORIZONTAL

SLIDER
908
466
1081
500
step_len_forage
step_len_forage
0
20
1.4060000000000001
0.1
1
NIL
HORIZONTAL

SLIDER
908
435
1081
469
step_len_travel
step_len_travel
0
20
3.0
0.1
1
NIL
HORIZONTAL

SLIDER
908
362
1097
396
max_rel_ang_travel_75q
max_rel_ang_travel_75q
0
180
50.0
1
1
NIL
HORIZONTAL

SLIDER
726
578
874
612
species_time
species_time
1
20
2.0
1
1
NIL
HORIZONTAL

TEXTBOX
744
615
868
644
max timesteps feeding on the same tree species
10
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is a spatially explicit individual-based model adapted for the black lion tamarin  (Leontopithecus chrysopygus). Our study area is a 100ha fragment at Guarei, SP, Brazil. 

## HOW IT WORKS

The agent is the tamarin who moves according to its energy level. It gains energy when feeding and foraging and loses while traveling, foraging and resting. 

## HOW TO USE IT

At the set up section, there are options for different scenarios: 
- to include all feeding trees and use field resting and sleeping trees: use files "trees_all_2"
- to include all feeding trees but simulating resting and sleeping trees: use files "trees_all_1"
- to simulate monthly fruit availability (with field resting and sleeping trees): use respective files for each month and set the number of days accordingly - "trees_april_2" (30 days), "trees_may_2" (31), "trees_june_2" (30), "trees_july_2" (31), "trees_august_2" (31)
- to simulate monthly fruit availability (simulating resting and sleeping trees): use respective files for each month and set the number of days accordingly - "trees_april_1" (30 days), "trees_may_1" (31), "trees_june_1" (30), "trees_july_1" (31), "trees_august_1" (31)

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)
- GIS extension

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)
- BIALOZYT et al., 2014.
Bialozyt, R.; Flinkerbusch, S.; Niggemann, M.; Heymann, E.W. Predicting the seed shadows of a Neotropical tree species dispersed by primates using an agent-based model with internal decision making for movements. Ecological Modelling, v.278, p.74-84, 2014.

## CREDITS AND REFERENCES

Mayara Mulato dos Santos: Laboratory of Primatology - Department of Biodiversity (UNESP Rio Claro), Rio Claro, Brazil

Ronald Bialozyt: Nordwest German Forest Research Institute (Nordwestdeutsche Forstliche Versuchsanstalt, NW-FVA), Department of Growth and Yield, Göttingen, Alemanha

Laurence Culot: Laboratory of Primatology - Department of Biodiversity (UNESP Rio Claro), Rio Claro, Brazil

Eckhard Heymann: German Primate Center (Deutches Primatenzentrum), Behavioral Ecology and Sociobiology Unit, Göttingen, Alemanha

Always cite financial aids: CAPES (Masters grant); FAPESP (Masters grant) 2018/15625-0; FAPESP (JP Laurence) 014/14739-0

Other coauthors: Felipe S Bufalo; Gabriel P Sabino (bothanical field trips) and Joice de Lima (BLT field trips).
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

banana
false
0
Polygon -7500403 false true 25 78 29 86 30 95 27 103 17 122 12 151 18 181 39 211 61 234 96 247 155 259 203 257 243 245 275 229 288 205 284 192 260 188 249 187 214 187 188 188 181 189 144 189 122 183 107 175 89 158 69 126 56 95 50 83 38 68
Polygon -1184463 true false 39 69 26 77 30 88 29 103 17 124 12 152 18 179 34 205 60 233 99 249 155 260 196 259 237 248 272 230 289 205 284 194 264 190 244 188 221 188 185 191 170 191 145 190 123 186 108 178 87 157 68 126 59 103 52 88
Line -16777216 false 54 169 81 195
Line -16777216 false 75 193 82 199
Line -16777216 false 99 211 118 217
Line -16777216 false 241 211 254 210
Line -16777216 false 261 224 276 214
Polygon -16777216 true false 283 196 273 204 287 208
Polygon -16777216 true false 36 114 34 129 40 136
Polygon -16777216 true false 46 146 53 161 53 152
Line -16777216 false 65 132 82 162
Line -16777216 false 156 250 199 250
Polygon -16777216 true false 26 77 30 90 50 85 39 69

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

mlp-1
true
0
Circle -7500403 true true 120 90 90
Circle -7500403 true true 120 15 60
Polygon -7500403 true true 195 180 195 270 180 285 210 270 210 150 195 165
Polygon -7500403 true true 135 105 105 135 90 135 90 150 105 150 135 120
Polygon -7500403 true true 135 30 150 0 150 30 135 30
Rectangle -7500403 true true 105 45 120 60
Polygon -7500403 true true 150 165 135 180 165 180 150 165
Polygon -7500403 true true 165 165 135 195 150 195 180 195 195 180 180 165
Polygon -7500403 true true 120 30 105 45 90 45 90 60 105 75
Polygon -7500403 true true 105 75 135 75 135 75 105 60
Polygon -7500403 true true 90 45 135 30 120 45
Polygon -7500403 true true 105 180 120 180 135 165 135 150 120 150 120 165 105 180
Polygon -7500403 true true 120 75 135 90 150 90 150 60 150 45

mlp-2
false
14
Polygon -16777216 true true 90 90 75 165 90 195 105 210 135 210 150 225 165 210 195 210 210 195 225 165 210 75
Circle -16777216 true true 86 71 127
Circle -16777216 true true 150 60 60
Circle -16777216 true true 90 60 60
Circle -7500403 false false 89 74 122
Circle -7500403 true false 105 90 60
Circle -7500403 true false 135 90 60
Circle -7500403 true false 116 116 67
Polygon -7500403 true false 165 135 165 135 165 150 150 135
Polygon -16777216 true true 150 150 165 135 165 150 150 150
Polygon -16777216 true true 135 135 135 150 150 150 135 135
Circle -16777216 true true 120 105 30
Circle -16777216 true true 150 105 30
Line -16777216 true 135 165 165 165
Line -6459832 false 165 120 150 120
Line -6459832 false 135 120 150 120

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

plant medium
false
0
Rectangle -7500403 true true 135 165 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 165 120 120 150 90 180 120 165 165

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Sensitivity-Analysis" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[ behavior ] of monkeys</metric>
    <enumeratedValueSet variable="step_forget">
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visual">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop_trees_to_reset_memory">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
