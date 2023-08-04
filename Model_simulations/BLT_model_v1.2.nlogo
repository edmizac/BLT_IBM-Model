; ==== Black lion tamarin model ============
; Eduardo Zanette & Ronald Bialozyt. Jun. 2023
; Two travel modes: long-distance (target selected when energy > lvl 2 and travels to this direction up to when energy < lvl 1) and short-distance (when energy < lvl 1)
; Parameterized step model: step length and turning angles
; Parameterized p_foraging_while_traveling: % foraging / ( %foraging + %traveling)
; Parameterized feeding-bout: tree species-specific energy_species and species_time values on and off by switcher feedingbout-on?
; Parameterized resting time: simulation-time when resting is possible + max number of steps spent resting (duration)
; ------------------------------------------------

extensions [ gis sr palette pathdir] ; using the GIS extension of NetLogo

;; BREEDS ;;
turtles-own [
  x_UTM y_UTM
  X_coords Y_coords ; X and Y list of coordinates (x_UTM and y_UTM) for home range calculation with the r extension in the end of each run (days = n_days)
  day_list          ; day of the event (to use in amt package with X_ and Y_coords)
  x_scaled y_scaled ; x and y * patch-scale for calc-seed-aggregation

  ;; BUILD_FOREST ;;

  X
  Y
  component
  iterations
  MYNND
  ID
  X_M
  Y_M

]

; trees
breed [feeding-trees feeding-tree] ; feeding trees have species and id code and a visited counter
feeding-trees-own [
  species
  id-tree
  visitations
  dist-to-homerange-center  ; list of distances of possible ld targets to homerange-center
  dist-to-ld-tamarins          ; list of distances of possible ld targets to the tamarin (to select the farther one)
]

breed [sleeping-trees sleeping-tree]
sleeping-trees-own [ species id-tree visitations ]

breed [resting-trees resting-tree]
resting-trees-own [ species id-tree ]

breed [legend-trees legend-tree] ; to set up a legend with the color of trees

breed [seeds seed]
seeds-own [
  id-seed species mother-tree mother-tree-who mother-tree-agent
  disp-day
  SDD
]

breed [monkeys monkey]
monkeys-own [
  energy_stored   ; amount of energy accumulated throughout the simulation
  energy          ; energy the tamarin has left
  enlvl1             ; energy level 1 of every simulation
  enlvl2             ; energy level 2 of every simulation
  enstart             ; start energy of every simulation
;  status         ; what is the desire ===== DO WE REALLY NEED THIS? ==============
  action          ; what was the last action
  action-time     ; how long you do the same action again (other than frugivory)
  frugivory-time  ; how long you consume the same species (= feeding bout)
  going-sleeping? ; if timestep > 0.9 * timestep, tamarins are going to sl
  behavior        ; as in activity budget data tables
  dist-traveled   ; distance traveled this time step (= step length)
  steps-moved     ; number of steps taken
  travel_mode     ; if it is short or long distance travel
  tree_target     ; target tree (short distance, but = long distance in this travel mode)
  tree_target_mem ; target tree memory for when tamarins are avoiding the matrix (avoid-matrix and avoid-patch-set)
  tree_target_mem2 ; target tree memory for when tamarins are go < energy_level_1 and redefine their ld_target
  tree_target_dist ; target tree distance
  ld_tree_target  ; long distance target tree
  tree_target_species ; species of the target tree independent of travel mode
  patch_avoid_matrix ; patch to walk to when straight-line-to-target? = FALSE
  front_patches   ; patches in-cone or in-radius
  candidate_patches ; patches to go when avoiding the matrix
  straight-line-to-target? ; if there is matrix in front of the tamarin in straight line
  travelmodelist  ; list to make travel mode histogram
  tree_current    ; old_tree

  homerange-center ; center of home range
  candidate_ld_targets ; list of possible ld targets
;  candidate_ld_dists   ; list of distances of possible ld targets to homerange-center

  tree_pot_list   ; list of all feeding trees in homerange for that tamarin
  tree_ate_list   ; list of trees the tamarins did eat
  tree_mem_list   ; list of timesteps since the tamarin feeded on that tree
  tree_add_list   ; helper list to increase the mem list
  tree_bucket     ; list of ate trees that are coming back to tree_pot_list by the enhance_memory_list procedure

  seed_ate_list   ; list of trees they fed on ([who] of tree_current)
  seed_mem_list   ; list of timesteps since the tamarin ate the seed
  seed_add_list   ; helper list to increase the mem list by 1 each time step
  seed_gtt_list   ; list of timesteps that each seed will take to be defecated


  ; OUPUT MONKEY VARIABLES
  Name ; monkey who number for home range calculation with the r extension in case there's more than one group
;  KDE_values ; not being used anymore
  MCP_100         ; output of amt package in calc-homerange
  KDE_95         ; output of amt package in calc-homerange (in m²)
  KDE_50         ; output of amt package in calc-homerange (in m²)
  KDE_95_cropped ; cropped with st_intersect() or hr_overlap(), in hectares
  KDE_50_cropped ; cropped with st_intersect() or hr_overlap(), in hectares


  ; activity budget
  p_feeding
  p_foraging
  p_traveling
  p_resting

  ; movement patterns
  step_length_mean
  step_length_sd
  turn_ang_mean
  turn_ang_sd

  ; calculated in the sleeping procedure (because it becomes 0 everyday)
  DPL             ; daily path length. It is a daily value, but it become the average in the end of the run
  DPL_mean        ; for outputing mean DPL
  DPL_sd          ; sd daily path length. It is only calculated by the end of the run
  DPL_d           ; list with values of DPL for the DPL plot

  MR              ; movement rate (as in Fuzessy et al. 2017) (DPL / activity time in hours)
  MR_mean         ; for outputing mean MR
  MR_sd           ; sd movement rate. It is only calculated by the end of the run
  MR_d            ; list of movement rate values (as in DPL_d)


  ; aditional metrics
  PT              ; path twisting (as in Fuzessy et al. 2017). It is only calculated by the end of the run as it requires the home range
  PT_mean         ; for outputing mean PT
  PT_sd           ; sd path twisting. It is only calculated by the end of the run
  PT_d            ; list of path twisting values (as in DPL_d)

  MSD             ; from amt
  intensity_use   ; from amt
  straightness    ; from amt
  sinuosity       ; from amt

  ; defendability indices
  DI_index
  M_index
]

breed [blobs blob]
blobs-own [patch_before]

patches-own [
  habitat

  ;; FROM BUILD_FOREST MODEL ;;
   ; Eyal
  lc-val
  hm-val
  cluster
  cluster-n

  ; Eduardo
;  habitat
  hr_cell
  lambda
  nn-distance
  xP
  yP


]

;; GLOBALS ;;
globals [
  sr-extension-set?    ; check if SimpleR extension was setted up already
  survived? ; to check if tamarins survived up to the end of the run and test parameterizations

  R_seeds ; aggregation index of seeds
  R_seeds_p ; clarkevans test p value
  NN_seeds  ; nearest neighbor distances for seeds (defecation events)
  n_visited_trees ; number of visited trees in the end of the run
  n_unvisited_trees ; number of unvisited trees in the end of the run (calculate proportion afterwards instead of giving a very long metric to nlrx)

  ; THESE ARE MONKEY VARIABLES THAT WE TAKE AS GLOBAL TO AVOID NLRX ERRORS (OR DEAD AGENTS OUTPUTING EMPTY VALUES)
  g_SDD                   ; mean seed dispersal distance for all events (same and next day)
  g_SDD_sd                ; sd of SDD for all events (same and next day)
  g_SDD_sameday
  g_SDD_nextday
  g_SDD_sd_sameday
  g_SDD_sd_nextday
  g_energy_stored
  g_KDE_95
  g_KDE_50
  g_p_feeding
  g_p_foraging
  g_p_traveling
  g_p_resting
  g_step_length_mean
  g_step_length_sd
  g_turn_ang_mean
  g_turn_ang_sd
  g_DPL
  g_DPL_sd
  g_DPL_d
  g_MR
  g_MR_sd
  g_MR_d
  g_PT
  g_PT_sd
  g_PT_d
  g_MSD
  g_intensity_use
  g_straightness
  g_sinuosity

  g_n_visited_trees
  g_n_unvisited_trees

  ; defendability indexes
  g_DI_index
  g_M_index


  patch-scale
  behaviorsequence

  timestep ; step counter during one day
  day  ; present day in the simulation
  ;scale ; to fit the data to the simulation area
  meanxcoord ; translating the geo coordinates to world coordinates
  meanycoord ; translating the geo coordinates to world coordinates
  ;step_forget ; amount of timesteps until the tamarin forgets to be in that tree
  midday ; the time of the middle of the day (important for resting) ; now depends on simulation-time slider
  midday_start ; depends on midday
  midday_end ; depends on midday

  ;; patch sets:
  forest_set
  matrix_set
  border_patches

  ;; for homerange-center
  position-all-trees-x-list
  position-all-trees-y-list

  species_time ; how long the tamarin feeds on the tree species
  energy_species ; value of energy they get from feeding of each tree species

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

  ;for R index
  limitsOwin

  ;param values;
;  gut_transit_time ; amount of timesteps until the tamarin defecates (time the seed takes to go throught all the digestive system)
;  travel_speed ; global speed for travel


  ;; OUTPUT ;;
;  path ; for inputting generated_patches
  local-path  ; path for the model to run in different CPUs
  output-locations ; base filename for the monkey locations
  output-seeds-locations ; base filename for the seed locations
  output-rest-locations ; base filename for data from the simulated resting trees
  output-sleep-locations ; base filename for data from the simulated sleeping trees
  output-trees-locations ; base filename to check the geo coordinates for feeding trees


  ;; IMPORTED WORLD VARIABLES (FROM BUILD_FOREST MODEL) ;;
  ; Eyal
  stop-report
  chosen-cluster

  field.shape.factor
  final-frac
  final-patches

  ; Eduardo
  R_feeding_trees
  R_feeding_trees_p
  NN_feeding_trees

  R_sleeping_trees
  R_sleeping_trees_p
  NN_sleeping_trees

  n_points_total
  n_points_ftrees
  n_points_strees

  aggregation_type
  sd-displacement
  n_reps_random
  n_reps_clumped
  n_reps_ordered

  simulated_hr_list
  hr_id
  hr_x
  sd-prop

;  patch-scale
  lc-patch-size

  n
  n-clusters
  n-reps-hr
  n-reps-resource
  n-sleeping-trees
  only-random?
  sd_displacement_start

  setup_mode
  setup-mode
  smooth
  use-predicted-hr?

;  heigth
  height
  width


  batch2_input
  output_folder
  patch_type
;  patch-type
  resolution
  cluster_type
  density
  export-csv1?
  export-csv2?
  frac-index
  home-range-forced
  home-ranges-n
  lc-patch-size-ha
  max-angle-diversion
  max-corrections
;  mean_node_length
;  sd_node_length
  mean-node-lengh
  sd-node-lengh
  river-nodes
  cluster-type
  home_ranges_n
  hr-size-final


]




;--------------------------------------------------------------------------------
; SETTING UP
;--------------------------------------------------------------------------------
to setup
  clear-all

  if USER = "Ronald"
  [ set local-path "/home/rbialozyt/BLT_IBM-Model/" ]
  if USER = "Eduardo"
  [
    set local-path "D:/Data/Documentos/github/BLT_IBM-Model/"
;    if patch-type = "generated" [ set path "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment7/batch_all/" ]
  ]
  if USER = "LASi"
  [set local-path "D:/EDUARDO_LAP"]
  if USER = "LEEC"
  [set local-path "D:/Eduardo_LaP/"]

  if USER = "PC02"
  [ set local-path "D:/Eduardo_LaP/" ]

  if USER = "AORUS-2"
  [ set local-path "C:/Users/User/Documents/Eduardo_LaP/" ]

  if USER = "Others"
  [ set local-path "~/" ]


  set sr-extension-set? FALSE

  setup-patches
  setup-gis
  get-patch-scale
  setup-trees
  setup-monkeys

  set hr-size-final count patches with [pcolor = 58] / 100


;  output-files

;  create-legend

  set day 1
;  set midday 58 ; do not hardwire this one
  set midday simulation-time / 2
  set midday_start round ( p-timesteps-to-rest / 2 * simulation-time )
  set midday_end round ( ( 1 - p-timesteps-to-rest / 2) * simulation-time )
  type "*************** midday_start at: timestep " print midday_start
  type "*************** midday_end at: timestep " print midday_end

  set timestep 0
;  set gut_transit_time round (gut_transit_time)
;  set travel_speed travel_speed

  if gtt-param? = TRUE [
    if patch_type = "empirical" [
      if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set gut_transit_time 17 ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set gut_transit_time 18 ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set gut_transit_time 13 ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set gut_transit_time 19 ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Mar"[ set gut_transit_time 16 ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Apr"[ set gut_transit_time 16 ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set gut_transit_time 26 ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set gut_transit_time 21 ]
      if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set gut_transit_time 16 ]
    ]

    if patch_type = "generated" [
      ; *** STILL NEEDS TO SPECIFY RIPARIAN FORESTS (AS IT IS LOWER)
      set gut_transit_time 23.5 ; = rough estimate of mean gtt of Suzano (as we still haven't generated riparian forests)
      if patch-size-ha < 200 [ set gut_transit_time 16 ] ; = Santa Maria
      if patch-size-ha >= 200 [ set gut_transit_time 17 ] ; = rough estimate of mean gtt of Guareí
      if patch-size-ha > 2000 [ set gut_transit_time 16 ] ; = Taquara
    ]
  ]

  if p-forage-param? = TRUE [
    if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set p_foraging_while_traveling 0.36 ]
    if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set p_foraging_while_traveling 0.47 ]
    if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set p_foraging_while_traveling 0.54 ]
    if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set p_foraging_while_traveling 0.70 ]
    if study_area = "SantaMaria" AND feeding-trees-scenario = "Mar"[ set p_foraging_while_traveling 0.59 ]
    if study_area = "SantaMaria" AND feeding-trees-scenario = "Apr"[ set p_foraging_while_traveling 0.61 ]
    if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set p_foraging_while_traveling 0.31 ]
    if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set p_foraging_while_traveling 0.21 ]
    if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set p_foraging_while_traveling 0.21 ]
  ]

  print-parameters

  reset-ticks
end

; PATCHES
to setup-patches
;  ask patches with [pcolor = 105 ] [set pcolor green + 3]
end

; GIS
to setup-gis


  if patch-type = "empirical" [
    set-patch-size 3

    if study_area = "Guareí" [

      ifelse USER = "Eduardo" [
        ; load .prj and .asc (raster 10 x 10 m)
        gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei-poligono.prj" ; WGS_1984_UTM_Zone_22S
        set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei-poligono2_reproj.asc" ; fragment/study area raster (reprojected***)

        ; load the poligon (.shp) to determine forest and matrix patches
        set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp" ; fragment/study area polygon
      ][
        gis:load-coordinate-system word (local-path) "Model_Documentation/shapefiles-to-rasterize/Guarei-poligono.prj"
        set bb-gis gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/Guarei-poligono2_reproj.asc"
        set bb-gis-shp gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp"
      ]

    ]


    if study_area = "Suzano" [

      ifelse USER = "Eduardo" [
        ; load .prj and .asc (raster 10 x 10 m)
        gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.prj" ; WGS_1984_UTM_Zone_22S
        set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_rasterized_reproj.asc" ; fragment/study area raster (reprojected***)

        ; load the poligon (.shp) to determine forest and matrix patches
        set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp" ; fragment/study area polygon
      ][
        gis:load-coordinate-system word (local-path) "Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.prj"
        set bb-gis gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_rasterized_reproj.asc"
        set bb-gis-shp gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp"
      ]
    ]


    if study_area = "Taquara" [ ;;
      set-patch-size floor (0.8 * patch-size) ; Taquara large raster is too big for the world

      ifelse USER = "Eduardo" [
        ; load .prj and .asc (raster 10 x 10 m)
        gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only4_rec_rasterized_reproj.prj" ; WGS_1984_UTM_Zone_22S
        set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only4_rec_rasterized_reproj.asc" ; fragment/study area raster (reprojected***)

        ; load the poligon (.shp) to determine forest and matrix patches
        set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp" ; fragment/study area polygon
      ][
        gis:load-coordinate-system word (local-path) "Model_Documentation/shapefiles-to-rasterize/Taquara_only4_rec_rasterized_reproj.prj"
        set bb-gis gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/Taquara_only4_rec_rasterized_reproj.asc"
        set bb-gis-shp gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp"
      ]
    ]


    if study_area = "SantaMaria" [
      set-patch-size floor (1 * patch-size) ; SantaMaria large raster results in a large world

      ifelse USER = "Eduardo" [
        ; load .prj and .asc (raster 10 x 10 m)
        gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_recortado_rasterized_reproj.prj" ; WGS_1984_UTM_Zone_22S
        set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_recortado_rasterized_reproj.asc" ; fragment/study area raster (reprojected***)

        ; load the poligon (.shp) to determine forest and matrix patches
        set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp" ; fragment/study area polygon
      ][
        gis:load-coordinate-system word (local-path) "Model_Documentation/shapefiles-to-rasterize/SantaMaria_recortado_rasterized_reproj.prj"
        set bb-gis gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/SantaMaria_recortado_rasterized_reproj.asc"
        set bb-gis-shp gis:load-dataset word (local-path) "Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp"
      ]
    ]


    ; make each raster cell = patch in NetLogo
    let widt floor (gis:width-of bb-gis / 2)
    let heigh floor (gis:height-of bb-gis / 2)
    resize-world (-1 * widt ) widt (-1 * heigh ) heigh

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
  ]

  if patch-type = "generated" [

    set-patch-size 1
    resize-world 0 1250 0 1250
    import-world ( word path generated_patch )

    ask patches with [pcolor = 105 ] [set pcolor green + 3]

    ask patches with [pcolor = green + 1 OR pcolor = green + 3] [
      set habitat "forest"
      ;      set lc-val 1
    ]

    ask patches with [pcolor = 0] [
      set habitat "matrix"
      set pcolor yellow + 4
    ]

    ;    ask patches with [count neighbors with [pcolor = 0] >= 7 ] [
    ;      set habitat "matrix"
    ;    ]

    ask patches with [ habitat = "forest" AND count neighbors with [habitat = "matrix"] <= 7 AND count neighbors with [habitat = "forest"] <= 7 ] [
      set habitat "border"
      ;      set lc-val 1
    ]
  ]


  get-patch-scale

  reset-ticks

end

to get-patch-scale
  create-blobs 1

;  if patch-type = "generated" [ set patch-scale 10 ] ; all patches have 10 m

  if patch-type = "empirical" [
    ask one-of blobs [
      move-to patch 0 0

      set x_UTM (item 0 gis:envelope-of self)
      set y_UTM (item 2 gis:envelope-of self)
      set patch_before patch-here

      move-to patch 1 0

      let x_UTM2 (item 0 gis:envelope-of self)
      let y_UTM2 (item 0 gis:envelope-of self)

      set patch-scale ( x_UTM2 - x_UTM )
      die
    ]
  ]
end


; TREES INPUT
to setup-trees

  if patch-type = "generated" [
    ask feeding-trees [
      set size 3
      set shape "tree"
      set color green
      set visitations 0
;        setxy item 0 location item 1 location
      set species "general"
      set id-tree random 100000
;        if species = "" [ set species "NA" ]
;        if id-tree = "" [ set id-tree "NA" ]
      set x_scaled xcor * patch-scale         ; generated patches do not have coordinate projection
      set y_scaled ycor * patch-scale         ; generated patches do not have coordinate projection
    ]

    ask sleeping-trees [
      set size 3
      set shape "tree"
      set color magenta
      set visitations 0
;      setxy item 0 location-slp item 1 location-slp
      set id-tree random 100000
      set x_scaled xcor * patch-scale         ; generated patches do not have coordinate projection
      set y_scaled ycor * patch-scale         ; generated patches do not have coordinate projection
    ]
  ]


  if patch-type = "empirical" [
    ;; INPUT SLEEPING TREES OF ALL STUDY PERIOD INDEPENDENT OF MONTH:

    let id-tree-slp 0
    if ( sleeping-trees-scenario = "empirical" AND all-slp-trees? = TRUE )  [
      if ( study_area = "Guareí")  [ set sleep-file word ( local-path) "/Data/Resource-Trees/guarei_trees_unique_slp.shp" ]
      if ( study_area = "SantaMaria")  [ set sleep-file word ( local-path) "/Data/Resource-Trees/sma_trees_unique_slp.shp" ]
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
    if ( study_area = "Guareí" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/guarei_trees_unique_all.shp" ]
    if ( study_area = "Guareí" AND feeding-trees-scenario = "May" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/guarei_trees_unique_May.shp" ]
    if ( study_area = "Guareí" AND feeding-trees-scenario = "Jun" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/guarei_trees_unique_Jun.shp" ]
    if ( study_area = "Guareí" AND feeding-trees-scenario = "Jul" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/guarei_trees_unique_Jul.shp" ]
    if ( study_area = "Guareí" AND feeding-trees-scenario = "Aug" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/guarei_trees_unique_Aug.shp" ]

    ; SantaMaria
    if ( study_area = "SantaMaria" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/sma_trees_unique_all.shp" ]
    if ( study_area = "SantaMaria" AND feeding-trees-scenario = "Mar" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/sma_trees_unique_Mar.shp" ]
    if ( study_area = "SantaMaria" AND feeding-trees-scenario = "Apr" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/sma_trees_unique_Apr.shp" ]
    if ( study_area = "SantaMaria" AND feeding-trees-scenario = "May" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/sma_trees_unique_May.shp" ]

    ; Taquara
    if ( study_area = "Taquara" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_all.shp" ]
    if ( study_area = "Taquara" AND feeding-trees-scenario = "Jan" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_Jan.shp" ]
    ;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Feb" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_Feb.shp" ]
    ;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Apr" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_Apr.shp" ]
    ;  if ( study_area = "Taquara" AND feeding-trees-scenario = "May" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_May.shp" ]
    ;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Jul" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_Jul.shp" ]
    ;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Sep" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_Sep.shp" ]
    ;  if ( study_area = "Taquara" AND feeding-trees-scenario = "Dec" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/taq_trees_unique_Dec.shp" ]

    ; Suzano
    if ( study_area = "Suzano" AND feeding-trees-scenario = "All months" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/suz_trees_unique_all.shp" ]
    if ( study_area = "Suzano" AND feeding-trees-scenario = "Feb" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/suz_trees_unique_Feb.shp" ]
    if ( study_area = "Suzano" AND feeding-trees-scenario = "Apr" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/suz_trees_unique_Apr.shp" ]
    if ( study_area = "Suzano" AND feeding-trees-scenario = "Sep" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/suz_trees_unique_Sep.shp" ]
    if ( study_area = "Suzano" AND feeding-trees-scenario = "Dec" )   [ set tree-file word ( local-path) "Data/Movement/Resource-Trees/suz_trees_unique_Dec.shp" ]


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

  ]




end



; TAMARINS
to setup-monkeys

  set step_len_travel 0
  set step_len_forage 0
  set max_rel_ang_forage_75q 0
  set max_rel_ang_travel_75q 0

  create-monkeys 1
  ask monkeys [

    set enlvl1 energy_level_1
    set enlvl2 energy_level_2
;    set enstart start-energy
    set energy_stored energy_stored_val

    ; for the behaviorsequence plot
    set behaviorsequence []

    ; activity budget:
    set p_feeding 0
    set p_foraging 0
    set p_traveling 0
    set p_resting 0

    ; for home range calculation with the r extension
    set Name word "BLT_" ( [who] of self )
    set X_coords [] ;( list x_UTM )
    set Y_coords [] ;( list y_UTM )
    set day_list []

;    set shape "mlp-2"
;    set color black
    set size 2.5

    if sleeping-trees-scenario = "empirical" AND sleeping-trees? = FALSE [
      setxy random xcor random ycor
      set tree_current -1
    ]

    if sleeping-trees-scenario = "empirical" AND sleeping-trees? = TRUE [
      let start one-of sleeping-trees
      setxy [xcor] of start [ycor] of start
      set tree_current start
    ]

    if patch-type = "empirical" [
      set x_UTM (item 0 gis:envelope-of self)
      set y_UTM (item 2 gis:envelope-of self)

    ]
    ; for selecting ld_trees not randomly, but those that are distant from the home range center (this is a territoriality factor/influence)
;    let dist-all-trees
    set position-all-trees-x-list ( [xcor] of feeding-trees )
    set position-all-trees-y-list ( [ycor] of feeding-trees )

;        print position-all-trees-y-list

    set homerange-center patch (mean position-all-trees-x-list) (mean position-all-trees-y-list)
    ask homerange-center [ set pcolor red ask patches in-radius 3 [ set pcolor red ]  ]
    ask feeding-trees [ set dist-to-homerange-center distance [ homerange-center ] of myself ]

    set travel_mode "short_distance"
    set tree_target -1
    set ld_tree_target -1
    set tree_target_mem2 -1
    set patch_avoid_matrix patch-ahead step_len_travel
    set straight-line-to-target? TRUE ; this should be checked (travel procedure)

    set steps-moved 0
    set action-time 0
    set action "travel"
    set behavior ""
    set going-sleeping? FALSE
;    set energy start-energy
    set energy energy_level_1

    ; create empty lists
    set tree_ate_list []
    set tree_mem_list []
    set tree_add_list []
    set tree_pot_list []
    set seed_ate_list []
    set seed_mem_list []
    set seed_add_list []
    set seed_gtt_list []

    set travelmodelist []
    set DPL_d []
    set MR_d []
    set PT_d []

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


;  print step_len_travel

  if step-model-param? = TRUE [

    if patch-type = "generated" [
      ; ROUGH ESTIMATES OF VELOCITIES


      if patch-size-ha <= 200 [  ; ( = Guareí )
        set step_len_travel ( 24 / patch-scale ) ; ( BLT mean velocity in meters / patch resolution)
        set step_len_forage ( 13 / patch-scale ) ; ( BLT mean velocity in meters / patch resolution)
        set max_rel_ang_travel_75q 67
        set max_rel_ang_forage_75q 74
      ]
      if patch-size-ha > 200 AND patch-size-ha <= 2000 [ ; ( = Santa Maria, check Ponte Branca aftewards -> Gabi Rezende dissertation )
        set step_len_travel ( 34 / patch-scale ) ; ( BLT mean velocity in meters / patch resolution)
        set step_len_forage ( 18 / patch-scale ) ; ( BLT mean velocity in meters / patch resolution)
        set max_rel_ang_travel_75q 75
        set max_rel_ang_forage_75q 72
      ]
      if patch-size-ha > 2000 [  ; ( = Taquara, check Ponte Branca aftewards -> Gabi Rezende dissertation )
        set step_len_travel  ( 39.31 / patch-scale ) ; ( BLT mean velocity in meters / patch resolution)
        set step_len_forage ( 30.89 / patch-scale ) ; ( BLT mean velocity in meters / patch resolution)
        set max_rel_ang_travel_75q 17.85
        set max_rel_ang_forage_75q 43.02
      ]

    ]

    if patch-type = "empirical" [
      ;; Parameterizing BLT velocity with empirical data:
      ;option 1 (= mean velocities, Zanette et al 2021 ATBC) (I believe this underestimates tamarin velocities as our model only uses travelling speed
      ;  if study_area = "Guareí" [ set travel_speed ( 15.27 / 10 ) ]   ; 15.27 m / 10 m ( BLT mean velocity / patch resolution)
      ;  if study_area = "SantaMaria" [ set travel_speed ( 18.4 / 10 ) ]
      ;  if study_area = "Taquara" [ set travel_speed ( 23.24 / 10 ) ]
      ;  if study_area = "Guareí" [ set travel_speed ( 8.93 / 10 )  ]

      ;    ;option 2 (= 3 steps before feeding on fruits, Zanette et al 2021 ATBC)
      ;    if study_area = "Guareí" [ set travel_speed ( 20 / 10 ) ]   ; 15.27 m / 10 m ( BLT mean velocity / patch resolution)
      ;    if study_area = "SantaMaria" [ set travel_speed ( 20.2 / 10 ) ]
      ;    if study_area = "Taquara" [ set travel_speed ( 33.4 / 10 ) ]
      ;    if study_area = "Suzano" [ set travel_speed ( 13.7 / 10 )  ]
      ;  ]

      ;option 3 (= Data/Parameter-table.csv)


      ; travel velocity
      if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set step_len_travel ( 23.43 / 10 ) ]   ; 15.27 m / 10 m ( BLT mean velocity / patch resolution)
      if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set step_len_travel ( 25.44 / 10 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set step_len_travel ( 25.20 / 10 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set step_len_travel ( 25.30 / 10 ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Mar"[ set step_len_travel ( 32.37 / 10 ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Apr"[ set step_len_travel ( 35.97 / 10 ) ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set step_len_travel ( 17.94 / 10 )  ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set step_len_travel ( 17.49 / 10 )  ]
      if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set step_len_travel ( 39.31 / 10 ) ]

;        print step_len_travel

      ; forage velocity
      if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set step_len_forage ( 14.06 / 10 ) ]  ; ( BLT mean velocity / patch resolution)
      if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set step_len_forage ( 12.14 / 10 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set step_len_forage ( 12.93 / 10 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set step_len_forage ( 13.87 / 10 ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Mar"[ set step_len_forage ( 16.95 / 10 ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Apr"[ set step_len_forage ( 21.3 / 10 ) ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set step_len_forage ( 7.51 / 10 )  ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set step_len_forage ( 8.83 / 10 )  ]
      if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set step_len_forage ( 30.89 / 10 ) ]

      ; travel angle
      if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set max_rel_ang_travel_75q ( 67.86 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set max_rel_ang_travel_75q ( 75.63 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set max_rel_ang_travel_75q ( 72.75 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set max_rel_ang_travel_75q ( 59.53 ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Mar"[ set max_rel_ang_travel_75q ( 68.99 ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Apr"[ set max_rel_ang_travel_75q ( 58.76 ) ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set max_rel_ang_travel_75q ( 63.61 )  ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set max_rel_ang_travel_75q ( 47.53 )  ]
      if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set max_rel_ang_travel_75q ( 17.85 ) ]

      ; forage angle
      if study_area = "Guareí" AND feeding-trees-scenario = "May"[ set max_rel_ang_forage_75q ( 68.98 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jun"[ set max_rel_ang_forage_75q ( 78.99 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jul"[ set max_rel_ang_forage_75q ( 75.66 ) ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Aug"[ set max_rel_ang_forage_75q ( 77.22 ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Mar"[ set max_rel_ang_forage_75q ( 89.73  ) ]
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Apr"[ set max_rel_ang_forage_75q ( 63.00) ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Sep"[ set max_rel_ang_forage_75q ( 55.92  )  ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Dec"[ set max_rel_ang_forage_75q ( 51.20 )  ]
      if study_area = "Taquara" AND feeding-trees-scenario = "Jan"[ set max_rel_ang_forage_75q ( 43.02 ) ]

    ]
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





;--------------------------------------------------------------------------------------------
; Activities commands
;--------------------------------------------------------------------------------------------
to go

  ; to print on the R console during multiple simulations:
  if ticks = 1 [  type "running area = " type study_area type ", month = " print feeding-trees-scenario ]

  if ticks > 10000 [stop]

  get_stored_energy

  ask monkeys [
    if energy <= 0 AND energy_stored <= 0 [
      set survived? "no"
      ifelse day > 3 [
        print "calculating from GO"
        calc-movement-dead
        store-as-globals
        ; calc resource metrics
        NNdist
        SDDcalc
        calc-seed-aggregation
      ][
      print "not enough days to calculate movement and seed dispersal metrics"
      ]

;      set day no_days + 999 ; to achieve the stop condition of nlrx
    ; only after calculating all variables and storing as globals otherwise nlrx does not take monkey variables. Instead, stop]
    die
    stop
    ]

  ]

;  ask monkeys [
;    if energy < 0 [  ; if monkeys are dead
;      if day > 3 [
;        print "FROM THE GO PROCEDURE"
;        calc-movement-dead
;        store-as-globals
;      ]
;      set survived? "no"
;      stop
;    ]
;  ]

  if all? monkeys [action = "sleeping"] [
    set day day + 1
    set timestep 0
    ask monkeys [
      set action "travel"
    ]
    if day > no_days [ ; if the simulation has ended
      ask monkeys [ set action "" ]
      output-print "run-days click finished"

      start-r-extension

      output-print "calculating home range with r extension"
      calc-homerange
      output-print "home range calculation with r extension finished"

      output-print "calculating activity budget"
      calc-activity-budget
      output-print "calculating activity budget finished"

      output-print "calculating other movement metrics"
      calc-movement-metrics ; these are being estimated within calc-homerange
      output-print "calculating movement metrics finished"

      ; calc near neighbor distance (in NetLogo)
      NNdist
      SDDcalc

      output-print "calculating R index for seeds"
      calc-seed-aggregation
      output-print "calculating R index for seeds finished"

      set survived? "yes" ; tamarins are alive by the end of the run

      ask monkeys [ store-as-globals ]


      stop
    ]
  ]

  move-monkeys

;  ask monkeys [ set behaviorsequence lput behavior behaviorsequence ]

  set timestep timestep + 1
  tick

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


to print-step
  ask monkeys [
    type "---- STEP ---- " print timestep
    type " ---- MODE: " print travel_mode
    type "tree_target: " type tree_target type " "
    type "ld_tree_target: " type ld_tree_target type " "
    type "tree_current: " type tree_current type " "
    type "behavior: " type behavior type " "
    type "action: " print action
    ;      type "tree_pot_list: " print length tree_pot_list print tree_pot_list
    ;      type "tree_ate_list: " print length tree_ate_list print tree_ate_list
    ;      type "tree_mem_list: " print length tree_mem_list print tree_mem_list
    ;    type "tree_add_list " print length tree_add_list print tree_add_list
    type "action-time: " print action-time
    type "energy: " print energy
    ;      if patch-type = "empirical" [ type "x: " print x_UTM type "y: " print y_UTM ]
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
    ;      print " ----------------step end------------------------"
  ]

end


to step ; FOR DEBUG PURPOSES ONLY

  ask monkeys [
    if energy <= 0 AND energy_stored <= 0 [
      set survived? "no"
      die
      stop
    ]
  ]

  if not any? monkeys [
    print "MONKEY IS DEAD!"
    stop
  ]

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

  get_stored_energy

  repeat 1 [ move-monkeys ]
  set timestep timestep + 1
  tick

  ;; DEBUGGING
  if print-step? = TRUE [
    print-step
  ]

end


;-------------------------------------------------------------
to run_days


  repeat no_days [ next_day ]

end


;-------------------------------------------------------------
to next_day

  if not any? monkeys [
    print "MONKEY IS DEAD!"
    stop
  ]

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
;    if print-step? = TRUE [
;      print-step
;    ]

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

    if energy < 0 AND energy_stored <= 0 [
      if day > 3 [
        print "calculating from move-monkeys"
        calc-movement-dead
        store-as-globals
      ]
      set survived? "no"
;      set day no_days + 999 ; to achieve the stop condition of nlrx
      stop
      ; only after calculating all variables and storing as globals otherwise nlrx does not take monkey variables. Instead, stop
      die


    ]


    ; for home range calculation with r extension:
    if patch-type = "empirical" [
      set X_coords lput x_UTM X_coords
      set Y_coords lput y_UTM Y_coords
    ]
    if patch-type = "generated" [
      set x_scaled (xcor * patch-scale)         ; generated patches do not have coordinate projection
      set y_scaled (ycor * patch-scale)         ; generated patches do not have coordinate projection
      set X_coords lput (xcor * patch-scale) X_coords
      set Y_coords lput (ycor * patch-scale) Y_coords
    ]


    ; Avoid matrix (for other implementations, check v1.1_matrix-avoidance branch):
;    avoid-matrix



  ;; BLT ROUTINE

    if timestep = 0 [
;      set energy_stored energy_stored + (energy - start-energy)
;      set energy start-energy ; we want the tamarins to flutuate between level 1 and 2; only take this out if you calibrate the energy values
      ; removing start-energy from the model (2023-07-11d):
      set energy_stored energy_stored + (energy - energy_level_1)
      set energy energy_level_1 ; we want the tamarins to flutuate between level 1 and 2; only take this out if you calibrate the energy values
      set tree_current -1
      set going-sleeping? FALSE
;      remove_trees_surrounding ; to avoid feeding in the closest tree
    ]

    if timestep = random 2 [ ; morning-defecation can happen a few timesteps after waking up (check Param-table.csv)
      morning-defecation
    ]

    if timestep >= (0.95 * simulation-time) [
      ;if timestep = simulation-time [
      ;set tree_target -1
      ;set tree_target_dist 0
      set going-sleeping? TRUE
      ;] ; force monkey select a sleeping site
      ;print " ------------ CALLING FROM set going-sleeping? TRUE ----------- "
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


    if timestep < simulation-time AND going-sleeping? = FALSE [ ; energy levels: energy_level_1 = 80 and energy_level_2 = 150
      ; modulate memory_list
      enhance_memory_list

      ; keep track of distance to the target
      if tree_target = nobody [ enhance_memory_list search-feeding-tree ]
      if tree_target != -1 [ set tree_target_dist distance tree_target ]

;      print "CHECKPOINT HERE"

      ifelse energy < energy_level_1 [ ; energy < level 1
        set travel_mode "short_distance"
        if ld_tree_target = tree_target [
          if ld_tree_target != -1 [ set tree_target_mem2 ld_tree_target ]            ; to avoid losing the ld_tree_target on its way
          set tree_target -1 ; remove tree_target when coming from "long_distance"
          set ld_tree_target -1  ; if this is not here it will make the tamarin lose the target very close to the tree when coming from long distance bc of the condition ld_tree_target = tree_target (Ronald debugged on the 14th of July 2022)
        ]
;        print " **** BEING CALLED FROM FRUGIVORY 1 ***** "
        frugivory
      ][ ; energy > level 1
;        print "CHECKPOINT HERE"
;        ifelse (action = "feeding" or action = "travel") [              ;; v1.0 version
        ifelse (travel_mode = "short_distance" ) [                       ;; v1.1 version (long and short-distance travel)
          ifelse energy > energy_level_2 [ ; energy > level 2 ==> other activities
            if tree_current = -1 [
             set travel_mode "long_distance"
;              print "TEST HERE ****** "
            ]
            ifelse (timestep > midday_start AND timestep < midday_end) [
;              print "resting 1"
              resting
            ][
              random-action
            ]
          ][ ; energy_level_1 < energy < energy_level_2
;            print " **** BEING CALLED FROM FRUGIVORY 2 ***** "
            frugivory
          ] ;; energy > level 2 ==> other activities
        ][ ; travel_mode = "long_distance"

;          print "CHECKPOINT HERE A"

          ifelse random (duration) < action-time [ ; action time for other than feeding
            ; set ld_tree_target -1  ;; commented out by Ronnald on 14th of July ;; has to be set to -1 otherwise tamarins will come back in the next ld travel cycle
;            print " *********** 2"

            ; if tamarins reached up to 2*duration resting, make energy come back to energy_level_1 (suppose they have stored it as glucogen or similar)
            set action-time 0 ; restart the resting counter (action-time)
;            store_energy
;            print " **** BEING CALLED FROM FRUGIVORY 3 ***** "
            frugivory
          ][
;            print " LAST-ACTION-AGAIN"
;            set action-time action-time + 1
            last-action-again
          ]

        ]
      ] ;; energy > level 1
      forget_trees
      defecation


      ifelse on-feeding-tree? = TRUE [
        store_energy ; tamarins have arrived and they are gonna feed. Thus, store the extra energy they have
        ; movement and x_ and y_UTM setting is done in the on-feeding-tree? reporter (to avoid target errors)
      ][
        ; if tamarins are not in the target trees, just update its x_ and y_UTM
        if patch-type = "empirical" [
          set x_UTM (item 0 gis:envelope-of self)
          set y_UTM (item 2 gis:envelope-of self)
        ]
      ]

      set DPL DPL + dist-traveled

    ] ; end of daily routine

    set day_list lput day day_list

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

;  print "I'm in the middle of the sugarcane!"
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
;    print "there's some matrix in front of me!"
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

;  print "FRUGIVORY BEING CALLED *******"  ; debugging

  avoid-patch-set ; bump on the territory borders


  if travel_mode = "short_distance" [   ;; short distance frugivory
    set travelmodelist lput 1 travelmodelist ; to the travel mode histogram
;    type "ON-FEEDING-TREE? = " print on-feeding-tree?
    ifelse on-feeding-tree? [
      ifelse species_time > frugivory-time [
;      ifelse random (2 * species_time ) > frugivory-time [
;        print "SD_I'm feeding 1!" ; for debugging
        feeding
      ][
        set tree_current -1
;        print "SD_time over -- New feeding tree" ; for debugging
        to-feeding-tree
      ]
    ][
;      print "SD_New feeding tree" ; for debugging
      to-feeding-tree
    ]
  ]

  if travel_mode = "long_distance" [    ;; long distance frugivory
    set travelmodelist lput 2 travelmodelist ; to the travel mode histogram
    ifelse on-feeding-tree? [
      ifelse species_time > frugivory-time [
;      ifelse random (2 * species_time ) > frugivory-time [
;        print "LD_I'm feeding! 2" ; for debugging
        feeding
      ][
        set tree_current -1
;        print "LD_time over -- New feeding tree" ; for debugging
        to-feeding-tree
      ]
    ][
;      print "LD_New long distance feeding tree" ; for debugging
      to-feeding-tree
    ]
  ]

end


;----------------------------------------

to-report on-feeding-tree?


  if travel_mode = "short_distance" [   ;; short distance frugivory
    ifelse action = "travel" OR action = "forage" AND tree_target != -1 [
;      type "on-feeding-tree? reporter distance to target : "
;      print distance tree_target ; for debugging
      ifelse distance tree_target <  step_len_travel [

;        print "HERE ***************"

        set tree_current tree_target

        set dist-traveled dist-traveled + distance tree_target
        move-to tree_target

        ; make UTM of tamarins match UTM of trees (like empirical data collection):
        if patch-type = "empirical" [
          set x_UTM [ x_UTM ] of tree_current
          set y_UTM [ y_UTM ] of tree_current
        ]
        ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
        set xcor [xcor] of tree_current + 0.01
        set ycor [ycor] of tree_current + 0.01

        ask tree_target [ set visitations visitations + 1 ]
        set tree_target -1
        ifelse feedingbout-on?
        [ set species_time [ species_time ] of tree_current ]
;        [ set species_time duration ] ;; duration = 2 is the most common value over all species, but as there's a random variation on the 'random (2 * species_time), I'll leave it as the same as duration
        [ set species_time species_time_val ]
;        type "tree_current: " print tree_current
;        type "tree_target: " print tree_target
;        print "on-feeding-tree? TRUE" ; for debugging
;        print "short distance: ON tree 1"
        report true

      ][
;        print "on-feeding-tree? FALSE" ; for debugging
;        print tree_target
;        print "short distance: NOT on tree 1"
        report false
      ]
    ][
;      print "Action != travel OR tree_target = -1" ; for debugging
      ifelse action = "feeding" [
;        print "short distance: ON tree 2"
        report true
      ][
;        print "short distance: NOT on tree 2"
        report false
      ]
    ]
  ]


  if travel_mode = "long_distance" [    ;; long distance frugivory

    ifelse action = "travel" OR action = "forage" AND ld_tree_target != -1 [
;      type "on-feeding-tree? reporter distance to target : "
;      print distance ld_tree_target ; for debugging
      ifelse distance ld_tree_target < 0.8 * step_len_travel [
;        print "distance to ld_tree_target is < 80%"

        set tree_current ld_tree_target

        set dist-traveled dist-traveled + distance ld_tree_target
        move-to ld_tree_target   ; agent coordinates are going to be matched outside of on-feeding-tree? reporter

        ; make UTM of tamarins match UTM of trees (like empirical data collection):
        if patch-type = "empirical" [
          set x_UTM [ x_UTM ] of tree_current
          set y_UTM [ y_UTM ] of tree_current
        ]
        ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
        set xcor [xcor] of tree_current + 0.01
        set ycor [ycor] of tree_current + 0.01

        ask ld_tree_target [ set visitations visitations + 1 ]

        set tree_target -1
        if ld_tree_target != -1 [ set tree_target_mem2 ld_tree_target ]            ; to avoid losing the ld_tree_target on its way
        set ld_tree_target -1

;        set tree_target ld_tree_target ;; IMPORTANT FOR NOT HAAVING TO CHANGE ALL THE FEEDING PROCESS
        ifelse feedingbout-on?
        [ set species_time [ species_time ] of tree_current ]
;        [ set species_time duration ] ;; duration = 2 is the most common value over all species, but as there's a random variation on the 'random (2 * species_time), I'll leave it as the same as duration
        [ set species_time species_time_val ]
;        print "long distance: ON tree 1"
        report true
      ][
;       print "long distance: NOT on tree 1"
        report false
      ]
    ][
;      print "Action != travel OR tree_target = -1" ; for debugging
      ifelse action = "feeding" [
;        print "long distance: ON tree 1"
        report true
      ][
;        print "long distance: NOT on tree 2"
        report false
      ]
    ]
  ]
end

;----------------------------------------

to feeding
;  print " ==> FEEDING IS HAPPENING ===================="    ; debugging
;  if travel_mode = "long_distance" [ print "FEEDING IS HAPPENING" ]

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

;  if tree_current = -1 [ print "========== tree_current = -1 ============" ]

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
;    remove_trees_surrounding
  ]


  ; consume seeds:
  set seed_ate_list lput [who] of tree_current seed_ate_list
  set seed_mem_list lput 0 seed_mem_list
  set seed_add_list lput 1 seed_add_list


  ; if the model has parameterized gtt, add also the gtt for each seed based on empirical gtt distribution to the seed_gtt_list
  if ( gtt-param? = TRUE ) [

    if patch-type = "empirical" [

      ; if you want parameterized gtt values (using values on Data/Param-table.csv)
      if study_area = "Guareí" AND feeding-trees-scenario = "May"  [ set seed_gtt_list lput random-poisson 13 seed_gtt_list ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jun"  [ set seed_gtt_list lput random-poisson 18 seed_gtt_list ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Jul"  [ set seed_gtt_list lput random-poisson 17 seed_gtt_list ]
      if study_area = "Guareí" AND feeding-trees-scenario = "Aug"  [ set seed_gtt_list lput random-poisson 19 seed_gtt_list ]

      if study_area = "SantaMaria" AND feeding-trees-scenario = "Mar"  [ set seed_gtt_list lput random-poisson 16.6 seed_gtt_list ] ;this value is not empirical, it is estimated
      if study_area = "SantaMaria" AND feeding-trees-scenario = "Apr"  [ set seed_gtt_list lput random-poisson 16 seed_gtt_list ]

      if study_area = "Suzano" AND feeding-trees-scenario = "Sep"  [ set seed_gtt_list lput random-poisson 26 seed_gtt_list ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Dec"  [ set seed_gtt_list lput random-poisson 21 seed_gtt_list ]
      if study_area = "Suzano" AND feeding-trees-scenario = "Feb"  [ set seed_gtt_list lput random-poisson 21 seed_gtt_list ] ; = September value
      if study_area = "Suzano" AND feeding-trees-scenario = "Apr"  [ set seed_gtt_list lput random-poisson 21 seed_gtt_list ] ; = September value


      if study_area = "Taquara" AND feeding-trees-scenario = "Jan"  [ set seed_gtt_list lput random-poisson 16 seed_gtt_list ]
    ]


    if patch-type = "generated" [ set seed_gtt_list lput random-poisson gut_transit_time seed_gtt_list ]


  ]

  ask feeding-trees with [color = yellow] [ set color green set size 3 if study_area = "Taquara" [set size 5] ]

end

;-----------------------------------------
;to remove_trees_surrounding
;  ;; HERE REMOVE ADDITIONAL TREES within the visual range of the tamarins (1 patch = 10 m)
;  let trees_remove filter [ s -> member? s [ who ] of feeding-trees with [ distance myself < visual] ] tree_pot_list
;
;;  ;; testing agentset selected by visual:
;;  ; Option 1) (does not work properly)  (trees_remove is just the who number and require more code):
;;  let trees_remove_agents feeding-trees with [ distance myself < visual]
;;  type "+++++++TREES SURROUNDING: " print trees_remove_agents
;;  type "======= trees_remove: " print trees_remove
;;
;;  foreach trees_remove [ x -> ask patch-here [ ask neighbors [ set pcolor red] ] ]
;;
;;   Option 2) (does not work) asking the agentset:
;;  ask trees_remove_agents [
;;    ask patch-here [ set pcolor red]
;;  ]
;
;  let l length trees_remove
;;  print l ; for debugging
;  while [ l > 0 ]
;  [
;    set l l - 1
;    set tree_pot_list remove-item ( position (item l trees_remove) tree_pot_list ) tree_pot_list
;    set tree_ate_list lput (item l trees_remove) tree_ate_list
;    set tree_mem_list lput 0 tree_mem_list
;    set tree_add_list lput 1 tree_add_list
;  ]
;end

to enhance_memory_list

  ;; make pot_list increase again if it is too small (otherwise will return an error) -> revisitation to trees is more common when primates are in small fragments (less trees availble) (Boyle et al 2009);
  ;; don't make prop_trees_to_reset_memory bigger than 8 otherwise the potential list will get very very small (high chances to return an error)
    let n_trees round ( count feeding-trees  / prop_trees_to_reset_memory ) + 3 ; don't know what should be the number exactly. The smaller it is, more the tamarins will travel around to find other trees in the pot_list while avoiding returning to visited trees ;
;  let n_trees round ( count feeding-trees  / prop_trees_to_reset_memory ) ; testing on 2023-07-04d: removed '- 2' (line above)
;  type "length tree_pot_list before = " print length tree_pot_list
;  type "length tree_ate_list before = " print length tree_ate_list
;  type "n_trees = " print n_trees

  ; modulating memory procedure
  if ( length tree_pot_list <= n_trees ) [
    print "ENHANCING MEMORY"

    ifelse length tree_ate_list >= n_trees [
      ;if there's enough trees available in the eaten list (more than n_trees), they will take 0 to n_trees out of list to put it back in the potential list and reconsider it to visit
      set tree_bucket sublist tree_ate_list ( 0 ) ( n_trees )
      print "CASE 1"

      ; enhance potential list (taking trees from the ate_list and putting it back in the potential list)
      ( foreach tree_bucket [ ax -> set tree_pot_list lput ax tree_pot_list ] )

      ; reduce ate_list and accessory lists (mem_list and add_list)
      set tree_mem_list sublist tree_mem_list ( n_trees ) ( length tree_mem_list)
      set tree_add_list sublist tree_add_list ( n_trees ) ( length tree_add_list)
      set tree_ate_list sublist tree_ate_list ( n_trees ) ( length tree_ate_list)

    ][
      ;if there's not, they will take all the list and put back in the potential list
      set tree_bucket sublist tree_ate_list ( 0 ) ( length tree_ate_list)
      print "CASE 2"

      ; enhance potential list (taking trees from the ate_list and putting it back in the potential list)
      ( foreach tree_bucket [ ax -> set tree_pot_list lput ax tree_pot_list ] )

      ; reduce ate_list and accessory lists (mem_list and add_list)
      set tree_mem_list sublist tree_mem_list ( 0 ) ( length tree_mem_list)
      set tree_add_list sublist tree_add_list ( 0 ) ( length tree_add_list)
      set tree_ate_list sublist tree_ate_list ( 0 ) ( length tree_ate_list)

    ]
;        print tree_bucket

    set tree_bucket [] ; empty the list

  ]

;  ; modulating while [] loop in search-feeding-tree procedure for finding a new tree on the border
;  let n_trees2 round ( p_disputed_trees * count feeding-trees ) - 1
;  print n_trees2
;
;    if ( length tree_pot_list < n_trees2 ) [
;    print "ENHANCING MEMORY TO FIND BORDER TREES"
;;    set tree_bucket sublist tree_ate_list ( 0 ) ( n_trees2 )
;    ifelse length tree_ate_list >= n_trees2 [
;      set tree_bucket sublist tree_ate_list ( 0 ) ( n_trees2 )
;    ][
;      set tree_bucket sublist tree_ate_list ( 0 ) ( length tree_ate_list)
;    ]
;;        print tree_bucket
;
;    ; enhance potential list
;    ( foreach tree_bucket [ ax -> set tree_pot_list lput ax tree_pot_list ] )
;
;    ; reduce mem_list and add_list
;    set tree_mem_list sublist tree_mem_list ( n_trees2 ) ( length tree_mem_list)
;    set tree_add_list sublist tree_add_list ( n_trees2 ) ( length tree_add_list)
;    set tree_ate_list sublist tree_ate_list ( n_trees2 ) ( length tree_ate_list)
;
;    set tree_bucket [] ; empty the list
;
;  ]


;  print "called enhance_memory_list"
;  type "length tree_pot_list after = " print length tree_pot_list
;  type "length tree_ate_list after = " print length tree_ate_list
end


;-----------------------------------------

to to-feeding-tree

;  print "TO-FEEDING-TREE"
;  if tree_target != -1 [ type "distance to target = " print distance tree_target ]


  if travel_mode = "short_distance" [
    if tree_target = -1 [

;      ; check if there is an old target on memory:
      if tree_target_mem2 != -1 [ set ld_tree_target tree_target_mem2 ]

      set frugivory-time 0
      search-feeding-tree
      if tree_target = -1 [ print "SEARCH FEEDING TREE FAILED" stop ]
    ]

    ;    if on-feeding-tree? = FALSE [ set heading towards tree_target ] ; to avoid the same point (x,y) error
    if straight-line-to-target? = TRUE [ ; otherwise it might enter the matrix
      if distance tree_target != 0 [ set heading towards tree_target ]
    ]

    ; the first part of the following ifelse was created because Taquara group would circle around the trees because their timesteps were too large and
    ; they would frequently circumvent the tree. This happened because the rule of "approaching the tree" was 'if distance tree_target < 0.8 step length', which was
    ; basically not relevant for other groups. But on Taquara's case, 0.8 of the step lengh is almost 30 m, which we find relevant. Thus, if the distance to the
    ; next feeding-tree is lower than a step legnth, a random angle is not drawn and the tamarins move straight to it.
    ; The code is basically the same as the one on-feeding-tree? reporter
    ifelse distance tree_target < step_len_travel [
;      print "NEW PROCEDURE"

      set tree_current tree_target
      set dist-traveled dist-traveled + distance tree_target
      move-to tree_target

      ; make UTM of tamarins match UTM of trees (like empirical data collection):
      if patch-type = "empirical" [
        set x_UTM [ x_UTM ] of tree_current
        set y_UTM [ y_UTM ] of tree_current
      ]
      ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
      set xcor [xcor] of tree_current + 0.01
      set ycor [ycor] of tree_current + 0.01

      ask tree_target [ set visitations visitations + 1 ]
      set tree_target -1
      ifelse feedingbout-on?
      [ set species_time [ species_time ] of tree_current ] ; Dec 2022: this procedure is wrong because species_time is a global
      ;        [ set species_time duration ] ;; duration = 2 is the most common value over all species, but as there's a random variation on the 'random (2 * species_time), I'll leave it as the same as duration
      [ set species_time species_time_val ]
      ;      type "tree_current: " print tree_current
      ;      type "tree_target: " print tree_target
      ;        print "on-feeding-tree? TRUE" ; for debugging
;      print "ON tree NEW PROCEDURE"

      set action "feeding"

      set energy energy + ( energy-loss-traveling * dist-traveled )

    ][
      ifelse ( action = "travel" OR action = "forage" ) [

        ifelse ( random-float 1 < p_foraging_while_traveling ) [

          if tree_target != -1 AND distance tree_target > step_len_forage [ ; otherwise it migh forage for 3 sequential steps while arriving in the feeding tree
            forage
            set action "forage"
            set behavior "forage"
            ;; RANDOM movement while foraging:
            if step-model-param? = TRUE  AND distance tree_target > 1.5 * step_len_travel [
              rt ( random max_rel_ang_forage_75q ) - ( random max_rel_ang_forage_75q )  ; feeding is less directed than travel
            ]
            travel ; because the travel procedure has an option for forage type of travel
            set behaviorsequence lput 3 behaviorsequence
          ]

        ][

          if step-model-param? = TRUE  AND distance tree_target > 1.5 * step_len_travel [
            rt ( random max_rel_ang_travel_75q ) - ( random max_rel_ang_travel_75q)  ; travel is more directed than foraging, so we don't divide the max-random-angle
          ]
          travel
          set action "travel"
          set behavior "travel"
          set behaviorsequence lput 3 behaviorsequence

        ]
      ][
        if step-model-param? = TRUE  AND distance tree_target > 1.5 * step_len_travel [ ; keep step_len_travel here as it represents the real movement capacity when tamarins are arriving at a tree
          rt ( random max_rel_ang_travel_75q ) - ( random max_rel_ang_travel_75q )  ; travel is more directed than foraging, so we don't divide the max-random-angle
        ]
        travel
        set action "travel"
        set behavior "travel"
        set behaviorsequence lput 3 behaviorsequence

      ]

    ]

  ]



  if travel_mode = "long_distance" [
    if ld_tree_target = -1 [

      ; check if there is an old target on memory. If there is, take it as ld_tree_target and delete from target_mem2
      if tree_target_mem2 != -1 [
        set ld_tree_target tree_target_mem2
        set tree_target_mem2 -1
      ]

;      print "LD_1"

      set frugivory-time 0
      search-feeding-tree
    ]

    ;    if on-feeding-tree? = FALSE [ set heading towards ld_tree_target ] ; to avoid the same point (x,y) error
    if straight-line-to-target? = TRUE [ ; otherwise it might enter the matrix
      face ld_tree_target
    ]

    ifelse distance ld_tree_target < step_len_travel [
;      print "LD_NEW PROCEDURE"

      set tree_current ld_tree_target
      set dist-traveled dist-traveled + distance ld_tree_target
      move-to ld_tree_target

      ; make UTM of tamarins match UTM of trees (like empirical data collection):
      if patch-type = "empirical" [
        set x_UTM [ x_UTM ] of tree_current
        set y_UTM [ y_UTM ] of tree_current
      ]
      ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
      set xcor [xcor] of tree_current + 0.01
      set ycor [ycor] of tree_current + 0.01

      ask ld_tree_target [ set visitations visitations + 1 ]
      set ld_tree_target -1
      set tree_target_mem2 -1  ; it was visited already

      ifelse feedingbout-on?
      [ set species_time [ species_time ] of tree_current ] ;Dec 2022: this procedure is wrong because species_time is a global
      ;        [ set species_time duration ] ;; duration = 2 is the most common value over all species, but as there's a random variation on the 'random (2 * species_time), I'll leave it as the same as duration
      [ set species_time species_time_val ]
      ;      type "tree_current: " print tree_current
      ;      type "tree_target: " print tree_target
      ;        print "on-feeding-tree? TRUE" ; for debugging
;      print "ON tree NEW PROCEDURE LD"

      set action "feeding"

;      print "I AM HERE!!!! 1"

      set energy energy + ( energy-loss-traveling * dist-traveled )

    ][

;      print "I AM HERE!!!! 2"

      ifelse ( action = "travel" OR action = "forage" ) [

        ifelse ( random-float 1 < p_foraging_while_traveling ) [

          if ld_tree_target != -1 AND distance ld_tree_target > step_len_forage [ ; otherwise it migh forage for 3 sequential steps while arriving in the feeding tree
            forage
            set action "forage"
            set behavior "forage"
            ;; RANDOM movement while foraging:
            if step-model-param? = TRUE  AND distance ld_tree_target > 1.5 * step_len_travel [ ; keep step_len_travel here as it represents the real movement capacity when tamarins are arriving at a tree
              rt ( random max_rel_ang_forage_75q ) - ( random max_rel_ang_forage_75q )  ; feeding is less directed than travel
            ]
            travel ; because the travel procedure has an option for forage type of travel
            set behaviorsequence lput 2 behaviorsequence
          ]

;          print "I AM HERE!!!! forage"

        ][

          if step-model-param? = TRUE  AND distance ld_tree_target > 1.5 [
            rt ( random max_rel_ang_travel_75q ) - ( random max_rel_ang_travel_75q )  ; travel is more directed than foraging, so we don't divide the max-random-angle
          ]
          travel
          set action "travel"
          set behavior "travel"
          set behaviorsequence lput 3 behaviorsequence

;          print "I AM HERE!!!! travel"

        ]
      ][
        travel
        set action "travel"
        set behavior "travel"
        set behaviorsequence lput 3 behaviorsequence

;        print "I AM HERE!!!! travel2"

      ]
    ]

  ]

;  print "I AM HERE!!!! 3"



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

;   print "SEARCHING FEEDING TREE PROCEDURE"

  ask feeding-trees with [color = red OR color = blue] [ set color green ]  ; make last target (short or long distance) green again

  if travel_mode = "short_distance" [
    let let_pot_list tree_pot_list

    set tree_target min-one-of feeding-trees with [member? who let_pot_list] [distance myself] ;; CLOSEST TREE
    ask tree_target [ set color red ]    ; make close tree target red
    set tree_target_species [ species ] of tree_target
;    print tree_target   ; debugging
  ]




  if travel_mode = "long_distance" [
    let let_pot_list tree_pot_list

    if ld-target-random? = TRUE [
      ;; RANDOM TREE
      ; check if there was a ld_target before
      ifelse tree_target_mem2 = -1 [
        set ld_tree_target one-of feeding-trees with [member? who let_pot_list]
;        print "random ld target defined 1"
      ] [
        set ld_tree_target tree_target_mem2
        set tree_target_mem2 -1 ; reset
;        print "last ld target reutilized 1"
      ]

    ]


    if ld-target-random? = FALSE [
      ;; RANDOM TREE AT THE BORDER OF THE HOME RANGE (TERRITORIALITY)
      set let_pot_list tree_pot_list
      set candidate_ld_targets feeding-trees with [member? who let_pot_list]
;      type "candidate_ld_targets = " print candidate_ld_targets


;      print "LD_CHECK"


      ; DROPPED OUT (2023-07-09d): this procedure was causing lots of errors for the sensitivity analysis (stress test).
      ; Now enhance_memory_list is called only in the end of this procedure (if tree_target = nobody)
;      ; while loops are always dangerous:
;       while [ count candidate_ld_targets <= round ( p_disputed_trees * count feeding-trees ) - 1 ] [
;        print "LD_CHECK 2"
;        enhance_memory_list
;        set let_pot_list tree_pot_list
;        set candidate_ld_targets feeding-trees with [member? who let_pot_list]
;        print "increased candidate_ld_targets"
;        type "candidate_ld_targets = " print count candidate_ld_targets
;      ] ; if available trees are few, enhance memory list

      set candidate_ld_targets max-n-of ( round (p_disputed_trees * (length let_pot_list) ) ) candidate_ld_targets [dist-to-homerange-center]
      ; debugging:
;      ask monkeys [ ask max-n-of ( round (p_disputed_trees * (length tree_pot_list) ) ) candidate_ld_targets [dist-to-homerange-center] [ set color pink ] ]

;      if length candidate_ld_targets = 0 [ set candidate_ld_targets candidate_ld_targets with-max [dist-to-homerange-center] ]
;      set candidate_ld_targets candidate_ld_targets with-max [dist-to-homerange-center]

      ;    print candidate_ld_targets
      ask candidate_ld_targets [
        set color yellow
        if study_area = "Taquara" [ set size 5 ]
      ]


      ; Option 1: select any of the candidate_ld_targets

      ; check if there was a ld_target before
      ifelse tree_target_mem2 != -1 [
        set ld_tree_target tree_target_mem2
        set tree_target_mem2 -1
;        print "last ld target reutilized 2"
        print ld_tree_target
      ] [
        ; this procedure avoids an error if the list of candidate_ld_targets is empty (might happen when p_disputed_trees is too low)
        ifelse candidate_ld_targets = nobody [ set candidate_ld_targets let_pot_list with-max [dist-to-homerange-center] ] [
          set ld_tree_target one-of candidate_ld_targets
          ;        print "random ld target defined 1"
        ]
      ]
;      set ld_tree_target one-of candidate_ld_targets with-min [distance [homerange-center] of myself] ; WORKS but not as intended


      ; Option 2: select the farthest tree among the candidate_ld_targets

;      ; check if there was a ld_target before
;      ifelse tree_target_mem2 != -1 [
;        set ld_tree_target tree_target_mem2
;      ] [
;        ask candidate_ld_targets [ set dist-to-ld-tamarins distance myself ]
;        ;      set candidate_ld_targets n-of 5 candidate_ld_targets
;        set ld_tree_target max-one-of candidate_ld_targets [ dist-to-ld-tamarins ]
;
;
;        ;    print ld_tree_target
;        ;    print distance ld_tree_target
;      ]

    ]

    ask ld_tree_target [ set size 15 ] ; debugging

    set tree_target ld_tree_target ; VERY IMPORTANT FOR NOT HAVING TO CHANGE ALL THE FEEDING PROCEDURE
    if tree_target = nobody [
      print "monkey ran out of tree options"
      enhance_memory_list
      search-feeding-tree
    ]

;    ask tree_target [ set color blue set size 5 ]    ; make long distance target blue

    set tree_target_species [ species ] of ld_tree_target
;    print ld_tree_target    ; debugging
  ]


  if feedingbout-on? [


    ;; TREE ENERGY VARIABLE WAS DERIVED BY ECKHARD AND MAYARA; SPECIES-TIME EMPIRICAL BASED ON FELIPE BUFALO AND ANNE SOPHIE ALMEIDA E SILVA THESIS AND DISSERTATION (use 'check tree species' button to correct string names)
    ;; ***OBS: species_time was multiplied by two in the feeding procedure on the previous version ('ifelse random (2 * species_time ) > frugivory-time'), so the following parameters were HALVED
    ;; Now (Dec 2022) these are not multiplied by 2

    set species_time 0
    while [species_time <= 0] [ ; the random-normal can return negative values

      if patch-type = "generated" [
        set species_time round ( random-normal 2.5 3.0 )
      ]


      if patch-type = "empirical" [

        if study_area = "Guareí" [
          set species_time round ( random-normal 1.5 0.71 ) ; mean sd values for Guareí (for NA species or species not specified below)

          ;      if tree_target_species = "Annona emarginata" [
          ;        set species_time 1
          ;        set energy_species 5
          ;      ]
          if tree_target_species = "Celtis iguanaea" [
            set species_time round ( random-normal 2.4 4.57 )
            ;      set energy_species 2
          ]
          if tree_target_species = "Cissus sulcicaulis" [
            set species_time 1                         ; no sd (few observations)
                                                       ;      set energy_species 4
          ]
          if tree_target_species = "Cordia ecalyculata" [ ; check if this species occurs in the input of trees
            set species_time 4
            ;      set energy_species 4
          ]
          if tree_target_species = "Dyospiros inconstans" [ ; only one tree in May (Guareí)
            set species_time 2                         ; no sd (few observations)
                                                       ;      set energy_species 3
          ]
          ;      if tree_target_species = "ficus" [ ; check if this species occurs in the input of trees
          ;        set species_time 5  ; very variable time
          ;        ;      set energy_species 2
        ]
        if tree_target_species = "Pereskia aculeata" [
          set species_time round ( random-normal 2.71 1.81 )
          ;      set energy_species 5
        ]
        if tree_target_species = "Rhipsalis cereuscula" [
          set species_time 3                         ; no sd (few observations)
                                                     ;      set energy_species 1
        ]
        if tree_target_species = "Syagrus romanzoffiana" [
          set species_time round ( random-normal 2.87 1.60 )
          ;      set energy_species 3
        ]
        ;      if tree_target_species = "rhamnidium" [ ; check if this species occurs in the input of trees
        ;        set species_time 1
        ;        ;      set energy_species 4
        ;      ]
        ;      if tree_target_species = "unknown" [  ; I don't know to which trees in Felipe dataset this one referes to so I didn't change the values
        ;        set species_time 3
        ;        ;      set energy_species 1
        ;      ]
        ;      if tree_target_species = "claussenii" [  ; This one either
        ;        set species_time 3
        ;        ;      set energy_species 1
        ;      ]
        if tree_target_species = "Eugenia sp." [
          set species_time round ( random-normal 4 2.83 )
          ;      set energy_species 3
        ]
        ;      if tree_target_species = "sp_five" [     ; This one either
        ;        set species_time 3
        ;        ;      set energy_species 2
        ;      ]
        ;      if tree_target_species = "NA" [     ; This one either
        ;        set species_time 2
        ;        ;      set energy_species 2
        ;      ]

        ;    ]

        if study_area = "SantaMaria" [
          set species_time round ( random-normal 2.83 2.55 ) ; mean sd values for SantaMaria (for NA species or species not specified below)
        ]

        if tree_target_species = "Myrcia splendens" [
          set species_time round ( random-normal 4.5 2.08 )
          ;        set energy_species 5
        ]

        if tree_target_species = "Phoradendron quadrangulare" [
          set species_time round ( random-normal 3.0 1.83 )
          ;        set energy_species 5
        ]

        if tree_target_species = "Celtis fluminensis" [
          set species_time round ( random-normal 2.66 2.21 )
          ;        set energy_species 5
        ]

        if tree_target_species = "Eugenia aff. ramboi" [
          set species_time round ( random-normal 1.65 1.27 )
          ;        set energy_species 5
        ]


        if study_area = "Taquara" [
          set species_time random-normal 2.5 0.61 ; average of mean and sd values for SantaMaria (for NA species or species not specified below)

          if tree_target_species = "Allophylus edulis" [
            set species_time round ( random-normal 1.5 0.58 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Campomanesia xanthocarpa" [
            set species_time round ( random-normal 3.44 1.01 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Campomanesia xanthocarpa" [
            set species_time round ( random-normal 3.44 1.01 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Eugenia brasiliensis" [
            set species_time round ( random-normal 2.91 0.70 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Eugenia punicifolia" [
            set species_time 4                          ; no sd (few observations)
                                                        ;        set energy_species 5
          ]

          if tree_target_species = "Ficus enormis" [
            set species_time round ( random-normal 3.33 0.82 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Myrceugenia ovata" [
            set species_time round ( random-normal 2.0 1.41 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Psidium longipetiolatum" [
            set species_time round ( random-normal 3.22 0.44 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Psidium myrtoides" [
            set species_time round ( random-normal 2.5 0.71 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Rhipsalis cereuscula" [
            set species_time round ( random-normal 2.50 0.58 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Rhipsalis teres" [
            set species_time 1                          ; no sd (few observations)
                                                        ;        set energy_species 5
          ]

          if tree_target_species = "Sorocea bonplandii" [
            set species_time round ( random-normal 3.44 1.01 )
            ;        set energy_species 5
          ]

        ]

        if study_area = "Suzano" [
          set species_time random-normal 2.85 0.75 ; average of mean and sd values for SantaMaria (for NA species or species not specified below)

          if tree_target_species = "Abuta selloana" [
            set species_time 3                          ; no sd (few observations)
                                                        ;        set energy_species 5
          ]

          if tree_target_species = "Casearia sylvestris" [
            set species_time 3                          ; no sd (few observations)
                                                        ;        set energy_species 5
          ]

          if tree_target_species = "Cordia sellowiana" [
            set species_time 3                          ; no sd (few observations)
                                                        ;        set energy_species 5
          ]

          if tree_target_species = "Myrciaria cuspidata" [
            set species_time round ( random-normal 2.50 0.71 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Myrsine umbellata" [
            set species_time 3                          ; no sd (few observations)
                                                        ;          set energy_species 5
          ]

          if tree_target_species = "Pera glabrata" [
            set species_time round ( random-normal 4.0 1.41 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Philodendron spp." [
            set species_time round ( random-normal 2.5 0.71 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Plinia trunciflora" [
            set species_time round ( random-normal 2.86 0.69 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Randia armata" [
            set species_time 3                          ; no sd (few observations)
                                                        ;        set energy_species 5
          ]

          if tree_target_species = "Rhipsalis teres" [
            set species_time round ( random-normal 3.33 1.53 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Syagrus romanzoffiana" [
            set species_time round ( random-normal 2.50 0.80 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Trichilia catigua" [
            set species_time round ( random-normal 2.33 0.82 )
            ;        set energy_species 5
          ]

          if tree_target_species = "Xylopia brasiliensis" [
            set species_time round ( random-normal 3.0 0.82 )
            ;        set energy_species 5
          ]


        ]
      ]
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
    if (action = "travel" ) [
      forward step_len_travel
      set energy energy + ( energy-loss-traveling * step_len_travel )
    ]
    if (action = "forage" ) [
      forward step_len_forage
      set energy energy + ( energy-loss-traveling * step_len_forage )
    ]
    set straight-line-to-target? TRUE
;    print "travel 3"
  ]


end

;---------------------------------------------------------------------------------------------
; Defecation commands
;---------------------------------------------------------------------------------------------
to defecation
;  output-print "voiding seeds ****************** "

  ifelse ( timestep < simulation-time * 0.9 ) [ ; if the time is below 90% of the simulation-time, seeds should be defecated (check parameterization);   Mayara's model: 84 timesteps is for 7 hours after waking up (after 3pm)

    ; testing if the monkey defecates the seeds AND put the seeds to the seeds' agent list

    ifelse ( gtt-param? = TRUE ) [

      foreach seed_gtt_list [ ax ->                        ; based on the gtt list

        let loc_index_gtt position ax seed_gtt_list            ; get the position of x in the seed_gtt_list
        let loc_index_mem item loc_index_gtt seed_mem_list    ; get the position of x in the seed_mem_list (it has to be the same position in both lists)
        let loc_who item loc_index_gtt seed_ate_list      ; get the who number of x

        ;debugging
        ;        type "gtt done = " print x
        ;        type "loc_index_gtt = " print loc_index_gtt
        ;        type "loc_who_gtt = " print loc_who_gtt
        ;        print " - "


        if ( loc_index_mem = ax) [                             ; if the atributed gtt to each seed (x) is equal to the time passed since consumption (seed_mem_list) in its position

          ;debugging:
;          print "LISTS MATCH, DEFECATION SHUOLD OCCUR NOW"

          set seed_ate_list remove-item loc_index_gtt seed_ate_list
          set seed_add_list remove-item loc_index_gtt seed_add_list
          set seed_mem_list remove-item loc_index_gtt seed_mem_list
          set seed_gtt_list remove-item loc_index_gtt seed_gtt_list

          hatch-seeds n_seeds_hatched [ ; change to hatch more seeds! <<<
            setxy xcor ycor
            if patch-type = "generated" [
              set x_scaled (xcor * patch-scale)         ; generated patches do not have coordinate projection
              set y_scaled (ycor * patch-scale)         ; generated patches do not have coordinate projection
            ]
            if patch-type = "empirical" [
              set x_UTM (item 0 gis:envelope-of self)
              set y_UTM (item 2 gis:envelope-of self)
            ]

            set mother-tree [id-tree] of feeding-trees with [ who = loc_who ]
            set mother-tree-agent feeding-trees with [ who = loc_who ]
;            set mother-tree-who [who] of feeding-trees with [ who = loc_who ]
            set species [species] of feeding-trees with [ who = loc_who ]
            set id-seed who
            set disp-day "same day"
            set SDD distance ( feeding-tree loc_who ) * patch-scale ;with [id-tree] = mother-tree]
;            type "your mother tree is: " print feeding-tree loc_who
            set label ""
            set shape "plant"
            set size 1.45
            set color 4
          ]

        ]

        ;debugging
;        print " ==================================== "
;        type "gtt done = " print x
;        type "loc_index_gtt = " print loc_index_gtt
;        type "loc_who_gtt = " print loc_who_gtt
;        print " - "

      ]





      ; this has to happen independently of the timestep
      set seed_mem_list (map + seed_add_list seed_mem_list)

    ][

      if member? gut_transit_time seed_mem_list [                            ; if the timestep since consumed (seed_mem_list) is equal to gut_transit_time ...
        let loc_index position gut_transit_time seed_mem_list                ;
        let loc_who item loc_index seed_ate_list                             ; take the who number of the consumed seed based on the seed_mem_lit and save it in an index


        hatch-seeds n_seeds_hatched [ ; change to hatch more seeds! <<<
          setxy xcor ycor

          if patch-type = "generated" [
            set x_scaled (xcor * patch-scale)         ; generated patches do not have coordinate projection
            set y_scaled (ycor * patch-scale)         ; generated patches do not have coordinate projection
          ]
          if patch-type = "empirical" [
            set x_UTM (item 0 gis:envelope-of self)
            set y_UTM (item 2 gis:envelope-of self)
          ]

          set mother-tree [id-tree] of feeding-trees with [ who = loc_who ]
;          set mother-tree-who [who] of feeding-trees with [ who = loc_who ]
          set mother-tree-agent feeding-trees with [ who = loc_who ]
          set species [species] of feeding-trees with [ who = loc_who ]
          set id-seed who
          set disp-day "same day"
          set SDD distance ( feeding-tree loc_who ) * patch-scale ;with [id-tree] = mother-tree]
;          type "your mother tree is: " print feeding-tree loc_who
          set label ""
          set shape "plant"
          set size 1.45
          set color 4
        ]
        set seed_ate_list remove-item 0 seed_ate_list                        ; remove the first seed from the seed_ate_list
        set seed_add_list remove-item 0 seed_add_list                        ; do the same for the helper list (seed_add_list)
        set seed_mem_list remove gut_transit_time seed_mem_list              ; remove the gut_transit_time item from the seed_mem_list
      ]
      ; this has to happen independently of the timestep
      set seed_mem_list (map + seed_add_list seed_mem_list)

    ]

  ][ ; otherwise, seeds will be kept to morning-defecation
    ; this has to happen independently of the timestep
      set seed_mem_list (map + seed_add_list seed_mem_list)
  ]


end

;----------------------------------------------------

to morning-defecation

  ;debugging:
;  type "MORNING-DEFECATION step: " print timestep

  foreach seed_ate_list [
    ax ->  hatch-seeds n_seeds_hatched [ ; change to hatch more seeds! <<<
      setxy xcor ycor
      if patch-type = "generated" [
        set x_scaled (xcor * patch-scale)         ; generated patches do not have coordinate projection
        set y_scaled (ycor * patch-scale)         ; generated patches do not have coordinate projection
      ]
      if patch-type = "empirical" [
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
      ]
      set mother-tree [id-tree] of feeding-trees with [ who = ax ]
      set mother-tree-agent feeding-trees with [ who = ax ]
;      set mother-tree-who [who] of feeding-trees with [ who = loc_who ] ; loc_who has to be redefined
      set species [species] of feeding-trees with [ who = ax ]
      set id-seed who
      set disp-day "next day"
;      let loc_who [who] of feeding-trees with [ who = x ] ; this does not work becuse there are duplicated agents (more than one feeding-tree with id-tree = "AMf043"), thus this returns a list
;      print loc_who
;      set SDD distance feeding-trees with [ loc_who = x ]
      set SDD distance ( feeding-tree ax ) * patch-scale
      ; testing if the SDD is correct (print on command center):
      ; ask feeding-trees with [ id-tree = "AMf167" ] [ set color pink ]
      ; ask seed 105 [ print distance feeding-tree 27  ]
      set label ""
      set shape "plant"
      set size 1
      set color 1
    ]
  ]

  ; make lists empty as they were all defecated:
  set seed_ate_list []
  set seed_mem_list []
  set seed_add_list []
  if gtt-param? = TRUE [ set seed_gtt_list [] ]
end

;---------------------------------------------------------------------------------------------
; Resting commands
;---------------------------------------------------------------------------------------------
to resting
;  print "resting"   ; debugging
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

;  print "calling sleeping procedure"


  ;save fruiting tree target for next day
  if tree_target != -1 [ set tree_target_mem2 ld_tree_target ]

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


    if distance tree_target < 2 * step_len_travel  [ ; travel speed basically doubles when tamrarins are going to the sleeping site

      set dist-traveled dist-traveled + distance tree_target
      move-to tree_target
      if patch-type = "empirical" [
        set y_UTM [ y_UTM ] of tree_target
        set x_UTM [ x_UTM ] of tree_target
      ]
      ; don't make actual xcor and ycor of tamrins the same as tres to avoid the point (x,y) error; instead add a small variation (0.01 = 0.1 m) to xcor and ycor
      ; use set timestep 108 to test this
      set xcor [xcor] of tree_target + 0.01
      set ycor [ycor] of tree_target + 0.01

      ask tree_target [ set visitations visitations + 1 ]

      set tree_current tree_target
      set tree_target -1

      print "*** I am sleeping ****"
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


;      calc-movement-vars
;      let a "_" ; to make it possible to analyze it with nlrx
      set DPL DPL * patch-scale ; (same as multiplying by 10 m)
      set DPL_d lput ( precision DPL 2 ) DPL_d
;      set DPL_d lput a DPL_d

      set MR ( DPL / (timestep * 5 / 60 ) )  ; MR as calculated in Fuzessy et al. 2017: DPL/activity time (hours)
;      type "MR = " print MR
      set MR_d lput ( precision MR 2 ) MR_d
;      set MR_d lput a MR_d

      ; debugging:
;      type "MR = " print MR
;      type "DPL = " print DPL
;      type "time (min) = " print timestep * 5

      ; reset values everyday, except the last one (otherwise PT can't be calculated)
      if day != no_days [
        set DPL 0
        set MR 0
      ]


      output-type "*simulation-time* "
      output-type ticks
      output-print " ----"

    ]
  ]

  if action != "sleeping" [

;    print "BEING CALLED FROM SLEEPING"

    avoid-patch-set

    ifelse straight-line-to-target? = TRUE [ ; otherwise it might enter the matrix
      face tree_target

    ;; RANDOM movement while traveling:
    if step-model-param? = TRUE  AND distance tree_target > ( 2 * step_len_travel  ) [  ; travel speed basically doubles when tamrarins are going to the sleeping site
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

;      print "******************* sleeping debugging "

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

    store_energy

    output-print "AHOY"

  ]

  ifelse study_area = "Taquara" [
    ask feeding-trees [ set size 5 ]
    ask sleeping-trees [ set size 5 ]
  ][
    ask feeding-trees [ set size 3 ]
    ask sleeping-trees [ set size 3 ]
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

;  print "last-action-again"   ; debugging

;  if action = "forage" [
;    forage
;    ; the following lines were excluded from the foraging procedure to not conflict with the to-feeding-tree procedure (agents were doing two steps)
;
;    if travel_mode = "long_distance" AND distance ld_tree_target > travel_speed * 0.8 [
;      travel
;    ]
;    if travel_mode = "short_distance" AND distance tree_target > travel_speed * 0.8 [
;      travel
;;      set color grey ; in case the tamarin has foraged, it became magenta
;    ]
;  ]

;  if action = "resting" AND random-float 1 < p-resting-while-resting [ resting ] ; if the last action was resting, have x % chance of resting again
  if action = "resting" [  ; if the last action was resting, rest again
    set action-time action-time + 1
;    print "resting 2"
;    print "111111111111111111111------------"
;    type " ---- MODE: " print travel_mode
;    type "tree_target: " type tree_target type " "
;    type "ld_tree_target: " type ld_tree_target type " "
;    type "tree_current: " type tree_current type " "
;    type "behavior: " type behavior type " "
;    type "action: " print action
    resting
  ]

  ; if action was travel or foraging, go back to the frugivory loop
  if action = "travel" OR action = "forage" [
    frugivory
;    print "222222222222222222222------------"
;    type " ---- MODE: " print travel_mode
;    type "tree_target: " type tree_target type " "
;    type "ld_tree_target: " type ld_tree_target type " "
;    type "tree_current: " type tree_current type " "
;    type "behavior: " type behavior type " "
;    type "action: " print action
  ]

  ; when monkeys arrive on a ld_tree_target, they might get stuck in the last-action being called through frugivory. That happens because once they arrive at a long_distance_target, the travel mode should be set to
  if action = "feeding" [
;    print "333333333333333333333------------"
;    type " ---- MODE: " print travel_mode
;    type "tree_target: " type tree_target type " "
;    type "ld_tree_target: " type ld_tree_target type " "
;    type "tree_current: " type tree_current type " "
;    type "behavior: " type behavior type " "
;    type "action: " print action
;    type "action-time: " print action-time

    set travel_mode "short_distance"      ; leave it here!
    frugivory
  ]

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


to get_stored_energy

  ; Avoid that tamarins die by making them use their stored energy from other days
  ask monkeys [
;    if energy > 0 AND energy < ( 0.7 * start-energy ) [
    if energy > 0 AND energy < ( 0.7 * energy_level_1 ) [

;      ifelse ( ( energy_stored - (0.1 * energy_stored) ) > 0 ) [     ; if there's more than 10% start-energy
;        set energy energy + (0.1 * energy_stored)                    ; take 10% of stored energy
;        set energy_stored ( energy_stored - (0.1 * energy_stored) )  ; diminish 10% of stored energy
;        print "energy debug 1 ********"
;      ][
;       set energy energy + energy_stored
;        set energy_stored 0
;       print "stored energy over" print "" print "" print "" print "" print ""
;      ]

;      ifelse ( ( energy_stored - (0.1 * energy_stored) ) > (0.1 * start-energy) ) [     ; if there's more stored energy than 10% of start-energy
      ifelse ( ( energy_stored - (0.1 * energy_stored) ) > (0.1 * energy_level_1) ) [     ; if there's more stored energy than 10% of energy_level_1
;        set energy energy + (0.1 * start-energy)                    ; take 10% of start-energy from stored energy
        set energy energy + (0.1 * energy_level_1)                    ; take 10% of energy_level_1 from stored energy
;        set energy_stored ( energy_stored - (0.1 * start-energy) )  ; diminish 10% of start-energy from stored energy
        set energy_stored ( energy_stored - (0.1 * energy_level_1) )  ; diminish 10% of energy_level_1 from stored energy
;        print "energy debug 1 ********"
      ][
        set energy energy + energy_stored
        set energy_stored 0
        print "stored energy over" print "" print "" print "" print "" print ""
      ]

    ]
  ]

end

to store_energy
  ask monkeys [
    if energy > energy_level_1 [
      let spared_energy energy - energy_level_1
      set energy_stored energy_stored + spared_energy
    ]
  ]

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


to start-r-extension

  if R_EXTENSION = "SimpleR" [

    ; use sr:run to run code in R
    ; use sr:runresult to get values back to netlogo

    ;; setup SimpleR (mandatory):
    sr:setup
    set sr-extension-set? TRUE
    print "= simpleR extension setted up ="

    ; test:
    ;    sr:run "iris"
    ;    sr:run "iris %>% summary()"
    ;    print "==== OK ==== "

    ;    stop

    sr:run "suppressMessages(library(adehabitatHR, quietly = T))" ;; this package the new package ('adehabitat' is removed from CRAN sind 2018)
                                                ;  sr:run "library(udunits2)"
                                                ;  sr:run "library(units)"
    sr:run "suppressMessages(library('tidyverse', quietly = T))"
    ;    sr:run "library('dplyr', quietly = T)"
    ;    sr:run "library('tidyr', quietly = T)"
    sr:run "suppressMessages(library('amt', quietly = T))"
    sr:run "suppressMessages(library('sf'))"
;    sr:run "suppressMessages(library('circular'))"
;    sr:run "suppressMessages(library('st', quietly = T))"

    print "==== packages loaded ==== "

    ;        stop

    ;; create an empty data.frame"
    sr:run "monkeys_df <- data.frame()"
    ;sr:run "print(monkeys_df)"
    ;    print "==== SR debugging 0 ==== "

    if count monkeys = 1 [
      ;; merge the Name, X- and Y-lists of all animals to one big data.frame
      ask monkeys [
        ;        print "==== SR debugging 1 ==== "

        let X_coords_sr [X_coords] of self    ;print X_coords_sr  print length X_coords_sr
        let Y_coords_sr [Y_coords] of self    ;print Y_coords_sr  print length Y_coords_sr
        let day_list_sr [day_list] of self    ;print day_list_sr  print length day_list_sr
        let Name_sr [Name] of self            ;print Name_sr      print length Name_sr

        ;        print "==== SR debugging 2 ==== "

        ;      stop

;        (sr:set-data-frame "df1" "X_coords" X_coords_sr "Y_coords" Y_coords_sr "day_list" day_list_sr)
;        ;      if ticks < 110 [
;        ;        ;sr:run "print(df1)"            ; don't use this one       ;https://github.com/NetLogo/SimpleR-Extension/issues/2
;        ;        show sr:runresult "df1"         ; use this form instead    ;https://github.com/NetLogo/SimpleR-Extension/issues/2  ; still freezes when > 110 lines thoughthough
;        ;      ]
;
;        sr:set "Name_sr" Name_sr
;        sr:run "df1 <- data.frame(df1, Name = Name_sr)"
;
;        if ticks < 110 [ show sr:runresult "df1"    ]

        ;      stop

;        print "==== SR debugging 3 ==== "

        (sr:set-agent-data-frame "tamarins" monkeys "who" "x_coords" "y_coords" "day_list")
;        sr:run "print(typeof(tamarins))"

        ;      stop

;        print "==== SR debugging 4 ==== "

        ;      sr:set "tamarins" "data.frame(tamarins, Name = 'Name')"
        ;        sr:run "print(head(tamarins))"
        sr:run "names(tamarins) <- c('Name', 'X', 'Y', 'day')"


        ;    stop


        ;;; split the data.frame into coordinates and factor variable
        sr:run "xy <- tamarins[,c('X','Y', 'day')]"
        sr:run "id <- tamarins$Name"
        ;      (sr:run
        ;        "library('tidyverse')"
        ;        "xy <- turtles %>% dplyr::select(X, Y, day)"
        ;        "id <- turtles$Name"
        ;        )

        ;        print "==== SR debugging 5 ==== "

        ;    ;;; calculate homerange (mcp method, needs a SpatialPointDataFrame)
        ;      sr:run "homerange <- mcp(xy, id)"
        ;      show sr:runresult "homerange"
        ;
        ; calculate homerange (amt package)
        sr:run "db <- cbind(xy, id)"
        ;        if ticks < 110 [  print sr:runresult "colnames(db_)" ]
        ;        if ticks < 110 [  print sr:runresult "db_" ]

        ; Using non-nested data as we only have one group. When multiple tamarin groups are simulated, we will need to call nest():
        (
          sr:run "db_ <- db %>% make_track(.x=X, .y=Y, id = id, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')" ;%>% nest(data = -c(id))"
        )
        ;    show r:get "colnames(db_)"
        ;    show r:get "db_"

        ;        print "==== SR debugging 6 ==== "


      ]
    ]
  ]

end

to calc-homerange

; ############################################# start of SimpleR extension code ############################################# ;
  if R_EXTENSION = "SimpleR" [

        print " ------------- Home range size ------------------ "

;        ;MCP ===================================================
        sr:run "db_MCP <- db_ %>% hr_mcp(., levels = c(0.50, 1.0))"
        sr:run "db_MCP_area <- db_MCP %>% hr_area(.)"
;        print sr:runresult "db_MCP_area"

        sr:run "db_MCP_area <- db_MCP_area %>% dplyr::select(-what)"
        sr:run "db_MCP100 <- db_MCP_area %>% dplyr::filter(level == 1.0) %>% dplyr::select(area) %>% unlist() %>% as.vector()" ; %>% round(2) "
;        type "MCP100 = " print sr:runresult "db_MCP100 / 10000" stop


;        ; KDE ===================================================
        sr:run "db_KDE <- db_ %>% hr_kde(., levels = c(0.50, 0.95))"
;        print sr:runresult "db_KDE"
;        stop
        sr:run "db_KDE_area <- db_KDE %>% hr_area(.)"

        sr:run "db_KDE_area <- db_KDE_area %>% dplyr::select(-what)"
        sr:run "db_KDE95 <- db_KDE_area %>% dplyr::filter(level == 0.95) %>% dplyr::select(area) %>% unlist() %>% as.vector()" ; %>% round(2) "
        sr:run "db_KDE50 <- db_KDE_area %>% dplyr::filter(level == 0.50) %>% dplyr::select(area) %>% unlist() %>% as.vector()" ; %>% round(2) "

;        print "==== SR debugging 7 ===="

        ; Import shapefile of respective area and crop homerange UD with st_intersection()
        ifelse USER = "Eduardo" [
          if study_area = "Guareí" [
            sr:run "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp' %>% sf::read_sf()"
;            print sr:runresult "shp"
;            print "===== read_sf debug ====="
;            stop
          ]
          if study_area = "Suzano" [
            sr:run "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp' %>% sf::read_sf()"
          ]
          if study_area = "SantaMaria" [
            sr:run "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp' %>% sf::read_sf()"
          ]
          if study_area = "Taquara" [
            sr:run "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp' %>%  sf::read_sf()"
          ]
        ][
          if study_area = "Guareí" [
            sr:set "wpath" word (local-path) "Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp"
            sr:run "shp <- wpath %>% sf::read_sf()"
          ]
          if study_area = "Suzano" [
            sr:set "wpath" word (local-path) "Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp"
            sr:run "shp <- wpath %>% sf::read_sf()"
          ]
          if study_area = "SantaMaria" [
            sr:set "wpath" word (local-path) "Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp"
            sr:run "shp <- wpath %>% sf::read_sf()"
          ]
          if study_area = "Taquara" [
            sr:set "wpath" word (local-path) "Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp"
            sr:run "shp <- wpath %>% sf::read_sf()"
          ]
        ]

;        print "==== SR debugging 8 ==== "

        ;  sr:run "shp <- sf::read_sf(filepath)" ; make it an sf object
        sr:run "forest_area <- shp %>% sf::st_area()" ; print total forest area
        type "total forest area (ha) = " print sr:runresult "forest_area / 10000"

;        stop


;        ;; NOT WORKING ;; =========================================================================
;        ; (with st_interesect()) -> NetLogo freezes
;         ; Transform KDE to sf
;        sr:run "KDE_sf <- amt::hr_isopleths(db_KDE) %>% sf::st_as_sf(., crs = 32722)"
;        print sr:runresult "KDE_sf"
;        print sr:runresult "str(KDE_sf)"
;        print "==== SR debugging 9 ==== "
;        ; crop home range by the forest area
;        sr:run "cropped <- sf::st_intersection(KDE_sf[1], shp)"
;        type "cropped HR values = " print sr:runresult "cropped"
;        print "==== before sf_overlap ==== "
;        sr:run "sf_overlap <- cropped %>% sf::st_area()" ; first value = KDE95; second value = KD50
;        ;; NOT WORKING ;; =========================================================================


;        ;; NOT WORKING ;; =========================================================================
;        ; (with amt::hr_isopleths() and st_interestc()) -> NetLogo freezes
;        sr:run "KDE_iso <- amt::hr_isopleths(db_KDE)"
;        sr:run "KDE_iso$area[1] %>% as.numeric() / 10000"
;        show sr:runresult "KDE_iso"
;        sr:run "KDE_sf <- amt::hr_isopleths(db_KDE) %>% sf::st_as_sf(., crs = 32722)"
;        sr:run "cropped <- sf::st_intersection(KDE_sf, shp)"
;        sr:run "sf_overlap <- cropped %>% sf::st_area()"
;        ;; NOT WORKING ;; =========================================================================


;    print "==== SR debugging 9 ==== "
;    stop


        ;; WORKING ;; =========================================================================
        ; (with amt::hr_overlap_feature()) -> NetLogo does not freeze
        sr:run "amt_overlap <- amt::hr_overlap_feature(db_KDE, shp)"
        sr:run "amt_overlap[1,3] %>% as.numeric()"  ; 0.5 overlap
        sr:run "amt_overlap[2,3] %>% as.numeric()"  ; 0.95 overlap
;        type "overlap (forest-HR) values = " print sr:runresult "amt_overlap"
;        print "==== before hr_overlap ==== "
;        sr:run "sf_overlap <- cropped %>% sf::st_area()" ; first value = KDE95; second value = KD50
        ;; WORKING ;; =========================================================================

;        print "==== SR debugging 10 ==== "

;        stop

        ask monkeys [
          set MCP_100 sr:runresult "db_MCP100 / 10000"

          ; correct hr values by the overlap percentage:
          set KDE_95_cropped sr:runresult "amt_overlap[1,3] %>% as.numeric() * db_KDE95"
          set KDE_50_cropped sr:runresult "amt_overlap[2,3] %>% as.numeric() * db_KDE50"

          ; validate interescted values (in m²):
          ;    type "monkey " type who type " "
          type "KDE95 = " print sr:runresult "db_KDE95"
          type "cropped KDE95 = " type KDE_95_cropped / 10000 print " hectares"
          type "KDE50 = " print sr:runresult "db_KDE50"
          type "cropped KDE50 = " type KDE_50_cropped / 10000 print " hectares"

          ; set values in hectares:
          set KDE_95_cropped sr:runresult "db_KDE95 / 10000"
          set KDE_50_cropped sr:runresult "db_KDE50 / 10000"

          set KDE_95 sr:runresult "db_KDE95" ; in m²
          set KDE_50 sr:runresult "db_KDE50" ; in m²

        ]
        ;  print sr:runresult "db_KDE95"
        ;  print sr:runresult "db_KDE50"


;        print "==== SR debugging 11 ==== "

;        stop


      ]
;    ]

;  ]
; ############################################# end of SimpleR extension code ############################################# ;


; ############################################# start of R extension code ############################################# ;

;  if R_EXTENSION = "R" [ ; dropped this out after upgrading to NetLogo 6.3 ; https://github.com/NetLogo/R-Extension/issues/27
;
;    ; load up libraries:
;    ;  r:eval "suppressMessages(library(adehabitat))"
;    r:eval "suppressMessages(library(dplyr))"
;    r:eval "suppressMessages(library(tidyr))"
;    r:eval "suppressMessages(library(amt))"
;    r:eval "suppressMessages(library(circular))"
;    r:eval "suppressMessages(library(sf))"
;
;    ;; create an empty data.frame"
;    r:eval "turtles <- data.frame()"
;
;    print r:get "turtles"
;
;    ;; merge the Name, X- and Y-lists of all animals to one big data.frame
;    ask monkeys
;    [
;      (r:putdataframe "turtle" "X" X_coords "Y" Y_coords "day" day_list)
;      r:eval (word "turtle <- data.frame(turtle, Name = '" Name "')")
;      r:eval "turtles <- rbind(turtles, turtle)"
;      ;    type "*****turtles data frame: " print r:get "turtles"
;    ]
;
;;    print r:get "turtles"
;;    stop
;
;    ;; split the data.frame into coordinates and factor variable
;    ;  r:eval "xy <- turtles[,c('X','Y')]"
;    r:eval "xy <- turtles %>% dplyr::select(X, Y, day)"
;    r:eval "id <- turtles$Name"
;
;    ;; calculate homerange (mcp method)
;    ;  r:eval "homerange <- mcp(xy, id)"
;
;    ;; calculate homerange (amt package)
;    r:eval "db <- cbind(xy, id)"
;    ;    show r:get "colnames(db)"
;    ;    show r:get "db"
;
;    ; Using non-nested data as we only have one group. When multiple tamarin groups are simulated, we will need to call nest():
;    r:eval "db_ <- db %>% make_track(.x=X, .y=Y, id = id, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')" ;%>% nest(data = -c(id))"
;                                                                                                                                          ;    show r:get "colnames(db_)"
;                                                                                                                                          ;    show r:get "db_"
;
;    print " ------------- Home range size ------------------ "
;
;    ; MCP ===================================================
;    r:eval "db_MCP <- db_ %>% hr_mcp(., levels = c(0.50, 1.0))"
;    r:eval "db_MCP_area <- db_MCP %>% hr_area(.)"
;;    print r:get "db_MCP_area"
;
;    r:eval "db_MCP_area <- db_MCP_area %>% dplyr::select(-what)"
;    r:eval "db_MCP100 <- db_MCP_area %>% dplyr::filter(level == 1.0) %>% dplyr::select(area) %>% unlist() %>% as.vector()" ; %>% round(2) "
;;    type "MCP100 = " print r:get "db_MCP100 / 10000" stop
;
;
;    ; KDE ===================================================
;    r:eval "db_KDE <- db_ %>% hr_kde(., levels = c(0.50, 0.95))"
;    r:eval "db_KDE_area <- db_KDE %>% hr_area(.)"
;
;    r:eval "db_KDE_area <- db_KDE_area %>% dplyr::select(-what)"
;    r:eval "db_KDE95 <- db_KDE_area %>% dplyr::filter(level == 0.95) %>% dplyr::select(area) %>% unlist() %>% as.vector()" ; %>% round(2) "
;    r:eval "db_KDE50 <- db_KDE_area %>% dplyr::filter(level == 0.50) %>% dplyr::select(area) %>% unlist() %>% as.vector()" ; %>% round(2) "
;
;    ; Import shapefile of respective area and crop homerange UD with st_intersection()
;    ifelse USER = "Eduardo" [
;      if study_area = "Guareí" [
;        r:eval "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp' %>% sf::read_sf()"
;      ]
;      if study_area = "Suzano" [
;        r:eval "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp' %>% sf::read_sf()"
;      ]
;      if study_area = "SantaMaria" [
;        r:eval "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp' %>% sf::read_sf()"
;      ]
;      if study_area = "Taquara" [
;        r:eval "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp' %>%  sf::read_sf()"
;      ]
;    ][
;      if study_area = "Guareí" [
;        r:put "wpath" word (local-path) "Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp"
;        r:eval "shp <- wpath %>% sf::read_sf()"
;      ]
;      if study_area = "Suzano" [
;        r:eval "shp <- 'D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp' %>% sf::read_sf()"
;      ]
;      if study_area = "SantaMaria" [
;        r:put "wpath" word (local-path) "Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp"
;        r:eval "shp <- wpath %>% sf::read_sf()"
;      ]
;      if study_area = "Taquara" [
;        r:put "wpath" word (local-path) "Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp"
;        r:eval "shp <- wpath %>% sf::read_sf()"
;      ]
;    ]
;
;    ;  r:eval "shp <- sf::read_sf(filepath)" ; make it an sf object
;    r:eval "forest_area <- shp %>% sf::st_area()" ; print total forest area
;    type "total forest area (ha) = " print r:get "forest_area / 10000"
;
;    ; Transform KDE to sf
;    r:eval "KDE_sf <- amt::hr_isopleths(db_KDE) %>% sf::st_as_sf(., crs = 32722)"
;    ;  print r:get "KDE_sf"
;
;    ; crop home range by the forest area
;    r:eval "cropped <- sf::st_intersection(KDE_sf, shp)"
;    ;  type "cropped HR values = " print r:get "cropped"
;    r:eval "sf_overlap <- cropped %>% sf::st_area()" ; first value = KDE95; second value = KD50
;
;    ask monkeys [
;      set MCP_100 r:get "db_MCP100 / 10000"
;      set KDE_95_cropped r:get "sf_overlap[1] %>% as.numeric() / 10000"
;      set KDE_50_cropped r:get "sf_overlap[2] %>% as.numeric() / 10000"
;
;      ;    type "monkey " type who type " "
;      type "cropped KDE95 = " print KDE_95_cropped
;      type "cropped KDE50 = " print KDE_50_cropped
;    ]
;    ;  show r:get "db_KDE95"
;    ;  show r:get "db_KDE50"
;
;    ;  show r:get "db_"
;
;
;    ;  ; Using nested data with multiple tamarin groups are simulated (we will need to call nest())
;    ;  r:eval "db_nest <- db %>% make_track(.x=X, .y=Y, id = id, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs') %>% nest(data = -c(id))"
;    ;  ; calculate HR metrics for every list (=id = run) using map()
;    ;  r:eval "db_nest <- db_nest %>% mutate( KDE95 = amt::map(data, ~ hr_kde(., level = 0.95)), KDE50 = amt::map(data, ~ hr_kde(., level = 0.50)) )"
;    ;  r:eval "db_nest <- db_nest %>%  dplyr::select(-data) %>% pivot_longer(KDE95:KDE50, names_to = 'KDE_value', values_to = 'hr')"
;    ;  r:eval "db_nest <- db_nest %>% mutate(hr_area = map(hr, hr_area)) %>%  unnest(cols = hr_area)"
;    ;;  r:eval "db_nest <- db_nest %>% filter(KDE_value == 'KDE95') %>% dplyr::select(-c(3, 4))"
;    ;  r:eval "db_nest <- db_nest %>% dplyr::select(-c('what', 'hr'))"
;    ;;  r:eval "db_nest <- db_nest %>% mutate(hr_area_ha = area / 10000)" ; values in ha
;
;    ;  show r:get "db_nest"
;
;    ; for outputting on nlrx:
;    ; home range
;    ;  r:eval "db_nest$hr_area_ha <- paste0(db_nest$hr_area_ha, '_')"
;    ; coordinates
;    ;  r:eval "db$X <- paste0(db$X, '_')"
;    ;  r:eval "db$Y <- paste0(db$Y, '_')"
;    ;  r:eval "db$id <- paste(db$id, '_')"
;
;    ;  print "db: "
;    ;  show r:get "db"
;
;    ;  print "db_nest: "
;    ;  show r:get "db_nest"
;
;    r:gc
;
;    ; get hr values to agent variable
;
;    ask monkeys [
;
;      ; if only one group was simulated:
;      set KDE_95 r:get "db_KDE95"
;      set KDE_50 r:get "db_KDE50"
;
;      type "*******" print precision ( KDE_95 / 10000 ) 4
;
;      ;    ; if you used nested output (simulated multiple tamarin groups):
;      ;    set KDE_95 r:get "db_nest %>%  dplyr::filter(KDE_value == 'KDE95') %>%  dplyr::select(area) %>%  unlist() %>% as.vector()" ; %>% round(2)"
;      ;    set KDE_50 r:get "db_nest %>%  dplyr::filter(KDE_value == 'KDE50') %>%  dplyr::select(area) %>%  unlist() %>% as.vector()" ; %>% round(2)"
;
;      ;    type "KDE_95: " print round KDE_95
;      ;    type "KDE_50: " print round KDE_50
;      ;    type "KDE_50: " print precision ( KDE_50 / 10000 ) 4
;
;      ; update the KDE if it matches the cropped KDE (ie it didn't include non-habitat areas)
;      ifelse KDE_95 = KDE_95_cropped [
;        ;      type "KDE_95: " print precision ( KDE_95 / 10000 ) 4
;      ][
;        print "home range was cropped"
;        set KDE_95 KDE_95_cropped
;        ;      type "KDE_95: " print precision ( KDE_95 / 10000 ) 4
;      ]
;      ifelse KDE_50 = KDE_50_cropped [
;        ;      type "KDE_50: " print precision ( KDE_50 / 10000 ) 4
;      ][
;        print "home range was cropped"
;        set KDE_50 KDE_50_cropped
;        ;      type "KDE_50: " print precision ( KDE_50 / 10000 ) 4
;      ]
;    ]
;
;
;    ;  ; Merge HR to db and save
;    ;  r:eval "db <- left_join(db, db_nest)"
;    ;  r:eval "db <- db %>% mutate(hr_area_ha = area / 10000)"
;    ;
;    ;  ; drop columns
;    ;  r:eval "db <- db %>%    select(-c(KDE_value, area))"
;    ;  print "db: "
;    ;  show r:get "db"


;  ; ############################################# end of R extension code ############################################# ;


  ; remake lists to be readable in R with nlrx (I COULDN'T MANAGE TO PASTE A "_" BETWEEN EACH VALUE)
;  ask monkeys [
;    let a "_"
;    foreach DPL_d [
;      x ->
;      set DPL_d lput a DPL_d
;
;    ]
;    print DPL_d
;
;
;  ]

  ; SET RESOURCE REVISITATION
  set n_visited_trees count feeding-trees with [visitations > 0]
  set n_unvisited_trees count feeding-trees with [visitations = 0]


end

to calc-movement-metrics

  if R_EXTENSION = "SimpleR" [

    ; ############################################# start of SimpleR extension code ############################################# ;

    if sr-extension-set? = FALSE [ start-r-extension ]

    ; calculating other movement metrics:
    sr:run "db_ <- db %>% make_track(.x=X, .y=Y, id = day, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')" ;%>% nest(data = -c(id))"
    sr:run "mov1 <- db_ %>% mutate( step_length = step_lengths(., append_last = TRUE), turn_ang = direction_rel(., append_last = TRUE) )" ;,  turn_ang = as_degree(turn_ang) )"
                                                                                                                                          ;  print r:get "length(db_metr$step_length)"

    print " ------------- Step lengths/Turning angles ------------------ "

;    print sr:runresult "colnames(mov1)"
;    print sr:runresult "mov1"

;    print " ====== SR calc-movement-metrics debugging 1 ====== "
;    stop

    ;  print sr:runresult "length(mov1$step_length)"
    sr:run "mov1 <- mov1 %>% dplyr::summarise( step_length_mean = mean(step_length, na.rm = TRUE),  step_length_sd = sd(step_length, na.rm = TRUE),  turn_ang_mean = mean(turn_ang, na.rm = TRUE),  turn_ang_sd = sd(turn_ang, na.rm = TRUE) )"
    ;  print sr:runresult "colnames(mov1)"
;      print sr:runresult "mov1"

;    print " ====== SR calc-movement-metrics debugging 2 ====== "
;    stop


    ask monkeys [
      set step_length_mean sr:runresult "mov1 %>% dplyr::select(step_length_mean) %>%  unlist() %>% as.vector()"
      set step_length_sd sr:runresult "mov1 %>% dplyr::select(step_length_sd) %>%  unlist() %>% as.vector()"
      set turn_ang_mean sr:runresult "mov1 %>% dplyr::select(turn_ang_mean) %>%  unlist() %>% as.vector()"
      set turn_ang_sd sr:runresult "mov1 %>% dplyr::select(turn_ang_sd) %>%  unlist() %>% as.vector()"
    ]


    print " ------------- Other movement metrics ------------------ "

    ; nested by day
    ;type "*******" print sr:runresult "db"
    ;  type "*******" print sr:runresult "colnames(db)"

    sr:run "db_ <- db %>%  make_track(.x=Y, .y=X, id = day, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',  all_cols = TRUE)"
    sr:run "db_metr <- db_ %>% amt::nest(data = -'day')  %>%   mutate( MSD = amt::map(data, ~ msd(.)) %>% amt::map(., ~mean(., na.rm = TRUE)) %>% as.numeric(),  intensity_use = amt::map(data, ~ intensity_use(.)) %>% amt::map(., ~mean(., na.rm = TRUE)) %>% as.numeric(),    straightness = amt::map(data, ~ straightness(.)) %>% amt::map(., ~mean(., na.rm = TRUE)) %>% as.numeric() )  "
    sr:run "db_metr <- db_metr %>% dplyr::select(-data)"

    ; you can print all the movement matric variables by day:
;    print "movement metrics by day ---- " print sr:runresult "colnames(db_metr)" print sr:runresult "db_metr"

    sr:run "db_metr <- db_metr %>% na.omit() %>% summarize_all(  list(mean = ~ mean(., na.rm = TRUE)  ) ) "

;    print "==== SR debugging 11 ==== "

    ; not nested by day (considers all days as one path) (WRONG)
    ;  sr:run "db_metr <- db %>%  make_track(.x=Y, .y=X, id = id, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')"
    ;  sr:run "db_metr <- db_metr %>% summarise( MSD = msd(.),  intensity_use = intensity_use(.), straightness = straightness(.), sinuosity = sinuosity(.) )"

    ;    print sr:runresult "colnames(db_metr)"
    ;    print sr:runresult "db_metr"

    ;    print "checkpoint 1"


    ;    ask monkeys [
    ;      set MSD sr:runresult "db_metr %>% dplyr::select(MSD_mean) %>%  unlist() %>% as.vector()"
    ;      set intensity_use sr:runresult "db_metr %>% dplyr::select(intensity_use_mean) %>%  unlist() %>% as.vector()"
    ;      set straightness sr:runresult "db_metr %>% dplyr::select(straightness_mean) %>%  unlist() %>% as.vector()"
    ;;          set sinuosity sr:runresult "db_metr %>% dplyr::select(sinuosity_mean) %>%  unlist() %>% as.vector()"
    ;;      set sinuosity "wrong"
    ;    ]

    ; ############################################# end of SimpleR extension code ############################################# ;
  ]


  ; ############################################# start of R extension code ############################################# ;
 ;    ; calculating other movement metrics
;    r:eval "db_metr <- db %>%  make_track(.x=X, .y=Y, id = id, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')"
;    print r:get "colnames(db_metr)"
;    ;  print r:get "db_metr"
;    ;  print r:get "dim(db_metr)"
;    r:eval "mov1 <- db_metr %>% mutate( step_length = step_lengths(., append_last = TRUE), turn_ang = direction_rel(., append_last = TRUE) )" ;,  turn_ang = as_degree(turn_ang) )"
;                                                                                                                                              ;  print r:get "length(db_metr$step_length)"
;
;    print " ------------- Step lengths/Turning angles ------------------ "
;    ;  print r:get "colnames(mov1)"
;    ;  print r:get "mov1"
;    ;  print r:get "length(mov1$step_length)"
;    r:eval "mov1 <- mov1 %>% summarise( step_length_mean = mean(step_length, na.rm = TRUE),  step_length_sd = sd(step_length, na.rm = TRUE),  turn_ang_mean = circular::mean.circular(turn_ang, na.rm = TRUE),  turn_ang_sd = circular::sd.circular(turn_ang, na.rm = TRUE) )"
;    ;  print r:get "colnames(mov1)"
;    ;  print r:get "mov1"
;
;    ask monkeys [
;      set step_length_mean r:get "mov1 %>% dplyr::select(step_length_mean) %>%  unlist() %>% as.vector()"
;      set step_length_sd r:get "mov1 %>% dplyr::select(step_length_sd) %>%  unlist() %>% as.vector()"
;      set turn_ang_mean r:get "mov1 %>% dplyr::select(turn_ang_mean) %>%  unlist() %>% as.vector()"
;      set turn_ang_sd r:get "mov1 %>% dplyr::select(turn_ang_sd) %>%  unlist() %>% as.vector()"
;    ]
;
;
;    print " ------------- Other movement metrics ------------------ "
;
;    ; nested by day
;    ;type "*******" print r:get "db"
;    ;  type "*******" print r:get "colnames(db)"
;    r:eval "db_metr <- db %>%  make_track(.x=Y, .y=X, id = day, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',  all_cols = TRUE)"
;    r:eval "db_metr <- db_metr %>% amt::nest(data = -'day')  %>%   mutate( MSD = amt::map(data, ~ msd(.)) %>% amt::map(., ~mean(., na.rm = TRUE)) %>% as.numeric(),  intensity_use = amt::map(data, ~ intensity_use(.)) %>% amt::map(., ~mean(., na.rm = TRUE)) %>% as.numeric(),    straightness = amt::map(data, ~ straightness(.)) %>% amt::map(., ~mean(., na.rm = TRUE)) %>% as.numeric() )  "
;    r:eval "db_metr <- db_metr %>% dplyr::select(-data)"
;    ; you can print all the movement matric variables by day:
;    print "movement metrics by day ---- " print r:get "colnames(db_metr)" print r:get "db_metr"
;
;    r:eval "db_metr <- db_metr %>% na.omit() %>% summarize_all(  list(mean = ~ mean(., na.rm = TRUE)  ) ) "
;
;
;    ; not nested by day (considers all days as one path) (WRONG)
;    ;  r:eval "db_metr <- db %>%  make_track(.x=Y, .y=X, id = id, crs = '+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')"
;    ;  r:eval "db_metr <- db_metr %>% summarise( MSD = msd(.),  intensity_use = intensity_use(.), straightness = straightness(.), sinuosity = sinuosity(.) )"
;
;;    print r:get "colnames(db_metr)"
;;    print r:get "db_metr"
;
;;    print "checkpoint 1"
;
;
;    ask monkeys [
;      set MSD r:get "db_metr %>% dplyr::select(MSD_mean) %>%  unlist() %>% as.vector()"
;      set intensity_use r:get "db_metr %>% dplyr::select(intensity_use_mean) %>%  unlist() %>% as.vector()"
;      set straightness r:get "db_metr %>% dplyr::select(straightness_mean) %>%  unlist() %>% as.vector()"
;;          set sinuosity r:get "db_metr %>% dplyr::select(sinuosity_mean) %>%  unlist() %>% as.vector()"
;;      set sinuosity "wrong"
;    ]
;
;    ; calculating mean and sd DPL, MR and PT ONLY IF THE MONKEYS RAN FOR MORE THAN 3 DAYS
;
;    if day > 3 [
;      ask monkeys [
;        let aux-list []
;        foreach DPL_d [
;          ax -> ;print x
;          set aux-list lput (ax ^ 2) aux-list
;          ;     print aux-list
;        ]
;
;        foreach aux-list [
;          ax -> set PT_d lput (ax / (KDE_95 * 10000 ) ) PT_d ; PT = PL² (m)/HR (m²)
;                                                             ;     print PT_d
;        ]
;
;        set MR_mean mean (MR_d)
;        set MR_mean precision MR_mean 4
;        set MR_sd standard-deviation (MR_d)
;        set MR_sd precision MR_sd 4
;
;        set PT_mean mean (PT_d)
;        set PT_mean precision PT_mean 4
;        set PT_sd standard-deviation (PT_d)
;        set PT_sd precision PT_sd 4
;
;        set DPL_mean mean (DPL_d)
;        set DPL_mean precision DPL_mean 4
;        set DPL_sd standard-deviation (DPL_d)
;        set DPL_sd precision DPL_sd 4
;
;        type "MR mean = " print MR_mean
;        type "PT mean = " print PT_mean
;        type "DPL mean = " print DPL_mean
;
;        ;round also other outputs
;        set KDE_95 precision KDE_95 4
;        set KDE_50 precision KDE_50 4
;        set p_feeding precision p_feeding 4
;        set p_foraging precision p_foraging 4
;        set p_traveling precision p_traveling 4
;        set p_resting precision p_resting 4
;
;        set step_length_mean precision step_length_mean 4
;        set step_length_sd precision step_length_sd 4
;        set turn_ang_mean precision turn_ang_mean 4
;        set turn_ang_sd precision turn_ang_sd 4
;        set MSD precision MSD 4
;        set intensity_use precision intensity_use 4
;
;        set straightness precision straightness 6
;        ;      set sinuosity precision sinuosity 6
;
;
;        ; defendability_index DI (Mitani & Rodman, 1979) and M (Lowen & Dunbar 1994)
;        set DI_index ( (DPL_mean / 1000) / ( sqrt ( (4 * (KDE_95 / 100 )) / pi) ) ^ 0.5 )   ; (d / (sqrt(4A/pi)^0.5) (Mitani & Rodman 1979, Lowen & Dunbar 1994)
;        type "DI index = " print DI_index
;        ;      let d_ ( 4 * ( KDE_95 / 1000000 ) / pi )                                     ; in case KDE is in m² and not in ha
;        let d_ ( 4 * ( KDE_95 / 100 ) / pi )                                                ; in case KDE is in ha
;        type "diameter of home range (d' in km) =  " print d_
;        set M_index ( 1 * ( 0.120 * (DPL_mean / 1000) / (d_ ^ 2) ) )                        ; (M = N(sv/d²) Lowen & Dunbar 1994. s value from Ruiz-Miranda et al. 2019 MLD
;        type "M index = " print M_index
;
;      ]
;
;    ]
;
;
;  ]
  ; ############################################# end of R extension code ############################################# ;



  ; setting mean and sd DPL, MR and PT ONLY IF THE MONKEYS RAN FOR MORE THAN 3 DAYS

  if day > 3 [
    ask monkeys [
      let aux-list []
      foreach DPL_d [
        ax -> ;print x
        set aux-list lput (ax ^ 2) aux-list
        ;     print aux-list
      ]

      foreach aux-list [
        ax -> set PT_d lput (ax / (KDE_95_cropped * 10000 ) ) PT_d ; PT = PL² (m)/HR (m²)
                                                           ;     print PT_d
      ]

      set MR_mean mean (MR_d)
      set MR_mean precision MR_mean 4
      set MR_sd standard-deviation (MR_d)
      set MR_sd precision MR_sd 4

      set PT_mean mean (PT_d)
      set PT_mean precision PT_mean 4
      set PT_sd standard-deviation (PT_d)
      set PT_sd precision PT_sd 4

      set DPL_mean mean (DPL_d)
      set DPL_mean precision DPL_mean 4
      set DPL_sd standard-deviation (DPL_d)
      set DPL_sd precision DPL_sd 4

      type "MR mean = " print MR_mean
      type "PT mean = " print PT_mean
      type "DPL mean = " print DPL_mean

      ;round also other outputs
      set KDE_95 precision KDE_95 4
      set KDE_50 precision KDE_50 4
      set KDE_95_cropped precision KDE_95_cropped 4
      set KDE_50_cropped precision KDE_50_cropped 4

      set p_feeding precision p_feeding 4
      set p_foraging precision p_foraging 4
      set p_traveling precision p_traveling 4
      set p_resting precision p_resting 4

      set step_length_mean precision step_length_mean 4
      set step_length_sd precision step_length_sd 4
      set turn_ang_mean precision turn_ang_mean 4
      set turn_ang_sd precision turn_ang_sd 4
      set MSD precision MSD 4
      set intensity_use precision intensity_use 4

      set straightness precision straightness 6
      ;      set sinuosity precision sinuosity 6


      ; defendability_index DI (Mitani & Rodman, 1979) and M (Lowen & Dunbar 1994)
      set DI_index ( (DPL_mean / 1000) / ( sqrt ( (4 * (KDE_95 / 1000000 )) / pi) ) ^ 0.5 )   ; (d / (sqrt(4A/pi)^0.5) (Mitani & Rodman 1979, Lowen & Dunbar 1994)
      type "DI index = " print DI_index
      ;      let d_ ( 4 * ( KDE_95 / 1000000 ) / pi )                                     ; in case KDE is in m² and not in ha
      let d_ ( 4 * ( KDE_95 / 1000000 ) / pi )                                                ; in case KDE is in ha
      type "diameter of home range (d' in km) =  " print d_
      set M_index ( 1 * ( 0.120 * (DPL_mean / 1000) / (d_ ^ 2) ) )                        ; (M = N(sv/d²) Lowen & Dunbar 1994. s value from Ruiz-Miranda et al. 2019 MLD
      type "M index = " print M_index

    ]

  ]




end


to calc-activity-budget
;  foreach ( freq_map behaviorsequence ) [
;    x -> print first x print last x
;  ]

;  print freq_map behaviorsequence
  let activity-values freq_map behaviorsequence
;  print sublist activity-values 0 4 ; same as print activity-values
;  type "activity values: " print activity-values

  let activity-values-ordered sort-by [[list1 list2] -> first list1 < first list2] activity-values

  ;debugging:
;  print activity-values-ordered
;  print item 0 activity-values-ordered
;  print item 1 activity-values-ordered
;  print item 2 activity-values-ordered
;  print item 3 activity-values-ordered

  print " ------------- Activity budget ------------------ "
  ask monkeys [

    if length activity-values-ordered = 4 [
      let p_fee item 0 activity-values-ordered
      let p_for item 1 activity-values-ordered
      let p_tra item 2 activity-values-ordered
      let p_res item 3 activity-values-ordered

      set p_feeding item 1 p_fee
      set p_foraging item 1 p_for
      set p_traveling item 1 p_tra
      set p_resting item 1 p_res

      ;debugging:
      type "p_feeding = " print item 1 p_fee
      type "p_foraging = " print item 1 p_for
      type "p_traveling = "  print item 1 p_tra
      type "p_resting = " print item 1 p_res
      ;    type "Total activity budget = " print ( p_feeding + p_foraging + p_traveling + p_resting )

    ]

    if length activity-values-ordered = 3 [ ; no resting
      let p_fee item 0 activity-values-ordered
      let p_for item 1 activity-values-ordered
      let p_tra item 2 activity-values-ordered
      ;    let p_res item 3 activity-values-ordered

      set p_feeding item 1 p_fee
      set p_foraging item 1 p_for
      set p_traveling item 1 p_tra
      set p_resting 0

      ;debugging:
      type "p_feeding = " print item 1 p_fee
      type "p_foraging = " print item 1 p_for
      type "p_traveling = "  print item 1 p_tra
;      type "p_resting = " print item 1 p_res
      ;    type "Total activity budget = " print ( p_feeding + p_foraging + p_traveling + p_resting )
    ]


  ]

  ; for some reason the following code indexing does not work:
;;  print item 0 item 0 activity-values-ordered
;  print item 1 item 1 activity-values-ordered
;  print item 2 item 1 activity-values-ordered
;  print item 3 item 1 activity-values-ordered


;  ask monkeys [
  ; activity budget:
;    set p_feeding item 0 item 1 activity-values-ordered
;    set p_foraging item 1 item 1 activity-values-ordered
;    set p_traveling item 2 item 1 activity-values-ordered
;    set p_resting item 3 item 1 activity-values-ordered

;    type "activity 1 - %feeding =" print p_feeding
;    type "activity 2 - %foraging = " print p_foraging
;    type "activity 3 - %travel = " print p_traveling
;    type "activity 4 - %resting  = " print  p_resting
;  ]

end


to calc-movement-dead ; if tamarins die before days > no_days, their variables get lost. So we put it as global variables

  if sr-extension-set? = FALSE [ start-r-extension ]

  print "calculating movement from calc-movemement-dead"
  ifelse day > 3 [
;    print day
    output-print "monkeys died"
    output-print "calculating metrics"
    start-r-extension
    calc-homerange
    calc-activity-budget
    ;      calc-movement-metrics ; these are being estimated within calc-homerange
    NNdist
    SDDcalc
    calc-seed-aggregation
    print "estimated from movemend-dead (>3 days)"

  ][
    print "not enough days to calculate movement metrics"
    ; calc near neighbor distance (in NetLogo)
    set day 999
    die
    stop
  ]

  stop

end


to calc-seed-aggregation
  ; based on https://r-ext.sourceforge.net/listing4.php and Felipe Bufalo script

; ############################################# start of SimpleR extension code ############################################# ;
  if R_EXTENSION = "SimpleR" [

    if sr-extension-set? = FALSE [ start-r-extension ]

    sr:run "suppressMessages(library(spatstat))"
    sr:run "suppressMessages(library(maptools))"
    sr:run "suppressMessages(library(sf))"
    sr:run "suppressMessages(library(adehabitatHR))"


    ;; send agent variables into a R data-frame

    if patch-type = "generated" [

      (sr:set-agent-data-frame  "seeds" seeds "who" "x_scaled" "y_scaled")
      ;    (r:putagentdf  "seeds" seeds "who" "xcor" "ycor")
      ;    print r:get "colnames(seeds)"
      ;    print r:get "seeds"

;      ;; OPTION 1: use all turtle locations as owin (as MCP)
;      (sr:set-agent-data-frame "bbox" turtles "who" "x_scaled" "y_scaled")
;      ;    (r:putagentdf "trees" feeding-trees "who" "xcor" "ycor")
;      ;    print r:get "trees"
;      sr:run "xy <- SpatialPoints(bbox[ , 2:3])"
;      ;    print r:get "xy"
;      ;    type "xy = " print r:get "xy <- SpatialPoints(trees[ , 2:3])"

      ;; OPTION 2: use the MCP monkey locations as owin (as MCP)
      sr:run "xy <- SpatialPoints(db[ , 1:2])"
      ;    print r:get "xy"
      ;    type "xy = " print r:get "xy <- SpatialPoints(trees[ , 2:3])"

      sr:run "limitsOwin <- mcp(xy, percent = 100)" ; define mcp as owin
      sr:run "limitsOwin <- as.owin(limitsOwin)"

      ; make location of seeds unique (we are analyzing aggregation of feces, not seeds, because multiple seeds drop at the same place)
      sr:run "seeds <- seeds %>%  dplyr::select(x_scaled, y_scaled)  %>%  dplyr::distinct()"
      ;    sr:run "seeds <- seeds %>%  dplyr::select(xcor, ycor)  %>%  dplyr::distinct()"
      ;    print r:get "seeds"
      sr:run "sim <- ppp(seeds[,1], seeds[,2], window=limitsOwin)"

      ;; calc Nearest Neighbor distance within R
      ;    sr:run "NN_seeds <- mean(nndist(sim))"
      ;
      ;    ; same for trees and sleeping trees
      ;    (r:putagentdf  "ftrees" feeding-trees "who" "x_scaled" "y_scaled")
      ;    (r:putagentdf  "strees" sleeping-trees "who" "x_scaled" "y_scaled")
      ;
      ;    sr:run "ftrees <- ftrees %>%  dplyr::select(x_scaled, y_scaled)  %>%  distinct()"
      ;    sr:run "strees <- strees %>%  dplyr::select(x_scaled, y_scaled)  %>%  distinct()"
      ;    sr:run "simf <- ppp(ftrees[,1], ftrees[,2], window=limitsOwin)"
      ;    sr:run "sims <- ppp(strees[,1], strees[,2], window=limitsOwin)"
      ;    sr:run "NN_feeding_trees <- mean(nndist(simf))"
      ;    sr:run "NN_sleeping_trees <- mean(nndist(sims))"
      ;
      ;    set NN_seeds r:get "NN_seeds"
      ;    set NN_feeding_trees r:get "NN_feeding_trees"
      ;    set NN_sleeping_trees r:get "NN_sleeping_trees"
      ;
      ;        type "NN_seeds = " print NN_seeds
      ;        type "NN_feeding_trees = " print NN_feeding_trees
      ;        type "NN_sleeping_trees = " print NN_sleeping_trees

    ]

    if patch-type = "empirical" [
      (sr:set-agent-data-frame "seeds" seeds "who" "x_UTM" "y_UTM")
      ;    print r:get "colnames(seeds)"
      ;    print r:get "seeds"

      ;; OPTION 1: use all turtle locations as owin (as MCP)
      (sr:set-agent-data-frame "bbox" turtles "who" "x_UTM" "y_UTM")
      sr:run "bbox <- unique(bbox)"
      sr:run "xy <- SpatialPoints(bbox[ , 2:3])"
      sr:run "proj4string(xy) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')"
      sr:run "limitsOwin <- mcp(xy, percent = 100)" ; define mcp as owin
      sr:run "limitsOwin <- as.owin(limitsOwin)"

;      stop


;      ;; OPTION 2: use the MCP monkey locations as owin (as MCP)
;      sr:run "xy <- SpatialPoints(db[ , 1:2])"       ; db is estimated within the SimpleR setup
;;      print sr:runresult "xy[1:10, ]"
;      sr:run "proj4string(xy) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')"
;      sr:run "limitsOwin <- mcp(xy, percent = 100)" ; define mcp as owin
;      sr:run "limitsOwin <- as.owin(limitsOwin)"

      ; make monkey locations unique (ppp requires it)
;      sr:run "xy <- unique(db[ , 1:2])"
;      show sr:runresult "nrow(db)"
;      print sr:runresult "nrow(xy)"

;;      sr:run "limitsOwin <- owin(poly = list(x = db[ , 1], y = db[ , 2]))"
;      sr:run "limitsOwin <- spatstat.geom::owin(poly = list(x = xy[ , 1], y = xy[ , 2]))"
;      sr:run "limitsOwin <- spatstat.geom::ppp(x = xy[ , 1], y = xy[ , 2], limitsOwin)"
;;      sr:run "limitsOwin <- as.owin(limitsOwin)"
;      sr:run "limitsOwin <- spatstat.geom::as.owin(xy)"

;      print sr:runresult "limitsOwin"
;      stop


;      sr:run "xy <- SpatialPoints(xy[ , 1:2])"       ; db is estimated within the SimpleR setup
;;      print sr:runresult "xy[1:10, ]"
;      sr:run "proj4string(xy) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')"
;      sr:run "limitsOwin <- mcp(xy, percent = 100)" ; define mcp as owin
;      sr:run "limitsOwin <- spatstat.geom::as.owin(limitsOwin)"





      ; make location of seeds unique (we are analyzing aggregation of feces, not seeds, because multiple seeds drop at the same place)
      sr:run "seeds <- seeds %>%  dplyr::select(x_UTM, y_UTM)  %>%  dplyr::distinct()"
      sr:run "sim <- ppp(seeds[,1], seeds[,2], window=limitsOwin)"

      ;; calc Nearest Neighbor distance
      sr:run "NN_seeds <- mean(nndist(sim))"

      ; same for trees and sleeping trees
      ;    (sr:set-agent-data-frame  "ftrees" feeding-trees "who" "x_UTM" "y_UTM")
      ;    (sr:set-agent-data-frame  "strees" sleeping-trees "who" "x_UTM" "y_UTM")
      ;
      ;    sr:run "ftrees <- ftrees %>%  dplyr::select(x_UTM, y_UTM)  %>%  dplyr::distinct()"
      ;    sr:run "strees <- strees %>%  dplyr::select(x_UTM, y_UTM)  %>%  dplyr::distinct()"
      ;    sr:run "simf <- ppp(ftrees[,1], ftrees[,2], window=limitsOwin)"
      ;    sr:run "sims <- ppp(strees[,1], strees[,2], window=limitsOwin)"
      ;    sr:run "NN_feeding_trees <- mean(nndist(simf))"
      ;    sr:run "NN_sleeping_trees <- mean(nndist(sims))"
      ;
      ;    set NN_seeds r:get "NN_seeds"
      ;    set NN_feeding_trees r:get "NN_feeding_trees"
      ;    set NN_sleeping_trees r:get "NN_sleeping_trees"
      ;
      ;    type "NN_seeds = " print NN_seeds
      ;    type "NN_feeding_trees = " print NN_feeding_trees
      ;    type "NN_sleeping_trees = " print NN_sleeping_trees
    ]



    ; print r:get "colnames(sim)" ; ppp objects do not have colnames
    ;  type "sim object in R = " print r:get "sim"

    print " ------------- Seed/defecation aggregation ------------------ "
    ;; calc R index (Clark-Evans test)
    sr:run"sim.clark <- clarkevans.test(sim,correction = 'cdf', alternative=c('two.sided'))"

    set R_seeds sr:runresult "sim.clark$statistic %>% as.numeric"
    set R_seeds_p sr:runresult "sim.clark$p.value %>% as.numeric"
    ;    set NN_seeds sr:runresult "NN_seeds"



  ]
; ############################################# end of SimpleR extension code ############################################# ;





; ############################################# start of R extension code ############################################# ;
;  if R_EXTENSION = "R" [
;    r:eval "library(spatstat)"
;    r:eval "library(maptools)"
;    r:eval "library(sf)"
;    r:eval "library(adehabitatHR)"
;
;
;    ;; send agent variables into a R data-frame
;
;    if patch-type = "generated" [
;      (r:putagentdf  "seeds" seeds "who" "x_scaled" "y_scaled")
;      ;    (r:putagentdf  "seeds" seeds "who" "xcor" "ycor")
;      ;    print r:get "colnames(seeds)"
;      ;    print r:get "seeds"
;
;      ;; use all turtle locations as owin (MCP)
;      (r:putagentdf "bbox" turtles "who" "x_scaled" "y_scaled")
;      ;    (r:putagentdf "trees" feeding-trees "who" "xcor" "ycor")
;      ;    print r:get "trees"
;      r:eval "xy <- SpatialPoints(bbox[ , 2:3])"
;      ;    print r:get "xy"
;      ;    type "xy = " print r:get "xy <- SpatialPoints(trees[ , 2:3])"
;
;      r:eval "limitsOwin <- mcp(xy, percent = 100)" ; define mcp as owin
;      r:eval "limitsOwin <- as.owin(limitsOwin)"
;
;      ; make location of seeds unique (we are analyzing aggregation of feces, not seeds, because multiple seeds drop at the same place)
;      r:eval "seeds <- seeds %>%  dplyr::select(x_scaled, y_scaled)  %>%  dplyr::distinct()"
;      ;    r:eval "seeds <- seeds %>%  dplyr::select(xcor, ycor)  %>%  dplyr::distinct()"
;      ;    print r:get "seeds"
;      r:eval "sim <- ppp(seeds[,1], seeds[,2], window=limitsOwin)"
;
;      ;; calc Nearest Neighbor distance within R
;      ;    r:eval "NN_seeds <- mean(nndist(sim))"
;      ;
;      ;    ; same for trees and sleeping trees
;      ;    (r:putagentdf  "ftrees" feeding-trees "who" "x_scaled" "y_scaled")
;      ;    (r:putagentdf  "strees" sleeping-trees "who" "x_scaled" "y_scaled")
;      ;
;      ;    r:eval "ftrees <- ftrees %>%  dplyr::select(x_scaled, y_scaled)  %>%  distinct()"
;      ;    r:eval "strees <- strees %>%  dplyr::select(x_scaled, y_scaled)  %>%  distinct()"
;      ;    r:eval "simf <- ppp(ftrees[,1], ftrees[,2], window=limitsOwin)"
;      ;    r:eval "sims <- ppp(strees[,1], strees[,2], window=limitsOwin)"
;      ;    r:eval "NN_feeding_trees <- mean(nndist(simf))"
;      ;    r:eval "NN_sleeping_trees <- mean(nndist(sims))"
;      ;
;      ;    set NN_seeds r:get "NN_seeds"
;      ;    set NN_feeding_trees r:get "NN_feeding_trees"
;      ;    set NN_sleeping_trees r:get "NN_sleeping_trees"
;      ;
;      ;        type "NN_seeds = " print NN_seeds
;      ;        type "NN_feeding_trees = " print NN_feeding_trees
;      ;        type "NN_sleeping_trees = " print NN_sleeping_trees
;
;    ]
;
;    if patch-type = "empirical" [
;      (r:putagentdf  "seeds" seeds "who" "x_UTM" "y_UTM")
;      ;    print r:get "colnames(seeds)"
;      ;    print r:get "seeds"
;
;      ; load the poligon (.shp) to determine the window (owin) ;; IDEALLY SHOULD BE THE MCP OF THE GROUP
;      ;  if study_area = "Guareí" [ set limitsOwin       "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp" ]
;      ;  if study_area = "Suzano" [ set limitsOwin       "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp" ]
;      ;  if study_area = "Taquara" [ set limitsOwin      "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp" ]
;      ;  if study_area = "SantaMaria" [ set limitsOwin  "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp" ]
;      ;  r:put "limitsOwin" limitsOwin
;      ;  print r:get "limitsOwin"
;      ;  r:eval "limitsOwin) <- sf::st_read(limitsOwin)"
;      ;  r:eval "limitsOwin <- as.owin(limitsOwin))"
;
;      ;; use agents' locations locations as owin (MCP)
;      (r:putagentdf "bbox" turtles "who" "x_UTM" "y_UTM")
;      r:eval "xy <- SpatialPoints(bbox[ , 2:3])"
;      r:eval "proj4string(xy) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')"
;      r:eval "limitsOwin <- mcp(xy, percent = 100)" ; define mcp as owin
;      r:eval "limitsOwin <- as.owin(limitsOwin)"
;
;      ; make location of seeds unique (we are analyzing aggregation of feces, not seeds, because multiple seeds drop at the same place)
;      r:eval "seeds <- seeds %>%  dplyr::select(x_UTM, y_UTM)  %>%  dplyr::distinct()"
;      r:eval "sim <- ppp(seeds[,1], seeds[,2], window=limitsOwin)"
;
;      ;; calc Nearest Neighbor distance
;      r:eval "NN_seeds <- mean(nndist(sim))"
;
;      ; same for trees and sleeping trees
;      (r:putagentdf  "ftrees" feeding-trees "who" "x_UTM" "y_UTM")
;      (r:putagentdf  "strees" sleeping-trees "who" "x_UTM" "y_UTM")
;
;      ;    r:eval "ftrees <- ftrees %>%  dplyr::select(x_UTM, y_UTM)  %>%  dplyr::distinct()"
;      ;    r:eval "strees <- strees %>%  dplyr::select(x_UTM, y_UTM)  %>%  dplyr::distinct()"
;      ;    r:eval "simf <- ppp(ftrees[,1], ftrees[,2], window=limitsOwin)"
;      ;    r:eval "sims <- ppp(strees[,1], strees[,2], window=limitsOwin)"
;      ;    r:eval "NN_feeding_trees <- mean(nndist(simf))"
;      ;    r:eval "NN_sleeping_trees <- mean(nndist(sims))"
;
;      ;    set NN_seeds r:get "NN_seeds"
;      ;    set NN_feeding_trees r:get "NN_feeding_trees"
;      ;    set NN_sleeping_trees r:get "NN_sleeping_trees"
;
;      ;    type "NN_seeds = " print NN_seeds
;      ;    type "NN_feeding_trees = " print NN_feeding_trees
;      ;    type "NN_sleeping_trees = " print NN_sleeping_trees
;    ]
;
;
;
;    ; print r:get "colnames(sim)" ; ppp objects do not have colnames
;    ;  type "sim object in R = " print r:get "sim"
;
;    print " ------------- Seed/defecation aggregation ------------------ "
;    ;; calc R index (Clark-Evans test)
;    r:eval "sim.clark <- clarkevans.test(sim,correction = 'cdf', alternative=c('two.sided'))"
;
;    set R_seeds r:get "sim.clark$statistic"
;    set R_seeds_p r:get "sim.clark$p.value"
;    ;    set NN_seeds r:get "NN_seeds"
;
;
;  ]
; ############################################# end of R extension code ############################################# ;

    type "R index = " print R_seeds
    type "R index p-value  = " print R_seeds_p
    type "Nearest neighbor distance = " type NN_seeds print " meters"

  ask seeds [ set size 3 set color orange ]


end



to NNdist
  ifelse count seeds < 1 [

    print "LESS THAN ONE SEED"

  ][

    ask seeds [
      let myneighbor min-one-of other seeds [distance myself]  ;; choose my nearest neighbor based on distance
                                                               ;    print myneighbor
                                                               ;    if distance myneighbor > 0 [
      set myNND distance myneighbor * patch-scale
    ]

      set NN_seeds mean [MyNND] of seeds with [MyNND > 0]     ; for this to be correct one should make the seed locations unique, otherwise we will have 0 values.
                                                              ;    set NN_seeds mean [MyNND] of seeds                     ; wrong
  ]




  print "*********"


  if count feeding-trees > 1 [
    ask feeding-trees [
      let myneighbor min-one-of other feeding-trees [distance myself]  ;; choose my nearest neighbor based on distance
      set myNND distance myneighbor * patch-scale
  ]
    set NN_feeding_trees mean [MyNND] of feeding-trees
  ]

  if count sleeping-trees > 1 [
    ask sleeping-trees [
      let myneighbor min-one-of other sleeping-trees [distance myself]  ;; choose my nearest neighbor based on distance
      set myNND distance myneighbor * patch-scale
    ]
    set NN_sleeping_trees mean [MyNND] of sleeping-trees
  ]


  type "NN_seeds = " print NN_seeds
    type "NN_feeding_trees = " print NN_feeding_trees
    type "NN_sleeping_trees = " print NN_sleeping_trees

end



to SDDcalc

  ask seeds with [ species = ["Syagrus romanzoffiana"] ] [ die ]

  set g_SDD mean [SDD] of seeds
  set g_SDD_sd standard-deviation [SDD] of seeds

  set g_SDD_sameday mean [SDD] of seeds with [disp-day = "same day"]
  ifelse ( any? seeds with [disp-day = "next day"] ) [
    set g_SDD_nextday mean [SDD] of seeds with [disp-day = "next day"]
  ]  [
    set g_SDD_nextday 0
  ]

  set g_SDD_sd_sameday standard-deviation [SDD] of seeds with [disp-day = "same day"] ;should be the same as sd() in R
   ifelse ( count seeds with [disp-day = "next day"] > 1 ) [
    set g_SDD_sd_nextday standard-deviation [SDD] of seeds with [disp-day = "next day"] ;should be the same as sd() in R
  ] [
    set g_SDD_sd_nextday 0
  ]

  ;debugging:
  let nd-seeds seeds with [disp-day = "next day"]
  ask nd-seeds [ ask mother-tree-agent [ set color blue set size 10 ]]
  ask nd-seeds [ set size 12 ]

  type "g_SDD = " print g_SDD
  type "g_SDD_sd = " print g_SDD_sd
  type "g_SDD_sameday = " print g_SDD_sameday
  type "g_SDD_sd_sameday = " print g_SDD_sd_sameday
  type "g_SDD_nextday = " print g_SDD_nextday
  type "g_SDD_sd_nextday = " print g_SDD_sd_nextday

end


to store-as-globals
  set g_energy_stored energy_stored

  set g_KDE_95 KDE_95_cropped
  set g_KDE_50 KDE_50_cropped
  set g_p_feeding p_feeding
  set g_p_foraging p_foraging
  set g_p_traveling p_traveling
  set g_p_resting p_resting
  set g_step_length_mean step_length_mean
  set g_step_length_sd step_length_sd
  set g_turn_ang_mean turn_ang_mean
  set g_turn_ang_sd turn_ang_sd
  set g_DPL DPL
  set g_DPL_sd DPL_sd
  set g_DPL_d DPL_d
  set g_MR MR_mean
  set g_MR_sd MR_sd
  set g_MR_d MR_d
  set g_PT PT_mean
  set g_PT_sd PT_sd
  set g_PT_d PT_d
  set g_MSD MSD
  set g_intensity_use intensity_use
  set g_straightness straightness
  set g_sinuosity sinuosity

  set g_n_visited_trees n_visited_trees
  set g_n_unvisited_trees n_unvisited_trees

  set g_DI_index DI_index
  set g_M_index M_index



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

to-report p-visited-trees
  let ax count feeding-trees with [visitations > 0] / count feeding-trees
  report ax
end


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

;;   debugging
;  let a ( map [ [ x y ] -> list x ( y / len ) ] uniques counts )
;  print a

  ; report an xy pair for each unique value / proportion
  report ( map [ [ ax ay ] -> list ax ( ay / len ) ] uniques counts )


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

to print-parameters
  type "energy_level_1 = "             print energy_level_1
  type "energy_level_2 = "             print energy_level_2
  type "energy_stored_val = "          print energy_stored_val
;  type "start-energy = "               print start-energy
  type "energy-from-frui = "           print energy-from-fruits
  type "energy-from-prey = "           print energy-from-prey
  type "energy-loss-traveling = "      print energy-loss-traveling
  type "energy-loss-foraging = "       print energy-loss-foraging
  type "energy-loss-resting = "        print energy-loss-resting

  type "step_forget = "                print step_forget
  type "visualevel_1 = "               print visual
  type "prop_trees_to_reset_memory = " print prop_trees_to_reset_memory
  type "p-timesteps-to-rest = "        print p-timesteps-to-rest
  type "duration = "                   print duration
  type "p_disputed_trees = "           print p_disputed_trees

  if feedingbout-on? = "FALSE" [
    type "species_time_val = "           print species_time_val
  ]
end

to set-ref-values
  set energy_level_1 999
  set energy_level_2 1150
  set energy_stored_val 1000
;  set start-energy 905
  set energy-from-fruits 30
  set energy-from-prey 30
  set energy-loss-traveling -10
  set energy-loss-foraging -10
  set energy-loss-resting -5

  set step_forget 400
  set visual 0
  set prop_trees_to_reset_memory 3
  set p-timesteps-to-rest 0.15
  set duration 21
  set p_disputed_trees 0.25

  set species_time_val 5

end

;;; ----------------------------------- ;;;
;;;    GENERATED FORESTS' REPORTERS     ;;;
to-report final-patch-size ; in patches

  if patch-type = "generated" [
    ;    report count patches with [pcolor = green + 1] * 10
    report ( count patches with [habitat = "forest"] + count patches with [habitat = "border"] )
  ]


  if patch-type = "empirical" [
    ; original areas have cut shape files
    if study_area = "SantaMaria" [
      report 515 * 100 ; 515 ha * 10 = number of patches
    ]
    if study_area = "Guareí" [
      report 105 * 100 ; 105 ha * 10 = number of patches
    ]
    if study_area = "Taquara" [
      report 32641 * 100 ; 32641 ha * 10 = number of patches
    ]
    if study_area = "Suzano" [
      report 7600 ; *this is not the forest size of Suzano. We are not sure how to specify this because it is a very complex array of forest fragments. Maybe continuous (> 30000 ha)?
    ]
  ]

end


to-report patch-size-ha ; in hectares
  report final-patch-size / 100
end

;to-report NN-seeds-w0
;   set NN_seeds mean [MyNND] of seeds
;end
@#$#@#$#@
GRAPHICS-WINDOW
0
20
526
403
-1
-1
2.0
1
10
1
1
1
0
0
0
1
-129
129
-93
93
0
0
1
ticks
30.0

SLIDER
0
893
172
926
start-energy
start-energy
100
2000
905.0
1
1
NIL
HORIZONTAL

BUTTON
607
35
679
71
SETUP
if any? monkeys [ stop-inspecting one-of monkeys ]\nsetup\ninspect one-of monkeys
NIL
1
T
OBSERVER
NIL
P
NIL
NIL
1

SWITCH
5
635
123
668
show-energy?
show-energy?
1
1
-1000

SWITCH
5
680
125
713
show-path?
show-path?
0
1
-1000

SLIDER
567
110
739
143
simulation-time
simulation-time
0
170
112.0
1
1
NIL
HORIZONTAL

SLIDER
553
469
725
502
energy-from-fruits
energy-from-fruits
0
300
73.0
1
1
NIL
HORIZONTAL

BUTTON
597
207
664
240
STEP
;ask monkeys [ type \"tree_target = \" print tree_target ]\n\nstep\n\n\n; debug species_time:\n;type \"species_time = \" print species_time\n;type \"frugivory_time = \" print [frugivory-time] of monkeys\n;\n;type \"tree_target_mem2 = \" print [tree_target_mem] of monkeys\n\nprint \" ================ NEXT STEP ==============\"
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
607
70
677
104
go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

BUTTON
677
70
760
103
Next Day
next_day\nask monkeys [ print DPL_d ]
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
677
35
761
70
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
542
35
604
105
no_days
6.0
1
0
Number

SLIDER
553
507
725
540
energy-from-prey
energy-from-prey
0
300
31.0
1
1
NIL
HORIZONTAL

SLIDER
553
544
726
577
energy-loss-traveling
energy-loss-traveling
-100
0
-15.0
1
1
NIL
HORIZONTAL

SLIDER
553
579
726
612
energy-loss-foraging
energy-loss-foraging
-100
0
-10.0
1
1
NIL
HORIZONTAL

SLIDER
553
619
726
652
energy-loss-resting
energy-loss-resting
-100
0
-5.0
1
1
NIL
HORIZONTAL

MONITOR
617
155
693
200
timestep
timestep
17
1
11

OUTPUT
292
621
516
734
11

INPUTBOX
133
635
286
721
runtime
./runtime/
1
0
String

CHOOSER
1010
47
1160
92
feeding-trees-scenario
feeding-trees-scenario
"All months" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
1

CHOOSER
1017
122
1163
167
sleeping-trees-scenario
sleeping-trees-scenario
"empirical" "simulated"
0

SWITCH
5
595
125
628
export-png
export-png
1
1
-1000

SLIDER
747
357
909
390
step_forget
step_forget
0
500
400.0
1
1
NIL
HORIZONTAL

TEXTBOX
586
330
722
366
2. energy related
14
15.0
1

TEXTBOX
760
307
897
341
3. memory related
14
15.0
1

SLIDER
564
724
715
757
gut_transit_time
gut_transit_time
0
100
16.0
1
1
NIL
HORIZONTAL

TEXTBOX
947
307
1104
341
4. movement related
14
15.0
1

TEXTBOX
777
587
917
623
5. feeding bout
14
15.0
1

SLIDER
571
353
707
386
energy_level_1
energy_level_1
100
2000
900.0
1
1
NIL
HORIZONTAL

SLIDER
571
388
707
421
energy_level_2
energy_level_2
100
2000
1350.0
1
1
NIL
HORIZONTAL

TEXTBOX
574
672
701
708
6. seed dispersal
14
15.0
1

SLIDER
564
764
713
797
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
1017
162
1163
207
empirical-trees-choice
empirical-trees-choice
"closest" "random"
0

MONITOR
557
155
614
200
day
day
17
1
11

SWITCH
849
265
968
298
sleeping-trees?
sleeping-trees?
0
1
-1000

SWITCH
849
233
968
266
feeding-trees?
feeding-trees?
0
1
-1000

TEXTBOX
554
17
803
35
1. Resources scenario
12
15.0
1

SWITCH
1037
237
1163
270
all-slp-trees?
all-slp-trees?
1
1
-1000

TEXTBOX
1042
212
1147
240
make all trees from study period available:
9
0.0
1

BUTTON
133
607
258
640
middayend steps
repeat midday_end [ step ]
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
488
207
593
240
print-step?
print-step?
1
1
-1000

PLOT
1419
385
1642
579
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
"pen-1" 1.0 0 -2674135 true "" "ask monkeys [ plot 0.1 + length tree_mem_list ]"
"pen-2" 1.0 0 -1184463 true "" "ask monkeys [ plot length tree_ate_list ]"
"pen-3" 1.0 0 -13840069 true "" "ask monkeys [ plot (length tree_mem_list + length tree_pot_list) + 2 ]"
"pen-4" 1.0 0 -11221820 true "" "let n_trees round ( count feeding-trees  / prop_trees_to_reset_memory ) + 3\nplot n_trees"

TEXTBOX
1007
102
1204
130
1.4 Choose sleeping site scenario
11
0.0
1

TEXTBOX
1002
32
1181
61
1.3 Choose fruit trees scenario
11
0.0
1

BUTTON
682
207
759
240
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
968
649
1113
682
duration
duration
0
30
5.0
1
1
NIL
HORIZONTAL

MONITOR
697
155
755
200
Energy
[ round energy ] of monkeys
3
1
11

SLIDER
0
853
164
886
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
740
332
907
357
How many timesteps BLTs take to reconsider revisiting a tree:
10
0.0
1

TEXTBOX
4
826
171
852
(not being used) exclude trees in radii from pot list:
10
0.0
1

SWITCH
5
720
170
753
path-color-by-day?
path-color-by-day?
1
1
-1000

TEXTBOX
30
439
118
484
OUTPUT:
18
15.0
1

CHOOSER
0
465
128
510
USER
USER
"Ronald" "Eduardo" "LEEC" "LASi" "PC02" "Others"
1

SWITCH
3
553
121
586
output-print?
output-print?
1
1
-1000

SLIDER
946
565
1125
598
p_foraging_while_traveling
p_foraging_while_traveling
0
1
0.21
0.01
1
NIL
HORIZONTAL

TEXTBOX
986
682
1136
726
max sequential timesteps resting
9
0.0
1

PLOT
1202
352
1410
476
species_time (black) / frugivory-time (blue)
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
"default" 1.0 0 -5298144 true "" ";ask monkeys [ plot 2 * duration ]"
"pen-1" 1.0 0 -16777216 true "" "ask monkeys [\n  if tree_target != -1 [\n    plotxy ( ticks ) ( [species_time] of tree_target )\n    ]\n  ]\n  \n;ask monkeys [ ifelse tree_current != -1 [ plot [ species_time ] of tree_current ] [ plot 0 ] ]"
"pen-2" 1.0 0 -2139308 true "" ";ask monkeys [ plot action-time ]"
"pen-3" 1.0 0 -10649926 true "" "ask monkeys [ plot frugivory-time ]"

PLOT
1415
30
1659
220
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
"default" 1.0 1 -16777216 true "" "clear-plot\nforeach ( freq_map behaviorsequence ) [ ax -> \nplotxy first ax last ax ]"
"pen-1" 1.0 0 -7500403 true "" "plot 0.5"

TEXTBOX
1472
202
1515
222
frugivory
8
0.0
1

TEXTBOX
1512
202
1550
223
foraging
8
0.0
1

TEXTBOX
1552
202
1579
222
travel
8
0.0
1

TEXTBOX
1582
200
1615
220
resting
8
0.0
1

PLOT
1202
225
1410
348
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
"pen-2" 1.0 0 -11053225 true "" "plot energy_level_2"
"pen-3" 1.0 0 -2674135 true "" "ask monkeys [ plot energy_stored ]"

TEXTBOX
942
362
1116
389
add empirical step parameters. If so:
9
0.0
1

BUTTON
403
500
500
535
NIL
test-long-distance
NIL
1
T
OBSERVER
NIL
L
NIL
NIL
1

PLOT
1207
602
1407
752
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
754
607
901
640
feedingbout-on?
feedingbout-on?
0
1
-1000

BUTTON
134
465
253
498
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
263
465
359
499
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
467
499
501
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
134
503
396
536
show tree_pot_list
ask monkeys [ show length tree_pot_list \ntype \"tree_current: \" show tree_current\n]
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
742
401
906
434
prop_trees_to_reset_memory
prop_trees_to_reset_memory
2
8
8.0
1
1
NIL
HORIZONTAL

TEXTBOX
742
391
892
409
don't choose 1:
9
0.0
1

TEXTBOX
754
642
904
664
energy and time spent feeding for each tree species. If not:
9
0.0
1

PLOT
1202
477
1410
597
duration (red) / action_time (blue)
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
"default" 1.0 0 -13345367 true "" "ask monkeys [ plot action-time ]"
"pen-1" 1.0 0 -2674135 true "" ";ask monkeys [ ifelse tree_current != -1 [ plot [ duration ] of tree_current ] [ plot 0 ] ]\n\nplot duration\n\n"

BUTTON
134
535
243
570
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
1201
94
1410
231
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
"pen-1" 1.0 2 -2674135 true "" "ask monkeys [ if action = \"sleeping\" AND length DPL_d > 0 [ plotxy ticks last DPL_d ] ]\n\n;ask monkeys [ if action = \"sleeping\" [ plot DPL * patch-scale ] ]\n;ask monkeys [ foreach DPL_d [ dpl_i -> plot dpl_i ] ]\n"
"pen-2" 1.0 0 -7500403 true "" ";ask monkeys [ if behavior = \"sleeping\" [ plot mean DPL_d ] ]\n;ask monkeys [ plot mean DPL_d ]"

TEXTBOX
840
92
1007
112
1.1 choose fragment
11
0.0
1

CHOOSER
842
112
981
157
study_area
study_area
"Guareí" "SantaMaria" "Taquara" "Suzano"
2

BUTTON
245
537
368
571
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
210
438
448
482
MODEL VERIFICATION:
18
15.0
1

TEXTBOX
838
222
987
246
1.2 Create tree resources
11
0.0
1

MONITOR
457
75
543
120
patch size (m)
patch-scale
17
1
11

BUTTON
373
537
501
571
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
134
573
241
607
NIL
clear-drawing
NIL
1
T
OBSERVER
NIL
C
NIL
NIL
1

BUTTON
244
575
379
608
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
952
327
1095
360
step-model-param?
step-model-param?
0
1
-1000

SLIDER
940
412
1119
445
max_rel_ang_forage_75q
max_rel_ang_forage_75q
0
180
43.02
5
1
NIL
HORIZONTAL

SLIDER
940
485
1113
518
step_len_forage
step_len_forage
0
20
3.089
0.1
1
NIL
HORIZONTAL

SLIDER
940
452
1113
485
step_len_travel
step_len_travel
0
20
3.931
0.1
1
NIL
HORIZONTAL

SLIDER
940
382
1119
415
max_rel_ang_travel_75q
max_rel_ang_travel_75q
0
180
17.85
1
1
NIL
HORIZONTAL

SLIDER
754
670
902
703
species_time_val
species_time_val
1
20
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
769
707
893
736
max timesteps feeding on the same tree species
10
0.0
1

SWITCH
967
529
1097
562
p-forage-param?
p-forage-param?
0
1
-1000

SWITCH
579
692
696
725
gtt-param?
gtt-param?
0
1
-1000

PLOT
1420
232
1620
382
SDD
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
"default" 1.0 0 -16777216 true "" "plot mean [SDD] of seeds"
"pen-1" 1.0 0 -14070903 true "" "plot min [SDD] of seeds"
"pen-2" 1.0 0 -2674135 true "" "plot max [SDD] of seeds"

SLIDER
744
510
916
543
p_disputed_trees
p_disputed_trees
0
1
0.6
0.05
1
NIL
HORIZONTAL

TEXTBOX
745
487
895
509
random or border ld trees\ndepends on n feeding-trees:
9
0.0
1

SWITCH
756
452
894
485
ld-target-random?
ld-target-random?
1
1
-1000

CHOOSER
832
52
970
97
patch-type
patch-type
"empirical" "generated"
0

MONITOR
457
30
545
75
NIL
patch-size-ha
17
1
11

INPUTBOX
792
162
1011
222
generated_patch
generated_patches/8500_1.6.csv
1
0
String

BUTTON
392
575
488
609
tree_target
;ask one-of monkeys [ inspect tree_target ]\nfollow [ tree_target ] of one-of monkeys\n
NIL
1
T
OBSERVER
NIL
U
NIL
NIL
1

BUTTON
421
404
522
437
print targets
ask monkeys [ type \"tree_tgt = \" print tree_target type \"tree_current = \" print tree_current ]
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
427
365
531
398
dist to target
ask monkeys [ print distance tree_target ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1178
13
1393
73
path
D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/
1
0
String

SLIDER
824
10
996
43
monkey_runs
monkey_runs
1
100
5.0
1
1
NIL
HORIZONTAL

BUTTON
733
5
841
38
mov variables
type \"DPL_mean = \" ask monkeys [ show DPL ]\ntype \"DPL_sd = \" ask monkeys [ show DPL_sd ]\n\ntype \"PT_mean = \" ask monkeys [ show PT ]\ntype \"PT_sd = \" ask monkeys [ show PT_sd ]\ntype \"MR_mean = \" ask monkeys [ show MR ]\ntype \"MR_sd = \" ask monkeys [ show MR_sd ]\n\ntype \"DPL_d = \" ask monkeys [ show DPL_d ]\ntype \"PT_d = \" ask monkeys [ show PT_d ]\ntype \"MR_d = \" ask monkeys [ show MR_d ]\n
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
849
10
961
43
reset monkeys
;ask turtles [ die ]\nask monkeys [ die ]\nask seeds [ die ]\nset day 1\nset timestep 0\n;ask monkeys [ die ]\n;set survived? 0\n;ask seeds [ die ]\nsetup-monkeys\nreset-ticks\nclear-drawing\ninspect one-of monkeys\n\nask monkeys [ set energy_stored 1000 ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
459
0
537
45
NIL
hr-size-final
17
1
11

MONITOR
1119
297
1203
342
energy_stored
[energy_stored] of monkeys
17
1
11

SLIDER
554
421
726
454
energy_stored_val
energy_stored_val
0
10000
1000.0
1
1
NIL
HORIZONTAL

TEXTBOX
1167
78
1317
118
Guareí = May, Jun, Jul, Aug\nSanta Maria = Mar, Apr\nTaquara = Jan\nSuzano = Sep, Dec (Feb and Apr for debugging avoid-matrix)
6
0.0
1

SLIDER
951
610
1123
643
p-timesteps-to-rest
p-timesteps-to-rest
0
1
0.15
0.01
1
NIL
HORIZONTAL

CHOOSER
5
515
143
560
R_EXTENSION
R_EXTENSION
"R" "SimpleR"
1

BUTTON
247
365
398
398
calc-homerange
;repeat 3 [ next_day ]\n;ask monkeys [ print DPL_d ]\n\nif sr-extension-set? = FALSE [ start-r-extension ]\ncalc-homerange
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
157
365
236
398
print var
  ask one-of monkeys [\n      print \"==== SR debugging ==== \"\n\n      let X_coords_sr [X_coords] of self    print X_coords_sr  print length X_coords_sr\n      let Y_coords_sr [Y_coords] of self    print Y_coords_sr  print length Y_coords_sr\n      let day_list_sr [day_list] of self    print day_list_sr  print length day_list_sr\n      let Name_sr [Name] of self            print Name_sr  ;print length Name_sr\n]
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
586
272
694
305
set ref values
set-ref-values
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
760
543
901
576
color disputed trees
ask feeding-trees [ set color green ]\n\ntest-long-distance\n\n ask monkeys [\n  \n  if travel_mode = \"long_distance\" [\n    let let_pot_list tree_pot_list\n\n    if ld-target-random? = TRUE [\n      ifelse tree_target_mem2 = -1 [\n        set ld_tree_target one-of feeding-trees with [member? who let_pot_list]\n      ] [\n        set ld_tree_target tree_target_mem2\n        set tree_target_mem2 -1 ; reset\n      ]\n\n    ]\n\n\n    if ld-target-random? = FALSE [\n      ;; RANDOM TREE AT THE BORDER OF THE HOME RANGE (TERRITORIALITY)\n      set let_pot_list tree_pot_list\n      set candidate_ld_targets feeding-trees with [member? who let_pot_list]\n      type \"candidate_ld_targets = \" print candidate_ld_targets\n\n\n;      print \"LD_CHECK\"\n\n      set candidate_ld_targets max-n-of ( round (p_disputed_trees * (length let_pot_list) ) ) candidate_ld_targets [dist-to-homerange-center]\n      ; debugging:\n;      ask monkeys [ ask max-n-of ( round (p_disputed_trees * (length tree_pot_list) ) ) candidate_ld_targets [dist-to-homerange-center] [ set color pink ] ]\n\n;      if length candidate_ld_targets = 0 [ set candidate_ld_targets candidate_ld_targets with-max [dist-to-homerange-center] ]\n;      set candidate_ld_targets candidate_ld_targets with-max [dist-to-homerange-center]\n\n      ;    print candidate_ld_targets\n      ask candidate_ld_targets [\n        set color yellow\n        if study_area = \"Taquara\" [ set size 5 ]\n      ]\n     ]\n    ]\n       \n   ]
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
398
396
431
calc-movement-metrics
if sr-extension-set? = FALSE [ start-r-extension ]\ncalc-movement-metrics
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
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
NetLogo 6.3.0
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
  <experiment name="Sensitivity-Analysis-Dec2022-GuaJul" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>r:stop</final>
    <timeLimit steps="2000"/>
    <exitCondition>day &gt; no_days</exitCondition>
    <metric>;globals</metric>
    <metric>p-visited-trees</metric>
    <metric>R_seeds</metric>
    <metric>R_seeds_p</metric>
    <metric>NN_seeds</metric>
    <metric>;monkeys</metric>
    <metric>[DPL] of monkeys</metric>
    <metric>[DPL_sd] of monkeys</metric>
    <metric>[KDE_95] of monkeys</metric>
    <metric>[KDE_50] of monkeys</metric>
    <metric>[p_feeding] of monkeys</metric>
    <metric>[p_foraging] of monkeys</metric>
    <metric>[p_traveling] of monkeys</metric>
    <metric>[p_resting] of monkeys</metric>
    <metric>[MR] of monkeys</metric>
    <metric>[MR_sd] of monkeys</metric>
    <metric>[PT] of monkeys</metric>
    <metric>[PT_sd] of monkeys</metric>
    <metric>;seeds</metric>
    <metric>[SDD] of seeds</metric>
    <enumeratedValueSet variable="study_area">
      <value value="&quot;Guareí&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="feeding-trees-scenario">
      <value value="&quot;Jul&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-time">
      <value value="102"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no_days">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy_level_1">
      <value value="600"/>
      <value value="1000"/>
      <value value="1400"/>
      <value value="1800"/>
      <value value="2200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy_level_2">
      <value value="600"/>
      <value value="1000"/>
      <value value="1400"/>
      <value value="1800"/>
      <value value="2200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-energy">
      <value value="600"/>
      <value value="1000"/>
      <value value="1400"/>
      <value value="1800"/>
      <value value="2200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-fruits">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-loss-resting">
      <value value="-30"/>
      <value value="-50"/>
      <value value="-100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-loss-foraging">
      <value value="-30"/>
      <value value="-50"/>
      <value value="-100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-loss-traveling">
      <value value="-30"/>
      <value value="-50"/>
      <value value="-100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species_time">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step_forget">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
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
