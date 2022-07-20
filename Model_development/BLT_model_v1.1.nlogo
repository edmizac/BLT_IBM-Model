; ==== Black lion tamarin model ============
; Eduardo Zanette & Ronald Bialozyt
; Two travel modes: long-distance (target selected when energy > lvl 2 and travels to this direction up to when energy < lvl 1) and short-distance (when energy < lvl 1)
; Random angle at each step (random-angle? switch)
; Phenology values (specific energy and species-time values on and off by switcher phenology-on? Stil need to add phenology cycles though)
; ------------------------------------------------

extensions [ gis palette] ; using the GIS extension of NetLogo

;; BREEDS ;;
; trees
turtles-own [ x_UTM y_UTM ]
breed [feeding-trees feeding-tree]
feeding-trees-own [ species id-tree ] ; feeding trees have species and id code
breed [sleeping-trees sleeping-tree]
sleeping-trees-own [ species id-tree ]
breed [resting-trees resting-tree]
resting-trees-own [ species id-tree ]
breed [legend-trees legend-tree] ; to set up a legend with the color of trees
; seeds
breed [seeds seed]
seeds-own [ id-seed species mother-tree ]
; tamarins
breed [monkeys monkey]
monkeys-own [
  energy          ; energy the tamarin has left
;  status         ; what is the desire ===== DO WE REALLY NEED THIS? ==============
  action          ; what was the last action
  action-time     ; how long you do the same action again
  behavior        ; as in activity budget data tables
  steps-moved     ; number of steps taken
  dist-traveled   ; distance traveled this time step
  DPL             ; daily path length
  DPL_d           ; list with values of DPL for the DPL plot
  travel_mode     ; if it is short or long distance travel
  tree_target     ; target tree (short distance)
  ld_tree_target  ; long distance target tree
  tree_target_species ; species of the target tree independent of travel mode
  travelmodelist  ; list to make travel mode histogram
  tree_current    ; old_tree
  tree_pot_list   ; list of all feeding trees in homerange for that tamarin
  tree_ate_list   ; list of trees the tamarins did eat
  tree_mem_list   ; list of timesteps since the tamarin feeded on that tree
  tree_add_list   ; helper list to increase the mem list
  seed_ate_list   ; list of the seeds they fed on
  seed_mem_list   ; list of timesteps since the tamarin ate the seed
  seed_add_list   ; helper list to increase the mem list by 1 each time step
;  x_UTM
;  y_UTM

]

;; GLOBALS ;;
globals [
  behaviorsequence

  timestep ; step counter during one day
  day  ; present day in the simulation
  ;scale ; to fit the data to the simulation area
  meanxcoord ; translating the geo coordinates to world coordinates
  meanycoord ; translating the geo coordinates to world coordinates
  ;step_forget ; amount of timesteps until the tamarin forgets to be in that tree
  midday ; the time of the middle of the day (important for resting)

  ;; INPUT ;;
  ;gis;
  guarei-dataset
  shape-type
  property-names
  feature-list
  vertex-lists
  tree-file ; filename with the tree location and type
  sleep-file ; filename with the location of all sleeping sites

  ;param values;
  gut_transit_time ; amount of timesteps until the tamarin defecates (time the seed takes to go throught all the digestive system)
  travel_speed ; global speed for travel
;  foraging_speed ; global speed for foraging
; foraging_time ; global for how long the tamarins should spend on foraging (3 timesteps)
  species_time ; how long the tamarin feeds on the tree species
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
  if USER = "Others"
  [ set local-path "~/" ]


  setup-patches
  setup-gis
  setup-trees
  setup-monkeys

  output-files

  create-legend

  set day 1
  set midday 58
  set timestep 0
  set gut_transit_time gut_transit_time_val
  set travel_speed travel_speed_val
;  set foraging_speed foraging_speed_val
;  set species_time species_time_val

  reset-ticks
end

; PATCHES
to setup-patches
  ask patches [set pcolor yellow + 4]
end

; GIS
to setup-gis

  ; Based on: https://s3.amazonaws.com/complexityexplorer/ABMwithNetLogo/Field+Guide+to+NetLogo+v14-netlogoExtension-index_02.pdf
  set scale 28.28803 ; scale-size. Calculated based on speed_val = 1.0 (dist between UTM coordinates)
  resize-world -25 25 -25 25 ; same thing as selecting 25 x 25 in the interface
  set-patch-size 10

  gis:load-coordinate-system word ( local-path) "/Data/Shapefiles/Guarei-poligono.prj"
  set guarei-dataset gis:load-dataset word ( local-path) "/Data/Shapefiles/Guarei-poligono.shp"

  gis:set-drawing-color gray + 3
  let pen-width 1
  gis:draw guarei-dataset pen-width
  gis:set-drawing-color lime + 3
  gis:fill guarei-dataset 2

  set shape-type gis:shape-type-of guarei-dataset
  set property-names gis:property-names guarei-dataset
  set feature-list gis:feature-list-of guarei-dataset
  set vertex-lists gis:vertex-lists-of item 0 feature-list

;;;;;;;;; BLT Model v1 code:  ;;;;;;;;;;;
;  set scale 32 ; scale-size. Is this being used?
;  let mcp-gis gis:load-dataset word ( local-path) "/Data/Shapefiles/poligono_matriz.shp" ; area containing fragment and matrix
;  let trees-gis gis:load-dataset word ( local-path) "/Data/Shapefiles/bbox_certo_buffer.shp" ; home range bounding box
;  let bb-gis gis:load-dataset word ( local-path) "/Data/Shapefiles/polig_fragmento.shp" ; fragment/study area polygon
;  let all-gis gis:load-dataset word ( local-path) "/Data/Shapefiles/all_trees_shape.shp" ; points shape for the all the trees
;
;
;  gis:set-world-envelope (gis:envelope-of bb-gis) ; or, for a predefined domain (Banos et al 2015): gis:set-transformation gis:envelope-of name_of_the_layer [min-pxcor max-pxcor min-pycor max-pycor]
;  gis:set-drawing-color lime + 3
;  gis:draw mcp-gis 2
;  gis:set-drawing-color yellow
;  ask patches gis:intersecting bb-gis [ set pcolor lime + 3 ]
;  ask patches gis:intersecting trees-gis [set pcolor lime + 3 ]
;  foreach gis:feature-list-of all-gis [ vector-feature ->
;    gis:set-drawing-color scale-color lime (gis:property-value vector-feature "id") 500 1
;    gis:fill vector-feature 2.0 ]

end

; TREES INPUT
to setup-trees

;; SLEEPING TREES ONLY (ALL MONTHS)
  let id-tree-slp 0

  if ( sleeping-trees-scenario = "empirical" AND all-slp-trees? = TRUE )  [
    set sleep-file word ( local-path) "/Data/Trees-Guarei/Guarei_trees_unique_slp.shp" ;

    let sleep-gis gis:load-dataset sleep-file ; defined by tree-scenario chooser
    foreach gis:feature-list-of sleep-gis [ vector-feature ->
      let location-slp gis:location-of (first (first (gis:vertex-lists-of vector-feature)))
      set id-tree-slp gis:property-value vector-feature "point"

      create-sleeping-trees 1 [
        set size 1
        set shape "tree"
        set color magenta
        setxy item 0 location-slp item 1 location-slp
        set id-tree-slp gis:property-value vector-feature "point"
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
  ]]]

 ;; load tree-file according to tree-scenario chooser (.SHP)
  let number 0
  let xcoord 0
  let ycoord 0
  let tree-species 0
  let tree-type 0 ; phenology types
  let tree-id-tree 0

  ;; ALL TREES (DEFINED BY feeding-trees-scenario CHOOSER AND ****ing-tree INTERRUPTOR

  if ( feeding-trees-scenario = "trees_all" )   [ set tree-file word ( local-path) "/Data/Trees-Guarei/Guarei_trees_unique_all.shp" ]   ;
  if ( feeding-trees-scenario = "trees_may" )   [ set tree-file word ( local-path) "/Data/Trees-Guarei/Guarei_trees_unique_may.shp" ]   ;
  if ( feeding-trees-scenario = "trees_jun" )   [ set tree-file word ( local-path) "/Data/Trees-Guarei/Guarei_trees_unique_jun.shp" ]   ;
  if ( feeding-trees-scenario = "trees_jul" )   [ set tree-file word ( local-path) "/Data/Trees-Guarei/Guarei_trees_unique_jul.shp" ]   ;
  if ( feeding-trees-scenario = "trees_aug" )   [ set tree-file word ( local-path) "/Data/Trees-Guarei/Guarei_trees_unique_aug.shp" ]   ;

  let trees-gis gis:load-dataset tree-file ; defined by tree-scenario chooser
  foreach gis:feature-list-of trees-gis [ vector-feature ->
    let location gis:location-of (first (first (gis:vertex-lists-of vector-feature)))
    set tree-type gis:property-value vector-feature "behavior"
    set tree-species gis:property-value vector-feature "species"

    if ( feeding-trees? = TRUE AND tree-type = "feed_fruits" ) [
      create-feeding-trees 1 [
        set size 1
        set shape "tree"
        set color green
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "point"
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
    ]];

    if ( resting-trees? = TRUE AND tree-type = "resting" ) [
      create-resting-trees 1 [
        set size 1
        set shape "tree"
        set color 77
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "point"
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
    ]]
    ;

    if ( sleeping-trees? = TRUE AND all-slp-trees? = FALSE AND tree-type = "sleeping"  ) [
      create-sleeping-trees 1 [
        set size 1
        set shape "tree"
        set color magenta
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "point"
        set x_UTM (item 0 gis:envelope-of self)
        set y_UTM (item 2 gis:envelope-of self)
    ]] ;
  ]

end


; TAMARINS
to setup-monkeys

  create-monkeys 1
  ask monkeys [

    set behaviorsequence []
;    set behaviorsequence lput "" behaviorsequence

;;    set shape "banana"
    set color black
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

    set steps-moved 0
    set action-time 0
    set action "travel"
    set behavior ""
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
      set pen-mode "down"
      set pen-size 0.2
      set color gray
;;      set Color blue
    ][ set pen-mode "up" ]
  ] ; end ask monkeys

end

; LEGEND
to create-legend

  create-legend-trees 1 [ ; tamarin
    set size 1.5
    ;;      set shape "banana"
    set color black
    setxy min-pxcor + 1 max-pycor - 1
  ]
  create-legend-trees 1 [ ; feeding
    set size 1
    set shape "tree"
    set color green
    setxy min-pxcor + 1 max-pycor - 3
  ]
  create-legend-trees 1 [ ; sleeping
    set size 1
    set shape "tree"
    set color magenta
    setxy min-pxcor + 1 max-pycor - 5
  ]
  create-legend-trees 1 [ ; seeds
    set size 0.5
    set shape "circle"
    set color black
    setxy min-pxcor + 1 max-pycor - 7
  ]
  create-legend-trees 1 [ ; short distance target tree
    set size 1
    set shape "tree"
    set color red
    setxy min-pxcor + 2 min-pycor + 1
  ]
  create-legend-trees 1 [ ; long distance target tree
    set size 1
    set shape "tree"
    set color blue
    setxy min-pxcor + 2 min-pycor + 3
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

  if all? monkeys [action = "sleeping"] [
    set day day + 1
    set timestep 0
    ask monkeys [ set action "travel" ]
    if day > no_days [
      output-print "AHOY"
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
;      type "tree_ate_list: " print length tree_ate_list print tree_ate_list
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
      type "DPL (m): " print DPL
;      ifelse travel_mode = "short_distance" [
;        ; print distance tree_target
;      ][
;        print distance ld_tree_target
;      ]
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
  output-type "*simulation-time* "
  output-type simulation-time
  output-print " ----"


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

    if patch-ahead pcolor = yellow + 4 [ right 180 forward ( travel_speed / 3 ) ] ; no use of the matrix
    if energy < 1 [ die ]

    set x_UTM (item 0 gis:envelope-of self)
    set y_UTM (item 2 gis:envelope-of self)

;    ;; procedure not working for some reason (to plot average path length)
;    if action = "sleeping" [
;      set DPL_d lput DPL DPL_d
;    ]

  ;; BLT ROUTINE

    if timestep = 1
        [ set tree_current -1
          set DPL 0 ; set daily path length to 0 every day
          morning-defecation ]              ;; MORNING-DEFECATION PROCEDURE

    if timestep >= simulation-time [
      if timestep = simulation-time [ set tree_target -1 ] ; force monkey select a sleeping site
      sleeping
      if output-print? = TRUE [
        output-day-stats
      ]
    ]


    if timestep < simulation-time [ ; energy levels: energy_level_1 = 80 and energy_level_2 = 150
      enhance_memory_list

      ifelse energy < energy_level_1 [ ; energy < level 1
        set travel_mode "short_distance"
        if ld_tree_target = tree_target [
          set tree_target -1 ; remove tree_target when coming from "long_distance"
          set ld_tree_target -1  ; if this is not here it will make the tamarin lose the target very close to the tree when coming from long distance bc of the condition ld_tree_target = tree_target (Ronald debugged on the 14th of July 2022)
        ]
        frugivory
      ][
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
            frugivory
          ][
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
; the whole loop for frugivory
;--------------------------------------------------------------------------------
to frugivory

;  print "frugivory"  ; debugging

  if travel_mode = "short_distance" [   ;; short distance frugivory
    set travelmodelist lput 1 travelmodelist ; to the travel mode histogram
    ifelse on-feeding-tree? [
;      ifelse random (2 * species_time ) > action-time [
      ifelse random (2 * [species_time] of tree_current) > action-time [
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
      ifelse random (2 * species_time) > action-time [
        feeding
      ][
        set tree_current -1
        to-feeding-tree
      ]
    ][
      to-feeding-tree
    ]
  ]

end


;----------------------------------------

to-report on-feeding-tree?

  if travel_mode = "short_distance" [   ;; short distance frugivory
    ifelse action = "travel" OR action = "foraging" AND tree_target != -1 [
;      print distance tree_target ; for debugging
      ifelse distance tree_target < 0.8 [
        set tree_current tree_target
        set tree_target -1
;        print "on-feeding-tree? TRUE" ; for debugging
        report true
      ][
        report false
      ]
    ][
;      print "Action != travel OR tree_target = -1" ; for debugging
      ifelse action = "feeding" [
        report true
      ][
        report false
      ]
    ]
  ]


  if travel_mode = "long_distance" [    ;; long distance frugivory

    ifelse action = "travel" OR action = "foraging" AND ld_tree_target != -1 [
      ifelse distance ld_tree_target < 0.8 [
        set tree_current ld_tree_target
        set ld_tree_target -1
        set tree_target ld_tree_target ;; IMPORTANT FOR NOT HAAVING TO CHANGE ALL THE FEEDING PROCESS
        report true
      ][
        report false
      ]
    ][
      ifelse action = "feeding" [
        report true
      ][
        report false
      ]
    ]
  ]
end

;----------------------------------------

to feeding
;  print "feeding"    ; debugging
  set action "feeding"
  set behavior "frugivory"

  set behaviorsequence lput 1 behaviorsequence ;; activity budget

  set energy energy + energy-from-fruits + energy_species
  set action-time action-time + 1

  ; change mem list
  if( length tree_ate_list = 0 ) [
    ;; THIS REMOVES THE TREE THAT THE TAMARINS JUST ATE FROM THE POT LIST, THUS IT SHOULD STAY IN THE FEEDING COMAMND
    set tree_pot_list remove-item ( position [who] of tree_current tree_pot_list ) tree_pot_list
    set tree_ate_list lput [who] of tree_current tree_ate_list
    set tree_mem_list lput 0 tree_mem_list
    set tree_add_list lput 1 tree_add_list
    ; remove_trees_surrounding
  ]

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
  set seed_mem_list lput 1 seed_mem_list
  set seed_add_list lput 1 seed_add_list
end

;-----------------------------------------
to remove_trees_surrounding
  ;; HERE REMOVE ADDITIONAL TREES within the visual range of the tamarins (1 patch = 25m)
  ;; set tree_pot_list remove-item ( position [who] of
  ;; let trees_remove [ who ] of feeding-trees in-radius 2
  ;;  let trees_remove filter [ s -> member? s [ who ] of feeding-trees in-radius visual ] tree_pot_list
  let trees_remove filter [ s -> member? s [ who ] of feeding-trees with [ distance myself < visual] ] tree_pot_list
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
  let n_trees round ( count feeding-trees  / prop_trees_to_reset_memory ); don't know what should be the number exactly. The smaller it is, more the tamarins will travel around to find the only available trees in the pot_list ;
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

  if travel_mode = "short_distance" [
    if tree_target = -1 [
      set action-time 0
      search-feeding-tree
    ]

    set heading towards tree_target

    ifelse ( action = "travel" AND random-float 1 < p-foraging-while-traveling ) [
      forage
      ;    set action "travel" ;; KEEP THIS HERE OTHERWISE TAMARINS WILL KEEP FORAGING UP TO WHEN ENERGY < LVL 1
    ][
      ;    travel
      set action "travel"
      set behavior "travel"

      ;; RANDOM movement while traveling:
      if random-angle? = TRUE AND distance tree_target > 1.5 [
        rt ( random max-random-angle ) - ( max-random-angle / 2 )
      ]
    ]
  ]


  if travel_mode = "long_distance" [
    set action-time 0
    if ld_tree_target = -1 [
      search-feeding-tree
    ]

    set heading towards ld_tree_target

    ifelse ( action = "travel" AND random-float 1 < p-foraging-while-traveling ) [
      forage
      ;    set action "travel" ;; KEEP THIS HERE OTHERWISE TAMARINS WILL KEEP FORAGING UP TO WHEN ENERGY < LVL 1
    ][
      ;    travel
      set action "travel"
      set behavior "travel"

      ;; RANDOM movement while traveling:
      if random-angle? = TRUE AND distance ld_tree_target > 1.5 [
        rt ( random max-random-angle ) - ( max-random-angle / 2 )
      ]
    ]
  ]


  ; ========================================= ;
  ;; this procedure independs of travel mode:
  set behaviorsequence lput 3 behaviorsequence

  forward travel_speed
  set dist-traveled travel_speed
  set steps-moved steps-moved + 1
  set energy energy + energy-loss-traveling

end

;----------------------------------------

to search-feeding-tree


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
    set ld_tree_target one-of feeding-trees with [member? who let_pot_list] ;; RANDOM TREE
    set tree_target ld_tree_target ; VERY IMPORTANT FOR NOT HAVING TO CHANGE ALL THE FEEDING PROCEDURE
    ask tree_target [ set color blue ]    ; make long distance target blue

    set tree_target_species [ species ] of ld_tree_target
;    print ld_tree_target    ; debugging
  ]


  if phenology-on? [
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
  ]
end

to travel
;  forward travel_speed
;  set dist-traveled travel_speed
;  set behavior "travel"
;  set steps-moved steps-moved + 1
;  set energy energy + energy-loss-traveling
end

;---------------------------------------------------------------------------------------------
; Defecation commands
;---------------------------------------------------------------------------------------------
to defecation
  if timestep < 84 [ ; 84 is for 7 hours after waking up (after 3pm)
                     ; testing if the monkey defecates the seeds AND put the seeds to the seeds' agent list
    if member? gut_transit_time seed_mem_list [
      let loc_index position gut_transit_time seed_mem_list
      let loc_who item loc_index seed_ate_list
      set seed_ate_list remove-item 0 seed_ate_list
      set seed_add_list remove-item 0 seed_add_list
      set seed_mem_list remove gut_transit_time seed_mem_list
      hatch-seeds n_seeds_hatched [ ; change to hatch more seeds! <<<
        setxy xcor ycor
        set mother-tree [id-tree] of feeding-trees with [ who = loc_who ]
        set species [species] of feeding-trees with [ who = loc_who ]
        set id-seed who
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
;  print "resting"   ; debugging
  set action "resting"
  set behavior "resting"

  set tree_current -1

  set behaviorsequence lput 4 behaviorsequence

  set steps-moved steps-moved + 1
  set energy energy + energy-loss-traveling

  if display-hatched-trees? = TRUE [
    hatch-resting-trees 1 [
      set size 1
      set shape "tree"
      set color 77
      set label ""
      setxy xcor ycor
      set species "rest"
      set id-tree who
    ]
  ]

end

;---------------------------------------------------------------------------------------------
; Sleeping commands
;---------------------------------------------------------------------------------------------
to sleeping

  ifelse tree_target = -1 [

    if sleeping-trees-scenario = "empirical" [ search-sleeping-defined ]  ; when using field trees ; WITH THIS IT DOES NOT           ;; EMPIRICAL
;    if sleeping-trees-scenario = "simulated" [ search-sleeping-tree ]     ; when simulating trees  ; ONLY WORKS WITH THIS PROCEDURE ;; SIMULATED (procedure droped on July 20th 2022)
  ][

    set heading towards tree_target
    if distance tree_target < travel_speed * 0.8 [
      set tree_current tree_target
      set tree_target -1
      set action "sleeping"
      set behavior "sleeping"
      set action-time 0
;       trees in the tree_ate_list[] had to get back to the tree_pot_list[]
;       they forget about the trees they visited last day
;      while [length tree_ate_list > 0] [
;        set tree_pot_list lput first tree_ate_list tree_pot_list
;        set tree_ate_list remove (first tree_ate_list) tree_ate_list
;      ]
;      set tree_mem_list [] ;; COMMENT OUT THIS BECAUSE THE TAMARINS DON'T FORGET
;      set tree_add_list [] ;; COMMENT OUT THIS BECAUSE THE TAMARINS DON'T FORGET
    ]
  ]

  if action != "sleeping" [

    ;; RANDOM movement while traveling:
    if random-angle? = TRUE  AND distance tree_target > 1.5 [
      rt ( random max-random-angle ) - ( max-random-angle / 3 ) ; tamarins show more directed behavior when heading to sleeping sites, so here we divide by 3
    ]

    forward travel_speed
    set dist-traveled travel_speed
    set steps-moved steps-moved + 1
    set energy energy + energy-loss-traveling
    set action "travel"
    set behavior "travel"

    set behaviorsequence lput 3 behaviorsequence

  ]

;  if simulation-time-end = TRUE [
;    output-print "AHOY"
;  ]

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
  set color magenta
  set action "forage"
  set behavior "foraging"

  set behaviorsequence lput 2 behaviorsequence


;; RANDOM movement while foraging 1:
;  let n random 100
;  if n <= 5 [ left random 180 ]
;  if n > 5 AND n <= 20 [ right random 60 ]
;  if n > 20 AND n <= 60 [ rt random 30 ]
;  if n > 60 [ lt random 30 ]

;; RANDOM movement while foraging 2 (much simpler):
  if random-angle? = TRUE [
  rt ( random max-random-angle ) - ( max-random-angle / 2 )
  ]

;; movement is already done by the 'to-feeding-tree' procedure, so it has to be commented out from here
;  forward travel_speed
;  set dist-traveled travel_speed
;  set steps-moved steps-moved + 1
  set energy energy + energy-from-prey + energy-loss-foraging

  set color gray

end

;-------------------------------------------------------------
to random-action

;  print "random-action"    ; debugging

  set action-time 0
  ifelse random-float 1 > p-foraging-while-traveling [
;    set color black
;    ask patch-here [ set pcolor orange ]
    frugivory
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

  if action = "forage"   [ forage ]
  if action = "resting" [ resting ]
  if action = "travel" [ frugivory ]

end

;-------------------------------------------------------------
to change-bonus

;  print "change-bonus"   ; debugging
  set action-time 0

  let choice random 2
  if choice = 0 [
    random-action
  ]
  if choice = 1 [
    last-action-again
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
; end of commands ================================================
;-----------------------------------------------------------------


;; REPORTERS

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
    set energy 120
    set travel_mode "long_distance"
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
10
10
528
529
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-25
25
-25
25
0
0
1
ticks
30.0

SLIDER
560
207
732
240
start-energy
start-energy
energy_level_1
170
124.0
1
1
NIL
HORIZONTAL

BUTTON
533
12
607
54
SETUP
setup
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
47
54
197
72
Feeding tree
12
0.0
1

TEXTBOX
48
78
198
96
Resting tree
12
0.0
1

TEXTBOX
47
106
197
124
Sleeping tree
12
0.0
1

TEXTBOX
48
132
198
150
Defecated seeds
12
0.0
1

SWITCH
1001
236
1119
269
show-energy?
show-energy?
0
1
-1000

SWITCH
1001
278
1121
311
show-path?
show-path?
0
1
-1000

SLIDER
560
241
732
274
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
567
308
739
341
energy-from-fruits
energy-from-fruits
0
15
10.0
1
1
NIL
HORIZONTAL

BUTTON
533
140
600
173
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
533
59
607
92
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
612
49
695
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
612
12
696
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
699
12
761
82
no_days
10.0
1
0
Number

SLIDER
567
346
739
379
energy-from-prey
energy-from-prey
0
15
1.8
0.1
1
NIL
HORIZONTAL

SLIDER
567
383
740
416
energy-loss-traveling
energy-loss-traveling
-10
0
-1.8
0.1
1
NIL
HORIZONTAL

SLIDER
567
421
740
454
energy-loss-foraging
energy-loss-foraging
-10
0
-1.7
0.1
1
NIL
HORIZONTAL

SLIDER
567
457
740
490
energy-loss-resting
energy-loss-resting
-10
0
-1.8
0.1
1
NIL
HORIZONTAL

MONITOR
773
13
849
58
timestep
timestep
17
1
11

OUTPUT
759
199
983
312
11

TEXTBOX
51
30
201
48
Tamarin
12
0.0
1

INPUTBOX
9
641
416
727
runtime
./runtime/
1
0
String

CHOOSER
1042
34
1192
79
feeding-trees-scenario
feeding-trees-scenario
"trees_all" "trees_may" "trees_jun" "trees_jul" "trees_aug"
3

CHOOSER
891
55
1037
100
sleeping-trees-scenario
sleeping-trees-scenario
"empirical" "simulated"
0

SWITCH
1000
194
1120
227
export-png
export-png
1
1
-1000

SLIDER
770
373
932
406
step_forget
step_forget
0
1000
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
585
283
721
319
2. energy related
14
0.0
1

TEXTBOX
784
325
921
359
3. memory related
14
0.0
1

SLIDER
772
561
923
594
gut_transit_time_val
gut_transit_time_val
0
100
13.0
1
1
NIL
HORIZONTAL

TEXTBOX
951
326
1108
360
4. movement related
14
0.0
1

SLIDER
955
447
1097
480
travel_speed_val
travel_speed_val
0
1
0.8
0.1
1
NIL
HORIZONTAL

TEXTBOX
967
591
1107
627
6. phenology related
14
0.0
1

SLIDER
586
498
722
531
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
586
533
722
566
energy_level_2
energy_level_2
energy_level_1
1000
143.0
1
1
NIL
HORIZONTAL

TEXTBOX
784
541
911
577
5. dispersal related
14
0.0
1

SLIDER
773
600
922
633
n_seeds_hatched
n_seeds_hatched
0
100
7.0
1
1
NIL
HORIZONTAL

CHOOSER
891
100
1037
145
empirical-trees-choice
empirical-trees-choice
"closest" "random"
0

MONITOR
791
57
848
102
day
day
17
1
11

SWITCH
1048
111
1167
144
sleeping-trees?
sleeping-trees?
0
1
-1000

SWITCH
1048
143
1167
176
resting-trees?
resting-trees?
1
1
-1000

SWITCH
1048
79
1167
112
feeding-trees?
feeding-trees?
0
1
-1000

TEXTBOX
855
14
1032
48
1. Resources scenario
14
0.0
1

SWITCH
902
146
1028
179
all-slp-trees?
all-slp-trees?
1
1
-1000

TEXTBOX
806
154
911
182
make all trees from study period available:
9
0.0
1

BUTTON
693
141
769
174
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
605
101
707
134
print-step?
print-step?
0
1
-1000

PLOT
1134
444
1357
638
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
857
37
1054
65
1.2 Choose sleeping site scenario
11
0.0
1

TEXTBOX
1017
16
1228
44
1.1 Choose resource trees scenario
11
0.0
1

BUTTON
608
140
685
173
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
956
521
1101
554
duration
duration
0
20
1.0
1
1
NIL
HORIZONTAL

MONITOR
792
102
850
147
Energy
[ round energy ] of monkeys
3
1
11

SLIDER
770
422
934
455
visual
visual
0
10
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
763
348
930
373
How many timesteps BLTs take to forget a tree:
10
0.0
1

TEXTBOX
765
407
932
427
exclude trees in radii from pot list:
10
0.0
1

SWITCH
1127
196
1289
229
display-hatched-trees?
display-hatched-trees?
1
1
-1000

SWITCH
1127
237
1292
270
path-color-by-day?
path-color-by-day?
1
1
-1000

TEXTBOX
1224
14
1374
32
Output related
14
0.0
1

SWITCH
1216
87
1334
120
output-files?
output-files?
1
1
-1000

CHOOSER
1205
36
1343
81
USER
USER
"Ronald" "Eduardo" "Others"
1

SWITCH
1217
122
1335
155
output-print?
output-print?
1
1
-1000

SLIDER
956
483
1100
516
p-foraging-while-traveling
p-foraging-while-traveling
0
1
0.3
0.05
1
NIL
HORIZONTAL

TEXTBOX
966
553
1116
575
max timesteps repeating same behavior
9
0.0
1

PLOT
1362
376
1535
500
action-time
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
"default" 1.0 0 -16777216 true "" "ask monkeys [ plot action-time ]"

PLOT
1356
10
1600
200
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
1409
182
1452
202
frugivory
8
0.0
1

TEXTBOX
1450
182
1488
203
foraging
8
0.0
1

TEXTBOX
1490
182
1517
202
travel
8
0.0
1

TEXTBOX
1519
181
1552
201
resting
8
0.0
1

PLOT
1362
505
1542
631
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

SWITCH
954
346
1096
379
random-angle?
random-angle?
0
1
-1000

TEXTBOX
956
379
1106
405
add random variation in direction each step. If so:
9
0.0
1

BUTTON
278
574
411
608
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
1144
282
1344
432
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
963
610
1101
643
phenology-on?
phenology-on?
0
1
-1000

BUTTON
10
535
129
568
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
136
537
265
570
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
269
537
432
570
NIL
inspect one-of monkeys
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
10
573
272
606
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
770
465
934
498
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
771
500
921
518
don't choose 1
9
0.0
1

SLIDER
955
403
1096
436
max-random-angle
max-random-angle
0
180
90.0
1
1
NIL
HORIZONTAL

TEXTBOX
962
644
1112
666
energy and time spent feeding for each tree species
9
0.0
1

PLOT
497
577
697
727
species_time
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
"default" 1.0 0 -16777216 true "" "ask monkeys [ ifelse tree_current != -1 [ plot [ species_time ] of tree_current ] [ plot 0 ] ]"

BUTTON
10
607
144
641
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
1357
206
1602
365
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
"default" 1.0 2 -16777216 true ";if [behavior] of monkeys = \"sleeping\" [ plot-pen-down ]\nask monkeys [ if behavior = \"sleeping\" [ plot-pen-down ] ]" ";ask monkeys [ plot DPL * patch-size-m ]"
"pen-1" 1.0 2 -2674135 true "" "ask monkeys [ if action = \"sleeping\" [ plot DPL * scale ] ]"
"pen-2" 1.0 0 -7500403 true "" ";ask monkeys [ if behavior = \"sleeping\" [ plot mean DPL_d ] ]\n;ask monkeys [ plot mean DPL_d ]"

INPUTBOX
711
82
771
142
scale
28.28803
1
0
Number

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
    <metric>[ x_UTM ] of monkeys</metric>
    <metric>[ y_UTM ] of monkeys</metric>
    <metric>[ dist-traveled ] of monkeys</metric>
    <metric>[ travel_mode ] of monkeys</metric>
    <metric>[ energy ] of monkeys</metric>
    <metric>[ behavior ] of monkeys</metric>
    <metric>[ x_UTM ] of seeds</metric>
    <metric>[ y_UTM ] of seeds</metric>
    <metric>[ mother_tree ] of seeds</metric>
    <enumeratedValueSet variable="step_forget">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-foraging-while-traveling" first="0.3" step="0.1" last="1"/>
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
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-Analysis-Part1" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[ x_UTM ] of monkeys</metric>
    <metric>[ y_UTM ] of monkeys</metric>
    <metric>[ dist-traveled ] of monkeys</metric>
    <metric>[ travel_mode ] of monkeys</metric>
    <metric>[ energy ] of monkeys</metric>
    <metric>[ behavior ] of monkeys</metric>
    <metric>[ x_UTM ] of seeds</metric>
    <metric>[ y_UTM ] of seeds</metric>
    <metric>[ mother_tree ] of seeds</metric>
    <enumeratedValueSet variable="step_forget">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-foraging-while-traveling" first="0.3" step="0.1" last="1"/>
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
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-Analysis-Part2" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[ x_UTM ] of monkeys</metric>
    <metric>[ y_UTM ] of monkeys</metric>
    <metric>[ dist-traveled ] of monkeys</metric>
    <metric>[ travel_mode ] of monkeys</metric>
    <metric>[ energy ] of monkeys</metric>
    <metric>[ behavior ] of monkeys</metric>
    <metric>[ x_UTM ] of seeds</metric>
    <metric>[ y_UTM ] of seeds</metric>
    <metric>[ mother_tree ] of seeds</metric>
    <enumeratedValueSet variable="step_forget">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-foraging-while-traveling" first="0.3" step="0.1" last="1"/>
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
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-Analysis-Part3" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[ x_UTM ] of monkeys</metric>
    <metric>[ y_UTM ] of monkeys</metric>
    <metric>[ dist-traveled ] of monkeys</metric>
    <metric>[ travel_mode ] of monkeys</metric>
    <metric>[ energy ] of monkeys</metric>
    <metric>[ behavior ] of monkeys</metric>
    <metric>[ x_UTM ] of seeds</metric>
    <metric>[ y_UTM ] of seeds</metric>
    <metric>[ mother_tree ] of seeds</metric>
    <enumeratedValueSet variable="step_forget">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-foraging-while-traveling" first="0.3" step="0.1" last="1"/>
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
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-Analysis-Part4" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[ x_UTM ] of monkeys</metric>
    <metric>[ y_UTM ] of monkeys</metric>
    <metric>[ dist-traveled ] of monkeys</metric>
    <metric>[ travel_mode ] of monkeys</metric>
    <metric>[ energy ] of monkeys</metric>
    <metric>[ behavior ] of monkeys</metric>
    <metric>[ x_UTM ] of seeds</metric>
    <metric>[ y_UTM ] of seeds</metric>
    <metric>[ mother_tree ] of seeds</metric>
    <enumeratedValueSet variable="step_forget">
      <value value="30"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-foraging-while-traveling" first="0.3" step="0.1" last="1"/>
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
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-Analysis-Part5" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>[ x_UTM ] of monkeys</metric>
    <metric>[ y_UTM ] of monkeys</metric>
    <metric>[ dist-traveled ] of monkeys</metric>
    <metric>[ travel_mode ] of monkeys</metric>
    <metric>[ energy ] of monkeys</metric>
    <metric>[ behavior ] of monkeys</metric>
    <metric>[ x_UTM ] of seeds</metric>
    <metric>[ y_UTM ] of seeds</metric>
    <metric>[ mother_tree ] of seeds</metric>
    <enumeratedValueSet variable="step_forget">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-foraging-while-traveling" first="0.3" step="0.1" last="1"/>
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
