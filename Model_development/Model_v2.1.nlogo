; ==== Black lion tamarin model ============
; Eduardo Zanette & Ronald Bialozyt
; ------------------------------------------------

extensions [ gis ]

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

  ld_patch_target ; long distance patch target -> MAIN IMPLEMENTATION FOR THE ECO SIM MOD COURSE ====================================================
  start_patch     ; patch where tamarins have started. Serves for animals to stick to a home range (based on https://stackoverflow.com/questions/43507568/constraining-movement-of-agents-to-a-home-range-in-netlogo)
  homerange

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

patches-own [
  habitat
  visitors ;list of monkeys that have been visiting it
]

;; GLOBALS ;;
globals [
  behaviorsequence

  timestep ; step counter during one day
  day  ; present day in the simulation
  meanxcoord ; translating the geo coordinates to world coordinates
  meanycoord ; translating the geo coordinates to world coordinates
  ;step_forget ; amount of timesteps until the tamarin forgets to be in that tree
  midday ; the time of the middle of the day (important for resting)

  ;; INPUT ;;
  ;gis;
  bb-gis      ; raster (.asc) file for defining patch size (10 x 10 m)
  bb-gis-shp  ; shapefile for drawing the fragment and defining habitat/non-habitat patches

  ;; patch sets:
  forest_set
  matrix_set
  border_patches

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
  species_time ; how long the tamarin feeds on the tree species
  energy_species ; value of energy they get from feeding of each species

  ;; OUTPUT ;;
  local-path  ; path for the model to run in different CPUs

]




;--------------------------------------------------------------------------------
; SETTING UP
;--------------------------------------------------------------------------------
to setup
  clear-all

  if USER = "Ronald"
  [ set local-path "/home/rbialozyt/BLT_IBM-Model/" ]
  if USER = "Eduardo"
  [ set local-path "D:/Data/Documentos/Study/Mestrado/FAPESP/BEPE/Course_Simulation-Modelling/Exam" ]
  if USER = "Others"
  [ set local-path "~/" ]


  setup-patches
  setup-gis
  setup-trees
  setup-monkeys

  create-legend

  set day 1
  set midday 58
  set timestep 0
;  set gut_transit_time gut_transit_time_val
;  set travel_speed travel_speed_val


  reset-ticks
end

; PATCHES
to setup-patches
  ask patches [
    set visitors []
    ;set pcolor yellow + 4

  ]
end

; GIS
to setup-gis ; (= v1.1 Model)
  set-patch-size 3

  ; load .prj and .asc (raster 10 x 10 m)
    gis:load-coordinate-system "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei-poligono.prj" ; WGS_1984_UTM_Zone_22S
    set bb-gis gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei-poligono2_reproj.asc" ; fragment/study area raster (reprojected***)

    ; load the poligon (.shp) to determine forest and matrix patches
  set bb-gis-shp gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp" ; fragment/study area polygon


  ; make each raster cell = patch in NetLogo
  let width floor (gis:width-of bb-gis / 2)
  let height floor (gis:height-of bb-gis / 2)
  resize-world (-1 * width ) width (-1 * height ) height

  gis:set-world-envelope gis:envelope-of bb-gis
  gis:apply-raster bb-gis habitat

  gis:set-drawing-color black
  gis:draw bb-gis-shp 1

  ; to use make-trees-change-position procedure ( = BLT_model_v1_UTM.nlogo)
;  set guarei-dataset gis:load-dataset "D:/Data/Documentos/github/BLT_IBM-Model/Model_development/Model-cleaning/gis-extension/Guarei-poligono.shp"
  set guarei-dataset gis:load-dataset "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp"

  set shape-type gis:shape-type-of guarei-dataset
  set property-names gis:property-names guarei-dataset
  set feature-list gis:feature-list-of guarei-dataset
  set vertex-lists gis:vertex-lists-of item 0 feature-list

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

; TREES INPUT
to setup-trees

;; SLEEPING TREES ONLY (ALL MONTHS)
;  let id-tree-slp 0

  create-feeding-trees feedingtree_n [
    set size 2
    set shape "tree"
    set color green
    setxy random-xcor random-ycor
    set x_UTM (item 0 gis:envelope-of self)
    set y_UTM (item 2 gis:envelope-of self)
  ];

  create-sleeping-trees sleepsite_n [
    set size 2.5
    set shape "circle"
    set color white
    setxy random-xcor random-ycor
    set x_UTM (item 0 gis:envelope-of self)
    set y_UTM (item 2 gis:envelope-of self)
  ];

  make-trees-change-position

end


; TAMARINS
to setup-monkeys

  create-monkeys blt_n
  ask monkeys [

    set behaviorsequence []
    set size 2

    setxy random xcor random ycor
    set tree_current -1

    let start one-of sleeping-trees with [ not (any? other monkeys-here) ]
    setxy [xcor] of start [ycor] of start
    set tree_current start


;    set x_UTM (item 0 gis:envelope-of self)
;    set y_UTM (item 2 gis:envelope-of self)

    set travel_mode "short_distance"
    set tree_target -1
    set ld_tree_target -1
    set ld_patch_target -1
    set start_patch []
    set start_patch lput patch-here start_patch
    set homerange 0


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
      set color random color
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


to make-trees-change-position

  ask feeding-trees [
    let chosen random (length feature-list)
    let my-feature (item chosen feature-list)
;    let my-feature (item chosen guarei-dataset)
;
    ; translate the centroid of the chosen feature from
    ; GIS space to NetLogo patch space, then move there
    let new-location gis:location-of (gis:random-point-inside my-feature)
;    print new-location
    let new-x (item 0 new-location)
    let new-y (item 1 new-location)
    setxy new-x new-y
  ]

    ask sleeping-trees [
    let chosen random (length feature-list)
    let my-feature (item chosen feature-list)
;
    ; translate the centroid of the chosen feature from
    ; GIS space to NetLogo patch space, then move there
    let new-location gis:location-of (gis:random-point-inside my-feature)
;    print new-location
    let new-x (item 0 new-location)
    let new-y (item 1 new-location)
    setxy new-x new-y
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

  set timestep timestep + 1
  tick

  if not any? monkeys [ stop ]


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

  ;; DEBUGGING
  if print-step? = TRUE [
    type "---- STEP ---- " print timestep
    ask monkeys [
      type "---- WHO: " type who print " ----"
      type "MODE: " print travel_mode
      type "tree_target: " type tree_target type " "
      type "ld_tree_target: " type ld_tree_target type " "
      type "tree_current: " type tree_current type " "
      type "behavior: " type behavior type " "
      type "action: " print action
      type "tree_pot_list: " print length tree_pot_list print tree_pot_list
      type "tree_ate_list: " print length tree_ate_list print tree_ate_list
      type "tree_mem_list: " print length tree_mem_list print tree_mem_list
      type "tree_add_list " print length tree_add_list print tree_add_list
      type "action-time: " print action-time
      type "energy: " print energy
      type "x: " print x_UTM
      type "y: " print y_UTM
      if tree_target != -1 [
        type "distance: " print distance tree_target
        type "target_species: " print tree_target_species
      ]
;      type "DPL_d: " print DPL_d
      type "DPL (m): " print DPL
      ifelse travel_mode = "short_distance" AND tree_target != -1 [
         print distance tree_target
      ][
        if ld_tree_target != -1 [print distance ld_tree_target]
      ]
      type "ld_patch_target: " type ld_patch_target print "" print ""
    ]
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




end



;-------------------------------------------------------------------------------

to move-monkeys

  ask monkeys
  [
    set dist-traveled 0

    ; set home range color
    if color-home-range? = TRUE [
      ask patch-here [ set pcolor [color] of myself ]
;      ask neighbors [ set pcolor [color] of myself ]
    ]

    register-visitors

    ifelse show-energy? [
      set label round energy
      set label-color black
    ]
    [ set label "" ]

    if patch-ahead pcolor = yellow + 4 [ right 180 forward ( travel_speed / 3 ) ] ; no use of the matrix
    if energy < 1 [ die ]

    set x_UTM (item 0 gis:envelope-of self)
    set y_UTM (item 2 gis:envelope-of self)


  ;; BLT ROUTINE

    if timestep = 1
        [ set tree_current -1
          set DPL 0 ; set daily path length to 0 every day
          remove_trees_surrounding ; to avoid feeding in the closes tree
          morning-defecation ]              ;; MORNING-DEFECATION PROCEDURE

    if timestep = simulation-time [ set tree_target -1] ; force monkey select a sleeping site
    if timestep > simulation-time [
      sleeping
    ]


    if timestep < simulation-time [ ; energy levels: energy_level_1 = 80 and energy_level_2 = 150
      enhance_memory_list

      ifelse energy < energy_level_1 [ ; energy < level 1
        set travel_mode "short_distance"
        if ld_tree_target = tree_target [
          set tree_target -1 ; remove tree_target when coming from "long_distance"
          set ld_tree_target -1  ; if this is not here it will make the tamarin lose the target very close to the tree when coming from long distance bc of the condition ld_tree_target = tree_target (Ronald debugged on the 14th of July 2022)
          set ld_patch_target -1

        ]
        frugivory
      ][
;        ifelse (action = "feeding" or action = "travel") [              ;; v1.0 version
        ifelse (travel_mode = "short_distance" ) [                       ;; v1.1 version (long and short-distance travel)
          ifelse energy > energy_level_2 [ ; energy > level 2 ==> other activities
            if tree_current = -1 AND ld_patch_target = -1 [
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

      avoid-territory
      update-homerange
      recolor-ld-patches

    ] ; end of daily routine
] ; end ask monkeys
end

to register-visitors

  if (not (member? [who] of self visitors = TRUE)) [
    ask patch-here [
      ; put the who in the visitor
      set visitors lput [who] of myself visitors
    ]
  ]
end

to update-homerange
  ; home range calculation
  set homerange count patches with [ (member? [who] of myself visitors = TRUE) ]

  ;stablish new sleeping sites as start-patch
;  set start-patch
end

to avoid-territory
  ;; based on https://stackoverflow.com/questions/43507568/constraining-movement-of-agents-to-a-home-range-in-netlogo
  ifelse distance one-of start_patch > (max-pxcor / blt_n) [
    face one-of start_patch
    search-feeding-tree
  ][

  ]

  ;; based on https://stackoverflow.com/questions/24786908/get-mean-heading-of-neighboring-turtles
;  let neighbor-monkeys other monkeys in-radius 5 ; (5 is more than the estimated long-call hearing distance. obs 1 patch = 28.3 m)
;    print neighbor-monkeys
;  if any? neighbor-monkeys [
;    set heading mean [heading] of neighbor-monkeys
;    set heading 180
;    rt 180
;    search-feeding-tree
;  ]

;  let neighbor-patches patches with [pcolor != [color] of myself AND pcolor != 68] print neighbor-patches
;  if patch-ahead 1 member? [who] of neighbor-patches [
;    fd 90
;    search-feeding-tree
;
;  ]

end

to recolor-ld-patches
  ; make ld_patch color green again
  ask patches with [pcolor = blue] [ set pcolor lime + 3 ]
end

;--------------------------------------------------------------------------------
; the whole loop for frugivory
;--------------------------------------------------------------------------------
to frugivory

;  print "frugivory"  ; debugging

  if travel_mode = "short_distance" [   ;; short distance frugivory
    set travelmodelist lput 1 travelmodelist ; to the travel mode histogram
    ifelse on-feeding-tree? [
      ifelse random (2 * species_time ) > action-time [
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
  ; print "on-feeding-tree?"

  if travel_mode = "short_distance" [   ;; short distance frugivory
    ifelse action = "travel" OR action = "foraging" AND tree_target != -1 [
    ;  print distance tree_target ; for debugging
      ifelse distance tree_target < travel_speed * 0.8 [

        set tree_current tree_target

        set x_UTM [ x_UTM ] of tree_current
        set y_UTM [ y_UTM ] of tree_current
;        set xcor [ xcor] of tree_current
;        set ycor [ ycor ] of tree_current

        set tree_target -1
        set ld_patch_target -1

        set species_time duration ;; duration = 2 is the most common value over all species, but as there's a random variation on the 'random (2 * species_time), I'll leave it as the same as duration
  ;      print "on-feeding-tree? TRUE" ; for debugging
  ;      type "tree_current: " print tree_current
  ;      type "tree_target: " print tree_target
        report true

      ][
  ;      print "on-feeding-tree? FALSE" ; for debugging
  ;      print tree_target
        report false
      ]
    ][
  ;    print "Action != travel OR tree_target = -1" ; for debugging
      ifelse action = "feeding" [
        report true
      ][
        report false
      ]
    ]
  ]


  if travel_mode = "long_distance" [    ;; long distance frugivory
    let let_pot_list tree_pot_list

    ifelse action = "travel" OR action = "foraging" AND ld_patch_target != -1 [

      ifelse ( distance ld_patch_target < 0.8 ) [
        ;      let pot_trees feeding-trees with [member? who let_pot_list]
;        print "I'm close!!!"


        ifelse not any? feeding-trees-on patch-ahead 1 [
;          print "no trees, search neighbors!"

          ifelse not any? feeding-trees-on neighbors [  ; OR let a sum [ count turtles-here ] of neighbors ; if a = 0 [ print "no trees, search new patch" search-feeding-tree ]
;            print "no trees, search new patch"
            search-feeding-tree
          ][
            ;; SELECT TREE TARGET WITHIN THE ld_patch_target
            let neighbortrees feeding-trees-on neighbors
            set neighbortrees feeding-trees with [member? who let_pot_list]

            set ld_tree_target min-one-of feeding-trees with [member? who let_pot_list] [distance myself] ;; CLOSEST TREE
            ask ld_tree_target [ set color blue ]    ; make ld_tree_target blue
            set tree_target_species [ species ] of ld_tree_target
          ]

        ][
          set ld_tree_target min-one-of feeding-trees with [member? who let_pot_list] [distance myself] ;; CLOSEST TREE
          ask ld_tree_target [ set color blue ]    ; make ld_tree_target blue
          set tree_target_species [ species ] of ld_tree_target
        ]

        ;ask [neighbors] of ld_patch_target [ set pcolor blue ]
        ;          ask ld_patch_target [ set pcolor blue ]    ; make ld tree target blue
        ;ask patch-at ([pxcor] of ld_patch_target) ([pycor] of ld_patch_target) [set pcolor pink]
        ;ask feeding-trees-on ld_patch_target [ set color pink ]
        ;let trees_in_patch turtles-here
        ;    print tree_target   ; debugging

        set tree_current ld_tree_target
        set ld_tree_target -1
        ask ld_patch_target [ set pcolor lime + 3 ]
        set ld_patch_target -1



       set species_time duration

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
;     remove_trees_surrounding
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
  if tree_current != -1 [
    set seed_ate_list lput [who] of tree_current seed_ate_list
    set seed_mem_list lput 1 seed_mem_list
    set seed_add_list lput 1 seed_add_list
  ]
end

;-----------------------------------------
to remove_trees_surrounding ; (= v1.1 model on 2022-10-17d)
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
  let n_trees round ( count feeding-trees  / prop_trees_to_reset_memory ) - 2 ; don't know what should be the number exactly. The smaller it is, more the tamarins will travel around to find the only available trees in the pot_list ;
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

;    if xcor
;    if (heading towards tree_target != position tree_target) [
      set heading towards tree_target
    ;    ]
;    set heading towards patch_target

    ifelse ( action = "travel" AND random-float 1 < p-foraging-while-traveling ) [
      if tree_target != -1 AND distance tree_target > travel_speed * 0.8 [ ; otherwise it migh forage for 3 sequential steps while arriving in the feeding tree
        forage
      ]
      ;    set action "travel" ;; KEEP THIS HERE OTHERWISE TAMARINS WILL KEEP FORAGING UP TO WHEN ENERGY < LVL 1
    ][
      ;    travel
      set action "travel"
      set behavior "travel"

      ;; RANDOM movement while traveling:
;      if random-angle? = TRUE AND distance patch_target > 1.5 [
      if random-angle? = TRUE AND distance tree_target > travel_speed * 1.5 [
        rt ( random max-random-angle ) - ( max-random-angle / 2 )
      ]
    ]
  ]


  if travel_mode = "long_distance" [
    set action-time 0
    if ld_patch_target = -1 [
      search-feeding-tree
    ]

    set heading towards ld_patch_target

    ifelse ( action = "travel" AND random-float 1 < p-foraging-while-traveling ) [
      if ld_patch_target != -1 AND tree_target != -1 AND distance ld_patch_target > 0.8 [  ; otherwise it migh forage for 3 sequential steps while arriving in the feeding tree
        forage
      ]
      ;    set action "travel" ;; KEEP THIS HERE OTHERWISE TAMARINS WILL KEEP FORAGING UP TO WHEN ENERGY < LVL 1
    ][
      ;    travel
      set action "travel"
      set behavior "travel"

      ;; RANDOM movement while traveling:
      if random-angle? = TRUE AND distance ld_patch_target > 1.5 [
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

;  set color grey ; in case the tamarin foraged, it became magenta

end

;----------------------------------------
to new-patch-target
  ;; patch
  set ld_patch_target one-of patches with [habitat = "forest" AND distance myself < 5]
  ask ld_patch_target [ set pcolor blue ]    ; make long distance patch target blue
  set ld_tree_target -1
end



to search-feeding-tree

  ; make last target (short or long distance) green again
  ask feeding-trees with [color = red OR color = blue] [ set color green ]

  if travel_mode = "short_distance" [
    let let_pot_list tree_pot_list

    set tree_target min-one-of feeding-trees with [member? who let_pot_list] [distance myself] ;; CLOSEST TREE
    ask tree_target [ set color red ]    ; make close tree target red
    set tree_target_species [ species ] of tree_target
    ;    print tree_target   ; debugging
  ]

  if travel_mode = "long_distance" [
    let let_pot_list tree_pot_list

    ;; patch
    new-patch-target
    set ld_tree_target -1
  ]

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

end

;---------------------------------------------------------------------------------------------
; Sleeping commands
;---------------------------------------------------------------------------------------------
to sleeping

  if tree_target = -1 [
    search-sleeping-defined
  ]

  if action != "sleeping" [

;    let c [heading] of tree_target
;    if c = [heading] of monkeys [
    set heading towards tree_target
;    ]
    if distance tree_target < travel_speed * 0.8 [

      move-to tree_target
      set x_UTM [ x_UTM ] of tree_target
      set y_UTM [ y_UTM ] of tree_target
      set xcor [ xcor] of tree_target
      set ycor [ ycor ] of tree_target

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

  set tree_target min-one-of sleeping-trees [distance myself] ;; FOR CHOSSING THE CLOSEST SLEEPING SITE

end

;---------------------------------------------------------------------------------------------
; Commands for other activities
;---------------------------------------------------------------------------------------------
to forage ; (= v1.1 Model on 2022-10-17d)
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


to travel ; (= v1.1 model, but commented out because of avoid_matrix

  ;  avoid-patch-set ; bump on the territory borders
;  print "TRAVEL"

;  ifelse straight-line-to-target? = FALSE AND patch_avoid_matrix != nobody [
;;    print "straight line false"
;    face patch_avoid_matrix
;    forward travel_speed
;    set dist-traveled travel_speed
;    set steps-moved steps-moved + 1
;    set energy energy + ( energy-loss-traveling * travel_speed )
;;    print "travel 2"
;  ][
    forward travel_speed
    set dist-traveled travel_speed
    set steps-moved steps-moved + 1
    set energy energy + ( energy-loss-traveling * travel_speed )
;    set straight-line-to-target? TRUE
;    print "travel 3"
;  ]


end

;-------------------------------------------------------------
to random-action ; (= v1.1 Model on 2022-10-17d)

;  print "random-action"    ; debugging

  set action-time 0
  ifelse random-float 1 > 0.25 [
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
;to last-action-again ; ( = EcoSimMod Model)
;
;;  print "last-action-again"   ; debugging
;
;   if action = "forage"   [
;    forage
;    ; the following lines were excluded from the foraging procedure to not conflict with the to-feeding-tree procedure (agents were doing two steps)
;    forward travel_speed
;    set dist-traveled travel_speed
;    set steps-moved steps-moved + 1
;    set energy energy + energy-loss-traveling
;;    set color grey ; in case the tamarin has foraged, it became magenta
;  ]
;  if action = "resting" [ resting ]
;  if action = "travel" [ frugivory ]
;
;end

to last-action-again

;  print "last-action-again"   ; debugging

  if action = "forage" [
    forage
    ; the following lines were excluded from the foraging procedure to not conflict with the to-feeding-tree procedure (agents were doing two steps)

    if travel_mode = "long_distance" AND distance ld_patch_target > travel_speed * 0.8 [
      travel
    ]

    if travel_mode = "short_distance" AND distance tree_target > travel_speed * 0.8 [
      travel
;      set color grey ; in case the tamarin has foraged, it became magenta
    ]
  ]
  if action = "resting" [ resting ]
  if action = "travel" [ frugivory ]

end



;---------------------------------------------------------------------------------------------
; Extra commands
;---------------------------------------------------------------------------------------------

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
    set energy energy_level_2 + 100
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
1
1
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
533
534
705
567
start-energy
start-energy
30
170
125.0
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
852
84
970
117
show-energy?
show-energy?
0
1
-1000

SWITCH
853
118
971
151
show-path?
show-path?
0
1
-1000

SLIDER
539
196
665
229
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
537
273
709
306
energy-from-fruits
energy-from-fruits
0
15
4.0
1
1
NIL
HORIZONTAL

BUTTON
646
63
713
96
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
533
95
608
128
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

INPUTBOX
539
130
601
190
no_days
50.0
1
0
Number

SLIDER
537
310
709
343
energy-from-prey
energy-from-prey
0
15
1.7
0.1
1
NIL
HORIZONTAL

SLIDER
537
347
710
380
energy-loss-traveling
energy-loss-traveling
-10
0
-1.6
0.1
1
NIL
HORIZONTAL

SLIDER
537
385
710
418
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
537
422
710
455
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
742
14
818
59
timestep
timestep
17
1
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

SLIDER
737
304
899
337
step_forget
step_forget
0
1000
46.0
1
1
NIL
HORIZONTAL

TEXTBOX
555
247
691
283
2. energy related
14
0.0
1

TEXTBOX
752
255
889
289
3. memory related
14
0.0
1

SLIDER
905
270
1056
303
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
738
449
895
483
4. movement related
14
0.0
1

SLIDER
744
570
886
603
travel_speed
travel_speed
0
5
1.8
0.1
1
NIL
HORIZONTAL

TEXTBOX
916
348
1056
384
6. phenology related
14
0.0
1

SLIDER
555
463
691
496
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
555
497
691
530
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
917
250
1044
286
5. dispersal related
14
0.0
1

SLIDER
905
309
1054
342
n_seeds_hatched
n_seeds_hatched
0
100
1.0
1
1
NIL
HORIZONTAL

MONITOR
760
58
817
103
day
day
17
1
11

BUTTON
647
130
714
163
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
625
28
731
61
print-step?
print-step?
0
1
-1000

PLOT
1073
293
1295
483
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

BUTTON
648
97
714
130
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
745
644
890
677
duration
duration
0
20
2.0
1
1
NIL
HORIZONTAL

SLIDER
737
353
901
386
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
730
279
897
304
How many timesteps BLTs take to forget a tree:
10
0.0
1

TEXTBOX
733
338
900
358
exclude trees in radii from pot list:
10
0.0
1

TEXTBOX
838
10
1022
29
Select user (define path)
14
15.0
1

CHOOSER
845
29
983
74
USER
USER
"Ronald" "Eduardo" "Others"
1

SLIDER
745
605
889
638
p-foraging-while-traveling
p-foraging-while-traveling
0
1
0.15
0.05
1
NIL
HORIZONTAL

TEXTBOX
748
678
898
700
max timesteps repeating same behavior
9
0.0
1

PLOT
1297
293
1541
483
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
"default" 1.0 1 -16777216 true "" "clear-plot\nask monkeys [ ;if action = \"sleeping\" [\n\nforeach ( freq_map behaviorsequence ) [ x -> \nplotxy first x last x ]\n]\n;]"
"pen-1" 1.0 0 -7500403 true "" "plot 0.5"

TEXTBOX
1356
468
1399
488
frugivory
8
0.0
1

TEXTBOX
1396
468
1434
489
foraging
8
0.0
1

TEXTBOX
1436
468
1463
488
travel
8
0.0
1

TEXTBOX
1466
467
1499
487
resting
8
0.0
1

PLOT
1120
483
1295
640
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
"default" 1.0 0 -1184463 true "" "ask monkeys [ if action = \"sleeping\" [ plot energy ] ]"
"pen-1" 1.0 0 -16777216 true "" "plot energy_level_1"
"pen-2" 1.0 0 -955883 true "" "plot energy_level_2"

SWITCH
743
469
885
502
random-angle?
random-angle?
0
1
-1000

TEXTBOX
745
503
895
529
add random variation in direction each step. If so:
9
0.0
1

BUTTON
355
574
506
608
define ld patch target
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
1295
483
1465
642
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
"default" 1.0 1 -16777216 true "" "ask monkeys [ if action = \"sleeping\" [ histogram travelmodelist ] ]"

BUTTON
10
571
129
604
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
54
607
183
640
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
132
571
295
604
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

SLIDER
737
395
901
428
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
738
430
888
448
don't choose 1
9
0.0
1

SLIDER
744
525
885
558
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
909
400
1059
422
parameterize time spent feeding for each tree species
9
0.0
1

PLOT
1280
60
1513
252
Daily Path Length (m)
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
"pen-1" 1.0 2 -2674135 true "" "ask monkeys [ if action = \"sleeping\" [ plot DPL * 28.28803 ] ]"
"pen-2" 1.0 0 -7500403 true "" ";ask monkeys [ if behavior = \"sleeping\" [ plot mean DPL_d ] ]\n;ask monkeys [ plot mean DPL_d ]"

TEXTBOX
9
418
176
438
Long distance target
11
0.0
1

TEXTBOX
9
438
176
458
Short distance target
11
0.0
1

BUTTON
369
609
493
642
new-patch-target
ask monkeys [ new-patch-target ]
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
636
15
739
33
for debugging:
11
0.0
1

TEXTBOX
1207
33
1374
53
Validation patterns
14
15.0
1

TEXTBOX
1237
267
1404
287
Other graphs
14
15.0
1

SLIDER
702
169
795
202
blt_n
blt_n
1
30
1.0
1
1
NIL
HORIZONTAL

SLIDER
799
169
919
202
sleepsite_n
sleepsite_n
1
100
28.0
1
1
NIL
HORIZONTAL

BUTTON
331
649
521
683
patch-ahead other territory?
ask monkeys [let neighbor-patches patches with [pcolor != [color] of myself AND pcolor != 68]  print member? patch-ahead 1 neighbor-patches ]
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
1048
62
1274
254
Home range size
NIL
NIL
0.0
2.0
0.0
10.0
true
false
"" "ask monkeys [\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy homerange ticks\n]"
PENS
"default" 1.0 1 -16777216 true "" ";ask monkeys [plot mean homerange]\n;plot mean [homerange] of monkeys"

SWITCH
784
205
938
238
color-home-range?
color-home-range?
1
1
-1000

SLIDER
919
168
1038
201
feedingtree_n
feedingtree_n
50
1000
260.0
1
1
NIL
HORIZONTAL

TEXTBOX
170
540
364
574
DEBUGGING PROCEDURES:\n
14
15.0
1

BUTTON
326
418
512
451
NIL
make-trees-change-position
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
333
454
506
487
ask patches to list visitors
ask patches with [ length visitors >= 1 ][ print visitors ] 
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
333
489
512
522
count patches with visitors
print count patches with [ length visitors >= 1 ]
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
22
471
171
504
test forest patch size
type \"Area (ha): \" print ( count patches with [habitat = \"forest\"] * 10 * 10 / 10000 )
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

This is a spatially explicit individual-based model adapted for the black lion tamarin  (Leontopithecus chrysopygus). The study area is a continuous forest (most ideal example is Parque Estadual Morro do Diabo, in São Paulo State, Brazil)

The main question in this model version is: do the increasing number of tamarin groups in the same patch diminishes the home range size (i.e. crowding effect)? 

Further questions are related to resources: do the the number of feeding or sleeping trees change the home range size of the tamarins?


## HOW IT WORKS

Tamarins move according to its energy level. It gains energy when feeding and foraging and loses while traveling, foraging and resting. 

The main differences from the v1.1 model are:

1) A significant difference is that in this model, more tamarins are simulated, and they have a very simple mechanism of avoiding other groups' areas (avoid-territory).

2) In this version, tamarins have a long distance target based on a patch (ld_patch_target) and not a tree. Therefore, the tamarins first go for the patch and then they search for a target tree in the surroundings. 


## HOW TO USE IT

Setup the model AFTER setting your local path. Click setup and go (for running the no_days) or setup and next day (for running only one day).
After clicking 'go', check the home range size as the simulations run. Ideally, one would check the home range size after a specified number of days (no_days).


## THINGS TO NOTICE

For a better visualization of the home ranges, the chooser 'color-home-range' is on, but you can turn it off and see only the routes colored.


## THINGS TO TRY

You can test how different energy related varibles and traveling velocities affect the home range size and daily path length ("Validation patterns")


## EXTENDING THE MODEL

One aspect that addresses competition and territoriality in primates in the concept of home-range overlap (Pearce et al. 2013). This is a nice output variable to look on, because different levels of crowding probably would force tamarins to overlap more and more their home ranges.

Furthermore, the model does not include a direct avoidance of tamarins. What makes this avoidance is that they know which sleeping sites they can return and they only select long distance patches (areas of the home range) that are no further away than a portion of the world size (check 'avoid-territory' procedure). This direct avoidance could be implemented through two processes: 1) impeding tamarins to select sleeping sites in the same day as other tamarins and most importantly 2) creating a spatial avoidance based on a distance.

One thing that was done to avoid bugs was to turn down some procedures related to the memory. Most of them are related to setting targets, which is the most difficult part of the model program.


## NETLOGO FEATURES

None.


## RELATED MODELS

Bialozyt, R.; Flinkerbusch, S.; Niggemann, M.; Heymann, E.W. Predicting the seed shadows of a Neotropical tree species dispersed by primates using an agent-based model with internal decision making for movements. Ecological Modelling, v.278, p.74-84, 2014.

Ranc, Nathan, Francesca Cagnacci, and Paul R. Moorcroft. 2022. “Memory Drives the Formation of Animal Home Ranges: Evidence from a Reintroduction.” Ecology Letters, no. May 2021: 1–13. https://doi.org/10.1111/ele.13869.

Milles, Alexander, Melanie Dammhahn, and Volker Grimm. 2020. “Intraspecific Trait Variation in Personality‐related Movement Behavior Promotes Coexistence.” Oikos, June, oik.07431. https://doi.org/10.1111/oik.07431.


## CREDITS AND REFERENCES

Eduardo Miguel Zanette: Laboratory of Primatology - Department of Biodiversity (UNESP Rio Claro), Rio Claro, Brazil

Ronald Bialozyt: Nordwest German Forest Research Institute (Nordwestdeutsche Forstliche Versuchsanstalt, NW-FVA), Department of Growth and Yield, Göttingen, Germany

Mayara Mulato dos Santos: Laboratory of Primatology - Department of Biodiversity (UNESP Rio Claro), Rio Claro, Brazil

Laurence Culot: Laboratory of Primatology - Department of Biodiversity (UNESP Rio Claro), Rio Claro, Brazil

Eckhard Heymann: German Primate Center (Deutches Primatenzentrum), Behavioral Ecology and Sociobiology Unit, Göttingen, Germany

Always cite financial aids: CAPES (Masters grant); FAPESP (Masters grant) 2018/15625-0; FAPESP (JP Laurence) 014/14739-0

Data collectors: Felipe S Bufalo; Anne Sophie de Almeida e Silva; Yness Messaoudi; Gabriel P Sabino (bothanical field trips), Rodrigo Amaral and Joice de Lima (BLT field trips).


Pearce, Fiona, Chris Carbone, Guy Cowlishaw, and Nick J.B. Isaac. 2013. “Space-Use Scaling and Home Range Overlap in Primates.” Proceedings of the Royal Society B: Biological Sciences 280 (1751). https://doi.org/10.1098/rspb.2012.2122.
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
