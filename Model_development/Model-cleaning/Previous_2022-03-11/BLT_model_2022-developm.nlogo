; ==== Black lion tamarin model ============
; Eduardo Zanette & Ronald Bialozyt
; ------ -------- ------- -------- -------- ------
; Code by Mayara and Ronald:
; feeding trees available according to phenology
; simulated resting and sleeping trees
; ------------------------------------------------

extensions [ gis ] ; using the GIS extension of NetLogo

;; BREEDS ;;
; trees
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
  energy        ; energy the tamarin has left
  status        ; what is the desire
  action        ; what was the last action
  action-time   ; how long you do the same action again
  steps-moved   ; number of steps taken
  tree_target   ; memory
  tree_current  ; old_tree
  tree_ate_list ; list of trees the tamarins did eat
  tree_mem_list ; list of timesteps since the tamarin feeded on that tree
  tree_add_list ; helper list to increase the mem list
  tree_pot_list ; list of all feeding trees in homerange for that tamarin
  seed_ate_list ; list of the seeds they fed on
  seed_mem_list ; list of timesteps since the tamarin ate the seed
  seed_add_list ; helper list to increase the mem list
]

;; GLOBALS ;;
globals [
  timestep ; step counter during one day
  day  ; present day in the simulation
  scale ; to fit the data to the simulation area
  meanxcoord ; translating the geo coordinates to world coordinates
  meanycoord ; translating the geo coordinates to world coordinates
  input_forget ; amount of timesteps until the tamarin forgets to be in that tree
  midday ; the time of the middle of the day (important for resting)

  ;; INPUT ;;
  tree-file ; filename with the tree location and type
  gut_transit_time ; amount of timesteps until the tamarin defecates (time the seed takes to go throught all the digestive system)
  travel_speed ; global speed for travel
  foraging_speed ; global speed for foraging
; foraging_time ; global for how long the tamarins should spend on foraging (3 timesteps)
  species_time ; how long the tamarin feeds on the tree species
  energy_species ; value of energy they get from feeding of each species

  ;; OUTPUT ;;
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
  setup-patches
  setup-gis
  setup-trees
  setup-monkeys

  output-files

  create-legend
  set input_forget 18
  set day 1
  set midday 58
  set gut_transit_time 18
  set travel_speed 0.7
  set foraging_speed 0.5
  set species_time 3

  reset-ticks
end

; PATCHES
to setup-patches
  ask patches [set pcolor yellow + 4]
end

; GIS
to setup-gis
  set scale 32 ; scale-size. Is this being used?
  let mcp-gis gis:load-dataset "poligono_matriz.shp" ; area containing fragment and matrix
  let trees-gis gis:load-dataset "bbox_certo_buffer.shp" ; home range bounding box
  let bb-gis gis:load-dataset "polig_fragmento.shp" ; fragment/study area polygon
  let all-gis gis:load-dataset "all_trees_shape.shp" ; points shape for the all the trees

  gis:set-world-envelope (gis:envelope-of bb-gis) ; or, for a predefined domain (Banos et al 2015): gis:set-transformation gis:envelope-of name_of_the_layer [min-pxcor max-pxcor min-pycor max-pycor]
  gis:set-drawing-color lime + 3
  gis:draw mcp-gis 2
  gis:set-drawing-color yellow
  ask patches gis:intersecting bb-gis [ set pcolor lime + 3 ]
  ask patches gis:intersecting trees-gis [set pcolor lime + 3 ]
  foreach gis:feature-list-of all-gis [ vector-feature ->
    gis:set-drawing-color scale-color lime (gis:property-value vector-feature "id") 500 1
    gis:fill vector-feature 2.0 ]

  ; transform
end

; TREES
to setup-trees
  let number 0
  let xcoord 0
  let ycoord 0
  let tree-species 0
  let tree-type 0 ; phenology types
  let tree-id-tree 0

  ;; INPUT ;;
  ; load tree-file according to tree-scenario chooser
  if ( tree-scenario = "trees_all_1" )   [ set tree-file "trees_all_1.shp" ]   ; for simulated sleeping and resting trees; all trees for the 5 months without rest and sleeping sites (153 days, tree types: 1-9)
  if ( tree-scenario = "trees_all_2" )   [ set tree-file "trees_all_2.shp" ]   ; for real-data sleeping and resting trees; all trees for the 5 months (153 days, tree types: 1-9 = feeding, 10 = resting, 11 = sleeping)
  if ( tree-scenario = "trees_april_1" ) [ set tree-file "trees_april_1.shp" ] ; for April simulation (30 days - tree types: 1,7,9)
  if ( tree-scenario = "trees_may_1" )   [ set tree-file "trees_may_1.shp" ]   ; for May (31 days - tree types: 2,6,7,9)
  if ( tree-scenario = "trees_june_1" )  [ set tree-file "trees_june_1.shp" ]  ; for June (30 days - tree types: 3,6,7,8,9)
  if ( tree-scenario = "trees_july_1" )  [ set tree-file "trees_july_1.shp" ]  ; for July (31 days - tree types: 4,8,9)
  if ( tree-scenario = "trees_aug_1" )   [ set tree-file "trees_aug_1.shp" ]   ; for August (31 days - tree types: 5,8,9)

  let trees-gis gis:load-dataset tree-file ; defined by tree-scenario chooser
  foreach gis:feature-list-of trees-gis [ vector-feature ->
    let location gis:location-of (first (first (gis:vertex-lists-of vector-feature)))
    set tree-type gis:property-value vector-feature "type"
    if (tree-type  > 0 and tree-type <  10) [
      create-feeding-trees 1 [
        set size 1
        set shape "tree"
        set color green
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "month"
    ]];

    if tree-type  = 10 [
      create-resting-trees 1 [
        set size 1
        set shape "tree"
        set color magenta
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "month"
    ]] ;

    if tree-type  = 11 [
      create-sleeping-trees 1 [
        set size 1
        set shape "tree"
        set color sky - 2
        setxy item 0 location item 1 location
        set species gis:property-value vector-feature "species"
        set id-tree gis:property-value vector-feature "month"
    ]] ;
  ]
end

; LEGEND
to create-legend

   create-legend-trees 1 [ ; tamarin
      set size 1.5
      set shape "banana"
      set color yellow
      setxy min-pxcor + 1 max-pycor - 1
    ]
   create-legend-trees 1 [ ; feeding
      set size 1
      set shape "tree"
      set color green
      setxy min-pxcor + 1 max-pycor - 3
    ]
    create-legend-trees 1 [ ; resting
      set size 1
      set shape "tree"
      set color magenta
      setxy min-pxcor + 1 max-pycor - 5
    ]
    create-legend-trees 1 [ ; sleeping
      set size 1
      set shape "tree"
      set color sky - 2
      setxy min-pxcor + 1 max-pycor - 7
    ]
    create-legend-trees 1 [ ; seeds
      set size 1
      set shape "plant"
      set color 1
      setxy min-pxcor + 1 max-pycor - 9
    ]

end

; TAMARINS
to setup-monkeys

  create-monkeys 1
  ask monkeys [
    set steps-moved 0
    set action-time 0
    set shape "banana"
    set size 2
    set Color yellow

    if sleeping-trees-scenario = "empirical" [
      ; remove when simulating resting and sleeping sites:
      let start one-of sleeping-trees
      setxy [xcor] of start [ycor] of start
    ]

    set status "none"
    set energy start-energy

    ; create empty lists
    set tree_ate_list []
    set tree_mem_list []
    set tree_add_list []
    set tree_pot_list []
    set seed_ate_list []
    set seed_mem_list []
    set seed_add_list []

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
      set color gray ]
    [ set pen-mode "up" ]
  ] ; end ask monkeys
end

to output-files
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
;  file-print (word "ticks" "," "day" "," "timestep" "," "x" "," "y" "," "energy" "," "status" "," "action") ;; FOR CSV
  file-print (word " " "ticks" " " "day" " " "timestep" " " "x" " " "y" " " "energy" " " "status" " " "action") ;; FOR TXT
  file-close

  file-open output-seeds-locations
;  file-print ("id-seed" "," ) ;; FOR CSV
  file-print (word " " "id-seed" " " "x" " " "y" " " "species" " " "mother-tree") ;; FOR TXT
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

end



;--------------------------------------------------------------------------------------------
; Activities commands
;--------------------------------------------------------------------------------------------
to go

  if all? monkeys [status = "sleeping"] [
    ask monkeys [
      type timestep type " - Energy: " type energy type " Status: " type status type " "
      type tree_target  type " " show action
      ]
    set day day + 1
    stop
  ]

  move-monkeys
  set timestep timestep + 1
  tick
  write-to-file ;; WRITE-FILE IS CALLED AGAIN IN next_day(), BUT IT CAN'T BE COMMENTED OUT FROM HERE, JUST THERE
end


;-------------------------------------------------------------
to run_days

  repeat no_days [ next_day ]
  write-seeds
  write-rest
  write-sleep
  write-trees
end

;-------------------------------------------------------------
to next_day

; export the landscape as a .png if neccessary  (= Milles et al 2020)
  if day = no_days AND export-png = TRUE [
   let file-id random 9999
   let world-name (word runtime no_days "_" "e-" start-energy "_" file-id "_world.png") ; date-and-time
   export-view world-name
;   export-interface world-name
  ]

  set timestep 0

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


  ;; STOP CONDITION ;; NOT NECESSARY BECAUSE THE CODE RUNS repeat run_days
  if simulation-time-end = TRUE [
    stop
    reset-ticks
  ]


  ask monkeys [
    set status "none"
    set action-time 0
  ]

  loop [
    if all? monkeys [status = "sleeping"][
      ask monkeys [ set tree_target  0 ]
      set day day + 1
      stop
    ]
    go

  ]

  ;write-to-file

end


;-------------------------------------------------------------------------------

to move-monkeys

  ask monkeys
  [
    if patch-ahead pcolor = yellow + 4 [ right 180 forward 0.5 ] ; no use of the matrix
    if energy < 1 [die]
    if timestep = 1
    [ morning-defecation                 ;; MORNING-DEFECATION PROCEDURE
      set energy start-energy ]

  ;; BLT ROUTINE

    if timestep = simulation-time [ ; if the day is in the end  ; MAYARA HAS LET THIS IN THE SETUP BEFORE move-monkeys PROCEDURE
      ask monkeys [
        set status "night"
        set action ""
        set tree_target 0
        set tree_current 0
      ]
    ]

    ;; ASK RONALD: WHY KEEPING THIS AS IFELSE MAKES THE MODEL RUN INFINETLY
    if timestep > simulation-time [ ; if the day is over and if not  ; MAYARA CODE WAS SENDING TAMARINS TO SLEEPING-TREES ONLY IN THE LAST STEP
      set action "to sleeping tree"
      sleeping                           ;; SLEEPING PROCEDURE
    ]

    if timestep < simulation-time [ ; energy levels: level 1 = 80 and level 2 = 150
      ifelse energy > 80 [
        ifelse (status = "hungry" or status = "none") [
          ifelse energy > 150 [ ; energy > level 2 ==> other activities
            ifelse (timestep > (midday - 10) and timestep < (midday + 10)) [
              if status != "resting" [ set tree_target 0 set tree_current 0 ]
              set status "resting"
              resting                    ;; RESTING PROCEDURE
            ][
              set tree_target 0
              random-action              ;; RANDOM-ACTION PROCEDURE
             ]
      ][ ; 80 < energy < 150
              ifelse action = "on feeding tree" [
              ifelse random (2 * species_time) > action-time [
                feeding
              ][
                set action-time 0
                set tree_ate_list lput [who] of tree_current tree_ate_list
                set tree_mem_list lput 0 tree_mem_list
                set tree_add_list lput 1 tree_add_list

                let let_pot_list tree_pot_list
                set tree_current 0
                to-feeding-tree          ;; TO-FEEDING-TREE PROCEDURE
              ]
            ][
                to-feeding-tree          ;; TO-FEEDING-TREE PROCEDURE
              ]
          ]
        ][ ; status != "hungry"
          ifelse random (2 * 5) > action-time [ ; action time for other than feeding
            set action-time action-time + 1
            last-action-again            ;; LAST-ACTION-AGAIN PROCEDURE
          ][
            set action-time 0
            set tree_target 0
            change-bonus                 ;; CHANGE-BONUS PROCEDURE
          ]
        ]
      ][ ; energy < 80 (level 1)
        set status "hungry"
        ifelse action = "on feeding tree" [
          ifelse random (2 * species_time) > action-time [
            set action-time action-time + 1
            feeding                      ;; FEEDING PROCEDURE
          ][
            set action-time 0
            set tree_ate_list lput [who] of tree_current tree_ate_list
            set tree_mem_list lput 0 tree_mem_list
            set tree_add_list lput 1 tree_add_list

            let let_pot_list tree_pot_list
            set tree_current 0
            to-feeding-tree              ;; TO-FEEDING TREE PROCEDURE
          ]
        ][
          to-feeding-tree                ;; TO-FEEDING TREE PROCEDURE
        ]
      ]
      forget_trees                       ;; FORGET_TREES PROCEDURE
      defecation                         ;; DEFECATION PROCEDURE
    ] ; end of daily routine

 ; for the display
 ifelse show-energy? [
      set label round energy
      set label-color black
    ]
    [set label ""]

    ifelse show-path? [
      set pen-mode "down"
      set color gray ]
    [ set pen-mode "up" ]
] ; end ask monkeys
end

;--------------------------------------------------------------------------------
; Feeding commands
;--------------------------------------------------------------------------------
to feeding
  ;set status "feeding"
  set energy energy + energy-from-seeds + energy_species
  set action-time action-time + 1
  set seed_ate_list lput [who] of tree_current seed_ate_list
  set seed_mem_list lput 0 seed_mem_list
  set seed_add_list lput 1 seed_add_list
end

;-----------------------------------------

to to-feeding-tree

  ifelse tree_target = 0 [
    set action "search feeding tree"
    search-feeding-tree
  ][
    set steps-moved steps-moved + 1
    set energy energy + energy-loss-traveling
      set tree_current tree_target
      set tree_target 0
      set tree_pot_list remove-item position [who] of tree_current tree_pot_list tree_pot_list
      set action "on feeding tree"
  ]
end

;----------------------------------------

to search-feeding-tree

  set action "search feeding tree"
  let let_pot_list tree_pot_list
  set tree_target min-one-of feeding-trees with [member? who let_pot_list] [distance myself]
  set heading towards tree_target
  let tree_target_species [ species ] of tree_target

   if tree_target_species = "annona" [
    set species_time 3
    set energy_species 5
  ]
  if tree_target_species = "celtis" [
    set species_time 3
    set energy_species 2
  ]
  if tree_target_species = "cissus" [
    set species_time 3
    set energy_species 4
  ]
  if tree_target_species = "cordia" [
    set species_time 3
    set energy_species 4
  ]
   if tree_target_species = "diospyrus" [
    set species_time 3
    set energy_species 3
  ]
  if tree_target_species = "ficus" [
    set species_time 3
    set energy_species 2
  ]
  if tree_target_species = "pereskia" [
    set species_time 3
    set energy_species 5
  ]
  if tree_target_species = "rhipsalis" [
    set species_time 3
    set energy_species 1
  ]
  if tree_target_species = "syagrus" [
    set species_time 3
    set energy_species 3
  ]
 if tree_target_species = "rhamnidium" [
    set species_time 3
    set energy_species 4
  ]
  if tree_target_species = "unknown" [
    set species_time 3
    set energy_species 1
  ]
  if tree_target_species = "claussenii" [
    set species_time 3
    set energy_species 1
  ]
  if tree_target_species = "eugenia" [
    set species_time 3
    set energy_species 3
  ]
  if tree_target_species = "sp_five" [
    set species_time 3
    set energy_species 2
  ]

  forward travel_speed
  set steps-moved steps-moved + 1
  set energy energy + energy-loss-traveling
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
      hatch-seeds 1 [ ; change to hatch more seeds! <<<
        setxy xcor ycor
        set mother-tree [id-tree] of feeding-trees with [ who = loc_who ]
        set species [species] of feeding-trees with [ who = loc_who ]
        set id-seed who
        set label ""
        set shape "plant"
        set size 0.7
        set color 1
      ]
    ]
  ]
  set seed_mem_list (map + seed_add_list seed_mem_list)
end

;----------------------------------------------------

to morning-defecation

  foreach seed_ate_list [
    x ->  hatch-seeds 1 [ ; change to hatch more seeds! <<<
      setxy xcor ycor
      set mother-tree [id-tree] of feeding-trees with [ who = x ]
      set species [species] of feeding-trees with [ who = x ]
      set id-seed who
      set label ""
      set shape "plant"
      set size 0.7
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

  ifelse action = "on resting tree" [
    set energy energy + energy-loss-resting
    set status "resting"
  ][
    to-resting-tree
  ]
  if energy < 80 [ ; change from resting to feeding to forget the tree
    set status "hungry"
    set tree_target 0
    set tree_current 0
  ]
end

;----------------------------------------------------

to to-resting-tree

  ifelse tree_target = 0 [
    search-resting-tree ; when simulating trees
    ;search-resting-defined ; when using field trees
  ][
    set steps-moved steps-moved + 1
    set energy energy + energy-loss-traveling
      set tree_current tree_target
      set tree_target 0
      set action "on resting tree"
      ;set status "resting"
  ]
end

;----- active when NOT simulating resting trees (data from field) --------------
to search-resting-defined

  set action "to resting tree"
  set tree_target min-one-of resting-trees [distance myself]
  forward travel_speed
  set steps-moved steps-moved + 1
  set energy energy + energy-loss-traveling
end

;----- active when simulating resting trees --------------
to search-resting-tree

set action "to resting tree"
let n random 100
  ifelse n <= 30 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 0.12 ] [forward 0.12 ]]
  [ ifelse n <= 75 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 0.3 ] [ forward 0.3 ]]
    [ ifelse n <= 85 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 0.44 ][ forward 0.44 ]]
      [ ifelse n <= 95 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 0.84 ][ forward 0.84 ]]
            [ if n <= 100 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 1.84 ][ forward 1.84 ]]
          ]
        ]
      ]
    ]
  hatch-resting-trees 1 [
        set size 1
        set shape "tree"
        set color magenta
        set label ""
        setxy xcor ycor
        set species "rest"
        set id-tree who
  ]
set tree_target resting-trees-here
set steps-moved steps-moved + 1
set energy energy + energy-loss-traveling
end

;---------------------------------------------------------------------------------------------
; Sleeping commands
;---------------------------------------------------------------------------------------------
;; MAYARA CODE:
to sleeping
  ifelse tree_target = 0 and tree_current = 0 [
   search-sleeping-tree     ; when simulating trees  ; ONLY WORKS WITH THIS PROCEDURE
   ;search-sleeping-defined ; when using field trees ; WITH THIS IT DOES NOT
  ][
;    set steps-moved steps-moved + 1
;    set energy energy + energy-loss-traveling
;      set tree_current tree_target
;      set tree_target 0
;      set status "sleeping"
;      set action ""
;      ; trees in the tree_ate_list[] had to get back to the tree_pot_list[]
;      while [length tree_ate_list > 0] [
;        set tree_pot_list lput first tree_ate_list tree_pot_list
;        set tree_ate_list remove (first tree_ate_list) tree_ate_list
;      ]
      set tree_mem_list []
      set tree_add_list []
      output-day-stats
  ]
end

;----- activate when NOT simulating sleeping trees (data from field) --------------
to search-sleeping-defined

  set tree_target min-one-of sleeping-trees [distance myself]
  forward travel_speed
  set steps-moved steps-moved + 1
  set energy energy + energy-loss-traveling
  set status "sleeping"
end

;----- activate when simulating sleeping trees --------------
to search-sleeping-tree

; THIS CODE IS WHAT BASICALLY MAKES TAMARINS MOVE OUT OF WHERE THEY ARE AND GO TO OTHER PART OF THE HOME RANGE
  let n random 100
  ifelse n <= 15 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 1 ] [forward 1 ]]
  [ ifelse n <= 30 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 2 ] [ forward 2 ]]
    [ ifelse n <= 50 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 3.5 ][ forward 3.5 ]]
      [ ifelse n <= 70 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 4 ][ forward 4 ]]
        [ ifelse n <= 85 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 6 ][ forward 6 ]]
          [ if n <= 100 [ ifelse [pcolor] of patch-here = yellow + 4 [ right 180 forward 8 ][ forward 8 ]]
          ]
        ]
      ]
    ]
  ]

  ;forward travel_speed
  set steps-moved steps-moved + 1
  set energy energy + energy-loss-traveling
  hatch-sleeping-trees 1 [
        set size 1
        set shape "tree"
        set color sky - 2
        set label ""
        setxy xcor ycor
        set species "sleeptree"
        set id-tree who
  ]
  ;set tree_target sleeping-trees-here ;; sleeping-trees-here does not exist anywhere else
  set status "sleeping"
end

;---------------------------------------------------------------------------------------------
; Commands for other activities
;---------------------------------------------------------------------------------------------
to forage

set status "forage"

  let n random 100
  ifelse n <= 20 [  ]
  [ ifelse n <= 35 [ right 45 ]
    [ ifelse n <= 50 [ left 45 ]
      [ ifelse n <= 60 [ right 90 ]
        [ ifelse n <= 70 [ left 90 ]
          [ ifelse n <= 80 [ right 135 ]
            [ ifelse n <= 90 [ left 135 ]
              [ if n <= 100 [ left 180 ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
  ;
  forward foraging_speed
  set steps-moved steps-moved + 1
  set energy energy + energy-from-prey + energy-loss-foraging
end

;-------------------------------------------------------------
to random-action

set action-time 0
if action = "on feeding tree" [
    set tree_ate_list lput [who] of tree_current tree_ate_list
    set tree_mem_list lput 0 tree_mem_list
    set tree_add_list lput 1 tree_add_list
  ]
set tree_target 0
set tree_current 0
let choice random 100
  ifelse choice < 50 [
    set status "forage"
    set action "forage"
    forage
  ]
  [
    set status "resting"
    resting
  ]
set tree_target 0
set tree_current 0
end

;-------------------------------------------------------------
to last-action-again

  if status = "forage"   [ forage ]
  if status = "resting" [ resting ]

end

;-------------------------------------------------------------
to change-bonus

  set action-time 0
  let choice random 2
  let pot-status ["forage" "resting"]
  let old-status status
  let new-status ""
  let other-status remove old-status pot-status
  if choice = 0 [
    set status old-status
  ]
  if choice = 1 [
    set new-status one-of other-status
    set status new-status
    set action new-status
  ]
  if status != "resting" [
    set tree_target 0
    set tree_current 0
  ]
  last-action-again
end



;---------------------------------------------------------------------------------------------
; Extra commands
;---------------------------------------------------------------------------------------------
to forget_trees

  ; testing if the monkey forgets a tree AND returns this one back to the available list
  if member? input_forget tree_mem_list [
    let loc_index position input_forget tree_mem_list
    let loc_who item loc_index tree_ate_list
    set tree_ate_list remove-item loc_index tree_ate_list
    set tree_add_list remove-item loc_index tree_add_list
    set tree_mem_list remove input_forget tree_mem_list
    set tree_pot_list lput loc_who tree_pot_list
  ]
  ; increase the memory counter
  set tree_mem_list (map + tree_add_list tree_mem_list)
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
      file-write status
      file-write action
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

to-report simulation-time-end
  ifelse ticks = simulation-time
  [   report TRUE   ]
  [   report FALSE  ]

end

@#$#@#$#@
GRAPHICS-WINDOW
10
10
681
682
-1
-1
13.0
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
718
222
890
255
start-energy
start-energy
0
170
63.0
1
1
NIL
HORIZONTAL

BUTTON
714
47
788
89
RESET
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
946
222
1064
255
show-energy?
show-energy?
0
1
-1000

SWITCH
946
264
1066
297
show-path?
show-path?
0
1
-1000

SLIDER
717
267
889
300
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
719
349
891
382
energy-from-seeds
energy-from-seeds
0
15
0.0
1
1
NIL
HORIZONTAL

BUTTON
711
98
791
131
STEP
go
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
800
99
908
132
go (Continue)
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
708
147
791
180
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
799
147
885
182
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
808
17
884
77
no_days
7.0
1
0
Number

SLIDER
718
392
890
425
energy-from-prey
energy-from-prey
0
15
4.3
0.1
1
NIL
HORIZONTAL

SLIDER
717
437
893
470
energy-loss-traveling
energy-loss-traveling
-10
0
-2.0
0.1
1
NIL
HORIZONTAL

SLIDER
718
484
894
517
energy-loss-foraging
energy-loss-foraging
-10
0
-2.2
0.1
1
NIL
HORIZONTAL

SLIDER
717
532
895
565
energy-loss-resting
energy-loss-resting
-10
0
-1.4
0.1
1
NIL
HORIZONTAL

MONITOR
895
24
971
69
timestep
timestep
17
1
11

OUTPUT
913
313
1137
603
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
700
612
1107
698
runtime
\\runtime\\
1
0
String

CHOOSER
980
16
1123
61
tree-scenario
tree-scenario
"trees_all_1" "trees_all_2" "trees_april_1" "trees_may_1" "trees_june_1" "trees_july_1" "trees_aug_1"
1

CHOOSER
980
66
1125
111
sleeping-trees-scenario
sleeping-trees-scenario
"empirical" "simulated"
1

SWITCH
948
179
1074
212
export-png
export-png
0
1
-1000

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
