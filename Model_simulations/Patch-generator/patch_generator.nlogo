patches-own [lc-val]

to initialize
  clear-all
  resize-world 0 (width - 1) 0 (height - 1)

  let start-patch patch 0 0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if setup-mode = "random" [
    set start-patch one-of patches
    let blob-maker nobody
  crt 1 [ set blob-maker self
      setxy ([pxcor] of start-patch) ([pycor] of start-patch)
    ]
  repeat lc-patch-size [
    ask blob-maker [
      ask min-one-of patches with [ lc-val != 1 ] [ distance myself ] [
          set lc-val 1
          set pcolor green
          ]
      rt random 360
      fd 1

    ]
  ]
  ask blob-maker [ die ]

  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if setup-mode = "shape-index" [
    set start-patch one-of patches
    let blob-maker nobody
  crt 1 [ set blob-maker self
      setxy ([pxcor] of start-patch) ([pycor] of start-patch)
    ]

  ask blob-maker[
      make-new-field lc-patch-size start-patch
    ]

    ask blob-maker [ die ]
  ]
end



;shape index functions

To make-new-field [field-size starting-point]
  ;trys to establish a new field of the size field-size
  ;if necessary tries establishment in up to 5 different locations
  ;reports 1 if field was established, 0 otherwise

  let success 0
  let counter 0

  while [counter < 5 and success = 0] ;if necessary try in up to 5 different locations to establish a field
  [
    let startpatch starting-point
    let direction1-list [0 1 2 3] ;from this patch it can start in 4 directions 0 up; 1 right, 2 down, 3 left
    let successful-establishment 0 ;turns only 1 if the field is successfully established
    let direction-counter 0

    ;; CHeck if the startpatch exists, if not choose an exit strategy on any free forest cell - if that does not exists, stop the model and report warning:
    if (startpatch = nobody)
    [
      set startpatch starting-point
      ;; Check if still nobody:

    ]



    while [successful-establishment = 0 and direction-counter < 4]
    [
      move-to startpatch
      let direction1 one-of direction1-list ;select one direction
      set direction-counter direction-counter + 1  ;counter for the number of already tried directions
      set direction1-list remove direction1 direction1-list ;remove this direction from the list

      turn-turtles-head direction1 ;turtle faces towards direction1

      let field-patches establish-first-row field-size ;try to establish first field row in this direction

      if field-patches != [] ;only continue, if the first row was established
      [
        let first-row-length count field-patches ;store length of first row for later extension to the other side
        let last-row-length first-row-length ;length of this row needs to be known to determine how long the next row can be

        let direction2-list [0 1] ;for field expansion either to the right or left of the turtles perspective (0 to the right, 1 to the left)
        let direction2 one-of direction2-list ;select one direction
        set direction2-list remove direction2 direction2-list ;remove this direction from the list

        let still-expanding 1
        while [field-size > count field-patches and still-expanding = 1] ;as long as the field size is not reached and the field was still expanding in the last row
        [
          let try-move move-to-next-row direction1 direction2 ;if possible go to the next row and turn 180°

          ifelse try-move = 1
          [
            let new-patches expand-field-here last-row-length field-patches field-size ;expand field in this new row

            ifelse new-patches != []
            [
              set field-patches (patch-set field-patches new-patches) ;add new patches to the field
              set last-row-length count new-patches ;store length of this row
            ]
            [;else try the different direction
              set still-expanding 0
            ]
          ]
          [
            set still-expanding 0
          ]

        ] ;while [field-size > count field-patches and still-expanding = 1]


        if field-size > count field-patches ;continue at the other side, if the field is not finished yet
        [
          set still-expanding 1
          move-to startpatch ;go back to starting point of the field
          set direction2 one-of direction2-list ;and try to expand in the other direction

          let try-move move-to-next-row direction1 direction2 ;if possible go to the next row and turn 180°

          ifelse try-move = 1
          [
            turn-turtles-head direction1 ;start in the same direction as the first row

            set last-row-length first-row-length

            set still-expanding 1
            while [field-size > count field-patches and still-expanding = 1] ;as long as the field size is not reached and the field was still expanding in the last row
            [
              let new-patches expand-field-here last-row-length field-patches field-size ;expand field in this new row

              ifelse new-patches != []
              [
                set field-patches (patch-set field-patches new-patches) ;add new patches to the field
                set last-row-length count new-patches ;store length of this row
              ]
              [
                set still-expanding 0
              ]

              set try-move move-to-next-row direction1 direction2 ;if possible go to the next row and turn 180°
              if try-move = 0
              [set still-expanding  0]

            ] ;while [field-size > count field-patches and still-expanding = 1]

          ]
          [
            set still-expanding 0
          ]
        ] ;if field-size > count field-patches


       ifelse count field-patches = field-size
       [
         set successful-establishment 1
         ;and establish this field finally
         ask field-patches
         [
           set lc-val 1

         ]





       ]
       [
         move-to startpatch
         ask field-patches
         [
           set pcolor green
           ;ToDo remove all the coloring stuff
         ]

         set field-patches [] ;if field did not reach the intended size, delete field-patches
       ]
     ] ;if field-patches != []

  ]; while [successful-establishment = 0 and directionslist != []]

  if successful-establishment = 1
  [
    set success 1
  ]
  set counter counter + 1
  ];while [counter < 5 and success = 0] ;if necessary try in up to 5 different locations to establish a field



End





To-report establish-first-row [field-size]
  ;try to establish the first row of a field
  ;reports 1 if successful, 0 if unsuccessful

  let counter 1; the first field patch is the starting patch, therefore we start with 1
  let success 1

  let min-length floor sqrt field-size ;minimum length of the first field row should be the smallest square that fits into field-size
  let max-length ceiling sqrt field-size ;maximum length of the first field row should be the next larger square
  ;this rule can be changed, e.g. if one wants to allow also very narrow fields
  ;!!!!!!!!!!!!!!!! TEST: ALLOW NARROW FIELDS:
  set max-length max-length * field.shape.factor
  if (max-length > field-size) [set max-length ceiling sqrt field-size]


  let field-patches patch-set patch-here ;this is the first patch of the field

  while [counter < max-length and success = 1 and can-move? 1] ;and turtle can move in this direction
  [
    forward 1
    ifelse [lc-val] of patch-here != 1 ;forest patch and not a home base  ;  p_homebase is now list
    [
      set field-patches (patch-set field-patches patch-here) ;add this patch to the patch-set
    ]
    [;else
      set success 0
      back 1 ;turtle should be placed in the last patch that belongs to the field
    ]
    set counter counter + 1
  ]

  if count field-patches < min-length ; in this case, establishment of the firs row failed
  [
    set field-patches []
  ]

  if field-patches != []
  [
   ask field-patches
   [
     set pcolor green
     if lc-val = 1
     [

     ]
   ]
  ]

  report field-patches

End

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To-report move-to-next-row [dir1 dir2]
  ;turtle turns around 180° and moves to the next row (left or right depending on dir2)
  ;reports 1 if this move is possible 0 if turtle would cross the edge of the world

  let turtle-ident who
  let success 1
  let location patch-here


  ask turtle turtle-ident
  [
    left 180
    if dir1 = 0 ;first row goes north
    [
      ifelse dir2 = 0
      [
        ifelse is-patch? patch-at 1 0
        [
          move-to patch-at 1 0 ;move one cell east
        ]
        [ set success 0]
      ]
      [
        ifelse is-patch? patch-at -1 0
        [
          move-to patch-at -1 0 ;move one cell west
        ]
        [ set success 0]
      ]
    ]

    if dir1 = 1 ;first row goes east
    [
     ifelse dir2 = 0
     [
       ifelse is-patch? patch-at 0 1
       [
         move-to patch-at 0 1 ;move one cell north
       ]
       [ set success 0]
     ]
     [
       ifelse is-patch? patch-at 0 -1
       [
         move-to patch-at 0 -1 ;move one cell south
       ]
       [ set success 0]
     ]
    ]

    if dir1 = 2 ;first row goes south
    [
     ifelse dir2 = 0
     [
       ifelse is-patch? patch-at 1 0
       [
         move-to patch-at 1 0 ;move one cell east
       ]
       [ set success 0]
     ]
     [
      ifelse is-patch? patch-at -1 0
      [
         move-to patch-at -1 0 ;move one cell west
      ]
      [ set success 0]
     ]
    ]

    if dir1 = 3
    [
     ifelse dir2 = 0
     [
       ifelse is-patch? patch-at 0 1
       [
         move-to patch-at 0 1 ;move one cell north
       ]
       [ set success 0]
     ]
     [
       ifelse is-patch? patch-at 0 -1
       [
         move-to patch-at 0 -1 ;move one cell south
       ]
       [ set success 0]
     ]
    ]
  ]

 report success

End

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To-report expand-field-here [length-last-row patches-so-far fieldsize]
  ;trys adding more cells to the field in a new row
  ;goes through all patches in the next row and if they are forest, but not homebases, add them to the temp list
  ;as a condition, only the first and/or the last patch in the new row can be left out, i.e. are allowed to have different uses (not forest or a home base)
  ;whenever one of the other patches in the new row is not forest or a homebase, this destroys the field shape condition (i.e. another patch apart from the first and the last one),
  ;and temp list is set empty
  ;reports the a patch-set with the new field patches

  let success 1
  let temp-set []
  let patches-needed fieldsize - count patches-so-far ;number of patches still needed to reach intended field size
  let last-field-patch []; store the last field patch of this row to be able to go back to it

  let counter 0
  while [counter < patches-needed and counter < length-last-row] ;this row can maximal have the length of the last row
  [
    ifelse [lc-val] of patch-here != 1  ;  p_homebase is now list
    [
      set temp-set (patch-set temp-set  patch-here) ;add patch to the field
      set last-field-patch patch-here
    ]
    [ ;else

      if counter >= 1 and counter <= (length-last-row - 2) ;if any patch apart from the first and the last of this row cannot be added, this row fails
      ; if e.g. length of last row is 5, counter goes from 0-4, it should fail, if counter >=1 or <= 3
      [
        set success 0
      ]

    ]
    forward 1
    set counter counter + 1
  ]


  ifelse last-field-patch != [] ;make sure that turtle stands on the last patch that belongs to the field
  [
    move-to last-field-patch
  ]
  [
    set success 0
  ]

  if success = 0 ;delete all added fields
  [
    set temp-set []
  ]

  if temp-set != [] ;ToDo this can be removed
  [
    ask temp-set
    [
      set pcolor green
    ]
  ]

  report temp-set

End

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To turn-turtles-head [dir]
  ;makes turtle face in a given direction
  ;0 up, 1 right, 2 down, 3 left

  let location patch-here ;patch were the turtle is right now

  let location-x [pxcor] of location
  let location-y [pycor] of location

  if dir = 0 ;turtle should face up north
  [
    facexy location-x (location-y + 0.1)
  ]

  if dir = 1 ;turtle should face right
  [
    facexy (location-x + 0.1)  location-y
  ]

  if dir = 2 ;turtle should face down south
  [
    facexy location-x (location-y - 0.1)
  ]

  if dir = 3 ;turtle should face left
  [
    facexy (location-x - 0.1) location-y
  ]

End
@#$#@#$#@
GRAPHICS-WINDOW
210
10
608
409
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
0
29
0
29
0
0
1
ticks
30.0

INPUTBOX
23
24
178
84
width
30.0
1
0
Number

INPUTBOX
24
90
179
150
height
30.0
1
0
Number

BUTTON
47
356
121
389
NIL
initialize
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
25
157
179
202
setup-mode
setup-mode
"random" "shape-index"
1

INPUTBOX
25
207
180
267
lc-patch-size
30.0
1
0
Number

SLIDER
30
281
202
314
field.shape.factor
field.shape.factor
0.5
5
4.5
.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.0.2
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
