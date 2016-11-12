<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orged35eb9">1. What is a level?</a>
<ul>
<li><a href="#org52db22e">1.1. Possibilities</a></li>
</ul>
</li>
<li><a href="#org0d52cb1">2. Map representation</a></li>
<li><a href="#org23d8cdf">3. Room representation</a>
<ul>
<li><a href="#org44954d5">3.1. Rooms</a>
<ul>
<li><a href="#orgf2211f0">3.1.1. Generation</a></li>
</ul>
</li>
<li><a href="#orgde7c3bf">3.2. Shapes</a>
<ul>
<li><a href="#org9d95740">3.2.1. Rectangles</a></li>
<li><a href="#org90acd67">3.2.2. Circles</a></li>
<li><a href="#org0bcfbc5">3.2.3. Triangles</a></li>
</ul>
</li>
<li><a href="#org0e3c53e">3.3. Generating random shapes</a>
<ul>
<li><a href="#org36bd347">3.3.1. Rectangles</a></li>
<li><a href="#org384a721">3.3.2. Circles</a></li>
<li><a href="#org3c862f5">3.3.3. Triangles</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#org5b0ce7d">4. Hallways</a></li>
</ul>
</div>
</div>

<a id="orged35eb9"></a>

# What is a level?

-   [Roguelike Map Structures](http://gamedev.stackexchange.com/questions/65861/roguelike-map-structure)


<a id="org52db22e"></a>

## Possibilities

-   A 2D vector, where each cell stores static terrain information and elements act as coordinates.
    -   Use SRFI-25, or roll your own in Racket since SRFI-25 uses opaque SRFI-9 type records.
        That might be time consuming though.
-   Entities (players, monsters, items) are stored separately as a list or a hash table per
    appropriate category. Maybe you can use the location in the array as the hash-key. To update, step
    through the table of living entities and update each one appropriately. It might be time-consuming
    though, since you'd have to create a new hash-table every turn as monsters and players are in a
    different coordinate in the array and therefore in a different location in the table.
-   A level is the set of all walkable coordinates (i.e., not walls). How do you represent
    "black space" between walls? Maybe a level is the set of all walkable coordinates AND
    the coordinates of the outer bounds? Then you simply render a black level, and overlay
    the open spaces on top of it at the appropriate coordinate points. The boundary can be
    represented as width and height, a square. (level 100 100 [Set-of Coordinates])


<a id="org0d52cb1"></a>

# Map representation

-   It would be nice if, instead of storing every coordinate of the level in a vector/etc., we could
    store only a "node" representing every room. Every level must have a set of nodes such that at least
    one path exists between the entry- and exit-node. Each node contains a function which describes its
    shape. Then when we draw the node as a room, we only need to apply its function. Collision detection
    is then just checking whether the function is no longer true for some value representing the
    player's position.
    -   How do we do collision detections for hallways between rooms? How do we represent hallways?
    -   Each node could contain a 2D-vector of cells, and have its own list of items and monsters maybe,
        instead of having them stored on the top-level.


<a id="org23d8cdf"></a>

# Room representation

A room has a name, a shape, a center, and 4 neighbors (1 in each cardinal direction).


<a id="org44954d5"></a>

## Rooms

Given a shape, how do we determine a center? 

1.  The shape will be created from the center, the center is generated then passed to the
    shape function.
2.  The center will be created from the shape and defined later when "arranging" all rooms
    in the grid. Each room has a shape, but then that shape is shifted around the grid so
    it does not collide with any other, then that is used as its center.

Currently, the center-field of a room is a lambda, binding a posn struct to a width and
height variable, so it can be called later when generating a grid. This is more of a
placeholder than anything. The two options are given above.


<a id="orgf2211f0"></a>

### Generation

Option: We create a function for a room, then we take the remaining grid which is not
within that function's codomain, and create new rooms from that grid recursively.


<a id="orgde7c3bf"></a>

## Shapes

*A shape is a conjunction of an arbitrary number of line functions*.
A shape is a function which consumes a point and determines whether that point is within that shape.
To create a shape we first determine the inequalities which represent the shapes outer boundaries, of
which there may be one or multiple. Shapes are defined in terms of logical cells, not real points.

-   Wouldn't it be simpler to represent a shape *only* as a set of inequalities, since we
    don't use the center except for circles?
-   In that case, how do we represent a circle without hard-coding its central point? Maybe we could
    make the central point a part of the returned function, and it would simply be grabbed from the
    struct representing the room when called.
-   Would it be possible to instead represent a room as a series of vertices, and then generate the
    inequalities from these vertices?
-   What if a line is a Bezier curve?
-   Experimenting with the following shapes led me to understand some of the brittleness of
    the inequality approach, namely, that it's really damn hard to generate a valid series
    of inequalities that represents a shape. Rectangles and triangles cannot be arbitrarily
    rotated easily. Here's an idea: what if we could image a level as a 2 2-dimensional
    planes overlaying one another, where the upper one may have shapes described as bezier
    curves, and the lower one is a grid. To create a level-as-a-grid, we "punch through" the
    top layer to the bottom one and align the curves to the grid, similar to how
    font-rendering works. This is what I was thinking of before.


<a id="org9d95740"></a>

### Rectangles

Four lines, determining that a point is right of its left side, left of its right side,
"above" its bottom, and "below" its top. Since Racket makes the origin (0, 0) the upper
left, for a point to be "above" the bottom it must have a lower y-value.


<a id="org90acd67"></a>

### Circles

A circle is a single line, and determines that any point is within a radius of a central point.


<a id="org0bcfbc5"></a>

### Triangles

This one is a doozy. It's hard to create a good model for the inequality functions for
triangles, because origin is at (0, 0), which is a flipped/mirrored version of what we
would expect when writing these inequalities (bottom left or center). It doesn't seem that
we can randomly create the inequalities for each side, because they differ based on how
the triangle is rotated and where the base is. If a triangle's base is on the bottom,
then:

-   Left side determines whether a point is right of it (below the line, y ≤ (f x))
-   Right side determines whether a point is left of it (below the line, y ≤ (f x))
-   Bottom side determines whether a point is above it (y ≤ (f x))

If a triangle's base is on the top, then:

-   Left side determines whether a point is right of it (above the line, y ≥ (f x))
-   Right side determines whether a point is left of it (above the line, y ≥ (f x))
-   Base side determines whether a point is below it (y ≥ (f x))


<a id="org0e3c53e"></a>

## Generating random shapes


<a id="org36bd347"></a>

### Rectangles

Simply generate a random series of numbers. Beginning with the left (random between 0 and
WIDTH minus ROOM-WIDTH to ensure it doesn't fall out of bounds), the right is a random
number between 1 + left and WIDTH, the top is between 0 and HEIGHT - ROOM-HEIGHT, and
the bottom is a random number between 1 + top and the maximum height at the bottom of the screen.


<a id="org384a721"></a>

### Circles

Generate a random number from half the maximum width for the radius, then define a center
which has two random points between the radius and the WIDTH minus the radius.


<a id="org3c862f5"></a>

### Triangles

First generate a base-width, a random number between 1 and ROOM-WIDTH. Currently unused,
but maybe used to generate sides by finding two lines which intersect at both ends of the
base, and intersect one another. Then generate a random y-intercept, between 7 and the
height. 7 is arbitrarily chosen as a minimum to ensure it isn't intersecting at the
origin, which caused some odd behavior when selecting random numbers. Then choose a base,
between the y-intercept and the HEIGHT. We then define a (random-slope) function, which
takes the y-intercept as the base for an exponent function, whose exponent is 0 or 1.
i.e., the slope is either 1 or the y-intercepts number. This might need to be fixed.
Then we generate 3 functions: The left side, which multiplies x by the random slope, and
adds it to the y-intercept for usual mx+b form. The 2nd is similar, but x is negative. The
last is the base, which is a constant function which always returns the base (flat line).


<a id="org5b0ce7d"></a>

# Hallways

Maybe we can generate hallways using a version of Dijkstra's algorithm. 

1.  We create all rooms and attach functions & constraints representing shapes to each room.
2.  Then we arrange all rooms in a grid such that no function's codomain overlaps another function's.
3.  Then we let every cell in the grid representa node of a graph which has neighbors in every cardinal
    direction.
4.  Then we generate hallways between connected rooms by finding the shortest path through each cell
    which does not intersect with an outer room's (a node) function.
    -   We can take the inverse of the graph of rooms to find all cells which are hallways.

; First we create a graph where each room is a node. Then we create a grid whose dimensions can
; contain every room. The grid can be represented as a graph where every cell is a node with an edge
; in every cardinal direction. Then we take a subset of that graph representing every cell which is
; not covered by a room.

