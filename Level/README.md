<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org7d15645">1. What is a level?</a>
<ul>
<li><a href="#org2dd1c93">1.1. Possibilities</a></li>
</ul>
</li>
<li><a href="#org1393d20">2. Map representation</a></li>
<li><a href="#org18374e1">3. Room representation</a>
<ul>
<li><a href="#org8f15750">3.1. Rooms</a>
<ul>
<li><a href="#orgdff7117">3.1.1. Generation</a></li>
</ul>
</li>
<li><a href="#orgd272c4d">3.2. Shapes</a>
<ul>
<li><a href="#orgd5055de">3.2.1. Rectangles</a></li>
<li><a href="#org92656d2">3.2.2. Circles</a></li>
<li><a href="#org3ee1d46">3.2.3. Triangles</a></li>
</ul>
</li>
<li><a href="#org9a5976c">3.3. Generating random shapes</a>
<ul>
<li><a href="#org79a66c6">3.3.1. Rectangles</a></li>
<li><a href="#org2ea3254">3.3.2. Circles</a></li>
<li><a href="#org56c628e">3.3.3. Triangles</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#org93b4042">4. Level generation</a>
<ul>
<li><a href="#org13af61f">4.1. Graph</a></li>
<li><a href="#orgae7dac4">4.2. Shapes</a></li>
<li><a href="#org2940ee9">4.3. Grid</a></li>
</ul>
</li>
<li><a href="#org975a8e5">5. Hallways</a>
<ul>
<li><a href="#org39805ec">5.1. New plan</a></li>
<li><a href="#orga76e77a">5.2. Connect</a>
<ul>
<li><a href="#orga520a66">5.2.1. Example</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>

<a id="org7d15645"></a>

# What is a level?

-   [Roguelike Map Structures](http://gamedev.stackexchange.com/questions/65861/roguelike-map-structure)


<a id="org2dd1c93"></a>

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


<a id="org1393d20"></a>

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


<a id="org18374e1"></a>

# Room representation

A room has a name, a shape, a center, and 4 neighbors (1 in each cardinal direction).


<a id="org8f15750"></a>

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


<a id="orgdff7117"></a>

### Generation

Option: We create a function for a room, then we take the remaining grid which is not
within that function's codomain, and create new rooms from that grid recursively.


<a id="orgd272c4d"></a>

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


<a id="orgd5055de"></a>

### Rectangles

Four lines, determining that a point is right of its left side, left of its right side,
"above" its bottom, and "below" its top. Since Racket makes the origin (0, 0) the upper
left, for a point to be "above" the bottom it must have a lower y-value.


<a id="org92656d2"></a>

### Circles

A circle is a single line, and determines that any point is within a radius of a central point.


<a id="org3ee1d46"></a>

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


<a id="org9a5976c"></a>

## Generating random shapes


<a id="org79a66c6"></a>

### Rectangles

Simply generate a random series of numbers. Beginning with the left (random between 0 and
WIDTH minus ROOM-WIDTH to ensure it doesn't fall out of bounds), the right is a random
number between 1 + left and WIDTH, the top is between 0 and HEIGHT - ROOM-HEIGHT, and
the bottom is a random number between 1 + top and the maximum height at the bottom of the screen.


<a id="org2ea3254"></a>

### Circles

Generate a random number from half the maximum width for the radius, then define a center
which has two random points between the radius and the WIDTH minus the radius.


<a id="org56c628e"></a>

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


<a id="org93b4042"></a>

# Level generation

A level is generated by walking down a ladder of abstraction. 


<a id="org13af61f"></a>

## Graph

A level at its most abstract is a graph. So to generate a level we generate a random
graph with connected nodes.


<a id="orgae7dac4"></a>

## Shapes

We ask, what is a level? A level is a series of shapes. So we assign every node a
series of shapes and a location on an absolute grid and name them for convenience.


<a id="org2940ee9"></a>

## Grid

Finally, a level is just a grid where some cells are floors and some are walls. We
overlay grid on the series of shapes and all the cells that fall through are the floor
and all those that don't are the walls. You can also think of it as "punching-out" the
rooms in the grid. As we punch out every room, we determine a path on the grid to its
neighbor and punch that out too. Then that new grid is used as the base grid for the
next recursion as we punch out the neighbor.


<a id="org975a8e5"></a>

# Hallways

Maybe we can generate hallways using a version of Dijkstra's algorithm. 

1.  We create all rooms and attach functions & constraints representing shapes to each room.
2.  Then we arrange all rooms in a grid such that no function's codomain overlaps another function's.
3.  Then we let every cell in the grid represent a node of a graph which has neighbors in every cardinal
    direction.
4.  Then we generate hallways between connected rooms by finding the shortest path through each cell
    which does not intersect with an outer room's (a node) function.
    -   We can take the inverse of the graph of rooms to find all cells which are hallways.


<a id="org39805ec"></a>

## New plan

The original plan was to use Dijkstra's algorithm, but something similar might be:

1.  Choose the cell of a room closest to a cell of the target room.
2.  Are they the same cell?
3.  If not, dig a cell in that direction (i.e., such that the new cell is closer in
    both x- and y-dimensions).
4.  Recurse on this cell.

Are intersections between rooms okay? I don't know. On the one hand, sure, it can
create a more dynamic space for the player to play in. On the other hand, it could lead
to some weird or dumb mistakes. I'll allow intersections for now, because it's easier
to write.

First we create a graph where each room is a node. Then we create a grid whose dimensions
  can contain every room. The grid can be represented as a graph where every cell is a
  node with an edge in every cardinal direction. Then we take a subset of that graph
  representing every cell which is not covered by a room.


<a id="orga76e77a"></a>

## Connect

A cell has some x- & y-value representing its absolute coordinates on the grid, and so
does the target cell. In the coordinate system, the upper-left is 0, 0 and cells
increase in downwards and rightwards such that the bottom-right is (n, m) where n and m
are the furthest coordinates possible. We want to find a path from the home cell to the
target cell. Orthogonal to the home cell are its immediate neighbors (we do not
consider diagonal paths). If the target cell is not the home cell it must necessarily
be closer to the home-cells: left, right, upper, or bottom cell. Select all cells
immediately orthogonal to the home-cell. If one of them is not the target cell, sum
their coordinates and select the cell whose sum is closest to the sum of the target
cell. If there are multiple options, select one randomly and recurse with that as the
home-cell.


<a id="orga520a66"></a>

### Example

The home-cell is at (1, 2) in a grid of 5 by 5 cells. The target cell is (4, 0) in
the upper-right.

