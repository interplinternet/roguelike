<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orga97f7d3">1. Focus</a></li>
<li><a href="#org85d0974">2. Key ideas</a>
<ul>
<li><a href="#org2f787b0">2.1. Magic</a></li>
<li><a href="#org3f63287">2.2. Level generation</a></li>
<li><a href="#orge7d0b57">2.3. Combat</a></li>
</ul>
</li>
</ul>
</div>
</div>

<a id="orga97f7d3"></a>

# Focus

I began this project as a way to improve my programming skills, particularly in larger
projects where it's more important to manage multiple modules and bring them together
instead of smaller scripts where you can just write it, run it, and then it's done.


<a id="org85d0974"></a>

# Key ideas


<a id="org2f787b0"></a>

## Magic

The main inspiration for this project is an improved magic system for roguelikes. Most
roguelikes treat magic as atomic and analogous to regular combat e.g., sword fighting or
archery. What I want to see in a game is a function-composition based magic system. 

For example, a spell is a curried function `((f spell ...) target)`, where `spell` is
another optional spell to combine. So when you want to create or "weave" your own spell
you can define it easily as a series of compositions `(f ∘ g ∘ h ∘ ...)` and then apply or
"cast" it on the target. For example fire spells can be combined with explosive spells
for AoE fire explosions. I was thinking it would be cool to build that off an
element-combining system (e.g., "explosion" comes from combining a fire spell and an ice
spell or something) but that would complicate things a bit, since I'd need to keep track
of which spells had been applied on the target already, but I was thinking it would be
really cool to model that with a stack. A N/PC has a stack which represents the effects
that it has, then element-combining is just like basic arithmetic with a stack where you
pop the two elements and combine them, which returns another element that is the
combination of both, and then repeat until no elements can be combined.


<a id="org3f63287"></a>

## Level generation

The level generation uses a model based on the idea of stepping down a ladder of
abstraction. 

1.  At it's most abstract, what is a map? A set of vertices in a graph. To create a
    map,generate a random graph. Each vertex is a room and the edges represent
    connections between them.

2.  At the next level, what is a map? A series of shapes. So give each vertex in the
    graph a function representing a geometric shape.

3.  Well, what is a map after that? A grid. So overlay the graph with its shape on the
    grid, and all the cells that "fall through" the shapes are the rooms.

4.  Then you just look at the shapes of the graph and trace a path between all of the
    connected rooms on the grid.


<a id="orge7d0b57"></a>

## Combat

