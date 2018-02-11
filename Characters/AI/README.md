# AI
The basic idea will be to use a basic deterministic finite state machine to model creature behavior.
## DFSA
The states are
* `wait`
    This indicates a continued holding pattern, such as resting or sleep.
* `wander`
    The creature will wander in a random valid direction for some time.
* `investigate`
    The creature will move towards a source of noise. (this one may introduce complications, how do you model noise detection?)
* `chase`
    The creature has seen the player (or perhaps another hostile creature) and is moving towards it.
* `fight`
    The creature has entered combat range with the player or other creature and is fighting it.
* `flee`
    The creature has been damaged and is attempting to escape from the player or other creature.
## Diagram
![DFSA Diagram on imgur](https://i.imgur.com/gjLGy9J.png)