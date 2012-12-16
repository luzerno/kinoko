===System Infrastructure===
The system is composed of five major parts: 
1. The surface language works as the input of the system; 
2. The parser parses the surface language into a state machine that can be understood by the FRP engine;
3. The state machine is the input to the FRP engine, it tells the engine what to do at a certain state, and when to change to another state; 
4. On every time tick, the FRP engine updates the motion behaviors of actors in the game, collects the events triggered by actors, determines the motions of the actors according to the events, and picks up the next state from the state machine.
5. The rendering module uses SDL as its backend. It reads the motions of the actors from the FRP engine and puts the actors on the screen.
<pre>
--------------------
| Surface Language |
--------------------
--------------------
|      Parser      |
--------------------
--------------------
|   State Machine  |
--------------------
--------------------        
|    FRP Engine    |
--------------------
--------------------
| Graphic Rendering|
--------------------
</pre>

===State Machine===
The state machine is translated from the surface language. Every state is defined by a <code>PlayerState</code> type, which contains the state name, the player motion of the state, and the transitions from this state to other states. 
<pre>
data PlayerState = PlayerState {
	stateName :: String,
	startPos :: Maybe P2,
	moveVelocity :: P2,
	moveAccel :: P2,
	transition :: [Transition]
} deriving (Show)
</pre>
The motion of the player is determined by the combination of the start position, the moving velocity, and the acceleration. The start position is a <code>Maybe P2</code>. When the start position is <code>Nothing</code>, the player will continue moving from the position of the last state. 
The <code>transition</code> is the state transition rules defined for the player. On the end of every tick, the game events are collected by the system, if any game event satisfies the rules, the system will pick up the state defined in <code>toStateName</code> as the next state.
<pre>
data Transition = Transition {
	onevent :: GameEvent,
	toStateName :: String
} deriving (Show)
</pre>

===FRP Engine===
Every object in the game is an actor. The currently implemented actors in the game are the player, the obstacles, the signal lights, and the bonus stars. 
The FRP engine manages all the actors. It uses a State monad to wrap all the actors in, and provides functions to create/delete/update/read the actors. 
The actor itself is not reactive, but it contains reactive values such as the motion (position, velocity), and the event triggers. It also contains non-reactive properties, like the image, the direction, the size, and label, etc. The label provides a way to distinguish different actor types (player, obstacle, etc.). 
<pre>
data Actor = Actor { 
	position :: Behavior P2,
	velocity :: Behavior P2, 
	direction :: Direction, 
	surface :: [Surface], 
	label :: Label, 
	width :: Double, 
	height :: Double, 
	triggers :: Trigger
	...
}
</pre>
The event triggers of the actor are important. They are reactive values used to detect the game events, such like "Did I hit some obstacles?", "Am I going to fall to the ground?", etc. 
<pre>
data Trigger = Trigger {
	hitBlockT :: Event AEnv,
	hitBlockTopT :: Event AEnv,
	hitBonusT :: Event AEnv,
	fallT :: Event (),
	...
}
</pre>
On every tick, the FRP engine is responsible to update all the actors and put the new actors in the State monad. The actor update here is a three-phase process. 
1. Age the motion behaviors to get new positions of the actors. 
2. Update the event triggers using the new positions. 
3. Modify the motions according to the events. Such like setting the horizontal velocity to zero when the player hits an obstacle, setting a vertical velocity when the player walks past the edge of a block, destroying the star actor when the player touches it... Age the new motion behaviors and get a final result, and put the new actors to the State monad. 

The actors should be updated at one time to avoid the conflicts. If the actors are updated separately, the actors got updated earlier may influence the other actors. This is why this three-phase update process is needed in our FRP engine. 

An <code>animate</code> function executes the FRP engine. It starts a main loop to get the time tick and passes it to the engine. Then it will use the rendering module to draw the actors onto the screen. Finally the function updates the actors and collects all the game events, the next state is determined according to the events. As we have defined a time length for each tick (15 ms), so if all the work have been done in this time period, the system will wait until the rest of the time passes.

===Graphic Rendering===
We use SDL to render to graphics. 
We keep a property called <code>step</code> in each actor, this value increases continuously after the actor is created. We use this value to show the animation. 

