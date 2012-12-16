===Dependecies===
SDL
SDL_image
SDL_ttf
SDL_gfx

===Run the code===
ghci -lSDL Run

use surface language to control the player
processFile (Just "filename") scene_setting
or
use keyboard to control the player
processFile Nothing scene_setting

e.g.
processFile (Just "scripts/signal.src") setting5
will use the movements defined in signal.src to control the player.
