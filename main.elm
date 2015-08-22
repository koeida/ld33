module LD33 where
import Graphics.Element exposing (..)
import Text exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Mouse
import List
import Debug
import Time exposing (..)
import Signal exposing (..)
import Keyboard
import Random
import Easing exposing (..) 
import Model exposing (..)
import Misc exposing (..)

screenWidth = 1024
screenHeight = 768

animLength = 1500
animAmount = 90

goblinImg = toForm (image 16 16 "assets/goblin_run_down.gif")
playerImg = toForm (image 32 32 "assets/player_stand.png")
backgroundImg = toForm (tiledImage screenWidth screenHeight "assets/sand_1.png")

startingGoblins = List.map (\x -> goblin (Random.initialSeed x)) [0..200]

startingPlayer = 
    { shape = playerImg
    , rot = 0
    , brain = Standing
    , kind = Player
    , pos = {x = 100, y = 100}
    , animations = []
    }

goblin seed = 
    let 
        (xpos,seed') = Random.generate (Random.float -50.0 150.0) seed
        (ypos,seed'') = Random.generate (Random.float -50.0 150.0) seed'
    in 
         { shape = goblinImg
         , rot = 0
         , brain = Standing
         , kind = Goblin
         , pos = {x = -50 + (xpos * 16),y = ypos}
         , animations = [Just (AnimationState 
                                 { elapsedTime = 0
                                 , delay = 0
                                 , startVal = ypos
                                 , easing = Easing.linear --easeOutLinear
                                 , len = 10000
                                 , animationType = YPos
                                 , endVal = ypos - 200
                                 })
                        ]
         }

-- Apply time t to animationstate
tickAnim : Time -> Maybe AnimationState -> Maybe AnimationState
tickAnim t a =
    case a of
        Just (AnimationState anim) -> 
            if anim.delay <= 0 
               then if anim.elapsedTime + t > anim.len
                    then Nothing
                    else Just (AnimationState { anim | elapsedTime <- anim.elapsedTime + t })
               else Just (AnimationState {anim | delay <- anim.delay - t})



        Nothing -> Nothing

tickSprite : Time -> Sprite -> Sprite
tickSprite t s = 
    let
        animations' = List.map (tickAnim t) s.animations
        s' = List.foldr stepAnimation s animations'
    in
       { s' | animations <- animations' }


update : Event -> World -> World
update event world =
    case event of
        NewFrame t -> {world | goblins <- List.map (tickSprite t) world.goblins }
        Keys k -> world 

view : World -> Element
view world = 
    let 
        shapes = List.map (\s -> s.shape
                        |> rotate (degrees s.rot)
                        |> moveX s.pos.x
                        |> moveY s.pos.y) world.goblins
        player = world.player.shape 
            |> moveX world.player.pos.x 
            |> moveY world.player.pos.y
    in
        collage screenWidth screenHeight 
                        ([ backgroundImg ] ++ shapes ++ [player]) 

keyinput : Signal Event
keyinput = 
    let 
        keySignal = map2 (\a s -> Keys {x = a.x, y = a.y, space = s}) Keyboard.arrows Keyboard.space
    in
        sampleOn (every <| 10 * millisecond) keySignal

ticks = NewFrame <~ fps 30
streams = merge ticks keyinput

startWorld = { goblins = startingGoblins, player = startingPlayer } 

main = view <~ foldp update startWorld streams
