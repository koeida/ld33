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

screenWidth = 1024
screenHeight = 768

type Action = Spin | Tick Time

type alias Position = {x: Float, y: Float}

type alias Sprite = 
    { shape : Form
    , rot : Float
    , animations : List (Maybe AnimationState)
    , pos : Position 
    , brain : Brain
    , kind : EntityKind
    }

type Brain = Standing | Moving Position
type EntityKind = Player | Goblin | Warrior 

type AnimationType 
    = Rotation 
    | XPos 
    | YPos

type AnimationState = AnimationState 
    { elapsedTime : Time
    , delay : Time
    , startVal : Float
    , endVal : Float
    , easing : (Float -> Float)
    , len : Float
    , animationType : AnimationType
    }

--Apply the current animation state to the sprite
stepAnimation : Maybe AnimationState -> Sprite -> Sprite
stepAnimation animation s =
    case animation of
        Nothing ->
            s
        Just (AnimationState anim) ->
           let 
               pos = s.pos
           in
               case anim.animationType of
                    Rotation ->
                        { s | rot <- ease anim.easing float anim.startVal anim.endVal anim.len anim.elapsedTime 
                        }
                    XPos ->
                        { s | pos <- {pos | x <-  ease anim.easing float anim.startVal anim.endVal anim.len anim.elapsedTime}
                        }
                    YPos ->
                        { s | pos <- {pos | y <-  ease anim.easing float anim.startVal anim.endVal anim.len anim.elapsedTime}
                        }
                    _ -> s

animLength = 1500
animAmount = 90

goblinImg = toForm (image 16 16 "assets/goblin_run_down.gif")
backgroundImg = toForm (tiledImage screenWidth screenHeight "assets/sand_1.png")

goblins = List.map (\x -> goblin (Random.initialSeed x)) [0..200]

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


update : Action -> (List Sprite) -> (List Sprite)
update action ss =
    case action of
        Tick t -> List.map (tickSprite t) ss
        Spin -> ss 

allNothing : List (Maybe a) -> Bool
allNothing l =
    let
        flist = List.filter (\m -> case m of 
                                       Nothing -> False 
                                       _ -> True) l
    in
        (List.length flist) == 0

view : List Sprite -> Element
view ss = 
    let 
        shapes = List.map (\s -> s.shape
                        |> rotate (degrees s.rot)
                        |> moveX s.pos.x
                        |> moveY s.pos.y) ss
    in
        collage screenWidth screenHeight 
                        ([ backgroundImg ] ++ shapes) 

ticks = map Tick (fps 60)
keys = map (\_ -> Spin) Keyboard.space
streams = merge ticks keys

main = view <~ foldp update goblins streams
