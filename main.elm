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

type Sprite = Sprite 
    { shape : Form
    , rot : Float
    , animations : List (Maybe AnimationState)
    , x : Float
    , y : Float
    }

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

stepAnimation : Time -> Maybe AnimationState -> Sprite -> Sprite
stepAnimation t animation (Sprite s) =
    case animation of
        Nothing ->
            Sprite s
        Just (AnimationState anim) ->
           case anim.animationType of
                Rotation ->
                    Sprite { s | rot <- ease anim.easing float anim.startVal anim.endVal anim.len anim.elapsedTime 
                    }
                XPos ->
                    Sprite { s | x <-  ease anim.easing float anim.startVal anim.endVal anim.len anim.elapsedTime
                    }
                YPos ->
                    Sprite { s | y <-  ease anim.easing float anim.startVal anim.endVal anim.len anim.elapsedTime
                    }
                _ -> Sprite s

animLength = 1500
animAmount = 90

goblinImg = toForm (image 16 16 "assets/goblin_run_down.gif")
backgroundImg = toForm (tiledImage screenWidth screenHeight "assets/sand_1.png")

button = Sprite
         { shape = goblinImg
         , rot = 0
         , x = -250
         , y = 150
         , animations = [Just (AnimationState 
                                 { elapsedTime = 0
                                 , delay = 250
                                 , startVal = 0
                                 , easing = easeOutCirc
                                 , len = 750
                                 , animationType = Rotation
                                 , endVal = 360
                                 }),
                         Just (AnimationState
                                 { elapsedTime = 0
                                 , startVal = -250
                                 , delay = 0
                                 , easing = easeOutCirc
                                 , len = 2000
                                 , animationType = XPos
                                 , endVal = 350}),
                         Just (AnimationState
                                 { elapsedTime = 0
                                 , len = 2000
                                 , delay = 0
                                 , easing = easeOutCirc
                                 , startVal = 150
                                 , animationType = YPos
                                 , endVal = -150})
                        ]
         }

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

update : Action -> Sprite -> Sprite
update action (Sprite sprite) =
    case action of
        Tick t ->
            let
                anims = List.map (tickAnim t) sprite.animations
                (Sprite newSprite) = List.foldr (stepAnimation t) (Sprite sprite) sprite.animations 
            in
                Sprite {newSprite | animations <- anims}
        Spin -> 
            if not (allNothing sprite.animations) then (Sprite sprite) else Sprite { sprite | animations <- [Just (AnimationState 
                                 { elapsedTime = 0
                                 , startVal = sprite.rot
                                 , delay = 100
                                 , easing = easeOutCubic
                                 , len = 1500
                                 , animationType = Rotation
                                 , endVal = -sprite.rot
                                 }),
                         Just (AnimationState
                                 { elapsedTime = 0
                                 , startVal = sprite.x
                                 , delay = 0
                                 , easing = easeOutCubic
                                 , len = 2000
                                 , animationType = XPos
                                 , endVal = -sprite.x}),
                         Just (AnimationState
                                 { elapsedTime = 0
                                 , len = 2000
                                 , delay = 0
                                 , startVal = sprite.y
                                 , easing = easeOutBounce
                                 , animationType = YPos
                                 , endVal = -sprite.y})
                        ]}

allNothing : List (Maybe a) -> Bool
allNothing l =
    let
        flist = List.filter (\m -> case m of 
                                       Nothing -> False 
                                       _ -> True) l
    in
        (List.length flist) == 0

view : Sprite -> Element
view (Sprite s) = collage screenWidth screenHeight 
                    [ backgroundImg
                    , s.shape 
                        |> rotate (degrees s.rot)
                        |> moveX s.x
                        |> moveY s.y
                    ]   

ticks = map Tick (fps 60)
keys = map (\_ -> Spin) Keyboard.space
streams = merge ticks keys

main = view <~ foldp update button streams
