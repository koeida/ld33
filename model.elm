module Model where
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List
import Time exposing (..)
import Easing exposing (..)
import Random exposing (..)
import Debug exposing (..)

type alias World = 
    { player : Sprite
    , goblins : List Sprite
    , goblinSpawns : List Sprite
    , humans : List Sprite
    , seed : Random.Seed
    }

type alias KeyInput = 
    { x : Int
    , y : Int
    , space: Bool }

type Event = Keys KeyInput | SpawnGoblin  | NewFrame Float  

type alias Position = {x: Float, y: Float}

type alias Sprite = 
    { shape : State -> Form
    , rot : Float
    , animations : List (Maybe AnimationState)
    , pos : Position 
    , target : Maybe Position
    , following : Maybe Position
    , nextFollow : Time
    , attackSpeed : Time
    , scale : Float
    , nextAttack : Time
    , moving : Bool
    , stress : Float 
    , state : State
    , kind : EntityKind
    }

type EntityKind = Player | Goblin | AlphaGoblin | Warrior | Building 

type State = Normal | Attacking | Fleeing | Dead

type AnimationType 
    = Rotation 
    | XPos 
    | YPos
    | Scale

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
               foo = Debug.watch "rot" s.rot
           in
               case anim.animationType of
                    Rotation ->
                        { s | rot <- ease anim.easing Easing.float anim.startVal anim.endVal anim.len anim.elapsedTime 
                        }
                    XPos ->
                        { s | pos <- {pos | x <-  ease anim.easing Easing.float anim.startVal anim.endVal anim.len anim.elapsedTime}
                        }
                    YPos ->
                        { s | pos <- {pos | y <-  ease anim.easing Easing.float anim.startVal anim.endVal anim.len anim.elapsedTime}}
                    Scale ->
                        { s | scale <- ease anim.easing Easing.float anim.startVal anim.endVal anim.len anim.elapsedTime}
                    _ -> s
