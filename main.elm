module Main where
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
humanImg = toForm (image 32 32 "assets/human_walking.gif")
backgroundImg = toForm (tiledImage screenWidth screenHeight "assets/sand_1.png")


startingPlayer = 
    { shape = playerImg
    , rot = 0
    , kind = Player
    , target = Nothing
    , following = Nothing
    , nextFollow = 0
    , moving = True
    , pos = {x = 100, y = 100}
    , animations = []
    }

human seed =
    let
        (xpos,seed') = Random.generate (Random.float 400 500.0) seed
        (ypos,seed'') = Random.generate (Random.float -330.0 330.0) seed'
        (hasTarget, seed''') = Random.generate (Random.float 0 100) seed''
        (targx,seed'''') = Random.generate (Random.float -500 500) seed'''
        (targy,seed''''') = Random.generate (Random.float -300 300) seed'''
    in
       { shape = humanImg
       , rot = 0
       , kind = Warrior
       , moving = True
       , following = Nothing
       , nextFollow = 0
       , target = Nothing
       , pos = {y = ypos , x = xpos }
       , animations = []
       }


goblin seed = 
    let 
        (xpos,seed') = Random.generate (Random.float -50.0 150.0) seed
        (ypos,seed'') = Random.generate (Random.float -50.0 150.0) seed'
        (hasTarget, seed''') = Random.generate (Random.float 0 100) seed''
        (targx,seed'''') = Random.generate (Random.float -500 500) seed'''
        (targy,seed''''') = Random.generate (Random.float -300 300) seed'''
    in 
         { shape = goblinImg
         , rot = 0
         , kind = if hasTarget > 90 then AlphaGoblin else Goblin
         , moving = True
         , following = Nothing
         , nextFollow = 250
         , target = if hasTarget > 90 then Just {x = targx, y = targy} else Nothing
         , pos = {x = -50 + (xpos * 16),y = ypos}
         , animations = []
                        --[Just (AnimationState 
                        --         { elapsedTime = 0
                        --         , delay = 0
                        --         , startVal = ypos
                        --         , easing = Easing.linear --easeOutLinear
                        --         , len = 10000
                        --         , animationType = YPos
                        --         , endVal = ypos - 200
                        --         })
                        --]
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
    case s.target of
        Nothing -> s
        Just target ->
            let
                animations' = List.map (tickAnim t) s.animations
                s' = List.foldr stepAnimation s animations'
                minDist = 10
                xmod = if (abs (target.x - s.pos.x)) > minDist
                          then getMotionMod s.pos.x target.x
                          else 0
                ymod = if (abs (target.y - s.pos.y)) > minDist
                          then getMotionMod s.pos.y target.y
                          else 0
                pos = s'.pos
                pos' = {pos | x <- pos.x + xmod, y <- pos.y + ymod}
                --target = if s.pos.x == s.target.x && s.pos.y = s.target.y
            in
               { s' | animations <- animations' 
                    , pos <- pos'
               }

getMods x1 y1 x2 y2 minDist = 
    let
        xmod = if (abs (x1 - x2)) > minDist
                  then getMotionMod x1 x2
                  else 0
        ymod = if (abs (y1 - y2)) > minDist
                  then getMotionMod y1 y2
                  else 0
    in
       (xmod,ymod)

--Find the closest moving goblin and go toward it 
getNewTarget : Random.Seed -> Time -> List Sprite -> Sprite -> Sprite
getNewTarget seed t gs g = 
    if g.nextFollow >= 0 
       then
            let 
                pos' = g.pos
                (xmod,ymod) = case g.following of
                    Nothing -> (0,0)
                    Just {x,y} -> getMods g.pos.x g.pos.y x y 16
            in
                {g | nextFollow <- g.nextFollow - t,
                     pos <- {pos' | x <- pos'.x + xmod,
                                    y <- pos'.y + ymod}}
       else
        if g.kind == AlphaGoblin then g else
            let
                minDistance = 30
                newTarget = gs 
                            |> List.filter .moving
                            |> List.map (\s -> {d = distance g s,
                                                x = s.pos.x,
                                                y = s.pos.y})
                            |> List.filter (\s -> s.d > 0 && s.d > minDistance)
                            |> List.sortBy .d
                            |> List.head
            in
               case newTarget of
                   Nothing -> { g | following <- Nothing, moving <- False }
                   Just {d,x,y} -> 
                       let
                           (xmod,ymod) = getMods g.pos.x g.pos.y x y 16
                           pos = g.pos
                           pos' = {pos | x <- pos.x + xmod, y <- pos.y + ymod}
                           (next,seed') = Random.generate (Random.float 2000 4000) (Random.initialSeed (round (t + g.pos.x + g.pos.y))) 
                           foo = Debug.watch "gs" gs
                       in
                          {g | pos <- pos', following <- Just {x = x, y = y}, nextFollow <- next}
                  
        


distance s1 s2 = 
    let
        a = abs ((s2.pos.x) - (s1.pos.x)) 
        b = abs ((s2.pos.y) - (s1.pos.y))
    in
       abs (sqrt ((a ^ 2) + (b ^ 2)))

getMotionMod : Float -> Float -> Float
getMotionMod x y =
    if | x == y -> 0
       | x > y -> -1
       | x < y -> 1

--If goblin within range of player,
--move make goblin start moving away from player
herding : Sprite -> Sprite -> Sprite
herding player goblin =
    let
        d = distance player goblin 
        minDistance = 100
        strength = 5
        xmod = getMotionMod player.pos.x goblin.pos.x
        ymod = getMotionMod player.pos.y goblin.pos.y
        pos = goblin.pos
        pos' = { pos | x <- pos.x + (xmod * 2),
                       y <- pos.y + (ymod * 2)}
    in
       if d < minDistance 
          then { goblin | pos <- pos'}
          else goblin

update : Event -> World -> World
update event world =
    case event of
        NewFrame t -> 
            let 
                (rfloat, seed') = Random.generate (Random.float 0 10) world.seed
                goblins' = List.map ((tickSprite t) << 
                                     (herding world.player) <<
                                     (getNewTarget seed' t world.goblins))
                                      world.goblins 
            in
               { world | goblins <- goblins', seed <- seed' }


        Keys k -> 
            let
                velocity = 5
                p = world.player
                pos = world.player.pos
                pos' = {pos | 
                            x <- pos.x + (toFloat (k.x * velocity)),
                            y <- pos.y + (toFloat (k.y * velocity))}  
                p' = {p | pos <- pos'}
            in
               {world | player <- p'}


view : World -> Element
view world = 
    let 
        shapes = List.map (\s -> s.shape
                        |> rotate (degrees s.rot)
                        |> moveX s.pos.x
                        |> moveY s.pos.y) (world.goblins ++ world.humans)
        targets = List.map (
            \{target,pos} ->
                case target of
                    Just t ->
                       traced (dashed blue) 
                       (path [(pos.x, pos.y), (t.x, t.y)])
                    Nothing ->
                        traced (dashed blue) (path [(pos.x,pos.y),(pos.x,pos.y)])
            ) world.goblins
        follows = List.map (
            \{following,pos} ->
                case following of
                    Just t ->
                       traced (dashed red) 
                       (path [(pos.x, pos.y), (t.x, t.y)])
                    Nothing ->
                        traced (dashed red) (path [(pos.x,pos.y),(pos.x,pos.y)])
            ) world.goblins
        player = world.player.shape 
            |> moveX world.player.pos.x 
            |> moveY world.player.pos.y
        debug = False
    in
       if debug
          then collage screenWidth screenHeight 
                        ([ backgroundImg ] ++ shapes ++ [player] ++ targets ++ follows) 
          else collage screenWidth screenHeight ([backgroundImg] ++ shapes ++ [player])



keyinput : Signal Event
keyinput = 
    let 
        keySignal = map2 (\a s -> Keys {x = a.x, y = a.y, space = s}) Keyboard.arrows Keyboard.space
    in
        sampleOn (every <| 10 * millisecond) keySignal

ticks = NewFrame <~ (fps 60)
streams = merge ticks keyinput

startingGoblins = List.map (\x -> goblin (Random.initialSeed x)) [1..200]
startingHumans = List.map (\x -> human (Random.initialSeed x)) [200..250]
startWorld = { goblins = startingGoblins, player = startingPlayer, humans = startingHumans, seed = Random.initialSeed 200 } 

main = view <~ foldp update startWorld streams
