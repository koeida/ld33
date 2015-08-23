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

goblinImg s = toForm (image 16 16 "assets/goblin_run_down.gif")
playerImg s = toForm (image 32 32 "assets/player_run.gif")
humanImg s = 
    case s of
        Normal -> toForm (image 32 32 "assets/human_walking.gif")
        Attacking -> toForm (image 64 32 "assets/human_attacking.gif") 
        Fleeing -> toForm (image 32 32 "assets/human_walking.gif")
backgroundImg = toForm (tiledImage screenWidth screenHeight "assets/sand_1.png")

startingPlayer = 
    { shape = playerImg
    , rot = 0
    , kind = Player
    , target = Nothing
    , following = Nothing
    , nextFollow = 0
    , nextAttack = 0
    , attackSpeed = 0
    , stress = 0
    , state = Normal
    , moving = True
    , pos = {x = 200, y = 100}
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
       , stress = 0
       , state = Normal
       , following = Nothing
       , attackSpeed = 1000
       , nextAttack = 0
       , nextFollow = 0
       , target = Nothing
       , pos = {y = ypos , x = xpos }
       , animations = []
       }


goblin seed = 
    let 
        (xpos,seed') = Random.generate (Random.float 100.0 200.0) seed
        (ypos,seed'') = Random.generate (Random.float -400.0 400.0) seed'
        (hasTarget, seed''') = Random.generate (Random.float 0 100) seed''
        (targx,seed'''') = Random.generate (Random.float 400 500) seed'''
        (targy,seed''''') = Random.generate (Random.float -300 300) seed'''
    in 
         { shape = goblinImg
         , rot = 0
         , kind = if hasTarget > 90 then AlphaGoblin else Goblin
         , moving = True
         , stress = 0
         , state = Normal
         , following = Nothing
         , nextAttack = 0
         , attackSpeed = 1000
         , nextFollow = 250
         , target = if hasTarget > 90 then Just {x = targx, y = targy} else Nothing
         , pos = {x = xpos,y = ypos}
         , animations = []
         }

animJumpFlip s height time =
    if not (allNothing s.animations)
       then []
       else
        [
        Just (AnimationState 
                 { elapsedTime = 0
                 , delay = 0
                 , startVal = s.pos.x
                 , easing = easeOutCirc --easeOutLinear
                 , len = time / 2
                 , animationType = XPos
                 , endVal = s.pos.x + height
                 }),
         Just (AnimationState 
                 { elapsedTime = 0
                 , delay = time / 2
                 , startVal = s.pos.x + height
                 , easing = easeOutCirc --easeOutLinear
                 , len = time / 2
                 , animationType = XPos
                 , endVal = s.pos.x
                 }),
        Just (AnimationState 
                 { elapsedTime = 0
                 , delay = 0
                 , startVal = s.pos.y
                 , easing = easeOutCirc --easeOutLinear
                 , len = time / 2
                 , animationType = YPos
                 , endVal = s.pos.y + height
                 }),
         Just (AnimationState 
                 { elapsedTime = 0
                 , delay = time / 2
                 , startVal = s.pos.y + height
                 , easing = easeOutCirc --easeOutLinear
                 , len = time / 2
                 , animationType = YPos
                 , endVal = s.pos.y
                 }),
         Just (AnimationState 
                 { elapsedTime = 0
                 , delay = 0
                 , startVal = s.rot
                 , easing = easeOutCirc --easeOutLinear
                 , len = time
                 , animationType = Rotation
                 , endVal = s.rot + 359
                 })]
                        

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
                minDist = 10
                xmod = if (abs (target.x - s.pos.x)) > minDist
                          then getMotionMod s.pos.x target.x
                          else 0
                ymod = if (abs (target.y - s.pos.y)) > minDist
                          then getMotionMod s.pos.y target.y
                          else 0
                pos = s.pos
                pos' = {pos | x <- pos.x + xmod, y <- pos.y + ymod}
                --target = if s.pos.x == s.target.x && s.pos.y = s.target.y
            in
               {s | pos <- pos'
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
                minDistance = 100
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

updateHuman : Time -> World -> Sprite -> Sprite
updateHuman t world human =
    case human.state of
        Normal ->
            let 
                pos = human.pos 
                pos' = {pos | x <- pos.x - (0.01 * t)}
                nearbyGobs = List.any (\g -> (distance g human) < 32) world.goblins 
                state' = if nearbyGobs then Attacking else human.state
            in
                {human | pos <- pos', state <- state'}
        Attacking ->
            human
        Fleeing ->
            let 
                pos = human.pos 
                pos' = {pos | x <- pos.x + (0.01 * t)}
            in
                {human | pos <- pos'}

updateGoblin : Time -> World -> Sprite -> Sprite
updateGoblin t world g =
    case g.state of
        Normal ->
            let 
                (rfloat, seed') = Random.generate (Random.float 0 10) world.seed
                nearbyHumans = List.any (\h -> (distance g h) < 16) world.humans 
                state' = if nearbyHumans then Attacking else g.state
                g' = g |> ((tickSprite t) << 
                           (herding world.player) <<
                           (getNewTarget seed' t world.goblins))
            in 
               {g' | state <- state'}
        Attacking ->
            let
                attackDelay = 500
                nextAttack' = g.nextAttack - (0.1 * t)
                attackNow = nextAttack' <= 0
                (hitGen,seed'') = Random.generate (Random.float 0 100) (Random.initialSeed (round (g.pos.x + g.pos.y + t)))
                successfulHit = hitGen > 75
                g' = {g | stress <- g.stress + (0.1 * t),
                          nextAttack <- if attackNow 
                                           then attackDelay 
                                           else nextAttack',
                          animations <- if attackNow
                                           then g.animations ++ (animJumpFlip g 16 500) 
                                           else g.animations
                     }
            in
               g'
        Fleeing ->
            g
        Dead ->
            g

updateAnims t s =
    let 
        animations' = List.map (tickAnim t) s.animations
        s' = List.foldr stepAnimation s animations'
    in 
       { s' | animations <- animations' } 

update : Event -> World -> World
update event world =
    case event of
        NewFrame t -> 
            let 
                (rfloat, seed') = Random.generate (Random.float 0 10) world.seed
                goblins' = List.map (updateGoblin t world << updateAnims t) world.goblins 
                humans' = List.map (updateHuman t world << updateAnims t) world.humans
            in
               { world | goblins <- goblins', seed <- seed', humans <- humans' }


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
        shapes = List.map (\s -> (s.shape s.state)
                        |> rotate (degrees s.rot)
                        |> moveX s.pos.x
                        |> moveY s.pos.y) world.goblins
        humans = List.map (\h -> 
            case h.state of
                Normal ->
                    (h.shape h.state)
                            |> rotate (degrees h.rot)
                            |> moveX h.pos.x
                            |> moveY h.pos.y
                Attacking ->
                    (h.shape h.state)
                            |> rotate (degrees h.rot)
                            |> moveX h.pos.x
                            |> moveY h.pos.y) world.humans
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
        player = (world.player.shape world.player.state)
            |> moveX world.player.pos.x 
            |> moveY world.player.pos.y
        debug = False
    in
       if debug
          then collage screenWidth screenHeight 
                        ([ backgroundImg ] ++ shapes ++ [player] ++ humans ++ targets ++ follows) 
          else collage screenWidth screenHeight ([backgroundImg] ++ humans ++ shapes ++ [player])



keyinput : Signal Event
keyinput = 
    let 
        keySignal = map2 (\a s -> Keys {x = a.x, y = a.y, space = s}) Keyboard.arrows Keyboard.space
    in
        sampleOn (every <| 10 * millisecond) keySignal

ticks = NewFrame <~ (fps 60)
streams = merge ticks keyinput



startingGoblins = List.map (\x -> goblin (Random.initialSeed x)) [1..50]
startingHumans = List.map (\x -> human (Random.initialSeed x)) [200..225]
startWorld = { goblins = startingGoblins, player = startingPlayer, humans = startingHumans, seed = Random.initialSeed 200 } 

main = view <~ foldp update startWorld streams
