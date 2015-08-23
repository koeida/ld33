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

goblinImg s = 
    case s of --
        Normal -> toForm (image 16 16 "assets/goblin_run_down.gif")
        Attacking -> toForm (image 16 16 "assets/goblin_run_down.gif")
        Fleeing -> toForm (image 16 16 "assets/goblin_run_down.gif")
        Dead -> toForm (image 16 16 "assets/goblin_dead.gif")

goblinSpawnImg s = toForm (image 32 32 "assets/gobspawn.gif") 

archerImg s = toForm (image 32 32 "assets/archer_shoot.gif")

playerImg s = toForm (image 32 32 "assets/player_run.gif")
humanImg s = 
    case s of
        Normal -> toForm (image 32 32 "assets/human_walking.gif")
        Attacking -> toForm (image 64 32 "assets/human_attacking.gif") 
        Fleeing -> toForm (image 32 32 "assets/human_walking.gif")
        Dead -> toForm (image 32 32 "assets/human_dead.gif")

backgroundImg = toForm (tiledImage screenWidth screenHeight "assets/sand_1.png")

startingPlayer = 
    { shape = playerImg
    , rot = 0
    , kind = Player
    , target = Nothing
    , following = Nothing
    , nextFollow = 0
    , nextAttack = 1000
    , scale = 1
    , attackSpeed = 0
    , stress = 0
    , state = Normal
    , moving = True
    , pos = {x = 10, y = 100}
    , animations = []
    }

makeSpawnPoint x y =
    { shape = goblinSpawnImg
    , rot = 0
    , kind = Building
    , target = Nothing
    , following = Nothing
    , nextFollow = 0
    , nextAttack = 1000
    , scale = 1
    , attackSpeed = 0
    , stress = 0
    , state = Normal
    , moving = True
    , pos = {x = x, y = y}
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
       , scale = 1
       , attackSpeed = 250
       , nextAttack = 250
       , nextFollow = 0
       , target = Nothing
       , pos = {y = ypos , x = xpos }
       , animations = []
       }

goblinSpawner seed x y = 
    let 
        (xpos,seed') = Random.generate (Random.float -20.0 20.0) seed
        (ypos,seed'') = Random.generate (Random.float -20.0 20.0) seed'
        (hasTarget, seed''') = Random.generate (Random.float 0 100) seed''
        (targx,seed'''') = Random.generate (Random.float 400 500) seed'''
        (targy,seed''''') = Random.generate (Random.float -300 300) seed'''
    in 
         ({ shape = goblinImg
         , rot = 0
         , kind = if hasTarget > 90 then AlphaGoblin else Goblin
         , moving = True
         , stress = 0
         , state = Normal
         , following = Nothing
         , scale = 1
         , nextAttack = 250
         , attackSpeed = 250
         , nextFollow = 250
         , target = if hasTarget > 90 then Just {x = targx, y = targy} else Nothing
         , pos = {x = x + xpos,y = y + ypos}
         , animations = [
             Just (AnimationState 
                 { elapsedTime = 0
                 , delay = 0
                 , startVal = 4
                 , easing = easeOutBounce --easeOutLinear
                 , len = 1500 
                 , animationType = Scale
                 , endVal = 1
                 })]
         },seed''''')

goblin seed = 
    let 
        (xpos,seed') = Random.generate (Random.float -400.0 0.0) seed
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
         , scale = 1
         , state = Normal
         , following = Nothing
         , nextAttack = 250
         , attackSpeed = 250
         , nextFollow = 250
         , target = if hasTarget > 90 then Just {x = targx, y = targy} else Nothing
         , pos = {x = xpos,y = ypos}
         , animations = [] 
         }

animDie height dist time s =
    let 
        rot = if s.kind == Warrior then -90 else 90
    in
        [ Just (AnimationState 
                 { elapsedTime = 0
                 , delay = 0
                 , startVal = s.pos.x
                 , easing = easeOutCirc --easeOutLinear
                 , len = time 
                 , animationType = XPos
                 , endVal = s.pos.x - dist
                 })
        , Just (AnimationState 
                 { elapsedTime = 0
                 , delay = 0
                 , startVal = s.pos.y
                 , easing = easeOutBounce --easeOutLinear
                 , len = time 
                 , animationType = YPos
                 , endVal = s.pos.y + height
                 })
        , Just (AnimationState 
                 { elapsedTime = 0
                 , delay = 0
                 , startVal = s.rot
                 , easing = easeOutCirc
                 , len = time 
                 , animationType = Rotation
                 , endVal = s.rot + rot
                 })
        ]

animJumpFlip s height time =
        [ Just (AnimationState 
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
               {s | pos <- pos' }

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


getNearestTo : Float -> Float -> List Sprite -> Sprite -> Maybe {d:Float,s:Sprite}
getNearestTo minDistance maxDistance sprites s =
    sprites 
        |> List.filter .moving  
        |> List.filter (\t -> t.state /= Dead)
        |> List.map (\t -> {d = distance s t, s = t})
        |> List.filter (\t -> t.d > 0 && t.d > minDistance && t.d < maxDistance)
        |> List.sortBy .d
        |> List.head

--Find the closest moving goblin and go toward it 
getNewFollow : Random.Seed -> Time -> List Sprite -> Sprite -> Sprite
getNewFollow seed t gs g = 
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
                maxDistance = 2000
                newTarget = getNearestTo minDistance maxDistance gs g
            in
               case newTarget of
                   Nothing -> { g | following <- Nothing, moving <- False }
                   Just {d,s} -> 
                       let
                           (xmod,ymod) = getMods g.pos.x g.pos.y s.pos.x s.pos.y 16
                           pos = g.pos
                           pos' = {pos | x <- pos.x + xmod, y <- pos.y + ymod}
                           (next,seed') = Random.generate (Random.float 2000 4000) (Random.initialSeed (round (t + g.pos.x + g.pos.y))) 
                       in
                          {g | pos <- pos', following <- Just {x = s.pos.x, y = s.pos.y}, nextFollow <- next}
                  
        


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
        pos' = { pos | x <- pos.x + (xmod * 3),
                       y <- pos.y + (ymod * 3)}
    in
       if d < minDistance 
          then { goblin | pos <- pos'}
          else goblin

getNewTarget : Float -> Float -> List Sprite -> Sprite -> Maybe Model.Position 
getNewTarget min max ss s = 
   let 
       newTarget = getNearestTo min max ss s
   in
       case newTarget of
           Nothing -> Nothing
           Just {d,s} -> Just {x = s.pos.x, y = s.pos.y} 

updateHuman : Time -> World -> Sprite -> Sprite
updateHuman t world human =
    case human.state of
        Normal ->
            let 
                pos = human.pos 
                pos' = if human.target == Nothing then
                          {pos | x <- pos.x - (0.02 * t)}
                       else pos
                nearbyGobs = nearby 32 human (world.goblins ++ world.goblinSpawns ++ [world.player]) 
                target' = getNewTarget 0 400 world.goblinSpawns human 
                state' = if nearbyGobs then Attacking else Normal
                human' = {human | target <- target', pos <- pos', state <- state'}
            in
               human' |> tickSprite t
        Attacking ->
            let
                nextAttack' = human.nextAttack - (0.1 * t)
                attackNow = nextAttack' <= 0
                nearbyGobs = nearby 32 human (world.goblins ++ world.goblinSpawns ++ [world.player])
                state' = if nearbyGobs then Attacking else Normal
            in
               {human | nextAttack <- if attackNow 
                                         then human.attackSpeed
                                         else nextAttack',
                        state <- state'}        
        Fleeing ->
            let 
                pos = human.pos 
                pos' = {pos | x <- pos.x + (0.01 * t)}
            in
                {human | pos <- pos'}
        Dead ->
            human

nearby maxDistance s ss =
    List.any (\e -> (distance s e) < maxDistance &&
                    e.state /= Dead) ss

updateGoblin : Time -> World -> Sprite -> Sprite
updateGoblin t world g =
    case g.state of
        Normal ->
            let 
                (rfloat, seed') = Random.generate (Random.float 0 10) world.seed
                nearbyHumans = nearby 32 g world.humans
                state' = if nearbyHumans then Attacking else g.state
                g' = g |> ((tickSprite t) << 
                           (herding world.player) <<
                           (getNewFollow seed' t world.goblins))
            in 
               {g' | state <- state'}
        Attacking ->
            let
                nearbyHumans = nearby 32 g world.humans 
                state' = if nearbyHumans then Attacking else Normal
                nextAttack' = g.nextAttack - (0.1 * t)
                attackNow = nextAttack' <= 0
                g' = {g | stress <- g.stress + (0.1 * t),
                          nextAttack <- if attackNow 
                                           then g.attackSpeed
                                           else nextAttack',
                          animations <- if attackNow
                                           then g.animations ++ (animJumpFlip g 16 g.attackSpeed) 
                                           else g.animations,
                          state <- state'
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

attack : Time -> List Sprite -> Sprite -> List Sprite
attack t enemies attacker =
    if attacker.nextAttack /= attacker.attackSpeed || attacker.state /= Attacking then [] else
        let
            attackNow = attacker.nextAttack == attacker.attackSpeed
            (hitGen,seed'') = Random.generate (Random.float 0 100) (Random.initialSeed (round (attacker.pos.x + attacker.pos.y + t)))
            chanceToHit = if
                | attacker.kind == Goblin -> 20
                | attacker.kind == Warrior -> 50
                | attacker.kind == AlphaGoblin -> 25
            successfulHit = hitGen < chanceToHit
            livingEnemies = List.filter (\x -> case x.state of 
                                                    Dead -> False
                                                    _ -> True) enemies 
            nearestEnemy = getNearestTo 0 32 livingEnemies attacker 
        in
           case nearestEnemy of
               Nothing -> []
               Just {d,s} -> if successfulHit
                    then List.filter (\e -> e.pos.x == s.pos.x && e.pos.y == s.pos.y) enemies
                    else []


kill deadList sprites entityKind =
    let 
        deathAnim = case entityKind of 
            Goblin -> animDie 50 50 1000  
            _ -> animDie 50 -50 1000 
    in 
        mapWhen (\s -> List.any (\dead -> (dead.pos.x == s.pos.x) && (dead.pos.y == s.pos.y)) deadList )
                (\s -> {s | state <- Dead, animations <- s.animations ++ (deathAnim s) })
                sprites

update : Event -> World -> World
update event world =
    case event of
        NewFrame t -> 
            let 
                (rfloat, seed') = Random.generate (Random.float 0 10) world.seed
                goblins' = world.goblins
                    |> List.map (updateGoblin t world << updateAnims t)
                humans' = world.humans
                    |> List.map (updateHuman t world << updateAnims t)
                deadGoblins = List.map (attack t goblins') humans' |> List.concat
                deadHumans = List.map (attack t humans') goblins' |> List.concat
                deadPlayer = List.concatMap (attack t [world.player]) humans'
                deadSpawns = List.concatMap (attack t world.goblinSpawns) humans'
                humans'' = kill deadHumans humans' Warrior
                goblins'' = kill deadGoblins goblins' Goblin
                spawns' = kill deadSpawns world.goblinSpawns Building
                spawns'' = List.map (updateAnims t) spawns'
                player = world.player |> updateAnims t
                player' = if (List.length deadPlayer) > 0
                             then {player | state <- Dead,
                                            animations <- player.animations ++ (animDie 50 50 1000 player)}
                             else player
            in
               { world | goblinSpawns <- spawns'', player <- player', goblins <- goblins'', seed <- seed', humans <- humans'' }
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
               if world.player.state == Dead then world else {world | player <- p'}
        SpawnGoblin ->
            if not (List.any (\s -> s.state /= Dead) world.goblinSpawns)
               then world
               else
                    let
                        spawners = List.filter (\s -> s.state /= Dead) world.goblinSpawns
                    
                        (spawnerId,seed') = Random.generate (Random.int 0 ((List.length spawners) - 1)) world.seed
                        spawner = nth spawnerId spawners
                        (newGoblin,seed'') = goblinSpawner world.seed spawner.pos.x spawner.pos.y
                    in
                        { world | goblins <- world.goblins ++ [newGoblin], seed <- seed'' }

nth n l = case l of
    x::xs -> if n == 0 then x else nth (n - 1) xs 


view : World -> Element
view world = 
    let 
        shapes = List.map (\s -> (s.shape s.state)
                        |> rotate (degrees s.rot)
                        |> scale s.scale
                        |> moveX s.pos.x
                        |> moveY s.pos.y) (world.goblinSpawns ++ world.goblins ++ world.humans)

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
            |> rotate (degrees world.player.rot)
            |> moveX world.player.pos.x 
            |> moveY world.player.pos.y
        debug = True
        debugStuff = if debug then targets ++ follows else []
    in
          collage screenWidth screenHeight ([backgroundImg] ++ debugStuff ++ shapes ++ [player])



keyinput : Signal Event
keyinput = 
    let 
        keySignal = map2 (\a s -> Keys {x = a.x, y = a.y, space = s}) Keyboard.arrows Keyboard.space
    in
        sampleOn (every <| 10 * millisecond) keySignal

spawnGoblin : Signal Event
spawnGoblin = map (\_ -> SpawnGoblin) (every <| 2 * second)

ticks = NewFrame <~ (fps 60)
streams = mergeMany [ticks, keyinput, spawnGoblin]

startingGoblins = List.map (\x -> goblin (Random.initialSeed x)) [1..25]
startingHumans = List.map (\x -> human (Random.initialSeed x)) [200..225]
startWorld = { goblinSpawns = [makeSpawnPoint -200 0,makeSpawnPoint -200 200, makeSpawnPoint -200 -200], goblins = startingGoblins, player = startingPlayer, humans = startingHumans, seed = Random.initialSeed 200 } 

main = view <~ foldp update startWorld streams
