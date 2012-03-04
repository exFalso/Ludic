{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeOperators, GADTs #-}

import WorldState
import Vec

import Prelude hiding (foldl1)
import Graphics.Gloss
import Graphics.Gloss.Interface.Game
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Foldable

import Debug.Trace

_AGENT_MASS = 5
_AGENT_RADIUS = 20

_TARGET_RADIUS :: Float
_TARGET_RADIUS = 5

_INIT_WORLD :: WorldState
_INIT_WORLD = WorldState { target = 100 :> 100 :> VecZero 
                         , ag = agent
                                _AGENT_MASS
                                _AGENT_RADIUS
                                (pure 0)
                                seek
                         }

-- fps
_SIM_RESOLUTION :: Int
_SIM_RESOLUTION = 60

_SPRING_CO :: Float
_SPRING_CO = 0.2

_FRICTION_CO :: Float
_FRICTION_CO = 1

_RESISTANCE_CO :: Float
_RESISTANCE_CO = 10

_G :: Float
_G = 9.8

d2 :: Vec Float -> Float
d2 v = foldl1 (+) $ (^ 2) <$> v

_ROUND_THRESHOLD :: Float
_ROUND_THRESHOLD = 0.000001

normalise :: Vec Float -> Vec Float
normalise v = let s2 = d2 v in
    if s2 < _ROUND_THRESHOLD
    then pure 0
    else (/ sqrt s2) <$> v

cap :: Float -> Float -> Float
cap mx f = if f > mx then mx else f

seek :: [Behaviour]
seek = [spring, friction, resistance]

-- spring force vector
spring :: Behaviour
spring Agent { position = pos 
             , mass = m }
    = do
    WorldState { target = tar } <- ask
    return $ (_SPRING_CO *) . (\x -> abs x * x) <$> liftA2 (-) tar pos

-- friction force vector (constant slow down)
friction :: Behaviour
friction Agent { velocity = vel        
               , mass = m }
    = return $ ((-_FRICTION_CO) * m * _G *) <$> normalise vel

-- resistance force vector (cap speed)
resistance :: Behaviour
resistance Agent { velocity = vel }
    = return $ ((-_RESISTANCE_CO) *) <$> vel        

update :: Float -> WorldS ()
update dt
    = do
        w@WorldState { ag = a@Agent { mass = m
                                    , position   = pos
                                    , velocity   = vel
                                    , behaviours = bs } } <- get
        let
            forces = runWorldR w . sequence . map ($ a) $ bs
            forceSum = foldl1 (liftA2 (+)) forces
            force = (/ fromIntegral (length forces)) <$> forceSum 
            dpos = (dt *) <$> vel
            dvel = (dt / m *) <$> force
        put w { ag = a { position = liftA2 (+) pos dpos
                       , velocity = liftA2 (+) vel dvel } }

main :: IO ()
main
    = play
      (InWindow "SeekDemo" (600, 600) (10, 10))
      white
      _SIM_RESOLUTION
      _INIT_WORLD
      drawWorld
      handleEvent
      (\dt w -> execWorldS w (update dt))

handleEvent :: Event -> WorldState -> WorldState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) w = w { target = x :> y :> VecZero }
handleEvent _ w = w

drawWorld :: WorldState -> Picture
drawWorld w@WorldState { target = tx :> ty :> _
                       , ag = Agent
                         { position = ax :> ay :> _
                         , radius = r
                         } }
    = Pictures [agentPic, targetPic] --, debugPic]
  where
    agentPic = Translate ax ay $ Circle r
    targetPic = Translate tx ty $ Circle _TARGET_RADIUS
    --debugPic = Translate (-340) 0 $ Scale 0.1 0.1 $ Text (show w)
