{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WorldState where

import Vec

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

data WorldState =
    WorldState { target :: Vec Float
               , ag :: Agent}
    deriving (Show)

newtype WorldS a = WorldS (State WorldState a)
                 deriving (Functor, Monad, MonadState WorldState)

newtype WorldR a = WorldR (Reader WorldState a)
                 deriving (Functor, Monad, MonadReader WorldState)

runWorldS :: WorldState -> WorldS a -> (a, WorldState)
runWorldS w (WorldS s) = runState s w

execWorldS :: WorldState -> WorldS a -> WorldState
execWorldS w = snd . runWorldS w

runWorldR :: WorldState -> WorldR a -> a
runWorldR w (WorldR s) = runReader s w


type Behaviour = Agent -> WorldR (Vec Float)

data Agent
    = Agent { mass :: Float
            , radius :: Float
            , position :: Vec Float
            , velocity :: Vec Float
            , behaviours :: [Behaviour]
              -- acceleration & force not needed because they are not part of state
            }

instance Show Agent where
    show Agent { position = pos } = "Agent " ++ show pos

agent :: Float -> Float -> Vec Float -> [Behaviour] -> Agent
agent m r p bs = Agent { mass = m
                       , radius = r
                       , position = p
                       , velocity = pure 0
                       , behaviours = bs }
