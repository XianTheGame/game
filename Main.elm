module Main where

import Window
import Maybe
import Keyboard
import Char
import String

import Fairy
import Immortal
import Hero

-- General options

frameRate = 25
keyCodeA = Char.toCode 'w'
keyCodeB = Char.toCode 'd'

--  Part 1: Model the user input ----------------------------------------------

-- What information do you need to represent all relevant user input?

-- Task: Redefine `UserInput` to include all of the information you need.
--       Redefine `userInput` to be a signal that correctly models the user
--       input as described by `UserInput`.

type Dir = { x : Int, y : Int}

data Button = A | B

type UserInput = {dir : Dir, action : Maybe Button}

getAction : Bool -> Bool -> Maybe Button
getAction a b = 
    case (a, b) of
      (True, _) -> Just A
      (_, True) -> Just B
      (_, _) -> Nothing

userInput : Signal UserInput
userInput = lift3 (\pos a b -> {dir = pos, action = getAction a b}) 
            Keyboard.arrows (Keyboard.isDown keyCodeA) (Keyboard.isDown keyCodeB)

type Input = { timeDelta:Float, userInput:UserInput }


-- Part 2: Model the game ----------------------------------------------------

-- What information do you need to represent the entire game?

-- Tasks: Redefine `GameState` to represent your particular game.
--        Redefine `defaultGame` to represent your initial game state.

-- For example, if you want to represent many objects that just have a position,
-- your GameState might just be a list of coordinates and your default game might
-- be an empty list (no objects at the start):

--     type GameState = { objects : [(Float,Float)] }
--     defaultGame = { objects = [] }



type GameState = {fairy : Fairy.World, immortal : Immortal.World, hero : Hero.State}

defaultGame : GameState
defaultGame = {fairy = Fairy.default, immortal = Immortal.default, hero = Hero.default}


-- Part 3: Update the game ---------------------------------------------------

-- How does the game step from one state to another based on user input?

-- Task: redefine `stepGame` to use the UserInput and GameState
--       you defined in parts 1 and 2. Maybe use some helper functions
--       to break up the work, stepping smaller parts of the game.


stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState = 
    case userInput.action of
      Just A -> {gameState | hero <- Hero.getOlder gameState.hero}
      Just B -> {gameState | hero <- Hero.getYounger gameState.hero}
      Nothing -> gameState 
                


-- Part 4: Display the game --------------------------------------------------

-- How should the GameState be displayed to the user?

-- Task: redefine `display` to use the GameState you defined in part 2.

display : (Int,Int) -> GameState -> Element
display (w,h) gameState = asText (String.concat ["Jeanne is ", (String.show gameState.hero.age)])

delta = fps frameRate
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
