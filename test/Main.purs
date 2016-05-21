module Test.Main where

import Prelude(Unit, (<>), ($), bind, const, pure, unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free(Free(), liftF)

import Data.Either(either)
import Data.Maybe(Maybe(..))
import Data.Lens.Prism(prism')

import Test.Mock.Mockfree(Op(..), OpPrism(), MockSpec(), assertEquals, expectWrite, expectRead, readOp, runMock, writeOp)

data ConsoleF a
  = WriteLine (Op String Unit   a)
  | ReadLine  (Op Unit   String a)

_WriteLine :: OpPrism ConsoleF String Unit
_WriteLine = prism' WriteLine deconstruct
  where
    deconstruct (WriteLine op) = Just op
    deconstruct _              = Nothing

_ReadLine :: OpPrism ConsoleF Unit String
_ReadLine = prism' ReadLine deconstruct
  where
    deconstruct (ReadLine op) = Just op
    deconstruct _             = Nothing

readLine :: Free ConsoleF String
readLine = readOp _ReadLine

writeLine :: String -> Free ConsoleF Unit
writeLine s = writeOp _WriteLine s

-- | Defines a mock specification for a program in `ConsoleF`.
mockSpec :: MockSpec ConsoleF
mockSpec = do
  expectWrite _WriteLine (assertEquals "What is your name?")
  expectRead  _ReadLine  "World"
  expectWrite _WriteLine (assertEquals "Hello, World!")

goodProgram :: Free ConsoleF Unit
goodProgram = do
  writeLine "What is your name?"
  name <- readLine
  writeLine ("Hello, " <> name <> "!")

informalProgram :: Free ConsoleF Unit
informalProgram = do
  writeLine "What is your first name?"
  name <- readLine
  writeLine ("Hello, " <> name <> "!")

rudeProgram :: Free ConsoleF Unit
rudeProgram = do
  writeLine "What is your name?"
  writeLine ("I don't care!")

dismissiveProgram :: Free ConsoleF Unit
dismissiveProgram = do
  writeLine "What is your name?"
  name <- readLine
  writeLine ("Goodbye, " <> name <> "!")

emptyProgram :: Free ConsoleF Unit
emptyProgram = pure unit

testProgram :: Free ConsoleF Unit -> String
testProgram program = either ((<>) "Failure: ") (const "Success!") (runMock mockSpec program)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Testing good program"
  log $ testProgram goodProgram

  log "Testing informal program"
  log $ testProgram informalProgram

  log "Testing rude program"
  log $ testProgram rudeProgram

  log "Testing dismissive program"
  log $ testProgram dismissiveProgram

  log "Testing empty program"
  log $ testProgram emptyProgram
