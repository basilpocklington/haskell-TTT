# Haskell-TTT

## Setup

Clone the repo and cd into the game directory

#### Install Haskell and Dependencies
```
brew cask install haskell-platform
```
```
cabal update && cabal install hspec
```

#### Playing the Game
```
runhaskell -isrc src/Main.hs
```

#### Running the Tests
```
runhaskell -itest -isrc test/TestRunner.hs
```

