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
```
export PATH="$HOME/Library/Haskell/bin:$PATH"
```

#### Playing the Game
```
cabal build && ./dist/build/haskell-TTT/haskell-TTT
```

#### Running the Tests
```
runhaskell -itest -isrc test/TestRunner.hs
```

