module Model where

data InfoToShow = ShowNothing
                | ShowNumber    Int
                | ShowChar      Char

data GameState = GameState {
                    infoToShow  :: InfoToShow,
                    elapsedTime :: Float --Maybe could be Int
                    }

initialState :: GameState
initialState = GameState ShowNothing 0