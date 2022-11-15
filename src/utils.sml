fun applyOnly pred f (x :: xs) 
    = case pred x of
          true => f x :: applyOnly pred f xs
        | false => x :: applyOnly pred f xs
