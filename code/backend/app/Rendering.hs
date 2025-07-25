module Rendering where

import Tree

render :: Tree -> String
render = error "render nyi"

printTree :: Tree -> IO ()
printTree = putStrLn . render
