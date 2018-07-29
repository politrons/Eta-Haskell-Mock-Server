module Main where

import HttpServer
import ScottyHttpServer

main :: IO ()

--main = myServer
main = scottyServer
