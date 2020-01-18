module Main where

import Rhino.Prelude
import qualified Prelude

import Control.Exception (catch)
import qualified Data.Map as Map
import System.Exit (exitFailure, exitSuccess)

import Rhino.Evaluate
import Rhino.InputProperties
import Rhino.Load
import Rhino.Reachability

import Data.DAG
import Rhino.AST

import Test.Tasty
import Test.Tasty.HUnit

newtype TestError = TestError String

mainWrapper :: IO a -> IO a
mainWrapper m = do
  void $ m `catch` \(e :: SomeException) -> do
    Prelude.putStrLn $ displayException e
    exitFailure
  exitSuccess

testTree modules = testGroup "Rhino tests" $

  [ testCase "inputProperties" $
      case inputProperties (unAnnnotateDAG modules) of
        Right inps ->
          Map.keys inps @?=
          [ "net_salary_for_alice"
          , "net_salary_for_bob"
          , "tax_rate"
          ]
        _ -> oops

  , testGroup "reachableInputs" $
      [ testCase "for all" $
          sort (toList $ reachableInputs modules "main") @?=
          [ "net_salary_for_alice"
          , "net_salary_for_bob"
          , "tax_rate"
          ]
      , testCase "for Bob" $
          sort (toList $ reachableInputs modules "salary_bob") @?=
          [ "net_salary_for_bob"
          , "tax_rate"
          ]
      ]

  , let env = Map.fromList
          [ ("net_salary_for_alice", Integer 20000)
          , ("net_salary_for_bob",   Integer 20000)
          , ("tax_rate",             Float   0.3)
          ]
     in testCase "evaluate" $
          evaluate env modules "main" @?=
          Float 28000
  ]

main :: IO ()
main = mainWrapper $ do
  modules <- loadAndCheckProgam ["examples"] "examples/salary.rh"
  defaultMain $ testTree modules
