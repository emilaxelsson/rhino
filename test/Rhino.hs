module Main where

import Rhino.Prelude
import qualified Prelude

import Control.Exception (catch, throwIO)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

import Rhino.Evaluate
import Rhino.InputProperties
import Rhino.Load
import Rhino.Reachability

import Data.DAG
import Rhino.AST

import Test.Tasty
import Test.Tasty.HUnit

newtype DisplayError = DisplayError String

instance Show DisplayError where
  show (DisplayError msg) = msg

instance Exception DisplayError

displayError :: IO a -> IO a
displayError m = m `catch` \(e :: SomeException) ->
  throwIO $ DisplayError $ displayException e

test_modules = unsafePerformIO $ displayError $
  loadAndCheckProgam ["examples"] "examples/test.rh"

salary_modules = unsafePerformIO $ displayError $
  loadAndCheckProgam ["examples"] "examples/salary.rh"

{-# NOINLINE test_modules #-}
{-# NOINLINE salary_modules #-}

test_tests =
  [ testCase "evaluate" $
      evaluate mempty test_modules "test_add" @?= Integer 9
  ]

salary_tests =
  [ testCase "inputProperties" $
      case inputProperties (unAnnnotateDAG salary_modules) of
        Right inps ->
          Map.keys inps @?=
          [ "net_salary_for_alice"
          , "net_salary_for_bob"
          , "tax_rate"
          ]
        _ -> oops

  , testGroup "reachableInputs" $
      [ testCase "for all" $
          sort (toList $ reachableInputs salary_modules "main") @?=
          [ "net_salary_for_alice"
          , "net_salary_for_bob"
          , "tax_rate"
          ]
      , testCase "for Bob" $
          sort (toList $ reachableInputs salary_modules "salary_bob") @?=
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
          evaluate env salary_modules "main" @?= Float 28000
  ]

testTree =
  testGroup "Rhino tests" $
    [ testGroup "test.rh"   test_tests
    , testGroup "salary.rh" salary_tests
    ]

main :: IO ()
main = defaultMain testTree
