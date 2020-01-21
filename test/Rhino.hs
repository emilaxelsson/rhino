{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

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
import Rhino.StaticCheck

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

imp1_modules = unsafePerformIO $ displayError $
  loadAndCheckProgam ["examples/imports"] "examples/imports/imp1.rh"

imp2_modules = unsafePerformIO $ displayError $
  loadAndCheckProgam ["examples/imports"] "examples/imports/imp2.rh"

fail1_modules = unsafePerformIO $
  loadProgam ["examples/imports"] "examples/imports/fail1.rh"

fail2_modules = unsafePerformIO $
  loadProgam ["examples/imports"] "examples/imports/fail2.rh"

fail3_modules = unsafePerformIO $
  loadProgam ["examples/imports"] "examples/imports/fail3.rh"

salary_modules = unsafePerformIO $ displayError $
  loadAndCheckProgam ["examples"] "examples/salary.rh"

{-# NOINLINE test_modules #-}
{-# NOINLINE imp1_modules #-}
{-# NOINLINE imp2_modules #-}
{-# NOINLINE fail1_modules #-}
{-# NOINLINE fail2_modules #-}
{-# NOINLINE fail3_modules #-}
{-# NOINLINE salary_modules #-}

test_tests =
  [ testCase "evaluate" $
      evaluate mempty test_modules "test_add" @?= Integer 9
  ]

import_tests =
  [ testCase "evaluate imp1" $
      evaluate mempty imp1_modules "main" @?= Integer 224

  , testCase "evaluate imp2" $
      evaluate mempty imp2_modules "main" @?= Integer 35

  , testCase "check fail1" $ case checkProgram fail1_modules of
      Left (NoContext (ImportConflict (d1, _) (d2, _))) -> do
        nameOf d1 @?= "aaa"
        nameOf d2 @?= "aaa"
        sourceName (location d1) @?= "examples/imports/a.rh"
        sourceName (location d2) @?= "examples/imports/b.rh"

  , testCase "check fail2" $ case checkProgram fail2_modules of
      Left (LocatedContext _ e) -> e @?=
        ModuleDoesNotExport "a" ["x"]

  , testCase "check fail3" $ case checkProgram fail3_modules of
      Left (LocatedContext _ e) -> e @?=
        ModuleDoesNotExport "a" ["x"]

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
    , testGroup "imports/"  import_tests
    , testGroup "salary.rh" salary_tests
    ]

main :: IO ()
main = defaultMain testTree
