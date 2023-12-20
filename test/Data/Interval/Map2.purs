module Test.Data.Interval.Map2 where

import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Data.Interval.Map2

spec :: Spec Unit
spec = 
  describe "Data.Interval.Map2" do 
    describe "inInterval" do 
      it "everything is in the whole range" do 
        inInterval 0 whole
          # shouldEqual EQ
      
      it "left open range" do 
        inInterval 0 (Interval (LeftBoundary 0 Open) PosInf)
          # shouldEqual LT

      it "left closed range" do 
        inInterval 0 (Interval (LeftBoundary 0 Closed) PosInf)
          # shouldEqual EQ

      it "right open range" do 
        inInterval 1 (Interval NegInf (RightBoundary 1 Open))
          # shouldEqual GT

      it "right closed range" do
        inInterval 1 (Interval NegInf (RightBoundary 1 Closed))
          # shouldEqual EQ