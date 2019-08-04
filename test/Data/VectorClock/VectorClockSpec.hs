module Data.VectorClock.VectorClockSpec (spec) where
import qualified Data.VectorClock as VClock
import Test.Hspec

import qualified Data.List as List

spec = do
  describe "increment" $ do
    context "when the pid is not yet in the clock" $ do
      it "sets the value for pid to 1" $ do
        (VClock.toList $ VClock.increment 42 VClock.empty) `shouldBe` [(42, 1)]
    context "when the pid is already exists" $ do
      it "increments the value for the pid" $ do
        let clock = VClock.fromList [(42, 3)]
        (VClock.toList $ VClock.increment 42 clock) `shouldContain` [(42, 4)]
  describe "merge" $ do
    context "when the pid is present in one clock but not in the other" $ do
      it "inserts the pid into the clock" $ do
        let c1 = VClock.fromList [(42, 3)]
            c2 = VClock.fromList [(44, 2)]
        (List.sortOn fst . VClock.toList $ VClock.merge c1 c2) `shouldBe` [(42,3), (44,2)]
    context "when the pid is present in both clocks" $ do
      it "inserts the bigger of the two values" $ do
        let c1 = VClock.fromList [(42, 3), (44, 5)]
            c2 = VClock.fromList [(44, 2)]
        (List.sortOn fst . VClock.toList $ VClock.merge c1 c2) `shouldBe` [(42,3), (44,5)]

  describe "descends" $ do
    context "when A is empty and B is not" $ do
      it "returns True" $ do
        let vcA = VClock.empty
            vcB = VClock.fromList [(42, 1)]
        VClock.descends vcB vcA `shouldBe` True
    context "when B is empty and A is not" $ do
      it "returns False" $ do
        let vcA = VClock.fromList [(42, 1)]
            vcB = VClock.empty
        VClock.descends vcB vcA `shouldBe` False
    context "when both A and B are empty" $ do
      it "returns True" $ do
        let vcA = VClock.empty :: VClock.VectorClock Integer
            vcB = VClock.empty :: VClock.VectorClock Integer
        VClock.descends vcB vcA `shouldBe` True
    context "when every value in A is less than or equal B" $ do
      it "returns True" $ do
        let vcA = VClock.fromList [(42, 1), (43, 3)]
            vcB = VClock.fromList [(42, 1), (43, 5)]
        VClock.descends vcB vcA `shouldBe` True
    context "when at least one value in B not greater or equal than the one in A" $ do
      it "returns False" $ do
        let vcA = VClock.fromList [(42, 5), (43, 3)]
            vcB = VClock.fromList [(42, 1), (43, 5)]
        VClock.descends vcB vcA `shouldBe` False
    context "when C is merged from A and B" $ do
      it "descends from A" $ do
        let vcA = VClock.fromList [(42, 5), (43, 3)]
            vcB = VClock.fromList [(42, 1), (43, 5)]
            vcC = VClock.merge vcA vcB
        VClock.descends vcC vcA `shouldBe` True
      it "descends from B" $ do
        let vcA = VClock.fromList [(42, 5), (43, 3)]
            vcB = VClock.fromList [(42, 1), (43, 5)]
            vcC = VClock.merge vcA vcB
        VClock.descends vcC vcB `shouldBe` True

  describe "dominates" $ do
    context "when A is empty and B is not" $ do
      it "returns True" $ do
        let vcA = VClock.empty
            vcB = VClock.fromList [(42, 1)]
        VClock.dominates vcB vcA `shouldBe` True
    context "when B is empty and A is not" $ do
      it "returns False" $ do
        let vcA = VClock.fromList [(42, 1)]
            vcB = VClock.empty
        VClock.dominates vcB vcA `shouldBe` False

    context "when both A and B are empty" $ do
      it "returns False" $ do
        let vcA = VClock.empty :: VClock.VectorClock Integer
            vcB = VClock.empty :: VClock.VectorClock Integer
        VClock.dominates vcB vcA `shouldBe` False

    context "when B has seen events A has not" $ do
      it "returns True" $ do
        let vcA = VClock.fromList [(42, 1), (43, 3)]
            vcB = VClock.fromList [(42, 1), (43, 3), (44, 1)]
        VClock.dominates vcB vcA `shouldBe` True

    context "when B has seen a more recent event than in A" $ do
       it "returns True" $ do
        let vcA = VClock.fromList [(42, 1), (43, 3)]
            vcB = VClock.increment 42 vcA
        VClock.dominates vcB vcA `shouldBe` True

    context "when A has seen events B has not" $ do
      it "returns False" $ do
        let vcA = VClock.fromList [(42, 1), (43, 3), (44, 1)]
            vcB = VClock.fromList [(42, 1), (43, 3)]
        VClock.dominates vcB vcA `shouldBe` False

    context "when A has seen the same events as B" $ do
      it "returns False" $ do
        let vcA = VClock.fromList [(42, 1), (43, 3), (44, 1)]
            vcB = VClock.fromList [(42, 1), (43, 3), (44, 1)]
        VClock.dominates vcB vcA `shouldBe` False

  describe "concurrent" $ do
    context "when A is empty and B is not" $ do
      it "returns False" $ do
        let vcA = VClock.empty :: VClock.VectorClock Integer
            vcB = VClock.fromList [(42, 1), (43, 3), (44, 1)]
        VClock.concurrent vcA vcB `shouldBe` False

    context "when B is empty and A is not" $ do
      it "returns False" $ do
        let vcA = VClock.fromList [(42, 1), (43, 3), (44, 1)]
            vcB = VClock.empty :: VClock.VectorClock Integer
        VClock.concurrent vcA vcB `shouldBe` False

    context "when B has events unseen in A but A has no events unseen in B" $ do
      it "returns False" $ do
         let vcA = VClock.fromList [(42, 1), (43, 3)]
             vcB = VClock.fromList [(42, 1), (43, 3), (44, 1)]
         VClock.concurrent vcA vcB `shouldBe` False
    context "when A has events unseen in B but B has no events unseen in A" $ do
      it "returns False" $ do
         let vcA = VClock.fromList [(42, 1), (43, 3), (44, 1)]
             vcB = VClock.fromList [(42, 1), (43, 3)]
         VClock.concurrent vcA vcB `shouldBe` False
    context "when A has events unseen in A and B has events unseen in A" $ do
      it "returns True" $ do
         let vcA = VClock.fromList [(42, 1), (43, 3), (44, 1)]
             vcB = VClock.fromList [(42, 1), (43, 3), (45, 1)]
         VClock.concurrent vcA vcB `shouldBe` True
