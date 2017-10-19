{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import "hspec" Test.Hspec (hspec, describe)

-- local imports
import qualified QM.Spec
import qualified QN.Spec
import qualified QMB.Spec
import qualified QNB.Spec
import qualified QMS.Spec
import qualified QNS.Spec

-- Generated tests for CRLF line-breaks
import qualified LineBreaks.CRLF.QM.Spec
import qualified LineBreaks.CRLF.QN.Spec
import qualified LineBreaks.CRLF.QMB.Spec
import qualified LineBreaks.CRLF.QNB.Spec
import qualified LineBreaks.CRLF.QMS.Spec
import qualified LineBreaks.CRLF.QNS.Spec


main :: IO ()
main = hspec $ do
  describe "QM" QM.Spec.spec
  describe "QN (QM but without interpolation)" QN.Spec.spec
  describe "QMB (interpolated string with line-*B*reaks)" QMB.Spec.spec
  describe "QNB (QMB but without interpolation)" QNB.Spec.spec
  describe "QMS (interpolated string with line-breaks replaced with *S*paces)"
            QMS.Spec.spec
  describe "QNS (QMS but without interpolation)" QNS.Spec.spec

  describe "QM (CRLF line-breaks)"  LineBreaks.CRLF.QM.Spec.spec
  describe "QN (CRLF line-breaks)"  LineBreaks.CRLF.QN.Spec.spec
  describe "QMB (CRLF line-breaks)" LineBreaks.CRLF.QMB.Spec.spec
  describe "QNB (CRLF line-breaks)" LineBreaks.CRLF.QNB.Spec.spec
  describe "QMS (CRLF line-breaks)" LineBreaks.CRLF.QMS.Spec.spec
  describe "QNS (CRLF line-breaks)" LineBreaks.CRLF.QNS.Spec.spec
