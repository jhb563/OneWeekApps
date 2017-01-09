module TestAppInfo where

import Model.OWAAppInfo

appInfo1 :: OWAAppInfo
appInfo1 = OWAAppInfo {
  appName = "MySampleApp",
  appPrefix = "MSA",
  authorName = "James Bowen",
  dateCreatedString = "2/16/2016",
  companyName = Just "One Week Apps"
}

appInfo2 :: OWAAppInfo
appInfo2 = OWAAppInfo {
  appName = "NewApp",
  appPrefix = "NAP",
  authorName = "jasdf",
  dateCreatedString = "3/15/2016",
  companyName = Just "OWA"
}

appInfo3 :: OWAAppInfo
appInfo3 = OWAAppInfo {
  appName = "App",
  appPrefix = "APP",
  authorName = "CT",
  dateCreatedString = "10/31/2015",
  companyName = Nothing
}

appInfo4 :: OWAAppInfo
appInfo4 = OWAAppInfo {
  appName = "SampleApp",
  appPrefix = "SMP",
  authorName = "",
  dateCreatedString = "3/16/2016",
  companyName = Just "MyCompany"
} 

appInfo5 :: OWAAppInfo
appInfo5 = OWAAppInfo {
  appName = "CommentedApp",
  appPrefix = "CMT",
  authorName = "James",
  dateCreatedString = "10/10/15",
  companyName = Just "MyCompany"
}
