module TestAppInfo where

import OWAAppInfo

appInfo1 :: OWAAppInfo
appInfo1 = OWAAppInfo {
  appName = "MySampleApp",
  authorName = "James BoweN",
  dateCreatedString = "2/16/2016",
  companyName = Just "One Week Apps"
}

appInfo2 :: OWAAppInfo
appInfo2 = OWAAppInfo {
  appName = "NewApp",
  authorName = "jasdf",
  dateCreatedString = "3/15/2016",
  companyName = Just "OWA"
}

appInfo3 :: OWAAppInfo
appInfo3 = OWAAppInfo {
  appName = "App",
  authorName = "CT",
  dateCreatedString = "10/31/2015",
  companyName = Nothing
}

appInfo4 :: OWAAppInfo
appInfo4 = OWAAppInfo {
  appName = "SampleApp",
  authorName = "",
  dateCreatedString = "3/16/2016",
  companyName = Just "MyCompany"
} 
