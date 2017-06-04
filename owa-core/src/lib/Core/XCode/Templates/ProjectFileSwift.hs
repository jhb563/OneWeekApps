{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Core.XCode.Templates.ProjectFileSwift
Description : Contains template strings for the .pbxproj file in a Swift project
Copyright   : (c) James Bowen, 2016
License     : MIT
Maintainer  : jhbowen047@gmail.com
-}

module Core.XCode.Templates.ProjectFileSwift where

import Data.Text

-- | Template for for project.pbxproj file in XCode.
swiftPbxProjTemplate :: Text
swiftPbxProjTemplate = "\
\// !$$*UTF8*$$!\n\
\{\n\
\    archiveVersion = 1;\n\
\    classes = {\n\
\    };\n\
\    objectVersion = 46;\n\
\    objects = {\n\
\\n\
\/* Begin PBXBuildFile section */\n\
\        9E4A20931DD1224003AABD3 /* AppDelegate.swift in Sources */ = {isa = PBXBuildFile; fileRef = 8E4A20921DD12224003AABD3 /* AppDelegate.swift */; };\n\
\        8E4A20951DD12224003AABD3 /* ViewController.swift in Sources */ = {isa = PBXBuildFile; fileRef = 8E4A20941DD12224003AABD3 /* ViewController.swift */; };\n\
\/* End PBXBuildFile section */\n\
\\n\
\/* Begin PBXFileReference section */\n\
\        8E4A208F1DD12224003AABD3 /* $projectname.app */ = {isa = PBXFileReference; explicitFileType = wrapper.application; includeInIndex = 0; path = \"$projectname.app\"; sourceTree = BUILT_PRODUCTS_DIR; };\n\
\        8E4A20921DD12224003AABD3 /* AppDelegate.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = AppDelegate.swift; sourceTree = \"<group>\"; };\n\
\        8E4A20941DD12224003AABD3 /* ViewController.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = ViewController.swift; sourceTree = \"<group>\"; };\n\
\        8E4A209E1DD12224003AABD3 /* Info.plist */ = {isa = PBXFileReference; lastKnownFileType = text.plist.xml; path = Info.plist; sourceTree = \"<group>\"; };\n\
\/* End PBXFileReference section */\n\
\\n\
\/* Begin PBXFrameworksBuildPhase section */\n\
\        8E4A208C1DD12224003AABD3 /* Frameworks */ = {\n\
\            isa = PBXFrameworksBuildPhase;\n\
\            buildActionMask = 2147483647;\n\
\            files = (\n\
\            );\n\
\            runOnlyForDeploymentPostprocessing = 0;\n\
\        };\n\
\/* End PBXFrameworksBuildPhase section */\n\
\\n\
\/* Begin PBXGroup section */\n\
\        8E4A20861DD12224003AABD3 = {\n\
\            isa = PBXGroup;\n\
\            children = (\n\
\                8E4A20911DD12224003AABD3 /* $projectname */\n\
\            );\n\
\            sourceTree = \"<group>\";\n\
\        };\n\
\        8E4A20911DD12224003AABD3 /* $projectname */ = {\n\
\            isa = PBXGroup;\n\
\            children = (\n\
\                8E4A20921DD12224003AABD3 /* AppDelegate.swift */,\n\
\                8E4A20941DD12224003AABD3 /* ViewController.swift */,\n\
\                8E4A209E1DD12224003AABD3 /* Info.plist */,\n\
\            );\n\
\            path = \"$projectname\";\n\
\            sourceTree = \"<group>\";\n\
\        };\n\
\/* End PBXGroup section */\n\
\\n\
\/* Begin PBXNativeTarget section */\n\
\        8E4A208E1DD12224003AABD3 /* $projectname */ = {\n\
\            isa = PBXNativeTarget;\n\
\            buildConfigurationList = 8E4A20A11DD12225003AABD3 /* Build configuration list for PBXNativeTarget \"$projectname\" */;\n\
\            buildPhases = (\n\
\                8E4A208B1DD12224003AABD3 /* Sources */,\n\
\                8E4A208C1DD12224003AABD3 /* Frameworks */,\n\
\                8E4A208D1DD12224003AABD3 /* Resources */,\n\
\            );\n\
\            buildRules = (\n\
\            );\n\
\            dependencies = (\n\
\            );\n\
\            name = \"$projectname\";\n\
\            productName = \"$projectname\";\n\
\            productReference = 8E4A208F1DD12224003AABD3 /* $projectname.app */;\n\
\            productType = \"com.apple.product-type.application\";\n\
\        };\n\
\/* End PBXNativeTarget section */\n\
\\n\
\/* Begin PBXProject section */\n\
\        8E4A20871DD12224003AABD3 /* Project object */ = {\n\
\            isa = PBXProject;\n\
\            attributes = {\n\
\                LastSwiftUpdateCheck = 0720;\n\
\                LastUpgradeCheck = 0720;\n\
\                ORGANIZATIONNAME = \"One Week Apps\";\n\
\                TargetAttributes = {\n\
\                    8E4A208E1DD12224003AABD3 = {\n\
\                        CreatedOnToolsVersion = 7.2.1;\n\
\                    };\n\
\                };\n\
\            };\n\
\            buildConfigurationList = 8E4A208A1DD12224003AABD3 /* Build configuration list for PBXProject \"$projectname\" */;\n\
\            compatibilityVersion = \"Xcode 3.2\";\n\
\            developmentRegion = English;\n\
\            hasScannedForEncodings = 0;\n\
\            knownRegions = (\n\
\                en,\n\
\                Base,\n\
\            );\n\
\            mainGroup = 8E4A20861DD12224003AABD3;\n\
\            productRefGroup = 8E4A20901DD12224003AABD3 /* Products */;\n\
\            projectDirPath = \"\";\n\
\            projectRoot = \"\";\n\
\            targets = (\n\
\                8E4A208E1DD12224003AABD3 /* $projectname */,\n\
\            );\n\
\        };\n\
\/* End PBXProject section */\n\
\\n\
\/* Begin PBXResourcesBuildPhase section */\n\
\        8E4A208D1DD12224003AABD3 /* Resources */ = {\n\
\            isa = PBXResourcesBuildPhase;\n\
\            buildActionMask = 2147483647;\n\
\            files = (\n\
\            );\n\
\            runOnlyForDeploymentPostprocessing = 0;\n\
\        };\n\
\/* End PBXResourcesBuildPhase section */\n\
\\n\
\/* Begin PBXSourcesBuildPhase section */\n\
\        8E4A208B1DD12224003AABD3 /* Sources */ = {\n\
\            isa = PBXSourcesBuildPhase;\n\
\            buildActionMask = 2147483647;\n\
\            files = (\n\
\                8E4A20951DD12224003AABD3 /* ViewController.swift in Sources */,\n\
\                9E4A20931DD1224003AABD3 /* AppDelegate.swift in Sources */,\n\
\            );\n\
\            runOnlyForDeploymentPostprocessing = 0;\n\
\        };\n\
\/* End PBXSourcesBuildPhase section */\n\
\\n\
\/* Begin XCBuildConfiguration section */\n\
\        8E4A209F1DD12224003AABD3 /* Debug */ = {\n\
\            isa = XCBuildConfiguration;\n\
\            buildSettings = {\n\
\                ALWAYS_SEARCH_USER_PATHS = NO;\n\
\                CLANG_CXX_LANGUAGE_STANDARD = \"gnu++0x\";\n\
\                CLANG_CXX_LIBRARY = \"libc++\";\n\
\                CLANG_ENABLE_MODULES = YES;\n\
\                CLANG_ENABLE_OBJC_ARC = YES;\n\
\                CLANG_WARN_BOOL_CONVERSION = YES;\n\
\                CLANG_WARN_CONSTANT_CONVERSION = YES;\n\
\                CLANG_WARN_DIRECT_OBJC_ISA_USAGE = YES_ERROR;\n\
\                CLANG_WARN_EMPTY_BODY = YES;\n\
\                CLANG_WARN_ENUM_CONVERSION = YES;\n\
\                CLANG_WARN_INT_CONVERSION = YES;\n\
\                CLANG_WARN_OBJC_ROOT_CLASS = YES_ERROR;\n\
\                CLANG_WARN_UNREACHABLE_CODE = YES;\n\
\                CLANG_WARN__DUPLICATE_METHOD_MATCH = YES;\n\
\                \"CODE_SIGN_IDENTITY[sdk=iphoneos*]\" = \"iPhone Developer\";\n\
\                COPY_PHASE_STRIP = NO;\n\
\                DEBUG_INFORMATION_FORMAT = dwarf;\n\
\                ENABLE_STRICT_OBJC_MSGSEND = YES;\n\
\                ENABLE_TESTABILITY = YES;\n\
\                GCC_C_LANGUAGE_STANDARD = gnu99;\n\
\                GCC_DYNAMIC_NO_PIC = NO;\n\
\                GCC_NO_COMMON_BLOCKS = YES;\n\
\                GCC_OPTIMIZATION_LEVEL = 0;\n\
\                GCC_PREPROCESSOR_DEFINITIONS = (\n\
\                    \"DEBUG=1\",\n\
\                    \"$$(inherited)\",\n\
\                );\n\
\                GCC_WARN_64_TO_32_BIT_CONVERSION = YES;\n\
\                GCC_WARN_ABOUT_RETURN_TYPE = YES_ERROR;\n\
\                GCC_WARN_UNDECLARED_SELECTOR = YES;\n\
\                GCC_WARN_UNINITIALIZED_AUTOS = YES_AGGRESSIVE;\n\
\                GCC_WARN_UNUSED_FUNCTION = YES;\n\
\                GCC_WARN_UNUSED_VARIABLE = YES;\n\
\                IPHONEOS_DEPLOYMENT_TARGET = 9.2;\n\
\                MTL_ENABLE_DEBUG_INFO = YES;\n\
\                ONLY_ACTIVE_ARCH = YES;\n\
\                SDKROOT = iphoneos;\n\
\                SWIFT_OPTIMIZATION_LEVEL = \"-Onone\";\n\
\            };\n\
\            name = Debug;\n\
\        };\n\
\        8E4A20A01DD12224003AABD3 /* Release */ = {\n\
\            isa = XCBuildConfiguration;\n\
\            buildSettings = {\n\
\                ALWAYS_SEARCH_USER_PATHS = NO;\n\
\                CLANG_CXX_LANGUAGE_STANDARD = \"gnu++0x\";\n\
\                CLANG_CXX_LIBRARY = \"libc++\";\n\
\                CLANG_ENABLE_MODULES = YES;\n\
\                CLANG_ENABLE_OBJC_ARC = YES;\n\
\                CLANG_WARN_BOOL_CONVERSION = YES;\n\
\                CLANG_WARN_CONSTANT_CONVERSION = YES;\n\
\                CLANG_WARN_DIRECT_OBJC_ISA_USAGE = YES_ERROR;\n\
\                CLANG_WARN_EMPTY_BODY = YES;\n\
\                CLANG_WARN_ENUM_CONVERSION = YES;\n\
\                CLANG_WARN_INT_CONVERSION = YES;\n\
\                CLANG_WARN_OBJC_ROOT_CLASS = YES_ERROR;\n\
\                CLANG_WARN_UNREACHABLE_CODE = YES;\n\
\                CLANG_WARN__DUPLICATE_METHOD_MATCH = YES;\n\
\                \"CODE_SIGN_IDENTITY[sdk=iphoneos*]\" = \"iPhone Developer\";\n\
\                COPY_PHASE_STRIP = NO;\n\
\                DEBUG_INFORMATION_FORMAT = \"dwarf-with-dsym\";\n\
\                ENABLE_NS_ASSERTIONS = NO;\n\
\                ENABLE_STRICT_OBJC_MSGSEND = YES;\n\
\                GCC_C_LANGUAGE_STANDARD = gnu99;\n\
\                GCC_NO_COMMON_BLOCKS = YES;\n\
\                GCC_WARN_64_TO_32_BIT_CONVERSION = YES;\n\
\                GCC_WARN_ABOUT_RETURN_TYPE = YES_ERROR;\n\
\                GCC_WARN_UNDECLARED_SELECTOR = YES;\n\
\                GCC_WARN_UNINITIALIZED_AUTOS = YES_AGGRESSIVE;\n\
\                GCC_WARN_UNUSED_FUNCTION = YES;\n\
\                GCC_WARN_UNUSED_VARIABLE = YES;\n\
\                IPHONEOS_DEPLOYMENT_TARGET = 9.2;\n\
\                MTL_ENABLE_DEBUG_INFO = NO;\n\
\                SDKROOT = iphoneos;\n\
\                VALIDATE_PRODUCT = YES;\n\
\            };\n\
\            name = Release;\n\
\        };\n\
\        8E4A20A21DD12225003AABD3 /* Debug */ = {\n\
\            isa = XCBuildConfiguration;\n\
\            buildSettings = {\n\
\                ASSETCATALOG_COMPILER_APPICON_NAME = AppIcon;\n\
\                INFOPLIST_FILE = \"$projectname/Info.plist\";\n\
\                LD_RUNPATH_SEARCH_PATHS = \"$$(inherited) @executable_path/Frameworks\";\n\
\                PRODUCT_BUNDLE_IDENTIFIER = \"com.$companyname.$projectbundlename\";\n\
\                PRODUCT_NAME = \"$$(TARGET_NAME)\";\n\
\            };\n\
\            name = Debug;\n\
\        };\n\
\        8E4A20A31DD12225003AABD3 /* Release */ = {\n\
\            isa = XCBuildConfiguration;\n\
\            buildSettings = {\n\
\                ASSETCATALOG_COMPILER_APPICON_NAME = AppIcon;\n\
\                INFOPLIST_FILE = \"$projectname/Info.plist\";\n\
\                LD_RUNPATH_SEARCH_PATHS = \"$$(inherited) @executable_path/Frameworks\";\n\
\                PRODUCT_BUNDLE_IDENTIFIER = \"com.$companyname.$projectbundlename\";\n\
\                PRODUCT_NAME = \"$$(TARGET_NAME)\";\n\
\            };\n\
\            name = Release;\n\
\        };\n\
\/* End XCBuildConfiguration section */\n\
\\n\
\/* Begin XCConfigurationList section */\n\
\        8E4A208A1DD12224003AABD3 /* Build configuration list for PBXProject \"$projectname\" */ = {\n\
\            isa = XCConfigurationList;\n\
\            buildConfigurations = (\n\
\                8E4A209F1DD12224003AABD3 /* Debug */,\n\
\                8E4A20A01DD12224003AABD3 /* Release */,\n\
\            );\n\
\            defaultConfigurationIsVisible = 0;\n\
\            defaultConfigurationName = Release;\n\
\        };\n\
\        8E4A20A11DD12225003AABD3 /* Build configuration list for PBXNativeTarget \"$projectname\" */ = {\n\
\            isa = XCConfigurationList;\n\
\            buildConfigurations = (\n\
\                8E4A20A21DD12225003AABD3 /* Debug */,\n\
\                8E4A20A31DD12225003AABD3 /* Release */,\n\
\            );\n\
\            defaultConfigurationIsVisible = 0;\n\
\            defaultConfigurationName = Release;\n\
\        };\n\
\/* End XCConfigurationList section */\n\
\    };\n\
\    rootObject = 8E4A20871DD12224003AABD3 /* Project object */;\n\
\}\n\
\"
