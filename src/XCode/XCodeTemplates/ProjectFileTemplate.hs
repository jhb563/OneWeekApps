{-# LANGUAGE OverloadedString #-}

module ProjectFileTemplate where

import Data.Text

baseTemplate :: Text
baseTemplate = "
// !$$*UTF8*$$!
{
	archiveVersion = 1;
	classes = {
	};
	objectVersion = 46;
	objects = {

/* Begin PBXBuildFile section */
		9E4A20931DD1224003AABD3 /* AppDelegate.swift in Sources */ = {isa = PBXBuildFile; fileRef = 8E4A20921DD12224003AABD3 /* AppDelegate.swift */; };
		8E4A20951DD12224003AABD3 /* ViewController.swift in Sources */ = {isa = PBXBuildFile; fileRef = 8E4A20941DD12224003AABD3 /* ViewController.swift */; };
/* End PBXBuildFile section */

/* Begin PBXFileReference section */
		8E4A208F1DD12224003AABD3 /* $projectname.app */ = {isa = PBXFileReference; explicitFileType = wrapper.application; includeInIndex = 0; path = $projectname.app; sourceTree = BUILT_PRODUCTS_DIR; };
		8E4A20921DD12224003AABD3 /* AppDelegate.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = AppDelegate.swift; sourceTree = \"<group>\"; };
		8E4A20941DD12224003AABD3 /* ViewController.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = ViewController.swift; sourceTree = \"<group>\"; };
		8E4A209E1DD12224003AABD3 /* Info.plist */ = {isa = PBXFileReference; lastKnownFileType = text.plist.xml; path = Info.plist; sourceTree = \"<group>\"; };
/* End PBXFileReference section */

/* Begin PBXFrameworksBuildPhase section */
		8E4A208C1DD12224003AABD3 /* Frameworks */ = {
			isa = PBXFrameworksBuildPhase;
			buildActionMask = 2147483647;
			files = (
			);
			runOnlyForDeploymentPostprocessing = 0;
		};
/* End PBXFrameworksBuildPhase section */

/* Begin PBXGroup section */
		8E4A20861DD12224003AABD3 = {
			isa = PBXGroup;
			children = (
				8E4A20911DD12224003AABD3 /* $projectname */
			);
			sourceTree = \"<group>\";
		};
		8E4A20911DD12224003AABD3 /* $projectname */ = {
			isa = PBXGroup;
			children = (
				8E4A20921DD12224003AABD3 /* AppDelegate.swift */,
				8E4A20941DD12224003AABD3 /* ViewController.swift */,
				8E4A209E1DD12224003AABD3 /* Info.plist */,
			);
			path = $projectname;
			sourceTree = \"<group>\";
		};
/* End PBXGroup section */

/* Begin PBXNativeTarget section */
		8E4A208E1DD12224003AABD3 /* $projectname */ = {
			isa = PBXNativeTarget;
			buildConfigurationList = 8E4A20A11DD12225003AABD3 /* Build configuration list for PBXNativeTarget \"$projectname\" */;
			buildPhases = (
				8E4A208B1DD12224003AABD3 /* Sources */,
				8E4A208C1DD12224003AABD3 /* Frameworks */,
				8E4A208D1DD12224003AABD3 /* Resources */,
			);
			buildRules = (
			);
			dependencies = (
			);
			name = $projectname;
			productName = $projectname;
			productReference = 8E4A208F1DD12224003AABD3 /* $projectname.app */;
			productType = \"com.apple.product-type.application\";
		};
/* End PBXNativeTarget section */

/* Begin PBXProject section */
		8E4A20871DD12224003AABD3 /* Project object */ = {
			isa = PBXProject;
			attributes = {
				LastSwiftUpdateCheck = 0720;
				LastUpgradeCheck = 0720;
				ORGANIZATIONNAME = \"One Week Apps\";
				TargetAttributes = {
					8E4A208E1DD12224003AABD3 = {
						CreatedOnToolsVersion = 7.2.1;
					};
				};
			};
			buildConfigurationList = 8E4A208A1DD12224003AABD3 /* Build configuration list for PBXProject \"$projectname\" */;
			compatibilityVersion = \"Xcode 3.2\";
			developmentRegion = English;
			hasScannedForEncodings = 0;
			knownRegions = (
				en,
				Base,
			);
			mainGroup = 8E4A20861DD12224003AABD3;
			productRefGroup = 8E4A20901DD12224003AABD3 /* Products */;
			projectDirPath = \"\";
			projectRoot = \"\";
			targets = (
				8E4A208E1DD12224003AABD3 /* $projectname */,
			);
		};
/* End PBXProject section */

/* Begin PBXResourcesBuildPhase section */
		8E4A208D1DD12224003AABD3 /* Resources */ = {
			isa = PBXResourcesBuildPhase;
			buildActionMask = 2147483647;
			files = (
			);
			runOnlyForDeploymentPostprocessing = 0;
		};
/* End PBXResourcesBuildPhase section */

/* Begin PBXSourcesBuildPhase section */
		8E4A208B1DD12224003AABD3 /* Sources */ = {
			isa = PBXSourcesBuildPhase;
			buildActionMask = 2147483647;
			files = (
				8E4A20951DD12224003AABD3 /* ViewController.swift in Sources */,
				9E4A20931DD1224003AABD3 /* AppDelegate.swift in Sources */,
			);
			runOnlyForDeploymentPostprocessing = 0;
		};
/* End PBXSourcesBuildPhase section */

/* Begin XCBuildConfiguration section */
		8E4A209F1DD12224003AABD3 /* Debug */ = {
			isa = XCBuildConfiguration;
			buildSettings = {
				ALWAYS_SEARCH_USER_PATHS = NO;
				CLANG_CXX_LANGUAGE_STANDARD = \"gnu++0x\";
				CLANG_CXX_LIBRARY = \"libc++\";
				CLANG_ENABLE_MODULES = YES;
				CLANG_ENABLE_OBJC_ARC = YES;
				CLANG_WARN_BOOL_CONVERSION = YES;
				CLANG_WARN_CONSTANT_CONVERSION = YES;
				CLANG_WARN_DIRECT_OBJC_ISA_USAGE = YES_ERROR;
				CLANG_WARN_EMPTY_BODY = YES;
				CLANG_WARN_ENUM_CONVERSION = YES;
				CLANG_WARN_INT_CONVERSION = YES;
				CLANG_WARN_OBJC_ROOT_CLASS = YES_ERROR;
				CLANG_WARN_UNREACHABLE_CODE = YES;
				CLANG_WARN__DUPLICATE_METHOD_MATCH = YES;
				\"CODE_SIGN_IDENTITY[sdk=iphoneos*]\" = \"iPhone Developer\";
				COPY_PHASE_STRIP = NO;
				DEBUG_INFORMATION_FORMAT = dwarf;
				ENABLE_STRICT_OBJC_MSGSEND = YES;
				ENABLE_TESTABILITY = YES;
				GCC_C_LANGUAGE_STANDARD = gnu99;
				GCC_DYNAMIC_NO_PIC = NO;
				GCC_NO_COMMON_BLOCKS = YES;
				GCC_OPTIMIZATION_LEVEL = 0;
				GCC_PREPROCESSOR_DEFINITIONS = (
					\"DEBUG=1\",
					\"$$(inherited)\",
				);
				GCC_WARN_64_TO_32_BIT_CONVERSION = YES;
				GCC_WARN_ABOUT_RETURN_TYPE = YES_ERROR;
				GCC_WARN_UNDECLARED_SELECTOR = YES;
				GCC_WARN_UNINITIALIZED_AUTOS = YES_AGGRESSIVE;
				GCC_WARN_UNUSED_FUNCTION = YES;
				GCC_WARN_UNUSED_VARIABLE = YES;
				IPHONEOS_DEPLOYMENT_TARGET = 9.2;
				MTL_ENABLE_DEBUG_INFO = YES;
				ONLY_ACTIVE_ARCH = YES;
				SDKROOT = iphoneos;
				SWIFT_OPTIMIZATION_LEVEL = \"-Onone\";
			};
			name = Debug;
		};
		8E4A20A01DD12224003AABD3 /* Release */ = {
			isa = XCBuildConfiguration;
			buildSettings = {
				ALWAYS_SEARCH_USER_PATHS = NO;
				CLANG_CXX_LANGUAGE_STANDARD = \"gnu++0x\";
				CLANG_CXX_LIBRARY = \"libc++\";
				CLANG_ENABLE_MODULES = YES;
				CLANG_ENABLE_OBJC_ARC = YES;
				CLANG_WARN_BOOL_CONVERSION = YES;
				CLANG_WARN_CONSTANT_CONVERSION = YES;
				CLANG_WARN_DIRECT_OBJC_ISA_USAGE = YES_ERROR;
				CLANG_WARN_EMPTY_BODY = YES;
				CLANG_WARN_ENUM_CONVERSION = YES;
				CLANG_WARN_INT_CONVERSION = YES;
				CLANG_WARN_OBJC_ROOT_CLASS = YES_ERROR;
				CLANG_WARN_UNREACHABLE_CODE = YES;
				CLANG_WARN__DUPLICATE_METHOD_MATCH = YES;
				\"CODE_SIGN_IDENTITY[sdk=iphoneos*]\" = \"iPhone Developer\";
				COPY_PHASE_STRIP = NO;
				DEBUG_INFORMATION_FORMAT = \"dwarf-with-dsym\";
				ENABLE_NS_ASSERTIONS = NO;
				ENABLE_STRICT_OBJC_MSGSEND = YES;
				GCC_C_LANGUAGE_STANDARD = gnu99;
				GCC_NO_COMMON_BLOCKS = YES;
				GCC_WARN_64_TO_32_BIT_CONVERSION = YES;
				GCC_WARN_ABOUT_RETURN_TYPE = YES_ERROR;
				GCC_WARN_UNDECLARED_SELECTOR = YES;
				GCC_WARN_UNINITIALIZED_AUTOS = YES_AGGRESSIVE;
				GCC_WARN_UNUSED_FUNCTION = YES;
				GCC_WARN_UNUSED_VARIABLE = YES;
				IPHONEOS_DEPLOYMENT_TARGET = 9.2;
				MTL_ENABLE_DEBUG_INFO = NO;
				SDKROOT = iphoneos;
				VALIDATE_PRODUCT = YES;
			};
			name = Release;
		};
		8E4A20A21DD12225003AABD3 /* Debug */ = {
			isa = XCBuildConfiguration;
			buildSettings = {
				ASSETCATALOG_COMPILER_APPICON_NAME = AppIcon;
				INFOPLIST_FILE = $projectname/Info.plist;
				LD_RUNPATH_SEARCH_PATHS = \"$$(inherited) @executable_path/Frameworks\";
				PRODUCT_BUNDLE_IDENTIFIER = com.oneweekapps.$projectname;
				PRODUCT_NAME = \"$$(TARGET_NAME)\";
			};
			name = Debug;
		};
		8E4A20A31DD12225003AABD3 /* Release */ = {
			isa = XCBuildConfiguration;
			buildSettings = {
				ASSETCATALOG_COMPILER_APPICON_NAME = AppIcon;
				INFOPLIST_FILE = $projectname/Info.plist;
				LD_RUNPATH_SEARCH_PATHS = \"$$(inherited) @executable_path/Frameworks\";
				PRODUCT_BUNDLE_IDENTIFIER = com.oneweekapps.$projectname;
				PRODUCT_NAME = \"$$(TARGET_NAME)\";
			};
			name = Release;
		};
/* End XCBuildConfiguration section */

/* Begin XCConfigurationList section */
		8E4A208A1DD12224003AABD3 /* Build configuration list for PBXProject \"$projectname\" */ = {
			isa = XCConfigurationList;
			buildConfigurations = (
				8E4A209F1DD12224003AABD3 /* Debug */,
				8E4A20A01DD12224003AABD3 /* Release */,
			);
			defaultConfigurationIsVisible = 0;
			defaultConfigurationName = Release;
		};
		8E4A20A11DD12225003AABD3 /* Build configuration list for PBXNativeTarget \"$projectname\" */ = {
			isa = XCConfigurationList;
			buildConfigurations = (
				8E4A20A21DD12225003AABD3 /* Debug */,
				8E4A20A31DD12225003AABD3 /* Release */,
			);
			defaultConfigurationIsVisible = 0;
			defaultConfigurationName = Release;
		};
/* End XCConfigurationList section */
	};
	rootObject = 8E4A20871DD12224003AABD3 /* Project object */;
}
"
