# Collaborating with OWA

## Instructions
1. Email me at james@oneweekapps.com and say hi! Include your Github username so I can add you as a collaborator.
2. Install [GHC Platform](https://www.haskell.org/platform/) and [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
3. Install [hspec](http://hspec.github.io/) and [hlint](https://hackage.haskell.org/package/hlint)
4. Clone the repo
5. Run `stack build` to build the project
6. Run `stack test` to run unit tests

## Don't be shy!
Haskell is hard. I am by no means an expert and I don’t expect you to be. If you want to contribute to this project as one of your first steps into the larger world of Haskell, that’s great! I will try to label certain tickets with the “Easy” flag if I think they’d be good for someone just getting into the project. 

## Grabbing Tickets
Tickets are created and tracked through the [Issues](https://github.com/jhb563/OneWeekApps/issues) section on this Github page. Find an issue which is not assigned, make youself the assignee, and add the "Assigned" label to the issue.

**Prioritize fixing bugs first!** If you are new to the project and/or Haskell, look for issues with "Easy" label. 

## Branches
The master branch is the latest released build. The dev branch contains work for the next version. All new features and bug fixes should be worked on in new branches off of dev. The branch name should contain your initials and the name of the feature/bug (e.g. `jb-colors-parsing`). 

## Testing
OWA uses [hspec](http://hspec.github.io/) for feature testing. Only behavior of public methods should be tested. Most tests are either parsing or printing related. See the [style guide](STYLE_GUIDE.md) for more details, both on testing and generally. 

## Submitting Pull Requests
Once you are done implementing and testing the feature, submit a pull request to merge into dev. Before merging a PR, it must:
 1. Get a LGTM / "ship" comment and label
 2. Have all tests pass
 3. Have a successful build on CircleCI
 4. Pass [hlint](https://hackage.haskell.org/package/hlint)

If the PR is for a Bug Fix, it should include an entry in the bug_fixes file for the current version. If it is a feature, it should correspond to an existing entry in the spec file for the current version. Email me with any questions: james@oneweekapps.com

## Don't be a Jerk!
We must strive to help everyone become better developers rather than tear them down with disparaging comments. Mean-spirited or intentionally hurtful comments on PRs will not be tolerated! 
