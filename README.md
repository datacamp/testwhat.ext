> :warning: **This repo has outdated tokens in its travisci config**
> To make new releases for this project it needs to be moved to circleci

# testwhat.ext

[![Build Status](https://travis-ci.org/datacamp/testwhat.ext.svg?branch=master)](https://travis-ci.org/datacamp/testwhat.ext)
[![codecov](https://codecov.io/gh/datacamp/testwhat.ext/branch/master/graph/badge.svg)](https://codecov.io/gh/datacamp/testwhat.ext)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat.ext.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat.ext?ref=badge_shield)

Extensions for testwhat for specific use-cases or more high level checks. Documentation can be found [here](https://datacamp/github.io/testwhat.ext).

To use the extensions in the submission correctness tests for an exercise, explicitly load the package before you use the function:

```R
library(testwhat.ext)
ex() %>% check_cpp_function_exported("int", "answer")
```

## Adding SCTs to testwhat.ext

Follow these steps

1. Open a PR, merge into master when appropriate.
2. Once merged, edit DESCRIPTION, incrementing `VERSION: 0.0.1` to reflect changes ([see semver for guidance](http://semver.org/)).
3. Create a github release labeled `vVERSION`. E.g. `v0.0.1`. (see [here](https://help.github.com/articles/creating-releases/)).


## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat.ext.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fdatacamp%2Ftestwhat.ext?ref=badge_large)
