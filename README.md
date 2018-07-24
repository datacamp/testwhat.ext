# testwhat.ext

[![Build Status](https://travis-ci.org/datacamp/testwhat.ext.svg?branch=master)](https://travis-ci.org/datacamp/testwhat.ext)
[![codecov](https://codecov.io/gh/datacamp/testwhat.ext/branch/master/graph/badge.svg)](https://codecov.io/gh/datacamp/testwhat.ext)

Extensions (high-level SCTs) for testwhat

Including in a DataCamp course
------------------------------

In the course's `requirements.R`, add

```R
library(remotes)

install_github("datacamp/testwhat")
install_github("datacamp/testwhat.ext")
```

To use the extensions in an exercise's SCT, import the function you want into the SCT block of the exercise:

```R
library(testwhat.ext)

ex() %>% check_object2(c('a', 'b', 'c'))
```

Adding SCTs to testwhat.ext
----------------------------

Follow these steps

1. Open a PR, merge into master when appropriate.
2. Once merged, edit DESCRIPTION, incrementing `VERSION: 0.0.1` to reflect changes ([see semver for guidance](http://semver.org/)).
3. Create a github release labeled `vVERSION`. E.g. `v0.0.1`. (see [here](https://help.github.com/articles/creating-releases/)).
