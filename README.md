| **`Windows`** | **`Linux`** |
|-------------|-------------|
[![Build status](https://ci.appveyor.com/api/projects/status/tcx4nbu4yb3qubhu/branch/master?svg=true)](https://ci.appveyor.com/project/rokoDev/rabbit/branch/master)|[![CircleCI](https://circleci.com/gh/rokoDev/rabbit/tree/master.svg?style=svg)](https://circleci.com/gh/rokoDev/rabbit/tree/master)|

# rabbit

## Building for Xcode

### Prerequisites:
 - Installed CMake with version 3.18.3 or higher.
 - Installed Xcode.

To generate Xcode project run this command sequence in terminal:
```
git clone -b unresponsive --single-branch https://github.com/rokoDev/rabbit.git
cd rabbit
mkdir build
cd build
cmake -GXcode ..
```

Then run this command to open Xcode project:
```
cmake --open ./
```