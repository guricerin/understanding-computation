# understanding-computation

## Requirements
.NET Core 3.1  
https://dotnet.microsoft.com/download  

## CLI

### Restoring after Clone
```shell
$ dotnet tool restore
```

### Build
```shell
$ dotnet fake build # Build all projects as Release
$ # or
$ dotnet build --project src/SampleApp [-c {Debug|Release}]
```

### Run
```shell
$ dotnet run --project src/SampleApp [-c {Debug|Release}]
```

### Tests
```shell
$ dotnet fake build -t Test
$ #or
$ dotnet run --project tests/SampleTest
```

## References
### [Paket](https://fsprojects.github.io/Paket/index.html)  
Each project requires `paket.references` file.

After updating [paket.dependencies](/paket.dependencies):
```shell
$ dotnet paket install
```

To Update Versions of Libraries,
```shell
$ dotnet paket update
```

### [FAKE](https://fake.build/)  
Scripting at [build.fsx](/build.fsx).  

```shell
$ dotnet fake build -t Clean # Run "Clean" Target
$ dotnet fake build # Run Default Taret
```

### Create Project
```shell
$ # Application
$ dotnet new console -lang=f# -o src/SampleApp
$ echo 'FSharp.Core' > src/SampleApp/paket.references
$ paket install

$ # Library
$ dotnet new classlib -lang=f# -o src/SampleLib
$ echo 'FSharp.Core' > src/SampleLib/paket.references
$ paket install
```

### Create Test Project
```shell
$ dotnet new console -lang=f# -o tests/SampleTest
$ echo -e 'FSharp.Core\nExpecto\nExpecto.FsCheck' > tests/SampleTest/paket.references

$ paket install # Add reference of Paket to .fsproj file
```
and then, Add **Project Name** to [build.fsx](/build.fsx).

### Create Solution
```shell
$ dotnet new sln
$ dotnet sln add src/SampleApp
$ dotnet sln add src/SampleLib
```

### Update Tool
```shell
$ dotnet fake build -t Tool
```
and then, commit [.config/dotnet-tools.json](/.config/dotnet-tools.json).
