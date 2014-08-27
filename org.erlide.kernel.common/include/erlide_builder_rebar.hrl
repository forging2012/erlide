
-record(project_info, {
                       rootDir,
                       sourceDirs=[],
                       includeDirs=[],
                       outDir="ebin",
                       opts=[],
                       min_otp_vsn=".*",
                       libs=[]
                      }).
