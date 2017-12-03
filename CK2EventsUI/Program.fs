namespace CK2Events

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open ElectronNET.API

module Program =
    let exitCode = 0

    let BuildWebHost args =
        WebHost
            .CreateDefaultBuilder(args)
            .CaptureStartupErrors(true)
            .UseSetting(WebHostDefaults.DetailedErrorsKey, "true")
            .UseStartup<Startup>()
            .UseElectron(args)
            .Build()

    [<EntryPoint>]
    let main args =
        BuildWebHost(args).Run()

        exitCode
