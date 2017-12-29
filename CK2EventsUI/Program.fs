namespace CK2Events

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open ElectronNET.API
open Microsoft.Extensions.Configuration
open System.Text

module Program =
    let exitCode = 0

    let BuildWebHost args =
        WebHost
            .CreateDefaultBuilder(args)
            .ConfigureAppConfiguration(fun _ config -> config.AddJsonFile("userSettings.json", optional=true, reloadOnChange=true) |> ignore)
            .CaptureStartupErrors(true)
            .UseSetting(WebHostDefaults.DetailedErrorsKey, "true")
            .UseStartup<Startup>()
            .UseElectron(args)
            .Build()

    [<EntryPoint>]
    let main args =
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        BuildWebHost(args).Run()

        exitCode






