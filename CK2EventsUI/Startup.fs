namespace CK2Events

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection

open ElectronNET.API
open ElectronNET.API.Entities
open CK2Events.Application
open Application.Localisation
open System



type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration
    

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.  
        services.AddMvc() |> ignore
        services.AddOptions() |> ignore
        services.Configure<CK2Settings>(this.Configuration.GetSection("userSettings")) |> ignore
        services.AddSingleton<IConfiguration>(this.Configuration) |> ignore
        services.AddScoped<LocalisationService>() |> ignore
        services.AddSingleton<AppSettings>() |> ignore


    member __.ElectronBootstrap(appSettings : AppSettings, port : string) =
        let devTools = MenuItem(Label = "Open Developer Tools",
                        Accelerator = "CmdOrCtrl+I",
                        Click = (fun _ -> Electron.WindowManager.BrowserWindows |> Seq.head |> (fun f -> f.WebContents.OpenDevTools())))
        let goToGame (x : Game) = Electron.WindowManager.BrowserWindows |> Seq.iter (fun w -> w.LoadURL ("http://localhost:" + port + "/home/index?game=" + x.ToString()))
        let click (x : Game) = (fun _ -> appSettings.currentGame <- x; goToGame x)
        let submenu = [|MenuItem(Label="Crusader Kings 2", Click = Action (click Game.CK2), Type = MenuType.radio, Checked = true);
                        MenuItem(Label="Hearts of Iron IV", Click = Action (click Game.HOI4), Type = MenuType.radio);|]
                        //devTools|]
        let menu = MenuItem(Label="Game", Submenu = submenu)
        let newMenu = Electron.Menu.MenuItems |> List.ofSeq |> (fun l -> menu::l) |> Array.ofList
        Electron.Menu.SetApplicationMenu(newMenu)

        let webprefs = WebPreferences (NodeIntegration = false)
        let prefs = BrowserWindowOptions (WebPreferences = webprefs, Title = "CK2 Events")
        Electron.App.CommandLineAppendArgument("--disable-http-cache")
        Electron.App.CommandLineAppendSwitch("--disable-http-cache")
        let window = Electron.WindowManager.CreateWindowAsync(prefs) |> Async.AwaitTask |> Async.RunSynchronously
        window.Maximize()
        window.Reload()
        Electron.App.add_WindowAllClosed (fun () -> Electron.App.Exit())
        
    

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment, appSettings : AppSettings) =

        if (env.IsDevelopment() || HybridSupport.IsElectronActive) then
            app.UseDeveloperExceptionPage() |> ignore
        else
            app.UseExceptionHandler("/Home/Error") |> ignore

        app.UseStaticFiles() |> ignore

        app.UseMvcWithDefaultRoute() |> ignore

        if HybridSupport.IsElectronActive then
            this.ElectronBootstrap(appSettings, BridgeSettings.WebPort)
            Electron.App.add_Quitting (fun _ -> Electron.Dialog.ShowMessageBoxAsync("Quitting") |> ignore)

    member val Configuration : IConfiguration = null with get, set
