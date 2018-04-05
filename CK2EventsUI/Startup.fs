namespace CK2Events

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection

open ElectronNET.API
open ElectronNET.API.Entities
open CK2Events.Application
open System
open Microsoft.Extensions.Options
open CWTools.Common
open CWTools.Localisation
open CWTools.Localisation.CK2Localisation
open CWTools.Localisation.EU4Localisation
open CWTools.Localisation.HOI4Localisation
open CWTools.Localisation.STLLocalisation


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
        services.AddTransient<CK2Settings>(fun p -> p.GetService<IOptionsSnapshot<CK2Settings>>().Value) |> ignore
        services.AddSingleton<AppSettings>() |> ignore
        services.AddScoped<ILocalisationAPI>(fun provider -> 
            let appsettings = provider.GetService<AppSettings>()
            let settings = provider.GetService<CK2Settings>()
            match appsettings.currentGame with
            |Game.CK2 -> CK2LocalisationService({ folder = settings.CK2Directory.localisationDirectory}).Api (settings.ck2Language)
            |Game.EU4 -> EU4LocalisationService({ folder = settings.EU4Directory.localisationDirectory}).Api (Lang.CK2 settings.ck2Language)
            |Game.HOI4 -> HOI4LocalisationService({ folder = settings.HOI4Directory.localisationDirectory}).Api (Lang.CK2 settings.ck2Language)
            |Game.STL -> STLLocalisationService({ folder = settings.STLDirectory.localisationDirectory}).Api (Lang.STL settings.stlLanguage)
            | x -> failwith ("Unknown game enum value " + x.ToString())
            ) |> ignore


    member __.ElectronBootstrap(appSettings : AppSettings, port : string) =
        let mutable newMenu = [||]
        let mutable gameSelects = [||]
        let setChecked (g : Game) (i : int) (m : MenuItem) =
            let enums : Game seq = unbox (Enum.GetValues(typeof<Game>))
            if List.exists (fun f -> (int f) = i) (enums |> List.ofSeq) then m.Checked <- ((int g) = i)
            
        let goToGame (x : Game) = Electron.WindowManager.BrowserWindows |> Seq.iter (fun w -> w.LoadURL ("http://localhost:" + port + "/home/index?game=" + x.ToString()))
        let click (x : Game) = (fun _ -> appSettings.currentGame <- x; goToGame x; gameSelects |> Array.iteri (setChecked x); Electron.Menu.SetApplicationMenu(newMenu) )
        gameSelects <- [|MenuItem(Label="Crusader Kings 2", Click = Action (click Game.CK2), Type = MenuType.radio, Checked = true);
                        MenuItem(Label="Hearts of Iron IV", Click = Action (click Game.HOI4), Type = MenuType.radio);
                        MenuItem(Label="Europa Universalis IV", Click = Action (click Game.EU4), Type = MenuType.radio);
                        MenuItem(Label="Stellaris", Click = Action (click Game.STL), Type = MenuType.radio);|]
        let createURL action controller = "http://localhost:" + port + "/" + controller + "/" + action
        let goToUrl action controller = Electron.WindowManager.BrowserWindows |> Seq.iter (fun w -> w.LoadURL(createURL action controller))

        let windowMenuItems = [|MenuItem(Label = "Events", Click = Action (fun _ -> goToUrl "index" "home"));
                                MenuItem(Label = "Localisation", Click = Action (fun _ -> goToUrl "localisation" "home"));
                                MenuItem(Label = "Validation", Click = Action (fun _ -> goToUrl "validation" "home"));
                                MenuItem(Label = "Settings", Click = Action (fun _ -> goToUrl "settings" "home"));
                                MenuItem(Label = "Select game", Submenu = gameSelects)
                                |]        
        let hiddenMenuItems = [|MenuItem(Role = MenuRole.cut);
                                MenuItem(Role = MenuRole.copy);
                                MenuItem(Role = MenuRole.paste);
                                MenuItem(Role = MenuRole.selectall);
                                MenuItem(Role = MenuRole.minimize);
                                MenuItem(Role = MenuRole.quit);
                                MenuItem(Role = MenuRole.close);
                                MenuItem(Role = MenuRole.togglefullscreen);
                                MenuItem(Type = MenuType.separator, Label="Debug");
                                MenuItem(Role = MenuRole.reload);
                                MenuItem(Role = MenuRole.toggledevtools);
                                |]

        let windowItem = MenuItem(Label="Menu", Submenu = windowMenuItems)
        let miscItems = MenuItem(Label = "Misc", Submenu = hiddenMenuItems)
        newMenu <- [|windowItem; miscItems|]
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
            Electron.App.add_Quitting (fun _ -> upcast Electron.Dialog.ShowMessageBoxAsync("Quitting"))

    member val Configuration : IConfiguration = null with get, set
