namespace CK2_Events

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open ElectronNET.API
open ElectronNET.API.Entities
open ElectronNET.API.Entities
open ElectronNET.API



type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddMvc() |> ignore

    member this.ElectronBootstrap() =

        let prefs = BrowserWindowOptions()
        let webprefs = WebPreferences()
        webprefs.NodeIntegration <- false
        prefs.Show <- true
        prefs.WebPreferences <- webprefs
       // prefs.Fullscreen <- true

    //     Electron.WindowManager.CreateWindowAsync(prefs) |> Async.AwaitTask |> Async.RunSynchronously |> ignore
        
        Task.Run((fun _ -> async {Electron.WindowManager.CreateWindowAsync(prefs)  |> ignore} |> Async.RunSynchronously |> ignore)) |> ignore

    

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =

        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore
        else
            app.UseExceptionHandler("/Home/Error") |> ignore

        app.UseStaticFiles() |> ignore

        app.UseMvcWithDefaultRoute() |> ignore


        this.ElectronBootstrap()
        Electron.App.add_Quitting (fun _ -> Electron.Dialog.ShowMessageBoxAsync("Quitting") |> ignore)

        // app.UseMvc(fun routes ->
        //     routes.MapRoute(
        //         name = "default",
        //         template = "{controller=Home}/{action=Index}/{id?}") |> ignore
        //     ) |> ignore

    member val Configuration : IConfiguration = null with get, set
