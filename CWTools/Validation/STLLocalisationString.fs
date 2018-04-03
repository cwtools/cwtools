namespace CWTools.Validation
open CWTools.Validation.ValidationCore
open CWTools.Process.STLProcess
open CWTools.Process
open CWTools.Process.ProcessCore
open CWTools.Parser.Types
open CWTools.Process.STLScopes
open CWTools.Common
open CWTools.Common.STLConstants
open DotNet.Globbing
open CWTools.Validation.STLValidation
open System.Xml.Linq
open System.Threading
open CWTools.Localisation

module STLLocalisationString =
    let validateLocalisation (api : ILocalisationAPI) =
        let all = api.Values
        all