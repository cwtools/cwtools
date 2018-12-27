// sets the current directory to be same as the script directory
System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)

// Requires FSharp.Data under script directory
// nuget install FSharp.Data -o Packages -ExcludeVersion
#r @"../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
open FSharp.Data
open System.IO

let eu4 = true
let eu4template = """
	## replace_scope = { $scopes$ }
	subtype[$type$] = {
		## cardinality = 0..1
		events = {
			## cardinality = 0..inf
			<$event$>
		}
		## cardinality = 0..1
		random_events = {
			## cardinality = 0..inf
			int = 0
			## cardinality = 0..inf
			int = <$event$>
		}

		alias_name[effect] = alias_match_left[effect]
	}"""
let stltemplate = """
	## replace_scope = { $scopes$ }
	subtype[$type$] = {
		## cardinality = 0..1
		events = {
			## cardinality = 0..inf
			<$event$>
		}
		## cardinality = 0..1
		random_events = {
			## cardinality = 0..inf
			int = 0
			## cardinality = 0..inf
			int = <$event$>
		}
	}"""
let typetemplate = """
		## type_key_filter = $key$
		subtype[$key$] = { }"""
let path = Path.GetFullPath(__SOURCE_DIRECTORY__)
let path2 = path + "/on_actions.csv"
let rows = CsvFile.Load(path2).Cache().Filter(fun r -> r.Columns.Length > 0)
let sb = new System.Text.StringBuilder()
sb.Append """types = {
	type[on_action] = {
		path = "game/common/on_actions"
		start_from_root = yes"""
for row in rows.Rows do
  let key = row.GetColumn "key"
  let output = typetemplate.Replace("$key$", key)
  sb.Append output
sb.Append """
	}
}
"""
sb.Append """on_action = {
	"""
let template = if eu4 then eu4template else stltemplate
for row in rows.Rows do
  let key = row.GetColumn "key"
  let scopes = row.GetColumn "scopes"
  let event = row.GetColumn "event"
  let eventtext = if event = "" then "event" else "event."+event
  let output = template.Replace("$type$", key).Replace("$scopes$",scopes).Replace("$event$",eventtext)
  sb.Append output
sb.Append """
}"""
printfn "%s" (sb.ToString())
File.WriteAllText(path + "/on_actions.cwt", sb.ToString())