// sets the current directory to be same as the script directory
System.IO.Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)

// Requires FSharp.Data under script directory
// nuget install FSharp.Data -o Packages -ExcludeVersion
#r @"../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"

open FSharp.Data
open System.IO

let eu4 = false
let hoi4 = false
let ck2 = true

let hoi4template =
    """
	## replace_scope = { $scopes$ }
	subtype[$type$] = {
		## cardinality = 0..1
		random_events = {
			## cardinality = 0..inf
			int = 0
			## cardinality = 0..inf
			int = <$event$>
		}
		## cardinality = 0..1
		effect = {
			alias_name[effect] = alias_match_left[effect]
		}
	}"""

let eu4template =
    """
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

let stltemplate =
    """
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

let ck2template =
    """
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
	        ### Left hand is weight. Chance of nothing happening.
			int = 0
			## cardinality = 0..inf
	        ### Left hand is weight, multiplied by the event's weight_multiplier. Right hand is event.
			int = <$event$>
	        ### Each block is evaluated separately and has one (or none if the 0 entry is chosen) event happen.
        	scalar = {
	            ## cardinality = 0..1
	            ### How many days to delay the event. Useful to ensure the player doesn't get spammed with a bunch of events at the same time.
	            delay = int

	            ## cardinality = 0..inf
	            ### Left hand is weight, multiplied by the event's weight_multiplier. Right hand is event.
	            int = <$event$>

	            ## cardinality = 0..1
	            ### Left hand is weight. Chance of nothing happening.
	            int = 0
        	}

		}

		## cardinality = 0..1
		effect = {
			alias_name[effect] = alias_match_left[effect]
		}
	}"""

let typetemplate =
    """
		## type_key_filter = $key$
		### $comment$
		subtype[$key$] = { }"""

let path = Path.GetFullPath(__SOURCE_DIRECTORY__)
let path2 = path + "/on_actions.csv"
let rows = CsvFile.Load(path2).Cache().Filter(fun r -> r.Columns.Length > 0)
let sb = new System.Text.StringBuilder()

sb.Append
    """types = {
	type[on_action] = {
		path = "game/common/on_actions"
		start_from_root = yes"""

for row in rows.Rows do
    let key = row.GetColumn "key"
    let comment = row.GetColumn "comment"
    let output = typetemplate.Replace("$key$", key).Replace("$comment$", comment)
    sb.Append output

sb.Append
    """
	}
}
"""

sb.Append
    """on_action = {
	"""

let template =
    if eu4 then eu4template
    else if hoi4 then hoi4template
    else if ck2 then ck2template
    else stltemplate

for row in rows.Rows do
    let key = row.GetColumn "key"
    let scopes = row.GetColumn "scopes"
    let event = row.GetColumn "event"
    let eventtext = if event = "" then "event" else "event." + event

    let output =
        template.Replace("$type$", key).Replace("$scopes$", scopes).Replace("$event$", eventtext)

    sb.Append output

sb.Append
    """
}"""

printfn "%s" (sb.ToString())
File.WriteAllText(path + "/on_actions.cwt", sb.ToString())
