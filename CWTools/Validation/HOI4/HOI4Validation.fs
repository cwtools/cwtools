namespace CWTools.Validation.HOI4

open CWTools.Process
open CWTools.Process.ProcessCore

module HOI4Validation =
    let findAllSetVariables (node: Node) =
        let getVar (n: Node) (a: string list) : string list =
            let varTag = n.TagText "var"

            if varTag <> "" then
                varTag :: a
            else
                match n.Values with
                | [] -> a
                | x :: _ -> x.Key :: a

        let keys =
            [ "set_variable"
              "add_to_variable"
              "subtract_from_variable"
              "multiply_variable"
              "divide_variable"
              "set_temp_variable"
              "add_to_temp_variable"
              "subtract_from_temp_variable"
              "multiply_temp_variable"
              "divide_temp_variable" ]

        let fNode =
            (fun (x: Node) acc ->
                x.Children
                |> List.fold (fun a n -> if List.contains n.Key keys then getVar n a else a) acc)

        foldNode7 fNode node |> List.ofSeq
