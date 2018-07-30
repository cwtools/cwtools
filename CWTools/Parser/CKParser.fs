namespace CWTools.Parser

open FParsec
open Types


 module CKParser =

    let parseEventFile filepath = runParserOnFile SharedParsers.alle () filepath (System.Text.Encoding.GetEncoding(1252))

    let private applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
        let reply = parser stream
        if reply.Status = Ok then
            Success(reply.Result, stream.UserState, stream.Position)
        else
            let error = ParserError(stream.Position, stream.UserState, reply.Error)
            Failure(error.ToString(stream), error, stream.UserState)

    let parseFile (filepath : string) =
        use stream = new CharStream<unit>(filepath, System.Text.Encoding.GetEncoding(1252))
        stream.UserState <- ()
        stream.Name <- filepath
        applyParser SharedParsers.all stream


    let private memoize keyFunction memFunction =
        let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            match dict.TryGetValue(keyFunction(n)) with
            | (true, v) -> v
            | _ ->
                let temp = memFunction(n)
                dict.Add(keyFunction(n), temp)
                temp

    let parseString fileString filename = runParserOnString SharedParsers.all () filename fileString
        // let inner = (fun (file, name) -> runParserOnString all () name file)
        // let hash = (fun (file, name) -> file.GetHashCode(), name)
        // (memoize hash inner) (fileString, filename)
    let parseEventString fileString fileName =
        let inner = (fun (file, name) -> runParserOnString SharedParsers.alle () name file)
        let hash = (fun (file, name) -> file.GetHashCode(), name)
        (memoize hash inner) (fileString, fileName)

    let getSuccess (result) =
        match result with
        |Success(s, _, _) -> s
        |_ -> EventFile []


