module ICPC
open System

type Pos =
| Behind
| Infront

let commaSprinkler input =
    //splitting the input into a list
    let inputList (input:string) =
        let rec apply (input:string) (newlist:string list) word =
            match input.[0],input.Length > 1 with
            | _, false -> word::newlist
            | ' ', true -> apply input.[1..] (word::newlist) ""
            | '.', true -> apply input.[2..] ("."::word::newlist) ""
            | ',', true -> apply input.[2..] (","::word::newlist) ""
            | a, true -> apply input.[1..] (newlist) (word + string a)
        List.rev (apply (input) ([]) (""))
    
    let sprinkler (sentanceList:string list) (word:string) pos =
        match pos with
        | Behind -> 
            let rec apply (sentanceList:string list) (newlist:string list) indx =
                match indx, sentanceList.[indx], indx <> sentanceList.Length - 1, sentanceList.[indx] = word with
                | _, a, false, _ -> a::newlist
                | 0, a, true, _ -> apply sentanceList (a::newlist) (indx + 1)
                | _, a, true, false -> apply sentanceList (a::newlist) (indx + 1)
                | _, a, true, true ->
                    match sentanceList.[indx - 1] with
                    | "." -> apply sentanceList (a::newlist) (indx + 1)
                    | "," -> apply sentanceList (a::newlist) (indx + 1)
                    | _ -> apply sentanceList (a::","::newlist) (indx + 1)
            List.rev (apply (sentanceList) ([]) (0))
        | Infront -> 
            let rec apply (sentanceList:string list) (newlist:string list) indx =
                match indx, sentanceList.[indx], indx <> sentanceList.Length - 1, sentanceList.[indx] = word with
                | _, a, false, _ -> a::newlist
                | 0, a, true, _ -> apply sentanceList (a::newlist) (indx + 1)
                | _, a, true, false -> apply sentanceList (a::newlist) (indx + 1)
                | _, a, true, true ->
                    match sentanceList.[indx + 1] with
                    | "." -> apply sentanceList (a::newlist) (indx + 1)
                    | "," -> apply sentanceList (a::newlist) (indx + 1)
                    | _ -> apply sentanceList (","::a::newlist) (indx + 1)
            List.rev (apply (sentanceList) ([]) (0))
    
    let sentance = inputList input
    let areEqual x y = x = y

    let rec outputList (sentance:string list) (newlist:string list) (foundWords:string list) indx =
        match sentance.[indx], indx <> sentance.Length - 1 with
        | a, false -> a::newlist
        | a, true ->
            match a with
            | "," -> failwith "still need to find a way to check if i've already used a word"
                (*match sentance.[indx-1], sentance.[indx + 1], foundWords.  with
                | ".", ".", _ -> outputList sentance newlist (indx + 1)
                | ".", ",", _ -> outputList sentance newlist (indx + 1)
                | ",", ".", _ -> outputList sentance newlist (indx + 1)
                | ",", ",", _ -> outputList sentance newlist (indx + 1)
                | x, y, true ->
                    outputList sentance ()*)
                    
    failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
