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
    // takes in the input and returns a string list of the words and space characters e.g ["Like"; " "; "this"]
    let inputList (input:string) =
        let rec apply (input:string) (newlist:string list) word =
            match input.[0],input.Length > 1 with
            | _, false -> word::newlist
            | ' ', true -> apply input.[1..] (" "::word::newlist) ""
            | a, true -> apply input.[1..] (newlist) (word + string a)
        List.rev (apply (input) ([]) (""))
    
    //takes in a list of strings and a line width and returns a list with the index of each space character
    //eg. [5, 8, 13, -1, 4] where -1 represents the start of a new line
    let spaceIndxList (input:string list) (lineWidth:int) =
        let rec apply (newlist:int list) (indx:int) (spaceindx:int) (length:int) =
            match indx < input.Length with
            | false -> newlist
            | true -> 
                match length + input.[indx].Length <= lineWidth with
                | false ->
                    match input.[indx] with
                    | " " -> apply (-1::newlist) (indx + 1) 0 0
                    | x -> apply (-1::newlist) (indx + 1) (x.Length) (x.Length)
                | true ->
                    match input.[indx] with
                    | " " -> apply (spaceindx::newlist) (indx + 1) (spaceindx + 1) (length + 1)
                    | x -> apply newlist (indx + 1) (spaceindx + x.Length) (length + x.Length)
        List.rev (apply [] 0 0 0)
    
    (*spaceIndxList ["When"; " "; "two"; " "; "or"; " "; "more"; " "; "rivers"; " "; "meet"; " ";
   "at"; " "; "a"; " "; "confluence"; " "; "other"; " "; "than"; " "; "the";
   " "; "sea"; " "; "the"; " "; "resulting"; " "; "merged"; " "; "river"; " ";
   "takes"; " "; "the"; " "; "name"; " "; "of"; " "; "one"; " "; "of"; " ";
   "those"; " "; "river"] 10*)

    //returns the longest river found
    let findRivers spaceIndxList = failwith "Not implemented"
        (*let rec apply (spaceIndxList:int list) (riverLengths:int list) (length:int)=
            let count = List.find (x = -1) spaceIndxList
            let x = 0
            match x < count with
            | false -> riverLengths
            | true ->
                indx = x
                let rec loop (indx:int)=
                    match indx < spaceIndxList.Length with
                    | false -> length::riverLengths
                    | true -> 
                        match spaceIndxList.[indx] with
                        | -1 -> loop (indx + 1)*)
                    

    // takes in a space index list and a line width and returns the longest river
    let riverFinder spaceIndxList lineWidth =
        failwith "Not implemented"    

    

    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
