module ICPC
open System
open System.Net.Sockets

// Data structure for words
type Position =
| Behind
| Infront

type Word = {
 position : Position
 word : string
}

let commaSprinkler (input : string) =
    let len = String.length input // get length of word
    // Split up sentence into a list w/ space as delimeter
    let rec split (sentence : string) (word: string) (newList : string list) idx =
        match idx < (len) with 
        | false -> "."::newList
        | true ->
            match sentence.[idx] with
            | ' ' -> split sentence ("") (word::newList) (idx + 1) // Ignore w/space & concat word to list
            | '.' -> split sentence (".") (word::newList) (idx + 1) // Ignore w/space & concat word to list
            | ',' -> split sentence (",") (word::newList) (idx + 1)
            | c -> split sentence (word + string c) newList (idx + 1) // Build word from sentece
    // return sentence as list
    let wordsList = List.rev (split input "" [] 0)
    printf "Sentence as list is %A\n" wordsList
    // Build list of all words precced or succeeded by ','
    let rec builder (sentence: string list) (searchWords: Word list) prevWord idx =
        match sentence with
        | [] -> searchWords
        | currWord::tail -> 
            match currWord, prevWord with
            | ",", prev -> builder (tail) ({position=Infront; word=prev}::searchWords) currWord (idx+1)
            | curr, "," -> builder (tail) ({position=Behind; word=curr}::searchWords) curr (idx+1)
            | a, _ -> builder (tail) searchWords a (idx+1) 
    let searchWords = builder (wordsList) [] ("") 0
    printf "builder returns %A\n" searchWords
    //finally place commas in appropriate positions.
    (*let rec addComma (sentence: string list) (searchWords: Word list) (prevWord: string) acc =
        match sentence with
        | [] -> Some acc
        | currWord::tail ->
            let word = searchWords |> List.filter (function x -> x.word = prevWord)
            match word with 
            | [] -> addComma (tail) (searchWords) (currWord) (acc + " " + currWord)
            | data -> 
                match data with
                | h1::t1 ->
                    match h1.position with
                    | Infront -> 
                        match currWord with
                        | ","|"." -> addComma tail (searchWords) currWord (currWord+acc)
                        | a -> addComma tail searchWords currWord (acc + ", " + a)
                    | Behind ->
                        match currWord with
                        | ","| "." -> addComma tail searchWords currWord acc
                        | a -> addComma tail searchWords currWord (","+a+acc)*)

    let sprinkler (sentanceList:string list) (word:Word) =
        match word.position with
        | Behind -> 
            let rec apply (sentanceList:string list) (newlist:string list) indx =
                match indx, sentanceList.[indx], indx <> sentanceList.Length - 1, sentanceList.[indx] = word.word with
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
                match indx, sentanceList.[indx], indx <> sentanceList.Length - 1, sentanceList.[indx] = word.word with
                | _, a, false, _ -> a::newlist
                | 0, a, true, _ -> apply sentanceList (a::newlist) (indx + 1)
                | _, a, true, false -> apply sentanceList (a::newlist) (indx + 1)
                | _, a, true, true ->
                    match sentanceList.[indx + 1] with
                    | "." -> apply sentanceList (a::newlist) (indx + 1)
                    | "," -> apply sentanceList (a::newlist) (indx + 1)
                    | _ -> apply sentanceList (","::a::newlist) (indx + 1)
            List.rev (apply (sentanceList) ([]) (0))

    let output wordslist = 
        let rec apply (wordlist: string list) (workingWords: Word List) (completedWords: Word List) =
            match workingWords with
            | [] -> wordlist
            | a::b -> 
                match (List.exists (fun x -> x=a) completedWords), (sprinkler (wordlist) a) with
                | false, x -> apply x (List.distinct (List.append (builder (x) [] ("") 0) (a::completedWords))) (a::completedWords)
                | true, _ -> apply wordlist b completedWords      
        apply wordslist (builder (wordsList) [] ("") 0) []
    
    let outputFormat (output:string list) =
        let rec apply (list:string list) (outString:string) (indx:int) =
            match indx < list.Length with
            | false -> outString.TrimEnd ()
            | true -> 
                match list.[indx] with
                | "," -> apply list (outString + ", ") (indx + 1)
                | "." -> apply list (outString + ". ") (indx + 1)
                | x ->
                    match indx <> list.Length - 1 with
                    | true -> 
                        match list.[indx + 1] with
                        | "," | "." -> apply list (outString + x) (indx + 1)
                        | _ -> apply list (outString + x + " ") (indx + 1)
                    | false -> apply list (outString + x) (indx + 1)
        apply output "" 0
    
    printf "Output is %A\n" (output wordsList)   
    printf "Output is %A\n" (Some (outputFormat (output wordsList)))
    Some (outputFormat (output wordsList))
    // printf "Search words are %A\n" searchWords

let rivers (input:string) = 
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
    

    //returns the longest river found
    let findRivers (spaceIndxList:int list) = 
        (*
            list.split
            1.foreach x in *initList*
                a.foreach y in *nextList*
                    if y = x +/- 1
                        length++
                    else
                        add length to riverLeangths

            //finding the next line
            find next -1 and add 1
                        
        *)

        // this splits the space index list into multiple lists per line
        // e.g [[]; [4; 8]; [2; 7]; [6]; [4; 7; 9]; []; [5]; [3; 7]; [3]; [9]; [6]; [5]; [5; 9]; [4; 7]; [3; 6]; [5]; []]
        // this works when only when the oldlist ends with -1
        let rec listSpliter (oldlist:int list) (newlist:int list) (outputlist:(int list) list) =
            match List.exists (fun x -> x = -1) oldlist with
            | false -> (List.rev newlist)::outputlist
            | true -> 
                match oldlist with
                | [] -> (List.rev newlist)::outputlist
                | a::b ->
                    match a with
                    | -1 -> listSpliter b [] ((List.rev newlist)::outputlist)
                    | _ -> listSpliter b (a::newlist) outputlist
        let splitlist = listSpliter (spaceIndxList) [] [[]]
        (*List.rev (listSpliter [4; 8; -1; 2; 7; -1; 6; -1; 4; 7; 9; -1; -1; 5; -1; 3; 7; -1; 3; -1; 9; -1;
   6; -1; 5; -1; 5; 9; -1; 4; 7; -1; 3; 6; -1; 5; -1] [] [[]]);;*)
        
        let splitlist = [[]; [4; 8]; [2; 7]; [6]; [4; 7; 9]; [5]; [3; 7]; [3]; [9]; [6]; [5]; [5; 9]; [4; 7]; [3; 6]; [5]; []]
        let findNextLine indx =
            match indx < splitlist.Length - 2 with
            | false -> -1
            | true -> indx + 1
        
        let findNextSpaceIndx xline indx = 
          match indx < splitlist.[xline].Length - 2 with
          | false -> -1
          | true -> indx + 1
        
        let rec outerloop (xline:int) (xelement:int) (riverlengths:int list) =
            //managing stop conditions
            match xline, xelement with
            | -1, _ -> riverlengths
            | _, -1 -> 
                match findNextLine xline with
                | -1 -> riverlengths
                | a -> 
                    match splitlist.[a] with
                    | [] -> riverlengths
                    | _ -> 
                        let rec innerloop (yline:int) (yelement:int) (length:int) =
                            match yline, yelement with
                            | -1, _ -> outerloop -1 0 (length::riverlengths)
                            | _, -1 -> outerloop -1 0 (length::riverlengths)
                            | _ -> 
                                //if y = x +/- 1
                                match splitlist.[yline].[yelement] = splitlist.[xline].[xelement] - 1 || splitlist.[yline].[yelement] = splitlist.[xline].[xelement] + 1 with
                                | false -> 
                                    match findNextSpaceIndx yline yelement with 
                                    | -1 -> outerloop (findNextLine xline) 0 (length::riverlengths)
                                    | a -> innerloop yline a length
                                | true -> innerloop (findNextLine yline) (findNextSpaceIndx yline yelement) (length + 1)
                        innerloop (findNextLine xline) 0 0
            | a, _ -> 
                match splitlist.[a] with
                | [] -> riverlengths
                | _ ->
                    let rec innerloop (yline:int) (yelement:int) (length:int) =
                        match yline, yelement with
                        | -1, _ -> outerloop -1 0 (length::riverlengths)
                        | _, -1 -> outerloop -1 0 (length::riverlengths)
                        | _ -> 
                            //if y = x +/- 1
                            match splitlist.[yline].[yelement] = splitlist.[xline].[xelement] - 1 || splitlist.[yline].[yelement] = splitlist.[xline].[xelement] + 1 with
                            | false -> 
                                match findNextSpaceIndx yline yelement with 
                                | -1 -> outerloop (findNextLine xline) 0 (length::riverlengths)
                                | a -> innerloop yline a length
                            | true -> innerloop (findNextLine yline) (findNextSpaceIndx yline yelement) (length + 1)
                    innerloop (findNextLine xline) 0 0
        outerloop 1 0 []
        let output = outerloop 1 0 []
        printf "findrivers returns %A\n" (output)
    //printf "findrivers returns %A\n" (findRivers (spaceIndxList (inputList "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country") 10))
        
    // takes in a space index list and a line width and returns the longest river
    let riverFinder spaceIndxList lineWidth =
        failwith "Not implemented"    

    failwith "Not implemented"
rivers "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
