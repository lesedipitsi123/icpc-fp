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

let rivers input = failwith ""
        

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
