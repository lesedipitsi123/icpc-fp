module ICPC
open System

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
        | false -> newList
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
            | ",", prev -> builder (tail) ({position=Behind; word=prev}::searchWords) currWord (idx+1)
            | curr, "," -> builder (tail) ({position=Infront; word=curr}::searchWords) curr (idx+1)
            | a, _ -> builder (tail) searchWords a (idx+1) 
    let searchWords = builder (wordsList) [] ("") 0

    //finally place commas in appropriate positions.
    let rec addComma (sentence: string list) (searchWords: Word list) (prevWord: string) acc =
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
                        | a -> addComma tail searchWords currWord (","+a+acc)
            
    // printf "Search words are %A\n" searchWords
    failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
