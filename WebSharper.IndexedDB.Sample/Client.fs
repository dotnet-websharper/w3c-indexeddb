namespace WebSharper.IndexedDB.Sample

open WebSharper
open WebSharper.IndexedDB
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Server
open WebSharper.UI.Html

[<JavaScript>]
module Client =
    
    type Person = {
        Name: string
        Age: uint
    }


    let progressList = Var.Create<Doc list> [h2 [] [text "DB Insert+Remove sample"]]

    let updateProgressList doc = progressList.Update (fun l -> List.insertAt (l.Length-1) doc l)
    let updateProgress msg = 
        Console.Log msg
        
        progressList.Update (fun l -> List.insertAt (l.Length-1) (div [] [text msg]) l)

    let logDatabases msg =
        let prom = promise {
            updateProgress msg
            let! dbs = JS.Window.IndexedDB.Databases()
            
            if dbs = null || dbs.Length = 0 then 
                div [] [text "No databases found."]
            else
                table [] [
                    tr [] [
                        th [] [text "Name"]
                        th [] [text "Version"]
                    ]
                    yield! dbs |> Array.map (fun dbinfo ->
                        tr [] [
                            td [] [text dbinfo.Name]
                            td [] [text <| $"%i{dbinfo.Version}"]
                        ]
                    )
                ]
            |> updateProgressList

        }
        prom.Catch(fun exn -> 
            updateProgressList <| div [] [text $"Error: {exn}"]
            Console.Error exn)
    [<SPAEntryPoint>]
    let Main () =
        logDatabases "Initial database list: " |> ignore
        try
            let dbEvt = JS.Window.IndexedDB.Open("db1", As<uint32> 1)
            dbEvt.Onupgradeneeded <- (fun e ->
                let db1 = dbEvt.Result
                
                if e?oldVersion < 1 then
                    let keyPath = nameof(Unchecked.defaultof<Person>.Name)
                    let store1 = 
                        db1
                            .CreateObjectStore("store1", 
                            IDBObjectStore.CreateOptions(
                                KeyPath=keyPath,
                                AutoIncrement=false
                            ))
                            .CreateIndex("personName", keyPath, IDBObjectStore.IndexOptions(Unique=false))
                            .ObjectStore

                    // adding individual objects
                    store1.Add({Name="Philip";Age=As<uint> 35}) |> ignore
                    store1.Add({Name="Joe"; Age=As<uint> 17}) |> ignore
                    
                    // mini boilerplate to add Seq types
                    [|
                        {Name="Lucy"; Age=As<uint> 24}
                        {Name="Matt"; Age=As<uint> 19}
                        {Name="Anna"; Age=As<uint> 44}
                    |] |> Array.map (store1.Add) |> ignore
                    
                    updateProgress """Initialized database and added members to "store1" """
                    logDatabases "Database list after db1 init: " |> ignore
            )
            dbEvt.Onsuccess <- (fun e ->
                let db1 = dbEvt.Result
                let transaction = db1.Transaction([|"store1"|], IDBTransaction.TransactionMode.Readwrite, IDBTransaction.Options(
                    IDBTransaction.TransactionDurability.Strict
                ))
                let transactionQuery = transaction.ObjectStore("store1").GetAll<Person>()
                transactionQuery.Onsuccess <- (fun e ->
                    
                    updateProgressList <| h3 [] [text "GetAll query test below: "]
                    updateProgressList <| table [] [
                        tr [] [
                            th [] [text "Name"]
                            th [] [text "Age"]
                        ]
                        yield! transactionQuery.Result |> Array.map (fun person ->
                            tr [] [
                                td [] [text person.Name]
                                td [] [text <| $"%i{person.Age}"]
                            ]
                        )
                    ]
                )
                //transaction.
                transaction.Oncomplete <- (fun e ->
                    Console.Log "Transaction completed"
                    let delEvt = JS.Window.IndexedDB.DeleteDatabase("db1")

                    delEvt.Onblocked <- (fun e ->
                        updateProgressList <| div [] [text "On database deletion, you have to listen to its onblocked event instead of onsuccess"]
                        logDatabases "After test database deletion : " |> ignore
                    )
                    delEvt.Onerror <- (fun e ->
                        updateProgressList <| div [] [text $"Error: {delEvt.Error}"]
                    )
                    
                )
            )
        with
        | e -> Console.Error e

        [
            h1 [] [text "IndexedDB sample page"]
            progressList.View |> Doc.BindSeqCached id
        ] |> Doc.Concat
        |> Doc.RunById "main"