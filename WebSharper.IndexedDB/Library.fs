namespace WebSharper.IndexedDB

open WebSharper
open WebSharper.JavaScript

[<JavaScript;AutoOpen>]
module Extensions =
    type JavaScript.WindowOrWorkerGlobalScope with
        [<Inline "window.indexedDB">]
        member this.IndexedDB with get() : IDBFactory = X<IDBFactory>

    type IDBObjectStore with 
        member inline this.AddPromise(a: obj) =
            let req = this.Add(a)
            Promise<Dom.Event>(fun (resolve,reject) ->
                req.Onsuccess <- resolve
                req.Onerror <- reject
            )
        member inline this.AddPromise(a:obj, b:string) =
            let req = this.Add(a,b)
            Promise<Dom.Event>(fun (resolve,reject) ->
                req.Onsuccess <- resolve
                req.Onerror <- reject
            )
        
