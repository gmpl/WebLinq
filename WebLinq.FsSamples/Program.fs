open System
open WebLinq
open WebLinq.Html

type WebLinqBuilder() =
    member __.Return(x: 't) = Query.Create(fun context -> QueryResult.Create(Seq.singleton (QueryResultItem.Create(context, x))))
    member __.Yield(x: 't)  = Query.Create(fun context -> QueryResult.Create(Seq.singleton (QueryResultItem.Create(context, x))))
    member __.Bind(p, rest: 't -> IQuery<'u>) = Query.SelectMany(p, Func<_,_> rest , fun _ x -> x)
    member __.For(p, rest: 't -> IQuery<'u>)  = Query.SelectMany(p, Func<_,_> rest , fun _ x -> x)
    member __.Let(p, rest) = rest p        
    member __.ReturnFrom(expr) = expr

    [<CustomOperation("select", MaintainsVariableSpace=true, AllowIntoPattern=true)>]
    member __.Select(x:IQuery<'t>, [<ProjectionParameter>] f:'t->'u) = Query.Select (x, Func<_,_> f) : IQuery<'u>

    [<CustomOperation("where", MaintainsVariableSpace=true)>]
    member __.Where(x, [<ProjectionParameter>] p) = Query.Where(x, Func<_,_> p)

// sample code
type Stock<'a,'b,'c,'d,'e,'f> = {Date : 'a; Open : 'b; High : 'c; Low :'d; Close : 'e; Volume : 'f}

[<EntryPoint>]
let main argv =
    let weblinq = new WebLinqBuilder()

    let data = weblinq {    
        let! html = Uri @"http://www.nasdaq.com/symbol/v/historical" |> HttpQuery.Http.Get |> HtmlQuery.Html |> HttpQuery.Content
        for table in html.Tables("#historicalContainer table").ToQuery() do
        for row   in table.QuerySelectorAll("tr").ToQuery() do
        let cells = row.QuerySelectorAll "td" |> Seq.map (fun td -> td.InnerText) |> Seq.toArray
        where (cells.Length = 6)
        select {
            Date   = cells.[0].Trim()
            Open   = cells.[1].Trim()
            High   = cells.[2].Trim()
            Low    = cells.[3].Trim()
            Close  = cells.[4].Trim()
            Volume = cells.[5].Trim() } into row
        where (row.Date.Length > 0) // skip invalid rows
        yield row }

    let res = data.ToEnumerable(Func<_> DefaultQueryContext.Create)

    printfn "%A" (Seq.toArray res)
    Console.ReadKey() |> ignore

    0 // return an integer exit code