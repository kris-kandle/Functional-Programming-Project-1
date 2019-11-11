//Benjamin Bissett s184449 - Kristofer Kandle s184352 - Mikkel Christophersen s184393

type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list
type ArrivalCatalogue = (Airport * Lid list) list

//Q1
let rec findRoute( lid:Lid , lC:LuggageCatalogue ) =
    match lC with
    | (a,b)::ys -> if a = lid then b else findRoute( lid , ys )
    | [] -> []

//Q2
let rec inRoute(flight:Flight, route:Route)=
    match route with
    | [] -> false
    | (a,b)::ys -> a = flight || inRoute(flight,ys)

//Q2 With Higher Order Functions aka Task 3 Part 1
//List.exists used to check if a specific flight shows up in a route list, checks instances of matching string in list, very convenient
let rec flightFromRoute route =
    match route with
    | [] -> []
    | (a:Flight,b)::ys -> [a] @ flightFromRoute(ys)

let rec inRouteHigher(flight:Flight,flightFromRoute)= List.exists (fun y -> y = flight) flightFromRoute 

//Q3
//function which checks if flight is a member of flight list and if so returns the luggage id
let rec isMember(f:Flight,flightFromRoute,id:Lid) =
    match flightFromRoute with
    | [] -> []
    | (a,_)::_ when a=f -> [id]
    | (a,_)::tail -> isMember(f,tail,id)

let rec withFlight f lc =
    match lc with
    | [] -> []
    | (id,flightFromRoute)::tail -> isMember(f,flightFromRoute,id) @ withFlight f tail

//Q3 With Higher Order Functions 
//List.map is used to seperate the id from the other elements and put them in a new list. 
//List.filter checks if a flight shows up in a route list and produces a new list that consists of the luggage id's from the flight that do.
let withFlightHigher f lc = List.map(fun(id,_)->id) (List.filter(fun(_,flightFromRoute)->inRouteHigher(f,flightFromRoute))lc)

//Q4
//When it finds an empty list, it adds id. When it finds a list with the id, it returns the list. Otherwise, it adds id to idlist and returns new idlist
let rec insertId(lid:Lid, lidList:Lid list) =
    match lidList with
    | [] -> lid::[]
    | x::xs when x = lid -> lidList
    | x::xs -> x::insertId(lid, xs)

//Searches in cat for id, inserts port and id if nothing is found. Calls insertId when it finds a port matching our id's route 
let rec findInCat(lid:Lid,port,cat) =
    match cat with
    | [] -> [(port,[lid])]
    | (p,idList)::xs when p = port -> (p,insertId(lid,idList))::xs
    | x::xs -> x::findInCat(lid,port,xs)

//Main function. Returns cat if there's no route, otherwise it calls findInCat to insert id and its route into cat
let rec extend(lid:Lid,r:Route,ac:ArrivalCatalogue) =
    match r with
    | [] -> ac
    | (_,port)::xs -> extend(lid,xs,findInCat(lid,port,ac))

//Q5
//Main Process. Takes the Luggage Catalogue and an empty ArrivalCatalogue, seperates the id and route of the LC and makes an arrival catalogue from those
(*let rec toac(lc,ac) =
    match lc with
    | [] ->ac
    | (id,route)::xs->toac(xs,extend(id, route, ac))
    *)
//Because toArrivalCatalogue takes only one input, we write it out as so.
//let toArrivalCatalogue lc = toac(lc,[])

//Q5 with List.fold
//Takes lc and folds a function of arrivalCatalogue and (id, r) and puts it in extend(id,r,ac), sets initial ac to [], and uses lc fo (id,r)
let toArrivalCatalogue lc = List.fold (fun ac (id, r) -> extend(id,r,ac)) [] lc

