open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec tokenize input = 
    let len = String.length input in
    let truere = Re.compile (Re.Perl.re "^(true)") in 
    let falsere = Re.compile (Re.Perl.re "^(false)") in 
    let posnumre = Re.compile (Re.Perl.re "^([0-9]+)") in
    let negnumre = Re.compile (Re.Perl.re "^(\(-[0-9]+\))") in
    let strre = Re.compile (Re.Perl.re "^(\"[^\"]*\")") in
    let idre = Re.compile (Re.Perl.re "^([a-zA-Z][a-zA-Z0-9]*)") in 
    let lpre = Re.compile (Re.Perl.re "^\(") in
    let rpre = Re.compile (Re.Perl.re "^\)") in
    let lcre = Re.compile (Re.Perl.re "^\{") in
    let rcre = Re.compile (Re.Perl.re "^\}") in
    let dotre = Re.compile (Re.Perl.re "^\.") in 
    let eqre = Re.compile (Re.Perl.re "^=") in 
    let neqre = Re.compile (Re.Perl.re "^<>") in 
    let greatre = Re.compile (Re.Perl.re "^>") in 
    let lessre = Re.compile (Re.Perl.re "^<") in 
    let greateqre = Re.compile (Re.Perl.re "^>=") in 
    let lesseqre = Re.compile (Re.Perl.re "^<=") in 
    let orre = Re.compile (Re.Perl.re "^\|\|") in 
    let andre = Re.compile (Re.Perl.re "^&&") in 
    let notre = Re.compile (Re.Perl.re "^not") in 
    let ifre = Re.compile (Re.Perl.re "^if") in 
    let thenre = Re.compile (Re.Perl.re "^then") in 
    let elsere = Re.compile (Re.Perl.re "^else") in 
    let addre = Re.compile (Re.Perl.re "^\+") in
    let subre = Re.compile (Re.Perl.re "^-") in
    let mulre = Re.compile (Re.Perl.re "^\*") in
    let divre = Re.compile (Re.Perl.re "^\/") in
    let conre = Re.compile (Re.Perl.re "^\^") in
    let letre = Re.compile (Re.Perl.re "^let") in
    let defre = Re.compile (Re.Perl.re "^def") in
    let inre = Re.compile (Re.Perl.re "^in") in
    let recre = Re.compile (Re.Perl.re "^rec") in
    let funre = Re.compile (Re.Perl.re "^fun") in
    let arrowre = Re.compile (Re.Perl.re "^->") in
    let doublesemire = Re.compile (Re.Perl.re "^;;") in
    let semire = Re.compile (Re.Perl.re "^;") in
    let wsre = Re.compile (Re.Perl.re "^(\s+)") in
    
    if input = "" then []

    else if Re.execp idre input then
        let idgroup = Re.exec idre input in 
        let id = Re.Group.get idgroup 1 in
        let idlen = String.length id in

        if ((Re.execp truere input) && idlen=4) then 
            Tok_Bool(true)::(tokenize (String.sub input 4 (len - 4)))

        else if ((Re.execp falsere input) && idlen=5) then 
            Tok_Bool(false)::(tokenize (String.sub input 5 (len - 5)))

        else if ((Re.execp notre input) && idlen=3) then
            Tok_Not::(tokenize (String.sub input 3 (len - 3)))
        
        else if ((Re.execp ifre input) && idlen=2) then
            Tok_If::(tokenize (String.sub input 2 (len - 2)))
        
        else if ((Re.execp thenre input) && idlen=4) then
            Tok_Then::(tokenize (String.sub input 4 (len - 4)))
        
        else if ((Re.execp elsere input) && idlen=4) then
            Tok_Else::(tokenize (String.sub input 4 (len - 4)))
        
        else if ((Re.execp letre input) && idlen=3) then
            Tok_Let::(tokenize (String.sub input 3 (len - 3)))

        else if ((Re.execp defre input) && idlen=3) then
            Tok_Def::(tokenize (String.sub input 3 (len - 3)))

        else if ((Re.execp inre input) && idlen=2) then
            Tok_In::(tokenize (String.sub input 2 (len - 2)))

        else if ((Re.execp recre input) && idlen=3) then
            Tok_Rec::(tokenize (String.sub input 3 (len - 3)))

        else if ((Re.execp funre input) && idlen=3) then
            Tok_Fun::(tokenize (String.sub input 3 (len - 3)))

        else
            Tok_ID(id)::(tokenize (String.sub input idlen (len - idlen)))
    
    else if Re.execp strre input then
        let strgroup = Re.exec strre input in 
        let str = Re.Group.get strgroup 1 in
        let strlen = String.length str in
        let cstr = (String.sub str 1 (strlen - 2)) in
        Tok_String(cstr)::(tokenize (String.sub input strlen (len - strlen)))

    else if Re.execp posnumre input then
        let pnumgroup = Re.exec posnumre input in
        let pnum = Re.Group.get pnumgroup 1 in
        let pnumlen = String.length pnum in
        let pnumint = int_of_string pnum in
        Tok_Int(pnumint)::(tokenize (String.sub input pnumlen (len - pnumlen)))
    
    else if Re.execp negnumre input then
        let nnumgroup = Re.exec negnumre input in
        let nnum = Re.Group.get nnumgroup 1 in
        let nnumlen = String.length nnum in
        let nnumint = int_of_string (String.sub nnum 2 (nnumlen - 3)) in
        Tok_Int((-1)*nnumint)::(tokenize (String.sub input nnumlen (len - nnumlen)))
    
    else if Re.execp lpre input then
        Tok_LParen::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp rpre input then
        Tok_RParen::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp lcre input then
        Tok_LCurly::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp rcre input then
        Tok_RCurly::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp dotre input then
        Tok_Dot::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp eqre input then
        Tok_Equal::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp neqre input then
        Tok_NotEqual::(tokenize (String.sub input 2 (len - 2)))
    
    else if Re.execp greateqre input then
        Tok_GreaterEqual::(tokenize (String.sub input 2 (len - 2)))
    
    else if Re.execp lesseqre input then
        Tok_LessEqual::(tokenize (String.sub input 2 (len - 2)))
    
    else if Re.execp greatre input then
        Tok_Greater::(tokenize (String.sub input 1 (len - 1)))
   
    else if Re.execp lessre input then
        Tok_Less::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp orre input then
        Tok_Or::(tokenize (String.sub input 2 (len - 2)))
    
    else if Re.execp andre input then
        Tok_And::(tokenize (String.sub input 2 (len - 2)))

    else if Re.execp arrowre input then
        Tok_Arrow::(tokenize (String.sub input 2 (len - 2)))
    
    else if Re.execp addre input then
        Tok_Add::(tokenize (String.sub input 1 (len - 1)))

    else if Re.execp subre input then
        Tok_Sub::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp mulre input then
        Tok_Mult::(tokenize (String.sub input 1 (len - 1)))

    else if Re.execp divre input then
        Tok_Div::(tokenize (String.sub input 1 (len - 1)))
    
    else if Re.execp conre input then
        Tok_Concat::(tokenize (String.sub input 1 (len - 1)))

    else if Re.execp doublesemire input then
        Tok_DoubleSemi::(tokenize (String.sub input 2 (len - 2)))

    else if Re.execp semire input then
        Tok_Semi::(tokenize (String.sub input 1 (len - 1)))

    else if Re.execp wsre input then 
        let wsgroup = Re.exec wsre input in 
        let ws = Re.Group.get wsgroup 1 in 
        let wslen = String.length ws in
        (tokenize (String.sub input wslen (len - wslen)))
    
    else
        raise (Failure "InvalidInputException")