--------------------------------------------------------------------------------
-- CHERI memory sml helpers
-- (c) Alexandre Joannou, University of Cambridge
--------------------------------------------------------------------------------

ifelse(dnl
regexp(CAP,p64),0,dnl
unit WriteDWORD (dwordAddr::bits(37), data::dword) =
    MEM(dwordAddr) <- Raw(data)

unit Write256 (addr::bits(35), data::bits(256)) =
{
    MEM(addr:'11') <- Raw (data<255:192>);
    MEM(addr:'10') <- Raw (data<191:128>);
    MEM(addr:'01') <- Raw (data<127:64>);
    MEM(addr:'00') <- Raw (data<63:0>)
}
,dnl
regexp(CAP,c128posits1\|c128c1\|c128c3\|c128concentrate\|p128),0,dnl
unit WriteDWORD (dwordAddr::bits(37), data::dword) =
{
    var fill = 0;
    match MEM(dwordAddr<36:1>)
    {
        case Raw (raw) => fill <- if dwordAddr<0> then raw<63:0> else raw<127:64>
        case _ => nothing
    };
    new_data = if dwordAddr<0> then Raw (data:fill) else Raw (fill:data);
    MEM(dwordAddr<36:1>) <- new_data
}

unit Write256 (addr::bits(35), data::bits(256)) =
{
    MEM(addr:'1') <- Raw (data<255:128>);
    MEM(addr:'0') <- Raw (data<127:0>)
}
,dnl
unit WriteDWORD (dwordAddr::bits(37), data::dword) =
{
    old_blob = match MEM(dwordAddr<36:Log2(CAPBYTEWIDTH)-3>)
    {
        case Cap (cap) => [&cap]
        case Raw (raw) => raw
    };
    new_blob = updateDwordInRaw (dwordAddr, data, ~0, old_blob);
    MEM(dwordAddr<36:Log2(CAPBYTEWIDTH)-3>) <- Raw(new_blob)
}

unit Write256 (addr::bits(35), data::bits(256)) =
{
    MEM(addr) <- Raw (data)
}
)dnl
