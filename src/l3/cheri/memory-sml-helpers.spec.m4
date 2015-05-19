--------------------------------------------------------------------------------
-- CHERI memory sml helpers
-- (c) Alexandre Joannou, University of Cambridge
--------------------------------------------------------------------------------

include(`helpers.m4')dnl
include(`cap-params.m4')dnl
ifelse(dnl
CAPBYTEWIDTH, 16,dnl
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
    old_blob = match MEM(dwordAddr<36:eval(log2(CAPBYTEWIDTH)-3)>)
    {
        case Cap (cap) => [&cap]
        case Raw (raw) => raw
    };
    new_blob = updateDwordInRaw (dwordAddr, data, ~0, old_blob);
    MEM(dwordAddr<36:eval(log2(CAPBYTEWIDTH)-3)>) <- Raw(new_blob)
}

unit Write256 (addr::bits(35), data::bits(256)) =
{
    MEM(addr) <- Raw (data)
}
)dnl
