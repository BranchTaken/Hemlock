open ContainerIntf

module MakeMono (T : IMono) : SMono
  with type t := T.t
  with type elm := T.elm
= struct
  include T

  let is_empty t =
    Stdlib.((Int64.compare (T.length t) 0L) = 0)

  include ContainerCommon.MakeMonoFold(T)
  include ContainerCommon.MakeMonoMem(T)
  include ContainerArray.MakeMonoArray(T)
end

module MakePoly (T : IPoly) : SPolyGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm
= struct
  include T

  let is_empty t =
    Stdlib.((Int64.compare (T.length t) 0L) = 0)

  include ContainerCommon.MakePolyFold(T)
  include ContainerCommon.MakePolyMem(T)
  include ContainerArray.MakePolyArray(T)
end
