defmodule AdventOfCode.Y2020.Day21Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2021

  alias AdventOfCode.Y2020.Day21, as: Solution

  test "Year 2020, Day 21," do
    assert Solution.run() == {1679, "lmxt,rggkbpj,mxf,gpxmf,nmtzlj,dlkxsxg,fvqg,dxzq"}
  end
end
