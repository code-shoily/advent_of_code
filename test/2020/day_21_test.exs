defmodule AdventOfCode.Y2020.Day21Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2021

  alias AdventOfCode.Y2020.Day21, as: Solution

  test "Year 2020, Day 21, Part 1" do
    assert Solution.run_1() == 1679
  end

  test "Year 2020, Day 21, Part 2" do
    assert Solution.run_2() == "lmxt,rggkbpj,mxf,gpxmf,nmtzlj,dlkxsxg,fvqg,dxzq"
  end
end
